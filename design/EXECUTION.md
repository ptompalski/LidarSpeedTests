# Process execution and resource monitoring

Status: implemented (Phase 3, Task 4). Code:
[`execution/process.py`](../src/lidarbench/execution/process.py),
[`execution/process_tree.py`](../src/lidarbench/execution/process_tree.py),
[`execution/attempt.py`](../src/lidarbench/execution/attempt.py),
[`monitoring/`](../src/lidarbench/monitoring/).

The same API is used on Windows, Linux and macOS. Platform differences are
contained in tree-control strategies and recorded in every result.

## 1. API

```python
from lidarbench.execution.attempt import execute_plan
from lidarbench.execution.plan import plan_invocation
from lidarbench.execution.process import CancellationToken, cancel_on_signals
from lidarbench.monitoring.psutil_backend import PsutilBackend

plan = plan_invocation(adapter, invocation)            # Task 3
token = CancellationToken()
with cancel_on_signals(token):                          # Ctrl+C -> token.cancel()
    attempt = execute_plan(plan, invocation, attempt_dir,
                           timeout_s=3600, cancel=token,
                           monitor_backend=PsutilBackend(interval_s=0.5))
attempt.execution_status        # succeeded | failed | timed_out | interrupted
```

`run_process(command, stdout_path=..., stderr_path=..., timeout_s=..., cancel=...,
monitor=..., thread_budget=...)` is the lower-level call used by `execute_plan`.
Both return immutable records; `to_record()` gives the JSON form
(`lidarbench.process-execution/1`, `lidarbench.attempt-execution/1`).
`execute_plan` writes `execution.json`, `stdout.log`, `stderr.log` and
`monitor/` in the attempt directory, all with exclusive create.

## 2. Launch

- `subprocess.Popen(list(argv), cwd=..., env=os.environ | delta, shell=False,
  stdin=DEVNULL, close_fds=True)`. Arguments are never shell-parsed, so paths
  with spaces or metacharacters need no quoting.
- stdout and stderr are redirected at OS level to files as raw bytes (no pipes,
  so no deadlocks and nothing held only in memory). `stdout_text()` decodes
  UTF-8 with `errors="replace"`.
- The orchestrator's environment is never mutated.

## 3. Timed interval

Default mode `fresh-process` with `timing_boundary = "process"`:

| Inside the timed interval | Outside it |
|---|---|
| process creation, interpreter/library start-up | input verification (`input_verification` phase) |
| input reading, computation | adapter config files (`config_files`) |
| output writing by the process | monitor sampler start-up and priming (`monitor_start`) |
| one `attach` line written to the sampler's stdin | tree cleanup after exit, sampler stop, summaries |
| | adapter interpretation of tool results (`interpretation`, only with an adapter) |
| | output presence check (`output_check`); validation is separate |

- Start: `perf_counter_ns()` immediately before `Popen`.
- End: captured in a dedicated reaper thread immediately after the OS reports
  the exit (`os.wait4` on POSIX, `WaitForSingleObject` on Windows). The
  supervising thread never polls for exit, so timing resolution is not limited
  by a poll interval.
- Both instants are stored with monotonic and wall clocks (`core/clock.py`).
- Data still in the OS write-back cache when the process exits is not timed
  unless the tool itself calls `fsync`.
- An adapter may report an inner workload interval through `interpret()`
  (`ToolTiming`, ADAPTERS.md §3). `execute_plan` records it with the derived
  start-up and shutdown overheads in `execution.json` → `interpretation.tool_timing`.
  It is descriptive: the timed interval above is unchanged.

## 4. Status safety

`ProcessResult.execution_status` is computed, never stored independently:

- `succeeded` only if the root process **exited by itself** with code `0`
  before any timeout or cancellation, and the engine recorded no exception;
- `timed_out` / `interrupted` when the engine stopped the process;
- `failed` for non-zero exits, POSIX signals (`exit_code < 0`,
  `termination_signal`), launch failures and engine errors.

A process that exits on its own just before a timeout or cancellation is
classified by its exit instant, not by when the engine noticed. Nothing is ever
retried automatically. `AttemptExecution` is never `succeeded` when the process
was not launched. An adapter's `interpret()` objection (or a defective
interpretation) makes a clean exit `failed` (`failure_phase = "interpretation"`).
The single exception to "nonzero is failed" is a named `AcceptedException`
that the command declared before launch (`CommandSpec.accepted_exceptions`,
normally enabled only by an explicit opt-in such as the LAStools cutoff-warning
policy). It applies only to a process that exited by itself with exactly the
declared status, and it is recorded with the raw exit code (ADAPTERS.md §3).
Timeouts, cancellations, signals and engine errors can never be accepted.

## 5. Process-tree control

| Strategy | Platforms | Launch | Termination | OS accounting |
|---|---|---|---|---|
| `posix-session` | Linux, macOS | `start_new_session=True` (new session and process group, pgid = pid) | `killpg(SIGTERM)`, grace (default 5 s), `killpg(SIGKILL)`; tracked escapees killed individually | `wait4` rusage |
| `windows-job` | Windows | `CREATE_SUSPENDED \| CREATE_NEW_PROCESS_GROUP`, assign to a Job Object with `KILL_ON_JOB_CLOSE`, then `NtResumeProcess` | `TerminateJobObject` (no grace phase) | Job accounting |
| `psutil-tree` | Windows fallback | plain | kill root + tracked descendants | none |

- Because the Windows child is assigned to the job **before** it runs, no
  grandchild can start outside the job.
- While the root runs, the engine snapshots its descendants every
  `descendant_poll_s` (default 0.5 s) so descendants that leave the process
  group (`setsid`, daemonisation) can still be killed. psutil verifies process
  creation times, so a recycled PID is never signalled.
- After a normal exit, surviving group/job members or tracked descendants are
  killed and reported (`leftover_cleanup`, warning). The status is unchanged:
  the root itself succeeded, but the warning shows that work may have continued
  outside the timed interval.
- `cleanup_complete` is `False` if any process is known to have survived.
- The sampler runs in its own session/process group, so terminal Ctrl+C and
  tree termination never hit it; it exits on its own when its stdin closes.
- `KeyboardInterrupt` during supervision terminates the tree and is re-raised
  as `ExecutionInterrupted` (a `KeyboardInterrupt` subclass) carrying the
  result; `execute_plan` writes `execution.json` first. `cancel_on_signals`
  converts `SIGINT`/`SIGTERM`/`SIGHUP` (Windows: `SIGINT`/`SIGBREAK`) into
  cancellation instead. Cleanup is interrupt-safe: a repeated Ctrl+C during the
  SIGTERM grace escalates straight to SIGKILL, and the tree is still reaped,
  the sampler stopped and the result produced.
- **Supervisor death (POSIX).** If the orchestrator itself is killed
  (`SIGKILL`, OOM killer), nothing else would stop the tool's session. A
  guardian process (`python -m lidarbench.execution.guardian`, started once per
  orchestrator process in its own session, outside every timed interval) is told
  `watch <pgid>` after each launch and `release <pgid>` after the tree is gone;
  when its stdin reaches EOF (the orchestrator exited, however it died) it sends
  `SIGKILL` to every group still watched. Tools never inherit the pipe
  (`close_fds`). On Windows the Job Object's `KILL_ON_JOB_CLOSE` does the same.
  A guardian failure is a warning, never a failed attempt.

## 6. Monitoring (`psutil` backend)

A separate sampler process (`python -m lidarbench.monitoring.sampler`) is
started and primed before the timed interval. Protocol: JSON lines; `ready` on
stdout; `attach` (pid, orchestrator pid, launch instant) and `stop` on stdin;
end of stdin also stops it. Samples are flushed row by row.

Membership of the process tree: the root and its descendants, plus on POSIX
every process of the root's **session** created after the launch (the root is a
session leader). Helpers that are reparented away from the tree but stay in the
session, such as R `parallelly`/`future` workers started through a shell or the
renv watchdog, are therefore sampled (Task 13 pilot finding: lidR workers at
2 CPUs were previously invisible). The capability `membership` records which
rule applied.

The two scopes are written to **separate files** and summarised in separate
sections:

| File | Scope | Contents |
|---|---|---|
| `machine-samples.csv` | `whole-machine` (includes unrelated activity) | CPU % of all logical CPUs, memory used/available/total, disk read/write bytes |
| `process-tree-samples.csv` | `process-tree` | process count, new/exited processes, CPU seconds, CPU % of **one** logical CPU, summed RSS, per-process I/O bytes |

Each row has `interval_start/end_perf_ns` and `interval_start/end_wall_ns`. The
first process-tree interval starts at the launch instant. If the monotonic
clocks of the two processes disagree (checked at `ready`), samples are aligned
on wall time and this is recorded.

Summaries (`monitoring/summary.py`) port the legacy R rules: only the overlap of
each sample interval with the timed interval counts, means are time-weighted,
and coverage below the interval (1 ms tolerance) is `partial`. Each metric has
its own `status`, `coverage_fraction` and either `mean`/`peak` or `total`
(`total_is_lower_bound` when coverage is incomplete). Per-process tree counters
(CPU, I/O, RSS, process count) in any sample where an observed process exited
are lower bounds: such metrics are `partial` with `values_are_lower_bounds`.
This always applies to the root's final interval, so sampled tree CPU is never
claimed to be complete; use `process_accounting` for complete totals. Overall
`monitoring_status` is `available`, `partial`, `unavailable` or `disabled`.

**Missing values are `null`, never `0`**: an empty CSV cell, a metric with no
valid samples (`total: null`, `reason`), or a whole scope that failed (`null`
section with reasons).

Monitoring failures never change `execution_status`:

- sampler fails to start: `execute_plan` does not launch when
  `monitor_required=True` (default; `failure_phase = "monitor_start"`), or runs
  with monitoring `unavailable` when `False`;
- sampler dies mid-run: the remaining samples are summarised as `partial` (or
  `unavailable`) with the reason.

### OS accounting (process-level, not sampled)

`process_accounting` comes from the kernel when the process is reaped and is
independent of sampling:

- POSIX `wait4`: user/system CPU and peak RSS of the root plus every descendant
  that was waited for by its parent (recursively). `max_rss_bytes` is the
  largest single process, not a sum. Orphans re-parented to init are excluded.
- Windows Job Object: CPU of **every** process that ran in the job (including
  short-lived ones), process count, peak committed job memory, and I/O bytes.

## 7. Thread budget, CPU pinning and environment

`environment.thread_budget` records the **configured** request
(`requested_threads`, `threading_model`) and, when the budget is enforced, the
`cpu_set` the tree is pinned to. Measured CPU use is in `monitoring.process_tree`
and `process_accounting`.

**Enforcement.** With `scheduling.cpu_affinity = "pinned"` (default) the
runner restricts every attempt's whole process tree to `cpus` logical CPUs
chosen by `execution/affinity.py`: one logical CPU per physical core in
(package, core) order from sysfs (Linux), the first core left to the
orchestrator and monitor when possible, SMT siblings only beyond the number of
cores; the same set for every attempt of a budget. Linux applies it with
`sched_setaffinity` in the child before `exec` (inherited by every descendant);
Windows sets it while the process is still suspended; the psutil fallback sets
it right after launch (children started earlier are not pinned); macOS has no
affinity API and records `unsupported`. The outcome is in
`process.cpu_affinity` (`status`, `cpus`, `method`, `verified`). Pinning caps
tools whose internal pools exceed the budget (Task 13: PDAL's `readers.las`
used ~2.9 cores at a 1-CPU budget). Adapters still request their budget
(threads, `-cores`, PDAL `readers.las threads = 1`). The results table reports
`process_cpu_seconds` (max of `rusage` and the sampled tree),
`measured_parallelism` (CPU-s per wall-s) and `cpu_budget_exceeded`
(`> cpus × elapsed × 1.10 + 0.1 s`). The record also
contains logical/physical CPUs, the orchestrator's CPU affinity, the adapter's
environment delta and the effective child values of thread-related variables
(`OMP_*`, BLAS, `RCPP_PARALLEL_NUM_THREADS`, `GDAL_NUM_THREADS`, ...), each
marked `delta`, `inherited` or `unset`. The full environment is never recorded.

## 8. Known limitations

- **Short-lived children.** The process tree is sampled at `interval_s`
  (default 0.5 s). Processes that start and exit between two samples are not
  observed; CPU and I/O a process used after its last observation are lost
  (`exited_processes` flags such intervals). OS accounting partly compensates:
  Windows job accounting is complete; POSIX `wait4` covers only waited-for
  descendants.
- **Re-parented descendants.** On POSIX they are sampled as session members;
  `wait4` accounting still excludes them (the report uses the larger of the
  sampled and the accounted CPU). On Windows a grandchild whose parent exits
  before it was observed cannot be found by psutil (job membership still
  covers it for termination).
- **CPU pinning** is not available on macOS; the budget is then only requested.
- **Process-group escape before observation (POSIX).** A descendant that calls
  `setsid` and whose ancestors all exit before the next descendant snapshot
  cannot be terminated. Linux child-subreaper and cgroup containment are
  possible future extensions; a BenchExec backend is deliberately not used yet.
- **Root exits before attach.** For a process shorter than the attach latency
  the tree is not observed; its metrics are `null` (not 0) and the scope is
  `unavailable`.
- **RSS sums** count shared pages once per process and overstate memory for
  multi-process tools; CPU % per interval can slightly exceed `100 × threads`
  because CPU counters have tick granularity.
- **Platform-specific counters.**
  - Per-process I/O: Linux reports storage-layer bytes (`/proc/<pid>/io`
    `read_bytes`/`write_bytes`; page-cache hits are not counted); Windows counts
    all I/O including pipes and network; macOS has no per-process I/O in psutil
    (`null` + reason).
  - Whole-machine disk counters may be unavailable in containers (`null`).
  - `max_rss_bytes` is KiB-scaled on Linux and bytes on macOS (normalised).
  - `PeakJobMemoryUsed` on Windows is committed private memory, not working set.
- **Windows termination** has no graceful phase: Windows provides no reliable
  console-independent signal, so the job is terminated immediately.
- **PID reuse.** psutil checks creation times before signalling. `killpg` on a
  group whose leader was reaped relies on the kernel not reusing a pgid that
  still has members.
