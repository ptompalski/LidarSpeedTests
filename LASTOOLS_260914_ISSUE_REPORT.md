# LAStools 260914 on Linux: intermittent executable-directory lookup failures

Hi,

I am getting occasional errors with LAStools 260914 on Linux. Sometimes a tool
says it is unlicensed, or cannot find a file needed to look up an EPSG code, even
though the license and other files are in the installation folder. Other runs
work without changing anything.

I asked an AI agent to investigate. It identified a likely bug in how LAStools
finds its installation folder, using execution traces and the public source
code. **The same AI agent wrote the detailed explanation and examples below.**
Please review its analysis and confirm whether the diagnosis is correct.

Could you advise whether there is a fix or a supported workaround?

---

## Technical report written by the AI agent

### Summary

LAStools 260914 on Linux intermittently fails license and EPSG/resource lookup.
Syscall traces show resources being sought **beneath the
executable filename**, rather than beside the executable:

```text
.../bin/lasheight64/lastoolslicense.txt
.../bin/lascanopy64/serf/geo/pcs.csv
```

These opens fail with `ENOTDIR`. The license and resource files are present at
their expected locations under `bin`, and successful launches read those same,
unchanged files.

Disassembly indicates an uninitialized-byte read in the native
`exe_path[abi:cxx11]()` routine shared by the examined binaries. There is also
evidence of a separate multicore error-propagation concern: the examined
implementation discards child exit statuses.

## Environment

| Item | Observed value |
|---|---|
| Platform | Linux x86-64, kernel `7.0.0-38-generic` |
| LAStools version | `260914` |
| Installation | `/home/ptompals/opt/lastools/bin` |
| Examined executables | `lasheight64`, `lascanopy64`, `blast2dem64` |
| Executable type | Regular ELF binaries, not shell launchers or symbolic links |
| License file | `bin/lastoolslicense.txt`; successful version queries report `(non-profit)` |
| Projection resources | `bin/serf/geo/pcs.csv` |
| Horizontal CRS | EPSG 2959, present in the installed `pcs.csv` as `NAD83(CSRS) / UTM zone 18N` |

The standalone startup example below launches the executable directly and does
not require LiDAR input files.

License contents are intentionally omitted from this report. Successful license
recognition is reported as an observation, not an audit of licensing entitlement.

## Standalone Bash demonstration

This example isolates the intermittent **license-file path lookup**. It only
requests version information with an EPSG argument. It does not process LiDAR
data, use demo mode, or change licensing configuration.

Requirements: Bash, `strace`, and GNU `timeout`. Adjust `bin` if necessary.
The script makes at most 256 startup attempts, with a five-second limit per
attempt, and stops when it detects the malformed license path.

```bash
bin=/home/ptompals/opt/lastools/bin

command -v strace >/dev/null || { echo "strace is required" >&2; exit 1; }
command -v timeout >/dev/null || { echo "GNU timeout is required" >&2; exit 1; }
test -x "$bin/lasheight64" || { echo "lasheight64 is not executable" >&2; exit 1; }
test -r "$bin/lastoolslicense.txt" || { echo "License file is not readable" >&2; exit 1; }

logs=$(mktemp -d /tmp/lastools-startup.XXXXXX) || exit 1
printf 'Evidence directory: %s\n' "$logs"
printf 'attempt\tstatus\n' >"$logs/status.tsv"
found=0

for n in {1..256}; do
    if timeout 5s strace -s 512 -e trace=readlink,openat \
        -o "$logs/$n.trace" \
        "$bin/lasheight64" -epsg 2959 -version \
        >"$logs/$n.stdout" 2>"$logs/$n.stderr"
    then
        status=0
    else
        status=$?
    fi

    printf '%s\t%s\n' "$n" "$status" >>"$logs/status.tsv"

    if grep -Fq "$bin/lasheight64/lastoolslicense.txt" "$logs/$n.trace"; then
        found=1
        printf 'Malformed lookup on attempt %s; status=%s\n' "$n" "$status"
        grep -F 'lastoolslicense.txt' "$logs/$n.trace"
        cat "$logs/$n.stderr"
        break
    fi
done

if [ "$found" -eq 0 ]; then
    echo "No malformed lookup observed in this bounded run."
    echo "This does not rule out the intermittent bug."
fi
```

All stdout, stderr, trace files, and statuses remain in the printed directory.
Without a timeout or launcher error, the recorded status is the native exit
status forwarded by `strace`; status 124 indicates that `timeout` expired.
Check stderr for any tracing or launcher errors before interpreting a record.

**Expected lookup:**

```text
/home/ptompals/opt/lastools/bin/lastoolslicense.txt
```

**Observed failing lookup:**

```text
openat(AT_FDCWD, "/home/ptompals/opt/lastools/bin/lasheight64/lastoolslicense.txt", O_RDONLY) = -1 ENOTDIR (Not a directory)
```

The corresponding version output includes:

```text
LAStools lasheight (by info@rapidlasso.de) version 260914 (unlicensed)
```

Version mode returned **0** in these observed unlicensed startup cases. That
status should not be interpreted as confirmation of successful license discovery.

Reproduction is intermittent, not guaranteed within 256 attempts. Argument and
environment layout may affect its occurrence because the native routine examines
uninitialized stack memory. The example launches the native executable directly.

## Observed failures

### 1. License discovery differs between launches

A captured standalone `lasheight64 -epsg 2959 -version` trace contains the
following sequence, abbreviated only to remove timestamps and process IDs:

```text
readlink("/proc/self/exe", "/home/ptompals/opt/lastools/bin/lasheight64", 4096) = 43
openat(AT_FDCWD, "/home/ptompals/opt/lastools/bin/lasheight64/lastoolslicense.txt", O_RDONLY) = -1 ENOTDIR (Not a directory)
readlink("/proc/self/exe", "/home/ptompals/opt/lastools/bin/lasheight64", 4096) = 43
openat(AT_FDCWD, "/home/ptompals/opt/lastools/bin/serf/geo/pcs.csv", O_RDONLY) = 3
```

Thus the executable path returned by the OS is correct. License lookup is
malformed, while projection-resource lookup in the same process is correct.
Other launches successfully open `bin/lastoolslicense.txt` and report
`(non-profit)`, without any license-file changes.

### 2. EPSG lookup fails because the executable is treated as a directory

`lascanopy64` can fail with status **3** and the following error:

```text
WARNING: cannot open [/home/ptompals/opt/lastools/bin/lascanopy64/serf/geo/pcs.csv]. please check your installation.
ERROR: unknown EPSG code in '-epsg 2959'.
```

The exact error was independently reproduced during an argument-shaped startup
probe. Its trace shows:

```text
readlink("/proc/self/exe", "/home/ptompals/opt/lastools/bin/lascanopy64", 4096) = 43
openat(AT_FDCWD, "/home/ptompals/opt/lastools/bin/lastoolslicense.txt", O_RDONLY) = 3
readlink("/proc/self/exe", "/home/ptompals/opt/lastools/bin/lascanopy64", 4096) = 43
openat(AT_FDCWD, "/home/ptompals/opt/lastools/bin/lascanopy64/serf/geo/pcs.csv", O_RDONLY) = -1 ENOTDIR (Not a directory)
exit_group(3)
```

The correctly located CSV is readable and contains EPSG 2959. This trace
demonstrates a path-construction failure, not an invalid EPSG identifier.

**Probe limitation:** the argument-shaped startup set appended `-version` and
used a nonexistent diagnostic output directory. Most controls therefore exited
3 on output-directory validation. The quoted EPSG reproduction failed earlier
with the exact resource error above; its trace must not be conflated with those
control errors. No LiDAR input was opened in that probe set. Minimal
`-epsg 2959 -version` probes reproduced license mislookups but did not reproduce
the EPSG failure.

### 3. Related BLAST helper-path symptom

The related malformed helper-path symptom is:

```text
/home/ptompals/opt/lastools/bin/blast2dem64/serf/blast
```

The examined `blast2dem64` contains the same defective executable-directory
routine, and its main function uses that routine when constructing `serf/blast`.
This strongly connects the earlier symptom to the same defect, but the exact
BLAST failure was not independently re-triggered. The shared implementation is
evidence of susceptibility, not proof of the cause of every BLAST failure.

## Native implementation evidence

The installed ELF binaries and the public source contain the same problematic
path-handling pattern.

### Public source

The Linux branch of [`exe_path()` in `LASzip/src/mydefs.cpp`](https://github.com/LAStools/LAStools/blob/a6a88da5875f4359b4ef5eec1a989c526b29a89b/LASzip/src/mydefs.cpp#L340-L362)
declares a stack buffer and assigns the result of `readlink()` to `len`:

```cpp
char path[MAX_PATH_LAS];
len = readlink("/proc/self/exe", path, MAX_PATH_LAS);
```

The separator search then starts at `path[len]`:

```cpp
while (len && (path[len] != '\\') && (path[len] != '/')) len--;
path[len] = '\0';
```

The link is pinned to the examined source revision, rather than a moving branch.

### Corresponding binary behavior

The shared `exe_path[abi:cxx11]()` routine:

1. Allocates an uninitialized stack buffer.
2. Calls `readlink("/proc/self/exe", buffer, 4096)`, receiving length `n`.
3. Starts its reverse separator search at `buffer[n]`, rather than the last
   returned byte, `buffer[n - 1]`.
4. Writes a terminating NUL only after finding a separator.

`readlink()` does not append a NUL terminator. Therefore `buffer[n]` has not been
written by that call. If its prior value is `/` or `\`, the routine accepts it as
the separator and retains the entire executable filename as the directory.

For `lascanopy64`, the relevant disassembly addresses are:

| Address | Operation |
|---|---|
| `0x183230` | `exe_path` entry |
| `0x183276` | Call `readlink` |
| `0x18327b` | Copy returned length to `rbx` |
| `0x18329e` | Read `buffer[rbx]`, initially `buffer[n]` |
| `0x1832a4`, `0x183290` | Compare against backslash and slash |
| `0x1832ad` | Write terminating NUL after finding a separator |

The same instruction sequence occurs in `lasheight64` at entry `0x16b930` and
`blast2dem64` at entry `0x155a70`. Addresses identify these examined builds and
should not be assumed to apply to other versions.

GDB observations immediately after `readlink()` showed different unwritten
bytes for different callers in a single process:

```text
n=43, next_byte=0x2d, caller=LASlicense::LASlicense()
n=43, next_byte=0x86, caller=GeoProjectionConverter::open_geo_file(bool, bool)
```

The debugger was used to observe execution, not to alter arguments or license
state. Both callers independently use this helper. This explains how license
and projection lookups can disagree within one launch, and why independent
multicore children need not behave identically. It does not require a shared
license-file race.

### Separate child-exit-status issue

The examined `lasheight64` multicore implementation calls:

```text
waitpid(child_pid, NULL, WNOHANG)
```

Successful two-core traces show the corresponding Linux syscall:

```text
wait4(child_pid, NULL, WNOHANG, NULL)
```

The null status pointer discards the child's exit status. Please verify whether
the parent can therefore report success when a child fails. This concern is
separate from the executable-directory lookup defect.

## Related existing report

[LAStools/LAStools#172](https://github.com/LAStools/LAStools/issues/172) describes
a Windows `lascopcindex64` problem locating projection CSV files. A maintainer
reported a fix in March 2024. It is related background, not a confirmed duplicate
of this Linux issue or proof that the same fix applies.

## Requested vendor action

Please confirm and correct:

1. Linux executable-directory resolution: handle `readlink()` errors and lengths
   correctly, and search only initialized path bytes when extracting the parent
   directory.
2. Multicore error propagation: collect and inspect child termination statuses
   and return failure when a child fails.

Please also advise which other tools/builds share these implementations and
whether an existing release addresses either issue. We are not requesting a
licensing bypass or a demo-mode workaround.

## Suggested attachments

Attach the failing startup's `.trace`, `.stdout`, `.stderr`, and `status.tsv`
files from the demonstration, along with a successful startup for comparison.
The selected syscall tracing records paths, not license-file contents.

Review attachments before sharing. Do not attach the license file, full
environment dumps, or LiDAR inputs.
