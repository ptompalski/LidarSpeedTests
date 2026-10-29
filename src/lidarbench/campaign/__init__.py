"""Campaign management: identity, deterministic schedules, execution, resume, results.

Modules:

- :mod:`~lidarbench.campaign.schedule` -- ``schedule-v2`` (SplitMix64, balanced or
  randomized tool order);
- :mod:`~lidarbench.campaign.identity` -- frozen identity components and cheap
  per-entry re-checks;
- :mod:`~lidarbench.campaign.store` -- write-once campaign and attempt records;
- :mod:`~lidarbench.campaign.runner` -- create/resume/dry-run and sequential execution;
- :mod:`~lidarbench.campaign.results` -- verified results tables (JSONL/CSV/Parquet);
- :mod:`~lidarbench.campaign.commands` -- CLI command implementations.

See ``design/CAMPAIGNS.md``.
"""
