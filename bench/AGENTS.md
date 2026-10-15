# bench/

`main.exe`: core_bench of each scoring step on a synthetic leader with about 2000 fills
and 400 daily snapshots. Run `dune exec bench/main.exe -- -quota 2`; record the numbers
with the machine and profile in the README when they change.

See [../AGENTS.md](../AGENTS.md).
