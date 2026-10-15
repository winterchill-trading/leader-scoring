# test/

Expect tests and Quickcheck properties of the core library. `gen.ml` generates random
fill histories; `helpers.ml` builds small ones by hand.

- Accept new output with `dune promote` from the workspace root with the same
  `DUNE_BUILD_DIR`, then read the diff.
- Round floats before printing: libm differences between macOS and Linux show in the
  last bits.
- `fixtures/model-parity.csv` is written by `python/train.py`; regenerate it with the model.

See [../AGENTS.md](../AGENTS.md).
