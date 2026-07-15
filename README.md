# CProg2 (merged into tina)

This package has been merged into [`tina`](https://github.com/mcruer/tina) and
is no longer maintained as a separate package. All of its functions
(`monthly()`, `events()`, `pt()`, `frank()`, `add_description_e()`/
`add_description_f()`, `school_year()`, `fiscal_year()`, and the rest) now
live directly in `tina` and are exported from there.

CProg2 and tina served the same reporting ecosystem, and tina was CProg2's
only consumer, so there was no reason to keep them split into two packages.

This repository is kept around for historical reference. New work should go
to [`mcruer/tina`](https://github.com/mcruer/tina) instead.
