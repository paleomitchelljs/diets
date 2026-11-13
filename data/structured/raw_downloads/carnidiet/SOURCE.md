# CarniDIET 1.0 (diets of terrestrial carnivorous mammals)

Downloaded 2026-11-11 by the source-intake session. Files are stored as downloaded.

| file | source | sha-256 | note |
|---|---|---|---|
| `CarniDIET_1.0_github-0301492.csv` | author's GitHub repo https://github.com/osmiddleton/GEB-2021-CarniDIET_database, commit 0301492b944ad1ecf7e17199c96fa99a33d6ef91, path `Version 1.0/CarniDIET 1.0.csv` | 4b3fca67eab47d121f0b19ed8f8a1b677863f8c07bd426205626cc972525dec7 | 24,848,622 bytes, 29,122 lines (LF) |
| `CarniDIET_1.0_Metadata.xlsx` | same repo, `Version 1.0/CarniDIET 1.0_Metadata.xlsx` | 6642e290eaaa8e61ef6fbfd7c87bf4a29a10481fc1c0f73851bacb5c07bb085d | byte-identical to the Dryad file |
| `README_github.md` | same repo, `README.md` | (not recorded) | version notes |

- Dataset of record: Dryad, doi:10.5061/dryad.2v6wwpzmr, version 2, published 2022-03-16, licence CC0 1.0.
- **The CSV here is NOT byte-identical to the Dryad v2 CSV.** Dryad lists `CarniDIET_1.0.csv` at 24,877,992 bytes, sha-256 5cc6a28341881a31adbfa56100075e339d9be42f5d0cf76a6363d20ccc23fafa. Converting the GitHub file to CRLF line endings gives 24,877,744 bytes and a different hash, so the content differs slightly. Dryad v2 may include corrections made after the GitHub commit. Dryad's API now requires a logged-in bearer token to download, so that copy was not fetched. See `docs/SOURCE_REQUESTS_FOR_JM.md`.
- The GitHub repo has no LICENSE file. The same data are CC0 on Dryad.
- Not fetched: `Scripts/Middleton et al_CarniDIET 1.0_GEB.R` (analysis script) and `Version 1.0/Supplementary data/` (data validation and potential species list).
- Paper: Middleton O, Svensson H, Scharlemann JPW, Faurby S, Sandom C (2021) CarniDIET 1.0: a database of terrestrial carnivorous mammal diets. Glob Ecol Biogeogr 30(6):1175-1182. doi:10.1111/geb.13296.

## Decision 2026-11-11

JM chose the GitHub CSV as the working copy. The Dryad v2 file will not be fetched.

Commit dates (GitHub API, checked 2026-11-11): the CSV was last changed in commit `ddafdc8` on 2021-04-06, "replace \xa0 (non-breaking space) with regular whitespace". The commit stored here (`0301492`, 2021-04-10) only removed an old script. Dryad v2 is dated 2022-03-16, so by date the Dryad file is later. But the Dryad CSV is 248 bytes larger than the CRLF-converted GitHub CSV. That is what you would expect if the Dryad file still holds about 248 non-breaking spaces (2 bytes each in UTF-8) that the GitHub commit replaced with ordinary spaces (1 byte). Not verified. If that is the cause, the two files hold the same records and the GitHub copy is the cleaner of the two.
