# External datasets to obtain (sandbox-off / manual fetch)

The Claude sandbox has no network and lacks `rotl`/`rfishbase`/`taxize`, so these
must be fetched manually. Drop files where noted; the pipeline hooks are ready.

## Mass sources (to fill prey/predator masses; wire into x03)
- **AVONET** (Tobias et al. 2022) — body mass for ~all birds. Fills *Indet Aves*, raptor-diet prey, etc. → `data/mass_references/avonet.csv`
- **FishBase** length–weight (a,b) coefficients (`rfishbase`) — fish prey (Acrochordus/filesnake fish prey, Platt croc fish, RaptorDiets fish). → `data/mass_references/fishbase_lw.csv`
- **Meiri 2010 / Feldman et al. 2016** squamate mass or SVL→mass — lizard/snake masses. → `data/mass_references/squamate_mass.csv`
- **AmphiBIO** (Oliveira et al. 2017) — amphibian masses.
- **EltonTraits / Smith et al.** mammal mass — supplements PanTHERIA (more complete; also replaces the malformed local `data/pantheria`).

## Diet-composition databases (need a mass source to become PPMR)
- **Avian Diet Database** (Hurlbert et al. 2021, Sci Data 8:260) — **73,075 quantitative diet records for 759 bird species** (mostly N. American). Trophic link per record + metadata (year/season/location/habitat/Study_Type); diet as fraction-by-number / fraction-by-weight / %-occurrence. Superset of the RaptorDiets slice. Fetch: Zenodo doi:10.5281/zenodo.5523102, GitHub `hurlbertlab/dietdatabase`, R pkg `aviandietdb`. Needs AVONET (bird masses) + PanTHERIA/etc (prey masses) to yield PPMR; mostly mixed-method (not gut-specific) — carry `Study_Type` as obstype.

## Predator–prey mass COMPILATIONS (mammal side is thin; high value)
- **Tucker, Ord & Rogers 2016** (J Evol Biol 29:2181) online supplement — Appendix S1, Figs S3–S4: prey+predator mass for **107 carnivorous mammals** (51 terrestrial, 56 marine). Key patterns: terrestrial break ~11 kg; large terr carnivores eat prey ~45% body mass (logPPMR ~ -0.8); small terr <2 kg prey; marine small prey. → `data/mass_references/tucker2016_mammal_ppmr.csv`
- **Carbone et al. 1999** (the foundational mammalian-carnivore predator–prey mass dataset that Tucker builds on) + **Tucker & Rogers 2014b**.
- (Consider) **Brose et al. 2019 / GlobAL predator–prey** body-size databases for a broad multi-clade backbone.

## Phylogeny (P3 — replace the Grafen taxonomy placeholder)
- Time-calibrated tree for the 167 extant predators: **BirdTree** (Jetz et al.), **Upham et al. 2019 / PHYLACINE** (mammals), **Tonini et al. 2016** (squamates), a crocodylian tree; or an **OpenTree/VertLife** synthesis. Graft fossil tips as dated terminals. Drop a Newick at `data/tree.tre` (x02 picks it up automatically).

## Digestion / preservation calibration (to ground g_mode(size); see observation_bias_notes.md)
- Actualistic-digestion / feeding-trial / method-comparison datasets that relate preserved/identifiable prey size to true prey size, per observation mode (gut, scat, pellet, regurgitate). Would let the size-dependent detection/preservation filter be an informative PRIOR (or a mechanical likelihood layer) rather than an assumption.

## Fossil
- **Wilson et al. 2010 PLoS Biol** (Sanajeh) PDF → `pdfs/fossil_cases/` so the ref_id 15 masses can be extracted instead of left PENDING.
