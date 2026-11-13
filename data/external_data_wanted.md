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

- **Tegu diet/morphometrics DB** (Harman, Yackel Adams, Falk & Romagosa 2025, USGS ScienceBase, **DOI 10.5066/P1CJ29AG**) — *Salvator merianae* (Argentine black-and-white tegu, a large TEIID → **Lacertilia**) **gastrointestinal (GUT) contents + individual morphometrics** for **275 tegus**, Florida 2016–2021; 12 classes/63 orders/126 families of diet items. HIGH VALUE: measured individual predator body masses (gold-standard predator data) + a big gut-contents lizard dataset that strengthens the thin Lacertilia cell. **[OBTAINED + processed 2026-06 via `x23`]** — but the diet is category PRESENCE/ABSENCE (no prey species-counts-masses), so per JM it's **predator-mass + breadth only, NOT PPMR**. `tegu_salvator_summary.csv`: *Salvator merianae* n=274, mean 989 g / median 924 g (range 56–2862), 86% with vertebrate prey, median breadth 9/35 categories. Strengthens the Lacertilia predator-mass reference + the active-forager-generalist foraging mode.
- **Nile monitor diet** (Mazzotti et al. 2020, J Herpetol 54:189, doi 10.1670/18-115) **[OBTAINED + processed]** — *Varanus niloticus*, Florida; same situation (percent-occurrence by design, no prey masses → predator-mass + breadth only). `varanus_niloticus_summary.csv`: mass range 110–12,300 g, n=68 GI tracts, 72% insects, generalist active forager (mammals/birds/fish in adults only); prey-species list (88 taxa) in `pdfs/extant_to_ingest/ai_found/nilemonitor/`. Parallels the tegu; both large invasive FL lizards bolster Lacertilia.

## Predator–prey mass COMPILATIONS (mammal side is thin; high value)
- **Tucker, Ord & Rogers 2016** (J Evol Biol 29:2181) online supplement — Appendix S1, Figs S3–S4: prey+predator mass for **107 carnivorous mammals** (51 terrestrial, 56 marine). Key patterns: terrestrial break ~11 kg; large terr carnivores eat prey ~45% body mass (logPPMR ~ -0.8); small terr <2 kg prey; marine small prey. → `data/mass_references/tucker2016_mammal_ppmr.csv`
- **Carbone et al. 1999** (the foundational mammalian-carnivore predator–prey mass dataset that Tucker builds on) + **Tucker & Rogers 2014b**.
- (Consider) **Brose et al. 2019 / GlobAL predator–prey** body-size databases for a broad multi-clade backbone.

## Phylogeny (P3 — replace the Grafen taxonomy placeholder)
- Time-calibrated tree for the 167 extant predators: **BirdTree** (Jetz et al.), **Upham et al. 2019 / PHYLACINE** (mammals), **Tonini et al. 2016** (squamates), a crocodylian tree; or an **OpenTree/VertLife** synthesis. Graft fossil tips as dated terminals. Drop a Newick at `data/tree.tre` (x02 picks it up automatically).

## Digestion / preservation calibration (to ground g_mode(size); see observation_bias_notes.md + actualistic_studies.{csv,md})
Actualistic-digestion / feeding-trial / method-comparison datasets that relate preserved/identifiable prey size to true prey size, per observation mode (gut, scat, pellet, regurgitate). Would let the size-dependent detection/preservation filter be an informative PRIOR (or a mechanical likelihood layer) rather than an assumption. Registry started in `data/actualistic_studies.csv`; PDFs to pull (→ `pdfs/methods/`), grouped by what they calibrate:
- **g for remains/pellets (method-comparison, camera-as-truth):** Collopy 1983 (golden eagle, USER SEED); Redpath et al. 2001 (hen harrier); Lewis, Fuller & Titus 2004; García-Salgado et al. 2015 (goshawk). → size-resolved over-representation of large prey.
- **g for scat (biomass-per-scat correction vs prey mass):** Floyd, Mech & Jordan 1978 (wolf, foundational regression); Weaver 1993; Klare, Kamler & Macdonald 2011; Trites & Joy 2005 (pinniped/otolith); Wachter et al. 2012 (USER SEED). → turn-key g_scat(size) prior.
- **g for gut (actualistic taphonomy, bone-survival):** Andrews 1990 *Owls, Caves and Fossils*; Fernández-Jalvo & Andrews 1992/2016 *Atlas* (digestion-corrosion stages 0–4 by predator category); Fisher 1981 (alligator bone destruction). → ordinal, predator+size-dependent loss; **directly fossil-applicable**.
- **meal-size tail + ID-window (digestion physiology / SDA):** Secor & Diamond 1995 (python, USER SEED); Secor 2008 (cross-taxon SDA review); McCue 2006. → shape the large-prey meal-size prior + temporal decay of g.

## Fossil
- **Wilson et al. 2010 PLoS Biol** (Sanajeh) PDF → `pdfs/fossil_cases/` so the ref_id 15 masses can be extracted instead of left PENDING.
