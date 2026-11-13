# ai_found intake — 2026-06-02 batch (7 files)

**Bottom line: 0 new PPMR-usable datapoints** (the prey~predator fit needs paired INDIVIDUAL predator+prey
masses). All seven are detection / frequency-of-occurrence / methods data — high value for the
OBSERVATION-PROCESS (obstype / g_mode) model, not for the mass-scaling fit.

| File | Source | Predator | Data type | PPMR | Disposition |
|---|---|---|---|--:|---|
| Thalinger_et_al-2015_MolEcolResources.pdf (+ "(1)" duplicate) | Thalinger et al. 2015, Mol. Ecol. Resources | kingfisher / cormorant | molecular diet-detection METHODS | 0 | DNA-method / obstype ref (dup ignored) |
| kingfisher+and+cormorant+molecular+screening.xlsx | (Thalinger system) | *Alcedo* (kingfisher), *Phalacrocorax* (cormorant) | DNA presence/absence per fish taxon | 0 | occurrence only: 97 samples (kingfisher feces 7, cormorant pellets 45, cormorant feces 45), **260 positive prey-detections**; predator-mass/breadth ref |
| Eurasian+otter+feeding+trial+results.xlsx | otter spraint feeding trial | *Lutra lutra* | ACTUALISTIC: known diet → recovery via DNA (105 spraints) + morphology (35) | 0 | **g_scat calibration** (true diet vs detected) — observation-process |
| Evidence of lizard predation by NZ kingfishers (van Winkel & Ji 2012, NZ J Zool 39:201) | — | *Todiramphus sanctus vagans* | regurgitated-pellet OCCURRENCE (lizard *Oligosoma moco* in 88% / 43% of pellets) | 0 (1 borderline) | breadth; one possible occurrence-derived pellet point (kingfisher ~65 g → *O. moco*) |
| 068.035.0207.pdf (Kok, Kok & Van Ee 2000, Acta Ornithol. 35:147) | — | Lesser Kestrel *Falco naumanni* | stomach, INSECTIVORE (94.5% inverts), aggregate g per taxon over 386 birds | 0 | predator-mass + breadth ref |
| Actaorn_2003_food_PT.pdf (Tryjanowski, Karg & Karg 2003, Acta Ornithol. 38:59) | — | Red-backed Shrike *Lanius collurio* | 3-method comparison (collar/pellet/larder), 98.9% inverts, % occurrence | 0 | method-bias ref (pellets over-rep. Coleoptera; larders find large prey); breadth |

## Totals from this batch
- **PPMR datapoints: 0** (1 borderline: NZ kingfisher → *Oligosoma moco*, occurrence-derived, not added per the occurrence rule).
- Occurrence / detection records: **~260** (DNA screening) + the insectivore frequency-of-occurrence diets.
- Method / actualistic-calibration datasets: **3** (Thalinger DNA-vs-morphology; otter feeding trial = 140 spraints; Tryjanowski 3-method) → feed the obstype / g_mode observation-process model.
- Predator-mass + diet-breadth references: Lesser Kestrel, kingfisher, cormorant, NZ kingfisher.

## Why no PPMR points
The fit needs paired INDIVIDUAL masses. These supply either (a) DNA presence/absence (no mass), (b) % frequency-of-occurrence (no per-prey mass), or (c) aggregate grams-per-taxon (not per individual) — and all are invertebrate-dominated or detection-only.

## Where they ARE valuable
The **otter feeding trial** is precisely the actualistic g_scat calibration the project wants (`external_data_wanted.md`): known diet vs DNA/bone recovery, i.e. the size-dependent detection function for scat. **Thalinger** (DNA vs morphology) and **Tryjanowski** (collar vs pellet vs larder) directly quantify obstype/method bias. These should be registered in `actualistic_studies` / `observation_bias_notes` — not the PPMR pipeline.

---

# ai_found/lizards/ batch (9 files) — saurophagy / lizard diet

Unlike the birds, these are **saurophagous** lizards, so they yield real vertebrate PPMR points.

| File | Source | Yield |
|---|---|---|
| 123234 collared (Paulissen & Montgomery, Colorado) | SW Naturalist 2024 | **1 clean pt**: *Crotaphytus collaris* → 2 juv *Sceloporus* (8500 mm³) [ref96] |
| tropidurus (Passos et al. 2016) | Biota Neotropica 16:e20150062 | **1 clean pt**: *T. hispidus* (SVL112.5) → *T. jaguaribanus* (SVL76.4); PPMR −1.16 [ref97] |
| Best & Pfaffenberger 1987 (collared) | SW Nat 32:415 | **4 occurrence pts**: collared → *Cnemidophorus*, *Phrynosoma*, *Sonora* snake, bird nestling [ref98–101] |
| BNLLFoodHabits (*Gambelia sila*) | — | lizard prey in scats (occurrence, prey unID) → breadth only |
| McAllister 1985 (collared) | SW Nat 30:597 | predominantly insects; **predator-mass reference** (body weights by SVL class) |
| Blair & Blair 1941 (collared, OK) | Am Midl Nat 26:230 | insectivore (no lizard prey) → breadth |
| Sluys 1993 (*Tropidurus itambere*) | — | insectivore → breadth |
| Parker & Pianka 1976 (leopard *C. wislizeni*) | Herpetologica 32:95 | saurophagous but diet as % volume by taxon → breadth (no individual masses) |
| **s41598-025-89810-7 (Sci Rep 2025)** | saurophagy meta-analysis | **127 predator–prey records, 47 spp** — but data is in **Supplementary Table S1, NOT in the folder**. Add the supp → ~127 Lacertilia saurophagy points (body sizes → mass). |

**Lizard batch yield: 6 new PPMR points (ref96–101; 2 high-confidence + 4 occurrence-derived).** Big locked opportunity = the Sci Rep supp (127 records).

---

# ai_found additions — Poulin, Martin & the Sci Rep supplement (the goldmines)

| Source | Predator→prey | New PPMR rows | Confidence |
|---|---|--:|---|
| **Oliveira et al. 2025 Sci Rep, Suppl. Table S1** (`MOESM1_ESM.docx`) | Lacertilia → Lacertilia (saurophagy/cannibalism) | **68** | solid (predator+prey SVL → mass; PPMR=3·ln(SVL ratio)) |
| **Poulin et al. 2001 J Trop Ecol** | Aves → *Anolis* / *Eleutherodactylus* (15 liz + 16 frog) | **31** | moderate (occurrence; prey=population-mean SVL) |
| Best & Pfaffenberger 1987 | *Crotaphytus* → whiptail/horned-lizard/snake/nestling | 4 | occurrence-aggregate |
| Martín & López 1996 | large raptors → *Lacerta lepida* (Circaetus, Aquila, Bubo…) | 5 | LOW (occurrence-only; prey size ESTIMATED ~150–200 g) |
| Passos et al. 2016 (tropidurus) | *T. hispidus* → *T. jaguaribanus* | 1 | solid (both SVL) |
| Paulissen & Montgomery (Colorado collared) | *Crotaphytus* → juv *Sceloporus* | 1 | solid (prey volume) |
| van Winkel & Ji 2012 (NZ kingfisher) | *Todiramphus* → *Oligosoma moco* | 1 | moderate (occurrence) |

**Today's total: 111 new extant PPMR records** (ref 95–205; file 94→205 rows). ~70 with measured prey sizes (Sci Rep 68 + tropidurus + Colorado + most Poulin), ~41 occurrence/estimated.
NOTE: Sci Rep SVL→mass uses the squamate relation (robust teiids' absolute mass may be underestimated, but PPMR = 3·ln(SVL ratio) is exact). These are mostly **Lacertilia saurophagy** + **Aves vertebrate-prey** — both thin cells before. Next x20 run will incorporate them (will materially shift Lacertilia & Aves).

---

# pdfs/extant_to_ingest/toadd/herps batch (29 files) — 2026-06-04

**Net new: 21 species-level vertebrate-prey records, all Crocodylia (ref_id 206-226)** + new `prey_stage` column added to extant_new_observations.csv.

## Integrated
- **27206** Dutra-Araujo etal 2017 (HerpNotes 10:169) — *Paleosuchus palpebrosus* (2.8 kg, weighed) -> *Atractus torquatus* snake (adult, 48cm TL). Clean per-event. **NEW croc genus** (model had only Alligator/Caiman/Melanosuchus/Crocodylus). obstype=Follow. cert 0.7. [ref 206]
- **Delany etal 1999** (SEAFWA 53:375) — *Alligator mississippiensis* (FL, 1.09-3.89m TL) Stomach %-volume -> 12 vert taxa: Dorosoma, Amia calva, Tilapia aurea, Lepisosteus, Lepomis, Micropterus salmoides, Pseudemys nelsoni, Apalone ferox, Trachemys scripta, Nerodia, Siren lacertina, Gallinula chloropus. [ref 207-218]
- **Gabrey 2010** (HCB 5:241) — *A. mississippiensis* (LA, adults >1.83m) Stomach freq-occ -> 8 vert taxa: Ictalurus, Amia calva, Atractosteus spatula, Myocastor coypus, Ondatra zibethicus, Regina rigida [ref 219-224], + birds **Strix varia + Gallinula chloropus [ref 225-226]**. Bait = chicken parts (+ occasional blackbirds/grackles), which the author differentiates from ingested birds via shotgun-pellet holes; so Barred Owl + Moorhen are genuine prey. (Initial blanket bird-exclusion was over-aggressive — corrected.)
- Alligator occurrence records: predator at representative study size (~80-100kg adult), prey at species-typical mass, prey_stage unknown, certainty 0.4 (coarse pairing; occurrence/%-volume, prey pooled across gator sizes). obstype=Stomach (matched filter).

## Dedup / skipped
- Within-folder dups: 27206=27206(1); CaimansBrazil1999=Silveira-DietsSpectacledBlack-1999 (Da Silveira & Magnusson 1999).
- Already in DB: Moldowanetal2016 (longform ref 179); Varanus_varius=Weavers (newobs). DB alligator refs = Delany1986(JWM), Delany1990(JHerp), Rice2004 -> folder's Delany1999 + Gabrey2010 are DIFFERENT papers (new).
- NOT species-resolved (category/proportion/isotope -> unusable per species-level rule): Da Silveira1999, CaimansPeru2013, NileCrocodileBotswana2008, Games&Moreau1997 (caiman/Nile diet by category); estuarycrocs(Adame2018) + Hanson2014 (stable-isotope, no prey items).
- Invertebrate-dominated (vert-only drops): Ameiva_exsul, ameivadiet, Plestiodon_Diet, Morato2011 (scorpion), Varanus acanthurus (King; mostly inverts+lizard category).
- Behavioral/carrion/egg (not prey-size gut records): whitingwhiting2011 (C.porosus sea-turtle predation tactics), Tegu_Diet_Brazil (carrion/eggs).
- Well-sampled clades, skipped: Lanius_excubitor x2 (Aves), LeastWeaselII + Cerdocyon (Mammalia).
- No data tables: AllVaranus_NoTables (review).
- **HELD (image-only, 0 text layer): DietsofAmazonianCrocodilians = Magnusson etal 1987 (11pp fax scan, classic 4-species Amazonian croc diet, likely category-level), Tupinambis (4pp scan). Need page image-read/OCR.**

## Reality
Folder is croc/varanid-heavy but, like most diet literature, dominated by category/occurrence/isotope studies that do not resolve prey to species WITH sizes. Yield is modest (19) but lands entirely in the degenerate Crocodylia cell, extending croc predator mass from a near-point at 925g to 2.8kg-100kg.

---

# toadd backlog screen (129 files) — 2026-06-05

Cleared pdfs/extant_to_ingest/toadd (now empty, ready for new drops). Filed to pdfs/_processed/extant_pdf/toadd_2026-06-05/ (see its README).

**Net new: 3 species-level records (all Lacertilia saurophagy), ref 227-229.**
- Kasperoviczus etal 2015 (HerpNotes 8:21) — *Salvator merianae* (tegu) -> *Xenodon neuwiedii* [227], *Chironius bicarinatus* [228], *Amphisbaena alba* [229]. Videotaped predation (obs_type=Observation/Follow); predator TL measured for 2/3; pred+prey masses est from length; PPMR -2.3 to -2.7 (the Lacertilia large-prey/saurophagy tail). cert 0.45-0.55. (Distinct from DB Salvator frugivory ref 167.)

**Screened, not extracted:**
- Herp but unusable: HJ_v16 (Anderson&McMurry, *Crocodylus moreletii* Belize — category by size class); OsteolamusVaranus (Luiselli etal 1999, *Osteolaemus* + *Varanus niloticus* — category/competition); Loso08Varanus (Losos&Greene 1988 monitor diet review — category); Phrynosoma + Bufo (invertivores). DUP: GopherSnakePituophis = Rodriguez-Robles 2002 (already newobs).
- Mammalia (72) + Aves (40) + other (10): well-sampled/saturated cells (104k bird + 32k mammal vert records, subsampled) -> not extracted; many are occurrence/no-prey-mass. Filed by clade for future densifying if needed.

## Session totals (herps folder + toadd backlog)
24 new species-level vert-prey records (ref 206-229): 21 Crocodylia (Paleosuchus + alligator) + 3 Lacertilia (tegu saurophagy). Both batches target the thin/fossil-relevant cells; mammal/bird backlog screened + filed, not extracted (saturated).

---

# toadd batch (7 items) — 2026-11-11

**Net new: 22 Crocodylia records (ref 230-251)** from Nifong 2016 (alligator, Guana River FL): species-resolved
vertebrate prey by size class; predator mass regressed from class-midpoint TL (Chabreck & Joanen 1979: juvenile
1.03 kg, sub-adult 9.03 kg, adult 46.2 kg); prey mass approximate species-typical; certainty 0.4.
Staged, not in fit: Glaudas 2019 snake RPM (115 spp; data/structured/glaudas2019_snake_rpm.csv); black caiman Dryad
(category counts, measured predator mass). Screened: Rosenblatt 2015 (+ESM), Webb 1983 (reference). Duplicate: Jekyll
Dryad (already x09). Filed: pdfs/_processed/extant_pdf/toadd_2026-11-11/ (README there). Model NOT refit.
