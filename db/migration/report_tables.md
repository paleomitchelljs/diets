## counts_by_source
| script | input | rows_in | unit_in | note_in | dropped | sources_out | pairs_out | measurements_out | pairs_both_masses | individual_pairs |
|---|---|---|---|---|---|---|---|---|---|---|
| 00_merged_sources | merged: | 63 | merged publications in evidence/duplicate_studies.csv |  | 0 | 63 | 0 | 0 | 0 | 0 |
| 01_rawdata | data/raw_data.csv | 517 | predator records (rows 4+ of raw_data.csv) | 7683 record x prey cells with count > 0; longform.csv has 171406 item rows | 315 | 143 | 7369 | 156 | 6000 | 77 |
| 02_aviandietdb | data/structured/aviandiet_gut_ppmr.csv | 214 | rows of aviandiet_gut_ppmr.csv |  | 0 | 14 | 214 | 0 | 214 | 3 |
| 03_extant_new_observations | data/extant_new_observations.csv | 251 | rows of extant_new_observations.csv |  | 7 | 79 | 244 | 176 | 188 | 92 |
| 04_raptordiets | data/structured/raptor_diets_long.csv | 58533 | rows of raptor_diets_long.csv |  | 3043 | 1048 | 55490 | 0 | 38092 | 0 |
| 05_tucker2016 | data/structured/tucker2016_mammal_ppmr.csv | 107 | rows of tucker2016_mammal_ppmr.csv | x13 already dropped 7 of 114 species that lack a prey mass | 0 | 1 | 107 | 0 | 107 | 0 |
| 06_jekyll | data/structured/gator_jekyll_stomach.csv | 69 | rows of gator_jekyll_stomach.csv (alligator x prey category) | 27 alligators | 4 | 1 | 65 | 119 | 0 | 65 |
| 07_glaudas2019 | data/structured/glaudas2019_snake_rpm.csv | 115 | rows of glaudas2019_snake_rpm.csv (one per snake species) |  | 0 | 1 | 115 | 115 | 115 | 0 |
| 08_tegu | data/structured/tegu_diet_ | 9625 | rows of tegu_diet_dietcategories.csv (275 tegus x 35 categories) | 2414 with Found = 1 | 647 | 1 | 1767 | 273 | 0 | 1767 |
| 09_varanus | data/structured/varanus_ | 95 | prey taxa in the two Varanus tables (88 Mazzotti Table 2 rows + 7 Weavers top prey) |  | 0 | 1 | 95 | 0 | 0 | 0 |

## tier_by_source (all pairs)
| script | A_both_measured | B_pred_measured_prey_average | C_prey_measured_pred_average | D_both_average | E_coarse_or_aggregate |
|---|---|---|---|---|---|
| 01_rawdata | 154 | 145 | 5 | 6345 | 720 |
| 02_aviandietdb | 0 | 0 | 0 | 214 | 0 |
| 03_extant_new_observations | 69 | 24 | 0 | 53 | 98 |
| 04_raptordiets | 0 | 0 | 0 | 55490 | 0 |
| 05_tucker2016 | 0 | 0 | 0 | 0 | 107 |
| 06_jekyll | 0 | 65 | 0 | 0 | 0 |
| 07_glaudas2019 | 0 | 0 | 0 | 0 | 115 |
| 08_tegu | 0 | 1767 | 0 | 0 | 0 |
| 09_varanus | 0 | 0 | 0 | 95 | 0 |

## tier_by_source (vertebrate prey, both masses)
| script | A_both_measured | B_pred_measured_prey_average | C_prey_measured_pred_average | D_both_average | E_coarse_or_aggregate |
|---|---|---|---|---|---|
| 01_rawdata | 56 | 24 | 5 | 4882 | 429 |
| 02_aviandietdb | 0 | 0 | 0 | 214 | 0 |
| 03_extant_new_observations | 67 | 17 | 0 | 6 | 96 |
| 04_raptordiets | 0 | 0 | 0 | 38092 | 0 |
| 07_glaudas2019 | 0 | 0 | 0 | 0 | 85 |

## pred method
| pred_mass_method | 01_rawdata | 02_aviandietdb | 03_extant_new_observations | 04_raptordiets | 05_tucker2016 | 06_jekyll | 07_glaudas2019 | 08_tegu | 09_varanus |
|---|---|---|---|---|---|---|---|---|---|
| (no mass) | 51 | 0 | 49 | 154 | 0 | 26 | 0 | 3 | 95 |
| assigned_approximate | 0 | 0 | 13 | 0 | 0 | 0 | 0 | 0 | 0 |
| measured | 468 | 0 | 59 | 0 | 0 | 20 | 0 | 1764 | 0 |
| regressed_from_bone | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| regressed_from_length | 0 | 0 | 68 | 0 | 0 | 19 | 115 | 0 | 0 |
| size_class_midpoint | 75 | 0 | 42 | 0 | 0 | 0 | 0 | 0 | 0 |
| species_average_database | 1967 | 214 | 9 | 55336 | 0 | 0 | 0 | 0 | 0 |
| species_average_literature | 4775 | 0 | 0 | 0 | 107 | 0 | 0 | 0 | 0 |
| unknown | 33 | 0 | 4 | 0 | 0 | 0 | 0 | 0 | 0 |

## prey method
| prey_mass_method | 01_rawdata | 02_aviandietdb | 03_extant_new_observations | 04_raptordiets | 05_tucker2016 | 06_jekyll | 07_glaudas2019 | 08_tegu | 09_varanus |
|---|---|---|---|---|---|---|---|---|---|
| (no mass) | 1345 | 0 | 56 | 17354 | 0 | 65 | 0 | 1767 | 95 |
| assigned_approximate | 190 | 0 | 60 | 0 | 0 | 0 | 0 | 0 | 0 |
| measured | 8 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| ratio_derived | 0 | 0 | 0 | 0 | 0 | 0 | 115 | 0 | 0 |
| reconstructed_from_remains | 151 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| regressed_from_bone | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| regressed_from_length | 0 | 0 | 82 | 0 | 0 | 0 | 0 | 0 | 0 |
| species_average_database | 928 | 214 | 8 | 38136 | 0 | 0 | 0 | 0 | 0 |
| species_average_literature | 4325 | 0 | 2 | 0 | 107 | 0 | 0 | 0 | 0 |
| unknown | 422 | 0 | 36 | 0 | 0 | 0 | 0 | 0 | 0 |

## observation type
| observation_type | 01_rawdata | 02_aviandietdb | 03_extant_new_observations | 04_raptordiets | 05_tucker2016 | 06_jekyll | 07_glaudas2019 | 08_tegu | 09_varanus |
|---|---|---|---|---|---|---|---|---|---|
| bite_trace | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| combat_association | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| direct_observation | 998 | 0 | 10 | 2552 | 0 | 0 | 0 | 0 | 0 |
| dna_detection | 0 | 0 | 10 | 110 | 0 | 0 | 0 | 0 | 0 |
| pellet | 3031 | 0 | 1 | 44548 | 0 | 0 | 0 | 0 | 0 |
| prey_remains | 166 | 0 | 2 | 6774 | 0 | 0 | 0 | 0 | 0 |
| regurgitate | 1 | 101 | 35 | 0 | 0 | 0 | 0 | 0 | 0 |
| scat | 1650 | 0 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| stomach_content | 1523 | 113 | 113 | 1441 | 0 | 65 | 115 | 1767 | 88 |
| unknown | 0 | 0 | 72 | 65 | 107 | 0 | 0 | 0 | 7 |

## dropped by reason
| script | source_file | reason | rows |
|---|---|---|---|
| 01_rawdata | data/raw_data.csv | duplicate entry within raw_data.csv: the same records are entered under another RefNo, which is kept (evidence/rawdata_study_decisions.csv) | 21 |
| 03_extant_new_observations | data/extant_new_observations.csv | duplicate of ref_id 97: the same Tropidurus hispidus x T. jaguaribanus predation (Passos et al. 2016), entered once from the paper (ref 97, kept) and once via Oliveira et al. 2025 Table S2 row 79 | 1 |
| 04_raptordiets | data/structured/raptor_diets_long.csv | duplicate record: the same publication, predator species and evidence kind is kept from another upstream table (evidence/duplicate_records.csv) | 3043 |
| 01_rawdata | data/raw_data.csv | duplicate record: the same publication, predator species and evidence kind is kept from AvianDietDB (evidence/duplicate_records.csv) | 25 |
| 03_extant_new_observations | data/extant_new_observations.csv | no prey taxon recorded (row states the predator's diet breadth only; species detail in Data S1 not extracted) | 6 |
| 01_rawdata | data/raw_data.csv | non-animal food item (prey class string is plant, fungus, placeholder or unclassified); not a predator-prey link | 268 |
| 08_tegu | data/structured/tegu_diet_dietcategories.csv | non-animal or man-made item (fruit, vegetation, seeds, man-made material); not a predator-prey link | 647 |
| 01_rawdata | data/raw_data.csv | record has no prey count > 0 in any prey column | 1 |
| 06_jekyll | data/structured/gator_jekyll_stomach.csv | seeds are not prey (non-animal food item) | 4 |

## unknown by source
| source_id | side | pairs | basis |
|---|---|---|---|
| Rice2004_DM146 | prey | 91 | mixed: live mass of fresh prey estimated from same-size field data, museum specimens and three reference books; raw_data value is a lake-level mean |
| Radloff_DM93 | prey | 60 | study-specific value for 'Aepyceros_melampus' that differs between records of this study; not found in the source text |
| Dryden1985_DM186 | prey | 48 | prey mass method not stated |
| Vitt1992_DM122 | prey | 37 | paper estimated prey volumes (prolate spheroid), not masses; conversion used in raw_data not recorded |
| Delany1986_DM126 | prey | 30 | paper reports volumetric stomach measurements; origin of the per-taxon masses in raw_data not found |
| Angelici2000_DM136 | prey | 28 | no statement of prey mass method found in the text layer |
| Itamies1972 | predator | 27 | one of 5 different masses for Pernis apivorus within this study; not found in the source text |
| Shine1998_DM120 | prey | 27 | paper reports prey mass summaries; how each value in raw_data was obtained is not stated |
| Delany1990_DM124 | prey | 26 | paper reports stomach contents volumes (ml); conversion to mass not recorded |
| Korpimaki1987 | prey | 24 | study-specific value for 'Indet' that differs between records of this study; source PDF not on disk or image-only |
| Poulin2001 | prey | 16 | staged 0.3 g from mean SVL 14.5 mm by an unrecorded equation; no frog length-mass equation is registered |
| Sutter2001_DM189 | prey | 10 | study-specific value for 'Indet' that differs between records of this study; source PDF not on disk or image-only |
| Olsen2013a | prey | 8 | study-specific value for 'Cyprinus_carpio' that differs between records of this study; source PDF not on disk or image-only |
| CapizziLuiselli1997 | prey | 7 | Table 2 biomass / count; biomass 'calculated on the basis of the fresh weight if the prey was in optimal condition, or' otherwise estimated: weighed and estimated items mixed |
| Wiseman2019 | prey | 7 | Table 2 biomass / frequency; intact prey were weighed but 20 partly digested prey were estimated, so the per-item mean mixes both |
| Luiselli1996_DM190 | predator | 6 | one of 3 different masses for Vipera ammodytes within this study; not found in the source text |
| Diller1988_DM163 | prey | 6 | intact prey were weighed in part; which raw_data values come from weighing is not stated |
| Rasoloarison1995_DM133 | prey | 5 | study-specific value for 'indet_large' that differs between records of this study; not found in the source text |
| Begg2006_DM199 | prey | 3 | study-specific value for 'Indet' (coarse label); not found in the source text |
| Mendelsohn1989_DM86 | prey | 3 | study-specific value for 'Dendromus/Steatomys' (coarse label); source PDF not on disk or image-only |
| Auffenberg1981 | both | 2 | stated in the staged row; method not checkable |
| Kasperoviczus2015 | both | 2 | mass estimated at ingest 'from length'; prey length not recorded |
| Brzezinski2010_DM162 | prey | 2 | study-specific value for 'Microtus_arvalis' that differs between records of this study; not found in the source text |
| Cui2008 | prey | 2 | study-specific value for 'Indet' (coarse label); not found in the source text |
| Nunez2000_DM101 | prey | 2 | study-specific value for 'Indet' that differs between records of this study; source PDF not on disk or image-only |
| ValladaresFaundez2015_DM195 | prey | 2 | study-specific value for 'Indet' (coarse label); not found in the source text |
| Bowland1993_DM169 | prey | 1 | study-specific value for 'Indet' that differs between records of this study; not found in the source text |
| Brown1965_DM87 | prey | 1 | study-specific value for 'Tachyoryctes_spp_massofsplendens' (coarse label); not found in the source text |
| Demeter1979_DM63 | prey | 1 | study-specific value for 'Indet' (coarse label); not found in the source text |
| Kamler2012_DM168 | prey | 1 | study-specific value for 'Civets' (coarse label); not found in the source text |
| Kasperoviczus2015 | prey | 1 | mass estimated at ingest 'from length'; prey length not recorded |
| Korpimaki1985 | prey | 1 | study-specific value for 'Mouse' (coarse label); not found in the source text |
| McNabb2002_DM68 | prey | 1 | study-specific value for 'Small bird' (coarse label); source PDF not on disk or image-only |
| Olsen2013b | prey | 1 | study-specific value for 'Indet' (coarse label); not found in the source text |
| Pande2011 | prey | 1 | study-specific value for 'Sunbird' (coarse label); not found in the source text |
| Weavers1989 | prey | 1 | 'a 500 g rabbit' as stated; whether weighed or estimated is not said |
