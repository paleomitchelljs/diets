## Rows added per source

| script | pairs | sources | predator species | with both masses | individual pairs | dropped rows |
|---|---|---|---|---|---|---|
| 20 Gronlund1970 | 59 | 1 | 1 | 53 | 0 | 0 |
| 21 Nikolov2004 | 22 | 1 | 1 | 21 | 0 | 0 |
| 22 Fearn2001 | 29 | 1 | 1 | 29 | 0 | 0 |
| 30 SquamataBase | 10797 | 1717 | 1232 | 7152 | 5797 | 143 |
| 31 CarniDIET | 23252 | 682 | 100 | 17146 | 0 | 5869 |
| 32 GATEWAy | 4765 | 11 | 124 | 4664 | 0 | 250 |
| 33 Pianka | 518 | 2 | 54 | 0 | 518 | 5 |
| All new | 39442 | NA | 1499 | 29065 | 6315 | 6267 |

## Part 2: prey masses filled

52267 pairs had a predator mass and no prey mass. 39162 now have a prey mass; 13105 remain without one.

| method | level | Freq |
|---|---|---|
| assigned_approximate | class |  3217 |
| assigned_approximate | family |  6272 |
| assigned_approximate | genus |  3534 |
| species_average_database | genus_mean |  2293 |
| assigned_approximate | order |  9598 |
| assigned_approximate | species |   300 |
| species_average_database | species | 13947 |
| assigned_approximate | subfamily |     1 |

Tier change of the filled rows (before → after):

| before | after | Freq |
|---|---|---|
| B_pred_measured_prey_average | B_pred_measured_prey_average |   585 |
| D_both_average | D_both_average | 15648 |
| A_both_measured | E_coarse_or_aggregate |     1 |
| B_pred_measured_prey_average | E_coarse_or_aggregate |  1883 |
| D_both_average | E_coarse_or_aggregate | 20985 |
| E_coarse_or_aggregate | E_coarse_or_aggregate |    60 |

Filled rows by source (top 15):

| source | rows filled |
|---|---|
| Harman2025_tegu | 1431 |
| Obuch2011_OSPa | 1291 |
| Aumann2001_OSP |  367 |
| Obuch2018_OSP |  363 |
| Zerunian1982_OSP |  316 |
| Klare2010_CD |  266 |
| Sidorovich2010a_CD |  260 |
| Lanszki2016_CD |  227 |
| Dawson2007_CD |  213 |
| Souttou2007_OSP |  201 |
| Bodendorfer2006_CD |  200 |
| Melville2015_CD |  198 |
| Sanchez2008_OSP |  189 |
| MottaJunior2004_OSP |  186 |
| SqB1989_1095f2 |  186 |

Rows that remain without a prey mass, by reason:

| reason | rows |
|---|---|
| taxon not found in the species databases or GATEWAy at its rank, family, order or class | 11699 |
| prey is a pooled or class-level placeholder |   630 |
| prey stage 'egg' is not adult; an adult mean would misstate it |   363 |
| prey stage 'juvenile' is not adult; an adult mean would misstate it |   184 |
| prey stage 'larva' is not adult; an adult mean would misstate it |   118 |
| prey stage 'egg_mass' is not adult; an adult mean would misstate it |    40 |
| prey stage 'neonate' is not adult; an adult mean would misstate it |    31 |
| prey stage 'nymph' is not adult; an adult mean would misstate it |    10 |
| prey stage 'Nestling/egg' is not adult; an adult mean would misstate it |     9 |
| prey stage 'hatchling' is not adult; an adult mean would misstate it |     8 |
| prey stage 'pupa' is not adult; an adult mean would misstate it |     8 |
| prey stage 'carrion (juvenile)' is not adult; an adult mean would misstate it |     3 |
| prey stage 'larvae' is not adult; an adult mean would misstate it |     2 |

Of the rows with a taxon not found, by prey rank:

| rank | rows |
|---|---|
| class | 7845 |
| family |  739 |
| genus | 1447 |
| order |  795 |
| species |  630 |
| subfamily |  205 |
| subspecies |    2 |
| tribe |   36 |

Taxa not found, most frequent (top 25 by rows):

| prey taxon | rows |
|---|---|
| Aves | 3611 |
| Mammalia | 2160 |
| Reptilia |  916 |
| Invertebrata |  425 |
| Amphibia |  424 |
| Cricetidae |  259 |
| Serpentes |  196 |
| Actinopterygii |  191 |
| Scorpiones |  106 |
| Haplotaxida |  101 |
| Eulipotyphla |  100 |
| Arvicolinae |   86 |
| Chilopoda |   78 |
| Osteichthyes |   76 |
| Arthropoda |   67 |
| Lacertilia |   66 |
| Scorpionidae |   65 |
| Solifugae |   64 |
| Decapoda |   56 |
| Rana_sp |   56 |
| Crustacea |   52 |
| Invertebrate |   44 |
| Lacerta_sp |   39 |
| Myodes |   32 |
| Cyprinus_carpio |   29 |

## Predator species added, per analysis clade

Counts of distinct predator taxa with at least one extant pair.

| clade | before | after | added |
|---|---|---|---|
| Aves | 238 |  331 |   93 |
| Crocodylia |   9 |    9 |    0 |
| Lacertilia |  51 |   99 |   48 |
| Mammalia | 138 |  192 |   54 |
| Serpentes | 131 | 1253 | 1122 |

## Clade by observation type by tier: species counts, all extant pairs

Cells are species counts after the expansion; the change from before is in brackets. A species can occupy several cells.

| clade | observation type | A | B | C | D | E |
|---|---|---|---|---|---|---|
| Aves | direct_observation |  |  |  | 75 | 60 (+47) |
| Aves | dna_detection |  |  |  | 4 | 1 (+1) |
| Aves | pellet |  |  |  | 168 | 150 (+111) |
| Aves | prey_remains |  |  |  | 77 (-1) | 70 (+65) |
| Aves | regurgitate |  | 15 |  | 6 | 16 |
| Aves | stomach_content |  | 2 |  | 49 (-1) | 45 (+39) |
| Aves | unknown |  |  |  | 99 (+96) | 18 (+13) |
| Crocodylia | direct_observation | 1 |  |  |  |  |
| Crocodylia | stomach_content | 1 | 3 |  | 5 | 5 (+2) |
| Lacertilia | direct_observation | 1 |  |  | 1 (+1) | 3 |
| Lacertilia | regurgitate |  |  |  |  | 1 |
| Lacertilia | scat |  | 1 |  | 2 | 3 (+3) |
| Lacertilia | stomach_content | 4 | 52 (+45) |  | 12 (+6) | 11 (+5) |
| Lacertilia | unknown | 36 (-1) |  |  | 2 (+1) | 1 (+1) |
| Mammalia | direct_observation |  |  |  | 10 (+3) | 7 (+2) |
| Mammalia | dna_detection |  |  |  | 2 |  |
| Mammalia | prey_remains |  |  |  | 17 (+15) | 14 (+14) |
| Mammalia | scat |  |  |  | 108 (+61) | 94 (+72) |
| Mammalia | stomach_content |  |  |  | 48 (+34) | 37 (+26) |
| Mammalia | unknown |  |  |  | 35 (+35) | 112 (+5) |
| Serpentes | direct_observation | 52 (+51) | 105 (+105) | 4 (+4) | 273 (+271) | 97 (+97) |
| Serpentes | regurgitate | 58 (+57) | 99 (+98) | 6 (+6) | 149 (+149) | 103 (+103) |
| Serpentes | scat | 2 (+2) | 10 (+9) |  | 53 (+46) | 33 (+33) |
| Serpentes | stomach_content | 88 (+88) | 201 (+199) | 17 (+16) | 806 (+794) | 614 (+493) |
| Serpentes | unknown |  | 7 (+7) | 1 (+1) | 74 (+74) | 42 (+42) |

## Clade by observation type by tier: species counts, vertebrate prey with both masses

Cells are species counts after the expansion; the change from before is in brackets. A species can occupy several cells.

| clade | observation type | A | B | C | D | E |
|---|---|---|---|---|---|---|
| Aves | direct_observation |  |  |  | 66 (+2) | 54 (+41) |
| Aves | dna_detection |  |  |  | 4 | 1 (+1) |
| Aves | pellet |  |  |  | 159 | 138 (+102) |
| Aves | prey_remains |  |  |  | 76 | 65 (+61) |
| Aves | regurgitate |  | 15 |  | 6 | 16 |
| Aves | stomach_content |  | 2 |  | 45 | 35 (+30) |
| Aves | unknown |  |  |  | 66 (+64) | 6 (+1) |
| Crocodylia | direct_observation | 1 |  |  |  |  |
| Crocodylia | stomach_content | 1 | 1 |  | 1 | 5 (+3) |
| Lacertilia | direct_observation | 1 |  |  | 1 (+1) | 3 |
| Lacertilia | regurgitate |  |  |  |  | 1 |
| Lacertilia | scat |  | 1 (+1) |  | 2 | 2 (+2) |
| Lacertilia | stomach_content | 4 | 5 (+1) |  | 3 (+3) | 10 (+4) |
| Lacertilia | unknown | 35 |  |  |  | 1 (+1) |
| Mammalia | direct_observation |  |  |  | 10 (+3) | 7 (+2) |
| Mammalia | prey_remains |  |  |  | 16 (+14) | 14 (+14) |
| Mammalia | scat |  |  |  | 90 (+47) | 92 (+76) |
| Mammalia | stomach_content |  |  |  | 36 (+22) | 36 (+25) |
| Mammalia | unknown |  |  |  | 30 (+30) | 7 (+7) |
| Serpentes | direct_observation | 49 (+48) | 83 (+83) | 3 (+3) | 166 (+165) | 90 (+90) |
| Serpentes | regurgitate | 54 (+53) | 63 (+63) | 6 (+6) | 87 (+87) | 92 (+92) |
| Serpentes | scat | 2 (+2) | 6 (+5) |  | 25 (+21) | 29 (+29) |
| Serpentes | stomach_content | 81 (+81) | 131 (+130) | 14 (+13) | 365 (+357) | 547 (+456) |
| Serpentes | unknown |  | 7 (+7) | 1 (+1) | 37 (+37) | 38 (+38) |

## Duplicates dropped

882 records in all. Each block is listed once with the number of rows and the db copy that was kept (full row list in `db/migration/dropped_<script>.csv`).

| script | block | rows |
|---|---|---|
| 30_squamatabase | kept Camp1980_DM178 | 41 |
| 30_squamatabase | kept Layloo2017_DM170 | 12 |
| 30_squamatabase | kept Luiselli1996_DM190 | 12 |
| 30_squamatabase | kept Diller1988_DM163 | 10 |
| 30_squamatabase | kept Rudolph2002_DM193 |  6 |
| 30_squamatabase | kept Dolia2018 |  2 |
| 31_carnidiet | [Dowd, J 2012, Northwest Science] / kept Dowd2012_DM191 | 91 |
| 31_carnidiet | [Nowak, S 2005, Acta Theriologica] / kept Nowak2005_DM95 | 82 |
| 31_carnidiet | [Ackerman, B 1984, Journal of Wildlife Management] / kept Ackerman1984_DM24 | 63 |
| 31_carnidiet | [Knopff, K 2010, Journal of Wildlife Management] / kept Knopff2010_DM49 | 46 |
| 31_carnidiet | [Nunez, R 2000, Journal of Zoology] / kept Nunez2000_DM101 | 45 |
| 31_carnidiet | [Zielinski, W 1999, Journal of Mammalogy] / kept Zielinski1999_DM198 | 44 |
| 31_carnidiet | [Ross, S 2010, Journal of Mammalogy] / kept Ross2010_DM157 | 42 |
| 31_carnidiet | [Hidalgo-Mihart, M 2001, The American Midland Naturalist] / kept HidalgoMihart2001_DM139 | 41 |
| 31_carnidiet | [McDonald, R 2000, Journal of Zoology] / kept McDonald2000_DM151 | 41 |
| 31_carnidiet | [Bianchi, R 2011, Zoologia] / kept Bianchi2011_DM205 | 31 |
| 31_carnidiet | [Chetri, M 2017, PloS one] / kept Chetri2017_DM158 | 26 |
| 31_carnidiet | [Bowland, J 1993, South African Journal of Zoology] / kept Bowland1993_DM169 | 25 |
| 31_carnidiet | [Bagchi, S 2003, Journal of Zoology] / kept Bagchi2003_DM141 | 24 |
| 31_carnidiet | [Sovada, M 1999, The American midland naturalist] / kept Sovada1999_DM2 | 23 |
| 31_carnidiet | [Elmeros, M 2006, Acta Theriologica] / kept Elmeros2006_DM182 | 22 |
| 31_carnidiet | [Rau, J 2002, Studies on Neotropical Fauna and Environment] / kept Rau2002_DM143 | 22 |
| 31_carnidiet | [Lofroth, E 2007, Wildlife Biology] / kept Lofroth2007_DM8 | 20 |
| 31_carnidiet | [Kamler, J 2012, Journal of Mammalogy] / kept Kamler2012_DM168 | 18 |
| 31_carnidiet | [Raine, R 1987, Canadian Journal of Zoology] / kept Raine1987_DM9 | 18 |
| 31_carnidiet | [Wang, H 2003, Mammalia] / kept Wang2003_DM161 | 18 |
| 31_carnidiet | [Gazzola, B 2005, Italian Journal of Zoology] / kept Gazzola2005_DM94 | 17 |
| 31_carnidiet | [Zielinski, W 2004, Journal of Mammalogy] / kept Zielinski2004_DM196 | 16 |
| 31_carnidiet | [Aranda, M. 1996, Studies on Neotropical Fauna and Environment] / kept Aranda1996_DM100 | 15 |
| 31_carnidiet | [Gormezano, L 2013, Ecology and Evolution] / kept Gormezano2013_DM145 |  9 |

## Part 1 ledger: outcome by file

| outcome | files |
|---|---|
| already_in_db |  25 |
| documentation |   2 |
| ingested |   4 |
| not_a_source |   3 |
| not_extracted | 171 |
| nothing |   6 |

