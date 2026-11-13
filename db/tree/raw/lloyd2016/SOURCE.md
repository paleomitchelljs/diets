# Lloyd et al. 2016 dinosaur metatrees

- `graemeTrees.tre`: 100 undated metatrees, 962 tips (polytomies kept).
- `occurrence_times.txt`: first and last appearance (Ma) per tip, tab separated: taxon, FAD, LAD. 961 of 962 tips.
- Copied 2026-11-12 from JM's BAMM dinosaur project (`Research/BAMM_dinosaur/lloydFiles/`), where they were used for the BAMM analyses. Provenance of the occurrence table inside that project is not recorded here; it was distributed with the metatrees.
- Citation (verify before publishing): Lloyd GT, Bapst DW, Friedman M, Davis KE. 2016. Probabilistic divergence time estimation without branch lengths: dating the origins of dinosaurs, avian flight and crown birds. Biology Letters 12: 20160609.
- Not used: the cal3-rescaled trees in that project (`cal3_trees.tre`). Their absolute ages are inflated by the BAMM-rate rescaling (paravian split from the crown-bird line about 224 Ma; root 252–447 Ma).
- Dated here by `code/db_analysis/a02_theropod_tree.R` (minimum branch length).
