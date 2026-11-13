###################################################################
# x18_predatorTraits.R
# Scaffold a predator life-history trait table for use as explanatory
# factors in the scaling/PPMR models (JM's idea: hunting style + ecology).
#   foraging_mode : ambush | pursuit | mixed                  (how prey is captured)
#   sociality     : solitary | social | mixed                 (alone vs group hunting)
#   habitat_zone  : aquatic | semiaquatic | terrestrial |     (medium / locomotor zone the
#                   fossorial | scansorial | arboreal | aerial  predator hunts in)
#
# WHY habitat_zone (the new axis): the medium a predator hunts in imposes
# MECHANICAL limits on relative prey size that are partly independent of clade:
#   - aerial raptors face a lift/carry ceiling -> a hard cap on prey:predator mass;
#   - fossorial/burrow hunters (e.g. Mustela) can corner prey large for their size
#     (a candidate explanation for the Mustela nivalis large-prey outlier in x17);
#   - semiaquatic ambushers (crocs, herons) sit at a water interface;
#   - scansorial/arboreal hunters (Martes) take a tree-bound prey field.
# It is INTENTIONALLY a within-clade contrast where it can be: most signal lives in
# Mustela(fossorial) vs Martes(scansorial) within Mammalia, and herons(semiaquatic)
# vs raptors/owls(aerial) within Aves. Across clades it is collinear with `clade`
# (almost all Crocodylia are semiaquatic, etc.) -> do NOT enter it as a free fixed
# effect ALONGSIDE clade without checking aliasing; see notes in the trait section
# of diet-project-findings + the design block at the foot of this file.
#
# Builds `data/predator_traits.csv` listing every predator species in longform,
# seeded with HIGH-CONFIDENCE clade/genus defaults + a few species overrides,
# flagged `certainty` (seed_clade / seed_genus / verify). Most rows need
# expert/literature verification -- this is a DRAFT to fill in, not authority.
# Merge-preserving: hand-edited rows are NOT clobbered on re-run.
# Once complete, join into x17/x20 and add as fixed effects, e.g.
#   preyMass ~ predmass_c * foraging_mode + sociality + (1+predmass_c|clade) + ...
#   (a trait-based ALTERNATIVE to clade: ~ predmass_c * habitat_zone, dropping clade)
###################################################################
args <- commandArgs(trailingOnly = FALSE); fa <- grep("^--file=", args, value = TRUE)
ROOT <- if (length(fa)) normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", fa))), "..")) else normalizePath(getwd())
d <- read.csv(file.path(ROOT, "data", "longform.csv"))
sp <- unique(d[, c("species", "genus", "subclade", "clade")])
sp <- sp[order(sp$clade, sp$genus, sp$species), ]

# --- conservative seeds (foraging_mode, sociality, habitat_zone) ---
# clade-level defaults (coarsest)
clade_def <- list(
  Crocodylia = c("ambush", "solitary", "semiaquatic"),   # sit-and-wait at the water interface
  Aves       = c("pursuit", "solitary", "aerial"))        # default raptor/owl; waders overridden below
# genus-level overrides
genus_def <- list(
  # felids -- terrestrial ambush
  Panthera=c("ambush","solitary","terrestrial"), Puma=c("ambush","solitary","terrestrial"),
  Lynx=c("ambush","solitary","terrestrial"), Leopardus=c("ambush","solitary","terrestrial"),
  Felis=c("ambush","solitary","terrestrial"), Otocolobus=c("ambush","solitary","terrestrial"),
  # canids -- terrestrial pursuit
  Canis=c("pursuit","social","terrestrial"), Lycaon=c("pursuit","social","terrestrial"),
  Vulpes=c("mixed","solitary","terrestrial"),
  # mustelids -- the interesting within-Mammalia habitat contrast
  Mustela=c("pursuit","solitary","fossorial"),   # burrow/subnivean hunter -> corners large prey (x17 outlier hypothesis)
  Martes=c("pursuit","solitary","scansorial"),   # climbs; arboreal-ish prey field
  Ursus=c("mixed","solitary","terrestrial"),
  # snakes -- mostly terrestrial; pythons/Morelia toward climbing/semiaquatic
  Crotalus=c("ambush","solitary","terrestrial"), Sistrurus=c("ambush","solitary","terrestrial"),
  Vipera=c("ambush","solitary","terrestrial"), Bitis=c("ambush","solitary","terrestrial"),
  Python=c("ambush","solitary","terrestrial"), Morelia=c("ambush","solitary","arboreal"),
  Pituophis=c("mixed","solitary","terrestrial"), Elaphe=c("mixed","solitary","scansorial"),
  Hemorrhois=c("pursuit","solitary","terrestrial"), Lampropeltis=c("pursuit","solitary","terrestrial"),
  # lizards -- terrestrial active foragers (some Varanus semiaquatic; verify per sp)
  Varanus=c("pursuit","solitary","terrestrial"), Salvator=c("pursuit","solitary","terrestrial"),
  # birds: raptors/owls aerial; herons semiaquatic waders
  Tyto=c("pursuit","solitary","aerial"), Bubo=c("pursuit","solitary","aerial"),
  Strix=c("pursuit","solitary","aerial"), Asio=c("pursuit","solitary","aerial"),
  Accipiter=c("pursuit","solitary","aerial"), Buteo=c("pursuit","solitary","aerial"),
  Aquila=c("pursuit","solitary","aerial"), Falco=c("pursuit","solitary","aerial"),
  Ardea=c("ambush","solitary","semiaquatic"), Egretta=c("ambush","solitary","semiaquatic"),
  Butorides=c("ambush","solitary","semiaquatic"))
sp_def <- list(
  Acinonyx_jubatus=c("pursuit","solitary","terrestrial"),
  Panthera_leo=c("ambush","social","terrestrial"))

trait <- function(species, genus, clade) {
  if (species %in% names(sp_def))   return(c(sp_def[[species]], "verify"))
  if (genus   %in% names(genus_def)) return(c(genus_def[[genus]], "seed_genus"))
  if (clade   %in% names(clade_def)) return(c(clade_def[[clade]], "seed_clade"))
  c("", "", "", "verify")
}
tr <- t(mapply(trait, sp$species, sp$genus, sp$clade))
out <- data.frame(species = sp$species, genus = sp$genus, clade = sp$clade,
                  foraging_mode = tr[,1], sociality = tr[,2], habitat_zone = tr[,3],
                  certainty = tr[,4], source = "", stringsAsFactors = FALSE)

OUT <- file.path(ROOT, "data", "predator_traits.csv")
if (file.exists(OUT)) {
  # don't clobber hand-edited rows: keep existing non-empty values per column
  old <- read.csv(OUT, stringsAsFactors = FALSE)
  m <- match(out$species, old$species)
  for (cc in intersect(c("foraging_mode","sociality","habitat_zone","certainty","source"), names(old))) {
    keep <- !is.na(m) & nzchar(ifelse(is.na(old[[cc]][m]), "", old[[cc]][m]))
    out[[cc]][keep] <- old[[cc]][m][keep]
  }
  message("merged with existing predator_traits.csv (preserved filled rows; habitat_zone added where blank)")
}
write.csv(out, OUT, row.names = FALSE)
cat(sprintf("Wrote data/predator_traits.csv: %d predator species\n", nrow(out)))
cat(sprintf("  foraging_mode filled: %d | habitat_zone filled: %d | blank/verify: %d\n",
            sum(nzchar(out$foraging_mode)), sum(nzchar(out$habitat_zone)), sum(!nzchar(out$foraging_mode))))
print(table(habitat_zone = out$habitat_zone, clade = out$clade))
cat("\nNOTE: DRAFT seeds -- verify against literature before using as a covariate.\n")
cat("CAUTION: habitat_zone is collinear with clade across clades; its identifiable\n",
    "signal is WITHIN clade (Mustela fossorial vs Martes scansorial; herons vs raptors).\n", sep="")
