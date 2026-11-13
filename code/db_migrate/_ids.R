###################################################################
# _ids.R -- id helpers shared by the migration scripts and the evidence builders.
###################################################################
## ids: letters, digits and _ . : - only
make_id <- function(x) {
  x <- iconv(trimws(x), to = "ASCII//TRANSLIT")
  x <- gsub("/", "_or_", x, fixed = TRUE)
  x <- gsub("[^A-Za-z0-9_.:-]+", "_", x)
  x <- gsub("_+", "_", x); x <- gsub("^[_.:-]+|[_.:-]+$", "", x)
  x
}
## taxon ids and names: one case convention so "Bubo Bubo" and "Bubo bubo" are one taxon
## (genus capitalised, epithets lower case; higher taxa capitalised; ids with "_or_" or a code suffix left alone)
norm_case <- function(x) vapply(strsplit(as.character(x), "_", fixed = TRUE), function(v) {
  if (!length(v) || !nzchar(v[1])) return(paste(v, collapse = "_"))
  v[1] <- paste0(toupper(substr(v[1], 1, 1)), tolower(substring(v[1], 2)))
  if (length(v) %in% 2:3 && !"or" %in% v && all(grepl("^[A-Za-z-]+$", v[-1])) && !any(tolower(v[-1]) %in% c("pooled", "indet", "or")))
    v[-1] <- tolower(v[-1])
  paste(v, collapse = "_") }, "")
taxon_id_of <- function(x) norm_case(make_id(x))
