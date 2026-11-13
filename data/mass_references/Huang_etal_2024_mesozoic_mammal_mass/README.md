# Data from: High-precision body mass predictors for small mammals: a case study in the Mesozoic

<https://doi.org/10.5061/dryad.tmpg4f550>

One collection of extant therian dentoskeletal measurments, one collection of Eutriconodonta dentoskeletal measurments, and one collection of Eutriconodonta body mass predictions.

## Description of the data and file structure

'data.csv' is the csv file containing measurements of extant therians, 'eutridata.csv' is the csv file containing measurements of Eutriconodonta and 'allmass.csv' is the file for plotting the figure of fossil mass prediction. 'pgls.tre' and 'pglstime.tre' are tree files in Newick format required to run the below R markdown file.

For data.csv, NA represents missing values due to either the absence or immeasurability of the structure.

For eutridata.csv, the associated specimen for a measurement is either next to the measurement in the 'name' column, or the first one above in that column; the same is true for the associated publication in the 'Publication' column.

For allmass.csv, the different number of rows for the columns is due to differences in number of predictors avaiable for each species.

## Code/Software

The R markdown file was last run with RStudio version 2023.06.1+524 and R version 4.3.1. Required packages include gt, gridExtra, tidyverse, glue, data.table, kableExtra, knitr, ape, maps, phytools, geiger, nlme, caret, schoolmath.
