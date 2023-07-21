# TITLE:          Herbarium data: Finding samples unique to Symbiota
# AUTHORS:        Kara Dobson
# DATA INPUT:     Data imported as csv files
# DATA OUTPUT:    Herbarium data transformed into the needed template
# DATE:           May-July 2023


# Clear all existing data
rm(list=ls())

# Set working directory
setwd("~/Documents/research/Herbarium") # set to wherever files are stored

# Load packages
library(tidyverse)
library(readr)



### Read in data ###
# download Symbiota data and Specify barcodes as UTF-8
# change the name for the symbiota data file to match the name of the file you downloaded from Symbiota
sym <- read_csv("20230706 Symbiota.csv")
sym <- as.data.frame(sym)
spec <- read_csv("All Specify barcodes.csv")
spec <- as.data.frame(spec)


### subsetting data ###
# only keeping rows in symbiota that have a catalogNumber not present in the specify database
sym2 <- subset(sym, !(catalogNumber %in% spec$catalogNumber))


### amending the Specify barcodes ###
# merging the new Symbiota barcodes onto the Specify barcode dataframe, so that it contains all
# barcodes present in Specify (this way, you will not need to re-download specify data every time
# you want to find which barcodes are unique to Symbiota)
spec2 <- bind_rows(spec,sym2)
spec2 <- spec2 %>%
  select(catalogNumber)


### replacing NA's with blank strings ###
sym2[is.na(sym2)] <- ""


### save outputs as csv files ###
# can change "name.csv" to whatever name you want the new file to be named
Encoding(sym2$identifiedBy) = "latin1"
Encoding(sym2$recordedBy) = "latin1"
Encoding(sym2$associatedCollectors) = "latin1"
Encoding(sym2$scientificNameAuthorship) = "latin1"
write_excel_csv(sym2, file="unique_symbiota_data.csv")
write.csv(spec2, "updated_Specify_barcodes.csv", row.names=F, fileEncoding = "UTF-8")
