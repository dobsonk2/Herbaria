# TITLE:          Herbarium data: Label -> Symbiota
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


############################### Label template to symbiota template #################################
### notes for things to check in the raw excel file before running this code:
# make sure only 1 name is present for "primary collector", all other collectors should go under "additional collectors"


### Read in data ###
# export the excel file to the UTF-8 CSV format
# change the name for the label file from "test.csv" to the name of the file you want to re-format
label <- read.csv("official_test.csv",fileEncoding="UTF-8")


### splitting the dataframe based on Kingdom column ###
label$Kingdom <- gsub(" ","",label$Kingdom) # removing any empty spaces in the kingdom column
unique(label$Kingdom) # checking kingdom names
# fixing kingdom names that are misspelled
# the misspelled kingdom goes first, then the correct spelling goes second (don't need to run this line if everything is spelled correctly)
label$Kingdom[label$Kingdom == "LIchens"] <- "Lichens"
# only run the lines of code for the kingdom you want to work with
# e.g., to work with just plants, run the two lines of code that make "label2" contain plants data
# skip the other kingdoms you don't need (in this example, fungi and lichens) and run the rest of the script on the label2 that contains plants
# once the code has been run on the plants dataframe, come back to this code and re-run the
# label2 lines for the other wanted kingdoms - run the rest of the script the same as before & save the output (be sure to use different file names for each output for each kingdom)
label2 <- label %>%
  filter(Kingdom == "Plants")
label2 <- label %>%
  filter(Kingdom == "Fungi")
label2 <- label %>%
  filter(Kingdom == "Lichens")


### selecting columns from label template that symbiota needs ###
colnames(label2)
label_col_remove <- label2 %>%
  select(any_of(c("Collection.number","Barcode","Accession.number","Cultivated","Family","Genus","Species","State",             
                  "County","Locality","Latitude","Longitude","Habitat","Associated.species","Abundance","Description",        
                  "Additional.notes","Primary.collector","Additional.collectors","Date.collected","Determiner","Date.determined",
                  "iNat.URL","Infrarank","infraEpithet","Substrate")))


### removing any spaces before or after all entries from all columns
label_col_remove <- as.data.frame(apply(label_col_remove, 2, function(y) gsub("\\s$","", y)))
label_col_remove <- as.data.frame(apply(label_col_remove, 2, function(y) gsub("^\\s","", y)))


### re-naming label columns to be correct for symbiota ###
colnames(label_col_remove)[colnames(label_col_remove) == "Collection.number"] <- "recordNumber"
colnames(label_col_remove)[colnames(label_col_remove) == "Barcode"] <- "catalogNumber"
colnames(label_col_remove)[colnames(label_col_remove) == "Accession.number"] <- "otherCatalogNumbers"
colnames(label_col_remove)[colnames(label_col_remove) == "Cultivated"] <- "cultivationStatus"
colnames(label_col_remove)[colnames(label_col_remove) == "Family"] <- "family"
colnames(label_col_remove)[colnames(label_col_remove) == "Genus"] <- "genus"
colnames(label_col_remove)[colnames(label_col_remove) == "Species"] <- "specificEpithet"
colnames(label_col_remove)[colnames(label_col_remove) == "State"] <- "state"
colnames(label_col_remove)[colnames(label_col_remove) == "County"] <- "county"
colnames(label_col_remove)[colnames(label_col_remove) == "Locality"] <- "locality"
colnames(label_col_remove)[colnames(label_col_remove) == "Latitude"] <- "decimalLatitude"
colnames(label_col_remove)[colnames(label_col_remove) == "Longitude"] <- "decimalLongitude"
colnames(label_col_remove)[colnames(label_col_remove) == "Habitat"] <- "habitat"
colnames(label_col_remove)[colnames(label_col_remove) == "Associated.species"] <- "associatedTaxa"
colnames(label_col_remove)[colnames(label_col_remove) == "Abundance"] <- "individualCount"
colnames(label_col_remove)[colnames(label_col_remove) == "Description"] <- "verbatimAttributes"
colnames(label_col_remove)[colnames(label_col_remove) == "Primary.collector"] <- "recordedBy"
colnames(label_col_remove)[colnames(label_col_remove) == "Additional.collectors"] <- "associatedCollectors"
colnames(label_col_remove)[colnames(label_col_remove) == "Date.collected"] <- "eventDate"
colnames(label_col_remove)[colnames(label_col_remove) == "Determiner"] <- "identifiedBy"
colnames(label_col_remove)[colnames(label_col_remove) == "Date.determined"] <- "dateIdentified"
colnames(label_col_remove)[colnames(label_col_remove) == "Infrarank"] <- "verbatimTaxonRank"
colnames(label_col_remove)[colnames(label_col_remove) == "infraEpithet"] <- "infraSpecificEpithet"
colnames(label_col_remove)[colnames(label_col_remove) == "Substrate"] <- "substrate"


### making combined scientific name column (merging genus and species) ###
label_col_remove$scientificName <- paste0(label_col_remove$genus," ",label_col_remove$specificEpithet)


### making combined occurrence remarks column (merging additional notes & iNat URL) ###
# first, removing punctuation from the end of each additional notes cell (this is so that we don't end up with '..' when merging with the iNat data)
label_col_remove$Additional.notes <- sub('\\.\\s?$', '', label_col_remove$Additional.notes)
# if an iNat URL exists, then we're merging it with the additional notes to form "occurrenceRemarks" column
# if an iNat URL does not exist, we're just pasting in additional notes as "occurrenceRemarks"
label_col_remove$occurrenceRemarks <- ifelse(label_col_remove$iNat.URL == "",
                                             label_col_remove$Additional.notes,
                                             paste0(label_col_remove$Additional.notes,". iNaturalist URL: ",label_col_remove$iNat.URL))
# removing ". " from cells that just had iNat url
label_col_remove$occurrenceRemarks <- sub('^\\.\\s', '', label_col_remove$occurrenceRemarks)
# adding a period to the end of the occurrenceRemarks
label_col_remove$occurrenceRemarks <- ifelse(label_col_remove$occurrenceRemarks == "",
                                             label_col_remove$occurrenceRemarks,
                                             paste0(label_col_remove$occurrenceRemarks,"."))
# just a check to see how the occurrenceRemarks column outputs look
unique(label_col_remove$occurrenceRemarks)


### adding "processingStatus" column
# if the specimen contains latitude, then processingStatus = 3
# if not, processingStatus = 2
label_col_remove$processingStatus <- ifelse(label_col_remove$decimalLatitude == "",
                                            "Stage 2",
                                            "Stage 3")


### if determiner column is empty, make the determiner the first collector ###
label_col_remove$identifiedBy <- ifelse(label_col_remove$identifiedBy == "",
                                        label_col_remove$recordedBy,
                                        label_col_remove$identifiedBy)


### remove columns we needed for merging, but not on their own ###
label_col_remove2 = subset(label_col_remove, select = -c(Additional.notes,iNat.URL))


### replacing NA's with blank strings ###
label_col_remove2[is.na(label_col_remove2)] <- ""


### save output as a csv file ###
# can change "test1" to whatever name you want the new file to be named
# when working with specific kingdoms, be sure to differentiate the names here
write.csv(label_col_remove2, "test3_plants.csv", row.names=F, fileEncoding = "UTF-8")

