# TITLE:          Herbarium data: Label -> Specify
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


############################### Label template to specify template #################################
### notes for things to check in the raw excel file before running this code:
# make sure only 1 name is present for "primary collector", all other collectors should go under "additional collectors"


### Read in data ###
# change the name for the label file from "Label_test.csv" to the name of the file you want to re-format
label <- read.csv("official_test.csv",fileEncoding="UTF-8")


### selecting columns from symbiota that specify needs ###
colnames(label)
label_col_remove <- label %>%
  select(any_of(c("Collection.number","Barcode","Accession.number",
                  "Genus","Species","State",
                  "County","Locality","Latitude","Longitude",
                  "Habitat","Associated.species","Abundance","Description",   
                  "Additional.notes","Primary.collector","Additional.collectors",
                  "Date.collected","Determiner","Date.determined","iNat.URL",
                  "infraEpithet","Substrate")))


### removing any spaces before or after all entries from all columns ###
label_col_remove <- as.data.frame(apply(label_col_remove, 2, function(y) gsub("\\s$","", y)))
label_col_remove <- as.data.frame(apply(label_col_remove, 2, function(y) gsub("^\\s","", y)))


### adding in empty columns that are created during specify processing ###
label_col_remove$Catalog.. <- NA
label_col_remove$Initials <- "MTC" # Change this to your initials if you're doing the upload


### making columns that are duplicated in specify ###
label_col_remove$Verbatim.Date <- label_col_remove$Date.collected
label_col_remove$Verbatim.Coll.. <- label_col_remove$Collection.number.1
label_col_remove$Verb.Ann.Date.1 <- label_col_remove$Date.determined
label_col_remove$Duplicate.BarCode <- label_col_remove$Barcode


### making combined lat/long column ###
label_col_remove$Verbatim.Lat.Long <- paste0(label_col_remove$Latitude,", ",label_col_remove$Longitude)


### merging additional notes & iNat URL into specimen description & general comments columns ###
# first, removing punctuation from the end of each additional notes cell (this is so that we don't end up with '..' when merging with the iNat data)
label_col_remove$Additional.notes <- sub('\\.\\s?$', '', label_col_remove$Additional.notes)
# if an iNat URL exists, then we're merging it with the additional notes to form "general comments" column
# if an iNat URL does not exist, we're just pasting in additional notes as "general comments"
label_col_remove$General..nComments <- ifelse(label_col_remove$iNat.URL == "",
                                              label_col_remove$Additional.notes,
                                              paste0(label_col_remove$Additional.notes,". iNaturalist URL: ",label_col_remove$iNat.URL))
# removing ". " from cells that just had iNat url
label_col_remove$General..nComments <- sub('^\\.\\s', '', label_col_remove$General..nComments)
# adding a period to the end of the general comments
label_col_remove$General..nComments <- ifelse(label_col_remove$General..nComments == "",
                                              label_col_remove$General..nComments,
                                              paste0(label_col_remove$General..nComments,"."))
# just a check to see how the general comments column outputs look
unique(label_col_remove$General..nComments)


### # of sheets designation ###
# pull out the final number in "Sheet X of X" and store it in "# of Sheets"
label_col_remove$X..of.Sheets <- str_extract(label_col_remove$General..nComments, '(?<=of) \\d')


### checking name formats ###
# checking names present in the dataframe
unique(label_col_remove$Primary.collector)
unique(label_col_remove$Determiner)
unique(label_col_remove$Additional.collectors)
# fixing names in the wrong format - only need to run the lines below if things are formatted incorrectly
# any names written as "Last name, First name" will need corrected
# note: if multiple names are within "Primary collectors", its probably easiest to separate this into "primary collector" and "additional collectors"
# in the raw csv before reading it into R. 
# the incorrect format goes first, followed by the correct format
label_col_remove$Primary.collector[label_col_remove$Primary.collector == "Turpin, BC"] <- "B.C. Turpin"
label_col_remove$Determiner[label_col_remove$Determiner == "Sorrells, Ryan"] <- "Ryan Sorrells"
label_col_remove$Determiner[label_col_remove$Determiner == "Nugent, Therese"] <- "Therese Nugent"
label_col_remove$Additional.collectors[label_col_remove$Additional.collectors == "Burrows, JE and Marks, RA"] <- "J.E. Burrows, R.A. Marks"


### splitting primary collector column into first, last, and middle names ###
label_names <- label_col_remove %>%
  extract(Primary.collector,
          into = c("Collector.First.Name1", "Collector.Middle1", "Collector.Last.Name1"),
          regex = "([A-Za-zÀ-ȕ]+\\.?)\\s*(?:([^\\s,]+)\\s)?(?:([A-Za-zÀ-ȕ]+(?:,\\sJr\\.)?))?")


### splitting determiner column into first, last, and middle names ###
# we're only choosing the first name listed for determiner because "determiner 2" means a different determination was made by someone, not an associated determiner
label_names <- label_names %>%
  mutate(Determiner=str_replace_all(Determiner,"\\, Jr\\.?"," Jr.")) %>% # this removes the comma before "Jr." so the next line of code works
  mutate(Determiner=str_replace_all(Determiner,"\\,\\s(?!and)"," and ")) %>% # this replaces any ", " with " and "
  mutate(Determiner=str_replace_all(Determiner,"\\, and "," and ")) %>% # this replaces any ", and " with " and "
  mutate(Determiner=str_replace_all(Determiner," with "," and ")) %>% # this replaces any " with " with " and "
  mutate(Determiner=str_replace_all(Determiner,"; "," and ")) %>% # this replaces any "; " with " and "
  separate(col = Determiner, into = c("first_det", "second_det"), sep = " and ") %>%
  extract(first_det,
          into = c("Determiner.First.Name1", "Determiner.Middle1", "Determiner.Last.Name1"),
          regex = "([A-Za-zÀ-ȕ]+\\.?)\\s*(?:([^\\s,]+)\\s)?(?:([A-Za-zÀ-ȕ]+(?:,\\sJr\\.)?))?")
# removing extra second_det column we don't need now
label_names = subset(label_names, select = -c(second_det))


### splitting additional collectors into their names ###
# this might output a warning message that missing pieces are filled with NA - this is okay, it will happen if two associated collectors
# aren't present for every record (likely there will not be 2 for every record)
label_names <- label_names %>%  
  mutate(Additional.collectors=str_replace_all(Additional.collectors,"\\, Jr\\.?"," Jr.")) %>% # this removes the comma before "Jr." so the next line of code works
  mutate(Additional.collectors=str_replace_all(Additional.collectors,"\\,\\s(?!and)"," and ")) %>% # this replaces any ", " separators with " and "
  mutate(Additional.collectors=str_replace_all(Additional.collectors,"\\, and "," and ")) %>% # this replaces and ", and " with " and "
  mutate(Additional.collectors=str_replace_all(Additional.collectors," with "," and ")) %>% # this replaces any " with " with " and "
  mutate(Additional.collectors=str_replace_all(Additional.collectors,"; "," and ")) %>% # this replaces any "; " with " and "
  separate(col = Additional.collectors, into = c("second", "third", "fourth" ,"fifth","sixth","seventh"), sep = " and ") %>%
  extract(second,
          into = c("Collector.First.Name2", "Collector.Middle2", "Collector.Last.Name2"),
          regex = "([A-Za-zÀ-ȕ]+\\.?)\\s*(?:([^\\s,]+)\\s)?(?:([A-Za-zÀ-ȕ]+(?:,\\sJr\\.)?))?") %>%
  extract(third,
          into = c("Collector.First.Name3", "Collector.Middle3", "Collector.Last.Name3"),
          regex = "([A-Za-zÀ-ȕ]+\\.?)\\s*(?:([^\\s,]+)\\s)?(?:([A-Za-zÀ-ȕ]+(?:,\\sJr\\.)?))?") %>%
  extract(fourth,
          into = c("Collector.First.Name4", "Collector.Middle4", "Collector.Last.Name4"),
          regex = "([A-Za-zÀ-ȕ]+\\.?)\\s*(?:([^\\s,]+)\\s)?(?:([A-Za-zÀ-ȕ]+(?:,\\sJr\\.)?))?") %>%
  extract(fifth,
          into = c("Collector.First.Name5", "Collector.Middle5", "Collector.Last.Name5"),
          regex = "([A-Za-zÀ-ȕ]+\\.?)\\s*(?:([^\\s,]+)\\s)?(?:([A-Za-zÀ-ȕ]+(?:,\\sJr\\.)?))?") %>%
  extract(sixth,
          into = c("Collector.First.Name6", "Collector.Middle6", "Collector.Last.Name6"),
          regex = "([A-Za-zÀ-ȕ]+\\.?)\\s*(?:([^\\s,]+)\\s)?(?:([A-Za-zÀ-ȕ]+(?:,\\sJr\\.)?))?") %>%
  extract(seventh,
          into = c("Collector.First.Name7", "Collector.Middle7", "Collector.Last.Name7"),
          regex = "([A-Za-zÀ-ȕ]+\\.?)\\s*(?:([^\\s,]+)\\s)?(?:([A-Za-zÀ-ȕ]+(?:,\\sJr\\.)?))?")


### removing unneeded columns ###
label_names = subset(label_names, select = -c(Additional.notes,iNat.URL))


### replace periods in column names with spaces ###
names(label_names) <- gsub(x = names(label_names), pattern = "\\.", replacement = " ")


### re-naming label columns to be correct for specify ###
colnames(label_names)[colnames(label_names) == "Collection number"] <- "Collection Number"
colnames(label_names)[colnames(label_names) == "Verbatim Coll  "] <- "Verbatim Coll #"
colnames(label_names)[colnames(label_names) == "Barcode"] <- "Bar Code # 1"
colnames(label_names)[colnames(label_names) == "Accession number"] <- "MSC Accession #"
colnames(label_names)[colnames(label_names) == "Genus"] <- "Genus1"
colnames(label_names)[colnames(label_names) == "Species"] <- "Species1"
colnames(label_names)[colnames(label_names) == "Latitude"] <- "Latitude1"
colnames(label_names)[colnames(label_names) == "Longitude"] <- "Longitude1"
colnames(label_names)[colnames(label_names) == "Verbatim Lat Long"] <- "Verbatim Lat/Long"
colnames(label_names)[colnames(label_names) == "Habitat"] <- "Verbatim Habitat"
colnames(label_names)[colnames(label_names) == "Associated species"] <- "Associated Species"
colnames(label_names)[colnames(label_names) == "Description"] <- "Field Characteristics"
colnames(label_names)[colnames(label_names) == "Date collected"] <- "Date"
colnames(label_names)[colnames(label_names) == "Date determined"] <- "Ann Date 1"
colnames(label_names)[colnames(label_names) == "X  of Sheets"] <- "# of Sheets"
colnames(label_names)[colnames(label_names) == "Catalog  "] <- "Catalog #"
colnames(label_names)[colnames(label_names) == "General  nComments"] <- "General \\nComments"
colnames(label_names)[colnames(label_names) == "infraEpithet"] <- "Variety1"
colnames(label_names)[colnames(label_names) == "Substrate"] <- "Soil Type"


### replacing NA's with blank strings ###
label_names[is.na(label_names)] <- ""


### save output as a csv file ###
# can change "test1.csv" to whatever name you want the new file to be named
write.csv(label_names, "test1.csv", row.names=F)

