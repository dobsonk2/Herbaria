# TITLE:          Herbarium data: Symbiota -> Specify
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


############################### Symbiota template to specify template #################################

### Read in data ###
# download Symbiota data as UTF-8
# change the name for the label file from "test.csv" to the name of the file you want to re-format
label <- read_csv("20230706 Symbiota.csv")
label <- as.data.frame(label)


### selecting columns from symbiota that specify needs ###
colnames(label)
label_col_remove <- label %>%
  select(any_of(c("occurrenceID","catalogNumber","otherCatalogNumbers","genus","specificEpithet","infraspecificEpithet","identifiedBy","dateIdentified",  
                  "recordedBy","associatedCollectors","recordNumber","eventDate","occurrenceRemarks","habitat","substrate","verbatimAttributes",
                  "associatedTaxa","individualCount","preparations","country","stateProvince","county","locality","decimalLatitude",  
                  "decimalLongitude","verbatimCoordinates")))


### adding in empty columns that are created during specify processing ###
label_col_remove$Catalog.. <- NA
label_col_remove$Initials <- "MTC" # Change this to your initials if you're doing the upload


### sheet X of X designation ###
# if occurrenceRemarks contains "Sheet X of X" or "Box X of X", pull that out and put it in General \nComments instead
label_col_remove$General..nComments <- str_extract(label_col_remove$occurrenceRemarks, '(\\w+)?\\s?\\d of \\d')
# then, remove "sheet/box X of X" from occurrence remarks
label_col_remove$occurrenceRemarks <- str_remove_all(label_col_remove$occurrenceRemarks, '(\\w+)?\\s?\\d of \\d')
# capitalizing first letter of sheet or box
label_col_remove$General..nComments <- gsub("^([a-z])", "\\U\\1", label_col_remove$General..nComments, perl=TRUE)
# replacing NA's with blank strings
label_col_remove$General..nComments[is.na(label_col_remove$General..nComments)] <- ""
# if the comments only contain '# of #", add sheet infront
label_col_remove$General..nComments <- ifelse(grepl("\\w+ \\d of \\d",label_col_remove$General..nComments),
                                              label_col_remove$General..nComments,
                                              paste0("Sheet ",label_col_remove$General..nComments))
# remove blank sheet designations
label_col_remove$General..nComments[label_col_remove$General..nComments == "Sheet "] <- ""
# removing weird remnant punctuation after removing "sheet/box X of x" from occurrenceRemarks
label_col_remove$occurrenceRemarks <- str_remove_all(label_col_remove$occurrenceRemarks, '^(\\.\\s)')
label_col_remove$occurrenceRemarks <- str_remove_all(label_col_remove$occurrenceRemarks, '(\\s\\.)$')
# check occurrence remarks & notes
unique(label_col_remove$occurrenceRemarks)
unique(label_col_remove$General..nComments)


### # of sheets designation ###
# pull out the final number in "Sheet X of X" and store it in "# of Sheets"
label_col_remove$X..of.Sheets <- str_extract(label_col_remove$General..nComments, '(?<=of) \\d')


### adding preparation type ###
# replacing NA's with blank strings
label_col_remove$preparations[is.na(label_col_remove$preparations)] <- ""
# if prep type is blank, input "sheet" - if not, use what was already present in that column
label_col_remove$PrepType1 <- ifelse(label_col_remove$preparations == "",
                                     "sheet",
                                     label_col_remove$preparations)


### extracting coordinate information ###
# only run these lines about coordinates and TRS if "verbatimCoordinates" column exists
# separating between decimal degree, TRS, and UTM (code here from James Mickley)
label_col_remove <- label_col_remove %>%
  mutate(
    latLong = gsub(
      ".*?;? ?([NSEW]? ?[\\d\\.]+°?[ \\d'\",-NSEW°?]*[NSEW°?'\"])? ?;?.*", # I don't think this part works, but thats okay since we have separate columns for lat and long
      "\\1", verbatimCoordinates, perl = TRUE),
    trs = gsub(
      ".*?;? ?(TRS:[\\w /]*\\w)? ?;?.*", 
      "\\1", verbatimCoordinates, perl = TRUE),
    utm = gsub(
      ".*?;? ?(\\d{1,2}[A-Z]? \\d+E \\d+N)? ?;?.*", 
      "\\1", verbatimCoordinates, perl = TRUE)
  )


### parsing out TRS information ###
# removing "TRS:" from the beginning
label_col_remove$trs <- str_remove_all(label_col_remove$trs, 'TRS:\\s?')
# pulling out township information
label_col_remove$Township <- str_extract(label_col_remove$trs, 'T\\d\\d?\\w')
label_col_remove$TownshipDirection <- str_extract(label_col_remove$Township, '\\w$')
label_col_remove$Township <- str_extract(label_col_remove$Township, '\\d\\d?')
# pulling out range information
label_col_remove$Range <- str_extract(label_col_remove$trs, 'R\\d\\d?\\w')
label_col_remove$RangeDirection <- str_extract(label_col_remove$Range, '\\w$')
label_col_remove$Range <- str_extract(label_col_remove$Range, '\\d\\d?')
# pulling out section information
label_col_remove$Section <- str_extract(label_col_remove$trs, '[Ss]\\w?\\w?\\s?\\d\\d?')
label_col_remove$Section <- str_extract(label_col_remove$Section, '\\d\\d?')
# pulling out section part
label_col_remove$SectionPart <- str_extract(label_col_remove$trs, '[:upper:][:upper:].*')


### removing extra coordinate columns we don't need now
label_col_remove = subset(label_col_remove, select = -c(verbatimCoordinates,trs,utm))


### if determiner column is empty, make the first collector the determiner ###
# replacing NA's with blank strings
label_col_remove$identifiedBy[is.na(label_col_remove$identifiedBy)] <- ""
# moving names over
label_col_remove$identifiedBy <- ifelse(label_col_remove$identifiedBy == "",
                                        label_col_remove$recordedBy,
                                        label_col_remove$identifiedBy)


### checking name formats ###
# removing spaces after names
label_col_remove$recordedBy <- gsub("\\s$","",label_col_remove$recordedBy)
label_col_remove$identifiedBy <- gsub("\\s$","",label_col_remove$identifiedBy)
label_col_remove$associatedCollectors <- gsub("\\s$","",label_col_remove$associatedCollectors)
# checking names present in the dataframe
unique(label_col_remove$recordedBy)
unique(label_col_remove$identifiedBy)
unique(label_col_remove$associatedCollectors)
# fixing names in the wrong format - only need to run the lines below if things are formatted incorrectly
# any names written as "Last name, First name" will need corrected
# note: if multiple names are within "recordedBy", its probably easiest to separate this into "recordedBy" and "associatedCollectors"
# in the raw csv before reading it into R. 
# the incorrect format goes first, followed by the correct format
label_col_remove$recordedBy[label_col_remove$recordedBy == "Turpin, BC"] <- "B.C. Turpin"
label_col_remove$identifiedBy[label_col_remove$identifiedBy == "Burrows, JE"] <- "J.E. Burrows"
label_col_remove$associatedCollectors[label_col_remove$associatedCollectors == "Burrows, JE and Marks, RA"] <- "J.E. Burrows and R.A. Marks"


### splitting recorded column into first, last, and middle names ###
label_names <- label_col_remove %>%
  extract(recordedBy,
          into = c("Collector.First.Name1", "Collector.Middle1", "Collector.Last.Name1"),
          regex = "([A-Za-zÀ-ȕ]+\\.?)\\s*(?:([^\\s,]+)\\s)?(?:([A-Za-zÀ-ȕ]+(?:,\\sJr\\.)?))?")


### splitting identifier column into first, last, and middle names ###
# we're only choosing the first name listed for determiner because "determiner 2" means a different determination was made by someone, not an associated determiner
label_names <- label_names %>%
  mutate(identifiedBy=str_replace_all(identifiedBy,"\\, Jr\\.?"," Jr.")) %>% # this removes the comma before "Jr." so the next line of code works
  mutate(identifiedBy=str_replace_all(identifiedBy,"\\,\\s(?!and)"," and ")) %>% # this replaces any ", " with " and "
  mutate(identifiedBy=str_replace_all(identifiedBy,"\\, and "," and ")) %>% # this replaces any ", and " with " and "
  mutate(identifiedBy=str_replace_all(identifiedBy," with "," and ")) %>% # this replaces any " with " with " and "
  mutate(identifiedBy=str_replace_all(identifiedBy,"; "," and ")) %>% # this replaces any "; " with " and "
  separate(col = identifiedBy, into = c("first_det", "second_det"), sep = " and ") %>%
  extract(first_det,
          into = c("Determiner.First.Name1", "Determiner.Middle1", "Determiner.Last.Name1"),
          regex = "([A-Za-zÀ-ȕ]+\\.?)\\s*(?:([^\\s,]+)\\s)?(?:([A-Za-zÀ-ȕ]+(?:,\\sJr\\.)?))?")
# removing extra second_det column we don't need now
label_names = subset(label_names, select = -c(second_det))


### splitting additional collectors into their names ###
# this might output a warning message that missing pieces are filled with NA - this is okay, it will happen if two or more associated collectors
# aren't present for every record (likely there will not be 2 or more for every record)
label_names <- label_names %>%  
  mutate(associatedCollectors=str_replace_all(associatedCollectors,"\\, Jr\\.?"," Jr.")) %>% # this removes the comma before "Jr." so the next line of code works
  mutate(associatedCollectors=str_replace_all(associatedCollectors,"\\,\\s(?!and)"," and ")) %>% # this replaces any ", " separators with " and "
  mutate(associatedCollectors=str_replace_all(associatedCollectors,"\\, and "," and ")) %>% # this replaces and ", and " with " and "
  mutate(associatedCollectors=str_replace_all(associatedCollectors," with "," and ")) %>% # this replaces any " with " with " and "
  mutate(associatedCollectors=str_replace_all(associatedCollectors,"; "," and ")) %>% # this replaces any "; " with " and "
  separate(col = associatedCollectors, into = c("second", "third", "fourth" ,"fifth","sixth","seventh"), sep = " and ") %>%
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


### making columns that have duplicate info in specify
label_names$verbatimCollNum <- label_names$recordNumber
label_names$verbatimDate <- label_names$eventDate
label_names$verbatimAnnDate <- label_names$dateIdentified
label_names$BarCode <- label_names$catalogNumber


### replace periods in column names with spaces ###
names(label_names) <- gsub(x = names(label_names), pattern = "\\.", replacement = " ")


### re-naming label columns to be correct for specify ###
colnames(label_names)[colnames(label_names) == "occurrenceID"] <- "GUID"
colnames(label_names)[colnames(label_names) == "recordNumber"] <- "Collection Number"
colnames(label_names)[colnames(label_names) == "verbatimCollNum"] <- "Verbatim Coll #"
colnames(label_names)[colnames(label_names) == "otherCatalogNumbers"] <- "MSC Accession #"
colnames(label_names)[colnames(label_names) == "catalogNumber"] <- "Bar Code # 1"
colnames(label_names)[colnames(label_names) == "BarCode"] <- "Duplicate BarCode"
colnames(label_names)[colnames(label_names) == "genus"] <- "Genus1"
colnames(label_names)[colnames(label_names) == "specificEpithet"] <- "Species1"
colnames(label_names)[colnames(label_names) == "country"] <- "Country"
colnames(label_names)[colnames(label_names) == "stateProvince"] <- "State"
colnames(label_names)[colnames(label_names) == "county"] <- "County"
colnames(label_names)[colnames(label_names) == "substrate"] <- "Soil Type"
colnames(label_names)[colnames(label_names) == "locality"] <- "Locality"
colnames(label_names)[colnames(label_names) == "decimalLatitude"] <- "Latitude1"
colnames(label_names)[colnames(label_names) == "decimalLongitude"] <- "Longitude1"
colnames(label_names)[colnames(label_names) == "habitat"] <- "Verbatim Habitat"
colnames(label_names)[colnames(label_names) == "associatedTaxa"] <- "Associated Species"
colnames(label_names)[colnames(label_names) == "individualCount"] <- "Abundance"
colnames(label_names)[colnames(label_names) == "occurrenceRemarks"] <- "Comments (Collection Data)"
colnames(label_names)[colnames(label_names) == "infraspecificEpithet"] <- "Variety1"
colnames(label_names)[colnames(label_names) == "eventDate"] <- "Date"
colnames(label_names)[colnames(label_names) == "verbatimDate"] <- "Verbatim Date"
colnames(label_names)[colnames(label_names) == "dateIdentified"] <- "Ann Date 1"
colnames(label_names)[colnames(label_names) == "verbatimAnnDate"] <- "Verb Ann Date 1"
colnames(label_names)[colnames(label_names) == "verbatimAttributes"] <- "Field Characteristics"
colnames(label_names)[colnames(label_names) == "latLong"] <- "Verbatim Lat/Long"
colnames(label_names)[colnames(label_names) == "TownshipDirection"] <- "Township Direction"
colnames(label_names)[colnames(label_names) == "RangeDirection"] <- "Range Direction"
colnames(label_names)[colnames(label_names) == "SectionPart"] <- "Section Part"
colnames(label_names)[colnames(label_names) == "X  of Sheets"] <- "# of Sheets"
colnames(label_names)[colnames(label_names) == "General  nComments"] <- "General \\nComments"
colnames(label_names)[colnames(label_names) == "Catalog  "] <- "Catalog #"
colnames(label_names)[colnames(label_names) == "PrepType1"] <- "Prep Type1"


### replacing NA's with blank strings ###
label_names[is.na(label_names)] <- ""


### save output as a csv file ###
# can change "label_to_specify" to whatever name you want the new file to be named
write.csv(label_names, "sym_to_spec_test1.csv", row.names=F, fileEncoding = "UTF-8")

