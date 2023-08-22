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
library(readr)


############################### Symbiota template to specify template #################################

### Read in data ###
# download Symbiota data as UTF-8
# change the name for the sym file from "test.csv" to the name of the file you want to re-format
sym <- read_csv("unique_symbiota_data.csv")
sym <- as.data.frame(sym)
multimedia <- read_csv("multimedia.csv")
multimedia <- as.data.frame(multimedia)


### keeping only stage 1, 2, 3, and closed processing status ###
# check the output from the unique function below
# then, edit the filtering code to contain all instances of how closed, reviewed, stage 1, stage 2, and stage 3 are spelled
unique(sym$processingStatus)
sym <- sym %>%
  filter(processingStatus == "closed" | processingStatus == "stage 3" | processingStatus == "stage 2" |
         processingStatus == "stage 1" | processingStatus == "STAGE_3" | processingStatus == "Stage 2" |
         processingStatus == "Stage 3" | processingStatus == "Stage 1" | processingStatus == "reviewed")


### selecting columns from symbiota that specify needs ###
colnames(sym)
sym_col_remove <- sym %>%
  select(any_of(c("id","occurrenceID","catalogNumber","otherCatalogNumbers","genus","specificEpithet","infraspecificEpithet","identifiedBy","dateIdentified",  
                  "recordedBy","associatedCollectors","recordNumber","eventDate","occurrenceRemarks","habitat","substrate","verbatimAttributes",
                  "associatedTaxa","individualCount","preparations","country","stateProvince","county","locality","decimalLatitude",  
                  "decimalLongitude","verbatimCoordinates","verbatimEventDate","cultivationStatus")))


### selecting ID and image url from multimedia dataframe ###
pic_urls <- multimedia %>%
  select(any_of(c("coreid","identifier")))


### making id column name match the name in symbiota data ###
colnames(pic_urls)[colnames(pic_urls) == "coreid"] <- "id"


### only keeping rows in pic_urls that have a ID present in the symbiota database ###
pic_urls <- subset(pic_urls, (id %in% sym$id))


### merging pic urls with symbiota dataframe by ID ###
sym_col_remove <- merge(sym_col_remove, pic_urls, by="id", all=T)


### adding in empty columns that are created during specify processing ###
sym_col_remove$Catalog.. <- NA
sym_col_remove$Initials <- "MTC" # Change this to your initials if you're doing the upload


### adding preparation type ###
# replacing NA's with blank strings
sym_col_remove$preparations[is.na(sym_col_remove$preparations)] <- ""
# if prep type is blank, input "sheet" - if not, use what was already present in that column
sym_col_remove$PrepType1 <- ifelse(sym_col_remove$preparations == "",
                                   "sheet",
                                   sym_col_remove$preparations)


### sheet X of X designation ###
# if occurrenceRemarks contains "Sheet X of X" or "Box X of X", pull that out and put it in General \nComments instead
sym_col_remove$General..nComments <- str_extract(sym_col_remove$occurrenceRemarks, '(\\w+)?\\s?\\d of \\d')
# then, remove "sheet/box X of X" from occurrence remarks
sym_col_remove$occurrenceRemarks <- str_remove_all(sym_col_remove$occurrenceRemarks, '(\\w+)?\\s?\\d of \\d')
# capitalizing first letter of sheet or box
sym_col_remove$General..nComments <- gsub("^([a-z])", "\\U\\1", sym_col_remove$General..nComments, perl=TRUE)
# replacing NA's with blank strings
sym_col_remove$General..nComments[is.na(sym_col_remove$General..nComments)] <- ""
# if the comments only contain '# of #", add sheet infront
sym_col_remove$General..nComments <- ifelse(grepl("\\w+ \\d of \\d",sym_col_remove$General..nComments),
                                              sym_col_remove$General..nComments,
                                              paste0("Sheet ",sym_col_remove$General..nComments))
# remove blank sheet designations
sym_col_remove$General..nComments[sym_col_remove$General..nComments == "Sheet "] <- ""
# removing weird remnant punctuation after removing "sheet/box X of x" from occurrenceRemarks
sym_col_remove$occurrenceRemarks <- str_remove_all(sym_col_remove$occurrenceRemarks, '^(\\.\\s)')
sym_col_remove$occurrenceRemarks <- str_remove_all(sym_col_remove$occurrenceRemarks, '(\\s\\.)$')
# check occurrence remarks & notes
unique(sym_col_remove$occurrenceRemarks)
unique(sym_col_remove$General..nComments)


### adding cultivation status ###
# if cultivationStatus is NA, make it a 0
sym_col_remove$cultivationStatus[is.na(sym_col_remove$cultivationStatus)] <- 0
# if cultivationStatus = 1, add "Cultivated" to occurrenceRemarkrs (i.e., Comments collection data)
sym_col_remove$occurrenceRemarks <- ifelse(sym_col_remove$cultivationStatus == 1,
                                paste0(sym_col_remove$occurrenceRemarks, " Cultivated."),
                                sym_col_remove$occurrenceRemarks)
# if "cultivated" was already written in occurrenceRemarks, remove the second "cultivated"
sym_col_remove$occurrenceRemarks <-ifelse((str_detect(sym_col_remove$occurrenceRemarks, "[Cc]ultivated.\\s*Cultivated."))==T,
                               str_replace_all(sym_col_remove$occurrenceRemarks,"[Cc]ultivated. Cultivated.","Cultivated."),
                               sym_col_remove$occurrenceRemarks)
# removing NA from beginning of "Cultivated." if occurrenceRemarks originally had NA
sym_col_remove$occurrenceRemarks <-ifelse((str_detect(sym_col_remove$occurrenceRemarks, "NA\\s*"))==T,
                               str_replace_all(sym_col_remove$occurrenceRemarks,"NA\\s*",""),
                               sym_col_remove$occurrenceRemarks)
# check occurrence remarks
unique(sym_col_remove$occurrenceRemarks)


### # of sheets designation ###
# pull out the final number in "Sheet X of X" and store it in "# of Sheets"
sym_col_remove$X..of.Sheets <- str_extract(sym_col_remove$General..nComments, '(?<=of) \\d')


### keeping only the first value in otherCatalogNumbers
sym_col_remove$otherCatalogNumbers <- str_extract(sym_col_remove$otherCatalogNumbers, "[^;]+")
#sym_col_remove$otherCatalogNumbers <- gsub("Accession Number: (\\d+)", "\\1", sym_col_remove$otherCatalogNumbers)


### extracting coordinate information ###
# only run these lines about coordinates and TRS if "verbatimCoordinates" column exists
# separating between decimal degree, TRS, and UTM (code here from James Mickley)
sym_col_remove <- sym_col_remove %>%
  mutate(
    latLong = gsub(
      ".*?;? ?([NSEW]? ?[\\d\\.]+°[ \\d'\",-NSEW°]*[NSEW°'\"])? ?;?.*", # I don't think this part works, but thats okay since we have separate columns for lat and long
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
sym_col_remove$trs <- str_remove_all(sym_col_remove$trs, 'TRS:\\s?')
# pulling out township information
sym_col_remove$Township <- str_extract(sym_col_remove$trs, 'T\\d\\d?\\w')
sym_col_remove$TownshipDirection <- str_extract(sym_col_remove$Township, '\\w$')
sym_col_remove$Township <- str_extract(sym_col_remove$Township, '\\d\\d?')
# pulling out range information
sym_col_remove$Range <- str_extract(sym_col_remove$trs, 'R\\d\\d?\\w')
sym_col_remove$RangeDirection <- str_extract(sym_col_remove$Range, '\\w$')
sym_col_remove$Range <- str_extract(sym_col_remove$Range, '\\d\\d?')
# pulling out section information
sym_col_remove$Section <- str_extract(sym_col_remove$trs, '[Ss]\\w?\\w?\\s?\\d\\d?')
sym_col_remove$Section <- str_extract(sym_col_remove$Section, '\\d\\d?')
# pulling out section part
sym_col_remove$SectionPart <- str_extract(sym_col_remove$trs, '[:upper:][:upper:].*')


### removing extra coordinate columns we don't need now
sym_col_remove = subset(sym_col_remove, select = -c(verbatimCoordinates,trs,utm))


### if determiner column is empty, make the first collector the determiner ###
# replacing NA's with blank strings
sym_col_remove$identifiedBy[is.na(sym_col_remove$identifiedBy)] <- ""
# moving names over
sym_col_remove$identifiedBy <- ifelse(sym_col_remove$identifiedBy == "",
                                        sym_col_remove$recordedBy,
                                        sym_col_remove$identifiedBy)


### removing spaces after names ###
sym_col_remove$recordedBy <- gsub("\\s$","",sym_col_remove$recordedBy)
sym_col_remove$identifiedBy <- gsub("\\s$","",sym_col_remove$identifiedBy)
sym_col_remove$associatedCollectors <- gsub("\\s$","",sym_col_remove$associatedCollectors)


### splitting recorded column into first, last, and middle names ###
# removing the comma between a last name and Jr, Sr, II, or III
sym_col_remove <- sym_col_remove %>%
  mutate(recordedBy=str_replace_all(recordedBy,"\\, Jr\\.?"," Jr.")) %>%
  mutate(recordedBy=str_replace_all(recordedBy,"\\, Sr\\.?"," Sr.")) %>%
  mutate(recordedBy=str_replace_all(recordedBy,"\\, II\\.?"," II")) %>%
  mutate(recordedBy=str_replace_all(recordedBy,"\\, III\\.?"," III"))
# if multiple names are separated by commas, replace the comma with "and"
sym_col_remove$recordedBy <- ifelse((str_detect(sym_col_remove$recordedBy, "[A-Za-zÀ-ÖØ-öø-ÿ]+\\.?\\s[A-Za-zÀ-ÖØ-öø-ÿ]+\\.?\\,\\s?(?!and)")) == TRUE,
                   str_replace_all(sym_col_remove$recordedBy,"\\,\\s?(?!and)"," and "),
                   sym_col_remove$recordedBy)
# replace any "et al" with nothing
sym_col_remove$recordedBy <- ifelse((str_detect(sym_col_remove$recordedBy, "et al\\.?")) == TRUE,
                                    str_replace_all(sym_col_remove$recordedBy,"et al\\.?",""),
                                    sym_col_remove$recordedBy)
# splitting the first collector name into first, middle, and last
# this might output a warning message that missing pieces are filled with NA - this is okay, it will happen if there aren't two or more collectors listed
sym_names <- sym_col_remove %>%
  mutate(recordedBy= gsub(pattern = "(\\w+\\,) (\\w+)","\\2 \\1", recordedBy)) %>% # fixing "Last name, First name" to be "First name Last name"
  mutate(recordedBy=str_replace_all(recordedBy,"\\s?\\;\\s?(?!and)"," and ")) %>% # this replaces any ";" separators with "and"
  mutate(recordedBy=str_replace_all(recordedBy," or "," and ")) %>% # this replaces any "or" separators with "and"
  mutate(recordedBy=str_replace_all(recordedBy," & "," and ")) %>% # this replaces any "&" separators with "and"
  mutate(recordedBy=str_replace_all(recordedBy," with "," and ")) %>% # this replaces any "with" separators with "and"
  mutate(recordedBy=str_replace_all(recordedBy," [Dd]e [Ll]a "," dela")) %>% # making sure "de la" doesn't get separated into separate names
  mutate(recordedBy=str_replace_all(recordedBy," [Dd]e "," de")) %>% # making sure "de" doesn't get separated into a separate name
  mutate(recordedBy=str_replace_all(recordedBy," [Dd]u "," du")) %>% # making sure "du" doesn't get separated into a separate name
  mutate(recordedBy=str_replace_all(recordedBy," [Vv]on "," von")) %>% # making sure "von" doesn't get separated into a separate name
  separate(col = recordedBy, into = c("first_collect", "additional_collect"), sep = " and ", extra = "merge") %>%
  extract(first_collect,
          into = c("Collector.First.Name1", "Collector.Middle1", "Collector.Last.Name1"),
          regex = "([A-Za-zÀ-ÖØ-öø-ÿ]+\\.?(?:\\-[A-Za-zÀ-ÖØ-öø-ÿ]+)?)\\s*(?:([^\\s,]+)\\s)?(?:([A-Za-zÀ-ÖØ-öø-ÿ]+(?:\\sJr\\.)?))?") %>%
  mutate(Collector.Last.Name1 = gsub(pattern = "^(de)(la)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2 \\3", Collector.Last.Name1)) %>%
  mutate(Collector.Last.Name1 = gsub(pattern = "^(de)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2", Collector.Last.Name1)) %>%
  mutate(Collector.Last.Name1 = gsub(pattern = "^(du)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2", Collector.Last.Name1)) %>%
  mutate(Collector.Last.Name1 = gsub(pattern = "^(von)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2", Collector.Last.Name1))
# pasting in extra names as additional collectors
sym_names$additional_collect[is.na(sym_names$additional_collect)] <- ""
sym_names$associatedCollectors <- ifelse(sym_names$additional_collect == "",
                                         sym_names$associatedCollectors,
                                         sym_names$additional_collect)
sym_names = subset(sym_names, select = -c(additional_collect))


### splitting identifier column into first, last, and middle names ###
# we're only choosing the first name listed for determiner because "determiner 2" means a different determination was made by someone, not an associated determiner
# removing the comma between a last name and Jr, Sr, II, or III
sym_names <- sym_names %>%
  mutate(identifiedBy=str_replace_all(identifiedBy,"\\, Jr\\.?"," Jr.")) %>%
  mutate(identifiedBy=str_replace_all(identifiedBy,"\\, Sr\\.?"," Sr.")) %>%
  mutate(identifiedBy=str_replace_all(identifiedBy,"\\, II\\.?"," II")) %>%
  mutate(identifiedBy=str_replace_all(identifiedBy,"\\, III\\.?"," III"))
# if multiple names are separated by commas, replace the comma with "and"
sym_names$identifiedBy <- ifelse((str_detect(sym_names$identifiedBy, "[A-Za-zÀ-ÖØ-öø-ÿ]+\\.?\\s[A-Za-zÀ-ÖØ-öø-ÿ]+\\.?\\,\\s?(?!and)")) == TRUE,
                                    str_replace_all(sym_names$identifiedBy,"\\,\\s?(?!and)"," and "),
                                    sym_names$identifiedBy)
# splitting the first collector name into first, middle, and last
# this might output a warning message that missing pieces are filled with NA - this is okay, it will happen if there aren't two or more determiners listed
sym_names <- sym_names %>%
  mutate(identifiedBy= gsub(pattern = "(\\w+\\,) (\\w+)","\\2 \\1", identifiedBy)) %>% # fixing "Last name, First name" to be "First name Last name"
  mutate(identifiedBy=str_replace_all(identifiedBy,"\\s?\\;\\s?(?!and)"," and ")) %>% # this replaces any ";" separators with "and"
  mutate(identifiedBy=str_replace_all(identifiedBy," or "," and ")) %>% # this replaces any "or" separators with "and"
  mutate(identifiedBy=str_replace_all(identifiedBy," & "," and ")) %>% # this replaces any "&" separators with "and"
  mutate(identifiedBy=str_replace_all(identifiedBy," with "," and ")) %>% # this replaces any "with" separators with "and"
  mutate(identifiedBy=str_replace_all(identifiedBy," [Dd]e [Ll]a "," dela")) %>% # making sure "de la" doesn't get separated into separate names
  mutate(identifiedBy=str_replace_all(identifiedBy," [Dd]e "," de")) %>% # making sure "de" doesn't get separated into a separate name
  mutate(identifiedBy=str_replace_all(identifiedBy," [Dd]u "," du")) %>% # making sure "du" doesn't get separated into a separate name
  mutate(identifiedBy=str_replace_all(identifiedBy," [Vv]on "," von")) %>% # making sure "von" doesn't get separated into a separate name
  mutate(identifiedBy=str_replace_all(identifiedBy,"[Nn]omenclatur[A-Za-z]+ [Uu]pdate","NomenclatureUpdate")) %>% # making sure 'nomenclature update' doesn't get split into first and last name
  mutate(identifiedBy=str_replace_all(identifiedBy,"[Nn]omenclatur[A-Za-z]+ [Aa]djustment","NomenclatureUpdate")) %>% # making sure 'nomenclature adjustment' doesn't get split into first and last name
  mutate(identifiedBy=str_replace_all(identifiedBy,"[Nn]om. [Rr]ev","NomenclatureUpdate")) %>% # making sure 'nom. rev' doesn't get split into first and last name
  separate(col = identifiedBy, into = c("first_ID", "additional_ID"), sep = " and ", extra = "merge") %>%
  extract(first_ID,
          into = c("Determiner.First.Name1", "Determiner.Middle1", "Determiner.Last.Name1"),
          regex = "([A-Za-zÀ-ÖØ-öø-ÿ]+\\.?(?:\\-[A-Za-zÀ-ÖØ-öø-ÿ]+)?)\\s*(?:([^\\s,]+)\\s)?(?:([A-Za-zÀ-ÖØ-öø-ÿ]+(?:\\sJr\\.)?))?") %>%
  mutate(Determiner.Last.Name1 = gsub(pattern = "^(de)(la)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2 \\3", Determiner.Last.Name1)) %>%
  mutate(Determiner.Last.Name1 = gsub(pattern = "^(de)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2", Determiner.Last.Name1)) %>%
  mutate(Determiner.Last.Name1 = gsub(pattern = "^(du)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2", Determiner.Last.Name1)) %>%
  mutate(Determiner.Last.Name1 = gsub(pattern = "^(von)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2",Determiner.Last.Name1))
# removing additional determiners since we don't need them
sym_names = subset(sym_names, select = -c(additional_ID))


### splitting additional collectors into their names ###
# removing the comma between a last name and Jr, Sr, II, or III
sym_names <- sym_names %>%
  mutate(associatedCollectors=str_replace_all(associatedCollectors,"\\, Jr\\.?"," Jr.")) %>%
  mutate(associatedCollectors=str_replace_all(associatedCollectors,"\\, Sr\\.?"," Sr.")) %>%
  mutate(associatedCollectors=str_replace_all(associatedCollectors,"\\, II\\.?"," II")) %>%
  mutate(associatedCollectors=str_replace_all(associatedCollectors,"\\, III\\.?"," III"))
# if multiple names are separated by commas, replace the comma with "and"
sym_names$associatedCollectors <- ifelse((str_detect(sym_names$associatedCollectors, "[A-Za-zÀ-ÖØ-öø-ÿ]+\\.?\\s[A-Za-zÀ-ÖØ-öø-ÿ]+\\.?\\,\\s?(?!and)")) == TRUE,
                                 str_replace_all(sym_names$associatedCollectors,"\\,\\s?(?!and)"," and "),
                                 sym_names$associatedCollectors)
sym_names$associatedCollectors <- ifelse((str_detect(sym_names$associatedCollectors, "et al\\.?")) == TRUE,
                                              str_replace_all(sym_names$associatedCollectors,"\\;?\\s?et al\\.?",""),
                                              sym_names$associatedCollectors)
# splitting the first collector name into first, middle, and last
# this might output a warning message that missing pieces are filled with NA - this is okay, it will happen if there aren't two or more determiners listed
sym_names <- sym_names %>%
  mutate(associatedCollectors= gsub(pattern = "([A-Za-z0-9]+\\,) ([A-Za-z0-9]+)","\\2 \\1", associatedCollectors)) %>% # fixing "Last name, First name" to be "First name Last name"
  mutate(associatedCollectors=str_replace_all(associatedCollectors,"\\s?\\;\\s?(?!and)"," and ")) %>% # this replaces any ";" separators with "and"
  mutate(associatedCollectors=str_replace_all(associatedCollectors," or "," and ")) %>% # this replaces any "or" separators with "and"
  mutate(associatedCollectors=str_replace_all(associatedCollectors," & "," and ")) %>% # this replaces any "&" separators with "and"
  mutate(associatedCollectors=str_replace_all(associatedCollectors," with "," and ")) %>% # this replaces any "with" separators with "and"
  mutate(associatedCollectors=str_replace_all(associatedCollectors," [Dd]e [Ll]a "," dela")) %>% # making sure "de la" doesn't get separated into separate names
  mutate(associatedCollectors=str_replace_all(associatedCollectors," [Dd]e "," de")) %>% # making sure "de" doesn't get separated into a separate name
  mutate(associatedCollectors=str_replace_all(associatedCollectors," [Dd]u "," du")) %>% # making sure "du" doesn't get separated into a separate name
  mutate(associatedCollectors=str_replace_all(associatedCollectors," [Vv]on "," von")) %>% # making sure "von" doesn't get separated into a separate name
  separate(col = associatedCollectors, into = c("second", "third", "fourth" ,"fifth","sixth","seventh"), sep = " and ") %>%
  extract(second,
          into = c("Collector.First.Name2", "Collector.Middle2", "Collector.Last.Name2"),
          regex = "([A-Za-zÀ-ÖØ-öø-ÿ]+\\.?(?:\\-[A-Za-zÀ-ÖØ-öø-ÿ]+)?)\\s*(?:([^\\s,]+)\\s)?(?:([A-Za-zÀ-ÖØ-öø-ÿ]+(?:\\sJr\\.)?))?") %>%
  extract(third,
          into = c("Collector.First.Name3", "Collector.Middle3", "Collector.Last.Name3"),
          regex = "([A-Za-zÀ-ÖØ-öø-ÿ]+\\.?(?:\\-[A-Za-zÀ-ÖØ-öø-ÿ]+)?)\\s*(?:([^\\s,]+)\\s)?(?:([A-Za-zÀ-ÖØ-öø-ÿ]+(?:\\sJr\\.)?))?") %>%
  extract(fourth,
          into = c("Collector.First.Name4", "Collector.Middle4", "Collector.Last.Name4"),
          regex = "([A-Za-zÀ-ÖØ-öø-ÿ]+\\.?(?:\\-[A-Za-zÀ-ÖØ-öø-ÿ]+)?)\\s*(?:([^\\s,]+)\\s)?(?:([A-Za-zÀ-ÖØ-öø-ÿ]+(?:\\sJr\\.)?))?") %>%
  extract(fifth,
          into = c("Collector.First.Name5", "Collector.Middle5", "Collector.Last.Name5"),
          regex = "([A-Za-zÀ-ÖØ-öø-ÿ]+\\.?(?:\\-[A-Za-zÀ-ÖØ-öø-ÿ]+)?)\\s*(?:([^\\s,]+)\\s)?(?:([A-Za-zÀ-ÖØ-öø-ÿ]+(?:\\sJr\\.)?))?") %>%
  extract(sixth,
          into = c("Collector.First.Name6", "Collector.Middle6", "Collector.Last.Name6"),
          regex = "([A-Za-zÀ-ÖØ-öø-ÿ]+\\.?(?:\\-[A-Za-zÀ-ÖØ-öø-ÿ]+)?)\\s*(?:([^\\s,]+)\\s)?(?:([A-Za-zÀ-ÖØ-öø-ÿ]+(?:\\sJr\\.)?))?") %>%
  extract(seventh,
          into = c("Collector.First.Name7", "Collector.Middle7", "Collector.Last.Name7"),
          regex = "([A-Za-zÀ-ÖØ-öø-ÿ]+\\.?(?:\\-[A-Za-zÀ-ÖØ-öø-ÿ]+)?)\\s*(?:([^\\s,]+)\\s)?(?:([A-Za-zÀ-ÖØ-öø-ÿ]+(?:\\sJr\\.)?))?") %>%
  mutate(Collector.Last.Name2 = gsub(pattern = "^(de)(la)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2 \\3", Collector.Last.Name2)) %>%
  mutate(Collector.Last.Name2 = gsub(pattern = "^(de)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2", Collector.Last.Name2)) %>%
  mutate(Collector.Last.Name3 = gsub(pattern = "^(de)(la)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2 \\3", Collector.Last.Name3)) %>%
  mutate(Collector.Last.Name3 = gsub(pattern = "^(de)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2", Collector.Last.Name3)) %>%
  mutate(Collector.Last.Name4 = gsub(pattern = "^(de)(la)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2 \\3", Collector.Last.Name4)) %>%
  mutate(Collector.Last.Name4 = gsub(pattern = "^(de)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2", Collector.Last.Name4)) %>%
  mutate(Collector.Last.Name5 = gsub(pattern = "^(de)(la)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2 \\3", Collector.Last.Name5)) %>%
  mutate(Collector.Last.Name5 = gsub(pattern = "^(de)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2", Collector.Last.Name5)) %>%
  mutate(Collector.Last.Name6 = gsub(pattern = "^(de)(la)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2 \\3", Collector.Last.Name6)) %>%
  mutate(Collector.Last.Name6 = gsub(pattern = "^(de)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2", Collector.Last.Name6)) %>%
  mutate(Collector.Last.Name7 = gsub(pattern = "^(de)(la)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2 \\3", Collector.Last.Name7)) %>%
  mutate(Collector.Last.Name7 = gsub(pattern = "^(de)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2", Collector.Last.Name7)) %>%
  mutate(Collector.Last.Name2 = gsub(pattern = "^(du)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2", Collector.Last.Name2)) %>%
  mutate(Collector.Last.Name2 = gsub(pattern = "^(von)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2",Collector.Last.Name2)) %>%
  mutate(Collector.Last.Name3 = gsub(pattern = "^(du)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2", Collector.Last.Name3)) %>%
  mutate(Collector.Last.Name3 = gsub(pattern = "^(von)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2",Collector.Last.Name3)) %>%
  mutate(Collector.Last.Name4 = gsub(pattern = "^(du)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2", Collector.Last.Name4)) %>%
  mutate(Collector.Last.Name4 = gsub(pattern = "^(von)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2",Collector.Last.Name4)) %>%
  mutate(Collector.Last.Name5 = gsub(pattern = "^(du)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2", Collector.Last.Name5)) %>%
  mutate(Collector.Last.Name5 = gsub(pattern = "^(von)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2",Collector.Last.Name5)) %>%
  mutate(Collector.Last.Name6 = gsub(pattern = "^(du)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2", Collector.Last.Name6)) %>%
  mutate(Collector.Last.Name6 = gsub(pattern = "^(von)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2",Collector.Last.Name6)) %>%
  mutate(Collector.Last.Name7 = gsub(pattern = "^(du)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2", Collector.Last.Name7)) %>%
  mutate(Collector.Last.Name7 = gsub(pattern = "^(von)([A-Za-zÀ-ÖØ-öø-ÿ]+)", "\\1 \\2",Collector.Last.Name7))


### transforming dates so that they are all one format ###
# not currently working - takes years w/o the full century and puts them in the 2000s (e.g., 54 becomes 2054)
#sym_names$eventDate <- as.Date(sym_names$eventDate, tryFormats = c("%Y-%m-%d","%m/%d/%y","%Y"))


### making duplicate dataframe containing old column names ###
# this is used below to print out barcodes for samples missing a genus
genus_check_df <- sym_names


### making columns that have duplicate info in specify
sym_names$verbatimCollNum <- sym_names$recordNumber
sym_names$verbatimDate <- sym_names$verbatimEventDate
sym_names$verbatimAnnDate <- sym_names$dateIdentified
sym_names$BarCode <- sym_names$catalogNumber


### removing unneeded column
sym_names <- subset(sym_names, select=-c(id, cultivationStatus))


### replace periods in column names with spaces ###
names(sym_names) <- gsub(x = names(sym_names), pattern = "\\.", replacement = " ")


### re-naming sym columns to be correct for specify ###
colnames(sym_names)[colnames(sym_names) == "occurrenceID"] <- "GUID"
colnames(sym_names)[colnames(sym_names) == "recordNumber"] <- "Collection Number"
colnames(sym_names)[colnames(sym_names) == "verbatimCollNum"] <- "Verbatim Coll #"
colnames(sym_names)[colnames(sym_names) == "otherCatalogNumbers"] <- "MSC Accession #"
colnames(sym_names)[colnames(sym_names) == "catalogNumber"] <- "Bar Code # 1"
colnames(sym_names)[colnames(sym_names) == "BarCode"] <- "Duplicate BarCode"
colnames(sym_names)[colnames(sym_names) == "genus"] <- "Genus1"
colnames(sym_names)[colnames(sym_names) == "specificEpithet"] <- "Species1"
colnames(sym_names)[colnames(sym_names) == "country"] <- "Country"
colnames(sym_names)[colnames(sym_names) == "stateProvince"] <- "State"
colnames(sym_names)[colnames(sym_names) == "county"] <- "County"
colnames(sym_names)[colnames(sym_names) == "substrate"] <- "Soil Type"
colnames(sym_names)[colnames(sym_names) == "locality"] <- "Locality"
colnames(sym_names)[colnames(sym_names) == "decimalLatitude"] <- "Latitude1"
colnames(sym_names)[colnames(sym_names) == "decimalLongitude"] <- "Longitude1"
colnames(sym_names)[colnames(sym_names) == "habitat"] <- "Verbatim Habitat"
colnames(sym_names)[colnames(sym_names) == "associatedTaxa"] <- "Associated Species"
colnames(sym_names)[colnames(sym_names) == "individualCount"] <- "Abundance"
colnames(sym_names)[colnames(sym_names) == "occurrenceRemarks"] <- "Comments (Collection Data)"
colnames(sym_names)[colnames(sym_names) == "infraspecificEpithet"] <- "Variety1"
colnames(sym_names)[colnames(sym_names) == "eventDate"] <- "Date"
colnames(sym_names)[colnames(sym_names) == "verbatimEventDate"] <- "Verbatim Date"
colnames(sym_names)[colnames(sym_names) == "dateIdentified"] <- "Ann Date 1"
colnames(sym_names)[colnames(sym_names) == "verbatimAnnDate"] <- "Verb Ann Date 1"
colnames(sym_names)[colnames(sym_names) == "verbatimAttributes"] <- "Field Characteristics"
colnames(sym_names)[colnames(sym_names) == "latLong"] <- "Verbatim Lat/Long"
colnames(sym_names)[colnames(sym_names) == "TownshipDirection"] <- "Township Direction"
colnames(sym_names)[colnames(sym_names) == "RangeDirection"] <- "Range Direction"
colnames(sym_names)[colnames(sym_names) == "SectionPart"] <- "Section Part"
colnames(sym_names)[colnames(sym_names) == "X  of Sheets"] <- "# of Sheets"
colnames(sym_names)[colnames(sym_names) == "General  nComments"] <- "General \\nComments"
colnames(sym_names)[colnames(sym_names) == "Catalog  "] <- "Catalog #"
colnames(sym_names)[colnames(sym_names) == "PrepType1"] <- "Prep Type1"
colnames(sym_names)[colnames(sym_names) == "identifier"] <- "External image URL"


### replacing NA's with blank strings ###
sym_names[is.na(sym_names)] <- ""


### save output as a csv file ###
# fixing encoding of columns
#Encoding(sym_names$`Collector First Name1`) = "latin1"
#Encoding(sym_names$`Collector Middle1`) = "latin1"
#Encoding(sym_names$`Collector Last Name1`) = "latin1"
#Encoding(sym_names$`Collector First Name2`) = "latin1"
#Encoding(sym_names$`Collector Middle2`) = "latin1"
#Encoding(sym_names$`Collector Last Name2`) = "latin1"
#Encoding(sym_names$`Collector First Name3`) = "latin1"
#Encoding(sym_names$`Collector Middle3`) = "latin1"
#Encoding(sym_names$`Collector Last Name3`) = "latin1"
#Encoding(sym_names$`Collector First Name4`) = "latin1"
#Encoding(sym_names$`Collector Middle4`) = "latin1"
#Encoding(sym_names$`Collector Last Name4`) = "latin1"
#Encoding(sym_names$`Collector First Name5`) = "latin1"
#Encoding(sym_names$`Collector Middle5`) = "latin1"
#Encoding(sym_names$`Collector Last Name5`) = "latin1"
#Encoding(sym_names$`Collector First Name6`) = "latin1"
#Encoding(sym_names$`Collector Middle6`) = "latin1"
#Encoding(sym_names$`Collector Last Name6`) = "latin1"
#Encoding(sym_names$`Collector First Name7`) = "latin1"
#Encoding(sym_names$`Collector Middle7`) = "latin1"
#Encoding(sym_names$`Collector Last Name7`) = "latin1"
#Encoding(sym_names$`Determiner First Name1`) = "latin1"
#Encoding(sym_names$`Determiner Middle1`) = "latin1"
#Encoding(sym_names$`Determiner Last Name1`) = "latin1"
# can change "sym_to_spec" to whatever name you want the new file to be named
write_excel_csv(sym_names, file="sym_to_spec.csv")


### sending a warning message if genus column is empty ###
# shows which barcodes contain an empty genus
check_genus <- function(df){
  if (any(is.na(genus_check_df$genus)) ){
    print(genus_check_df$catalogNumber[is.na(genus_check_df$genus)])
  }
  else {
    print()
  }
}
df1 <- as.data.frame(check_genus(genus_check_df))

write.csv(df1, "missing_genus.csv",row.names=F)

