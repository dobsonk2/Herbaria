# TITLE:          Herbarium data frame manipulations
# AUTHORS:        Kara Dobson
# DATA INPUT:     Data imported as csv files
# DATA OUTPUT:    Herbarium data transformed into the needed template
# DATE:           May-June 2023


# Clear all existing data
rm(list=ls())

# Set working directory
setwd("~/Documents/research/Herbarium") # set to wherever files are stored

# Load packages
library(tidyverse)




############################### Label template to symbiota template #################################
### notes for things to check in the raw excel file before running this code:
# make sure only 1 name is present for "primary collector", all other collectors should go under "additional collectors"
# make sure additional collectors are separated by commas or "and"


### Read in data ###
# export the excel file to the UTF-8 CSV format
# change the name for the label file from "test.csv" to the name of the file you want to re-format
label <- read.csv("official_test.csv",fileEncoding="UTF-8")
#symbiota_template <- read.csv("Symbiota_template.csv", check.names=F) #commented out this line because we don't need it


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


### removing columns from label template that symbiota template does not need ###
label_col_remove = subset(label2, select = -c(Kingdom,Infra.epithet,Authority,Associated_with.,With.,Det.different.,Collection.number.1,
                                              GPS.accuracy,Accuracy.unit,Datum,Common.name))


### removing any spaces before or after all entries from all columns
label_col_remove <- as.data.frame(apply(label_col_remove, 2, function(y) gsub("\\s$","", y)))
label_col_remove <- as.data.frame(apply(label_col_remove, 2, function(y) gsub("^\\s","", y)))


### re-naming label columns to be correct for symbiota ###
colnames(label_col_remove)[colnames(label_col_remove) == "Collection.number"] ="recordNumber"
colnames(label_col_remove)[colnames(label_col_remove) == "Barcode"] ="catalogNumber"
colnames(label_col_remove)[colnames(label_col_remove) == "Accession.number"] ="otherCatalogNumbers"
colnames(label_col_remove)[colnames(label_col_remove) == "Cultivated"] ="cultivationStatus"
colnames(label_col_remove)[colnames(label_col_remove) == "Family"] ="family"
colnames(label_col_remove)[colnames(label_col_remove) == "Genus"] ="genus"
colnames(label_col_remove)[colnames(label_col_remove) == "Species"] ="specificEpithet"
colnames(label_col_remove)[colnames(label_col_remove) == "State"] ="state"
colnames(label_col_remove)[colnames(label_col_remove) == "County"] ="county"
colnames(label_col_remove)[colnames(label_col_remove) == "Locality"] ="locality"
colnames(label_col_remove)[colnames(label_col_remove) == "Latitude"] ="decimalLatitude"
colnames(label_col_remove)[colnames(label_col_remove) == "Longitude"] ="decimalLongitude"
colnames(label_col_remove)[colnames(label_col_remove) == "Habitat"] ="habitat"
colnames(label_col_remove)[colnames(label_col_remove) == "Associated.species"] ="associatedTaxa"
colnames(label_col_remove)[colnames(label_col_remove) == "Abundance"] ="individualCount"
colnames(label_col_remove)[colnames(label_col_remove) == "Description"] ="verbatimAttributes"
colnames(label_col_remove)[colnames(label_col_remove) == "Primary.collector"] ="recordedBy"
colnames(label_col_remove)[colnames(label_col_remove) == "Additional.collectors"] ="associatedCollectors"
colnames(label_col_remove)[colnames(label_col_remove) == "Date.collected"] ="eventDate"
colnames(label_col_remove)[colnames(label_col_remove) == "Determiner"] ="identifiedBy"
colnames(label_col_remove)[colnames(label_col_remove) == "Date.determined"] ="dateIdentified"
colnames(label_col_remove)[colnames(label_col_remove) == "Infrarank"] ="verbatimTaxonRank"
colnames(label_col_remove)[colnames(label_col_remove) == "infraEpithet"] ="infraSpecificEpithet"
colnames(label_col_remove)[colnames(label_col_remove) == "Substrate"] ="substrate"


### making combined scientific name column (merging genus and species) ###
label_col_remove$scientificName <- paste0(label_col_remove$genus," ",label_col_remove$specificEpithet)


### making combined occurrence remarks column (merging additional notes & iNat URL) ###
# first, removing punctuation from the end of each additional notes cell (this is so that we don't end up with '..' when merging with the iNat data)
label_col_remove$Additional.notes <- sub('\\.\\s?$', '', label_col_remove$Additional.notes)
# if an iNat URL exists, then we're merging it with the additional notes to form "general comments" column
# if an iNat URL does not exist, we're just pasting in additional notes as "general comments"
label_col_remove$occurrenceRemarks <- ifelse(label_col_remove$iNat.URL == "",
                                             label_col_remove$Additional.notes,
                                             paste0(label_col_remove$Additional.notes,". iNaturalist URL: ",label_col_remove$iNat.URL))
# removing ". " from cells that just had iNat url
label_col_remove$occurrenceRemarks <- sub('^\\.\\s', '', label_col_remove$occurrenceRemarks)
# adding a period to the end of the general comments
label_col_remove$occurrenceRemarks <- ifelse(label_col_remove$occurrenceRemarks == "",
                                             label_col_remove$occurrenceRemarks,
                                             paste0(label_col_remove$occurrenceRemarks,"."))
# just a check to see how the general comments column outputs look
unique(label_col_remove$occurrenceRemarks)


### adding "processingStatus" column
# if the specimen contains latitude, then processingStatus = 3
# if not, processingStatus = 2
label_col_remove$processingStatus <- ifelse(label_col_remove$decimalLatitude == "",
                                            2,
                                            3)


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






############################### Symbiota template to specify template #################################

### Read in data ###
# change the name for the label file from "test.csv" to the name of the file you want to re-format
label <- read.csv("occurrences4.csv",fileEncoding="UTF-8")


### removing columns from symbiota data that specify template does not need ###
label_col_remove = subset(label, select = -c(id, institutionCode, collectionCode, ownerInstitutionCode,basisOfRecord,
                                             higherClassification, kingdom, phylum, class,
                                             order,family,scientificName,taxonID,scientificNameAuthorship,subgenus,
                                             verbatimTaxonRank,taxonRank,identificationReferences,identificationRemarks,
                                             taxonRemarks,identificationQualifier,typeStatus,eventDate2,year,month,day,
                                             startDayOfYear,endDayOfYear,verbatimEventDate,fieldNumber,
                                             eventID,informationWithheld,dataGeneralizations,dynamicProperties,
                                             associatedOccurrences,associatedSequences,reproductiveCondition,
                                             establishmentMeans,cultivationStatus,lifeStage,sex,
                                             preparations,locationID,continent,waterBody,islandGroup,island,municipality,
                                             locationRemarks,localitySecurity,localitySecurityReason,geodeticDatum,
                                             coordinateUncertaintyInMeters,georeferencedBy,georeferenceProtocol,
                                             georeferenceSources,georeferenceVerificationStatus,georeferenceRemarks,
                                             minimumElevationInMeters,maximumElevationInMeters,minimumDepthInMeters,
                                             maximumDepthInMeters,verbatimDepth,verbatimElevation,disposition,
                                             language,recordEnteredBy,modified,sourcePrimaryKey.dbpk,collID,
                                             recordID,references))


### adding in empty columns that are created during specify processing ###
label_col_remove$Catalog.. <- NA
label_col_remove$Initials <- "M.T.C." # Change this to your initials if you're doing the upload


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


### if determiner column is empty, make the determiner the first collector ###
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
# multiple names that aren't separated by commas or "and" will need corrected (e.g., semicolons, or "with")
    # note: if multiple names are within "recordedBy", its probably easiest to separate this into "recordedBy" and "associatedCollectors"
    # in the raw csv before reading it into R. 
# the incorrect format goes first, followed by the correct format
# can also change "identifiedBy" to "recordedBy" or "associatedCollectors" if those columns contain weird name formats
label_col_remove$identifiedBy[label_col_remove$identifiedBy == "Nugent, Therese"] <- "Therese Nugent"
label_col_remove$identifiedBy[label_col_remove$identifiedBy == "Sorrells, Ryan"] <- "Ryan Sorrells"
label_col_remove$recordedBy[label_col_remove$recordedBy == "Turpin, BC"] <- "B.C. Turpin"
label_col_remove$identifiedBy[label_col_remove$identifiedBy == "Burrows, JE"] <- "J.E. Burrows"
label_col_remove$identifiedBy[label_col_remove$identifiedBy == "Will Burke with Anna Bunting, Bailey Greene, Michael Orbain"] <- "Will Burke, Anna Bunting, Bailey Greene, Michael Orbain"
label_col_remove$identifiedBy[label_col_remove$identifiedBy == "Michael Orbain; Will Burke; Anna Bunting; Bailey Greene"] <- "Michael Orbain, Will Burke, Anna Bunting, Bailey Greene"
label_col_remove$associatedCollectors[label_col_remove$associatedCollectors == "Burrows, JE and Marks, RA"] <- "J.E. Burrows, R.A. Marks"


### splitting recorded column into first, last, and middle names ###
label_names <- label_col_remove %>%
  extract(recordedBy,
          into = c("Collector.First.Name1", "Collector.Middle1", "Collector.Last.Name1"),
          regex = "([A-Za-zÀ-ȕ]+\\.?)\\s*(?:([^\\s,]+)\\s)?(?:([A-Za-zÀ-ȕ]+(?:,\\sJr\\.)?))?")


### splitting identifier column into first, last, and middle names ###
# this might output a warning message that missing pieces are filled with NA - this is okay, it will happen if two determiners
# aren't present for every record (likely there will not be 2 for every record)
label_names <- label_names %>%
  mutate(identifiedBy=str_replace_all(identifiedBy,"\\, Jr\\.?"," Jr.")) %>% # this removes the comma before "Jr." so the next line of code works
  mutate(identifiedBy=str_replace_all(identifiedBy,"\\,\\s(?!and)"," and ")) %>% # this replaces any ", " separators with " and "
  mutate(identifiedBy=str_replace_all(identifiedBy,"\\, and "," and ")) %>% # this replaces and ", and " with " and "
  separate(col = identifiedBy, into = c("first_det", "second_det"), sep = " and ") %>%
  extract(first_det,
          into = c("Determiner.First.Name1", "Determiner.Middle1", "Determiner.Last.Name1"),
          regex = "([A-Za-zÀ-ȕ]+\\.?)\\s*(?:([^\\s,]+)\\s)?(?:([A-Za-zÀ-ȕ]+(?:,\\sJr\\.)?))?") %>%
  extract(second_det,
          into = c("Determiner.First.Name2", "Determiner.Middle2", "Determiner.Last.Name2"),
          regex = "([A-Za-zÀ-ȕ]+\\.?)\\s*(?:([^\\s,]+)\\s)?(?:([A-Za-zÀ-ȕ]+(?:,\\sJr\\.)?))?")


### splitting additional collectors into their names ###
# this might output a warning message that missing pieces are filled with NA - this is okay, it will happen if two associated collectors
# aren't present for every record (likely there will not be 2 for every record)
label_names <- label_names %>%  
  mutate(associatedCollectors=str_replace_all(associatedCollectors,"\\, Jr\\.?"," Jr.")) %>% # this removes the comma before "Jr." so the next line of code works
  mutate(associatedCollectors=str_replace_all(associatedCollectors,"\\,\\s(?!and)"," and ")) %>% # this replaces any ", " separators with " and "
  mutate(associatedCollectors=str_replace_all(associatedCollectors,"\\, and "," and ")) %>% # this replaces and ", and " with " and "
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


### replace periods in column names with spaces ###
names(label_names) <- gsub(x = names(label_names), pattern = "\\.", replacement = " ")


### making columns that have duplicate info in specify
label_names$verbatimCollNum <- label_names$recordNumber
label_names$verbatimDate <- label_names$eventDate
label_names$verbatimAnnDate <- label_names$dateIdentified
label_names$BarCode <- label_names$catalogNumber


### re-naming label columns to be correct for specify ###
colnames(label_names)[colnames(label_names) == "occurrenceID"] ="GUID"
colnames(label_names)[colnames(label_names) == "recordNumber"] ="Collection Number"
colnames(label_names)[colnames(label_names) == "otherCatalogNumbers"] ="MSC Accession #"
colnames(label_names)[colnames(label_names) == "verbatimCollNum"] ="Verbatim Coll #"
colnames(label_names)[colnames(label_names) == "catalogNumber"] ="Bar Code # 1"
colnames(label_names)[colnames(label_names) == "BarCode"] ="Duplicate BarCode"
colnames(label_names)[colnames(label_names) == "genus"] ="Genus1"
colnames(label_names)[colnames(label_names) == "specificEpithet"] ="Species1"
colnames(label_names)[colnames(label_names) == "country"] ="Country"
colnames(label_names)[colnames(label_names) == "stateProvince"] ="State"
colnames(label_names)[colnames(label_names) == "county"] ="County"
colnames(label_names)[colnames(label_names) == "substrate"] ="Soil Type"
colnames(label_names)[colnames(label_names) == "locality"] ="Locality"
colnames(label_names)[colnames(label_names) == "decimalLatitude"] ="Latitude1"
colnames(label_names)[colnames(label_names) == "decimalLongitude"] ="Longitude1"
colnames(label_names)[colnames(label_names) == "habitat"] ="Verbatim Habitat"
colnames(label_names)[colnames(label_names) == "associatedTaxa"] ="Associated Species"
colnames(label_names)[colnames(label_names) == "individualCount"] ="Abundance"
colnames(label_names)[colnames(label_names) == "occurrenceRemarks"] ="Comments (Collection Data)"
colnames(label_names)[colnames(label_names) == "infraspecificEpithet"] ="Variety1"
colnames(label_names)[colnames(label_names) == "eventDate"] ="Date"
colnames(label_names)[colnames(label_names) == "verbatimDate"] ="Verbatim Date"
colnames(label_names)[colnames(label_names) == "dateIdentified"] ="Ann Date 1"
colnames(label_names)[colnames(label_names) == "verbatimAnnDate"] ="Verb Ann Date 1"
colnames(label_names)[colnames(label_names) == "verbatimAttributes"] ="Field Characteristics"
colnames(label_names)[colnames(label_names) == "latLong"] ="Verbatim Lat/Long"
colnames(label_names)[colnames(label_names) == "TownshipDirection"] ="Township Direction"
colnames(label_names)[colnames(label_names) == "RangeDirection"] ="Range Direction"
colnames(label_names)[colnames(label_names) == "SectionPart"] ="Section Part"
colnames(label_names)[colnames(label_names) == "X  of Sheets"] ="# of Sheets"
colnames(label_names)[colnames(label_names) == "General  nComments"] ="General \\nComments"
colnames(label_names)[colnames(label_names) == "Catalog  "] ="Catalog #"


### replacing NA's with blank strings ###
label_names[is.na(label_names)] <- ""


### save output as a csv file ###
# can change "label_to_specify" to whatever name you want the new file to be named
write.csv(label_names, "sym_to_spec.csv", row.names=F, fileEncoding = "UTF-8")







# note: below is old code to go from label template to specify
# we decided it was best to go label -> symbiota -> specify so that the GUIDs are maintained across uploads
# this code below likely won't be needed unless records are only going in specify
# also note - this code hasn't been fully tested in a while, so things might not all work
############################### Label template to specify template #################################

# Read in data
# change the name for the label file from "Label_test.csv" to the name of the file you want to re-format
label <- read.csv("test.csv",fileEncoding="latin1")
specify_template <- read.csv("Specify_template.csv") #commented out this line because we don't need it

# First, removing columns from label template that specify template does not need
label_col_remove = subset(label, select = -c(Kingdom,Family,Authority,Associated_with.,With.,Det.different.))

# adding in empty columns that are created during specify processing
label_col_remove$Cataloged.Date <- NA
label_col_remove$Catalog.. <- NA
label_col_remove$MSC.Accession.. <- NA
label_col_remove$Initials <- NA
label_col_remove$Cataloger.First.Name <- NA
label_col_remove$Cataloger.Last.Name <- NA
label_col_remove$Duplicate.BarCode <- NA
label_col_remove$Handwritten.Non.English.1 <- NA
label_col_remove$Species...1 <- NA
label_col_remove$Annotation...1 <- NA
label_col_remove$Handwritten.Non.English.2 <- NA
label_col_remove$Species...2 <- NA
label_col_remove$Annotation...2 <- NA
label_col_remove$Prep.Type1 <- NA
label_col_remove$Bar.Code...1 <- NA

# note to self: adding these in as blank as placeholders for now, might need to change that after talking w/ Matt
label_col_remove$X..of.Sheets <- NA # written as "sheet X of Y" in additional notes column
label_col_remove$Comments..Collection.Data. <- NA
label_col_remove$Comments..n.SpecDeterm..1 <- NA
label_col_remove$Variety1 <- NA
label_col_remove$Ann.Date.2 <- NA
label_col_remove$Determiner.Last.Name2 <- NA # if a determiner isn't listed, put the name of collector 1
label_col_remove$Determiner.First.Name2 <- NA
label_col_remove$Determiner.Middle2 <- NA
label_col_remove$Comments..n.SpecDeterm..2 <- NA
label_col_remove$Verb.Ann.Date.2 <- NA
label_col_remove$Genus2 <- NA
label_col_remove$Species2 <- NA
label_col_remove$Variety2 <- NA
label_col_remove$Country <- NA
label_col_remove$Min.Elev <- NA
label_col_remove$Elev.Unit <- NA

# re-naming label columns to be correct for specify
colnames(label_col_remove)[colnames(label_col_remove) == "Habitat"] ="Verbatim.Habitat"
colnames(label_col_remove)[colnames(label_col_remove) == "Collection.number"] ="Verbatim.Coll.."
colnames(label_col_remove)[colnames(label_col_remove) == "Associated.species"] ="Associated.Species"
colnames(label_col_remove)[colnames(label_col_remove) == "Description"] ="Field.Characteristics"
colnames(label_col_remove)[colnames(label_col_remove) == "Date.collected"] ="Date"
colnames(label_col_remove)[colnames(label_col_remove) == "Latitude"] ="Latitude1"
colnames(label_col_remove)[colnames(label_col_remove) == "Longitude"] ="Longitude1"
colnames(label_col_remove)[colnames(label_col_remove) == "Date.determined"] ="Ann.Date.1"
colnames(label_col_remove)[colnames(label_col_remove) == "Genus"] ="Genus1"
colnames(label_col_remove)[colnames(label_col_remove) == "Species"] ="Species1"

# making columns that are duplicated in specify
label_col_remove$Verbatim.Date <- label_col_remove$Date
label_col_remove$Collection.Number <- label_col_remove$Verbatim.Coll..
label_col_remove$Verb.Ann.Date.1 <- label_col_remove$Ann.Date.1

# making combined lat/long column
label_col_remove$Verbatim.Lat.Long <- paste0(label_col_remove$Latitude1,", ",label_col_remove$Longitude1)

# merging additional notes & iNat URL into specimen description & general comments columns
# first, removing punctuation from the end of each additional notes cell (this is so that we don't end up with '..' when merging with the iNat data)
label_col_remove$Additional.notes <- sub('[[:punct:]]+$', '', label_col_remove$Additional.notes)
# if an iNat URL exists, then we're merging it with the additional notes to form "general comments" column
# if an iNat URL does not exist, we're just pasting in additional notes as "general comments"
label_col_remove$General..nComments <- ifelse(label_col_remove$iNat.URL == "",
                                              label_col_remove$Additional.notes,
                                              paste0(label_col_remove$Additional.notes,". iNaturalist URL: ",label_col_remove$iNat.URL))
# adding a period to the end of the general comments
label_col_remove$General..nComments <- ifelse(label_col_remove$General..nComments == "",
                                              NA,
                                              paste0(label_col_remove$General..nComments,"."))

# just a check to see how the general comments column outputs look
unique(label_col_remove$General..nComments)

# splitting name column into first, last, and middle names
# ([A-Za-z]+) a word of any size
# ([A-Za-z\\.]+ )? a word of any size (that may or may not be present, hence ?), that might have a dot somewhere, and a space at the end
label_names <- label_col_remove %>%
  mutate(Primary.collector = gsub('([A-Za-z]+) ([A-Za-z\\.]+ )?([A-Za-z]+)', '\\1=\\2=\\3', Primary.collector)) %>%
  separate(Primary.collector, c("Collector.First.Name1", "Collector.Middle1", "Collector.Last.Name1"), "=")

# splitting determiner column into first, last, and middle names
label_names <- label_names %>%
  mutate(Determiner = gsub('([A-Za-z]+) ([A-Za-z\\.]+ )?([A-Za-z]+)', '\\1=\\2=\\3', Determiner)) %>%
  separate(Determiner, c("Determiner.First.Name1", "Determiner.Middle1", "Determiner.Last.Name1"), "=")

# splitting additional collectors into their names
# the specify template only had columns for 3 additional collectors (collectors 2-4), but I went up to seven here (collectors 2-7)
# can limit this to three if the additional collector columns don't work in specify
label_names <- label_names %>%
  mutate(Additional.collectors=str_replace_all(Additional.collectors," and ",", ")) %>%
  separate(col = Additional.collectors, into = c("second", "third", "fourth", "fifth", "sixth", "seventh"), sep = "\\,") %>%
  mutate(second = gsub('([A-Za-z]+) ([A-Za-z\\.]+ )?([A-Za-z]+)', '\\1=\\2=\\3', second)) %>%
  separate(second, c("Collector.First.Name2", "Collector.Middle2", "Collector.Last.Name2"), "=") %>%
  mutate(third = gsub('([A-Za-z]+) ([A-Za-z\\.]+ )?([A-Za-z]+)', '\\1=\\2=\\3', third)) %>%
  separate(third, c("Collector.First.Name3", "Collector.Middle3", "Collector.Last.Name3"), "=") %>%
  mutate(fourth = gsub('([A-Za-z]+) ([A-Za-z\\.]+ )?([A-Za-z]+)', '\\1=\\2=\\3', fourth)) %>%
  separate(fourth, c("Collector.First.Name4", "Collector.Middle4", "Collector.Last.Name4"), "=") %>%
  mutate(fifth = gsub('([A-Za-z]+) ([A-Za-z\\.]+ )?([A-Za-z]+)', '\\1=\\2=\\3', fifth)) %>%
  separate(fifth, c("Collector.First.Name5", "Collector.Middle5", "Collector.Last.Name5"), "=") %>%
  mutate(sixth = gsub('([A-Za-z]+) ([A-Za-z\\.]+ )?([A-Za-z]+)', '\\1=\\2=\\3', sixth)) %>%
  separate(sixth, c("Collector.First.Name6", "Collector.Middle6", "Collector.Last.Name6"), "=") %>%
  mutate(seventh = gsub('([A-Za-z]+) ([A-Za-z\\.]+ )?([A-Za-z]+)', '\\1=\\2=\\3', seventh)) %>%
  separate(seventh, c("Collector.First.Name7", "Collector.Middle7", "Collector.Last.Name7"), "=")

# removing unneeded columns
label_names = subset(label_names, select = -c(Additional.notes,iNat.URL))

# replace periods in column names with spaces
names(label_names) <- gsub(x = names(label_names), pattern = "\\.", replacement = " ")

# manually renaming some names to match specify template
colnames(label_names)[colnames(label_names) == "Catalog  "] ="Catalog #"
colnames(label_names)[colnames(label_names) == "X  of Sheets"] ="# of Sheets"
colnames(label_names)[colnames(label_names) == "MSC Accession  "] ="MSC Accession #"
#colnames(label_names)[colnames(label_names) == "Specimen nDescription"] ="Specimen\\nDescription"
colnames(label_names)[colnames(label_names) == "General  nComments"] ="General \\nComments"
colnames(label_names)[colnames(label_names) == "MSC Accession  "] ="MSC Accession #"
colnames(label_names)[colnames(label_names) == "Comments  Collection Data "] ="Comments (Collection Data)"
colnames(label_names)[colnames(label_names) == "Verbatim Coll  "] ="Verbatim Coll #"
colnames(label_names)[colnames(label_names) == "Comments  n SpecDeterm  1"] ="Comments \\n(SpecDeterm) 1"
colnames(label_names)[colnames(label_names) == "Handwritten Non English 1"] ="Handwritten/Non-English 1"
colnames(label_names)[colnames(label_names) == "Species   1"] ="Species # 1"
colnames(label_names)[colnames(label_names) == "Annotation   1"] ="Annotation # 1"
colnames(label_names)[colnames(label_names) == "Comments  n SpecDeterm  2"] ="Comments \\n(SpecDeterm) 2"
colnames(label_names)[colnames(label_names) == "Species   2"] ="Species # 2"
colnames(label_names)[colnames(label_names) == "Annotation   2"] ="Annotation # 2"
colnames(label_names)[colnames(label_names) == "Bar Code   1"] ="Bar Code # 1"
colnames(label_names)[colnames(label_names) == "Handwritten Non English 2"] ="Handwritten/Non-English 2"
colnames(label_names)[colnames(label_names) == "Verbatim Lat Long"] ="Verbatim Lat/Long"

# replacing NA's with blank strings
label_name9[is.na(label_names)] <- ""

# save output as a csv file
# can change "label_to_specify" to whatever name you want the new file to be named
write.csv(label_names, "test1.csv", row.names=F)


