# TITLE:          Herbarium data: Specify -> Symbiota
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


############################### Specify template to Symbiota template #################################
# Specify data downloads automatically transform the columns to be correct for Symbiota
# we just need to fix the way the names are written


### Read in data ###
# change the name for the sym file from "test.csv" to the name of the file you want to re-format
spec <- read_csv("specify_data_test.csv")
spec <- as.data.frame(spec)


### fix naming structure ###
# making names "First name Last name" rather than "Last name, First name"
spec2 <- spec %>%
  mutate(recordedBy = gsub(pattern = "([A-Za-z0-9]*\\s?[A-Za-z0-9]+\\,) ([A-Za-z0-9]+\\.?\\w?\\.?)","\\2 \\1", recordedBy)) %>%
  mutate(recordedBy = gsub(pattern = ",$", "", recordedBy)) %>%
  mutate(recordedBy = gsub(pattern = ",;", "; ", recordedBy)) %>%
  mutate(recordedBy = gsub(pattern = "  ", " ", recordedBy))


### fixing encoding of name columns ###
Encoding(spec2$recordedBy) = "latin1"
# can change "spec_to_sym" to whatever name you want the new file to be named
write_excel_csv(spec2, file="spec_to_sym.csv")
