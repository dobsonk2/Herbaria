# Herbaria data management
Code for manipulating MSU herbaria data

Collaborators: Kara Dobson, Matt Chansler, James Mickley

Each script transforms a given data type into the needed data format. For example, the Label_to_Specify.R script takes data formatted into the label template and transforms it to fit the Specify data format.

The Unique_Symbiota_data.R script pulls in Symbiota data and compares its barcodes to all barcodes present in Specify to determine which samples in the Symbiota data are unique to Symbiota. The Symbiota data is then subset to only the unique barcodes, which can then be run through Symbiota_to_Specify.R to prep those data for Specify. The Unique_Symbiota_data.R script then amends the Specify barcode dataframe to contain the new barcodes from Symbiota, creating an up-to-date list of barcodes in Specify.
