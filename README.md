# Herbaria data management
Code for manipulating MSU herbaria data

Collaborators: Kara Dobson, Matt Chansler, James Mickley

Each script transforms a given data type into the needed data format. For example, the Label_to_Specify.R script takes data formatted into the label template and transforms it to fit the Specify data format.

The Unique_Symbiota_data.R script pulls in Symbiota data and compares its barcodes to all barcodes present in Specify to determine which samples in the Symbiota data are unique to Symbiota. The Symbiota data is then subset to only the unique barcodes, which can then be run through Symbiota_to_Specify.R to prep those data for Specify. The Unique_Symbiota_data.R script then amends the Specify barcode dataframe to contain the new barcodes from Symbiota, creating an up-to-date list of barcodes in Specify.

If R encoding is not set to UTF-8, be sure to set the encoding to open the script(s) with UTF-8 encoding. The name regex will not be correct if the script is opened with a different encoding (UTF-8 is the default encoding for Mac - to check on a PC, go to file -> Reopen with encoding, and select UTF-8).

Also note this code is written to work with latin1 encoding for data downloaded from Symbiota - you can select which data type you want to download from Symbiota. If data is downloaded as UTF-8, the code that specifies 'latin1' when reading in a csv can be removed.
