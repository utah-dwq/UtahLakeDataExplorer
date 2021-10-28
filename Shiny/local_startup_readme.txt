###Utah Lake Data Explorer
#For help, contact Jake Vander Laan (jvander@utah.gov)

###ReadMe
The Utah Lake Data Explorer application requires the installation of R and several R pacakges.

To install and run the application on a local computer:
1. Unzip this folder to a known location
2. Download and install R (https://cran.cnr.berkeley.edu/)
3. Install required R packages:
	a) Open R console
	b) Install required applications by entering:
	install.packages("lubridate")
	install.packages("reshape2")
	install.packages("gplots")
	install.packages("sciplot")
	install.packages("sp")
	install.packages("RColorBrewer")
	install.packages("raster")
	install.packages("rgdal")
	install.packages("gdalUtils")
	install.packages("mapview")
	install.packages("Matrix")
	install.packages("gdistance")
	install.packages("ipdw")
	install.packages("shiny")
4. To run the app:
	a) Open R console
	b) Load the shiny package by entering: library(shiny)
	c) Run the app with the function by pointing to your unzipped application folder: runApp("C:\\path\\to\\your\\application\\folder\\UtahLakeDataExplorer")
		Note that R uses \\ or / in path names. \ will cause an error.
