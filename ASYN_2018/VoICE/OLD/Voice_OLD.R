### Packages for Running VoICE 

source("http://www.bioconductor.org/biocLite.R")
biocLite("GO.db")
install.packages("WGCNA")
install.packages("gdata")
biocLite("impute") 
biocLite("preprocessCore")
install.packages("ggmap") 
install.packages("png")

### How to fix error: where Error in file(file, "rt") : cannot open the connection Calls: read.csv -> read.table -> file Execution halted 