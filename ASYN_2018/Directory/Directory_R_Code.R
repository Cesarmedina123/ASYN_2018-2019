# Use readxl package

library(readxl)


# Script for moving files between directories

#- Print current working directory
getwd()
  #Change working directory
setwd("E:/A_Syn_Project")
  # Checking working dictory
getwd()

#- Following code performs a recursive search
#- using list.files while excluding specific folders
#- excluding folders named "OLD"

myfiles <- list.files(path = ".", 
                      pattern = "_meanMedianPitch_N.csv",
                      recursive = T)

myworkingfiles <- myfiles[!grepl("OLD", myfiles)]

sinkdirectory <- "C:/Users/cesar/Documents/Julie_Lab/Data/FF"

#- Move files from one directory to another

for(e in 1: length (myworkingfiles)) {
  file.copy(myworkingfiles, sinkdirectory, 
            overwrite = F, recursive = T)
}

#copiedfiles <- list.files(path = ".", pattern = ".xlsx")

#for(a in 1: length)

#-- convert xlsx files to .csv

#--- Creates matrices of excel files to read (if needed change
# <- operator to =)

setwd(sinkdirectory)

infiles <- list.files(pattern ="_UD_VoICE.xlsx")
                     #recursive = TRUE)

#--- Read each file and write it to csv 
#--- REMEMBER TO SPECIFY SHEET NUMBER


lapply(infiles, function(f) {
  df = read_excel(f, sheet = 1)
  write.csv(df, gsub(".xlsx", "_Sheet1.csv", f), 
            row.names = FALSE, col.names = TRUE)
}
)

lapply(infiles, function(f) {
  df = read_excel(f, sheet = 2)
  write.csv(df, gsub(".xlsx", "_Sheet2.csv", f), 
            row.names = FALSE, col.names = TRUE)
}
)

lapply(infiles, function(f) {
  df = read_excel(f, sheet = 3)
  write.csv(df, gsub(".xlsx", "_Sheet3.csv", f), 
            row.names = FALSE, col.names = TRUE)
}
)

lapply(infiles, function(f) {
  df = read_excel(f, sheet = 4)
  write.csv(df, gsub(".xlsx", "_Sheet4.csv", f), 
            row.names = FALSE, col.names = TRUE)
}
)
