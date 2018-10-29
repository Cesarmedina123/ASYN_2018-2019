## Read packages into local library

library(tidyverse)
library(reshape2)
library(plyr)
library(skimr)
library(readr)
library(readxl)
library(doBy)
library(plotrix)
library(dplyr)
library(pastecs)
library(rio)
library(BSDA)
library(broom)

#- Finding and change working directory to "analyze/A_Syn_Project_
#- ASyn_Birds_SJM/Asyn_Birds_UD_SJM

findDir <- "E:/A_Syn_Project"
mainDir <- "C:/Users/cesar/Documents/Julie_Lab/Data/SB"

setwd(findDir)
getwd() # confirm working directory is in proper location
dir() # check contents within working directory


#-- Script for converting VoiCE .xlsx to .csv format

myfiles <- list.files(path = ".", 
                      pattern = "_UD_SB.xlsx",
                      recursive = T)

myworkingfiles <- myfiles[!grepl("OLD", myfiles)]

sinkdirectory <- "C:/Users/cesar/Documents/Julie_Lab/Data/SB"

for(e in 1: length (myworkingfiles)) {
  file.copy(myworkingfiles, sinkdirectory, 
            overwrite = F, recursive = T)
}

setwd(sinkdirectory)

infiles <- list.files(pattern ="_UD_SB.xlsx")
#recursive = TRUE)

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


# For loop to read .csv file of a single subject
# into r with all of the data frames that shares it's ID in
# the file name

#- Specify Bird ID here 

birdID <- "Bk280"

#- Specific string of file names to be read into r

filenames <- list.files(pattern = "UD_SB_Sheet")

# list .csv files with birdID in name
# for loop to read in .csv files with birdID in name
# reads in contents of dataframes also

infiles <- filenames[grep(birdID, filenames)]

outfiles <- list() # contains a list of the dataframes

for(i in 1: length(infiles)) {
   outfiles[[i]] <- read.table(infiles[i], header = TRUE, 
                               sep = ",", nrows = 75)
}

# head(outfiles[[1]]) # check if the data.frame is correct (change i to 1)

names(outfiles) <- infiles # retain name of original data frame for data manipulation

# Create a master dataframe for all acoustic feature (i.e., column)
# by binding columns from above dataframes together with similar name
# birdfile <- do.call(rbind, unname(Map(cbind, Information = names(outfiles), outfiles)))

birdfile <- bind_rows(outfiles, .id = "Information") # plyr package

#- specify extension of file name that corresponds to timepoint and syllable
#- Example: Sheet 1 is pre, sheet 2 is Post 1m...
#- Example: "_A_" is syllable A and "B" is syllable B
#- these character strings were written into name of .csv files

Syllable <- c("_A_", "_B_", "_C_", "_D_", "_E_", "_F_", "_G_")

Timepoints <- c("_Sheet1", "_Sheet2", "_Sheet3", "_Sheet4")

# create new columns, Timepoints and Syllable, and bind them
# to a new dataframe: birdfile

a <- list() # Reads from "Timepoint"

for(e in 1: length(Timepoints)) {
  a[[e]] <- grep(Timepoints[e], birdfile [,1], 
                 fixed = TRUE)
  print(a)
}

b <- c()

for(e in 1: dim(birdfile) [1]) {
  b[e] <- if (e %in% a[[1]]) {
    "0"
  } else {
    if (e %in% a[[2]]) {
      "1"
    } else {
      if (e %in% a[[3]]) {
        "2"
      } else {
        if (e %in% a[[4]]) {
          "3"
        }
      }
    }
  }
  print(b)}

birdfile <- cbind.data.frame(birdfile, Timepoint = b)

d <- list() # Reads from "Syllable"

for (e in 1: length(Syllable)) {
  d[[e]] <- grep(Syllable[e], birdfile[,1],
                 fixed = TRUE)
  print(d)
}

f <- c()

for(e in 1: dim(birdfile) [1]) {
  f[e] <- if (e %in% d[[1]]) {
    "A"
  } else {
    if (e %in% d[[2]]) {
      "B"
    } else {
      if (e %in% d[[3]]) {
        "C"
      } else {
        if (e %in% d[[4]]) {
          "D"
        } else {
          if (e %in% d[[5]]) {
            "E"
          } else {
            if (e %in% d[[6]]) {
              "F"
            } else {
              if (e %in% d[[7]])
                "G"
            }
          }
        }
      }
    }
  }
  print(f)}

birdfile <- cbind.data.frame(birdfile, Syllable = f)

# Change to include only columns: Information and metrics
# Change to delete columns with NA values

# birdfile <- birdfile[,c(1:11,58:59)] # approach knowing what columns to keep
# birdfile <- birdfile[,colSums(is.na(birdfile))<nrow(birdfile)] # universal approach

birdfile <- Filter(function(x)!all(is.na(x)), birdfile) # approach using filter

#birdfile$Timepoint <- factor(birdfile$Timepoint,
#                             levels = c('0', '1',
 #                                       '2', '3'),
  #                           ordered = TRUE)

# Add column with birdID as variable

birdfile[,19] <- birdID

colnames(birdfile) [19] <- "BirdID"

# Make new column a factor variable: used in skim for summary stats

birdfile$BirdID <- as.factor(birdfile$BirdID)

# Remove column "Information" from dataframe

birdfile <- birdfile[,2:19]

# birdfile[,15:16] <- as.character(birdfile[,15:16])

# Create column containing BirdID, Timepoint, and Syllable info

birdfile$Bird.Syll <- with(birdfile, interaction(BirdID,  Syllable))

birdfile$Bird.Syll.Time <- with(birdfile, interaction(Bird.Syll,Timepoint))

# removes all rows that are incomplete 
# or have NAs
# Error will be returned if no NAs found

birdfile <- birdfile[complete.cases(birdfile),]

# Converts birdfile dataframe to datatable format
# data frame now workable as a datatable
# as.data.table(birdfile) also works

setDT(birdfile)

# create new folder to save files to if one doesn't exist

subDir <- birdID

dir.create(file.path(mainDir, subDir))

# Final data frame will contain 29 variables for VoICE
# birdfile <- data.table(birdfile) # converts to data.table
# setkey(birdfile, Timepoint, Syllable) # assigns keys for functionality like as.factor

setwd(mainDir)

# Export final raw data frame in .csv format

#write.csv(birdfile, file = paste(birdID, "UD_VoICE_Mastertable.csv", 
 #                                sep = "_"), 
  #        row.names = FALSE,
   #       file.path(mainDir,subDir))

# Create summary statistics for dataframe 
# Mean, SD, SE, CV, and n
# SummaryStats <- birdfile %>% group_by(Syllable, BirdID, Timepoint) %>% stat.desc()
# SummaryStats <- summaryBy(as.formula(paste0("syllable.duration+" ,"mean.amplitude","~Bird.Syll.Time")), 
# data=birdfile, FUN=c(mean,var,length))

# vector of variables to graph
AFname <- colnames(birdfile[,3:10])

# list of EXTRA summary stats to run

CV <- function(x){
  Coeff.Variation=sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE)
}

# Extra functions that can be created

# summary.list <- function(x)list(
#  N.with.NA.removed = length(x[!is.na(x)]),
#  Count.of.NA = length(x[is.na(x)]),
#  Mean = mean(x, na.rm = TRUE),
#  Median = median(x, na.rm = TRUE),
#  Max.Min = range(x, na.rm = TRUE),
#  Range = max(Data$ Fish, na.rm = TRUE) - min(Data$ Fish, na.rm = TRUE),
#  Variance = var(x, na.rm = TRUE),
#  Std.Dev = sd(x, na.rm = TRUE),
#  CV = sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE),
#  Std.Error=sd(x, na.rm = TRUE)/sqrt(length(x[!is.na(x)])),
#  Quantile=quantile(x, na.rm = TRUE)
# )

# Create stat summary for all acoustic features 
# by looping through all columns with summaryBy function
# names(SummaryStats) <- paste0(birdID, paste0(".", AFname))

SummaryStats <- list()

for(i in 1: length(AFname)) {
  
SummaryStat <- as.data.frame(summaryBy(as.formula(paste0(AFname[i],"~Bird.Syll.Time")), data=birdfile,
          FUN=c(mean,var,length, sd, std.error, CV)))

names(SummaryStat)<-c('Information',"mean","var","length", "sd", "std.error", "CV")

SummaryStat <- cbind.data.frame(Variable=AFname[i],SummaryStat)

SummaryStats[[i]] <-SummaryStat
}


#AFname <- AFname[1:14]

#for(i in 1: length(AFname)) {
  
#  SummaryStats[[i]] <- summaryBy(as.formula(paste0(AFname[i],"~Bird.Syll.Time")), data=birdfile,
 #                                        FUN=c(mean, sd, std.error, CV))
#}

# Create a master summary dataframe for all acoustic features
# by binding columns from above dataframes together with similar name
# change to cbind.data.frame for second option above commented out

birdstats <- do.call("rbind.data.frame", SummaryStats)

# Export final data frame in .csv format

write.csv(birdstats, file = paste0(mainDir,"/",subDir,"/",
                                  paste0(birdID,
                                        "_", "UD_SB_Summarytable.csv")), 
          row.names = FALSE)

################## Create MasterSummaryTable #####################

# Pull all SummaryTables into R

masterfilenames <- list.files(pattern = "UD_SB_Summarytable",
                              recursive = TRUE)

# Create list of bird summarytable dataframes

masterinfiles <- list() # contains a list of the dataframes

for(i in 1: length(masterfilenames)) {
  masterinfiles[[i]] <- read.table(masterfilenames[i], header = TRUE, 
                              sep = ",")
}
  
names(masterinfiles) <- masterinfiles

# Concatanate into one dataframe

masterbirdfile <- bind_rows(masterinfiles) # plyr package

## Add additional columns ##

# Add column for BirdID.Syll which will be used as data filter

BirdID.Syll <- masterbirdfile[,2]

BirdID.Syll <- str_sub(masterbirdfile[,2], start = 1, end = 7)

masterbirdfile [,9] <- BirdID.Syll

colnames(masterbirdfile) [9] <- "BirdID.Syll"

# Add column for Timepoint which will be used on x axis

Timepoint <- masterbirdfile[,2]

Timepoint <- str_sub(masterbirdfile[,2],start = 9, end = 9)

masterbirdfile [,10] <- Timepoint

colnames(masterbirdfile) [10] <- "Timepoint"

# Convert Timepoint and BirdID.Syll columns to factor

masterbirdfile[,9] <- as.factor(masterbirdfile[,9])

masterbirdfile[,10] <- as.factor(masterbirdfile[,10])

str(masterbirdfile)

# Add column for experimental condition which will be used to group
# masterbirdfile[grep(paste(asyn, collapse = "|"), masterbirdfile[,9])
# ,]

asyn <- c("Bk174","Rd321","Bk262","Bk278","Bk293","Bk295","Wh203","Bk280","Wh206","Wh214")
con <- c("Wh145","Bk268","Rd346","Bk277","Wh211","Bk286")

masterbirdfile$Condition <- masterbirdfile[,9]

birdrowasyn <- grep(paste(asyn, collapse = "|"), masterbirdfile[,9])

masterbirdfile$Condition <- ifelse(rownames(masterbirdfile) %in% 
                                     as.character(birdrowasyn), 
                                   "asyn", "con")

# Add column for if harmonic syllable
# change normbirdfile to masterbirdfile if 
# done before adding normalized values

# modulated and unmodulated harmonic syllables included
#harmonic <- c("Bk280.D", "Wh214.G","Bk278.C", "Bk278.F", "Bk293.E",
 #             "Bk293.B", "Rd346.B", "Rd346.E", "Bk174.D", "Rd321.D",
  #            "Rd321.B", "Wh145.B", "Bk262.C", "Bk268.E", "Bk268.F",
   #           "Bk277.A", "Bk277.B", "Bk277.F", "Bk295.C", "Bk295.F",
    #          "Wh203.B", "Wh203.F", "Wh206.B", "Wh206.D", "Wh206.F",
     #         "Wh211.C", "Wh211.E", "Wh211.F") 


harmonic <- c("Wh214.G","Bk278.C","Bk293.E","Red346.B","Bk174.D",
              "Bk174.E","Red321.D","Wh145.B","Bk262.C","Bk268.E",
              "Bk277.B","Bk277.D","Bk295.F","Wh203.F","Wh211.E")
  
normbirdfile$Harmonic <- normbirdfile[,13]

harmonicrows <- grep(paste(harmonic, collapse = "|"), normbirdfile[,"BirdID.Syll"])

normbirdfile$Harmonic <- ifelse((rownames(normbirdfile)) %in%
                                  harmonicrows,"yes", "no")

# Add normalized Mean and CV columns to masterbirdfile

normalize <- function(level){

newmasterbirdfile <- list()

  for(i in 1: length(levels(masterbirdfile$BirdID.Syll))){ 
    
a <- masterbirdfile[masterbirdfile$Variable == level,]

b <- a[a$BirdID.Syll == levels(a$BirdID.Syll)[i],]

valuesMean <- b$mean/b$mean[1]

valuesCV <- b$CV/b$CV[1]

newmatrix <- cbind.data.frame(b,valuesMean,valuesCV)
 
newmasterbirdfile [[i]] <- newmatrix

  }

return(do.call("rbind.data.frame", newmasterbirdfile))

}

normbirdfile <- do.call("rbind.data.frame", lapply(levels(masterbirdfile$Variable),
                                                   normalize))

normbirdfile <- normbirdfile[complete.cases(normbirdfile),]

#- Export file to conditions subfolder

subDir <- "Conditions"

dir.create(file.path(mainDir, subDir))

write.csv(normbirdfile, file = paste0(mainDir,"/",subDir,"/",
                                      paste0("UD_SB_MasterSummarytable.csv")), 
          row.names = FALSE)
