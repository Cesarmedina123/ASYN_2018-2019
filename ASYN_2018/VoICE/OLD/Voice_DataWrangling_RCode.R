#-- Script for converting VoiCE .xlsx to .csv format

#- Packages required readr, dplyr, ggplot2, openxslxrio
#- if not install using install.packages("")
## Install packages needed for code below

install.packages("tidyverse")
install.packages("reshape2")
install.packages("plyr")
install.packages("skimr")
install.packages("ggpubr")
inatall.packages("reshape")
install.packages("plotrix")
install.packages("pastecs")
install.packages("BSDA")
install.packages("broom")
install.packages("doBy")


## Read packages into local library

library(plyr)
library(tidyverse)
library(reshape2)
library(skimr)
library(readr)
library(data.table)
library(doBy)
library(plotrix)
library(dplyr)
library(pastecs)
library(rio)
library(BSDA)
library(broom)
library(doBy)

#- Finding and change working directory to "analyze/A_Syn_Project_
#- ASyn_Birds_SJM/Asyn_Birds_UD_SJM

getwd()
setwd("C:/Users/cesar/Documents/Julie_Lab/Data/ASYN_2018/VoICE")
getwd() # confirm working directory is in proper location
dir() # check contents within working directory


# save working directory as a vector for future 

mainDir <- "C:/Users/cesar/Documents/Julie_Lab/Data/ASYN_2018/VoICE"

# For loop to read .csv file of a single subject
# into r with all of the data frames that shares it's ID in
# the file name

#- Specify Bird ID here 

birdID <- "Rd346"

#- Specific string of file names to be read into r

filenames <- list.files(pattern = "UD_VoICE_Sheet")

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

birdfile <- birdfile [,c(1, 4,7:19, 100,101)]

#birdfile$Timepoint <- factor(birdfile$Timepoint,
#                             levels = c('0', '1',
 #                                       '2', '3'),
  #                           ordered = TRUE)

# Add column with birdID as variable

birdfile[,18] <- birdID

colnames(birdfile) [18] <- "BirdID"

# Make new column a factor variable: used in skim for summary stats

birdfile$BirdID <- as.factor(birdfile$BirdID)

# Remove column "Information" from dataframe

birdfile <- birdfile[,2:18]

# birdfile[,15:16] <- as.character(birdfile[,15:16])

# Create column containing BirdID, Timepoint, and Syllable info

birdfile$Bird.Syll <- with(birdfile, interaction(BirdID,  Syllable))

birdfile$Bird.Syll.Time <- with(birdfile, interaction(Bird.Syll,Timepoint))

# removes all rows that are incomplete 
# or have NAs
# Error will be returned if no NAs found

birdfile <- birdfile[complete.cases(birdfile), ]

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
AFname <- colnames(birdfile[,1:18])

# list of EXTRA summary stats to run

CV <- function(x){
  Coeff.Variation=sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE)
}

std.error <- function(x) {
  std.error = sd(x, na.rm = TRUE)/sqrt(length(x[!is.na(x)]))
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
                                        "_", "UD_VoICE_Summarytable.csv")), 
          row.names = FALSE)

################## Create MasterSummaryTable #####################

# Pull all voice tables into R

masterfilenames <- list.files(pattern = "UD_VoICE_Summarytable",
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

write.csv(normbirdfile, file = paste0(mainDir,"/",subDir,"/",
                                      paste0("UD_VoICE_MasterSummarytable.csv")), 
          row.names = FALSE)

######### Wrangle All Data into a single dataframe one by one ##########

setwd("C:/Users/cesar/Documents/Julie_Lab/Data/ASYN_2018/VoICE")

mainDir <- "C:/Users/cesar/Documents/Julie_Lab/Data/ASYN_2018/VoICE"

birdIDs <- c("Bk174","Rd321","Bk262","Bk278","Bk293","Bk295","Wh203","Bk280",
             "Wh206","Wh214","Wh145","Bk268","Rd346","Bk277","Wh211","Bk286")


birdIDs <- str_c(birdIDs, collapse = "|")

filenames <- list.files(pattern = "UD_VoICE_Sheet")

outfiles <- list()

for(i in 1: length(filenames)) {
  
  outfiles[[i]] <- read.table(filenames[i], header = TRUE, 
                              sep = ",", nrows = 75)
  
}

names(outfiles) <- filenames

##- Merge dataframes with same BirdID from list of dataframes

list2env(outfiles, .GlobalEnv) #unlist a list of dataframes

files.combined <- list()

for(i in 1: length(birdIDs)) {

files.combined[[i]] <- mget(ls(pattern = birdIDs [i])) %>% bind_rows(.id = "Information")

}

files.combined <- files.combined[[1]]

files.combined <- files.combined[,c(1,4,7:19)]

files.combined <- files.combined[complete.cases(files.combined),]

file.gathered <- gather(files.combined, key = "Variable", value = "Score", 2:15)

file.gathered$BirdID <- str_sub(file.gathered$Information, start = 1L, end = 5L)

file.gathered$Syll <- str_sub(file.gathered$Information, start = 7L, end = 7L)

Timepoints <- c("_Sheet1", "_Sheet2", "_Sheet3", "_Sheet4")

a <- list() # Reads from "Timepoint"

for(e in 1: length(Timepoints)) {
  a[[e]] <- grep(Timepoints[e], file.gathered [,1], 
                 fixed = TRUE)
  #print(a)
}

b <- c()

for(e in 1: dim(file.gathered) [1]) {
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
  #print(b)
  }

file.gathered <- cbind.data.frame(file.gathered, Timepoint = b)

file.gathered <- within(file.gathered, BirdID.Timepoint <- 
         str_c(file.gathered$BirdID,file.gathered$Timepoint,sep = "."))

file.gathered <- within(file.gathered, BirdID.Syll <- 
                          str_c(file.gathered$BirdID,file.gathered$Syll,sep = "."))

file.gathered <- within(file.gathered, BirdID.Syll.Time <- 
                          str_c(file.gathered$BirdID.Syll,file.gathered$Timepoint,sep = "."))

harmonic <- c("Wh214.G","Bk278.C","Bk293.E","Red346.B","Bk174.D",
              "Bk174.E","Red321.D","Wh145.B","Bk262.C","Bk268.E",
              "Bk277.B","Bk277.D","Bk295.F","Wh203.F","Wh211.E")

harmonicrows <- grep(paste(harmonic, collapse = "|"), file.gathered[,"BirdID.Syll"])

file.gathered$Harmonic <- ifelse((rownames(file.gathered)) %in%
                                  harmonicrows,"yes", "no")

asyn <- c("Bk174","Rd321","Bk262","Bk278","Bk293","Bk295","Wh203","Bk280","Wh206","Wh214")
con <- c("Wh145","Bk268","Rd346","Bk277","Wh211","Bk286")

birdrowasyn <- grep(paste(asyn, collapse = "|"), file.gathered[,9])

file.gathered$Condition <- ifelse(rownames(file.gathered) %in% 
                                     as.character(birdrowasyn), 
                                   "asyn", "con")

subDir <- "Conditions"

write.csv(file.gathered, file = paste0(mainDir,"/",subDir,
                                  "/","VoICE_UD_Table.csv"), 
          row.names = FALSE)

####- Create Summary Table containing mean, CV, std error of each score
####- By BirdID

mainDir <- "C:/Users/cesar/Documents/Julie_Lab/Data/ASYN_2018/VoICE/Conditions"

df <- read.table(file = paste0(mainDir,"/VoICE_UD_Table.csv"), 
                 header = TRUE, sep = ",")

CV <- function(x){
  Coeff.Variation=sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE)
}

std.error <- function(x) {
  std.error = sd(x, na.rm = TRUE)/sqrt(length(x[!is.na(x)]))
}

###- Create a master summary dataframe for all acoustic features
###- by BirdID.Syll.Time
  
BirdSummary <- as.data.frame(summaryBy(Score ~ BirdID.Syll.Time + Variable, 
                                         data = df, id = c("BirdID.Syll","Condition","Harmonic"),
                                         FUN=c(mean, CV, var,length, sd, std.error, median,IQR)))
  
names(BirdSummary)<-c("BirdID.Syll.Time","Variable","mean", "CV","var","length", 
                       "sd", "std.error","median","IQR","BirdID.Syll","Condition","Harmonic")

BirdSummary <- BirdSummary[complete.cases(BirdSummary),]

write.csv(BirdSummary, file = paste0(mainDir,"/VoICE_UD_Table_Birds.csv"), 
          row.names = FALSE)

###- Create summary dataframe with scores in a single column
###- done on above dataframe

df <- read.table(file = paste0(mainDir,"/VoICE_UD_Table_Birds.csv"), 
                 header = TRUE, sep = ",")

df <- gather(data = df, key = Score, value = Values, c(3:4,9, 14:17))

df <- df[,-c(3:7)]

write.csv(df, file = paste0(mainDir,"/VoICE_UD_Table_Birds_2.csv"), 
          row.names = FALSE)

###- Add normalized Mean and CV columns to masterbirdfile

df <- read.table(file = paste0(mainDir,"/VoICE_UD_Table_Birds.csv"), 
                 header = TRUE, sep = ",")

##- Create normalize function to normalize values 
##- by baseline timepoint for each Bird.Syll

normalize <-function(data, factor_name, variable_name){
  
  data<-as.data.frame(data)
  lev<-unique(data[,factor_name])
  lev_vars<-unique(data[,variable_name])
  
  newNormFile <- list()
  newNormFile_K <- list()
  
  for(k in seq_along(lev_vars)){
    
    for(i in 1: length(lev) ){ 
      
      a <- as.data.frame(data[data[,variable_name] == lev_vars[k], ])
      
      b <- a[a[,factor_name] == lev[i],]
      
      NormMean <- b$mean / b$mean[1]
      
      NormCV <- b$CV / b$CV[1]
      
      #NormSD <- b$sd / b$sd[1]
      
      #NormStdErr <- b$std.error / b$std.error[1]
      
      newmatrix <- cbind.data.frame(b,NormMean,NormCV) #NormSD,NormStdErr
      
      newNormFile [[i]] <- newmatrix
      
    }
    
    newNormFile_K[[k]] <- newNormFile
    
    
  }
  
  
  return(newNormFile_K)
  
}

df <- normalize(data = df, factor_name = "BirdID.Syll", variable_name = "Variable")

df <- do.call(c, df) # concatonates a list of lists into one list

df <- rbind.fill(df) # concatonates a list of DFs into one DF

##- Modulation Index for Individual Acoustic Features
##- Modulation Index: (Post-Pre)/(Post+Pre)

#df <- df[df$BirdID.Syll != "Wh203.B" | "Why206.A"] # Only has 0,1, and 2 month time

modulationindex <-function(data, factor_name, variable_name) {
  
  data<-as.data.frame(data)
  lev<-unique(data[,factor_name])
  lev_vars<-unique(data[,variable_name])
  
  newNormFile <- list()
  newNormFile_K <- list()
  
  for(k in seq_along(lev_vars)) {
    
    for(i in 1: length(lev) ) { 
      
      a <- as.data.frame(data[data[,variable_name] == lev_vars[k], ])
      
      b <- a[a[,factor_name] == lev[i],]
      
      modulationindex.mean <- (b$mean - b$mean[1]) / (b$mean + b$mean[1])
      
      modulationindex.CV <- (b$CV - b$CV[1]) / (b$CV + b$CV[1])
      
      #modulationindex.sd <- (b$sd - b$sd[1]) / (b$sd + b$sd[1])
      
      #modulationindex.std.err <- (b$std.error - b$std.error[1]) / (b$std.error + b$std.error[1])
      
      newmatrix <- cbind.data.frame(b,modulationindex.mean, modulationindex.CV) # modulationindex.sd, modulationindex.std.err
      
      newNormFile [[i]] <- newmatrix
      
    }
    
    newNormFile_K[[k]] <- newNormFile
    
  }
  
  return(newNormFile_K)
  
}

df <- modulationindex(data = df, factor_name = "BirdID.Syll", variable_name = "Variable")

df <- do.call(c, df) # concatonates a list of lists into one list

df <- rbind.fill(df) # concatonates a list of DFs into one DF

df$Timepoint <- str_sub(df$BirdID.Syll.Time, 9)

write.csv(df, file = paste0(mainDir,"/VoICE_UD_Table_Birds.csv"), row.names = FALSE)

####- Create Summary Table containing mean, CV, std error of each score
####- By Condition and Timepoint for All Syllables

mainDir <- "C:/Users/cesar/Documents/Julie_Lab/Data/ASYN_2018/VoICE/Conditions"

df <- read.table(file = paste0(mainDir,"/VoICE_UD_Table_Birds.csv"), 
                 header = TRUE, sep = ",")

ConditionSummary <- as.data.frame(summaryBy(mean + CV + NormMean + NormCV + 
                                              modulationindex.mean + modulationindex.CV  
                                         ~ Condition + Variable + Timepoint, 
                                       data = df,
                                       FUN = c(mean, CV, length, sd, std.error)))

# ConditionSummary <- ConditionSummary[complete.cases(ConditionSummary),]

ConditionSummary <- ConditionSummary[ ,c(1:10,12,14,22:33)]

###- Export Condition Summary Table for All Syllables

write.csv(ConditionSummary, file = paste0(mainDir,"/VoICE_UD_Table_Condition_AllSylls.csv"), 
          row.names = FALSE)

####- Create Summary Table containing mean, CV, std error of each score
####- By Condition and Timepoint for Harmonic Syllables

mainDir <- "C:/Users/cesar/Documents/Julie_Lab/Data/ASYN_2018/VoICE/Conditions"

df <- read.table(file = paste0(mainDir,"/VoICE_UD_Table_Birds.csv"), 
                 header = TRUE, sep = ",")

df <- df[df$Harmonic == "yes", ]

ConditionSummary <- as.data.frame(summaryBy(mean + CV + NormMean + NormCV + 
                                              modulationindex.mean + modulationindex.CV  
                                            ~ Condition + Variable + Timepoint, 
                                            data = df,
                                            FUN = c(mean, CV, length, sd, std.error)))

# ConditionSummary <- ConditionSummary[complete.cases(ConditionSummary),]

ConditionSummary <- ConditionSummary[ ,c(1:10,12,14,22:33)]

###- Export Condition Summary Table for Harmonic Syllables

write.csv(ConditionSummary, file = paste0(mainDir,"/VoICE_UD_Table_Condition_Harmonic.csv"), 
          row.names = FALSE)

####- Create Summary Table containing mean, CV, std error of each score
####- By Condition and Timepoint for Noisy Syllables

mainDir <- "C:/Users/cesar/Documents/Julie_Lab/Data/ASYN_2018/VoICE/Conditions"

df <- read.table(file = paste0(mainDir,"/VoICE_UD_Table_Birds.csv"), 
                 header = TRUE, sep = ",")

df <- df[df$Harmonic == "no", ]

ConditionSummary <- as.data.frame(summaryBy(mean + CV + NormMean + NormCV + 
                                              modulationindex.mean + modulationindex.CV  
                                            ~ Condition + Variable + Timepoint, 
                                            data = df,
                                            FUN = c(mean, CV, length, sd, std.error)))

# ConditionSummary <- ConditionSummary[complete.cases(ConditionSummary), ]

ConditionSummary <- ConditionSummary[ ,c(1:10,12,14,22:33)]

###- Export Condition Summary Table for Noisy Syllables

write.csv(ConditionSummary, file = paste0(mainDir,"/VoICE_UD_Table_Condition_Noisy.csv"), 
          row.names = FALSE)

