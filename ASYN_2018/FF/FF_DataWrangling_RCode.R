## Read packages into local library

install.packages("tidyverse")
install.packages("data.table")
install.packages("ggpubr")
install.packages("ggsignif")
install.packages("svglite")

library(tidyverse)
library(ggpubr)
library(reshape2)
library(data.table)
library(ggsignif)
library(broom)
library(plyr)
library(skimr)
library(readxl)
library(doBy)
library(plotrix)
library(pastecs)
library(rio)
library(BSDA)


#- Finding and change working directory to "analyze/A_Syn_Project_
#- ASyn_Birds_SJM/Asyn_Birds_UD_SJM

findDir <- "E:/A_Syn_Project"
mainDir <- "C:/Users/cesar/Documents/Julie_Lab/Data/FF"

setwd(findDir)
getwd() # confirm working directory is in proper location
dir() # check contents within working directory


#-- Script for converting VoiCE .xlsx to .csv format
#-- and copying them to new directory

myfiles <- list.files(path = ".", 
                      pattern = "_meanMedianPitch_N.csv",
                      recursive = T)

myworkingfiles <- myfiles[!grepl("OLD", myfiles)]

sinkdirectory <- "C:/Users/cesar/Documents/Julie_Lab/Data/FF"

for(e in 1: length (myworkingfiles)) {
  file.copy(myworkingfiles, sinkdirectory, 
            overwrite = F, recursive = T)
}

setwd(sinkdirectory)

#- Specific string of file names to be read into r

myinfiles <- list.files(path = ".",
                        pattern = "_meanMedianPitch_N")

# For loop to read .csv file of a single subject
# into r with all of the data frames that shares it's ID in
# the file name

# list .csv files with birdID in name
# for loop to read in .csv files with birdID in name
# reads in contents of dataframes also

myreadfiles <- list() # contains a list of the dataframes

for(i in 1: length(myinfiles)) {
   myreadfiles[[i]] <- read.table(myinfiles[i], header = TRUE, 
                               sep = ",", nrows = 75)
}

# head(outfiles[[1]]) # check if the data.frame is correct (change i to 1)

names(myreadfiles) <- myinfiles # retain name of original data frame for data manipulation

# Create a master dataframe 
# by binding columns from above dataframes together with similar name
# birdfile <- do.call(rbind, unname(Map(cbind, Information = names(myreadfiles), myreadfiles)))

masterfile <- bind_rows(myreadfiles, .id = "Information") # plyr package

masterfile <- read.table(file = paste0(sinkdirectory,"/UD_FF_UnfinishedTable1.csv"), 
            header = TRUE, sep = ",")


#- specify extension of file name that corresponds to timepoint and syllable
#- Example: Sheet 1 is pre, sheet 2 is Post 1m...
#- Example: "_A_" is syllable A and "B" is syllable B
#- these character strings were written into name of .csv files
#- Timepoints <- c("_0_", "_1_", "_2_", "_3_")

Syllable <- c("WAV_A_", "WAV_B_", "WAV_C_", "WAV_D_", "WAV_E_", "WAV_F_", "WAV_G_")

BirdID <- c("Bk174","Bk262","Bk268","Bk277","Bk278","Bk280","Bk286","Bk293","Bk295",
            "Rd321","Rd346","Wh145","Wh203","Wh206","Wh211","Wh214")

# create new columns, Timepoints and Syllable, and bind them
# to a new dataframe: birdfile

# a <- list() # Reads from "Timepoint"

# for(e in 1: length(Timepoints)) {
#  a[[e]] <- grep(Timepoints[e], masterfile [,1], 
#                 fixed = TRUE)
#  print(a)
#}

# b <- c()

#for(e in 1: dim(masterfile) [1]) {
#  b[e] <- if (e %in% a[[1]]) {
#    "0"
#  } else {
#    if (e %in% a[[2]]) {
#      "1"
#    } else {
#      if (e %in% a[[3]]) {
#        "2"
#      } else {
#        if (e %in% a[[4]]) {
#          "3"
#        }
#      }
#    }
#  }
#  print(b)}

# masterfile <- cbind.data.frame(masterfile, Timepoint = b)

d <- list() # Reads from "Syllable"

for (e in 1: length(Syllable)) {
  d[[e]] <- grep(Syllable[e], masterfile[,1],
                 fixed = TRUE)
  print(d)
}

f <- c()

for(e in 1: dim(masterfile) [1]) {
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

masterfile <- cbind.data.frame(masterfile, Syllable = f)

write.csv(masterfile, file = paste0(sinkdirectory,"/","UD_FF_UnfinishedTable2.csv"), 
          row.names = FALSE)

g <- list()

BirdID <- strtrim(masterfile$Information, 5)

masterfile <- cbind.data.frame(masterfile, BirdID = BirdID)

# create new folder to save files to if one doesn't exist

subDir <- "Conditions"

dir.create(file.path(sinkdirectory, subDir))

# Change to include only columns: Information and metrics
# Change to delete columns with NA values
# birdfile <- birdfile[,colSums(is.na(birdfile))<nrow(birdfile)] # universal approach
# birdfile <- Filter(function(x)!all(is.na(x)), birdfile) # approach using filter

masterfile <- masterfile[,c(2:10)] # approach knowing what columns to keep

# Make column a factor variable: used in skim for summary stats
# masterfile$birdid <- as.factor(masterfile$BirdID)

# Create column containing BirdID, Timepoint, and Syllable info

masterfile$Bird.Syll <- with(masterfile, interaction(BirdID,  Syllable))

masterfile$Bird.Syll.Time <- with(masterfile, interaction(Bird.Syll, Timepoint))

# Convert Timepoint and BirdID.Syll columns to factor

masterfile[,"Timepoint"] <- as.factor(masterfile$Timepoint)

masterfile[,"Bird.Syll"] <- as.factor(masterfile$Bird.Syll)

masterfile[, "Bird.Syll.Time"] <- as.factor(masterfile$Bird.Syll.Time)

# Add column for experimental condition which will be used to group
# masterbirdfile[grep(paste(asyn, collapse = "|"), masterbirdfile[,9])
# ,]

asyn <- c("Bk174","Rd321","Bk262","Bk278","Bk293","Bk295","Wh203","Bk280","Wh206","Wh214")
con <- c("Wh145","Bk268","Rd346","Bk277","Wh211","Bk286")

as.data.frame(masterfile)

masterfile$Condition <- NA

birdasyn <- grep(paste(asyn, collapse = "|"), masterfile[,"BirdID"])

masterfile$Condition <- ifelse(rownames(masterfile) %in% 
                                     as.character(birdasyn), 
                                   "asyn", "con")

# Get mean, median, SD, and CV for meanPitchAll and medianPitchAll
# to represent intrasyllable-interrendition

setDT(masterfile)

# Sort text data in R by multiple columns using the order() function

masterfile <- masterfile[with(masterfile, order(masterfile$Bird.Syll, 
                                                masterfile$Timepoint,
                                                decreasing = FALSE)), ]

write.csv(masterfile, file = paste0(sinkdirectory,"/","UD_FF_UnfinishedTable3.csv"), 
          row.names = FALSE)

masterfile <- read.table(file = paste0(sinkdirectory,"/UD_FF_UnfinishedTable3.csv"), 
                         header = TRUE, sep = ",")

#- Experimental Condition of birds

asyn <- c("Bk174","Rd321","Bk262","Bk278","Bk293","Bk295","Wh203","Bk280","Wh206","Wh214")
con <- c("Wh145","Bk268","Rd346","Bk277","Wh211","Bk286")

#- Create normalize function to normalize values 
#- by baseline timepoint for each Bird.Syll

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
      
      normMean <- b$mean / b$mean[1]
      
      normMedian <- b$median / b$median[1]
      
      normSD <- b$sd / b$sd[1]
      
      normCV <- b$cv / b$cv [1]
      
      newmatrix <- cbind.data.frame(b,normMean,normMedian,normSD,normCV)
      
      newNormFile [[i]] <- newmatrix
      
    }
    
    newNormFile_K[[k]] <- newNormFile
    
    
  }
  
  
  return(newNormFile_K)
  
}

##- MeanPitchAll

meanPitchAll <- masterfile[,list(mean=mean(meanPitchAll), median = median(meanPitchAll),
                                     sd=sd(meanPitchAll), 
                                     cv = sd(meanPitchAll)/mean(meanPitchAll)*100),
                               by=Bird.Syll.Time]

BirdID <- strtrim(meanPitchAll$Bird.Syll.Time, 5)

Bird.Syll <- strtrim(meanPitchAll$Bird.Syll.Time, 7)

Timepoint <- str_sub(meanPitchAll$Bird.Syll.Time, start = 9)

meanPitchAll <- cbind.data.frame(meanPitchAll, BirdID = BirdID, 
                                     Bird.Syll = Bird.Syll, Timepoint = Timepoint)

meanPitchAll[,"Variable"] <- "Pitch"

NormMeanPitchAll <- normalize(data= meanPitchAll, factor_name = "Bird.Syll", variable_name = "Variable")

NormMeanPitchAll <- rbind.fill(rbind.fill(NormMeanPitchAll))

NormMeanPitchAll <- as.data.frame(NormMeanPitchAll)

NormMeanPitchAll$Condition <- NA

birdasyn <- grep(paste(asyn, collapse = "|"), NormMeanPitchAll[,"BirdID"])

NormMeanPitchAll$Condition <- ifelse(rownames(NormMeanPitchAll) %in% 
                                   as.character(birdasyn), 
                                 "asyn", "con")

write.csv(NormMeanPitchAll, file = paste0(sinkdirectory,"/",subDir,
                                     "/FF_UD_NormMeanPitchAllTable.csv"), row.names = FALSE)

##- MedianPitchAll

medianPitchAll <- masterfile[,list(mean=mean(medianPitchAll), median = median(medianPitchAll),
                                       sd=sd(medianPitchAll), 
                                       cv = sd(medianPitchAll)/mean(medianPitchAll)*100),
                                 by=Bird.Syll.Time]

BirdID <- strtrim(medianPitchAll$Bird.Syll.Time, 5)

Bird.Syll <- strtrim(medianPitchAll$Bird.Syll.Time, 7)

Timepoint <- str_sub(medianPitchAll$Bird.Syll.Time, start = 9)

medianPitchAll <- cbind.data.frame(medianPitchAll, BirdID = BirdID, 
                                     Bird.Syll = Bird.Syll, Timepoint = Timepoint)

medianPitchAll[,"Variable"] <- "Pitch"


NormMedianPitchAll <- normalize(data= medianPitchAll, factor_name = 
            "Bird.Syll", variable_name='Variable')

NormMedianPitchAll <- rbind.fill(rbind.fill(NormMedianPitchAll))

as.data.frame(NormMedianPitchAll)

NormMedianPitchAll$Condition <- NA

birdasyn <- grep(paste(asyn, collapse = "|"), NormMedianPitchAll[,"BirdID"])

NormMedianPitchAll$Condition <- ifelse(rownames(NormMedianPitchAll) %in% 
                                           as.character(birdasyn), 
                                         "asyn", "con")

write.csv(NormMedianPitchAll, file = paste0(sinkdirectory,"/",subDir,
                                    "/FF_UD_NormMedianPitchAllTable.csv"), row.names = FALSE)

##- intrasyllable-intrarendition scores 
##- Intrarendition CV and MAD

normalizeIntraRendition <-function(data, factor_name, variable_name) {
  
  data<-as.data.frame(data)
  lev<-unique(data[,factor_name])
  lev_vars<-unique(data[,variable_name])
  
  newNormFile <- list()
  newNormFile_K <- list()
  
  for(k in seq_along(lev_vars)) {
    
    for(i in 1: length(lev) ) { 
      
      a <- as.data.frame(data[data[,variable_name] == lev_vars[k], ])
      
      b <- a[a[,factor_name] == lev[i],]
      
      normmeanCV <- b$meanCV / b$meanCV[1]
      
      normmedianCV <- b$medianCV / b$medianCV[1]
      
      normsdCV <- b$sdCV / b$sdCV[1]
      
      normcvCV <- b$cv.CV / b$cv.CV [1]
      
      normmeanMAD <- b$meanMAD / b$meanMAD[1]
      
      normmedianMAD <- b$medianMAD / b$medianMAD[1]
      
      normsdMAD <- b$sdMAD / b$sdMAD[1]
      
      normcvMAD <- b$cv.MAD / b$cv.MAD [1]
      
      newmatrix <- cbind.data.frame(b,normmeanCV,normmedianCV,normsdCV,normcvCV,
                                    normmeanMAD,normmedianMAD,normsdMAD,normcvMAD)
      
      newNormFile [[i]] <- newmatrix
      
    }
    
    newNormFile_K[[k]] <- newNormFile
    
  }
  
  return(newNormFile_K)
  
}

IntraRenditionScores <- masterfile[,list(meanCV = mean(cv), 
                                         medianCV = median(cv),
                                         sdCV = sd(cv), 
                                         cv.CV = sd(cv)/mean(cv)*100,
                                         meanMAD = mean(mad), 
                                         medianMAD = median(mad),
                                         sdMAD = sd(mad), 
                                         cv.MAD = sd(mad)/mean(mad)*100), by = Bird.Syll.Time]


BirdID <- strtrim(IntraRenditionScores$Bird.Syll.Time, 5)

Bird.Syll <- strtrim(IntraRenditionScores$Bird.Syll.Time, 7)

Timepoint <- str_sub(IntraRenditionScores$Bird.Syll.Time, start = 9)

IntraRenditionScores <- cbind.data.frame(IntraRenditionScores, BirdID = BirdID, 
                                     Bird.Syll = Bird.Syll, Timepoint = Timepoint)

IntraRenditionScores$Variable <- "Pitch"

NormIntraRenditionScores <- normalizeIntraRendition(data= IntraRenditionScores, factor_name = 
                           "Bird.Syll", variable_name='Variable')

NormIntraRenditionScores <- rbind.fill(rbind.fill(NormIntraRenditionScores))

as.data.frame(NormIntraRenditionScores)

NormIntraRenditionScores$Condition <- NA

birdasyn <- grep(paste(asyn, collapse = "|"), NormIntraRenditionScores[,"BirdID"])

NormIntraRenditionScores$Condition <- ifelse(rownames(NormIntraRenditionScores) %in% 
                                 as.character(birdasyn), 
                               "asyn", "con")

write.csv(NormIntraRenditionScores, file = paste0(sinkdirectory,"/",subDir,
                                              "/FF_UD_IntraRenditionScoresTable.csv"), 
          row.names = FALSE)

###- Modulation Index for MeanPitchAll, MedianPitchAll, and Intrarendition Scores
###- Modulation Index: (Post-Pre)/(Post+Pre)

mainDir <- "C:/Users/cesar/Documents/Julie_Lab/Data/FF/Conditions/"


##- MeanPitchAll

df <- read.table("~/Documents/Julie_Lab/Data/FF/Conditions/FF_UD_NormMeanPitchAllTable.csv",
                    header = TRUE, sep = ",")

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
      
      modulationindex.median <- (b$median - b$median[1]) / (b$median + b$median[1])
      
      modulationindex.CV <- (b$cv - b$cv[1]) / (b$cv + b$cv[1])
      
      newmatrix <- cbind.data.frame(b,modulationindex.mean, modulationindex.median,
                                    modulationindex.CV)
      
      newNormFile [[i]] <- newmatrix
      
    }
    
    newNormFile_K[[k]] <- newNormFile
    
  }
  
  return(newNormFile_K)
  
}

df <- modulationindex(data = df, factor_name = "Bird.Syll", variable_name='Variable')

df <- rbind.fill(rbind.fill(df))

write.csv(df, file = paste0(mainDir,"/FF_UD_MeanPitchAllTable.csv"), row.names = FALSE)

##- MedianPitchAll

df <- read.table("~/Documents/Julie_Lab/Data/FF/Conditions/FF_UD_NormMedianPitchAllTable.csv",
                 header = TRUE, sep = ",")

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
      
      modulationindex.median <- (b$median - b$median[1]) / (b$median + b$median[1])
      
      modulationindex.CV <- (b$cv - b$cv[1]) / (b$cv + b$cv[1])
      
      newmatrix <- cbind.data.frame(b,modulationindex.mean, modulationindex.median,
                                    modulationindex.CV)
      
      newNormFile [[i]] <- newmatrix
      
    }
    
    newNormFile_K[[k]] <- newNormFile
    
  }
  
  return(newNormFile_K)
  
}

df <- modulationindex(data = df, factor_name = "Bird.Syll", variable_name='Variable')

df <- rbind.fill(rbind.fill(df))

write.csv(df, file = paste0(mainDir,"/FF_UD_MedianPitchAllTable.csv"), row.names = FALSE)

##- intrasyllable-intrarendition scores 
##- Intrarendition CV and MAD

df <- read.table("~/Documents/Julie_Lab/Data/FF/Conditions/FF_UD_IntraRenditionScoresTable.csv",
                 header = TRUE, sep = ",")

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
      
      modulationindex.meanCV <- (b$meanCV - b$meanCV[1]) / (b$meanCV + b$meanCV[1])
      
      modulationindex.medianCV <- (b$medianCV - b$medianCV[1]) / (b$medianCV + b$medianCV[1])
      
      modulationindex.cvCV <- (b$cv.CV - b$cv.CV[1]) / (b$cv.CV + b$cv.CV[1])
      
      modulationindex.meanMAD <- (b$meanMAD - b$meanMAD[1]) / (b$meanMAD + b$meanMAD[1])
      
      modulationindex.medianMAD <- (b$medianMAD - b$medianMAD[1]) / (b$medianMAD + b$medianMAD[1])
      
      modulationindex.cvMAD <- (b$cv.MAD - b$cv.MAD[1]) / (b$cv.MAD + b$cv.MAD[1])
      
      newmatrix <- cbind.data.frame(b,modulationindex.meanCV, modulationindex.medianCV,
                                    modulationindex.cvCV, modulationindex.meanMAD, 
                                    modulationindex.medianMAD, modulationindex.cvMAD)
      
      newNormFile [[i]] <- newmatrix
      
    }
    
    newNormFile_K[[k]] <- newNormFile
    
  }
  
  return(newNormFile_K)
  
}

df <- modulationindex(data = df, factor_name = "Bird.Syll", variable_name='Variable')

df <- rbind.fill(rbind.fill(df))

write.csv(df, file = paste0(mainDir,"/FF_UD_IntraRenditionScoresTable.csv"), row.names = FALSE)

