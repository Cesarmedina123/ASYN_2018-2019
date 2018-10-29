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

myfiles <- list.files(path = ".", 
                      pattern = "_parametersN.csv",
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

write.csv(masterfile, file = paste0(sinkdirectory,"/","UD_FF_UnfinishedTable3.csv"), 
          row.names = FALSE)

masterfile <- read.table(file = paste0(sinkdirectory,"/UD_FF_UnfinishedTable3.csv"), 
                         header = TRUE, sep = ",")

# Get mean, median, SD, and CV for meanPitchAll and medianPitchAll
# to represent intrasyllable-interrendition

setDT(masterfile)

setkey(masterfile$Bird.Syll.Time)

# Sort text data in R by multiple columns using the order() function

masterfile <- masterfile[with(masterfile, order(masterfile$Bird.Syll, 
                                                masterfile$Timepoint,
                                                decreasing = FALSE)), ]

# Experimental Condition of birds

asyn <- c("Bk174","Rd321","Bk262","Bk278","Bk293","Bk295","Wh203","Bk280","Wh206","Wh214")
con <- c("Wh145","Bk268","Rd346","Bk277","Wh211","Bk286")

# Create normalize function to normalize values by baseline timepoint for each Bird.Syll

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

#MeanmeanPitchAll

meanmeanPitchAll <- masterfile[,list(mean=mean(meanPitchAll), median = median(meanPitchAll),
                                     sd=sd(meanPitchAll), 
                                     cv = sd(meanPitchAll)/mean(meanPitchAll)*100),
                               by=Bird.Syll.Time]

BirdID <- strtrim(meanmeanPitchAll$Bird.Syll.Time, 5)

Bird.Syll <- strtrim(meanmeanPitchAll$Bird.Syll.Time, 7)

Timepoint <- str_sub(meanmeanPitchAll$Bird.Syll.Time, start = 9)

meanmeanPitchAll <- cbind.data.frame(meanmeanPitchAll, BirdID = BirdID, 
                                     Bird.Syll = Bird.Syll, Timepoint = Timepoint)

meanmeanPitchAll$Variable <- "Pitch"

normalize(data= meanmeanPitchAll, factor_name = "Bird.Syll", variable_name='Variable')

NormMeanPA <- rbind.fill(rbind.fill(newNormFile))

as.data.frame(NormMeanPA)

NormMeanPA$Condition <- NA

birdasyn <- grep(paste(asyn, collapse = "|"), NormMeanPA[,"BirdID"])

NormMeanPA$Condition <- ifelse(rownames(NormMeanPA) %in% 
                                   as.character(birdasyn), 
                                 "asyn", "con")

write.csv(NormMeanPA, file = paste0(sinkdirectory,"/",subDir,
                                     "/UD_FF_NormMeanPA.csv"), row.names = FALSE)

## meanMedianPitchAll

meanmedianPitchAll <- masterfile[,list(mean=mean(medianPitchAll), median = median(medianPitchAll),
                                       sd=sd(medianPitchAll), 
                                       cv = sd(medianPitchAll)/mean(medianPitchAll)*100),
                                 by=Bird.Syll.Time]

BirdID <- strtrim(meanmedianPitchAll$Bird.Syll.Time, 5)

Bird.Syll <- strtrim(meanmedianPitchAll$Bird.Syll.Time, 7)

Timepoint <- str_sub(meanmedianPitchAll$Bird.Syll.Time, start = 9)

meanmedianPitchAll <- cbind.data.frame(meanmedianPitchAll, BirdID = BirdID, 
                                     Bird.Syll = Bird.Syll, Timepoint = Timepoint)

meanmedianPitchAll$Variable <- "Pitch"

newNormFile <- normalize(data= meanmedianPitchAll, factor_name = 
            "Bird.Syll", variable_name='Variable')

NormMedianPA <- rbind.fill(rbind.fill(newNormFile))

as.data.frame(NormMedianPA)

NormMedianPA$Condition <- NA

birdasyn <- grep(paste(asyn, collapse = "|"), NormMedianPA[,"BirdID"])

NormMedianPA$Condition <- ifelse(rownames(NormMedianPA) %in% 
                                           as.character(birdasyn), 
                                         "asyn", "con")

write.csv(NormMedianPA, file = paste0(sinkdirectory,"/",subDir,
                                    "/UD_FF_NormMedianPA.csv"), row.names = FALSE)

# Get averages for all of the 'metrics' to represent 
# intrasyllable-intrarendition scores 

meanMasterFileScores <- masterfile[,list(avgmeanPitchAll = mean(meanPitchAll), 
                                         avgSD = mean(std), avgCV = mean(cv),
                                         avgmedianPitchAll = mean(medianPitchAll),
                                         avgMAD = mean(mad)), by = Bird.Syll.Time]


BirdID <- strtrim(meanMasterFileScores$Bird.Syll.Time, 5)

Bird.Syll <- strtrim(meanMasterFileScores$Bird.Syll.Time, 7)

Timepoint <- str_sub(meanMasterFileScores$Bird.Syll.Time, start = 9)

meanMasterFileScores <- cbind.data.frame(meanMasterFileScores, BirdID = BirdID, 
                                     Bird.Syll = Bird.Syll, Timepoint = Timepoint)

meanMasterFileScores$Variable <- "Pitch"

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
      
      normMean <- b$avgmeanPitchAll / b$avgmeanPitchAll[1]
      
      normMedian <- b$avgmedianPitchAll / b$avgmedianPitchAll[1]
      
      normSD <- b$avgSD / b$avgSD[1]
      
      normCV <- b$avgCV / b$avgCV [1]
      
      normMAD <- b$avgMAD / b$avgMAD [1]
      
      newmatrix <- cbind.data.frame(b,normMean,normMedian,normSD,normCV, normMAD)
      
      newNormFile [[i]] <- newmatrix
      
    }
    
    newNormFile_K[[k]] <- newNormFile
    
    
  }
  
  
  return(newNormFile_K)
  
}

newNormFile <- normalize(data= meanMasterFileScores, factor_name = 
                           "Bird.Syll", variable_name='Variable')

NormMasterFileScores <- rbind.fill(rbind.fill(newNormFile))

as.data.frame(NormMasterFileScores)

NormMasterFileScores$Condition <- NA

birdasyn <- grep(paste(asyn, collapse = "|"), NormMasterFileScores[,"BirdID"])

NormMasterFileScores$Condition <- ifelse(rownames(NormMasterFileScores) %in% 
                                 as.character(birdasyn), 
                               "asyn", "con")

write.csv(NormMasterFileScores, file = paste0(sinkdirectory,"/",subDir,
                                              "/UD_FF_NormMasterFileScores.csv"), 
          row.names = FALSE)


# normfile <- normfile[complete.cases(normfile), ]

########## Graph values for Fundamental Frequency ############
# subset by condition and timepoint since its all pitch data
# to run comparisons using ggpubr or ggsignif
############## Graph using GGPUBR because it's BETTER ##############

library(readr)
library(tidyverse)
library(ggsignif)
library(svglite)
library(ggpubr)
library(broom)

newDir <- paste0(mainDir,"/",subDir)

setwd(newDir)

graph <- read.table(file = paste0(newDir,"/UD_FF_NormMeanPA.csv"), 
                    header = TRUE, sep = ",")

acousticfeature <- "Pitch"

# Loop through all rows and identify rows with 0 and remove them
# This filters down the timepoints

row_sub <- apply(graph, 1, function(row) all(row != 0 ))

graph <- graph[row_sub,]

# Remove specific data from data

graph <- graph[graph$birdid.Syll != "Bk286.A", ] # Similarity batch awful (so removed)
graph <- graph[graph$birdID.Syll != "Wh214.F", ] # Similarity batch awful (so removed)


graph <- graph[graph$Variable == acousticfeature,]

# graph <- graph[graph$Timepoint != 1,] # filter by timepoint with example of removing 1 month

# Columname to be used later on for plotting

columname <- as.character(as.vector(graph$Variable[1]))

columname <- gsub("."," ", columname, fixed = TRUE)

# Capitalize first letter of each word with toupper function
# Will be used for naming in plots

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

columname <- sapply(columname,simpleCap) # not necessary to loop over 1 element

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

# - Shapiro-wilk normality test for the differences

res <- aggregate(graph$mean ~ Condition + Timepoint, 
                 data = graph, FUN = function(X) shapiro.test(x)$p.value)

shapiro.test(graph$avgmeanPitchAll)

# - Check if two populations' variance are similar
# -- F-test to test for homogeneity in variance using var.test

y <- var.test(graoh ~ Timepoint, 
              subset = Timepoint %in% c(0, 1))

## Run a Wilcoxon Rank Sum Test on data across timepoints ##
## Group by condition ##

WilcoxTest.1 <- wilcox.test(mean ~ Timepoint + Bird.Syll, 
                            data = graph, subset = Condition 
                            %in% c(0,1),
                            paired = FALSE, correct = FALSE)

WilcoxTest.T1 <- tidy(WilcoxTest.1)

write.table(WilcoxTest.T1, 
            file = paste0(newDir,
                         "/WilTest_0_1_mean_NormMeanPA.csv"),
            sep = ",")


#--- Construction of for loop to save all plots made 
#--- automatically to a specified folder

# Prefered plot (Wilcoxon Rank Sum Test)

#- avgmeanPitchAll

plot1 <- ggbarplot(graph, x = "Condition", y = "mean", 
                   add = c("mean_se", "jitter"),
                   color = "Condition", fill = "Condition",
                   position = position_dodge(0.8),
                   facet.by = "Timepoint",
                   ylab = paste("Mean of", columname)) +
  scale_color_manual(values =c("grey10","grey28")) +
  scale_fill_manual(values = c("grey28","grey56")) +
  facet_wrap(~ Timepoint, strip.position = "left") +
  geom_signif(comparisons = list(c("asyn","con")),
              test = 'wilcox.test', 
              test.args = list(alternative = "two.sided",
                               var.equal = FALSE, paired = FALSE), 
              map_signif_level = TRUE,
              step_increase = .15, textsize = 3.25) 

plot1 <- ggpar(plot1, xlab = FALSE, ylim = c(0.75,1.5)) #ylim = c(?,?) for scale

#- avgCV

plot2 <- ggbarplot(graph, x = "Condition", y = "cv", 
                   add = c("mean_se", "jitter"),
                   color = "Condition", fill = "Condition",
                   position = position_dodge(0.8),
                   facet.by = "Timepoint",
                   ylab = paste("CV of", columname)) +
  scale_color_manual(values =c("grey10","grey28")) +
  scale_fill_manual(values = c("grey28","grey56")) +
  facet_wrap(~ Timepoint, strip.position = "left") +
  geom_signif(comparisons = list(c("asyn","con")),
              test = 'wilcox.test', 
              test.args = list(alternative = "two.sided",
                               var.equal = FALSE, paired = FALSE), 
              map_signif_level = TRUE,
              step_increase = .15, textsize = 3.25) 

plot2 <- ggpar(plot2, xlab = FALSE, ylim = c(0,17.5)) #ylim = c(?,?) for scale

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## Combine plots using multiplot function ##

multiplot <- multiplot(plot1, plot2, cols = 1)

print(multiplot)

# Line graph for paired comparisons

plot1 <- ggline(graph, x = "Condition", y = "normMean",
                add = c("mean_se", "jitter"),
                group= c("Timepoint","Condition"),
                color = "Condition", fill = "Condition",
                position = position_dodge(0.8),
                ylab = paste("Mean of", columname)) +
  scale_color_manual(values =c("grey10","grey28")) +
  scale_fill_manual(values = c("grey28","grey56")) +
  facet_wrap(~ Timepoint, strip.position = "left") +
  geom_signif(comparisons = list(c("asyn","con")),
              test = 'wilcox.test', 
              test.args = list(alternative = "two.sided",
                               var.equal = FALSE, paired = FALSE), 
              map_signif_level = TRUE,
              step_increase = .15, textsize = 3.25)

plot6 <- ggbarplot(graph, x = "Condition", y = "avgCV", 
                   add = c("mean_se"),
                   color = "Condition", fill = "Condition",
                   facet.by = "Timepoint",
                   ylab = paste("CV of", columname)) +
  scale_color_manual(values =c("red3","green4")) +
  scale_fill_manual(values = c("pink1","greenyellow")) +
  facet_wrap(~ Timepoint, strip.position = "left") +
  geom_signif(comparisons = list(c("asyn","gfp")),
              test = 'wilcox.test', 
              test.args = list(alternative = "two.sided",
                               var.equal = FALSE, paired = FALSE), 
              map_signif_level = TRUE,
              step_increase = -2.5, textsize = 3.25) 

ggpar(plot6, xlab = FALSE, ylim = c(500,1300)) #ylim = c(?,?) for scale



  
