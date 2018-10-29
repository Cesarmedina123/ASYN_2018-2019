#-- Script for converting VoiCE .xlsx to .csv format

#- Packages required readr, dplyr, ggplot2, openxslxrio
#- if not install using install.packages("")
## Install packages needed for code below

install.packages("tidyverse")
install.packages("reshape2")
install.packages("plyr")
install.packages("skimr")
install.packages("ggsignif")
install.packages("geom_boxlot")
inatall.packages("reshape")
install.packages("plotrix")
install.packages("pastecs")
install.packages("BSDA")
install.packages("broom")
install.packages("doBy")


## Read packages into local library

library(tidyverse)
library(reshape2)
library(plyr)
library(ggsignif)
library(skimr)
library(readr)
library(data.table)
library(doBy)
library(plotrix)
library(ggplot2)
library(dplyr)
library(pastecs)
library(readxl)
library(rio)
library(BSDA)
library(broom)



#- Finding and change working directory to "analyze/A_Syn_Project_
#- ASyn_Birds_SJM/Asyn_Birds_UD_SJM

getwd()
setwd("C:/Users/cesar/Documents/Julie_Lab/Data")
getwd() # confirm working directory is in proper location
dir() # check contents within working directory


# save working directory as a vector for future 

mainDir <- "C:/Users/cesar/Documents/Julie_Lab/Data"

# For loop to read .csv file of a single subject
# into r with all of the data frames that shares it's ID in
# the file name

#- Specify Bird ID here 

birdID <- "Wh214"

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

birdfile <- birdfile [,c(1, 4,7:19, 100,101)]

birdfile$Timepoint <- factor(birdfile$Timepoint,
                             levels = c('0', '1',
                                        '2', '3'),
                             ordered = TRUE)

# Converts birdfile dataframe to datatable format
# data frame now workable as a datatable
# as.data.table(birdfile) also works

setDT(birdfile)

# create new folder to save files to mainDir if one doesn't exist

subDir <- birdID

dir.create(file.path(mainDir, subDir))
setwd(file.path(mainDir, subDir))

# Final data frame will contain 29 variables for VoICE
# birdfile <- data.table(birdfile) # converts to data.table
# setkey(birdfile, Timepoint, Syllable) # assigns keys for functionality like as.factor

setwd(mainDir)

# Export final raw data frame in .csv format

write.csv(birdfile, file = paste(birdID, "UD_VoICE_Table.csv", 
                                 sep = "_"), 
          row.names = FALSE,
          file.path(subDir))



# Create function for graphing #

# vector of variables to graph

AFname <- colnames(birdfile[,1:18])

AF.graph <- fucntion(df, na.rm = TRUE, ...) {
  
  # for loop to produce ggplot2 graphs
  
  for(i in seq_along(birdfile[,1:14])) {
    
    # create plot for each acoustic featrure 
    # (comparing across timepoints) and facet by syllable
    
    ggplotaf <- ggplot(data = birdfile, aes(y= birdfile[,2],
                                           x = factor(Timepoint),
                                           fill = Timepoint)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(name = colnames(birdfile)[2]) +
    ggtitle(birdID) +
    labs(x = "Timepoint (month)", 
         caption = "Wilcox signed-rank test")+
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.text.x = element_text(angle  = 90, 
                                     hjust = 1, vjust = .5))+
    facet_wrap(~ Syllable, scale = "free", 
               strip.position = "left") +
    geom_signif(comparisons = list(c("Pre", "Post 1M"),
                                   c("Pre", "Post 2M"), 
                                   c("Pre", "Post 3M")), 
                test = 'wilcox.test',
                map_signif_level = TRUE, y_position = ,
                step_increase = .15, textsize = 3)
    
    # Print plot to screen
    
    print(ggplotaf)
  }
}

# Bar plot faceted by syllable and compared across timepoints

birdfile1 <- birdfile[,2:15]

af.graph <- function(birdfile, na.rm = TRUE, ...){
  for(i in seq_along(acousticfeatures)) { 
  AFplot <- ggplot(data = birdfile1, aes(
                            x = birdfile$Timepoint, 
                            y = birdfile1[,2], 
                            fill = birdfile$Timepoint)) +
  geom_boxplot() +
  scale_y_continuous(name = acousticfeatures [1])+
  ggtitle(birdID) +
  labs(x = "Timepoint (month)", 
       caption = "Wilcox signed-rank test")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = 
          element_text(angle  = 90, hjust = 1, vjust = .5))+
  facet_wrap(~ Syllable, scale = "free", 
             strip.position = "left") +
  geom_signif(comparisons = list(c("Pre", "Post 1M"),
                                 c("Pre", "Post 2M"), 
                                 c("Pre", "Post 3M")), 
              test = 'wilcox.test', step_increase = .15,
              map_signif_level = TRUE, textsize = 3) +
    ggsave()
  print(AFplot)
  }
}

# aes(y = birdfile$syllable.duration, fill = birdfile$Timepoint), 
# stat = "summary",fun.y = "mean"
# Change boxplot to bar plot

# Print Statistics

WilcoxTest.1 <- wilcox.test(value ~ Timepoint, 
                                 Wh214.SD, subset = Timepoint 
                                 %in% c("Pre", "Post 1M"),
                                 paired = TRUE, correct = F)

WilcoxTest.T1 <- tidy(WilcoxTest.1)

write.table(WilcoxTest.T1, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

# Print out statistics
  
# - Check paired t-test assumptions
# -- Are samples paired?
# -- Is this a large sample?
# -- Check the normality in the sample set
# -- When p > .05 the data are normally distributed
  
# - Computes the difference
  
w <- with(Wh214.SD,
          value[Timepoint == "Pre"] - value[Timepoint == "Post 1M"])

# - Shapiro-wilk normality test for the differences
  
shapiro.test(w)

# - Check if two populations' variance are similar
# -- F-test to test for homogeneity in variance using var.test
  
y <- var.test(value ~ Timepoint, Wh214.SD, 
              subset = Timepoint %in% c("Pre", "Post 1M"))

y

# -- w <- var.test(Wh214.SD,
# --          value[Timepoint == "Pre"] - value[Timepoint == "Post 1M"])


# - Comparing Pre to all post conditions

  
#--- Construction of for loop to save all plots made 
#--- automatically to a specified folder
  
bmp(file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE")
dev.off()


  