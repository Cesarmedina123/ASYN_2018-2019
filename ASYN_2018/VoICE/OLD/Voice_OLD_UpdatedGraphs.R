#-- Script for converting VoiCE .xlsx to .csv format

#- Packages required readr, dplyr, ggplot2, openxslxrio
#- if not install using install.packages("")

#- Finding and change working directory to "analyze/A_Syn_Project_
#- ASyn_Birds_SJM/Asyn_Birds_UD_SJM

getwd()
setwd("C:/Users/cesar/Documents/Julie_Lab/Data/OLD")
getwd() # confirm working directory is in proper location
dir() # check contents within working directory

# - Install packages

install.packages("readr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggsignif")
install.packages("geom_boxlot")
inatall.packages("readshape")
install.packages("xlsx")
install.packages("BSDA")
install.packages("broom")

#- Import multiple libraries/packages

library(readr)
library(ggplot2)
library(ggsignif)
library(readxl)
library(reshape)
library(rio)
library(BSDA)
library(dplyr)
library(broom)

#- Search through all subdirectories

#-- ifelse(condtion, x, y)

# Search through all subdirectories 

#- $ at the end means that its the end of the string
#- basename(list.files()) for without the directory prepended 

#- oldfiles <- list.files(pattern ="_UD_VoICE_SummaryTable.xlsx",
#-                        recursive = TRUE)

#- print(oldfiles) # check files

#-- convert xlsx files to .csv

#--- Creates vector of excel files to read

#infiles = list.files(pattern ="_UD_VoICE_SummaryTable.xlsx",
  #                   recursive = TRUE)

#--- Read each file and write it to csv 
#--- REMEMBER TO SPECIFY SHEET NUMBER

#lapply(infiles, function(f) {
 # df = read_excel(f, sheet = 4)
  #write.csv(df, gsub(".xlsx", "_Sheet4.csv", f), 
   #         row.names = FALSE, col.names = TRUE)
#}
#)

#-- lapply(infiles, file.remove) deletes .xlsx files



#- Read in excel file into R 
#- If sheet is not specified in the loop above
#- sheet can be specified here with sheet() commands

Wh214_A_0 <- read.table("Wh214_A_UD_VoICE_SummaryTable_Sheet1.csv",
                        header = TRUE, sep = ",", nrows=75)
Wh214_A_1 <- read.table("Wh214_A_UD_VoICE_SummaryTable_Sheet2.csv",
                        header = TRUE, sep = ",", nrows=75)
Wh214_A_2 <- read.table("Wh214_A_UD_VoICE_SummaryTable_Sheet3.csv",
                        header = TRUE, sep = ",", nrows=75)
Wh214_A_3 <- read.table("Wh214_A_UD_VoICE_SummaryTable_Sheet4.csv",
                        header = TRUE, sep = ",", nrows=75)

Wh214_B_0 <- read.table("Wh214_B_UD_VoICE_SummaryTable_Sheet1.csv",
                        header = TRUE, sep = ",", nrows=75)
Wh214_B_1 <- read.table("Wh214_B_UD_VoICE_SummaryTable_Sheet2.csv",
                        header = TRUE, sep = ",", nrows=75)
Wh214_B_2 <- read.table("Wh214_B_UD_VoICE_SummaryTable_Sheet3.csv",
                        header = TRUE, sep = ",", nrows=75)
Wh214_B_3 <- read.table("Wh214_B_UD_VoICE_SummaryTable_Sheet4.csv",
                        header = TRUE, sep = ",", nrows=75)

Wh214_C_0 <- read.table("Wh214_C_UD_VoICE_SummaryTable_Sheet1.csv",
                        header = TRUE, sep = ",", nrows=75)
Wh214_C_1 <- read.table("Wh214_C_UD_VoICE_SummaryTable_Sheet2.csv",
                        header = TRUE, sep = ",", nrows=75)
Wh214_C_2 <- read.table("Wh214_C_UD_VoICE_SummaryTable_Sheet3.csv",
                        header = TRUE, sep = ",", nrows=75)
Wh214_C_3 <- read.table("Wh214_C_UD_VoICE_SummaryTable_Sheet4.csv",
                        header = TRUE, sep = ",", nrows=75)

Wh214_D_0 <- read.table("Wh214_D_UD_VoICE_SummaryTable_Sheet1.csv",
                        header = TRUE, sep = ",", nrows=75)
Wh214_D_1 <- read.table("Wh214_D_UD_VoICE_SummaryTable_Sheet2.csv",
                        header = TRUE, sep = ",", nrows=75)
Wh214_D_2 <- read.table("Wh214_D_UD_VoICE_SummaryTable_Sheet3.csv",
                        header = TRUE, sep = ",", nrows=75)
Wh214_D_3 <- read.table("Wh214_D_UD_VoICE_SummaryTable_Sheet4.csv",
                        header = TRUE, sep = ",", nrows=75)

Wh214_E_0 <- read.table("Wh214_E_UD_VoICE_SummaryTable_Sheet1.csv",
                        header = TRUE, sep = ",", nrows=75)
Wh214_E_1 <- read.table("Wh214_E_UD_VoICE_SummaryTable_Sheet2.csv",
                        header = TRUE, sep = ",", nrows=75)
Wh214_E_2 <- read.table("Wh214_E_UD_VoICE_SummaryTable_Sheet3.csv",
                        header = TRUE, sep = ",", nrows=75)
Wh214_E_3 <- read.table("Wh214_E_UD_VoICE_SummaryTable_Sheet4.csv",
                        header = TRUE, sep = ",", nrows=75)

Wh214_F_0 <- read.table("Wh214_F_UD_VoICE_SummaryTable_Sheet1.csv",
                        header = TRUE, sep = ",", nrows=75)
Wh214_F_1 <- read.table("Wh214_F_UD_VoICE_SummaryTable_Sheet2.csv",
                        header = TRUE, sep = ",", nrows=75)
Wh214_F_2 <- read.table("Wh214_F_UD_VoICE_SummaryTable_Sheet3.csv",
                        header = TRUE, sep = ",", nrows=75)
Wh214_F_3 <- read.table("Wh214_F_UD_VoICE_SummaryTable_Sheet4.csv",
                        header = TRUE, sep = ",", nrows=75)

Wh214_G_0 <- read.table("Wh214_G_UD_VoICE_SummaryTable_Sheet1.csv",
                        header = TRUE, sep = ",", nrows=75)
Wh214_G_1 <- read.table("Wh214_G_UD_VoICE_SummaryTable_Sheet2.csv",
                        header = TRUE, sep = ",", nrows=75)
Wh214_G_2 <- read.table("Wh214_G_UD_VoICE_SummaryTable_Sheet3.csv",
                        header = TRUE, sep = ",", nrows=75)
Wh214_G_3 <- read.table("Wh214_G_UD_VoICE_SummaryTable_Sheet4.csv",
                        header = TRUE, sep = ",", nrows=75)

# Create a dataframe for individual acoustic features (re-do)
# by binding columns from above dataframes together
# Use this to generate graphs for individual acoustic features
# And run wilcoxon rank signed test for paired, nonparametric data

# Global vectors (applied to all features below)

Timepoint <- c("_0$","_1$", "_2$", "_3$")

Syllable <- c("_A_", "_B_", "_C_", "_D_", "_E_", "_F_", "_G_")

# Graph Syllable Duration

Wh214.SD <- cbind.data.frame(Wh214_A_0$syllable.duration, 
                 Wh214_A_1$syllable.duration, 
                 Wh214_A_2$syllable.duration, 
                 Wh214_A_3$syllable.duration,
                 Wh214_B_0$syllable.duration, 
                 Wh214_B_1$syllable.duration, 
                 Wh214_B_2$syllable.duration,
                 Wh214_B_3$syllable.duration,
                 Wh214_C_0$syllable.duration, 
                 Wh214_C_1$syllable.duration, 
                 Wh214_C_2$syllable.duration, 
                 Wh214_C_3$syllable.duration,
                 Wh214_D_0$syllable.duration, 
                 Wh214_D_1$syllable.duration, 
                 Wh214_D_2$syllable.duration, 
                 Wh214_D_3$syllable.duration,
                 Wh214_E_0$syllable.duration, 
                 Wh214_E_1$syllable.duration, 
                 Wh214_E_2$syllable.duration, 
                 Wh214_E_3$syllable.duration, 
                 Wh214_F_0$syllable.duration, 
                 Wh214_F_1$syllable.duration, 
                 Wh214_F_2$syllable.duration, 
                 Wh214_F_3$syllable.duration, 
                 Wh214_G_0$syllable.duration, 
                 Wh214_G_1$syllable.duration, 
                 Wh214_G_2$syllable.duration, 
                 Wh214_G_3$syllable.duration)

Wh214.SD.RS <- melt(Wh214.SD) 
#colnames(Wh214.SD.RS)

a <- list() # Reads from "Timepoint"

for(e in 1: length(Timepoint)) {
  a[[e]] <- grep(Timepoint[e], Wh214.SD.RS$variable, 
                 fixed = TRUE)
  print(a)
}

b <- c()

for(e in 1: dim(Wh214.SD.RS) [1]) {
  b[e] <- if (e %in% a[[1]]) {
    "Pre"
  } else {
    if (e %in% a[[2]]) {
      "Post 1M"
    } else {
      if (e %in% a[[3]]) {
        "Post 2M"
      } else {
        if (e %in% a[[4]]) {
          "Post 3M"
        }
      }
    }
  }
  print(b)}

Wh214.SD.Time <- cbind.data.frame(Wh214.SD.RS, Timepoint = b)
#colnames(Wh214.SD.Time)

d <- list() # Reads from "Syllable"

for (e in 1: length(Syllable)) {
  d[[e]] <- grep(Syllable[e], Wh214.SD.Time$variable,
                 fixed = TRUE)
  print(d)
}

f <- c()

for(e in 1: dim(Wh214.SD.Time) [1]) {
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

Wh214.SD <- cbind.data.frame(Wh214.SD.Time, Syllable = f)

Wh214.SD$Timepoint <- factor(Wh214.SD$Timepoint,
                             levels = c('Pre', 'Post 1M',
                                        'Post 2M', 'Post 3M'),
                             ordered = TRUE)

ggplot(Wh214.SD, aes(fill = Timepoint, y=value, x = Timepoint)) +
  geom_boxplot() +
  scale_y_continuous(name = "Mean Syllable Duration (ms)")+
  ggtitle("White 214") +
  labs(x = "Timepoint (month)", 
       caption = "Wilcox signed-rank test")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle  = 90, hjust = 1, vjust = .5))+
  facet_wrap(~ Syllable, scale = "free", 
             strip.position = "left") +
  geom_signif(comparisons = list(c("Pre", "Post 1M"),
                                 c("Pre", "Post 2M"), 
                                 c("Pre", "Post 3M")), 
              test = 'wilcox.test',
              map_signif_level = TRUE, y_position = ,
              step_increase = .15, textsize = 3)

Wh214.SD.Wilcox.1 <- wilcox.test(value ~ Timepoint, 
                                 Wh214.SD, subset = Timepoint 
                                 %in% c("Pre", "Post 1M"),
                                 paired = TRUE, correct = F)
Wh214.SD.Wilcox.1

Wh214.SD.Wilcox.T1 <- tidy(Wh214.SD.Wilcox)

write.table(Wh214.SD.Wilcox.1, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

Wh214.SD.Wilcox.2 <- wilcox.test(value ~ Timepoint, 
                                 Wh214.SD, subset = Timepoint 
                                 %in% c("Pre", "Post 2M"),
                                 paired = TRUE, correct = F)
Wh214.SD.Wilcox.2

write.table(Wh214.SD.Wilcox.2, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

Wh214.SD.Wilcox.3 <- wilcox.test(value ~ Timepoint, 
                                 Wh214.SD, subset = Timepoint 
                                 %in% c("Pre", "Post 3M"),
                                 paired = TRUE, correct = F)
Wh214.SD.Wilcox.3

write.table(Wh214.SD.Wilcox.3, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")


#-- Graph Mean Amplitude

Wh214.MA <- cbind.data.frame(Wh214_A_0$mean.amplitude, 
                             Wh214_A_1$mean.amplitude, 
                             Wh214_A_2$mean.amplitude, 
                             Wh214_A_3$mean.amplitude,
                             Wh214_B_0$mean.amplitude, 
                             Wh214_B_1$mean.amplitude, 
                             Wh214_B_2$mean.amplitude,
                             Wh214_B_3$mean.amplitude,
                             Wh214_C_0$mean.amplitude, 
                             Wh214_C_1$mean.amplitude, 
                             Wh214_C_2$mean.amplitude, 
                             Wh214_C_3$mean.amplitude,
                             Wh214_D_0$mean.amplitude, 
                             Wh214_D_1$mean.amplitude, 
                             Wh214_D_2$mean.amplitude, 
                             Wh214_D_3$mean.amplitude,
                             Wh214_E_0$mean.amplitude, 
                             Wh214_E_1$mean.amplitude, 
                             Wh214_E_2$mean.amplitude, 
                             Wh214_E_3$mean.amplitude,
                             Wh214_F_0$mean.amplitude, 
                             Wh214_F_1$mean.amplitude, 
                             Wh214_F_2$mean.amplitude, 
                             Wh214_F_3$mean.amplitude,
                             Wh214_G_0$mean.amplitude, 
                             Wh214_G_1$mean.amplitude, 
                             Wh214_G_2$mean.amplitude, 
                             Wh214_G_3$mean.amplitude)

Wh214.MA.RS <- melt(Wh214.MA) 
#colnames(Wh214.MA.RS)

a <- list() # Reads from "Timepoint"

for(e in 1: length(Timepoint)) {
  a[[e]] <- grep(Timepoint[e], Wh214.MA.RS$variable, 
                 fixed = TRUE)
  print(a)
}

b <- c()

for(e in 1: dim(Wh214.MA.RS) [1]) {
  b[e] <- if (e %in% a[[1]]) {
    "Pre"
  } else {
    if (e %in% a[[2]]) {
      "Post 1M"
    } else {
      if (e %in% a[[3]]) {
        "Post 2M"
      } else {
        if (e %in% a[[4]]) {
          "Post 3M"
        }
      }
    }
  }
  print(b)}

Wh214.MA.Time <- cbind.data.frame(Wh214.MA.RS, Timepoint = b)
#colnames(Wh214.MA.Time)


d <- list()

for (e in 1: length(Syllable)) {
  d[[e]] <- grep(Syllable[e], Wh214.MA.Time$variable,
                 fixed = TRUE)
  print(d)
}

f <- c()

for(e in 1: dim(Wh214.MA.Time) [1]) {
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

Wh214.MA <- cbind.data.frame(Wh214.MA.Time, Syllable = f)

Wh214.MA$Timepoint <- factor(Wh214.MA$Timepoint,
                             levels = c('Pre', 'Post 1M',
                                        'Post 2M', 'Post 3M'),
                             ordered = TRUE)

ggplot(Wh214.MA, aes(fill = Timepoint, y=value, x = Timepoint)) +
  geom_boxplot() +
  scale_y_continuous(name = "Mean Amplitude (dB)")+
  ggtitle("White 214") +
  labs(x = "Timepoint (month)", 
       caption = "Wilcox signed-rank test")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle  = 90, hjust = 1, vjust = .5))+
  facet_wrap(~ Syllable, scale = "free", 
             strip.position = "left") +
  geom_signif(comparisons = list(c("Pre", "Post 1M"),
                                 c("Pre", "Post 2M"), 
                                 c("Pre", "Post 3M")), 
              test = 'wilcox.test',
              map_signif_level = TRUE, y_position = ,
              step_increase = .15, textsize = 3)

Wh214.MA.Wilcox.1 <- wilcox.test(value ~ Timepoint, 
                                 Wh214.MA, subset = Timepoint 
                                 %in% c("Pre", "Post 1M"),
                                 paired = TRUE, correct = F)
Wh214.MA.Wilcox.1

Wh214.MA.Wilcox.T1 <- tidy(Wh214.MA.Wilcox)

write.table(Wh214.MA.Wilcox.1, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

Wh214.MA.Wilcox.2 <- wilcox.test(value ~ Timepoint, 
                                 Wh214.MA, subset = Timepoint 
                                 %in% c("Pre", "Post 2M"),
                                 paired = TRUE, correct = F)
Wh214.MA.Wilcox.2

write.table(Wh214.MA.Wilcox.2, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

Wh214.MA.Wilcox.3 <- wilcox.test(value ~ Timepoint, 
                                 Wh214.MA, subset = Timepoint 
                                 %in% c("Pre", "Post 3M"),
                                 paired = TRUE, correct = F)
Wh214.MA.Wilcox.3

write.table(Wh214.MA.Wilcox.3, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

#-- Graph Mean Pitch

Wh214.MP <- cbind.data.frame(Wh214_A_0$mean.pitch, 
                             Wh214_A_1$mean.pitch, 
                             Wh214_A_2$mean.pitch, 
                             Wh214_A_3$mean.pitch,
                             Wh214_B_0$mean.pitch, 
                             Wh214_B_1$mean.pitch, 
                             Wh214_B_2$mean.pitch,
                             Wh214_B_3$mean.pitch,
                             Wh214_C_0$mean.pitch, 
                             Wh214_C_1$mean.pitch, 
                             Wh214_C_2$mean.pitch, 
                             Wh214_C_3$mean.pitch,
                             Wh214_D_0$mean.pitch, 
                             Wh214_D_1$mean.pitch, 
                             Wh214_D_2$mean.pitch, 
                             Wh214_D_3$mean.pitch,
                             Wh214_E_0$mean.pitch, 
                             Wh214_E_1$mean.pitch, 
                             Wh214_E_2$mean.pitch, 
                             Wh214_E_3$mean.pitch,
                             Wh214_F_0$mean.pitch, 
                             Wh214_F_1$mean.pitch, 
                             Wh214_F_2$mean.pitch, 
                             Wh214_F_3$mean.pitch,
                             Wh214_G_0$mean.pitch, 
                             Wh214_G_1$mean.pitch, 
                             Wh214_G_2$mean.pitch, 
                             Wh214_G_3$mean.pitch)

Wh214.MP.RS <- melt(Wh214.MP) 
#colnames(Wh214.MP.RS)

a <- list() # Reads from "Timepoint"

for(e in 1: length(Timepoint)) {
  a[[e]] <- grep(Timepoint[e], Wh214.MP.RS$variable, 
                 fixed = TRUE)
  print(a)
}

b <- c()

for(e in 1: dim(Wh214.MP.RS) [1]) {
  b[e] <- if (e %in% a[[1]]) {
    "Pre"
  } else {
    if (e %in% a[[2]]) {
      "Post 1M"
    } else {
      if (e %in% a[[3]]) {
        "Post 2M"
      } else {
        if (e %in% a[[4]]) {
          "Post 3M"
        }
      }
    }
  }
  print(b)}

Wh214.MP.Time <- cbind.data.frame(Wh214.MP.RS, Timepoint = b)
#colnames(Wh214.MP.Time)

d <- list()

for (e in 1: length(Syllable)) {
  d[[e]] <- grep(Syllable[e], Wh214.MP.Time$variable,
                 fixed = TRUE)
  print(d)
}

f <- c()

for(e in 1: dim(Wh214.MP.Time) [1]) {
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

Wh214.MP <- cbind.data.frame(Wh214.MP.Time, Syllable = f)

Wh214.MP$Timepoint <- factor(Wh214.MP$Timepoint,
                             levels = c('Pre', 'Post 1M',
                                        'Post 2M', 'Post 3M'),
                             ordered = TRUE)

ggplot(Wh214.MP, aes(fill = Timepoint, y=value, x = Timepoint)) +
  geom_boxplot() +
  scale_y_continuous(name = "Mean Pitch (kHZ)")+
  ggtitle("White 214") +
  labs(x = "Timepoint (month)", 
       caption = "Wilcox signed-rank test")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle  = 90, hjust = 1, vjust = .5))+
  facet_wrap(~ Syllable, scale = "free", 
             strip.position = "left") +
  geom_signif(comparisons = list(c("Pre", "Post 1M"),
                                 c("Pre", "Post 2M"), 
                                 c("Pre", "Post 3M")), 
              test = 'wilcox.test',
              map_signif_level = TRUE, y_position = ,
              step_increase = .15, textsize = 3)

Wh214.MP.Wilcox.1 <- wilcox.test(value ~ Timepoint, 
                                 Wh214.MP, subset = Timepoint 
                                 %in% c("Pre", "Post 1M"),
                                 paired = TRUE, correct = F)
Wh214.MP.Wilcox.1

Wh214.MP.Wilcox.T1 <- tidy(Wh214.MP.Wilcox)

write.table(Wh214.MP.Wilcox.1, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

Wh214.MP.Wilcox.2 <- wilcox.test(value ~ Timepoint, 
                                 Wh214.MP, subset = Timepoint 
                                 %in% c("Pre", "Post 2M"),
                                 paired = TRUE, correct = F)
Wh214.MP.Wilcox.2

write.table(Wh214.MP.Wilcox.2, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

Wh214.MP.Wilcox.3 <- wilcox.test(value ~ Timepoint, 
                                 Wh214.MP, subset = Timepoint 
                                 %in% c("Pre", "Post 3M"),
                                 paired = TRUE, correct = F)
Wh214.MP.Wilcox.3

write.table(Wh214.MP.Wilcox.3, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")


#-- Graph Mean Frequency Modulation

Wh214.MFM <- cbind.data.frame(Wh214_A_0$mean.FM, 
                             Wh214_A_1$mean.FM, 
                             Wh214_A_2$mean.FM, 
                             Wh214_A_3$mean.FM,
                             Wh214_B_0$mean.FM, 
                             Wh214_B_1$mean.FM, 
                             Wh214_B_2$mean.FM,
                             Wh214_B_3$mean.FM,
                             Wh214_C_0$mean.FM, 
                             Wh214_C_1$mean.FM, 
                             Wh214_C_2$mean.FM, 
                             Wh214_C_3$mean.FM,
                             Wh214_D_0$mean.FM, 
                             Wh214_D_1$mean.FM, 
                             Wh214_D_2$mean.FM, 
                             Wh214_D_3$mean.FM,
                             Wh214_E_0$mean.FM, 
                             Wh214_E_1$mean.FM, 
                             Wh214_E_2$mean.FM, 
                             Wh214_E_3$mean.FM,
                             Wh214_F_0$mean.FM, 
                             Wh214_F_1$mean.FM, 
                             Wh214_F_2$mean.FM, 
                             Wh214_F_3$mean.FM,
                             Wh214_G_0$mean.FM, 
                             Wh214_G_1$mean.FM, 
                             Wh214_G_2$mean.FM, 
                             Wh214_G_3$mean.FM)

Wh214.MFM.RS <- melt(Wh214.MFM) 
#colnames(Wh214.MFM.RS)

a <- list() # Reads from "Timepoint"

for(e in 1: length(Timepoint)) {
  a[[e]] <- grep(Timepoint[e], Wh214.MFM.RS$variable, 
                 fixed = TRUE)
  print(a)
}

b <- c()

for(e in 1: dim(Wh214.MFM.RS) [1]) {
  b[e] <- if (e %in% a[[1]]) {
    "Pre"
  } else {
    if (e %in% a[[2]]) {
      "Post 1M"
    } else {
      if (e %in% a[[3]]) {
        "Post 2M"
      } else {
        if (e %in% a[[4]]) {
          "Post 3M"
        }
      }
    }
  }
  print(b)}


Wh214.MFM.Time <- cbind.data.frame(Wh214.MFM.RS, Timepoint = b)
colnames(Wh214.MFM.Time)

d <- list()

for (e in 1: length(Syllable)) {
  d[[e]] <- grep(Syllable[e], Wh214.MFM.Time$variable,
                 fixed = TRUE)
  print(d)
}

f <- c()

for(e in 1: dim(Wh214.MFM.Time) [1]) {
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

Wh214.MFM <- cbind.data.frame(Wh214.MFM.Time, Syllable = f)

Wh214.MFM$Timepoint <- factor(Wh214.MFM$Timepoint,
                              levels = c('Pre', 'Post 1M',
                                         'Post 2M', 'Post 3M'),
                              ordered = TRUE)

ggplot(Wh214.MFM, aes(fill = Timepoint, y=value, x = Timepoint)) +
  geom_boxplot(outlier.color = "NA") +
  scale_y_continuous(name = "Mean Frequency Modulation (kHz/s)")+
  ggtitle("White 214") +
  labs(x = "Timepoint (month)", 
       caption = "Wilcox signed-rank test")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle  = 90, hjust = 1, vjust = .5))+
  facet_wrap(~ Syllable, scale = "free", 
             strip.position = "left") +
  geom_signif(comparisons = list(c("Pre", "Post 1M"),
                                 c("Pre", "Post 2M"), 
                                 c("Pre", "Post 3M")), 
              test = 'wilcox.test',
              map_signif_level = TRUE, y_position = ,
              step_increase = .15, textsize = 3)

Wh214.MFM.Wilcox.1 <- wilcox.test(value ~ Timepoint, 
                                  Wh214.MFM, subset = Timepoint 
                                  %in% c("Pre", "Post 1M"),
                                  paired = TRUE, correct = F)
Wh214.MFM.Wilcox.1

Wh214.MFM.Wilcox.T1 <- tidy(Wh214.MFM.Wilcox)

write.table(Wh214.MFM.Wilcox.1, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

Wh214.MFM.Wilcox.2 <- wilcox.test(value ~ Timepoint, 
                                  Wh214.MFM, subset = Timepoint 
                                  %in% c("Pre", "Post 2M"),
                                  paired = TRUE, correct = F)
Wh214.MFM.Wilcox.2

write.table(Wh214.MFM.Wilcox.2, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

Wh214.MFM.Wilcox.3 <- wilcox.test(value ~ Timepoint, 
                                  Wh214.MFM, subset = Timepoint 
                                  %in% c("Pre", "Post 3M"),
                                  paired = TRUE, correct = F)
Wh214.MFM.Wilcox.3

write.table(Wh214.MFM.Wilcox.3, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

#-- Graph Mean Amplitude Modulation

Wh214.MAM <- cbind.data.frame(Wh214_A_0$mean.AM.2, 
                              Wh214_A_1$mean.AM.2, 
                              Wh214_A_2$mean.AM.2, 
                              Wh214_A_3$mean.AM.2,
                              Wh214_B_0$mean.AM.2, 
                              Wh214_B_1$mean.AM.2, 
                              Wh214_B_2$mean.AM.2,
                              Wh214_B_3$mean.AM.2,
                              Wh214_C_0$mean.AM.2, 
                              Wh214_C_1$mean.AM.2, 
                              Wh214_C_2$mean.AM.2, 
                              Wh214_C_3$mean.AM.2,
                              Wh214_D_0$mean.AM.2, 
                              Wh214_D_1$mean.AM.2, 
                              Wh214_D_2$mean.AM.2, 
                              Wh214_D_3$mean.AM.2,
                              Wh214_E_0$mean.AM.2, 
                              Wh214_E_1$mean.AM.2, 
                              Wh214_E_2$mean.AM.2, 
                              Wh214_E_3$mean.AM.2,
                              Wh214_F_0$mean.AM.2, 
                              Wh214_F_1$mean.AM.2, 
                              Wh214_F_2$mean.AM.2, 
                              Wh214_F_3$mean.AM.2,
                              Wh214_G_0$mean.AM.2, 
                              Wh214_G_1$mean.AM.2, 
                              Wh214_G_2$mean.AM.2, 
                              Wh214_G_3$mean.AM.2)

Wh214.MAM.RS <- melt(Wh214.MAM) 
#colnames(Wh214.MAM.RS)

a <- list() # Reads from "Timepoint"

for(e in 1: length(Timepoint)) {
  a[[e]] <- grep(Timepoint[e], Wh214.MAM.RS$variable, 
                 fixed = TRUE)
  print(a)
}

b <- c()

for(e in 1: dim(Wh214.MAM.RS) [1]) {
  b[e] <- if (e %in% a[[1]]) {
    "Pre"
  } else {
    if (e %in% a[[2]]) {
      "Post 1M"
    } else {
      if (e %in% a[[3]]) {
        "Post 2M"
      } else {
        if (e %in% a[[4]]) {
          "Post 3M"
        }
      }
    }
  }
  print(b)}

Wh214.MAM.Time <- cbind.data.frame(Wh214.MAM.RS, Timepoint = b)
#colnames(Wh214.MAM.Time)

d <- list()

for (e in 1: length(Syllable)) {
  d[[e]] <- grep(Syllable[e], Wh214.MAM.Time$variable,
                 fixed = TRUE)
  print(d)
}

f <- c()

for(e in 1: dim(Wh214.MAM.Time) [1]) {
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

Wh214.MAM <- cbind.data.frame(Wh214.MAM.Time, Syllable = f)

Wh214.MAM$Timepoint <- factor(Wh214.MAM$Timepoint,
                              levels = c('Pre', 'Post 1M',
                                         'Post 2M', 'Post 3M'),
                              ordered = TRUE)

ggplot(Wh214.MAM, aes(fill = Timepoint, y=value, x = Timepoint)) +
  geom_boxplot() +
  scale_y_continuous(name = "Mean Amplitude Modulation (dB/s))")+
  ggtitle("White 214") +
  labs(x = "Timepoint (month)", 
       caption = "Wilcox signed-rank test")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle  = 90, hjust = 1, vjust = .5))+
  facet_wrap(~ Syllable, scale = "free", 
             strip.position = "left") +
  geom_signif(comparisons = list(c("Pre", "Post 1M"),
                                 c("Pre", "Post 2M"), 
                                 c("Pre", "Post 3M")), 
              test = 'wilcox.test',
              map_signif_level = TRUE, y_position = ,
              step_increase = .15, textsize = 3)

Wh214.MAM.Wilcox.1 <- wilcox.test(value ~ Timepoint, 
                                  Wh214.MAM, subset = Timepoint 
                                  %in% c("Pre", "Post 1M"),
                                  paired = TRUE, correct = F)
Wh214.MAM.Wilcox.1

Wh214.MAM.Wilcox.T1 <- tidy(Wh214.MAM.Wilcox)

write.table(Wh214.MAM.Wilcox.1, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

Wh214.MAM.Wilcox.2 <- wilcox.test(value ~ Timepoint, 
                                  Wh214.MAM, subset = Timepoint 
                                  %in% c("Pre", "Post 2M"),
                                  paired = TRUE, correct = F)
Wh214.MAM.Wilcox.2

write.table(Wh214.MAM.Wilcox.2, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

Wh214.MAM.Wilcox.3 <- wilcox.test(value ~ Timepoint, 
                                  Wh214.MAM, subset = Timepoint 
                                  %in% c("Pre", "Post 3M"),
                                  paired = TRUE, correct = F)
Wh214.MAM.Wilcox.3

write.table(Wh214.MAM.Wilcox.3, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

#-- Graph Mean Entropy

Wh214.ME <- cbind.data.frame(Wh214_A_0$mean.entropy, 
                              Wh214_A_1$mean.entropy, 
                              Wh214_A_2$mean.entropy, 
                              Wh214_A_3$mean.entropy,
                              Wh214_B_0$mean.entropy, 
                              Wh214_B_1$mean.entropy, 
                              Wh214_B_2$mean.entropy,
                              Wh214_B_3$mean.entropy,
                              Wh214_C_0$mean.entropy, 
                              Wh214_C_1$mean.entropy, 
                              Wh214_C_2$mean.entropy, 
                              Wh214_C_3$mean.entropy,
                              Wh214_D_0$mean.entropy, 
                              Wh214_D_1$mean.entropy, 
                              Wh214_D_2$mean.entropy, 
                              Wh214_D_3$mean.entropy, 
                              Wh214_E_0$mean.entropy, 
                              Wh214_E_1$mean.entropy, 
                              Wh214_E_2$mean.entropy, 
                              Wh214_E_3$mean.entropy, 
                              Wh214_F_0$mean.entropy, 
                              Wh214_F_1$mean.entropy, 
                              Wh214_F_2$mean.entropy, 
                              Wh214_F_3$mean.entropy, 
                              Wh214_G_0$mean.entropy, 
                              Wh214_G_1$mean.entropy, 
                              Wh214_G_2$mean.entropy, 
                              Wh214_G_3$mean.entropy)

Wh214.ME.RS <- melt(Wh214.ME) 
#colnames(Wh214.ME.RS)

a <- list() # Reads from "Timepoint"

for(e in 1: length(Timepoint)) {
  a[[e]] <- grep(Timepoint[e], Wh214.ME.RS$variable, 
                 fixed = TRUE)
  print(a)
}

b <- c()

for(e in 1: dim(Wh214.ME.RS) [1]) {
  b[e] <- if (e %in% a[[1]]) {
    "Pre"
  } else {
    if (e %in% a[[2]]) {
      "Post 1M"
    } else {
      if (e %in% a[[3]]) {
        "Post 2M"
      } else {
        if (e %in% a[[4]]) {
          "Post 3M"
        }
      }
    }
  }
  print(b)}

Wh214.ME.Time <- cbind.data.frame(Wh214.ME.RS, Timepoint = b)
#colnames(Wh214.ME.Time)

d <- list()

for (e in 1: length(Syllable)) {
  d[[e]] <- grep(Syllable[e], Wh214.ME.Time$variable,
                 fixed = TRUE)
  print(d)
}

f <- c()

for(e in 1: dim(Wh214.ME.Time) [1]) {
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

Wh214.ME <- cbind.data.frame(Wh214.ME.Time, Syllable = f)

Wh214.ME$Timepoint <- factor(Wh214.ME$Timepoint,
                             levels = c('Pre', 'Post 1M',
                                        'Post 2M', 'Post 3M'),
                             ordered = TRUE)

ggplot(Wh214.ME, aes(fill = Timepoint, y=value, x = Timepoint)) +
  geom_boxplot() +
  scale_y_continuous(name = "Mean Entropy")+
  ggtitle("White 214") +
  labs(x = "Timepoint (month)", 
       caption = "Wilcox signed-rank test")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle  = 90, hjust = 1, vjust = .5))+
  facet_wrap(~ Syllable, scale = "free", 
             strip.position = "left") +
  geom_signif(comparisons = list(c("Pre", "Post 1M"),
                                 c("Pre", "Post 2M"), 
                                 c("Pre", "Post 3M")), 
              test = 'wilcox.test',
              map_signif_level = TRUE, y_position = ,
              step_increase = .15, textsize = 3)

Wh214.ME.Wilcox.1 <- wilcox.test(value ~ Timepoint, 
                                 Wh214.ME, subset = Timepoint 
                                 %in% c("Pre", "Post 1M"),
                                 paired = TRUE, correct = F)
Wh214.ME.Wilcox.1

Wh214.ME.Wilcox.T1 <- tidy(Wh214.ME.Wilcox)

write.table(Wh214.ME.Wilcox.1, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

Wh214.ME.Wilcox.2 <- wilcox.test(value ~ Timepoint, 
                                 Wh214.ME, subset = Timepoint 
                                 %in% c("Pre", "Post 2M"),
                                 paired = TRUE, correct = F)
Wh214.ME.Wilcox.2

write.table(Wh214.ME.Wilcox.2, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

Wh214.ME.Wilcox.3 <- wilcox.test(value ~ Timepoint, 
                                 Wh214.ME, subset = Timepoint 
                                 %in% c("Pre", "Post 3M"),
                                 paired = TRUE, correct = F)
Wh214.ME.Wilcox.3

write.table(Wh214.ME.Wilcox.3, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

#-- Graph Mean Pitch Goodness

Wh214.MPG <- cbind.data.frame(Wh214_A_0$mean.pitch.goodness, 
                              Wh214_A_1$mean.pitch.goodness, 
                              Wh214_A_2$mean.pitch.goodness, 
                              Wh214_A_3$mean.pitch.goodness,
                              Wh214_B_0$mean.pitch.goodness, 
                              Wh214_B_1$mean.pitch.goodness, 
                              Wh214_B_2$mean.pitch.goodness,
                              Wh214_B_3$mean.pitch.goodness,
                              Wh214_C_0$mean.pitch.goodness, 
                              Wh214_C_1$mean.pitch.goodness, 
                              Wh214_C_2$mean.pitch.goodness, 
                              Wh214_C_3$mean.pitch.goodness,
                              Wh214_D_0$mean.pitch.goodness, 
                              Wh214_D_1$mean.pitch.goodness, 
                              Wh214_D_2$mean.pitch.goodness, 
                              Wh214_D_3$mean.pitch.goodness,
                              Wh214_E_0$mean.pitch.goodness, 
                              Wh214_E_1$mean.pitch.goodness, 
                              Wh214_E_2$mean.pitch.goodness, 
                              Wh214_E_3$mean.pitch.goodness,
                              Wh214_F_0$mean.pitch.goodness, 
                              Wh214_F_1$mean.pitch.goodness, 
                              Wh214_F_2$mean.pitch.goodness, 
                              Wh214_F_3$mean.pitch.goodness,
                              Wh214_G_0$mean.pitch.goodness, 
                              Wh214_G_1$mean.pitch.goodness, 
                              Wh214_G_2$mean.pitch.goodness, 
                              Wh214_G_3$mean.pitch.goodness)

Wh214.MPG.RS <- melt(Wh214.MPG) 
#colnames(Wh214.MPG.RS)

a <- list() # Reads from "Timepoint"

for(e in 1: length(Timepoint)) {
  a[[e]] <- grep(Timepoint[e], Wh214.MPG.RS$variable, 
                 fixed = TRUE)
  print(a)
}

b <- c()

for(e in 1: dim(Wh214.MPG.RS) [1]) {
  b[e] <- if (e %in% a[[1]]) {
    "Pre"
  } else {
    if (e %in% a[[2]]) {
      "Post 1M"
    } else {
      if (e %in% a[[3]]) {
        "Post 2M"
      } else {
        if (e %in% a[[4]]) {
          "Post 3M"
        }
      }
    }
  }
  print(b)}

Wh214.MPG.Time <- cbind.data.frame(Wh214.MPG.RS, Timepoint = b)
#colnames(Wh214.MPG.Time)

d <- list()

for (e in 1: length(Syllable)) {
  d[[e]] <- grep(Syllable[e], Wh214.MPG.Time$variable,
                 fixed = TRUE)
  print(d)
}

f <- c()

for(e in 1: dim(Wh214.MPG.Time) [1]) {
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

Wh214.MPG <- cbind.data.frame(Wh214.MPG.Time, Syllable = f)

Wh214.MPG$Timepoint <- factor(Wh214.MPG$Timepoint,
                              levels = c('Pre', 'Post 1M',
                                         'Post 2M', 'Post 3M'),
                              ordered = TRUE)

ggplot(Wh214.MPG, aes(fill = Timepoint, y=value, x = Timepoint)) +
  geom_boxplot() +
  scale_y_continuous(name = "Mean Pitch Goodness")+
  ggtitle("White 214") +
  labs(x = "Timepoint (month)", 
       caption = "Wilcox signed-rank test")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle  = 90, hjust = 1, vjust = .5))+
  facet_wrap(~ Syllable, scale = "free", 
             strip.position = "left") +
  geom_signif(comparisons = list(c("Pre", "Post 1M"),
                                 c("Pre", "Post 2M"), 
                                 c("Pre", "Post 3M")), 
              test = 'wilcox.test',
              map_signif_level = TRUE, y_position = ,
              step_increase = .15, textsize = 3)

Wh214.MPG.Wilcox.1 <- wilcox.test(value ~ Timepoint, 
                                  Wh214.MPG, subset = Timepoint 
                                  %in% c("Pre", "Post 1M"),
                                  paired = TRUE, correct = F)
Wh214.MPG.Wilcox.1

Wh214.MPG.Wilcox.T1 <- tidy(Wh214.MPG.Wilcox)

write.table(Wh214.MPG.Wilcox.1, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

Wh214.MPG.Wilcox.2 <- wilcox.test(value ~ Timepoint, 
                                  Wh214.MPG, subset = Timepoint 
                                  %in% c("Pre", "Post 2M"),
                                  paired = TRUE, correct = F)
Wh214.MPG.Wilcox.2

write.table(Wh214.MPG.Wilcox.2, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

Wh214.MPG.Wilcox.3 <- wilcox.test(value ~ Timepoint, 
                                  Wh214.MPG, subset = Timepoint 
                                  %in% c("Pre", "Post 3M"),
                                  paired = TRUE, correct = F)
Wh214.MPG.Wilcox.3

write.table(Wh214.MPG.Wilcox.3, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")


#-- Graph Mean Mean Frequency

Wh214.MMF <- cbind.data.frame(Wh214_A_0$mean.mean.freq, 
                              Wh214_A_1$mean.mean.freq, 
                              Wh214_A_2$mean.mean.freq, 
                              Wh214_A_3$mean.mean.freq,
                              Wh214_B_0$mean.mean.freq, 
                              Wh214_B_1$mean.mean.freq, 
                              Wh214_B_2$mean.mean.freq,
                              Wh214_B_3$mean.mean.freq,
                              Wh214_C_0$mean.mean.freq, 
                              Wh214_C_1$mean.mean.freq, 
                              Wh214_C_2$mean.mean.freq, 
                              Wh214_C_3$mean.mean.freq,
                              Wh214_D_0$mean.mean.freq, 
                              Wh214_D_1$mean.mean.freq, 
                              Wh214_D_2$mean.mean.freq, 
                              Wh214_D_3$mean.mean.freq,
                              Wh214_E_0$mean.mean.freq, 
                              Wh214_E_1$mean.mean.freq, 
                              Wh214_E_2$mean.mean.freq, 
                              Wh214_E_3$mean.mean.freq,
                              Wh214_F_0$mean.mean.freq, 
                              Wh214_F_1$mean.mean.freq, 
                              Wh214_F_2$mean.mean.freq, 
                              Wh214_F_3$mean.mean.freq,
                              Wh214_G_0$mean.mean.freq, 
                              Wh214_G_1$mean.mean.freq, 
                              Wh214_G_2$mean.mean.freq, 
                              Wh214_G_3$mean.mean.freq)

Wh214.MMF.RS <- melt(Wh214.MMF) 
#colnames(Wh214.MMF.RS)

a <- list() # Reads from "Timepoint"

for(e in 1: length(Timepoint)) {
  a[[e]] <- grep(Timepoint[e], Wh214.MMF.RS$variable, 
                 fixed = TRUE)
  print(a)
}

b <- c()

for(e in 1: dim(Wh214.MMF.RS) [1]) {
  b[e] <- if (e %in% a[[1]]) {
    "Pre"
  } else {
    if (e %in% a[[2]]) {
      "Post 1M"
    } else {
      if (e %in% a[[3]]) {
        "Post 2M"
      } else {
        if (e %in% a[[4]]) {
          "Post 3M"
        }
      }
    }
  }
  print(b)}

Wh214.MMF.Time <- cbind.data.frame(Wh214.MMF.RS, Timepoint = b)
#colnames(Wh214.MMF.Time)

d <- list()

for (e in 1: length(Syllable)) {
  d[[e]] <- grep(Syllable[e], Wh214.MMF.Time$variable,
                 fixed = TRUE)
  print(d)
}


f <- c()

for(e in 1: dim(Wh214.MMF.Time) [1]) {
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

Wh214.MMF <- cbind.data.frame(Wh214.MMF.Time, Syllable = f)

Wh214.MMF$Timepoint <- factor(Wh214.MMF$Timepoint,
                              levels = c('Pre', 'Post 1M',
                                         'Post 2M', 'Post 3M'),
                              ordered = TRUE)

ggplot(Wh214.MMF, aes(fill = Timepoint, y=value, x = Timepoint)) +
  geom_boxplot() +
  scale_y_continuous(name = "Mean Frequency (kHz)")+
  ggtitle("White 214") +
  labs(x = "Timepoint (month)", 
       caption = "Wilcox signed-rank test")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle  = 90, hjust = 1, vjust = .5))+
  facet_wrap(~ Syllable, scale = "free", 
             strip.position = "left") +
  geom_signif(comparisons = list(c("Pre", "Post 1M"),
                                 c("Pre", "Post 2M"), 
                                 c("Pre", "Post 3M")), 
              test = 'wilcox.test',
              map_signif_level = TRUE, y_position = ,
              step_increase = .15, textsize = 3)

Wh214.MMF.Wilcox.1 <- wilcox.test(value ~ Timepoint, 
                                  Wh214.MMF, subset = Timepoint 
                                  %in% c("Pre", "Post 1M"),
                                  paired = TRUE, correct = F)
Wh214.MMF.Wilcox.1

Wh214.MMF.Wilcox.T1 <- tidy(Wh214.MMF.Wilcox)

write.table(Wh214.MMF.Wilcox.1, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

Wh214.MMF.Wilcox.2 <- wilcox.test(value ~ Timepoint, 
                                  Wh214.MMF, subset = Timepoint 
                                  %in% c("Pre", "Post 2M"),
                                  paired = TRUE, correct = F)
Wh214.MMF.Wilcox.2

write.table(Wh214.MMF.Wilcox.2, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

Wh214.MMF.Wilcox.3 <- wilcox.test(value ~ Timepoint, 
                                  Wh214.MMF, subset = Timepoint 
                                  %in% c("Pre", "Post 3M"),
                                  paired = TRUE, correct = F)
Wh214.MMF.Wilcox.3

write.table(Wh214.MMF.Wilcox.3, 
            file = "C:/Users/cesar/Documents/Julie_Lab/Analysis/VoICE/a_syn")

#--- Transforms the 4 columns with the factor and 
#--- the values into two columns containing both details


#--- Create a vector of values to input into old data frame


#----- creaing a list with the elements specified from Timepoint list


#----- We created a vector named "b" with c() to just write the list
#----- if loop to take the groups in a and write a new list with the
#----- conditions written in


#--- Creates a new data frame with timepoint added using cbind.data.frame


#--- Create master sheet of SD data for this bird
#--- Contains 2 new columns (Timepoint, syllable)
#--- Creating new column "Condition" for syllable type


# Use below in case top for loop doesn't work
#f <- c(rep("A", 300), rep("B", 300), rep("C", 300),
 #      rep("D", 300))


#--- Graphing data for acoustic features
#--- Change fill to condition


#---- Specify order of data read for box plots


#--- Graph data by acoustic feature and timepoint, 
#--- then run WILCOX rank signed test for significance


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

##### Code for calculating and graphing CV of acoustic features ####

condition <- 