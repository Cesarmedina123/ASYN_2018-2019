######- Hypothesis Testing of Individual Acoustic Features
######- subset by variable, ondition and timepoint
######- to run comparisons using ggpubr or ggsignif

library(readr)
library(tidyverse)
library(ggpubr)
library(svglite)
library(car)
library(broom)
library(data.table)

mainDir <- "C:/Users/cesar/Documents/Julie_Lab/Data/ASYN_2018/VoICE/Conditions"

df <- read.table(file = paste0(mainDir,"/VoICE_UD_Table_Birds.csv"), 
                   header = TRUE, sep = ",")

####- Statistics function to loop over two columns 
####- with X levels and generate a tidy dataframe 
####- made-up of appropriate statistical comparisons 
####- (e.g., wilcoxon rank sum test, unpaired t-test)
####- depending on homogeneity (shapiro-wilks test)
####- and variance of data (bartlett/levene test)


##### Acoustic Features for Intersyllable-interrendition comparisons #####
##### By timepoint and condition #####

data <- read.table(file = paste0(mainDir,"/VoICE_UD_Table_Birds.csv"), 
                 header = TRUE, sep = ",")

#### Mean Scores ####

### Wilcoxon Rank Sum Test on Normalized Mean AM 2 ###
### by timepoint and comparing conditions ###

##- 0 Timepoint 

data_meanAM2_asyn_0 <- data[data$Timepoint == 0 & 
                           data$Variable == "mean.AM.2" &
                           data$Condition == "asyn", ]

data_meanAM2_con_0 <- data[data$Timepoint == 0 & 
                          data$Variable == "mean.AM.2" &
                          data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanAM2_asyn_0, x = "NormMean")

ggdensity(data = data_meanAM2_asyn_0, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanAM2_asyn_0_normmean <- shapiro.test(x = data_meanAM2_asyn_0$NormMean)

if(annot_meanAM2_asyn_0_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanAM2_con_0, x = "NormMean")

ggdensity(data = data_meanAM2_con_0, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanAM2_con_0_normmean <- shapiro.test(x = data_meanAM2_con_0$NormMean)

if(annot_meanAM2_con_0_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanAM2_0 <- data[data$Timepoint == 0 & data$Variable == "mean.AM.2", ]

if(annot_meanAM2_con_0_normmean$p.value & 
   annot_meanAM2_asyn_0_normmean$p.value > 0.05){
  
  annot_meanAM2_0_var <- bartlett.test(NormMean ~ Condition, 
                                  data = data_meanAM2_0)
  print(annot_meanAM2_0_var)
  print("Variance is equal between groups")
  
} else if(annot_meanAM2_con_0_normmean$p.value | 
          annot_meanAM2_asyn_0_normmean$p.value < 0.05){
  
  annot_meanAM2_0_var <- leveneTest(NormMean ~ Condition, 
                               data = data_meanAM2_0)
  print(annot_meanAM2_0_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_0_meanAM2 <- wilcox.test(NormMean ~ Condition, data = data_meanAM2_0,
                                  paired = FALSE, alternative = "two.sided")

annot_Valmean_0_meanAM2 <- tidy(annot_Valmean_0_meanAM2)

##- 1 Timepoint 

data_meanAM2_asyn_1 <- data[data$Timepoint == 1 & 
                              data$Variable == "mean.AM.2" &
                              data$Condition == "asyn", ]

data_meanAM2_con_1 <- data[data$Timepoint == 1 & 
                             data$Variable == "mean.AM.2" &
                             data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanAM2_asyn_1, x = "NormMean")

ggdensity(data = data_meanAM2_asyn_1, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanAM2_asyn_1_normmean <- tidy(shapiro.test(x = data_meanAM2_asyn_1$NormMean), id = data)

if(annot_meanAM2_asyn_1_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanAM2_con_1, x = "NormMean")

ggdensity(data = data_meanAM2_con_1, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanAM2_con_1_normmean <- shapiro.test(x = data_meanAM2_con_1$NormMean)

if(annot_meanAM2_con_1_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanAM2_1 <- data[data$Timepoint == 1 & data$Variable == "mean.AM.2", ]

if(annot_meanAM2_con_1_normmean$p.value & 
   annot_meanAM2_asyn_1_normmean$p.value > 0.05){
  
  annot_meanAM2_1_var <- bartlett.test(NormMean ~ Condition, 
                                       data = data_meanAM2_1)
  print(annot_meanAM2_1_var)
  print("Variance is equal between groups")
  
} else if(annot_meanAM2_con_1_normmean$p.value | 
          annot_meanAM2_asyn_1_normmean$p.value < 0.05){
  
  annot_meanAM2_1_var <- leveneTest(NormMean ~ Condition, 
                                    data = data_meanAM2_1)
  print(annot_meanAM2_1_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_1_meanAM2 <- wilcox.test(NormMean ~ Condition, data = data_meanAM2_1,
                                       paired = FALSE, alternative = "two.sided")

annot_Valmean_1_meanAM2 <- tidy(annot_Valmean_1_meanAM2)

##- 2 Timepoint 

data_meanAM2_asyn_2 <- data[data$Timepoint == 2 & 
                              data$Variable == "mean.AM.2" &
                              data$Condition == "asyn", ]

data_meanAM2_con_2 <- data[data$Timepoint == 2 & 
                             data$Variable == "mean.AM.2" &
                             data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanAM2_asyn_2, x = "NormMean")

ggdensity(data = data_meanAM2_asyn_2, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanAM2_asyn_2_normmean <- shapiro.test(x = data_meanAM2_asyn_2$NormMean)

if(annot_meanAM2_asyn_2_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanAM2_con_2, x = "NormMean")

ggdensity(data = data_meanAM2_con_2, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanAM2_con_2_normmean <- shapiro.test(x = data_meanAM2_con_2$NormMean)

if(annot_meanAM2_con_2_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanAM2_2 <- data[data$Timepoint == 2 & data$Variable == "mean.AM.2", ]

if(annot_meanAM2_con_2_normmean$p.value & 
   annot_meanAM2_asyn_2_normmean$p.value > 0.05){
  
  annot_meanAM2_2_var <- bartlett.test(NormMean ~ Condition, 
                                       data = data_meanAM2_2)
  print(annot_meanAM2_2_var)
  print("Variance is equal between groups")
  
} else if(annot_meanAM2_con_2_normmean$p.value | 
          annot_meanAM2_asyn_2_normmean$p.value < 0.05){
  
  annot_meanAM2_2_var <- leveneTest(NormMean ~ Condition, 
                                    data = data_meanAM2_2)
  print(annot_meanAM2_2_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_2_meanAM2 <- wilcox.test(NormMean ~ Condition, data = data_meanAM2_2,
                                       paired = FALSE, alternative = "two.sided")

annot_Valmean_2_meanAM2 <- tidy(annot_Valmean_2_meanAM2)

##- 3 Timepoint 

data_meanAM2_asyn_3 <- data[data$Timepoint == 3 & 
                              data$Variable == "mean.AM.2" &
                              data$Condition == "asyn", ]

data_meanAM2_con_3 <- data[data$Timepoint == 3 & 
                             data$Variable == "mean.AM.2" &
                             data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanAM2_asyn_3, x = "NormMean")

ggdensity(data = data_meanAM2_asyn_3, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanAM2_asyn_3_normmean <- shapiro.test(x = data_meanAM2_asyn_3$NormMean)

if(annot_meanAM2_asyn_3_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanAM2_con_3, x = "NormMean")

ggdensity(data = data_meanAM2_con_3, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanAM2_con_3_normmean <- shapiro.test(x = data_meanAM2_con_3$NormMean)

if(annot_meanAM2_con_3_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanAM2_3 <- data[data$Timepoint == 3 & data$Variable == "mean.AM.2", ]

if(annot_meanAM2_con_3_normmean$p.value & 
   annot_meanAM2_asyn_3_normmean$p.value > 0.05){
  
  annot_meanAM2_3_var <- bartlett.test(NormMean ~ Condition, 
                                       data = data_meanAM2_3)
  print(annot_meanAM2_3_var)
  print("Variance is equal between groups")
  
} else if(annot_meanAM2_con_3_normmean$p.value | 
          annot_meanAM2_asyn_3_normmean$p.value < 0.05){
  
  annot_meanAM2_3_var <- leveneTest(NormMean ~ Condition, 
                                    data = data_meanAM2_3)
  print(annot_meanAM2_3_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_3_meanAM2 <- wilcox.test(NormMean ~ Condition, data = data_meanAM2_3,
                                       paired = FALSE, alternative = "two.sided")

annot_Valmean_3_meanAM2 <- tidy(annot_Valmean_3_meanAM2)

###- Create Dataframe of P-values for each comparison

annot_Valmean_Names_meanAM2 <- c("annot_Valmean_0_meanAM2","annot_Valmean_1_meanAM2",
                                 "annot_Valmean_2_meanAM2","annot_Valmean_3_meanAM2")

annot_Valmean_List_meanAM2 <- list(annot_Valmean_0_meanAM2,annot_Valmean_1_meanAM2,
                                   annot_Valmean_2_meanAM2,annot_Valmean_3_meanAM2)

names(annot_Valmean_List_meanAM2) <- annot_Valmean_Names_meanAM2

annot_Valmean_meanAM2 <- bind_rows(annot_Valmean_List_meanAM2,
                              .id = "Information")

### Wilcoxon Rank Sum Test on Normalized Mean Amplitude ###
### by timepoint and comparing conditions ###

##- 0 Timepoint 

data_meanamplitude_asyn_0 <- data[data$Timepoint == 0 & 
                              data$Variable == "mean.amplitude" &
                              data$Condition == "asyn", ]

data_meanamplitude_con_0 <- data[data$Timepoint == 0 & 
                             data$Variable == "mean.amplitude" &
                             data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanamplitude_asyn_0, x = "NormMean")

ggdensity(data = data_meanamplitude_asyn_0, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanamplitude_asyn_0_normmean <- shapiro.test(x = data_meanamplitude_asyn_0$NormMean)

if(annot_meanamplitude_asyn_0_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanamplitude_con_0, x = "NormMean")

ggdensity(data = data_meanamplitude_con_0, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanamplitude_con_0_normmean <- shapiro.test(x = data_meanamplitude_con_0$NormMean)

if(annot_meanamplitude_con_0_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanamplitude_0 <- data[data$Timepoint == 0 & data$Variable == "mean.amplitude", ]

if(annot_meanamplitude_con_0_normmean$p.value & 
   annot_meanamplitude_asyn_0_normmean$p.value > 0.05){
  
  annot_meanamplitude_0_var <- bartlett.test(NormMean ~ Condition, 
                                       data = data_meanamplitude_0)
  print(annot_meanamplitude_0_var)
  print("Variance is equal between groups")
  
} else if(annot_meanamplitude_con_0_normmean$p.value | 
          annot_meanamplitude_asyn_0_normmean$p.value < 0.05){
  
  annot_meanamplitude_0_var <- leveneTest(NormMean ~ Condition, 
                                    data = data_meanamplitude_0)
  print(annot_meanamplitude_0_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_0_meanamplitude <- wilcox.test(NormMean ~ Condition, data = data_meanamplitude_0,
                                       paired = FALSE, alternative = "two.sided")

annot_Valmean_0_meanamplitude <- tidy(annot_Valmean_0_meanamplitude)

##- 1 Timepoint 

data_meanamplitude_asyn_1 <- data[data$Timepoint == 1 & 
                              data$Variable == "mean.amplitude" &
                              data$Condition == "asyn", ]

data_meanamplitude_con_1 <- data[data$Timepoint == 1 & 
                             data$Variable == "mean.amplitude" &
                             data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanamplitude_asyn_1, x = "NormMean")

ggdensity(data = data_meanamplitude_asyn_1, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanamplitude_asyn_1_normmean <- shapiro.test(x = data_meanamplitude_asyn_1$NormMean)

if(annot_meanamplitude_asyn_1_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanamplitude_con_1, x = "NormMean")

ggdensity(data = data_meanamplitude_con_1, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanamplitude_con_1_normmean <- shapiro.test(x = data_meanamplitude_con_1$NormMean)

if(annot_meanamplitude_con_1_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanamplitude_1 <- data[data$Timepoint == 1 & data$Variable == "mean.amplitude", ]

if(annot_meanamplitude_con_1_normmean$p.value & 
   annot_meanamplitude_asyn_1_normmean$p.value > 0.05){
  
  annot_meanamplitude_1_var <- bartlett.test(NormMean ~ Condition, 
                                       data = data_meanamplitude_1)
  print(annot_meanamplitude_1_var)
  print("Variance is equal between groups")
  
} else if(annot_meanamplitude_con_1_normmean$p.value | 
          annot_meanamplitude_asyn_1_normmean$p.value < 0.05){
  
  annot_meanamplitude_1_var <- leveneTest(NormMean ~ Condition, 
                                    data = data_meanamplitude_1)
  print(annot_meanamplitude_1_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_1_meanamplitude <- wilcox.test(NormMean ~ Condition, data = data_meanamplitude_1,
                                       paired = FALSE, alternative = "two.sided")

annot_Valmean_1_meanamplitude <- tidy(annot_Valmean_1_meanamplitude)

##- 2 Timepoint 

data_meanamplitude_asyn_2 <- data[data$Timepoint == 2 & 
                              data$Variable == "mean.amplitude" &
                              data$Condition == "asyn", ]

data_meanamplitude_con_2 <- data[data$Timepoint == 2 & 
                             data$Variable == "mean.amplitude" &
                             data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanamplitude_asyn_2, x = "NormMean")

ggdensity(data = data_meanamplitude_asyn_2, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanamplitude_asyn_2_normmean <- shapiro.test(x = data_meanamplitude_asyn_2$NormMean)

if(annot_meanamplitude_asyn_2_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanamplitude_con_2, x = "NormMean")

ggdensity(data = data_meanamplitude_con_2, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanamplitude_con_2_normmean <- shapiro.test(x = data_meanamplitude_con_2$NormMean)

if(annot_meanamplitude_con_2_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanamplitude_2 <- data[data$Timepoint == 2 & data$Variable == "mean.amplitude", ]

if(annot_meanamplitude_con_2_normmean$p.value & 
   annot_meanamplitude_asyn_2_normmean$p.value > 0.05){
  
  annot_meanamplitude_2_var <- bartlett.test(NormMean ~ Condition, 
                                       data = data_meanamplitude_2)
  print(annot_meanamplitude_2_var)
  print("Variance is equal between groups")
  
} else if(annot_meanamplitude_con_2_normmean$p.value | 
          annot_meanamplitude_asyn_2_normmean$p.value < 0.05){
  
  annot_meanamplitude_2_var <- leveneTest(NormMean ~ Condition, 
                                    data = data_meanamplitude_2)
  print(annot_meanamplitude_2_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_2_meanamplitude <- wilcox.test(NormMean ~ Condition, data = data_meanamplitude_2,
                                       paired = FALSE, alternative = "two.sided")

annot_Valmean_2_meanamplitude <- tidy(annot_Valmean_2_meanamplitude)

##- 3 Timepoint 

data_meanamplitude_asyn_3 <- data[data$Timepoint == 3 & 
                              data$Variable == "mean.amplitude" &
                              data$Condition == "asyn", ]

data_meanamplitude_con_3 <- data[data$Timepoint == 3 & 
                             data$Variable == "mean.amplitude" &
                             data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanamplitude_asyn_3, x = "NormMean")

ggdensity(data = data_meanamplitude_asyn_3, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanamplitude_asyn_3_normmean <- shapiro.test(x = data_meanamplitude_asyn_3$NormMean)

if(annot_meanamplitude_asyn_3_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanamplitude_con_3, x = "NormMean")

ggdensity(data = data_meanamplitude_con_3, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanamplitude_con_3_normmean <- shapiro.test(x = data_meanamplitude_con_3$NormMean)

if(annot_meanamplitude_con_3_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanamplitude_3 <- data[data$Timepoint == 3 & data$Variable == "mean.amplitude", ]

if(annot_meanamplitude_con_3_normmean$p.value & 
   annot_meanamplitude_asyn_3_normmean$p.value > 0.05){
  
  annot_meanamplitude_3_var <- bartlett.test(NormMean ~ Condition, 
                                       data = data_meanamplitude_3)
  print(annot_meanamplitude_3_var)
  print("Variance is equal between groups")
  
} else if(annot_meanamplitude_con_3_normmean$p.value | 
          annot_meanamplitude_asyn_3_normmean$p.value < 0.05){
  
  annot_meanamplitude_3_var <- leveneTest(NormMean ~ Condition, 
                                    data = data_meanamplitude_3)
  print(annot_meanamplitude_3_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_3_meanamplitude <- wilcox.test(NormMean ~ Condition, data = data_meanamplitude_3,
                                       paired = FALSE, alternative = "two.sided")

annot_Valmean_3_meanamplitude <- tidy(annot_Valmean_3_meanamplitude)

###- Create Dataframe of P-values for each comparison

annot_Valmean_Names_meanamplitude <- c("annot_Valmean_0_meanamplitude","annot_Valmean_1_meanamplitude",
                                 "annot_Valmean_2_meanamplitude","annot_Valmean_3_meanamplitude")

annot_Valmean_List_meanamplitude <- list(annot_Valmean_0_meanamplitude,annot_Valmean_1_meanamplitude,
                                   annot_Valmean_2_meanamplitude,annot_Valmean_3_meanamplitude)

names(annot_Valmean_List_meanamplitude) <- annot_Valmean_Names_meanamplitude

annot_Valmean_meanamplitude <- bind_rows(annot_Valmean_List_meanamplitude,
                                   .id = "Information")

### Wilcoxon Rank Sum Test on Normalized Mean Entropy ###
### by timepoint and comparing conditions ###

##- 0 Timepoint 

data_meanentropy_asyn_0 <- data[data$Timepoint == 0 & 
                                    data$Variable == "mean.entropy" &
                                    data$Condition == "asyn", ]

data_meanentropy_con_0 <- data[data$Timepoint == 0 & 
                                   data$Variable == "mean.entropy" &
                                   data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanentropy_asyn_0, x = "NormMean")

ggdensity(data = data_meanentropy_asyn_0, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanentropy_asyn_0_normmean <- shapiro.test(x = data_meanentropy_asyn_0$NormMean)

if(annot_meanentropy_asyn_0_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanentropy_con_0, x = "NormMean")

ggdensity(data = data_meanentropy_con_0, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanentropy_con_0_normmean <- shapiro.test(x = data_meanentropy_con_0$NormMean)

if(annot_meanentropy_con_0_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanentropy_0 <- data[data$Timepoint == 0 & data$Variable == "mean.entropy", ]

if(annot_meanentropy_con_0_normmean$p.value & 
   annot_meanentropy_asyn_0_normmean$p.value > 0.05){
  
  annot_meanentropy_0_var <- bartlett.test(NormMean ~ Condition, 
                                             data = data_meanentropy_0)
  print(annot_meanentropy_0_var)
  print("Variance is equal between groups")
  
} else if(annot_meanentropy_con_0_normmean$p.value | 
          annot_meanentropy_asyn_0_normmean$p.value < 0.05){
  
  annot_meanentropy_0_var <- leveneTest(NormMean ~ Condition, 
                                          data = data_meanentropy_0)
  print(annot_meanentropy_0_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_0_meanentropy <- wilcox.test(NormMean ~ Condition, data = data_meanentropy_0,
                                             paired = FALSE, alternative = "two.sided")

annot_Valmean_0_meanentropy <- tidy(annot_Valmean_0_meanentropy)

##- 1 Timepoint 

data_meanentropy_asyn_1 <- data[data$Timepoint == 1 & 
                                    data$Variable == "mean.entropy" &
                                    data$Condition == "asyn", ]

data_meanentropy_con_1 <- data[data$Timepoint == 1 & 
                                   data$Variable == "mean.entropy" &
                                   data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanentropy_asyn_1, x = "NormMean")

ggdensity(data = data_meanentropy_asyn_1, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanentropy_asyn_1_normmean <- shapiro.test(x = data_meanentropy_asyn_1$NormMean)

if(annot_meanentropy_asyn_1_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanentropy_con_1, x = "NormMean")

ggdensity(data = data_meanentropy_con_1, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanentropy_con_1_normmean <- shapiro.test(x = data_meanentropy_con_1$NormMean)

if(annot_meanentropy_con_1_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanentropy_1 <- data[data$Timepoint == 1 & data$Variable == "mean.entropy", ]

if(annot_meanentropy_con_1_normmean$p.value & 
   annot_meanentropy_asyn_1_normmean$p.value > 0.05){
  
  annot_meanentropy_1_var <- bartlett.test(NormMean ~ Condition, 
                                             data = data_meanentropy_1)
  print(annot_meanentropy_1_var)
  print("Variance is equal between groups")
  
} else if(annot_meanentropy_con_1_normmean$p.value | 
          annot_meanentropy_asyn_1_normmean$p.value < 0.05){
  
  annot_meanentropy_1_var <- leveneTest(NormMean ~ Condition, 
                                          data = data_meanentropy_1)
  print(annot_meanentropy_1_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_1_meanentropy <- wilcox.test(NormMean ~ Condition, data = data_meanentropy_1,
                                             paired = FALSE, alternative = "two.sided")

annot_Valmean_1_meanentropy <- tidy(annot_Valmean_1_meanentropy)

##- 2 Timepoint 

data_meanentropy_asyn_2 <- data[data$Timepoint == 2 & 
                                    data$Variable == "mean.entropy" &
                                    data$Condition == "asyn", ]

data_meanentropy_con_2 <- data[data$Timepoint == 2 & 
                                   data$Variable == "mean.entropy" &
                                   data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanentropy_asyn_2, x = "NormMean")

ggdensity(data = data_meanentropy_asyn_2, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanentropy_asyn_2_normmean <- shapiro.test(x = data_meanentropy_asyn_2$NormMean)

if(annot_meanentropy_asyn_2_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanentropy_con_2, x = "NormMean")

ggdensity(data = data_meanentropy_con_2, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanentropy_con_2_normmean <- shapiro.test(x = data_meanentropy_con_2$NormMean)

if(annot_meanentropy_con_2_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanentropy_2 <- data[data$Timepoint == 2 & data$Variable == "mean.entropy", ]

if(annot_meanentropy_con_2_normmean$p.value & 
   annot_meanentropy_asyn_2_normmean$p.value > 0.05){
  
  annot_meanentropy_2_var <- bartlett.test(NormMean ~ Condition, 
                                             data = data_meanentropy_2)
  print(annot_meanentropy_2_var)
  print("Variance is equal between groups")
  
} else if(annot_meanentropy_con_2_normmean$p.value | 
          annot_meanentropy_asyn_2_normmean$p.value < 0.05){
  
  annot_meanentropy_2_var <- leveneTest(NormMean ~ Condition, 
                                          data = data_meanentropy_2)
  print(annot_meanentropy_2_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_2_meanentropy <- wilcox.test(NormMean ~ Condition, data = data_meanentropy_2,
                                             paired = FALSE, alternative = "two.sided")

annot_Valmean_2_meanentropy <- tidy(annot_Valmean_2_meanentropy)

##- 3 Timepoint 

data_meanentropy_asyn_3 <- data[data$Timepoint == 3 & 
                                    data$Variable == "mean.entropy" &
                                    data$Condition == "asyn", ]

data_meanentropy_con_3 <- data[data$Timepoint == 3 & 
                                   data$Variable == "mean.entropy" &
                                   data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanentropy_asyn_3, x = "NormMean")

ggdensity(data = data_meanentropy_asyn_3, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanentropy_asyn_3_normmean <- shapiro.test(x = data_meanentropy_asyn_3$NormMean)

if(annot_meanentropy_asyn_3_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanentropy_con_3, x = "NormMean")

ggdensity(data = data_meanentropy_con_3, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanentropy_con_3_normmean <- shapiro.test(x = data_meanentropy_con_3$NormMean)

if(annot_meanentropy_con_3_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanentropy_3 <- data[data$Timepoint == 3 & data$Variable == "mean.entropy", ]

if(annot_meanentropy_con_3_normmean$p.value & 
   annot_meanentropy_asyn_3_normmean$p.value > 0.05){
  
  annot_meanentropy_3_var <- bartlett.test(NormMean ~ Condition, 
                                             data = data_meanentropy_3)
  print(annot_meanentropy_3_var)
  print("Variance is equal between groups")
  
} else if(annot_meanentropy_con_3_normmean$p.value | 
          annot_meanentropy_asyn_3_normmean$p.value < 0.05){
  
  annot_meanentropy_3_var <- leveneTest(NormMean ~ Condition, 
                                          data = data_meanentropy_3)
  print(annot_meanentropy_3_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_3_meanentropy <- wilcox.test(NormMean ~ Condition, data = data_meanentropy_3,
                                             paired = FALSE, alternative = "two.sided")

annot_Valmean_3_meanentropy <- tidy(annot_Valmean_3_meanentropy)

###- Create Dataframe of P-values for each comparison

annot_Valmean_Names_meanentropy <- c("annot_Valmean_0_meanentropy","annot_Valmean_1_meanentropy",
                                       "annot_Valmean_2_meanentropy","annot_Valmean_3_meanentropy")

annot_Valmean_List_meanentropy <- list(annot_Valmean_0_meanentropy,annot_Valmean_1_meanentropy,
                                         annot_Valmean_2_meanentropy,annot_Valmean_3_meanentropy)

names(annot_Valmean_List_meanentropy) <- annot_Valmean_Names_meanentropy

annot_Valmean_meanentropy <- bind_rows(annot_Valmean_List_meanentropy,
                                         .id = "Information")

### Wilcoxon Rank Sum Test on Normalized Mean Frequency Modulation ###
### by timepoint and comparing conditions ###

##- 0 Timepoint 

data_meanFM_asyn_0 <- data[data$Timepoint == 0 & 
                                  data$Variable == "mean.FM" &
                                  data$Condition == "asyn", ]

data_meanFM_con_0 <- data[data$Timepoint == 0 & 
                                 data$Variable == "mean.FM" &
                                 data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanFM_asyn_0, x = "NormMean")

ggdensity(data = data_meanFM_asyn_0, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanFM_asyn_0_normmean <- shapiro.test(x = data_meanFM_asyn_0$NormMean)

if(annot_meanFM_asyn_0_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanFM_con_0, x = "NormMean")

ggdensity(data = data_meanFM_con_0, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanFM_con_0_normmean <- shapiro.test(x = data_meanFM_con_0$NormMean)

if(annot_meanFM_con_0_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanFM_0 <- data[data$Timepoint == 0 & data$Variable == "mean.FM", ]

if(annot_meanFM_con_0_normmean$p.value & 
   annot_meanFM_asyn_0_normmean$p.value > 0.05){
  
  annot_meanFM_0_var <- bartlett.test(NormMean ~ Condition, 
                                           data = data_meanFM_0)
  print(annot_meanFM_0_var)
  print("Variance is equal between groups")
  
} else if(annot_meanFM_con_0_normmean$p.value | 
          annot_meanFM_asyn_0_normmean$p.value < 0.05){
  
  annot_meanFM_0_var <- leveneTest(NormMean ~ Condition, 
                                        data = data_meanFM_0)
  print(annot_meanFM_0_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_0_meanFM <- wilcox.test(NormMean ~ Condition, data = data_meanFM_0,
                                           paired = FALSE, alternative = "two.sided")

annot_Valmean_0_meanFM <- tidy(annot_Valmean_0_meanFM)

##- 1 Timepoint 

data_meanFM_asyn_1 <- data[data$Timepoint == 1 & 
                                  data$Variable == "mean.FM" &
                                  data$Condition == "asyn", ]

data_meanFM_con_1 <- data[data$Timepoint == 1 & 
                                 data$Variable == "mean.FM" &
                                 data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanFM_asyn_1, x = "NormMean")

ggdensity(data = data_meanFM_asyn_1, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanFM_asyn_1_normmean <- shapiro.test(x = data_meanFM_asyn_1$NormMean)

if(annot_meanFM_asyn_1_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanFM_con_1, x = "NormMean")

ggdensity(data = data_meanFM_con_1, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanFM_con_1_normmean <- shapiro.test(x = data_meanFM_con_1$NormMean)

if(annot_meanFM_con_1_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanFM_1 <- data[data$Timepoint == 1 & data$Variable == "mean.FM", ]

if(annot_meanFM_con_1_normmean$p.value & 
   annot_meanFM_asyn_1_normmean$p.value > 0.05){
  
  annot_meanFM_1_var <- bartlett.test(NormMean ~ Condition, 
                                           data = data_meanFM_1)
  print(annot_meanFM_1_var)
  print("Variance is equal between groups")
  
} else if(annot_meanFM_con_1_normmean$p.value | 
          annot_meanFM_asyn_1_normmean$p.value < 0.05){
  
  annot_meanFM_1_var <- leveneTest(NormMean ~ Condition, 
                                        data = data_meanFM_1)
  print(annot_meanFM_1_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_1_meanFM <- wilcox.test(NormMean ~ Condition, data = data_meanFM_1,
                                           paired = FALSE, alternative = "two.sided")

annot_Valmean_1_meanFM <- tidy(annot_Valmean_1_meanFM)

##- 2 Timepoint 

data_meanFM_asyn_2 <- data[data$Timepoint == 2 & 
                                  data$Variable == "mean.FM" &
                                  data$Condition == "asyn", ]

data_meanFM_con_2 <- data[data$Timepoint == 2 & 
                                 data$Variable == "mean.FM" &
                                 data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanFM_asyn_2, x = "NormMean")

ggdensity(data = data_meanFM_asyn_2, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanFM_asyn_2_normmean <- shapiro.test(x = data_meanFM_asyn_2$NormMean)

if(annot_meanFM_asyn_2_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanFM_con_2, x = "NormMean")

ggdensity(data = data_meanFM_con_2, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanFM_con_2_normmean <- shapiro.test(x = data_meanFM_con_2$NormMean)

if(annot_meanFM_con_2_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanFM_2 <- data[data$Timepoint == 2 & data$Variable == "mean.FM", ]

if(annot_meanFM_con_2_normmean$p.value & 
   annot_meanFM_asyn_2_normmean$p.value > 0.05){
  
  annot_meanFM_2_var <- bartlett.test(NormMean ~ Condition, 
                                           data = data_meanFM_2)
  print(annot_meanFM_2_var)
  print("Variance is equal between groups")
  
} else if(annot_meanFM_con_2_normmean$p.value | 
          annot_meanFM_asyn_2_normmean$p.value < 0.05){
  
  annot_meanFM_2_var <- leveneTest(NormMean ~ Condition, 
                                        data = data_meanFM_2)
  print(annot_meanFM_2_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_2_meanFM <- wilcox.test(NormMean ~ Condition, data = data_meanFM_2,
                                           paired = FALSE, alternative = "two.sided")

annot_Valmean_2_meanFM <- tidy(annot_Valmean_2_meanFM)

##- 3 Timepoint 

data_meanFM_asyn_3 <- data[data$Timepoint == 3 & 
                                  data$Variable == "mean.FM" &
                                  data$Condition == "asyn", ]

data_meanFM_con_3 <- data[data$Timepoint == 3 & 
                                 data$Variable == "mean.FM" &
                                 data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanFM_asyn_3, x = "NormMean")

ggdensity(data = data_meanFM_asyn_3, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanFM_asyn_3_normmean <- shapiro.test(x = data_meanFM_asyn_3$NormMean)

if(annot_meanFM_asyn_3_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanFM_con_3, x = "NormMean")

ggdensity(data = data_meanFM_con_3, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanFM_con_3_normmean <- shapiro.test(x = data_meanFM_con_3$NormMean)

if(annot_meanFM_con_3_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanFM_3 <- data[data$Timepoint == 3 & data$Variable == "mean.FM", ]

if(annot_meanFM_con_3_normmean$p.value & 
   annot_meanFM_asyn_3_normmean$p.value > 0.05){
  
  annot_meanFM_3_var <- bartlett.test(NormMean ~ Condition, 
                                           data = data_meanFM_3)
  print(annot_meanFM_3_var)
  print("Variance is equal between groups")
  
} else if(annot_meanFM_con_3_normmean$p.value | 
          annot_meanFM_asyn_3_normmean$p.value < 0.05){
  
  annot_meanFM_3_var <- leveneTest(NormMean ~ Condition, 
                                        data = data_meanFM_3)
  print(annot_meanFM_3_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_3_meanFM <- wilcox.test(NormMean ~ Condition, data = data_meanFM_3,
                                           paired = FALSE, alternative = "two.sided")

annot_Valmean_3_meanFM <- tidy(annot_Valmean_3_meanFM)

###- Create Dataframe of P-values for each comparison

annot_Valmean_Names_meanFM <- c("annot_Valmean_0_meanFM","annot_Valmean_1_meanFM",
                                     "annot_Valmean_2_meanFM","annot_Valmean_3_meanFM")

annot_Valmean_List_meanFM <- list(annot_Valmean_0_meanFM,annot_Valmean_1_meanFM,
                                       annot_Valmean_2_meanFM,annot_Valmean_3_meanFM)

names(annot_Valmean_List_meanFM) <- annot_Valmean_Names_meanFM

annot_Valmean_meanFM <- bind_rows(annot_Valmean_List_meanFM,
                                       .id = "Information")

### Wilcoxon Rank Sum Test on Normalized Mean Mean Frequency ###
### by timepoint and comparing conditions ###

##- 0 Timepoint 

data_meanmeanfreq_asyn_0 <- data[data$Timepoint == 0 & 
                             data$Variable == "mean.mean.freq" &
                             data$Condition == "asyn", ]

data_meanmeanfreq_con_0 <- data[data$Timepoint == 0 & 
                            data$Variable == "mean.mean.freq" &
                            data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanmeanfreq_asyn_0, x = "NormMean")

ggdensity(data = data_meanmeanfreq_asyn_0, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanmeanfreq_asyn_0_normmean <- shapiro.test(x = data_meanmeanfreq_asyn_0$NormMean)

if(annot_meanmeanfreq_asyn_0_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanmeanfreq_con_0, x = "NormMean")

ggdensity(data = data_meanmeanfreq_con_0, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanmeanfreq_con_0_normmean <- shapiro.test(x = data_meanmeanfreq_con_0$NormMean)

if(annot_meanmeanfreq_con_0_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanmeanfreq_0 <- data[data$Timepoint == 0 & data$Variable == "mean.mean.freq", ]

if(annot_meanmeanfreq_con_0_normmean$p.value & 
   annot_meanmeanfreq_asyn_0_normmean$p.value > 0.05){
  
  annot_meanmeanfreq_0_var <- bartlett.test(NormMean ~ Condition, 
                                      data = data_meanmeanfreq_0)
  print(annot_meanmeanfreq_0_var)
  print("Variance is equal between groups")
  
} else if(annot_meanmeanfreq_con_0_normmean$p.value | 
          annot_meanmeanfreq_asyn_0_normmean$p.value < 0.05){
  
  annot_meanmeanfreq_0_var <- leveneTest(NormMean ~ Condition, 
                                   data = data_meanmeanfreq_0)
  print(annot_meanmeanfreq_0_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_0_meanmeanfreq <- wilcox.test(NormMean ~ Condition, data = data_meanmeanfreq_0,
                                      paired = FALSE, alternative = "two.sided")

annot_Valmean_0_meanmeanfreq <- tidy(annot_Valmean_0_meanmeanfreq)

##- 1 Timepoint 

data_meanmeanfreq_asyn_1 <- data[data$Timepoint == 1 & 
                             data$Variable == "mean.mean.freq" &
                             data$Condition == "asyn", ]

data_meanmeanfreq_con_1 <- data[data$Timepoint == 1 & 
                            data$Variable == "mean.mean.freq" &
                            data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanmeanfreq_asyn_1, x = "NormMean")

ggdensity(data = data_meanmeanfreq_asyn_1, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanmeanfreq_asyn_1_normmean <- shapiro.test(x = data_meanmeanfreq_asyn_1$NormMean)

if(annot_meanmeanfreq_asyn_1_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanmeanfreq_con_1, x = "NormMean")

ggdensity(data = data_meanmeanfreq_con_1, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanmeanfreq_con_1_normmean <- shapiro.test(x = data_meanmeanfreq_con_1$NormMean)

if(annot_meanmeanfreq_con_1_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanmeanfreq_1 <- data[data$Timepoint == 1 & data$Variable == "mean.mean.freq", ]

if(annot_meanmeanfreq_con_1_normmean$p.value & 
   annot_meanmeanfreq_asyn_1_normmean$p.value > 0.05){
  
  annot_meanmeanfreq_1_var <- bartlett.test(NormMean ~ Condition, 
                                      data = data_meanmeanfreq_1)
  print(annot_meanmeanfreq_1_var)
  print("Variance is equal between groups")
  
} else if(annot_meanmeanfreq_con_1_normmean$p.value | 
          annot_meanmeanfreq_asyn_1_normmean$p.value < 0.05){
  
  annot_meanmeanfreq_1_var <- leveneTest(NormMean ~ Condition, 
                                   data = data_meanmeanfreq_1)
  print(annot_meanmeanfreq_1_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_1_meanmeanfreq <- wilcox.test(NormMean ~ Condition, data = data_meanmeanfreq_1,
                                      paired = FALSE, alternative = "two.sided")

annot_Valmean_1_meanmeanfreq <- tidy(annot_Valmean_1_meanmeanfreq)

##- 2 Timepoint 

data_meanmeanfreq_asyn_2 <- data[data$Timepoint == 2 & 
                             data$Variable == "mean.mean.freq" &
                             data$Condition == "asyn", ]

data_meanmeanfreq_con_2 <- data[data$Timepoint == 2 & 
                            data$Variable == "mean.mean.freq" &
                            data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanmeanfreq_asyn_2, x = "NormMean")

ggdensity(data = data_meanmeanfreq_asyn_2, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanmeanfreq_asyn_2_normmean <- shapiro.test(x = data_meanmeanfreq_asyn_2$NormMean)

if(annot_meanmeanfreq_asyn_2_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanmeanfreq_con_2, x = "NormMean")

ggdensity(data = data_meanmeanfreq_con_2, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanmeanfreq_con_2_normmean <- shapiro.test(x = data_meanmeanfreq_con_2$NormMean)

if(annot_meanmeanfreq_con_2_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanmeanfreq_2 <- data[data$Timepoint == 2 & data$Variable == "mean.mean.freq", ]

if(annot_meanmeanfreq_con_2_normmean$p.value & 
   annot_meanmeanfreq_asyn_2_normmean$p.value > 0.05){
  
  annot_meanmeanfreq_2_var <- bartlett.test(NormMean ~ Condition, 
                                      data = data_meanmeanfreq_2)
  print(annot_meanmeanfreq_2_var)
  print("Variance is equal between groups")
  
} else if(annot_meanmeanfreq_con_2_normmean$p.value | 
          annot_meanmeanfreq_asyn_2_normmean$p.value < 0.05){
  
  annot_meanmeanfreq_2_var <- leveneTest(NormMean ~ Condition, 
                                   data = data_meanmeanfreq_2)
  print(annot_meanmeanfreq_2_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_2_meanmeanfreq <- wilcox.test(NormMean ~ Condition, data = data_meanmeanfreq_2,
                                      paired = FALSE, alternative = "two.sided")

annot_Valmean_2_meanmeanfreq <- tidy(annot_Valmean_2_meanmeanfreq)

##- 3 Timepoint 

data_meanmeanfreq_asyn_3 <- data[data$Timepoint == 3 & 
                             data$Variable == "mean.mean.freq" &
                             data$Condition == "asyn", ]

data_meanmeanfreq_con_3 <- data[data$Timepoint == 3 & 
                            data$Variable == "mean.mean.freq" &
                            data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanmeanfreq_asyn_3, x = "NormMean")

ggdensity(data = data_meanmeanfreq_asyn_3, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanmeanfreq_asyn_3_normmean <- shapiro.test(x = data_meanmeanfreq_asyn_3$NormMean)

if(annot_meanmeanfreq_asyn_3_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanmeanfreq_con_3, x = "NormMean")

ggdensity(data = data_meanmeanfreq_con_3, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanmeanfreq_con_3_normmean <- shapiro.test(x = data_meanmeanfreq_con_3$NormMean)

if(annot_meanmeanfreq_con_3_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanmeanfreq_3 <- data[data$Timepoint == 3 & data$Variable == "mean.mean.freq", ]

if(annot_meanmeanfreq_con_3_normmean$p.value & 
   annot_meanmeanfreq_asyn_3_normmean$p.value > 0.05){
  
  annot_meanmeanfreq_3_var <- bartlett.test(NormMean ~ Condition, 
                                      data = data_meanmeanfreq_3)
  print(annot_meanmeanfreq_3_var)
  print("Variance is equal between groups")
  
} else if(annot_meanmeanfreq_con_3_normmean$p.value | 
          annot_meanmeanfreq_asyn_3_normmean$p.value < 0.05){
  
  annot_meanmeanfreq_3_var <- leveneTest(NormMean ~ Condition, 
                                   data = data_meanmeanfreq_3)
  print(annot_meanmeanfreq_3_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_3_meanmeanfreq <- wilcox.test(NormMean ~ Condition, data = data_meanmeanfreq_3,
                                      paired = FALSE, alternative = "two.sided")

annot_Valmean_3_meanmeanfreq <- tidy(annot_Valmean_3_meanmeanfreq)

###- Create Dataframe of P-values for each comparison

annot_Valmean_Names_meanmeanfreq <- c("annot_Valmean_0_meanmeanfreq","annot_Valmean_1_meanmeanfreq",
                                "annot_Valmean_2_meanmeanfreq","annot_Valmean_3_meanmeanfreq")

annot_Valmean_List_meanmeanfreq <- list(annot_Valmean_0_meanmeanfreq,annot_Valmean_1_meanmeanfreq,
                                  annot_Valmean_2_meanmeanfreq,annot_Valmean_3_meanmeanfreq)

names(annot_Valmean_List_meanmeanfreq) <- annot_Valmean_Names_meanmeanfreq

annot_Valmean_meanmeanfreq <- bind_rows(annot_Valmean_List_meanmeanfreq,
                                  .id = "Information")

### Wilcoxon Rank Sum Test on Normalized Mean Pitch ###
### by timepoint and comparing conditions ###

##- 0 Timepoint 

data_meanpitch_asyn_0 <- data[data$Timepoint == 0 & 
                                   data$Variable == "mean.pitch" &
                                   data$Condition == "asyn", ]

data_meanpitch_con_0 <- data[data$Timepoint == 0 & 
                                  data$Variable == "mean.pitch" &
                                  data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanpitch_asyn_0, x = "NormMean")

ggdensity(data = data_meanpitch_asyn_0, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitch_asyn_0_normmean <- shapiro.test(x = data_meanpitch_asyn_0$NormMean)

if(annot_meanpitch_asyn_0_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanpitch_con_0, x = "NormMean")

ggdensity(data = data_meanpitch_con_0, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitch_con_0_normmean <- shapiro.test(x = data_meanpitch_con_0$NormMean)

if(annot_meanpitch_con_0_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanpitch_0 <- data[data$Timepoint == 0 & data$Variable == "mean.pitch", ]

if(annot_meanpitch_con_0_normmean$p.value & 
   annot_meanpitch_asyn_0_normmean$p.value > 0.05){
  
  annot_meanpitch_0_var <- bartlett.test(NormMean ~ Condition, 
                                            data = data_meanpitch_0)
  print(annot_meanpitch_0_var)
  print("Variance is equal between groups")
  
} else if(annot_meanpitch_con_0_normmean$p.value | 
          annot_meanpitch_asyn_0_normmean$p.value < 0.05){
  
  annot_meanpitch_0_var <- leveneTest(NormMean ~ Condition, 
                                         data = data_meanpitch_0)
  print(annot_meanpitch_0_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_0_meanpitch <- wilcox.test(NormMean ~ Condition, data = data_meanpitch_0,
                                            paired = FALSE, alternative = "two.sided")

annot_Valmean_0_meanpitch <- tidy(annot_Valmean_0_meanpitch)

##- 1 Timepoint 

data_meanpitch_asyn_1 <- data[data$Timepoint == 1 & 
                                   data$Variable == "mean.pitch" &
                                   data$Condition == "asyn", ]

data_meanpitch_con_1 <- data[data$Timepoint == 1 & 
                                  data$Variable == "mean.pitch" &
                                  data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanpitch_asyn_1, x = "NormMean")

ggdensity(data = data_meanpitch_asyn_1, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitch_asyn_1_normmean <- shapiro.test(x = data_meanpitch_asyn_1$NormMean)

if(annot_meanpitch_asyn_1_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanpitch_con_1, x = "NormMean")

ggdensity(data = data_meanpitch_con_1, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitch_con_1_normmean <- shapiro.test(x = data_meanpitch_con_1$NormMean)

if(annot_meanpitch_con_1_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanpitch_1 <- data[data$Timepoint == 1 & data$Variable == "mean.pitch", ]

if(annot_meanpitch_con_1_normmean$p.value & 
   annot_meanpitch_asyn_1_normmean$p.value > 0.05){
  
  annot_meanpitch_1_var <- bartlett.test(NormMean ~ Condition, 
                                            data = data_meanpitch_1)
  print(annot_meanpitch_1_var)
  print("Variance is equal between groups")
  
} else if(annot_meanpitch_con_1_normmean$p.value | 
          annot_meanpitch_asyn_1_normmean$p.value < 0.05){
  
  annot_meanpitch_1_var <- leveneTest(NormMean ~ Condition, 
                                         data = data_meanpitch_1)
  print(annot_meanpitch_1_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_1_meanpitch <- wilcox.test(NormMean ~ Condition, data = data_meanpitch_1,
                                            paired = FALSE, alternative = "two.sided")

annot_Valmean_1_meanpitch <- tidy(annot_Valmean_1_meanpitch)

##- 2 Timepoint 

data_meanpitch_asyn_2 <- data[data$Timepoint == 2 & 
                                   data$Variable == "mean.pitch" &
                                   data$Condition == "asyn", ]

data_meanpitch_con_2 <- data[data$Timepoint == 2 & 
                                  data$Variable == "mean.pitch" &
                                  data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanpitch_asyn_2, x = "NormMean")

ggdensity(data = data_meanpitch_asyn_2, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitch_asyn_2_normmean <- shapiro.test(x = data_meanpitch_asyn_2$NormMean)

if(annot_meanpitch_asyn_2_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanpitch_con_2, x = "NormMean")

ggdensity(data = data_meanpitch_con_2, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitch_con_2_normmean <- shapiro.test(x = data_meanpitch_con_2$NormMean)

if(annot_meanpitch_con_2_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanpitch_2 <- data[data$Timepoint == 2 & data$Variable == "mean.pitch", ]

if(annot_meanpitch_con_2_normmean$p.value & 
   annot_meanpitch_asyn_2_normmean$p.value > 0.05){
  
  annot_meanpitch_2_var <- bartlett.test(NormMean ~ Condition, 
                                            data = data_meanpitch_2)
  print(annot_meanpitch_2_var)
  print("Variance is equal between groups")
  
} else if(annot_meanpitch_con_2_normmean$p.value | 
          annot_meanpitch_asyn_2_normmean$p.value < 0.05){
  
  annot_meanpitch_2_var <- leveneTest(NormMean ~ Condition, 
                                         data = data_meanpitch_2)
  print(annot_meanpitch_2_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_2_meanpitch <- wilcox.test(NormMean ~ Condition, data = data_meanpitch_2,
                                            paired = FALSE, alternative = "two.sided")

annot_Valmean_2_meanpitch <- tidy(annot_Valmean_2_meanpitch)

##- 3 Timepoint 

data_meanpitch_asyn_3 <- data[data$Timepoint == 3 & 
                                   data$Variable == "mean.pitch" &
                                   data$Condition == "asyn", ]

data_meanpitch_con_3 <- data[data$Timepoint == 3 & 
                                  data$Variable == "mean.pitch" &
                                  data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanpitch_asyn_3, x = "NormMean")

ggdensity(data = data_meanpitch_asyn_3, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitch_asyn_3_normmean <- shapiro.test(x = data_meanpitch_asyn_3$NormMean)

if(annot_meanpitch_asyn_3_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanpitch_con_3, x = "NormMean")

ggdensity(data = data_meanpitch_con_3, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitch_con_3_normmean <- shapiro.test(x = data_meanpitch_con_3$NormMean)

if(annot_meanpitch_con_3_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanpitch_3 <- data[data$Timepoint == 3 & data$Variable == "mean.pitch", ]

if(annot_meanpitch_con_3_normmean$p.value & 
   annot_meanpitch_asyn_3_normmean$p.value > 0.05){
  
  annot_meanpitch_3_var <- bartlett.test(NormMean ~ Condition, 
                                            data = data_meanpitch_3)
  print(annot_meanpitch_3_var)
  print("Variance is equal between groups")
  
} else if(annot_meanpitch_con_3_normmean$p.value | 
          annot_meanpitch_asyn_3_normmean$p.value < 0.05){
  
  annot_meanpitch_3_var <- leveneTest(NormMean ~ Condition, 
                                         data = data_meanpitch_3)
  print(annot_meanpitch_3_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_3_meanpitch <- wilcox.test(NormMean ~ Condition, data = data_meanpitch_3,
                                            paired = FALSE, alternative = "two.sided")

annot_Valmean_3_meanpitch <- tidy(annot_Valmean_3_meanpitch)

###- Create Dataframe of P-values for each comparison

annot_Valmean_Names_meanpitch <- c("annot_Valmean_0_meanpitch","annot_Valmean_1_meanpitch",
                                      "annot_Valmean_2_meanpitch","annot_Valmean_3_meanpitch")

annot_Valmean_List_meanpitch <- list(annot_Valmean_0_meanpitch,annot_Valmean_1_meanpitch,
                                        annot_Valmean_2_meanpitch,annot_Valmean_3_meanpitch)

names(annot_Valmean_List_meanpitch) <- annot_Valmean_Names_meanpitch

annot_Valmean_meanpitch <- bind_rows(annot_Valmean_List_meanpitch,
                                        .id = "Information")

### Wilcoxon Rank Sum Test on Normalized Mean Pitch Goodness ###
### by timepoint and comparing conditions ###

##- 0 Timepoint 

data_meanpitchgoodness_asyn_0 <- data[data$Timepoint == 0 & 
                                data$Variable == "mean.pitch.goodness" &
                                data$Condition == "asyn", ]

data_meanpitchgoodness_con_0 <- data[data$Timepoint == 0 & 
                               data$Variable == "mean.pitch.goodness" &
                               data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanpitchgoodness_asyn_0, x = "NormMean")

ggdensity(data = data_meanpitchgoodness_asyn_0, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitchgoodness_asyn_0_normmean <- shapiro.test(x = data_meanpitchgoodness_asyn_0$NormMean)

if(annot_meanpitchgoodness_asyn_0_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanpitchgoodness_con_0, x = "NormMean")

ggdensity(data = data_meanpitchgoodness_con_0, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitchgoodness_con_0_normmean <- shapiro.test(x = data_meanpitchgoodness_con_0$NormMean)

if(annot_meanpitchgoodness_con_0_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanpitchgoodness_0 <- data[data$Timepoint == 0 & data$Variable == "mean.pitch.goodness", ]

if(annot_meanpitchgoodness_con_0_normmean$p.value & 
   annot_meanpitchgoodness_asyn_0_normmean$p.value > 0.05){
  
  annot_meanpitchgoodness_0_var <- bartlett.test(NormMean ~ Condition, 
                                         data = data_meanpitchgoodness_0)
  print(annot_meanpitchgoodness_0_var)
  print("Variance is equal between groups")
  
} else if(annot_meanpitchgoodness_con_0_normmean$p.value | 
          annot_meanpitchgoodness_asyn_0_normmean$p.value < 0.05){
  
  annot_meanpitchgoodness_0_var <- leveneTest(NormMean ~ Condition, 
                                      data = data_meanpitchgoodness_0)
  print(annot_meanpitchgoodness_0_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_0_meanpitchgoodness <- wilcox.test(NormMean ~ Condition, data = data_meanpitchgoodness_0,
                                         paired = FALSE, alternative = "two.sided")

annot_Valmean_0_meanpitchgoodness <- tidy(annot_Valmean_0_meanpitchgoodness)

##- 1 Timepoint 

data_meanpitchgoodness_asyn_1 <- data[data$Timepoint == 1 & 
                                data$Variable == "mean.pitch.goodness" &
                                data$Condition == "asyn", ]

data_meanpitchgoodness_con_1 <- data[data$Timepoint == 1 & 
                               data$Variable == "mean.pitch.goodness" &
                               data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanpitchgoodness_asyn_1, x = "NormMean")

ggdensity(data = data_meanpitchgoodness_asyn_1, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitchgoodness_asyn_1_normmean <- shapiro.test(x = data_meanpitchgoodness_asyn_1$NormMean)

if(annot_meanpitchgoodness_asyn_1_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanpitchgoodness_con_1, x = "NormMean")

ggdensity(data = data_meanpitchgoodness_con_1, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitchgoodness_con_1_normmean <- shapiro.test(x = data_meanpitchgoodness_con_1$NormMean)

if(annot_meanpitchgoodness_con_1_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanpitchgoodness_1 <- data[data$Timepoint == 1 & data$Variable == "mean.pitch.goodness", ]

if(annot_meanpitchgoodness_con_1_normmean$p.value & 
   annot_meanpitchgoodness_asyn_1_normmean$p.value > 0.05){
  
  annot_meanpitchgoodness_1_var <- bartlett.test(NormMean ~ Condition, 
                                         data = data_meanpitchgoodness_1)
  print(annot_meanpitchgoodness_1_var)
  print("Variance is equal between groups")
  
} else if(annot_meanpitchgoodness_con_1_normmean$p.value | 
          annot_meanpitchgoodness_asyn_1_normmean$p.value < 0.05){
  
  annot_meanpitchgoodness_1_var <- leveneTest(NormMean ~ Condition, 
                                      data = data_meanpitchgoodness_1)
  print(annot_meanpitchgoodness_1_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_1_meanpitchgoodness <- wilcox.test(NormMean ~ Condition, data = data_meanpitchgoodness_1,
                                         paired = FALSE, alternative = "two.sided")

annot_Valmean_1_meanpitchgoodness <- tidy(annot_Valmean_1_meanpitchgoodness)

##- 2 Timepoint 

data_meanpitchgoodness_asyn_2 <- data[data$Timepoint == 2 & 
                                data$Variable == "mean.pitch.goodness" &
                                data$Condition == "asyn", ]

data_meanpitchgoodness_con_2 <- data[data$Timepoint == 2 & 
                               data$Variable == "mean.pitch.goodness" &
                               data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanpitchgoodness_asyn_2, x = "NormMean")

ggdensity(data = data_meanpitchgoodness_asyn_2, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitchgoodness_asyn_2_normmean <- shapiro.test(x = data_meanpitchgoodness_asyn_2$NormMean)

if(annot_meanpitchgoodness_asyn_2_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanpitchgoodness_con_2, x = "NormMean")

ggdensity(data = data_meanpitchgoodness_con_2, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitchgoodness_con_2_normmean <- shapiro.test(x = data_meanpitchgoodness_con_2$NormMean)

if(annot_meanpitchgoodness_con_2_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanpitchgoodness_2 <- data[data$Timepoint == 2 & data$Variable == "mean.pitch.goodness", ]

if(annot_meanpitchgoodness_con_2_normmean$p.value & 
   annot_meanpitchgoodness_asyn_2_normmean$p.value > 0.05){
  
  annot_meanpitchgoodness_2_var <- bartlett.test(NormMean ~ Condition, 
                                         data = data_meanpitchgoodness_2)
  print(annot_meanpitchgoodness_2_var)
  print("Variance is equal between groups")
  
} else if(annot_meanpitchgoodness_con_2_normmean$p.value | 
          annot_meanpitchgoodness_asyn_2_normmean$p.value < 0.05){
  
  annot_meanpitchgoodness_2_var <- leveneTest(NormMean ~ Condition, 
                                      data = data_meanpitchgoodness_2)
  print(annot_meanpitchgoodness_2_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_2_meanpitchgoodness <- wilcox.test(NormMean ~ Condition, data = data_meanpitchgoodness_2,
                                         paired = FALSE, alternative = "two.sided")

annot_Valmean_2_meanpitchgoodness <- tidy(annot_Valmean_2_meanpitchgoodness)

##- 3 Timepoint 

data_meanpitchgoodness_asyn_3 <- data[data$Timepoint == 3 & 
                                data$Variable == "mean.pitch.goodness" &
                                data$Condition == "asyn", ]

data_meanpitchgoodness_con_3 <- data[data$Timepoint == 3 & 
                               data$Variable == "mean.pitch.goodness" &
                               data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanpitchgoodness_asyn_3, x = "NormMean")

ggdensity(data = data_meanpitchgoodness_asyn_3, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitchgoodness_asyn_3_normmean <- shapiro.test(x = data_meanpitchgoodness_asyn_3$NormMean)

if(annot_meanpitchgoodness_asyn_3_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanpitchgoodness_con_3, x = "NormMean")

ggdensity(data = data_meanpitchgoodness_con_3, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitchgoodness_con_3_normmean <- shapiro.test(x = data_meanpitchgoodness_con_3$NormMean)

if(annot_meanpitchgoodness_con_3_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanpitchgoodness_3 <- data[data$Timepoint == 3 & data$Variable == "mean.pitch.goodness", ]

if(annot_meanpitchgoodness_con_3_normmean$p.value & 
   annot_meanpitchgoodness_asyn_3_normmean$p.value > 0.05){
  
  annot_meanpitchgoodness_3_var <- bartlett.test(NormMean ~ Condition, 
                                         data = data_meanpitchgoodness_3)
  print(annot_meanpitchgoodness_3_var)
  print("Variance is equal between groups")
  
} else if(annot_meanpitchgoodness_con_3_normmean$p.value | 
          annot_meanpitchgoodness_asyn_3_normmean$p.value < 0.05){
  
  annot_meanpitchgoodness_3_var <- leveneTest(NormMean ~ Condition, 
                                      data = data_meanpitchgoodness_3)
  print(annot_meanpitchgoodness_3_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_3_meanpitchgoodness <- wilcox.test(NormMean ~ Condition, data = data_meanpitchgoodness_3,
                                         paired = FALSE, alternative = "two.sided")

annot_Valmean_3_meanpitchgoodness <- tidy(annot_Valmean_3_meanpitchgoodness)

###- Create Dataframe of P-values for each comparison

annot_Valmean_Names_meanpitchgoodness <- c("annot_Valmean_0_meanpitchgoodness","annot_Valmean_1_meanpitchgoodness",
                                   "annot_Valmean_2_meanpitchgoodness","annot_Valmean_3_meanpitchgoodness")

annot_Valmean_List_meanpitchgoodness <- list(annot_Valmean_0_meanpitchgoodness,annot_Valmean_1_meanpitchgoodness,
                                     annot_Valmean_2_meanpitchgoodness,annot_Valmean_3_meanpitchgoodness)

names(annot_Valmean_List_meanpitchgoodness) <- annot_Valmean_Names_meanpitchgoodness

annot_Valmean_meanpitchgoodness <- bind_rows(annot_Valmean_List_meanpitchgoodness,
                                     .id = "Information")

### Wilcoxon Rank Sum Test on Normalized Syllable Duration ###
### by timepoint and comparing conditions ###

##- 0 Timepoint 

data_syllableduration_asyn_0 <- data[data$Timepoint == 0 & 
                                        data$Variable == "syllable.duration" &
                                        data$Condition == "asyn", ]

data_syllableduration_con_0 <- data[data$Timepoint == 0 & 
                                       data$Variable == "syllable.duration" &
                                       data$Condition == "con", ]

#- asyn

ggqqplot(data = data_syllableduration_asyn_0, x = "NormMean")

ggdensity(data = data_syllableduration_asyn_0, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_syllableduration_asyn_0_normmean <- shapiro.test(x = data_syllableduration_asyn_0$NormMean)

if(annot_syllableduration_asyn_0_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_syllableduration_con_0, x = "NormMean")

ggdensity(data = data_syllableduration_con_0, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_syllableduration_con_0_normmean <- shapiro.test(x = data_syllableduration_con_0$NormMean)

if(annot_syllableduration_con_0_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_syllableduration_0 <- data[data$Timepoint == 0 & data$Variable == "syllable.duration", ]

if(annot_syllableduration_con_0_normmean$p.value & 
   annot_syllableduration_asyn_0_normmean$p.value > 0.05){
  
  annot_syllableduration_0_var <- bartlett.test(NormMean ~ Condition, 
                                                 data = data_syllableduration_0)
  print(annot_syllableduration_0_var)
  print("Variance is equal between groups")
  
} else if(annot_syllableduration_con_0_normmean$p.value | 
          annot_syllableduration_asyn_0_normmean$p.value < 0.05){
  
  annot_syllableduration_0_var <- leveneTest(NormMean ~ Condition, 
                                              data = data_syllableduration_0)
  print(annot_syllableduration_0_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_0_syllableduration <- wilcox.test(NormMean ~ Condition, data = data_syllableduration_0,
                                                 paired = FALSE, alternative = "two.sided")

annot_Valmean_0_syllableduration <- tidy(annot_Valmean_0_syllableduration)

##- 1 Timepoint 

data_syllableduration_asyn_1 <- data[data$Timepoint == 1 & 
                                        data$Variable == "syllable.duration" &
                                        data$Condition == "asyn", ]

data_syllableduration_con_1 <- data[data$Timepoint == 1 & 
                                       data$Variable == "syllable.duration" &
                                       data$Condition == "con", ]

#- asyn

ggqqplot(data = data_syllableduration_asyn_1, x = "NormMean")

ggdensity(data = data_syllableduration_asyn_1, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_syllableduration_asyn_1_normmean <- shapiro.test(x = data_syllableduration_asyn_1$NormMean)

if(annot_syllableduration_asyn_1_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_syllableduration_con_1, x = "NormMean")

ggdensity(data = data_syllableduration_con_1, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_syllableduration_con_1_normmean <- shapiro.test(x = data_syllableduration_con_1$NormMean)

if(annot_syllableduration_con_1_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_syllableduration_1 <- data[data$Timepoint == 1 & data$Variable == "syllable.duration", ]

if(annot_syllableduration_con_1_normmean$p.value & 
   annot_syllableduration_asyn_1_normmean$p.value > 0.05){
  
  annot_syllableduration_1_var <- bartlett.test(NormMean ~ Condition, 
                                                 data = data_syllableduration_1)
  print(annot_syllableduration_1_var)
  print("Variance is equal between groups")
  
} else if(annot_syllableduration_con_1_normmean$p.value | 
          annot_syllableduration_asyn_1_normmean$p.value < 0.05){
  
  annot_syllableduration_1_var <- leveneTest(NormMean ~ Condition, 
                                              data = data_syllableduration_1)
  print(annot_syllableduration_1_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_1_syllableduration <- wilcox.test(NormMean ~ Condition, data = data_syllableduration_1,
                                                 paired = FALSE, alternative = "two.sided")

annot_Valmean_1_syllableduration <- tidy(annot_Valmean_1_syllableduration)

##- 2 Timepoint 

data_syllableduration_asyn_2 <- data[data$Timepoint == 2 & 
                                        data$Variable == "syllable.duration" &
                                        data$Condition == "asyn", ]

data_syllableduration_con_2 <- data[data$Timepoint == 2 & 
                                       data$Variable == "syllable.duration" &
                                       data$Condition == "con", ]

#- asyn

ggqqplot(data = data_syllableduration_asyn_2, x = "NormMean")

ggdensity(data = data_syllableduration_asyn_2, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_syllableduration_asyn_2_normmean <- shapiro.test(x = data_syllableduration_asyn_2$NormMean)

if(annot_syllableduration_asyn_2_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_syllableduration_con_2, x = "NormMean")

ggdensity(data = data_syllableduration_con_2, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_syllableduration_con_2_normmean <- shapiro.test(x = data_syllableduration_con_2$NormMean)

if(annot_syllableduration_con_2_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_syllableduration_2 <- data[data$Timepoint == 2 & data$Variable == "syllable.duration", ]

if(annot_syllableduration_con_2_normmean$p.value & 
   annot_syllableduration_asyn_2_normmean$p.value > 0.05){
  
  annot_syllableduration_2_var <- bartlett.test(NormMean ~ Condition, 
                                                 data = data_syllableduration_2)
  print(annot_syllableduration_2_var)
  print("Variance is equal between groups")
  
} else if(annot_syllableduration_con_2_normmean$p.value | 
          annot_syllableduration_asyn_2_normmean$p.value < 0.05){
  
  annot_syllableduration_2_var <- leveneTest(NormMean ~ Condition, 
                                              data = data_syllableduration_2)
  print(annot_syllableduration_2_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_2_syllableduration <- wilcox.test(NormMean ~ Condition, data = data_syllableduration_2,
                                                 paired = FALSE, alternative = "two.sided")

annot_Valmean_2_syllableduration <- tidy(annot_Valmean_2_syllableduration)

##- 3 Timepoint 

data_syllableduration_asyn_3 <- data[data$Timepoint == 3 & 
                                        data$Variable == "syllable.duration" &
                                        data$Condition == "asyn", ]

data_syllableduration_con_3 <- data[data$Timepoint == 3 & 
                                       data$Variable == "syllable.duration" &
                                       data$Condition == "con", ]

#- asyn

ggqqplot(data = data_syllableduration_asyn_3, x = "NormMean")

ggdensity(data = data_syllableduration_asyn_3, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_syllableduration_asyn_3_normmean <- shapiro.test(x = data_syllableduration_asyn_3$NormMean)

if(annot_syllableduration_asyn_3_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_syllableduration_con_3, x = "NormMean")

ggdensity(data = data_syllableduration_con_3, x = "NormMean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_syllableduration_con_3_normmean <- shapiro.test(x = data_syllableduration_con_3$NormMean)

if(annot_syllableduration_con_3_normmean$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_syllableduration_3 <- data[data$Timepoint == 3 & data$Variable == "syllable.duration", ]

if(annot_syllableduration_con_3_normmean$p.value & 
   annot_syllableduration_asyn_3_normmean$p.value > 0.05){
  
  annot_syllableduration_3_var <- bartlett.test(NormMean ~ Condition, 
                                                 data = data_syllableduration_3)
  print(annot_syllableduration_3_var)
  print("Variance is equal between groups")
  
} else if(annot_syllableduration_con_3_normmean$p.value | 
          annot_syllableduration_asyn_3_normmean$p.value < 0.05){
  
  annot_syllableduration_3_var <- leveneTest(NormMean ~ Condition, 
                                              data = data_syllableduration_3)
  print(annot_syllableduration_3_var)
  print("Variance is not equal between groups")
  
}

annot_Valmean_3_syllableduration <- wilcox.test(NormMean ~ Condition, data = data_syllableduration_3,
                                                 paired = FALSE, alternative = "two.sided")

annot_Valmean_3_syllableduration <- tidy(annot_Valmean_3_syllableduration)

###- Create Dataframe of P-values for each comparison

annot_Valmean_Names_syllableduration <- c("annot_Valmean_0_syllableduration","annot_Valmean_1_syllableduration",
                                           "annot_Valmean_2_syllableduration","annot_Valmean_3_syllableduration")

annot_Valmean_List_syllableduration <- list(annot_Valmean_0_syllableduration,annot_Valmean_1_syllableduration,
                                             annot_Valmean_2_syllableduration,annot_Valmean_3_syllableduration)

names(annot_Valmean_List_syllableduration) <- annot_Valmean_Names_syllableduration

annot_Valmean_syllableduration <- bind_rows(annot_Valmean_List_syllableduration,
                                             .id = "Information")

####- Create Dataframe of P-values for 
####- mean acoustic feature comparisons

annot_Valmean <- bind_rows(annot_Valmean_meanAM2,annot_Valmean_meanamplitude,
                          annot_Valmean_meanentropy, annot_Valmean_meanFM,
                          annot_Valmean_meanmeanfreq, annot_Valmean_meanpitch,
                          annot_Valmean_meanpitchgoodness)

#### CV Scores ####

### Wilcoxon Rank Sum Test on Normalized Mean AM 2 ###
### by timepoint and comparing conditions ###

##- 0 Timepoint 

data_meanAM2_asyn_0 <- data[data$Timepoint == 0 & 
                              data$Variable == "mean.AM.2" &
                              data$Condition == "asyn", ]

data_meanAM2_con_0 <- data[data$Timepoint == 0 & 
                             data$Variable == "mean.AM.2" &
                             data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanAM2_asyn_0, x = "NormCV")

ggdensity(data = data_meanAM2_asyn_0, x = "NormCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanAM2_asyn_0_normCV <- shapiro.test(x = data_meanAM2_asyn_0$valuesCV)

if(annot_meanAM2_asyn_0_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanAM2_con_0, x = "valuesCV")

ggdensity(data = data_meanAM2_con_0, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanAM2_con_0_normCV <- shapiro.test(x = data_meanAM2_con_0$valuesCV)

if(annot_meanAM2_con_0_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanAM2_0 <- data[data$Timepoint == 0 & data$Variable == "mean.AM.2", ]

if(annot_meanAM2_con_0_normCV$p.value & 
   annot_meanAM2_asyn_0_normCV$p.value > 0.05){
  
  annot_meanAM2_0_var <- bartlett.test(valuesCV ~ Condition, 
                                       data = data_meanAM2_0)
  print(annot_meanAM2_0_var)
  print("Variance is equal between groups")
  
} else if(annot_meanAM2_con_0_normCV$p.value | 
          annot_meanAM2_asyn_0_normCV$p.value < 0.05){
  
  annot_meanAM2_0_var <- leveneTest(valuesCV ~ Condition, 
                                    data = data_meanAM2_0)
  print(annot_meanAM2_0_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_0_meanAM2 <- wilcox.test(valuesCV ~ Condition, data = data_meanAM2_0,
                                       paired = FALSE, alternative = "two.sided")

annot_ValCV_0_meanAM2 <- tidy(annot_ValCV_0_meanAM2)

##- 1 Timepoint 

data_meanAM2_asyn_1 <- data[data$Timepoint == 1 & 
                              data$Variable == "mean.AM.2" &
                              data$Condition == "asyn", ]

data_meanAM2_con_1 <- data[data$Timepoint == 1 & 
                             data$Variable == "mean.AM.2" &
                             data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanAM2_asyn_1, x = "valuesCV")

ggdensity(data = data_meanAM2_asyn_1, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanAM2_asyn_1_normCV <- shapiro.test(x = data_meanAM2_asyn_1$valuesCV)

if(annot_meanAM2_asyn_1_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanAM2_con_1, x = "valuesCV")

ggdensity(data = data_meanAM2_con_1, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanAM2_con_1_normCV <- shapiro.test(x = data_meanAM2_con_1$valuesCV)

if(annot_meanAM2_con_1_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanAM2_1 <- data[data$Timepoint == 1 & data$Variable == "mean.AM.2", ]

if(annot_meanAM2_con_1_normCV$p.value & 
   annot_meanAM2_asyn_1_normCV$p.value > 0.05){
  
  annot_meanAM2_1_var <- bartlett.test(valuesCV ~ Condition, 
                                       data = data_meanAM2_1)
  print(annot_meanAM2_1_var)
  print("Variance is equal between groups")
  
} else if(annot_meanAM2_con_1_normCV$p.value | 
          annot_meanAM2_asyn_1_normCV$p.value < 0.05){
  
  annot_meanAM2_1_var <- leveneTest(valuesCV ~ Condition, 
                                    data = data_meanAM2_1)
  print(annot_meanAM2_1_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_1_meanAM2 <- wilcox.test(valuesCV ~ Condition, data = data_meanAM2_1,
                                       paired = FALSE, alternative = "two.sided")

annot_ValCV_1_meanAM2 <- tidy(annot_ValCV_1_meanAM2)

##- 2 Timepoint 

data_meanAM2_asyn_2 <- data[data$Timepoint == 2 & 
                              data$Variable == "mean.AM.2" &
                              data$Condition == "asyn", ]

data_meanAM2_con_2 <- data[data$Timepoint == 2 & 
                             data$Variable == "mean.AM.2" &
                             data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanAM2_asyn_2, x = "valuesCV")

ggdensity(data = data_meanAM2_asyn_2, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanAM2_asyn_2_normCV <- shapiro.test(x = data_meanAM2_asyn_2$valuesCV)

if(annot_meanAM2_asyn_2_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanAM2_con_2, x = "valuesCV")

ggdensity(data = data_meanAM2_con_2, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanAM2_con_2_normCV <- shapiro.test(x = data_meanAM2_con_2$valuesCV)

if(annot_meanAM2_con_2_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanAM2_2 <- data[data$Timepoint == 2 & data$Variable == "mean.AM.2", ]

if(annot_meanAM2_con_2_normCV$p.value & 
   annot_meanAM2_asyn_2_normCV$p.value > 0.05){
  
  annot_meanAM2_2_var <- bartlett.test(valuesCV ~ Condition, 
                                       data = data_meanAM2_2)
  print(annot_meanAM2_2_var)
  print("Variance is equal between groups")
  
} else if(annot_meanAM2_con_2_normCV$p.value | 
          annot_meanAM2_asyn_2_normCV$p.value < 0.05){
  
  annot_meanAM2_2_var <- leveneTest(valuesCV ~ Condition, 
                                    data = data_meanAM2_2)
  print(annot_meanAM2_2_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_2_meanAM2 <- wilcox.test(valuesCV ~ Condition, data = data_meanAM2_2,
                                       paired = FALSE, alternative = "two.sided")

annot_ValCV_2_meanAM2 <- tidy(annot_ValCV_2_meanAM2)

##- 3 Timepoint 

data_meanAM2_asyn_3 <- data[data$Timepoint == 3 & 
                              data$Variable == "mean.AM.2" &
                              data$Condition == "asyn", ]

data_meanAM2_con_3 <- data[data$Timepoint == 3 & 
                             data$Variable == "mean.AM.2" &
                             data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanAM2_asyn_3, x = "valuesCV")

ggdensity(data = data_meanAM2_asyn_3, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanAM2_asyn_3_normCV <- shapiro.test(x = data_meanAM2_asyn_3$valuesCV)

if(annot_meanAM2_asyn_3_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanAM2_con_3, x = "valuesCV")

ggdensity(data = data_meanAM2_con_3, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanAM2_con_3_normCV <- shapiro.test(x = data_meanAM2_con_3$valuesCV)

if(annot_meanAM2_con_3_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanAM2_3 <- data[data$Timepoint == 3 & data$Variable == "mean.AM.2", ]

if(annot_meanAM2_con_3_normCV$p.value & 
   annot_meanAM2_asyn_3_normCV$p.value > 0.05){
  
  annot_meanAM2_3_var <- bartlett.test(valuesCV ~ Condition, 
                                       data = data_meanAM2_3)
  print(annot_meanAM2_3_var)
  print("Variance is equal between groups")
  
} else if(annot_meanAM2_con_3_normCV$p.value | 
          annot_meanAM2_asyn_3_normCV$p.value < 0.05){
  
  annot_meanAM2_3_var <- leveneTest(valuesCV ~ Condition, 
                                    data = data_meanAM2_3)
  print(annot_meanAM2_3_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_3_meanAM2 <- wilcox.test(valuesCV ~ Condition, data = data_meanAM2_3,
                                       paired = FALSE, alternative = "two.sided")

annot_ValCV_3_meanAM2 <- tidy(annot_ValCV_3_meanAM2)

###- Create Dataframe of P-values for each comparison

annot_ValCV_Names_meanAM2 <- c("annot_ValCV_0_meanAM2","annot_ValCV_1_meanAM2",
                                 "annot_ValCV_2_meanAM2","annot_ValCV_3_meanAM2")

annot_ValCV_List_meanAM2 <- list(annot_ValCV_0_meanAM2,annot_ValCV_1_meanAM2,
                                   annot_ValCV_2_meanAM2,annot_ValCV_3_meanAM2)

names(annot_ValCV_List_meanAM2) <- annot_ValCV_Names_meanAM2

annot_ValCV_meanAM2 <- bind_rows(annot_ValCV_List_meanAM2,
                                   .id = "Information")

### Wilcoxon Rank Sum Test on Normalized Mean Amplitude ###
### by timepoint and comparing conditions ###

##- 0 Timepoint 

data_meanamplitude_asyn_0 <- data[data$Timepoint == 0 & 
                                    data$Variable == "mean.amplitude" &
                                    data$Condition == "asyn", ]

data_meanamplitude_con_0 <- data[data$Timepoint == 0 & 
                                   data$Variable == "mean.amplitude" &
                                   data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanamplitude_asyn_0, x = "valuesCV")

ggdensity(data = data_meanamplitude_asyn_0, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanamplitude_asyn_0_normCV <- shapiro.test(x = data_meanamplitude_asyn_0$valuesCV)

if(annot_meanamplitude_asyn_0_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanamplitude_con_0, x = "valuesCV")

ggdensity(data = data_meanamplitude_con_0, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanamplitude_con_0_normCV <- shapiro.test(x = data_meanamplitude_con_0$valuesCV)

if(annot_meanamplitude_con_0_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanamplitude_0 <- data[data$Timepoint == 0 & data$Variable == "mean.amplitude", ]

if(annot_meanamplitude_con_0_normCV$p.value & 
   annot_meanamplitude_asyn_0_normCV$p.value > 0.05){
  
  annot_meanamplitude_0_var <- bartlett.test(valuesCV ~ Condition, 
                                             data = data_meanamplitude_0)
  print(annot_meanamplitude_0_var)
  print("Variance is equal between groups")
  
} else if(annot_meanamplitude_con_0_normCV$p.value | 
          annot_meanamplitude_asyn_0_normCV$p.value < 0.05){
  
  annot_meanamplitude_0_var <- leveneTest(valuesCV ~ Condition, 
                                          data = data_meanamplitude_0)
  print(annot_meanamplitude_0_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_0_meanamplitude <- wilcox.test(valuesCV ~ Condition, data = data_meanamplitude_0,
                                             paired = FALSE, alternative = "two.sided")

annot_ValCV_0_meanamplitude <- tidy(annot_ValCV_0_meanamplitude)

##- 1 Timepoint 

data_meanamplitude_asyn_1 <- data[data$Timepoint == 1 & 
                                    data$Variable == "mean.amplitude" &
                                    data$Condition == "asyn", ]

data_meanamplitude_con_1 <- data[data$Timepoint == 1 & 
                                   data$Variable == "mean.amplitude" &
                                   data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanamplitude_asyn_1, x = "valuesCV")

ggdensity(data = data_meanamplitude_asyn_1, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanamplitude_asyn_1_normCV <- shapiro.test(x = data_meanamplitude_asyn_1$valuesCV)

if(annot_meanamplitude_asyn_1_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanamplitude_con_1, x = "valuesCV")

ggdensity(data = data_meanamplitude_con_1, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanamplitude_con_1_normCV <- shapiro.test(x = data_meanamplitude_con_1$valuesCV)

if(annot_meanamplitude_con_1_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanamplitude_1 <- data[data$Timepoint == 1 & data$Variable == "mean.amplitude", ]

if(annot_meanamplitude_con_1_normCV$p.value & 
   annot_meanamplitude_asyn_1_normCV$p.value > 0.05){
  
  annot_meanamplitude_1_var <- bartlett.test(valuesCV ~ Condition, 
                                             data = data_meanamplitude_1)
  print(annot_meanamplitude_1_var)
  print("Variance is equal between groups")
  
} else if(annot_meanamplitude_con_1_normCV$p.value | 
          annot_meanamplitude_asyn_1_normCV$p.value < 0.05){
  
  annot_meanamplitude_1_var <- leveneTest(valuesCV ~ Condition, 
                                          data = data_meanamplitude_1)
  print(annot_meanamplitude_1_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_1_meanamplitude <- wilcox.test(valuesCV ~ Condition, data = data_meanamplitude_1,
                                             paired = FALSE, alternative = "two.sided")

annot_ValCV_1_meanamplitude <- tidy(annot_ValCV_1_meanamplitude)

##- 2 Timepoint 

data_meanamplitude_asyn_2 <- data[data$Timepoint == 2 & 
                                    data$Variable == "mean.amplitude" &
                                    data$Condition == "asyn", ]

data_meanamplitude_con_2 <- data[data$Timepoint == 2 & 
                                   data$Variable == "mean.amplitude" &
                                   data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanamplitude_asyn_2, x = "valuesCV")

ggdensity(data = data_meanamplitude_asyn_2, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanamplitude_asyn_2_normCV <- shapiro.test(x = data_meanamplitude_asyn_2$valuesCV)

if(annot_meanamplitude_asyn_2_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanamplitude_con_2, x = "valuesCV")

ggdensity(data = data_meanamplitude_con_2, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanamplitude_con_2_normCV <- shapiro.test(x = data_meanamplitude_con_2$valuesCV)

if(annot_meanamplitude_con_2_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanamplitude_2 <- data[data$Timepoint == 2 & data$Variable == "mean.amplitude", ]

if(annot_meanamplitude_con_2_normCV$p.value & 
   annot_meanamplitude_asyn_2_normCV$p.value > 0.05){
  
  annot_meanamplitude_2_var <- bartlett.test(valuesCV ~ Condition, 
                                             data = data_meanamplitude_2)
  print(annot_meanamplitude_2_var)
  print("Variance is equal between groups")
  
} else if(annot_meanamplitude_con_2_normCV$p.value | 
          annot_meanamplitude_asyn_2_normCV$p.value < 0.05){
  
  annot_meanamplitude_2_var <- leveneTest(valuesCV ~ Condition, 
                                          data = data_meanamplitude_2)
  print(annot_meanamplitude_2_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_2_meanamplitude <- wilcox.test(valuesCV ~ Condition, data = data_meanamplitude_2,
                                             paired = FALSE, alternative = "two.sided")

annot_ValCV_2_meanamplitude <- tidy(annot_ValCV_2_meanamplitude)

##- 3 Timepoint 

data_meanamplitude_asyn_3 <- data[data$Timepoint == 3 & 
                                    data$Variable == "mean.amplitude" &
                                    data$Condition == "asyn", ]

data_meanamplitude_con_3 <- data[data$Timepoint == 3 & 
                                   data$Variable == "mean.amplitude" &
                                   data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanamplitude_asyn_3, x = "valuesCV")

ggdensity(data = data_meanamplitude_asyn_3, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanamplitude_asyn_3_normCV <- shapiro.test(x = data_meanamplitude_asyn_3$valuesCV)

if(annot_meanamplitude_asyn_3_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanamplitude_con_3, x = "valuesCV")

ggdensity(data = data_meanamplitude_con_3, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanamplitude_con_3_normCV <- shapiro.test(x = data_meanamplitude_con_3$valuesCV)

if(annot_meanamplitude_con_3_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanamplitude_3 <- data[data$Timepoint == 3 & data$Variable == "mean.amplitude", ]

if(annot_meanamplitude_con_3_normCV$p.value & 
   annot_meanamplitude_asyn_3_normCV$p.value > 0.05){
  
  annot_meanamplitude_3_var <- bartlett.test(valuesCV ~ Condition, 
                                             data = data_meanamplitude_3)
  print(annot_meanamplitude_3_var)
  print("Variance is equal between groups")
  
} else if(annot_meanamplitude_con_3_normCV$p.value | 
          annot_meanamplitude_asyn_3_normCV$p.value < 0.05){
  
  annot_meanamplitude_3_var <- leveneTest(valuesCV ~ Condition, 
                                          data = data_meanamplitude_3)
  print(annot_meanamplitude_3_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_3_meanamplitude <- wilcox.test(valuesCV ~ Condition, data = data_meanamplitude_3,
                                             paired = FALSE, alternative = "two.sided")

annot_ValCV_3_meanamplitude <- tidy(annot_ValCV_3_meanamplitude)

###- Create Dataframe of P-values for each comparison

annot_ValCV_Names_meanamplitude <- c("annot_ValCV_0_meanamplitude","annot_ValCV_1_meanamplitude",
                                       "annot_ValCV_2_meanamplitude","annot_ValCV_3_meanamplitude")

annot_ValCV_List_meanamplitude <- list(annot_ValCV_0_meanamplitude,annot_ValCV_1_meanamplitude,
                                         annot_ValCV_2_meanamplitude,annot_ValCV_3_meanamplitude)

names(annot_ValCV_List_meanamplitude) <- annot_ValCV_Names_meanamplitude

annot_ValCV_meanamplitude <- bind_rows(annot_ValCV_List_meanamplitude,
                                         .id = "Information")

### Wilcoxon Rank Sum Test on Normalized Mean Entropy ###
### by timepoint and comparing conditions ###

##- 0 Timepoint 

data_meanentropy_asyn_0 <- data[data$Timepoint == 0 & 
                                  data$Variable == "mean.entropy" &
                                  data$Condition == "asyn", ]

data_meanentropy_con_0 <- data[data$Timepoint == 0 & 
                                 data$Variable == "mean.entropy" &
                                 data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanentropy_asyn_0, x = "valuesCV")

ggdensity(data = data_meanentropy_asyn_0, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanentropy_asyn_0_normCV <- shapiro.test(x = data_meanentropy_asyn_0$valuesCV)

if(annot_meanentropy_asyn_0_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanentropy_con_0, x = "valuesCV")

ggdensity(data = data_meanentropy_con_0, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanentropy_con_0_normCV <- shapiro.test(x = data_meanentropy_con_0$valuesCV)

if(annot_meanentropy_con_0_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanentropy_0 <- data[data$Timepoint == 0 & data$Variable == "mean.entropy", ]

if(annot_meanentropy_con_0_normCV$p.value & 
   annot_meanentropy_asyn_0_normCV$p.value > 0.05){
  
  annot_meanentropy_0_var <- bartlett.test(valuesCV ~ Condition, 
                                           data = data_meanentropy_0)
  print(annot_meanentropy_0_var)
  print("Variance is equal between groups")
  
} else if(annot_meanentropy_con_0_normCV$p.value | 
          annot_meanentropy_asyn_0_normCV$p.value < 0.05){
  
  annot_meanentropy_0_var <- leveneTest(valuesCV ~ Condition, 
                                        data = data_meanentropy_0)
  print(annot_meanentropy_0_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_0_meanentropy <- wilcox.test(valuesCV ~ Condition, data = data_meanentropy_0,
                                           paired = FALSE, alternative = "two.sided")

annot_ValCV_0_meanentropy <- tidy(annot_ValCV_0_meanentropy)

##- 1 Timepoint 

data_meanentropy_asyn_1 <- data[data$Timepoint == 1 & 
                                  data$Variable == "mean.entropy" &
                                  data$Condition == "asyn", ]

data_meanentropy_con_1 <- data[data$Timepoint == 1 & 
                                 data$Variable == "mean.entropy" &
                                 data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanentropy_asyn_1, x = "valuesCV")

ggdensity(data = data_meanentropy_asyn_1, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanentropy_asyn_1_normCV <- shapiro.test(x = data_meanentropy_asyn_1$valuesCV)

if(annot_meanentropy_asyn_1_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanentropy_con_1, x = "valuesCV")

ggdensity(data = data_meanentropy_con_1, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanentropy_con_1_normCV <- shapiro.test(x = data_meanentropy_con_1$valuesCV)

if(annot_meanentropy_con_1_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanentropy_1 <- data[data$Timepoint == 1 & data$Variable == "mean.entropy", ]

if(annot_meanentropy_con_1_normCV$p.value & 
   annot_meanentropy_asyn_1_normCV$p.value > 0.05){
  
  annot_meanentropy_1_var <- bartlett.test(valuesCV ~ Condition, 
                                           data = data_meanentropy_1)
  print(annot_meanentropy_1_var)
  print("Variance is equal between groups")
  
} else if(annot_meanentropy_con_1_normCV$p.value | 
          annot_meanentropy_asyn_1_normCV$p.value < 0.05){
  
  annot_meanentropy_1_var <- leveneTest(valuesCV ~ Condition, 
                                        data = data_meanentropy_1)
  print(annot_meanentropy_1_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_1_meanentropy <- wilcox.test(valuesCV ~ Condition, data = data_meanentropy_1,
                                           paired = FALSE, alternative = "two.sided")

annot_ValCV_1_meanentropy <- tidy(annot_ValCV_1_meanentropy)

##- 2 Timepoint 

data_meanentropy_asyn_2 <- data[data$Timepoint == 2 & 
                                  data$Variable == "mean.entropy" &
                                  data$Condition == "asyn", ]

data_meanentropy_con_2 <- data[data$Timepoint == 2 & 
                                 data$Variable == "mean.entropy" &
                                 data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanentropy_asyn_2, x = "valuesCV")

ggdensity(data = data_meanentropy_asyn_2, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanentropy_asyn_2_normCV <- shapiro.test(x = data_meanentropy_asyn_2$valuesCV)

if(annot_meanentropy_asyn_2_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanentropy_con_2, x = "valuesCV")

ggdensity(data = data_meanentropy_con_2, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanentropy_con_2_normCV <- shapiro.test(x = data_meanentropy_con_2$valuesCV)

if(annot_meanentropy_con_2_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanentropy_2 <- data[data$Timepoint == 2 & data$Variable == "mean.entropy", ]

if(annot_meanentropy_con_2_normCV$p.value & 
   annot_meanentropy_asyn_2_normCV$p.value > 0.05){
  
  annot_meanentropy_2_var <- bartlett.test(valuesCV ~ Condition, 
                                           data = data_meanentropy_2)
  print(annot_meanentropy_2_var)
  print("Variance is equal between groups")
  
} else if(annot_meanentropy_con_2_normCV$p.value | 
          annot_meanentropy_asyn_2_normCV$p.value < 0.05){
  
  annot_meanentropy_2_var <- leveneTest(valuesCV ~ Condition, 
                                        data = data_meanentropy_2)
  print(annot_meanentropy_2_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_2_meanentropy <- wilcox.test(valuesCV ~ Condition, data = data_meanentropy_2,
                                           paired = FALSE, alternative = "two.sided")

annot_ValCV_2_meanentropy <- tidy(annot_ValCV_2_meanentropy)

##- 3 Timepoint 

data_meanentropy_asyn_3 <- data[data$Timepoint == 3 & 
                                  data$Variable == "mean.entropy" &
                                  data$Condition == "asyn", ]

data_meanentropy_con_3 <- data[data$Timepoint == 3 & 
                                 data$Variable == "mean.entropy" &
                                 data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanentropy_asyn_3, x = "valuesCV")

ggdensity(data = data_meanentropy_asyn_3, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanentropy_asyn_3_normCV <- shapiro.test(x = data_meanentropy_asyn_3$valuesCV)

if(annot_meanentropy_asyn_3_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanentropy_con_3, x = "valuesCV")

ggdensity(data = data_meanentropy_con_3, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanentropy_con_3_normCV <- shapiro.test(x = data_meanentropy_con_3$valuesCV)

if(annot_meanentropy_con_3_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanentropy_3 <- data[data$Timepoint == 3 & data$Variable == "mean.entropy", ]

if(annot_meanentropy_con_3_normCV$p.value & 
   annot_meanentropy_asyn_3_normCV$p.value > 0.05){
  
  annot_meanentropy_3_var <- bartlett.test(valuesCV ~ Condition, 
                                           data = data_meanentropy_3)
  print(annot_meanentropy_3_var)
  print("Variance is equal between groups")
  
} else if(annot_meanentropy_con_3_normCV$p.value | 
          annot_meanentropy_asyn_3_normCV$p.value < 0.05){
  
  annot_meanentropy_3_var <- leveneTest(valuesCV ~ Condition, 
                                        data = data_meanentropy_3)
  print(annot_meanentropy_3_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_3_meanentropy <- wilcox.test(valuesCV ~ Condition, data = data_meanentropy_3,
                                           paired = FALSE, alternative = "two.sided")

annot_ValCV_3_meanentropy <- tidy(annot_ValCV_3_meanentropy)

###- Create Dataframe of P-values for each comparison

annot_ValCV_Names_meanentropy <- c("annot_ValCV_0_meanentropy","annot_ValCV_1_meanentropy",
                                     "annot_ValCV_2_meanentropy","annot_ValCV_3_meanentropy")

annot_ValCV_List_meanentropy <- list(annot_ValCV_0_meanentropy,annot_ValCV_1_meanentropy,
                                       annot_ValCV_2_meanentropy,annot_ValCV_3_meanentropy)

names(annot_ValCV_List_meanentropy) <- annot_ValCV_Names_meanentropy

annot_ValCV_meanentropy <- bind_rows(annot_ValCV_List_meanentropy,
                                       .id = "Information")

### Wilcoxon Rank Sum Test on Normalized Mean Frequency Modulation ###
### by timepoint and comparing conditions ###

##- 0 Timepoint 

data_meanFM_asyn_0 <- data[data$Timepoint == 0 & 
                             data$Variable == "mean.FM" &
                             data$Condition == "asyn", ]

data_meanFM_con_0 <- data[data$Timepoint == 0 & 
                            data$Variable == "mean.FM" &
                            data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanFM_asyn_0, x = "valuesCV")

ggdensity(data = data_meanFM_asyn_0, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanFM_asyn_0_normCV <- shapiro.test(x = data_meanFM_asyn_0$valuesCV)

if(annot_meanFM_asyn_0_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanFM_con_0, x = "valuesCV")

ggdensity(data = data_meanFM_con_0, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanFM_con_0_normCV <- shapiro.test(x = data_meanFM_con_0$valuesCV)

if(annot_meanFM_con_0_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanFM_0 <- data[data$Timepoint == 0 & data$Variable == "mean.FM", ]

if(annot_meanFM_con_0_normCV$p.value & 
   annot_meanFM_asyn_0_normCV$p.value > 0.05){
  
  annot_meanFM_0_var <- bartlett.test(valuesCV ~ Condition, 
                                      data = data_meanFM_0)
  print(annot_meanFM_0_var)
  print("Variance is equal between groups")
  
} else if(annot_meanFM_con_0_normCV$p.value | 
          annot_meanFM_asyn_0_normCV$p.value < 0.05){
  
  annot_meanFM_0_var <- leveneTest(valuesCV ~ Condition, 
                                   data = data_meanFM_0)
  print(annot_meanFM_0_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_0_meanFM <- wilcox.test(valuesCV ~ Condition, data = data_meanFM_0,
                                      paired = FALSE, alternative = "two.sided")

annot_ValCV_0_meanFM <- tidy(annot_ValCV_0_meanFM)

##- 1 Timepoint 

data_meanFM_asyn_1 <- data[data$Timepoint == 1 & 
                             data$Variable == "mean.FM" &
                             data$Condition == "asyn", ]

data_meanFM_con_1 <- data[data$Timepoint == 1 & 
                            data$Variable == "mean.FM" &
                            data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanFM_asyn_1, x = "valuesCV")

ggdensity(data = data_meanFM_asyn_1, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanFM_asyn_1_normCV <- shapiro.test(x = data_meanFM_asyn_1$valuesCV)

if(annot_meanFM_asyn_1_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanFM_con_1, x = "valuesCV")

ggdensity(data = data_meanFM_con_1, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanFM_con_1_normCV <- shapiro.test(x = data_meanFM_con_1$valuesCV)

if(annot_meanFM_con_1_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanFM_1 <- data[data$Timepoint == 1 & data$Variable == "mean.FM", ]

if(annot_meanFM_con_1_normCV$p.value & 
   annot_meanFM_asyn_1_normCV$p.value > 0.05){
  
  annot_meanFM_1_var <- bartlett.test(valuesCV ~ Condition, 
                                      data = data_meanFM_1)
  print(annot_meanFM_1_var)
  print("Variance is equal between groups")
  
} else if(annot_meanFM_con_1_normCV$p.value | 
          annot_meanFM_asyn_1_normCV$p.value < 0.05){
  
  annot_meanFM_1_var <- leveneTest(valuesCV ~ Condition, 
                                   data = data_meanFM_1)
  print(annot_meanFM_1_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_1_meanFM <- wilcox.test(valuesCV ~ Condition, data = data_meanFM_1,
                                      paired = FALSE, alternative = "two.sided")

annot_ValCV_1_meanFM <- tidy(annot_ValCV_1_meanFM)

##- 2 Timepoint 

data_meanFM_asyn_2 <- data[data$Timepoint == 2 & 
                             data$Variable == "mean.FM" &
                             data$Condition == "asyn", ]

data_meanFM_con_2 <- data[data$Timepoint == 2 & 
                            data$Variable == "mean.FM" &
                            data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanFM_asyn_2, x = "valuesCV")

ggdensity(data = data_meanFM_asyn_2, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanFM_asyn_2_normCV <- shapiro.test(x = data_meanFM_asyn_2$valuesCV)

if(annot_meanFM_asyn_2_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanFM_con_2, x = "valuesCV")

ggdensity(data = data_meanFM_con_2, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanFM_con_2_normCV <- shapiro.test(x = data_meanFM_con_2$valuesCV)

if(annot_meanFM_con_2_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanFM_2 <- data[data$Timepoint == 2 & data$Variable == "mean.FM", ]

if(annot_meanFM_con_2_normCV$p.value & 
   annot_meanFM_asyn_2_normCV$p.value > 0.05){
  
  annot_meanFM_2_var <- bartlett.test(valuesCV ~ Condition, 
                                      data = data_meanFM_2)
  print(annot_meanFM_2_var)
  print("Variance is equal between groups")
  
} else if(annot_meanFM_con_2_normCV$p.value | 
          annot_meanFM_asyn_2_normCV$p.value < 0.05){
  
  annot_meanFM_2_var <- leveneTest(valuesCV ~ Condition, 
                                   data = data_meanFM_2)
  print(annot_meanFM_2_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_2_meanFM <- wilcox.test(valuesCV ~ Condition, data = data_meanFM_2,
                                      paired = FALSE, alternative = "two.sided")

annot_ValCV_2_meanFM <- tidy(annot_ValCV_2_meanFM)

##- 3 Timepoint 

data_meanFM_asyn_3 <- data[data$Timepoint == 3 & 
                             data$Variable == "mean.FM" &
                             data$Condition == "asyn", ]

data_meanFM_con_3 <- data[data$Timepoint == 3 & 
                            data$Variable == "mean.FM" &
                            data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanFM_asyn_3, x = "valuesCV")

ggdensity(data = data_meanFM_asyn_3, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanFM_asyn_3_normCV <- shapiro.test(x = data_meanFM_asyn_3$valuesCV)

if(annot_meanFM_asyn_3_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanFM_con_3, x = "valuesCV")

ggdensity(data = data_meanFM_con_3, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanFM_con_3_normCV <- shapiro.test(x = data_meanFM_con_3$valuesCV)

if(annot_meanFM_con_3_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanFM_3 <- data[data$Timepoint == 3 & data$Variable == "mean.FM", ]

if(annot_meanFM_con_3_normCV$p.value & 
   annot_meanFM_asyn_3_normCV$p.value > 0.05){
  
  annot_meanFM_3_var <- bartlett.test(valuesCV ~ Condition, 
                                      data = data_meanFM_3)
  print(annot_meanFM_3_var)
  print("Variance is equal between groups")
  
} else if(annot_meanFM_con_3_normCV$p.value | 
          annot_meanFM_asyn_3_normCV$p.value < 0.05){
  
  annot_meanFM_3_var <- leveneTest(valuesCV ~ Condition, 
                                   data = data_meanFM_3)
  print(annot_meanFM_3_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_3_meanFM <- wilcox.test(valuesCV ~ Condition, data = data_meanFM_3,
                                      paired = FALSE, alternative = "two.sided")

annot_ValCV_3_meanFM <- tidy(annot_ValCV_3_meanFM)

###- Create Dataframe of P-values for each comparison

annot_ValCV_Names_meanFM <- c("annot_ValCV_0_meanFM","annot_ValCV_1_meanFM",
                                "annot_ValCV_2_meanFM","annot_ValCV_3_meanFM")

annot_ValCV_List_meanFM <- list(annot_ValCV_0_meanFM,annot_ValCV_1_meanFM,
                                  annot_ValCV_2_meanFM,annot_ValCV_3_meanFM)

names(annot_ValCV_List_meanFM) <- annot_ValCV_Names_meanFM

annot_ValCV_meanFM <- bind_rows(annot_ValCV_List_meanFM,
                                  .id = "Information")

### Wilcoxon Rank Sum Test on Normalized Mean Mean Frequency ###
### by timepoint and comparing conditions ###

##- 0 Timepoint 

data_meanmeanfreq_asyn_0 <- data[data$Timepoint == 0 & 
                                   data$Variable == "mean.mean.freq" &
                                   data$Condition == "asyn", ]

data_meanmeanfreq_con_0 <- data[data$Timepoint == 0 & 
                                  data$Variable == "mean.mean.freq" &
                                  data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanmeanfreq_asyn_0, x = "valuesCV")

ggdensity(data = data_meanmeanfreq_asyn_0, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanmeanfreq_asyn_0_normCV <- shapiro.test(x = data_meanmeanfreq_asyn_0$valuesCV)

if(annot_meanmeanfreq_asyn_0_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanmeanfreq_con_0, x = "valuesCV")

ggdensity(data = data_meanmeanfreq_con_0, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanmeanfreq_con_0_normCV <- shapiro.test(x = data_meanmeanfreq_con_0$valuesCV)

if(annot_meanmeanfreq_con_0_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanmeanfreq_0 <- data[data$Timepoint == 0 & data$Variable == "mean.mean.freq", ]

if(annot_meanmeanfreq_con_0_normCV$p.value & 
   annot_meanmeanfreq_asyn_0_normCV$p.value > 0.05){
  
  annot_meanmeanfreq_0_var <- bartlett.test(valuesCV ~ Condition, 
                                            data = data_meanmeanfreq_0)
  print(annot_meanmeanfreq_0_var)
  print("Variance is equal between groups")
  
} else if(annot_meanmeanfreq_con_0_normCV$p.value | 
          annot_meanmeanfreq_asyn_0_normCV$p.value < 0.05){
  
  annot_meanmeanfreq_0_var <- leveneTest(valuesCV ~ Condition, 
                                         data = data_meanmeanfreq_0)
  print(annot_meanmeanfreq_0_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_0_meanmeanfreq <- wilcox.test(valuesCV ~ Condition, data = data_meanmeanfreq_0,
                                            paired = FALSE, alternative = "two.sided")

annot_ValCV_0_meanmeanfreq <- tidy(annot_ValCV_0_meanmeanfreq)

##- 1 Timepoint 

data_meanmeanfreq_asyn_1 <- data[data$Timepoint == 1 & 
                                   data$Variable == "mean.mean.freq" &
                                   data$Condition == "asyn", ]

data_meanmeanfreq_con_1 <- data[data$Timepoint == 1 & 
                                  data$Variable == "mean.mean.freq" &
                                  data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanmeanfreq_asyn_1, x = "valuesCV")

ggdensity(data = data_meanmeanfreq_asyn_1, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanmeanfreq_asyn_1_normCV <- shapiro.test(x = data_meanmeanfreq_asyn_1$valuesCV)

if(annot_meanmeanfreq_asyn_1_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanmeanfreq_con_1, x = "valuesCV")

ggdensity(data = data_meanmeanfreq_con_1, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanmeanfreq_con_1_normCV <- shapiro.test(x = data_meanmeanfreq_con_1$valuesCV)

if(annot_meanmeanfreq_con_1_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanmeanfreq_1 <- data[data$Timepoint == 1 & data$Variable == "mean.mean.freq", ]

if(annot_meanmeanfreq_con_1_normCV$p.value & 
   annot_meanmeanfreq_asyn_1_normCV$p.value > 0.05){
  
  annot_meanmeanfreq_1_var <- bartlett.test(valuesCV ~ Condition, 
                                            data = data_meanmeanfreq_1)
  print(annot_meanmeanfreq_1_var)
  print("Variance is equal between groups")
  
} else if(annot_meanmeanfreq_con_1_normCV$p.value | 
          annot_meanmeanfreq_asyn_1_normCV$p.value < 0.05){
  
  annot_meanmeanfreq_1_var <- leveneTest(valuesCV ~ Condition, 
                                         data = data_meanmeanfreq_1)
  print(annot_meanmeanfreq_1_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_1_meanmeanfreq <- wilcox.test(valuesCV ~ Condition, data = data_meanmeanfreq_1,
                                            paired = FALSE, alternative = "two.sided")

annot_ValCV_1_meanmeanfreq <- tidy(annot_ValCV_1_meanmeanfreq)

##- 2 Timepoint 

data_meanmeanfreq_asyn_2 <- data[data$Timepoint == 2 & 
                                   data$Variable == "mean.mean.freq" &
                                   data$Condition == "asyn", ]

data_meanmeanfreq_con_2 <- data[data$Timepoint == 2 & 
                                  data$Variable == "mean.mean.freq" &
                                  data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanmeanfreq_asyn_2, x = "valuesCV")

ggdensity(data = data_meanmeanfreq_asyn_2, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanmeanfreq_asyn_2_normCV <- shapiro.test(x = data_meanmeanfreq_asyn_2$valuesCV)

if(annot_meanmeanfreq_asyn_2_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanmeanfreq_con_2, x = "valuesCV")

ggdensity(data = data_meanmeanfreq_con_2, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanmeanfreq_con_2_normCV <- shapiro.test(x = data_meanmeanfreq_con_2$valuesCV)

if(annot_meanmeanfreq_con_2_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanmeanfreq_2 <- data[data$Timepoint == 2 & data$Variable == "mean.mean.freq", ]

if(annot_meanmeanfreq_con_2_normCV$p.value & 
   annot_meanmeanfreq_asyn_2_normCV$p.value > 0.05){
  
  annot_meanmeanfreq_2_var <- bartlett.test(valuesCV ~ Condition, 
                                            data = data_meanmeanfreq_2)
  print(annot_meanmeanfreq_2_var)
  print("Variance is equal between groups")
  
} else if(annot_meanmeanfreq_con_2_normCV$p.value | 
          annot_meanmeanfreq_asyn_2_normCV$p.value < 0.05){
  
  annot_meanmeanfreq_2_var <- leveneTest(valuesCV ~ Condition, 
                                         data = data_meanmeanfreq_2)
  print(annot_meanmeanfreq_2_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_2_meanmeanfreq <- wilcox.test(valuesCV ~ Condition, data = data_meanmeanfreq_2,
                                            paired = FALSE, alternative = "two.sided")

annot_ValCV_2_meanmeanfreq <- tidy(annot_ValCV_2_meanmeanfreq)

##- 3 Timepoint 

data_meanmeanfreq_asyn_3 <- data[data$Timepoint == 3 & 
                                   data$Variable == "mean.mean.freq" &
                                   data$Condition == "asyn", ]

data_meanmeanfreq_con_3 <- data[data$Timepoint == 3 & 
                                  data$Variable == "mean.mean.freq" &
                                  data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanmeanfreq_asyn_3, x = "valuesCV")

ggdensity(data = data_meanmeanfreq_asyn_3, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanmeanfreq_asyn_3_normCV <- shapiro.test(x = data_meanmeanfreq_asyn_3$valuesCV)

if(annot_meanmeanfreq_asyn_3_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanmeanfreq_con_3, x = "valuesCV")

ggdensity(data = data_meanmeanfreq_con_3, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanmeanfreq_con_3_normCV <- shapiro.test(x = data_meanmeanfreq_con_3$valuesCV)

if(annot_meanmeanfreq_con_3_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanmeanfreq_3 <- data[data$Timepoint == 3 & data$Variable == "mean.mean.freq", ]

if(annot_meanmeanfreq_con_3_normCV$p.value & 
   annot_meanmeanfreq_asyn_3_normCV$p.value > 0.05){
  
  annot_meanmeanfreq_3_var <- bartlett.test(valuesCV ~ Condition, 
                                            data = data_meanmeanfreq_3)
  print(annot_meanmeanfreq_3_var)
  print("Variance is equal between groups")
  
} else if(annot_meanmeanfreq_con_3_normCV$p.value | 
          annot_meanmeanfreq_asyn_3_normCV$p.value < 0.05){
  
  annot_meanmeanfreq_3_var <- leveneTest(valuesCV ~ Condition, 
                                         data = data_meanmeanfreq_3)
  print(annot_meanmeanfreq_3_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_3_meanmeanfreq <- wilcox.test(valuesCV ~ Condition, data = data_meanmeanfreq_3,
                                            paired = FALSE, alternative = "two.sided")

annot_ValCV_3_meanmeanfreq <- tidy(annot_ValCV_3_meanmeanfreq)

###- Create Dataframe of P-values for each comparison

annot_ValCV_Names_meanmeanfreq <- c("annot_ValCV_0_meanmeanfreq","annot_ValCV_1_meanmeanfreq",
                                      "annot_ValCV_2_meanmeanfreq","annot_ValCV_3_meanmeanfreq")

annot_ValCV_List_meanmeanfreq <- list(annot_ValCV_0_meanmeanfreq,annot_ValCV_1_meanmeanfreq,
                                        annot_ValCV_2_meanmeanfreq,annot_ValCV_3_meanmeanfreq)

names(annot_ValCV_List_meanmeanfreq) <- annot_ValCV_Names_meanmeanfreq

annot_ValCV_meanmeanfreq <- bind_rows(annot_ValCV_List_meanmeanfreq,
                                        .id = "Information")

### Wilcoxon Rank Sum Test on Normalized Mean Pitch ###
### by timepoint and comparing conditions ###

##- 0 Timepoint 

data_meanpitch_asyn_0 <- data[data$Timepoint == 0 & 
                                data$Variable == "mean.pitch" &
                                data$Condition == "asyn", ]

data_meanpitch_con_0 <- data[data$Timepoint == 0 & 
                               data$Variable == "mean.pitch" &
                               data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanpitch_asyn_0, x = "valuesCV")

ggdensity(data = data_meanpitch_asyn_0, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitch_asyn_0_normCV <- shapiro.test(x = data_meanpitch_asyn_0$valuesCV)

if(annot_meanpitch_asyn_0_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanpitch_con_0, x = "valuesCV")

ggdensity(data = data_meanpitch_con_0, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitch_con_0_normCV <- shapiro.test(x = data_meanpitch_con_0$valuesCV)

if(annot_meanpitch_con_0_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanpitch_0 <- data[data$Timepoint == 0 & data$Variable == "mean.pitch", ]

if(annot_meanpitch_con_0_normCV$p.value & 
   annot_meanpitch_asyn_0_normCV$p.value > 0.05){
  
  annot_meanpitch_0_var <- bartlett.test(valuesCV ~ Condition, 
                                         data = data_meanpitch_0)
  print(annot_meanpitch_0_var)
  print("Variance is equal between groups")
  
} else if(annot_meanpitch_con_0_normCV$p.value | 
          annot_meanpitch_asyn_0_normCV$p.value < 0.05){
  
  annot_meanpitch_0_var <- leveneTest(valuesCV ~ Condition, 
                                      data = data_meanpitch_0)
  print(annot_meanpitch_0_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_0_meanpitch <- wilcox.test(valuesCV ~ Condition, data = data_meanpitch_0,
                                         paired = FALSE, alternative = "two.sided")

annot_ValCV_0_meanpitch <- tidy(annot_ValCV_0_meanpitch)

##- 1 Timepoint 

data_meanpitch_asyn_1 <- data[data$Timepoint == 1 & 
                                data$Variable == "mean.pitch" &
                                data$Condition == "asyn", ]

data_meanpitch_con_1 <- data[data$Timepoint == 1 & 
                               data$Variable == "mean.pitch" &
                               data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanpitch_asyn_1, x = "valuesCV")

ggdensity(data = data_meanpitch_asyn_1, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitch_asyn_1_normCV <- shapiro.test(x = data_meanpitch_asyn_1$valuesCV)

if(annot_meanpitch_asyn_1_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanpitch_con_1, x = "valuesCV")

ggdensity(data = data_meanpitch_con_1, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitch_con_1_normCV <- shapiro.test(x = data_meanpitch_con_1$valuesCV)

if(annot_meanpitch_con_1_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanpitch_1 <- data[data$Timepoint == 1 & data$Variable == "mean.pitch", ]

if(annot_meanpitch_con_1_normCV$p.value & 
   annot_meanpitch_asyn_1_normCV$p.value > 0.05){
  
  annot_meanpitch_1_var <- bartlett.test(valuesCV ~ Condition, 
                                         data = data_meanpitch_1)
  print(annot_meanpitch_1_var)
  print("Variance is equal between groups")
  
} else if(annot_meanpitch_con_1_normCV$p.value | 
          annot_meanpitch_asyn_1_normCV$p.value < 0.05){
  
  annot_meanpitch_1_var <- leveneTest(valuesCV ~ Condition, 
                                      data = data_meanpitch_1)
  print(annot_meanpitch_1_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_1_meanpitch <- wilcox.test(valuesCV ~ Condition, data = data_meanpitch_1,
                                         paired = FALSE, alternative = "two.sided")

annot_ValCV_1_meanpitch <- tidy(annot_ValCV_1_meanpitch)

##- 2 Timepoint 

data_meanpitch_asyn_2 <- data[data$Timepoint == 2 & 
                                data$Variable == "mean.pitch" &
                                data$Condition == "asyn", ]

data_meanpitch_con_2 <- data[data$Timepoint == 2 & 
                               data$Variable == "mean.pitch" &
                               data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanpitch_asyn_2, x = "valuesCV")

ggdensity(data = data_meanpitch_asyn_2, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitch_asyn_2_normCV <- shapiro.test(x = data_meanpitch_asyn_2$valuesCV)

if(annot_meanpitch_asyn_2_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanpitch_con_2, x = "valuesCV")

ggdensity(data = data_meanpitch_con_2, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitch_con_2_normCV <- shapiro.test(x = data_meanpitch_con_2$valuesCV)

if(annot_meanpitch_con_2_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanpitch_2 <- data[data$Timepoint == 2 & data$Variable == "mean.pitch", ]

if(annot_meanpitch_con_2_normCV$p.value & 
   annot_meanpitch_asyn_2_normCV$p.value > 0.05){
  
  annot_meanpitch_2_var <- bartlett.test(valuesCV ~ Condition, 
                                         data = data_meanpitch_2)
  print(annot_meanpitch_2_var)
  print("Variance is equal between groups")
  
} else if(annot_meanpitch_con_2_normCV$p.value | 
          annot_meanpitch_asyn_2_normCV$p.value < 0.05){
  
  annot_meanpitch_2_var <- leveneTest(valuesCV ~ Condition, 
                                      data = data_meanpitch_2)
  print(annot_meanpitch_2_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_2_meanpitch <- wilcox.test(valuesCV ~ Condition, data = data_meanpitch_2,
                                         paired = FALSE, alternative = "two.sided")

annot_ValCV_2_meanpitch <- tidy(annot_ValCV_2_meanpitch)

##- 3 Timepoint 

data_meanpitch_asyn_3 <- data[data$Timepoint == 3 & 
                                data$Variable == "mean.pitch" &
                                data$Condition == "asyn", ]

data_meanpitch_con_3 <- data[data$Timepoint == 3 & 
                               data$Variable == "mean.pitch" &
                               data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanpitch_asyn_3, x = "valuesCV")

ggdensity(data = data_meanpitch_asyn_3, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitch_asyn_3_normCV <- shapiro.test(x = data_meanpitch_asyn_3$valuesCV)

if(annot_meanpitch_asyn_3_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanpitch_con_3, x = "valuesCV")

ggdensity(data = data_meanpitch_con_3, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitch_con_3_normCV <- shapiro.test(x = data_meanpitch_con_3$valuesCV)

if(annot_meanpitch_con_3_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanpitch_3 <- data[data$Timepoint == 3 & data$Variable == "mean.pitch", ]

if(annot_meanpitch_con_3_normCV$p.value & 
   annot_meanpitch_asyn_3_normCV$p.value > 0.05){
  
  annot_meanpitch_3_var <- bartlett.test(valuesCV ~ Condition, 
                                         data = data_meanpitch_3)
  print(annot_meanpitch_3_var)
  print("Variance is equal between groups")
  
} else if(annot_meanpitch_con_3_normCV$p.value | 
          annot_meanpitch_asyn_3_normCV$p.value < 0.05){
  
  annot_meanpitch_3_var <- leveneTest(valuesCV ~ Condition, 
                                      data = data_meanpitch_3)
  print(annot_meanpitch_3_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_3_meanpitch <- wilcox.test(valuesCV ~ Condition, data = data_meanpitch_3,
                                         paired = FALSE, alternative = "two.sided")

annot_ValCV_3_meanpitch <- tidy(annot_ValCV_3_meanpitch)

###- Create Dataframe of P-values for each comparison

annot_ValCV_Names_meanpitch <- c("annot_ValCV_0_meanpitch","annot_ValCV_1_meanpitch",
                                   "annot_ValCV_2_meanpitch","annot_ValCV_3_meanpitch")

annot_ValCV_List_meanpitch <- list(annot_ValCV_0_meanpitch,annot_ValCV_1_meanpitch,
                                     annot_ValCV_2_meanpitch,annot_ValCV_3_meanpitch)

names(annot_ValCV_List_meanpitch) <- annot_ValCV_Names_meanpitch

annot_ValCV_meanpitch <- bind_rows(annot_ValCV_List_meanpitch,
                                     .id = "Information")

### Wilcoxon Rank Sum Test on Normalized Mean Pitch Goodness ###
### by timepoint and comparing conditions ###

##- 0 Timepoint 

data_meanpitchgoodness_asyn_0 <- data[data$Timepoint == 0 & 
                                        data$Variable == "mean.pitch.goodness" &
                                        data$Condition == "asyn", ]

data_meanpitchgoodness_con_0 <- data[data$Timepoint == 0 & 
                                       data$Variable == "mean.pitch.goodness" &
                                       data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanpitchgoodness_asyn_0, x = "valuesCV")

ggdensity(data = data_meanpitchgoodness_asyn_0, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitchgoodness_asyn_0_normCV <- shapiro.test(x = data_meanpitchgoodness_asyn_0$valuesCV)

if(annot_meanpitchgoodness_asyn_0_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanpitchgoodness_con_0, x = "valuesCV")

ggdensity(data = data_meanpitchgoodness_con_0, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitchgoodness_con_0_normCV <- shapiro.test(x = data_meanpitchgoodness_con_0$valuesCV)

if(annot_meanpitchgoodness_con_0_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanpitchgoodness_0 <- data[data$Timepoint == 0 & data$Variable == "mean.pitch.goodness", ]

if(annot_meanpitchgoodness_con_0_normCV$p.value & 
   annot_meanpitchgoodness_asyn_0_normCV$p.value > 0.05){
  
  annot_meanpitchgoodness_0_var <- bartlett.test(valuesCV ~ Condition, 
                                                 data = data_meanpitchgoodness_0)
  print(annot_meanpitchgoodness_0_var)
  print("Variance is equal between groups")
  
} else if(annot_meanpitchgoodness_con_0_normCV$p.value | 
          annot_meanpitchgoodness_asyn_0_normCV$p.value < 0.05){
  
  annot_meanpitchgoodness_0_var <- leveneTest(valuesCV ~ Condition, 
                                              data = data_meanpitchgoodness_0)
  print(annot_meanpitchgoodness_0_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_0_meanpitchgoodness <- wilcox.test(valuesCV ~ Condition, data = data_meanpitchgoodness_0,
                                                 paired = FALSE, alternative = "two.sided")

annot_ValCV_0_meanpitchgoodness <- tidy(annot_ValCV_0_meanpitchgoodness)

##- 1 Timepoint 

data_meanpitchgoodness_asyn_1 <- data[data$Timepoint == 1 & 
                                        data$Variable == "mean.pitch.goodness" &
                                        data$Condition == "asyn", ]

data_meanpitchgoodness_con_1 <- data[data$Timepoint == 1 & 
                                       data$Variable == "mean.pitch.goodness" &
                                       data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanpitchgoodness_asyn_1, x = "valuesCV")

ggdensity(data = data_meanpitchgoodness_asyn_1, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitchgoodness_asyn_1_normCV <- shapiro.test(x = data_meanpitchgoodness_asyn_1$valuesCV)

if(annot_meanpitchgoodness_asyn_1_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanpitchgoodness_con_1, x = "valuesCV")

ggdensity(data = data_meanpitchgoodness_con_1, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitchgoodness_con_1_normCV <- shapiro.test(x = data_meanpitchgoodness_con_1$valuesCV)

if(annot_meanpitchgoodness_con_1_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanpitchgoodness_1 <- data[data$Timepoint == 1 & data$Variable == "mean.pitch.goodness", ]

if(annot_meanpitchgoodness_con_1_normCV$p.value & 
   annot_meanpitchgoodness_asyn_1_normCV$p.value > 0.05){
  
  annot_meanpitchgoodness_1_var <- bartlett.test(valuesCV ~ Condition, 
                                                 data = data_meanpitchgoodness_1)
  print(annot_meanpitchgoodness_1_var)
  print("Variance is equal between groups")
  
} else if(annot_meanpitchgoodness_con_1_normCV$p.value | 
          annot_meanpitchgoodness_asyn_1_normCV$p.value < 0.05){
  
  annot_meanpitchgoodness_1_var <- leveneTest(valuesCV ~ Condition, 
                                              data = data_meanpitchgoodness_1)
  print(annot_meanpitchgoodness_1_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_1_meanpitchgoodness <- wilcox.test(valuesCV ~ Condition, data = data_meanpitchgoodness_1,
                                                 paired = FALSE, alternative = "two.sided")

annot_ValCV_1_meanpitchgoodness <- tidy(annot_ValCV_1_meanpitchgoodness)

##- 2 Timepoint 

data_meanpitchgoodness_asyn_2 <- data[data$Timepoint == 2 & 
                                        data$Variable == "mean.pitch.goodness" &
                                        data$Condition == "asyn", ]

data_meanpitchgoodness_con_2 <- data[data$Timepoint == 2 & 
                                       data$Variable == "mean.pitch.goodness" &
                                       data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanpitchgoodness_asyn_2, x = "valuesCV")

ggdensity(data = data_meanpitchgoodness_asyn_2, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitchgoodness_asyn_2_normCV <- shapiro.test(x = data_meanpitchgoodness_asyn_2$valuesCV)

if(annot_meanpitchgoodness_asyn_2_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanpitchgoodness_con_2, x = "valuesCV")

ggdensity(data = data_meanpitchgoodness_con_2, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitchgoodness_con_2_normCV <- shapiro.test(x = data_meanpitchgoodness_con_2$valuesCV)

if(annot_meanpitchgoodness_con_2_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanpitchgoodness_2 <- data[data$Timepoint == 2 & data$Variable == "mean.pitch.goodness", ]

if(annot_meanpitchgoodness_con_2_normCV$p.value & 
   annot_meanpitchgoodness_asyn_2_normCV$p.value > 0.05){
  
  annot_meanpitchgoodness_2_var <- bartlett.test(valuesCV ~ Condition, 
                                                 data = data_meanpitchgoodness_2)
  print(annot_meanpitchgoodness_2_var)
  print("Variance is equal between groups")
  
} else if(annot_meanpitchgoodness_con_2_normCV$p.value | 
          annot_meanpitchgoodness_asyn_2_normCV$p.value < 0.05){
  
  annot_meanpitchgoodness_2_var <- leveneTest(valuesCV ~ Condition, 
                                              data = data_meanpitchgoodness_2)
  print(annot_meanpitchgoodness_2_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_2_meanpitchgoodness <- wilcox.test(valuesCV ~ Condition, data = data_meanpitchgoodness_2,
                                                 paired = FALSE, alternative = "two.sided")

annot_ValCV_2_meanpitchgoodness <- tidy(annot_ValCV_2_meanpitchgoodness)

##- 3 Timepoint 

data_meanpitchgoodness_asyn_3 <- data[data$Timepoint == 3 & 
                                        data$Variable == "mean.pitch.goodness" &
                                        data$Condition == "asyn", ]

data_meanpitchgoodness_con_3 <- data[data$Timepoint == 3 & 
                                       data$Variable == "mean.pitch.goodness" &
                                       data$Condition == "con", ]

#- asyn

ggqqplot(data = data_meanpitchgoodness_asyn_3, x = "valuesCV")

ggdensity(data = data_meanpitchgoodness_asyn_3, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitchgoodness_asyn_3_normCV <- shapiro.test(x = data_meanpitchgoodness_asyn_3$valuesCV)

if(annot_meanpitchgoodness_asyn_3_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_meanpitchgoodness_con_3, x = "valuesCV")

ggdensity(data = data_meanpitchgoodness_con_3, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_meanpitchgoodness_con_3_normCV <- shapiro.test(x = data_meanpitchgoodness_con_3$valuesCV)

if(annot_meanpitchgoodness_con_3_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_meanpitchgoodness_3 <- data[data$Timepoint == 3 & data$Variable == "mean.pitch.goodness", ]

if(annot_meanpitchgoodness_con_3_normCV$p.value & 
   annot_meanpitchgoodness_asyn_3_normCV$p.value > 0.05){
  
  annot_meanpitchgoodness_3_var <- bartlett.test(valuesCV ~ Condition, 
                                                 data = data_meanpitchgoodness_3)
  print(annot_meanpitchgoodness_3_var)
  print("Variance is equal between groups")
  
} else if(annot_meanpitchgoodness_con_3_normCV$p.value | 
          annot_meanpitchgoodness_asyn_3_normCV$p.value < 0.05){
  
  annot_meanpitchgoodness_3_var <- leveneTest(valuesCV ~ Condition, 
                                              data = data_meanpitchgoodness_3)
  print(annot_meanpitchgoodness_3_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_3_meanpitchgoodness <- wilcox.test(valuesCV ~ Condition, data = data_meanpitchgoodness_3,
                                                 paired = FALSE, alternative = "two.sided")

annot_ValCV_3_meanpitchgoodness <- tidy(annot_ValCV_3_meanpitchgoodness)

###- Create Dataframe of P-values for each comparison

annot_ValCV_Names_meanpitchgoodness <- c("annot_ValCV_0_meanpitchgoodness","annot_ValCV_1_meanpitchgoodness",
                                           "annot_ValCV_2_meanpitchgoodness","annot_ValCV_3_meanpitchgoodness")

annot_ValCV_List_meanpitchgoodness <- list(annot_ValCV_0_meanpitchgoodness,annot_ValCV_1_meanpitchgoodness,
                                             annot_ValCV_2_meanpitchgoodness,annot_ValCV_3_meanpitchgoodness)

names(annot_ValCV_List_meanpitchgoodness) <- annot_ValCV_Names_meanpitchgoodness

annot_ValCV_meanpitchgoodness <- bind_rows(annot_ValCV_List_meanpitchgoodness,
                                             .id = "Information")

### Wilcoxon Rank Sum Test on Normalized Syllable Duration ###
### by timepoint and comparing conditions ###

##- 0 Timepoint 

data_syllableduration_asyn_0 <- data[data$Timepoint == 0 & 
                                       data$Variable == "syllable.duration" &
                                       data$Condition == "asyn", ]

data_syllableduration_con_0 <- data[data$Timepoint == 0 & 
                                      data$Variable == "syllable.duration" &
                                      data$Condition == "con", ]

#- asyn

ggqqplot(data = data_syllableduration_asyn_0, x = "valuesCV")

ggdensity(data = data_syllableduration_asyn_0, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_syllableduration_asyn_0_normCV <- shapiro.test(x = data_syllableduration_asyn_0$valuesCV)

if(annot_syllableduration_asyn_0_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_syllableduration_con_0, x = "valuesCV")

ggdensity(data = data_syllableduration_con_0, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_syllableduration_con_0_normCV <- shapiro.test(x = data_syllableduration_con_0$valuesCV)

if(annot_syllableduration_con_0_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_syllableduration_0 <- data[data$Timepoint == 0 & data$Variable == "syllable.duration", ]

if(annot_syllableduration_con_0_normCV$p.value & 
   annot_syllableduration_asyn_0_normCV$p.value > 0.05){
  
  annot_syllableduration_0_var <- bartlett.test(valuesCV ~ Condition, 
                                                data = data_syllableduration_0)
  print(annot_syllableduration_0_var)
  print("Variance is equal between groups")
  
} else if(annot_syllableduration_con_0_normCV$p.value | 
          annot_syllableduration_asyn_0_normCV$p.value < 0.05){
  
  annot_syllableduration_0_var <- leveneTest(valuesCV ~ Condition, 
                                             data = data_syllableduration_0)
  print(annot_syllableduration_0_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_0_syllableduration <- wilcox.test(valuesCV ~ Condition, data = data_syllableduration_0,
                                                paired = FALSE, alternative = "two.sided")

annot_ValCV_0_syllableduration <- tidy(annot_ValCV_0_syllableduration)

##- 1 Timepoint 

data_syllableduration_asyn_1 <- data[data$Timepoint == 1 & 
                                       data$Variable == "syllable.duration" &
                                       data$Condition == "asyn", ]

data_syllableduration_con_1 <- data[data$Timepoint == 1 & 
                                      data$Variable == "syllable.duration" &
                                      data$Condition == "con", ]

#- asyn

ggqqplot(data = data_syllableduration_asyn_1, x = "valuesCV")

ggdensity(data = data_syllableduration_asyn_1, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_syllableduration_asyn_1_normCV <- shapiro.test(x = data_syllableduration_asyn_1$valuesCV)

if(annot_syllableduration_asyn_1_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_syllableduration_con_1, x = "valuesCV")

ggdensity(data = data_syllableduration_con_1, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_syllableduration_con_1_normCV <- shapiro.test(x = data_syllableduration_con_1$valuesCV)

if(annot_syllableduration_con_1_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_syllableduration_1 <- data[data$Timepoint == 1 & data$Variable == "syllable.duration", ]

if(annot_syllableduration_con_1_normCV$p.value & 
   annot_syllableduration_asyn_1_normCV$p.value > 0.05){
  
  annot_syllableduration_1_var <- bartlett.test(valuesCV ~ Condition, 
                                                data = data_syllableduration_1)
  print(annot_syllableduration_1_var)
  print("Variance is equal between groups")
  
} else if(annot_syllableduration_con_1_normCV$p.value | 
          annot_syllableduration_asyn_1_normCV$p.value < 0.05){
  
  annot_syllableduration_1_var <- leveneTest(valuesCV ~ Condition, 
                                             data = data_syllableduration_1)
  print(annot_syllableduration_1_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_1_syllableduration <- wilcox.test(valuesCV ~ Condition, data = data_syllableduration_1,
                                                paired = FALSE, alternative = "two.sided")

annot_ValCV_1_syllableduration <- tidy(annot_ValCV_1_syllableduration)

##- 2 Timepoint 

data_syllableduration_asyn_2 <- data[data$Timepoint == 2 & 
                                       data$Variable == "syllable.duration" &
                                       data$Condition == "asyn", ]

data_syllableduration_con_2 <- data[data$Timepoint == 2 & 
                                      data$Variable == "syllable.duration" &
                                      data$Condition == "con", ]

#- asyn

ggqqplot(data = data_syllableduration_asyn_2, x = "valuesCV")

ggdensity(data = data_syllableduration_asyn_2, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_syllableduration_asyn_2_normCV <- shapiro.test(x = data_syllableduration_asyn_2$valuesCV)

if(annot_syllableduration_asyn_2_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_syllableduration_con_2, x = "valuesCV")

ggdensity(data = data_syllableduration_con_2, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_syllableduration_con_2_normCV <- shapiro.test(x = data_syllableduration_con_2$valuesCV)

if(annot_syllableduration_con_2_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_syllableduration_2 <- data[data$Timepoint == 2 & data$Variable == "syllable.duration", ]

if(annot_syllableduration_con_2_normCV$p.value & 
   annot_syllableduration_asyn_2_normCV$p.value > 0.05){
  
  annot_syllableduration_2_var <- bartlett.test(valuesCV ~ Condition, 
                                                data = data_syllableduration_2)
  print(annot_syllableduration_2_var)
  print("Variance is equal between groups")
  
} else if(annot_syllableduration_con_2_normCV$p.value | 
          annot_syllableduration_asyn_2_normCV$p.value < 0.05){
  
  annot_syllableduration_2_var <- leveneTest(valuesCV ~ Condition, 
                                             data = data_syllableduration_2)
  print(annot_syllableduration_2_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_2_syllableduration <- wilcox.test(valuesCV ~ Condition, data = data_syllableduration_2,
                                                paired = FALSE, alternative = "two.sided")

annot_ValCV_2_syllableduration <- tidy(annot_ValCV_2_syllableduration)

##- 3 Timepoint 

data_syllableduration_asyn_3 <- data[data$Timepoint == 3 & 
                                       data$Variable == "syllable.duration" &
                                       data$Condition == "asyn", ]

data_syllableduration_con_3 <- data[data$Timepoint == 3 & 
                                      data$Variable == "syllable.duration" &
                                      data$Condition == "con", ]

#- asyn

ggqqplot(data = data_syllableduration_asyn_3, x = "valuesCV")

ggdensity(data = data_syllableduration_asyn_3, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_syllableduration_asyn_3_normCV <- shapiro.test(x = data_syllableduration_asyn_3$valuesCV)

if(annot_syllableduration_asyn_3_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = data_syllableduration_con_3, x = "valuesCV")

ggdensity(data = data_syllableduration_con_3, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_syllableduration_con_3_normCV <- shapiro.test(x = data_syllableduration_con_3$valuesCV)

if(annot_syllableduration_con_3_normCV$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

data_syllableduration_3 <- data[data$Timepoint == 3 & data$Variable == "syllable.duration", ]

if(annot_syllableduration_con_3_normCV$p.value & 
   annot_syllableduration_asyn_3_normCV$p.value > 0.05){
  
  annot_syllableduration_3_var <- bartlett.test(valuesCV ~ Condition, 
                                                data = data_syllableduration_3)
  print(annot_syllableduration_3_var)
  print("Variance is equal between groups")
  
} else if(annot_syllableduration_con_3_normCV$p.value | 
          annot_syllableduration_asyn_3_normCV$p.value < 0.05){
  
  annot_syllableduration_3_var <- leveneTest(valuesCV ~ Condition, 
                                             data = data_syllableduration_3)
  print(annot_syllableduration_3_var)
  print("Variance is not equal between groups")
  
}

annot_ValCV_3_syllableduration <- wilcox.test(valuesCV ~ Condition, data = data_syllableduration_3,
                                                paired = FALSE, alternative = "two.sided")

annot_ValCV_3_syllableduration <- tidy(annot_ValCV_3_syllableduration)

###- Create Dataframe of P-values for each comparison

annot_ValCV_Names_syllableduration <- c("annot_ValCV_0_syllableduration","annot_ValCV_1_syllableduration",
                                          "annot_ValCV_2_syllableduration","annot_ValCV_3_syllableduration")

annot_ValCV_List_syllableduration <- list(annot_ValCV_0_syllableduration,annot_ValCV_1_syllableduration,
                                            annot_ValCV_2_syllableduration,annot_ValCV_3_syllableduration)

names(annot_ValCV_List_syllableduration) <- annot_ValCV_Names_syllableduration

annot_ValCV_syllableduration <- bind_rows(annot_ValCV_List_syllableduration,
                                            .id = "Information")

####- Create Dataframe of P-values for 
####- mean acoustic feature comparisons

annot_ValCV <- bind_rows(annot_ValCV_meanAM2,annot_ValCV_meanamplitude,
                           annot_ValCV_meanentropy, annot_ValCV_meanFM,
                           annot_ValCV_meanmeanfreq, annot_ValCV_meanpitch,
                           annot_ValCV_meanpitchgoodness)

####- Combine all Val dataframes into one ####
####- p values and statistical tests used ####

VoICE_UD_BG_StatisticsTable <- bind_rows(annot_Valmean, annot_ValCV)

write.csv(VoICE_UD_BG_StatisticsTable, 
          file = "~/Documents/Julie_Lab/Data/ASYN_2018/VoICE/Conditions/VoICE_UD_BG_StatisticsTable.csv", 
          row.names = FALSE)

##### Acoustic Features for Intersyllable-intrarendition comparisons #####
##### For each Bird over multiple timepoints #####

BirdID <- "Bk174" # insert bird ID here

data <- read.table(file = paste0(maindir,"/",BirdID,"/",BirdID,"_UD_VoICE.csv"), 
                   header = TRUE, sep = ",")

#### Acoustic Features for Intersyllable-interrendition comparisons ####
#### For each Bird over multiple timepoints ####
