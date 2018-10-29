###### Hypothesis Testing for Accuracy and Similarity Score #####

#- subset by variable
#- subset further by condition and timepoint
#- to run comparisons using ggpubr or ggsignif

###### Tests if data follows normal distribution, ######
###### equal variance, and if its significant ######

library(readr)
library(tidyverse)
library(ggpubr)
library(svglite)
library(car)
library(broom)

#### similarity Batch Scores of Intersyllable-interrendition ####

graph <- read.table("~/Documents/Julie_Lab/Data/SB/Conditions/UD_SB_MasterSummarytable.csv",
                    header = TRUE, sep = ",")

selectedfeatures <- c("Accuracy","X.Similarity")

graph <- graph[graph$BirdID.Syll != "Bk286.A", ] # Similarity batch awful (so removed)
graph <- graph[graph$BirdID.Syll != "Wh214.F", ] # Similarity batch awful (so removed)
graph <- graph[graph$Variable %in% selectedfeatures, ]

### Wilcoxon Rank Sum Test on Normalized Mean Accuracy scores ###
### by timepoint and comparing conditions ###


##- 0 Timepoint 

graph_AS_asyn_0 <- graph[graph$Timepoint == 0 & 
                           graph$Variable == "Accuracy" &
                           graph$Condition == "asyn", ]

graph_AS_con_0 <- graph[graph$Timepoint == 0 & 
                          graph$Variable == "Accuracy" &
                          graph$Condition == "con", ]

#- asyn

ggqqplot(data = graph_AS_asyn_0, x = "valuesmean")

ggdensity(data = graph_AS_asyn_0, x = "valuesmean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_AS_asyn_0_norm <- shapiro.test(x = graph_AS_asyn_0$valuesmean)

if(annot_AS_asyn_0_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = graph_AS_con_0, x = "valuesmean")

ggdensity(data = graph_AS_con_0, x = "valuesmean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_AS_con_0_norm <- shapiro.test(x = graph_AS_con_0$valuesmean)

if(annot_AS_con_0_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

graph_AS_0 <- graph[graph$Timepoint == 0 & graph$Variable == "Accuracy", ]

if(annot_AS_con_0_norm$p.value & annot_AS_asyn_0_norm > 0.05){
  
  annot_AS_0_var <- bartlett.test(valuesmean ~ Condition, 
                                  data = graph_AS_0)
  print(annot_AS_0_var)
  print("Variance is equal between groups")
  
} else if(annot_AS_con_0_norm$p.value | annot_AS_con_0_norm < 0.05){
  
  annot_AS_0_var <- leveneTest(valuesmean ~ Condition, 
                               data = graph_AS_0)
  print(annot_AS_0_var)
  print("Variance is not equal between groups")
  
}

annot_AS_0_Valmean <- wilcox.test(valuesmean ~ Condition, data = graph_AS_0,
                                  paired = FALSE, alternative = "two.sided")

annot_AS_0_Valmean <- tidy(annot_AS_0_Valmean)

##- 1 timepoint

graph_AS_asyn_1 <- graph[graph$Timepoint == 1 & 
                           graph$Variable == "Accuracy" &
                           graph$Condition == "asyn", ]

graph_AS_con_1 <- graph[graph$Timepoint == 1 & 
                          graph$Variable == "Accuracy" &
                          graph$Condition == "con", ]

#- asyn

ggqqplot(data = graph_AS_asyn_1, x = "valuesmean")

ggdensity(data = graph_AS_asyn_1, x = "valuesmean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_AS_asyn_1_norm <- shapiro.test(x = graph_AS_asyn_1$valuesmean)

if(annot_AS_asyn_1_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = graph_AS_con_1, x = "valuesmean")

ggdensity(data = graph_AS_con_1, x = "valuesmean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_AS_con_1_norm <- shapiro.test(x = graph_AS_con_1$valuesmean)

if(annot_AS_con_1_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

graph_AS_1 <- graph[graph$Timepoint == 1 & graph$Variable == "Accuracy", ]

if(annot_AS_con_1_norm$p.value & annot_AS_asyn_1_norm > 0.05){
  
  annot_AS_1_var <- bartlett.test(valuesmean ~ Condition, 
                                  data = graph_AS_1)
  print(annot_AS_1_var)
  print("Variance is equal between groups")
  
} else if(annot_AS_con_1_norm$p.value | annot_AS_con_1_norm < 0.05){
  
  annot_AS_1_var <- leveneTest(valuesmean ~ Condition, 
                               data = graph_AS_1)
  print(annot_AS_1_var)
  print("Variance is not equal between groups")
  
}

annot_AS_1_Valmean <- wilcox.test(valuesmean ~ Condition, data = graph_AS_1,
                                  paired = FALSE, alternative = "two.sided")

annot_AS_1_Valmean <- tidy(annot_AS_1_Valmean)

##- 2 timepoint

graph_AS_asyn_2 <- graph[graph$Timepoint == 2 & 
                           graph$Variable == "Accuracy" &
                           graph$Condition == "asyn", ]

graph_AS_con_2 <- graph[graph$Timepoint == 2 & 
                          graph$Variable == "Accuracy" &
                          graph$Condition == "con", ]

#- asyn

ggqqplot(data = graph_AS_asyn_2, x = "valuesmean")

ggdensity(data = graph_AS_asyn_2, x = "valuesmean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_AS_asyn_2_norm <- shapiro.test(x = graph_AS_asyn_2$valuesmean)

if(annot_AS_asyn_2_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = graph_AS_con_2, x = "valuesmean")

ggdensity(data = graph_AS_con_2, x = "valuesmean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_AS_con_2_norm <- shapiro.test(x = graph_AS_con_2$valuesmean)

if(annot_AS_con_2_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

graph_AS_2 <- graph[graph$Timepoint == 2 & graph$Variable == "Accuracy", ]

if(annot_AS_con_2_norm$p.value & annot_AS_asyn_2_norm > 0.05){
  
  annot_AS_2_var <- bartlett.test(valuesmean ~ Condition, 
                                  data = graph_AS_2)
  print(annot_AS_2_var)
  print("Variance is equal between groups")
  
} else if(annot_AS_con_2_norm$p.value | annot_AS_con_2_norm < 0.05){
  
  annot_AS_2_var <- leveneTest(valuesmean ~ Condition, 
                               data = graph_AS_2)
  print(annot_AS_2_var)
  print("Variance is not equal between groups")
  
}

annot_AS_2_Valmean <- wilcox.test(valuesmean ~ Condition, data = graph_AS_2,
                                  paired = FALSE, alternative = "two.sided")

annot_AS_2_Valmean <- tidy(annot_AS_2_Valmean)


##-  3 timepoint

graph_AS_asyn_3 <- graph[graph$Timepoint == 3 & 
                           graph$Variable == "Accuracy" &
                           graph$Condition == "asyn", ]

graph_AS_con_3 <- graph[graph$Timepoint == 3 & 
                          graph$Variable == "Accuracy" &
                          graph$Condition == "con", ]

#- asyn

ggqqplot(data = graph_AS_asyn_3, x = "valuesmean")

ggdensity(data = graph_AS_asyn_3, x = "valuesmean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_AS_asyn_3_norm <- shapiro.test(x = graph_AS_asyn_3$valuesmean)

if(annot_AS_asyn_3_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = graph_AS_con_3, x = "valuesmean")

ggdensity(data = graph_AS_con_3, x = "valuesmean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_AS_con_3_norm <- shapiro.test(x = graph_AS_con_3$valuesmean)

if(annot_AS_con_3_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

graph_AS_3 <- graph[graph$Timepoint == 3 & graph$Variable == "Accuracy", ]

if(annot_AS_con_3_norm$p.value & annot_AS_asyn_3_norm > 0.05){
  
  annot_AS_3_var <- bartlett.test(valuesmean ~ Condition, 
                                  data = graph_AS_3)
  print(annot_AS_3_var)
  print("Variance is equal between groups")
  
} else if(annot_AS_con_3_norm$p.value | annot_AS_con_3_norm < 0.05){
  
  annot_AS_3_var <- leveneTest(valuesmean ~ Condition, 
                               data = graph_AS_3)
  print(annot_AS_3_var)
  print("Variance is not equal between groups")
  
}

annot_AS_3_Valmean <- wilcox.test(valuesmean ~ Condition, data = graph_AS_3,
                                  paired = FALSE, alternative = "two.sided")

annot_AS_3_Valmean <- tidy(annot_AS_3_Valmean)

###- Create Dataframe of P-values for each comparison

annot_AS_Valmean_Names <- c("annot_AS_0_Valmean","annot_AS_1_Valmean",
                         "annot_AS_2_Valmean","annot_AS_3_Valmean")

annot_AS_Valmean_List <- list(annot_AS_0_Valmean,annot_AS_1_Valmean,
                         annot_AS_2_Valmean,annot_AS_3_Valmean)

names(annot_AS_Valmean_List) <- annot_AS_Valmean_Names

annot_AS_Valmean <- bind_rows(annot_AS_Valmean_List,
                              .id = "Information")

### Wilcoxon Rank Sum Test on Normalized CV Accuracy scores ###
### by timepoint and comparing conditions ###


##- 0 Timepoint 

graph_AS_asyn_0 <- graph[graph$Timepoint == 0 & 
                           graph$Variable == "Accuracy" &
                           graph$Condition == "asyn", ]

graph_AS_con_0 <- graph[graph$Timepoint == 0 & 
                          graph$Variable == "Accuracy" &
                          graph$Condition == "con", ]

#- asyn

ggqqplot(data = graph_AS_asyn_0, x = "valuesCV")

ggdensity(data = graph_AS_asyn_0, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_AS_asyn_0_norm <- shapiro.test(x = graph_AS_asyn_0$valuesCV)

if(annot_AS_asyn_0_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = graph_AS_con_0, x = "valuesCV")

ggdensity(data = graph_AS_con_0, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_AS_con_0_norm <- shapiro.test(x = graph_AS_con_0$valuesCV)

if(annot_AS_con_0_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

graph_AS_0 <- graph[graph$Timepoint == 0 & graph$Variable == "Accuracy", ]

if(annot_AS_con_0_norm$p.value & annot_AS_asyn_0_norm > 0.05){
  
  annot_AS_0_var <- bartlett.test(valuesCV ~ Condition, 
                                  data = graph_AS_0)
  print(annot_AS_0_var)
  print("Variance is equal between groups")
  
} else if(annot_AS_con_0_norm$p.value | annot_AS_con_0_norm < 0.05){
  
  annot_AS_0_var <- leveneTest(valuesCV ~ Condition, 
                               data = graph_AS_0)
  print(annot_AS_0_var)
  print("Variance is not equal between groups")
  
}

annot_AS_0_ValCV <- wilcox.test(valuesCV ~ Condition, data = graph_AS_0,
                                  paired = FALSE, alternative = "two.sided")

annot_AS_0_ValCV <- tidy(annot_AS_0_ValCV)

##- 1 timepoint

graph_AS_asyn_1 <- graph[graph$Timepoint == 1 & 
                           graph$Variable == "Accuracy" &
                           graph$Condition == "asyn", ]

graph_AS_con_1 <- graph[graph$Timepoint == 1 & 
                          graph$Variable == "Accuracy" &
                          graph$Condition == "con", ]

#- asyn

ggqqplot(data = graph_AS_asyn_1, x = "valuesCV")

ggdensity(data = graph_AS_asyn_1, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_AS_asyn_1_norm <- shapiro.test(x = graph_AS_asyn_1$valuesCV)

if(annot_AS_asyn_1_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = graph_AS_con_1, x = "valuesCV")

ggdensity(data = graph_AS_con_1, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_AS_con_1_norm <- shapiro.test(x = graph_AS_con_1$valuesCV)

if(annot_AS_con_1_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

graph_AS_1 <- graph[graph$Timepoint == 1 & graph$Variable == "Accuracy", ]

if(annot_AS_con_1_norm$p.value & annot_AS_asyn_1_norm > 0.05){
  
  annot_AS_1_var <- bartlett.test(valuesCV ~ Condition, 
                                  data = graph_AS_1)
  print(annot_AS_1_var)
  print("Variance is equal between groups")
  
} else if(annot_AS_con_1_norm$p.value | annot_AS_con_1_norm < 0.05){
  
  annot_AS_1_var <- leveneTest(valuesCV ~ Condition, 
                               data = graph_AS_1)
  print(annot_AS_1_var)
  print("Variance is not equal between groups")
  
}

annot_AS_1_ValCV <- wilcox.test(valuesCV ~ Condition, data = graph_AS_1,
                                  paired = FALSE, alternative = "two.sided")

annot_AS_1_ValCV <- tidy(annot_AS_1_ValCV)

##- 2 timepoint

graph_AS_asyn_2 <- graph[graph$Timepoint == 2 & 
                           graph$Variable == "Accuracy" &
                           graph$Condition == "asyn", ]

graph_AS_con_2 <- graph[graph$Timepoint == 2 & 
                          graph$Variable == "Accuracy" &
                          graph$Condition == "con", ]

#- asyn

ggqqplot(data = graph_AS_asyn_2, x = "valuesCV")

ggdensity(data = graph_AS_asyn_2, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_AS_asyn_2_norm <- shapiro.test(x = graph_AS_asyn_2$valuesCV)

if(annot_AS_asyn_2_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = graph_AS_con_2, x = "valuesCV")

ggdensity(data = graph_AS_con_2, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_AS_con_2_norm <- shapiro.test(x = graph_AS_con_2$valuesCV)

if(annot_AS_con_2_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

graph_AS_2 <- graph[graph$Timepoint == 2 & graph$Variable == "Accuracy", ]

if(annot_AS_con_2_norm$p.value & annot_AS_asyn_2_norm > 0.05){
  
  annot_AS_2_var <- bartlett.test(valuesCV ~ Condition, 
                                  data = graph_AS_2)
  print(annot_AS_2_var)
  print("Variance is equal between groups")
  
} else if(annot_AS_con_2_norm$p.value | annot_AS_con_2_norm < 0.05){
  
  annot_AS_2_var <- leveneTest(valuesCV ~ Condition, 
                               data = graph_AS_2)
  print(annot_AS_2_var)
  print("Variance is not equal between groups")
  
}

annot_AS_2_ValCV <- wilcox.test(valuesCV ~ Condition, data = graph_AS_2,
                                  paired = FALSE, alternative = "two.sided")

annot_AS_2_ValCV <- tidy(annot_AS_2_ValCV)

##-  3 timepoint

graph_AS_asyn_3 <- graph[graph$Timepoint == 3 & 
                           graph$Variable == "Accuracy" &
                           graph$Condition == "asyn", ]

graph_AS_con_3 <- graph[graph$Timepoint == 3 & 
                          graph$Variable == "Accuracy" &
                          graph$Condition == "con", ]

#- asyn

ggqqplot(data = graph_AS_asyn_3, x = "valuesCV")

ggdensity(data = graph_AS_asyn_3, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_AS_asyn_3_norm <- shapiro.test(x = graph_AS_asyn_3$valuesCV)

if(annot_AS_asyn_3_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = graph_AS_con_3, x = "valuesCV")

ggdensity(data = graph_AS_con_3, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_AS_con_3_norm <- shapiro.test(x = graph_AS_con_3$valuesCV)

if(annot_AS_con_3_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

graph_AS_3 <- graph[graph$Timepoint == 3 & graph$Variable == "Accuracy", ]

if(annot_AS_con_3_norm$p.value & annot_AS_asyn_3_norm > 0.05){
  
  annot_AS_3_var <- bartlett.test(valuesCV ~ Condition, 
                                  data = graph_AS_3)
  print(annot_AS_3_var)
  print("Variance is equal between groups")
  
} else if(annot_AS_con_3_norm$p.value | annot_AS_con_3_norm < 0.05){
  
  annot_AS_3_var <- leveneTest(valuesCV ~ Condition, 
                               data = graph_AS_3)
  print(annot_AS_3_var)
  print("Variance is not equal between groups")
  
}

annot_AS_3_ValCV <- wilcox.test(valuesCV ~ Condition, data = graph_AS_3,
                                  paired = FALSE, alternative = "two.sided")

annot_AS_3_ValCV <- tidy(annot_AS_3_ValCV)

###- Create Dataframe of P-values for each comparison

annot_AS_ValCV_Names <- c("annot_AS_0_ValCV","annot_AS_1_ValCV",
                            "annot_AS_2_ValCV","annot_AS_3_ValCV")

annot_AS_ValCV_List <- list(annot_AS_0_ValCV,annot_AS_1_ValCV,
                              annot_AS_2_ValCV,annot_AS_3_ValCV)

names(annot_AS_ValCV_List) <- annot_AS_ValCV_Names

annot_AS_ValCV <- bind_rows(annot_AS_ValCV_List,
                              .id = "Information")

### Wilcoxon Rank Sum Test on Normalized Mean X.Similarity scores ###
### by timepoint and comparing conditions ###


##- 0 Timepoint 

graph_SS_asyn_0 <- graph[graph$Timepoint == 0 & 
                           graph$Variable == "X.Similarity" &
                           graph$Condition == "asyn", ]

graph_SS_con_0 <- graph[graph$Timepoint == 0 & 
                          graph$Variable == "X.Similarity" &
                          graph$Condition == "con", ]

#- asyn

ggqqplot(data = graph_SS_asyn_0, x = "valuesmean")

ggdensity(data = graph_SS_asyn_0, x = "valuesmean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_SS_asyn_0_norm <- shapiro.test(x = graph_SS_asyn_0$valuesmean)

if(annot_SS_asyn_0_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = graph_SS_con_0, x = "valuesmean")

ggdensity(data = graph_SS_con_0, x = "valuesmean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_SS_con_0_norm <- shapiro.test(x = graph_SS_con_0$valuesmean)

if(annot_SS_con_0_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

graph_SS_0 <- graph[graph$Timepoint == 0 & graph$Variable == "X.Similarity", ]

if(annot_SS_con_0_norm$p.value & annot_SS_asyn_0_norm > 0.05){
  
  annot_SS_0_var <- bartlett.test(valuesmean ~ Condition, 
                                  data = graph_SS_0)
  print(annot_SS_0_var)
  print("Variance is equal between groups")
  
} else if(annot_SS_con_0_norm$p.value | annot_SS_con_0_norm < 0.05){
  
  annot_SS_0_var <- leveneTest(valuesmean ~ Condition, 
                               data = graph_SS_0)
  print(annot_SS_0_var)
  print("Variance is not equal between groups")
  
}

annot_SS_0_Valmean <- wilcox.test(valuesmean ~ Condition, data = graph_SS_0,
                                  paired = FALSE, alternative = "two.sided")

annot_SS_0_Valmean <- tidy(annot_SS_0_Valmean)

##- 1 timepoint

graph_SS_asyn_1 <- graph[graph$Timepoint == 1 & 
                           graph$Variable == "X.Similarity" &
                           graph$Condition == "asyn", ]

graph_SS_con_1 <- graph[graph$Timepoint == 1 & 
                          graph$Variable == "X.Similarity" &
                          graph$Condition == "con", ]

#- asyn

ggqqplot(data = graph_SS_asyn_1, x = "valuesmean")

ggdensity(data = graph_SS_asyn_1, x = "valuesmean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_SS_asyn_1_norm <- shapiro.test(x = graph_SS_asyn_1$valuesmean)

if(annot_SS_asyn_1_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = graph_SS_con_1, x = "valuesmean")

ggdensity(data = graph_SS_con_1, x = "valuesmean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_SS_con_1_norm <- shapiro.test(x = graph_SS_con_1$valuesmean)

if(annot_SS_con_1_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

graph_SS_1 <- graph[graph$Timepoint == 1 & graph$Variable == "X.Similarity", ]

if(annot_SS_con_1_norm$p.value & annot_SS_asyn_1_norm > 0.05){
  
  annot_SS_1_var <- bartlett.test(valuesmean ~ Condition, 
                                  data = graph_SS_1)
  print(annot_SS_1_var)
  print("Variance is equal between groups")
  
} else if(annot_SS_con_1_norm$p.value | annot_SS_con_1_norm < 0.05){
  
  annot_SS_1_var <- leveneTest(valuesmean ~ Condition, 
                               data = graph_SS_1)
  print(annot_SS_1_var)
  print("Variance is not equal between groups")
  
}

annot_SS_1_Valmean <- wilcox.test(valuesmean ~ Condition, data = graph_SS_1,
                                  paired = FALSE, alternative = "two.sided")

annot_SS_1_Valmean <- tidy(annot_SS_1_Valmean)

##- 2 timepoint

graph_SS_asyn_2 <- graph[graph$Timepoint == 2 & 
                           graph$Variable == "X.Similarity" &
                           graph$Condition == "asyn", ]

graph_SS_con_2 <- graph[graph$Timepoint == 2 & 
                          graph$Variable == "X.Similarity" &
                          graph$Condition == "con", ]

#- asyn

ggqqplot(data = graph_SS_asyn_2, x = "valuesmean")

ggdensity(data = graph_SS_asyn_2, x = "valuesmean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_SS_asyn_2_norm <- shapiro.test(x = graph_SS_asyn_2$valuesmean)

if(annot_SS_asyn_2_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = graph_SS_con_2, x = "valuesmean")

ggdensity(data = graph_SS_con_2, x = "valuesmean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_SS_con_2_norm <- shapiro.test(x = graph_SS_con_2$valuesmean)

if(annot_SS_con_2_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

graph_SS_2 <- graph[graph$Timepoint == 2 & graph$Variable == "X.Similarity", ]

if(annot_SS_con_2_norm$p.value & annot_SS_asyn_2_norm > 0.05){
  
  annot_SS_2_var <- bartlett.test(valuesmean ~ Condition, 
                                  data = graph_SS_2)
  print(annot_SS_2_var)
  print("Variance is equal between groups")
  
} else if(annot_SS_con_2_norm$p.value | annot_SS_con_2_norm < 0.05){
  
  annot_SS_2_var <- leveneTest(valuesmean ~ Condition, 
                               data = graph_SS_2)
  print(annot_SS_2_var)
  print("Variance is not equal between groups")
  
}

annot_SS_2_Valmean <- wilcox.test(valuesmean ~ Condition, data = graph_SS_2,
                                  paired = FALSE, alternative = "two.sided")

annot_SS_2_Valmean <- tidy(annot_SS_2_Valmean)


##-  3 timepoint

graph_SS_asyn_3 <- graph[graph$Timepoint == 3 & 
                           graph$Variable == "X.Similarity" &
                           graph$Condition == "asyn", ]

graph_SS_con_3 <- graph[graph$Timepoint == 3 & 
                          graph$Variable == "X.Similarity" &
                          graph$Condition == "con", ]

#- asyn

ggqqplot(data = graph_SS_asyn_3, x = "valuesmean")

ggdensity(data = graph_SS_asyn_3, x = "valuesmean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_SS_asyn_3_norm <- shapiro.test(x = graph_SS_asyn_3$valuesmean)

if(annot_SS_asyn_3_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = graph_SS_con_3, x = "valuesmean")

ggdensity(data = graph_SS_con_3, x = "valuesmean",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_SS_con_3_norm <- shapiro.test(x = graph_SS_con_3$valuesmean)

if(annot_SS_con_3_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

graph_SS_3 <- graph[graph$Timepoint == 3 & graph$Variable == "X.Similarity", ]

if(annot_SS_con_3_norm$p.value & annot_SS_asyn_3_norm > 0.05){
  
  annot_SS_3_var <- bartlett.test(valuesmean ~ Condition, 
                                  data = graph_SS_3)
  print(annot_SS_3_var)
  print("Variance is equal between groups")
  
} else if(annot_SS_con_3_norm$p.value | annot_SS_con_3_norm < 0.05){
  
  annot_SS_3_var <- leveneTest(valuesmean ~ Condition, 
                               data = graph_SS_3)
  print(annot_SS_3_var)
  print("Variance is not equal between groups")
  
}

annot_SS_3_Valmean <- wilcox.test(valuesmean ~ Condition, data = graph_SS_3,
                                  paired = FALSE, alternative = "two.sided")

annot_SS_3_Valmean <- tidy(annot_SS_3_Valmean)

###- Create Dataframe of P-values for each comparison

annot_SS_Valmean_Names <- c("annot_SS_0_Valmean","annot_SS_1_Valmean",
                            "annot_SS_2_Valmean","annot_SS_3_Valmean")

annot_SS_Valmean_List <- list(annot_SS_0_Valmean,annot_SS_1_Valmean,
                              annot_SS_2_Valmean,annot_SS_3_Valmean)

names(annot_SS_Valmean_List) <- annot_SS_Valmean_Names

annot_SS_Valmean <- bind_rows(annot_SS_Valmean_List,
                              .id = "Information")

### Wilcoxon Rank Sum Test on Normalized CV X.Similarity scores ###
### by timepoint and comparing conditions ###


##- 0 Timepoint 

graph_SS_asyn_0 <- graph[graph$Timepoint == 0 & 
                           graph$Variable == "X.Similarity" &
                           graph$Condition == "asyn", ]

graph_SS_con_0 <- graph[graph$Timepoint == 0 & 
                          graph$Variable == "X.Similarity" &
                          graph$Condition == "con", ]

#- asyn

ggqqplot(data = graph_SS_asyn_0, x = "valuesCV")

ggdensity(data = graph_SS_asyn_0, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_SS_asyn_0_norm <- shapiro.test(x = graph_SS_asyn_0$valuesCV)

if(annot_SS_asyn_0_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = graph_SS_con_0, x = "valuesCV")

ggdensity(data = graph_SS_con_0, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_SS_con_0_norm <- shapiro.test(x = graph_SS_con_0$valuesCV)

if(annot_SS_con_0_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

graph_SS_1 <- graph[graph$Timepoint == 1 & graph$Variable == "X.Similarity", ]

if(annot_SS_con_0_norm$p.value & annot_SS_asyn_0_norm > 0.05){
  
  annot_SS_0_var <- bartlett.test(valuesCV ~ Condition, 
                                  data = graph_SS_0)
  print(annot_SS_0_var)
  print("Variance is equal between groups")
  
} else if(annot_SS_con_0_norm$p.value | annot_SS_con_0_norm < 0.05){
  
  annot_SS_0_var <- leveneTest(valuesCV ~ Condition, 
                               data = graph_SS_0)
  print(annot_SS_0_var)
  print("Variance is not equal between groups")
  
}

annot_SS_0_ValCV <- wilcox.test(valuesCV ~ Condition, data = graph_SS_0,
                                paired = FALSE, alternative = "two.sided")

annot_SS_0_ValCV <- tidy(annot_SS_0_ValCV)

##- 1 timepoint

graph_SS_asyn_1 <- graph[graph$Timepoint == 1 & 
                           graph$Variable == "X.Similarity" &
                           graph$Condition == "asyn", ]

graph_SS_con_1 <- graph[graph$Timepoint == 1 & 
                          graph$Variable == "X.Similarity" &
                          graph$Condition == "con", ]

#- asyn

ggqqplot(data = graph_SS_asyn_1, x = "valuesCV")

ggdensity(data = graph_SS_asyn_1, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_SS_asyn_1_norm <- shapiro.test(x = graph_SS_asyn_1$valuesCV)

if(annot_SS_asyn_1_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = graph_SS_con_1, x = "valuesCV")

ggdensity(data = graph_SS_con_1, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_SS_con_1_norm <- shapiro.test(x = graph_SS_con_1$valuesCV)

if(annot_SS_con_1_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

graph_SS_1 <- graph[graph$Timepoint == 1 & graph$Variable == "X.Similarity", ]

if(annot_SS_con_1_norm$p.value & annot_SS_asyn_1_norm > 0.05){
  
  annot_SS_1_var <- bartlett.test(valuesCV ~ Condition, 
                                  data = graph_SS_1)
  print(annot_SS_1_var)
  print("Variance is equal between groups")
  
} else if(annot_SS_con_1_norm$p.value | annot_SS_con_1_norm < 0.05){
  
  annot_SS_1_var <- leveneTest(valuesCV ~ Condition, 
                               data = graph_SS_1)
  print(annot_SS_1_var)
  print("Variance is not equal between groups")
  
}

annot_SS_1_ValCV <- wilcox.test(valuesCV ~ Condition, data = graph_SS_1,
                                paired = FALSE, alternative = "two.sided")

annot_SS_1_ValCV <- tidy(annot_SS_1_ValCV)

##- 2 timepoint

graph_SS_asyn_2 <- graph[graph$Timepoint == 2 & 
                           graph$Variable == "X.Similarity" &
                           graph$Condition == "asyn", ]

graph_SS_con_2 <- graph[graph$Timepoint == 2 & 
                          graph$Variable == "X.Similarity" &
                          graph$Condition == "con", ]

#- asyn

ggqqplot(data = graph_SS_asyn_2, x = "valuesCV")

ggdensity(data = graph_SS_asyn_2, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_SS_asyn_2_norm <- shapiro.test(x = graph_SS_asyn_2$valuesCV)

if(annot_SS_asyn_2_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = graph_SS_con_2, x = "valuesCV")

ggdensity(data = graph_SS_con_2, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_SS_con_2_norm <- shapiro.test(x = graph_SS_con_2$valuesCV)

if(annot_SS_con_2_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

graph_SS_2 <- graph[graph$Timepoint == 2 & graph$Variable == "X.Similarity", ]

if(annot_SS_con_2_norm$p.value & annot_SS_asyn_2_norm > 0.05){
  
  annot_SS_2_var <- bartlett.test(valuesCV ~ Condition, 
                                  data = graph_SS_2)
  print(annot_SS_2_var)
  print("Variance is equal between groups")
  
} else if(annot_SS_con_2_norm$p.value | annot_SS_con_2_norm < 0.05){
  
  annot_SS_2_var <- leveneTest(valuesCV ~ Condition, 
                               data = graph_SS_2)
  print(annot_SS_2_var)
  print("Variance is not equal between groups")
  
}

annot_SS_2_ValCV <- wilcox.test(valuesCV ~ Condition, data = graph_SS_2,
                                paired = FALSE, alternative = "two.sided")

annot_SS_2_ValCV <- tidy(annot_SS_2_ValCV)


##-  3 timepoint

graph_SS_asyn_3 <- graph[graph$Timepoint == 3 & 
                           graph$Variable == "X.Similarity" &
                           graph$Condition == "asyn", ]

graph_SS_con_3 <- graph[graph$Timepoint == 3 & 
                          graph$Variable == "X.Similarity" &
                          graph$Condition == "con", ]

#- asyn

ggqqplot(data = graph_SS_asyn_3, x = "valuesCV")

ggdensity(data = graph_SS_asyn_3, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_SS_asyn_3_norm <- shapiro.test(x = graph_SS_asyn_3$valuesCV)

if(annot_SS_asyn_3_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

#- con

ggqqplot(data = graph_SS_con_3, x = "valuesCV")

ggdensity(data = graph_SS_con_3, x = "valuesCV",
          fill = "lightgray", add = "mean", rug = TRUE)

annot_SS_con_3_norm <- shapiro.test(x = graph_SS_con_3$valuesCV)

if(annot_SS_con_3_norm$p.value > 0.05){
  print("Data is normally distributed")} else {
    print("Data is not normally distributed")
  }

##- Hypothesis testing for normal distribution, 
##- equal variance and statistical significance 

graph_SS_3 <- graph[graph$Timepoint == 3 & graph$Variable == "X.Similarity", ]

if(annot_SS_con_3_norm$p.value & annot_SS_asyn_3_norm > 0.05){
  
  annot_SS_3_var <- bartlett.test(valuesCV ~ Condition, 
                                  data = graph_SS_3)
  print(annot_SS_3_var)
  print("Variance is equal between groups")
  
} else if(annot_SS_con_3_norm$p.value | annot_SS_con_3_norm < 0.05){
  
  annot_SS_3_var <- leveneTest(valuesCV ~ Condition, 
                               data = graph_SS_3)
  print(annot_SS_3_var)
  print("Variance is not equal between groups")
  
}

annot_SS_3_ValCV <- wilcox.test(valuesCV ~ Condition, data = graph_SS_3,
                                paired = FALSE, alternative = "two.sided")

annot_SS_3_ValCV <- tidy(annot_SS_3_ValCV)

###- Create Dataframe of P-values for each comparison

annot_SS_ValCV_Names <- c("annot_SS_0_ValCV","annot_SS_1_ValCV",
                          "annot_SS_2_ValCV","annot_SS_3_ValCV")

annot_SS_ValCV_List <- list(annot_SS_0_ValCV,annot_SS_1_ValCV,
                            annot_SS_2_ValCV,annot_SS_3_ValCV)

names(annot_SS_ValCV_List) <- annot_SS_ValCV_Names

annot_SS_ValCV <- bind_rows(annot_SS_ValCV_List,
                            .id = "Information")

#### Bind Accuracy and Similarity Score DataFrames Together ####
#### Export file for graphing ####

SB_Stat_List <- list(annot_AS_ValCV,annot_AS_Valmean,
                     annot_SS_ValCV,annot_SS_Valmean)

SB_MasterStatisticsTable <- bind_rows(SB_Stat_List)

write.csv(SB_MasterStatisticsTable, 
          file = "~/Documents/Julie_Lab/Data/SB/Conditions/UD_SB_MasterStatisticsTable.csv", 
          row.names = FALSE)


####- Create DF containing annotations (i.e.,Pvalues) 
####- that will be mapped onto graph

Variables_list <- unique(graph$Variable)

Condition_list <- unique(graph$Condition)

Timepoint_list <- unique(graph$Timepoint)

Scores_list <- c("valuesmean","valuesCV")

annotation_graph <- cross_df(list(Variable = Variables_list, Scores = Scores_list, 
                                  x_position = Timepoint_list)) %>%
  mutate(annotations = apply(combn(letters, 2), 2, paste0, collapse = "")[1:n()]) %>%
  group_by(Variable) %>%
  arrange(Variable, x_position) %>%
  #filter(Variable != "var.pitch") %>%
  mutate(y_position = 1 + .2 * row_number())

annotation_graph <- annotation_graph %>%
  arrange(x_position, Scores)

names(annotation_graph) [4] <- "p.value"

###- Add custom annotations to faceted plot
###- Use if few comparisons are being run

#annotation_graph <- data.frame(color = c("Var.AM","var.entropy","var.FM","var.mean.freq","var.pitch.goodness"),
#                         start = rep(0,) ,
#                        end = c(1,2,3),
#                       y = c(),
#                      label = c())


###- Creat DF containing statistical information (e.g., test and pvalue)
###- in TIDY format
###- Filter DF by timepoint to reduce levels 

Variables_list <- unique(graph$Variable)

annotation_table <-  graph %>%
  select(valuesmean, valuesCV,Variable, Timepoint, Condition) %>% 
  gather(key = score, value = value, - c(Variable,Timepoint,Condition)) %>%
  group_by(Variable, Timepoint, Condition, score) %>%
  summarise(value = list(value)) %>%
  #filter(Variable != "var.pitch") %>%
  spread(Condition, value) %>%
  group_by(Variable, Timepoint, score) %>%
  mutate(p_value = wilcox.test(unlist(asyn), unlist(con), paired = FALSE)$p.value,
         t_value = wilcox.test(unlist(asyn), unlist(con), paired = FALSE)$statistic) 

annotation_table <- annotation_table %>%
  arrange(Timepoint, score)

###- Transfer p.value to annotation_graph
###- from annotation_table then save DF

annotation_graph[,"p.value"] <- annotation_table[, "p_value"]

write.csv(annotation_graph, 
          file = "~/Documents/Julie_Lab/Data/SB/Conditions/SB_UD_MasterStatisticsTable.csv", 
          row.names = FALSE)
