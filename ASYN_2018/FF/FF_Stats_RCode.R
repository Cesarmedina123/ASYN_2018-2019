###### Hypothesis Testing for FF Scores #####

#- subset by condition and timepoint since its all pitch data
#- to run comparisons using ggpubr and ggsignif

###### Tests if data follows normal distribution, ######
###### equal variance, and if its significant ######

library(readr)
library(tidyverse)
library(plyr)
library(dplyr)
library(ggpubr)
library(svglite)
library(car)
library(broom)

#### similarity Batch Scores of Intersyllable-interrendition ####

graph <- read.table("~/Documents/Julie_Lab/Data/FF/Conditions/FF_UD_MasterSummarytable.csv",
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

####- Post/Pre and Modulation Index comparisons 
####- b/w groups and by timepoint
####- Create DF containing annotations (i.e.,Pvalues) 
####- that will be mapped onto graph

###- MeanPitchAll and Median Pitch All

df <- read.table("~/Documents/Julie_Lab/Data/FF/Conditions/FF_UD_MedianPitchAllTable.csv",
                    header = TRUE, sep = ",")

Variables_list <- unique(df$Variable)

Condition_list <- unique(df$Condition)

Timepoint_list <- unique(df$Timepoint)

Scores_list <- c("normMean","normMedian","normCV","modulationindex.mean",
                 "modulationindex.median","modulationindex.CV")

##- Call plyr first, then dplyr, then tidverse

annotation_df <- cross_df(list(Variable = Variables_list, Scores = Scores_list, 
                                  Timepoint = Timepoint_list)) %>%
  mutate(annotations = apply(combn(letters, 2), 2, paste0, collapse = "")[1:n()]) %>%
  group_by(Variable) %>%
  arrange(Variable, Timepoint) %>%
  mutate(y_position = 1 + .2 * row_number())

annotation_df <- annotation_df %>%
  arrange(Timepoint, Scores)

names(annotation_df) [4] <- "p.value"

##- Creat DF containing statistical information (e.g., test and pvalue)
##- in TIDY format
##- Filter DF by timepoint to reduce levels 

annotation_table <-  df %>%
  select(normMean, normMedian, normCV, modulationindex.mean, modulationindex.median,
         modulationindex.CV, Variable, Timepoint, Condition) %>% 
  gather(key = score, value = value, - c(Variable,Timepoint,Condition)) %>%
  group_by(Variable, Timepoint, Condition, score) %>%
  summarise(value = list(value)) %>%
  spread(Condition, value) %>%
  group_by(Variable, Timepoint, score) %>%
  mutate(p_value = wilcox.test(unlist(asyn), unlist(con), paired = FALSE)$p.value,
         t_value = wilcox.test(unlist(asyn), unlist(con), paired = FALSE)$statistic) 

annotation_table <- annotation_table %>%
  arrange(Timepoint, score)

##- Transfer p.value to annotation_graph
##- from annotation_table then save DF

annotation_df[,"p.value"] <- annotation_table[, "p_value"]

write.csv(annotation_df, 
          file = "~/Documents/Julie_Lab/Data/FF/Conditions/FF_UD_MedianPitchAllStatisticsTable.csv", 
          row.names = FALSE)

###- Intrasyllable-Intrarendition scores for CV and MAD

df <- read.table("~/Documents/Julie_Lab/Data/FF/Conditions/FF_UD_IntraRenditionScoresTable.csv",
                 header = TRUE, sep = ",")

Variables_list <- unique(df$Variable)

Condition_list <- unique(df$Condition)

Timepoint_list <- unique(df$Timepoint)

Scores_list <- c("normmeanCV","normmedianCV","normcvCV","modulationindex.meanCV",
                 "modulationindex.medianCV","modulationindex.cvCV","normmeanMAD",
                 "normmedianMAD","normcvMAD","modulationindex.meanMAD",
                 "modulationindex.medianMAD","modulationindex.cvMAD")

##- Call plyr first, then dplyr, then tidverse

annotation_df <- cross_df(list(Variable = Variables_list, Scores = Scores_list, 
                               Timepoint = Timepoint_list)) %>%
  mutate(annotations = apply(combn(letters, 2), 2, paste0, collapse = "")[1:n()]) %>%
  group_by(Variable) %>%
  arrange(Variable, Timepoint) %>%
  mutate(y_position = 1 + .2 * row_number())

annotation_df <- annotation_df %>%
  arrange(Timepoint, Scores)

names(annotation_df) [4] <- "p.value"

##- Creat DF containing statistical information (e.g., test and pvalue)
##- in TIDY format
##- Filter DF by timepoint to reduce levels 

annotation_table <-  df %>%
  select(normmeanCV,normmedianCV,normcvCV,modulationindex.meanCV,
         modulationindex.medianCV,modulationindex.cvCV,normmeanMAD,
         normmedianMAD,normcvMAD,modulationindex.meanMAD,
         modulationindex.medianMAD,modulationindex.cvMAD, 
         Variable, Timepoint, Condition) %>% 
  gather(key = score, value = value, - c(Variable,Timepoint,Condition)) %>%
  group_by(Variable, Timepoint, Condition, score) %>%
  summarise(value = list(value)) %>%
  spread(Condition, value) %>%
  group_by(Variable, Timepoint, score) %>%
  mutate(p_value = wilcox.test(unlist(asyn), unlist(con), paired = FALSE)$p.value,
         t_value = wilcox.test(unlist(asyn), unlist(con), paired = FALSE)$statistic) 

annotation_table <- annotation_table %>%
  arrange(Timepoint, score)

##- Transfer p.value to annotation_graph
##- from annotation_table then save DF

annotation_df[,"p.value"] <- annotation_table[, "p_value"]

write.csv(annotation_df, 
          file = "~/Documents/Julie_Lab/Data/FF/Conditions/FF_UD_IntraRenditionStatisticsTable.csv", 
          row.names = FALSE)
