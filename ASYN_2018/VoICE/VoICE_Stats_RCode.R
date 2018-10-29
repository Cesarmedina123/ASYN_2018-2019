######- Install.packages for statistics if needed

install.packages("resample")
install.packages("rsample")
install.packages("pwr")
install.packages("effsize")
install.packages("powerAnalysis")

######- Hypothesis Testing of Individual Acoustic Features
######- subset by variable, ondition and timepoint
######- to run comparisons using ggpubr or ggsignif

library(plyr)
library(readr)
library(tidyverse)
library(purrr)
library(ggpubr)
library(svglite)
library(car)
library(broom)
library(data.table)
library(doBy)
library(rsample)
library(resample)
library(pwr)
library(effsize)
library(powerAnalysis)

mainDir <- "C:/Users/cesar/Documents/Julie_Lab/Data/ASYN_2018/VoICE/Conditions"

df <- read.table(file = paste0(mainDir,"/VoICE_UD_Table_Birds_2.csv"), 
                   header = TRUE, sep = ",")

df <- df[df$Variable != "var.pitch", ]

df <- df[complete.cases(df), ]

#####- Shapiro Test to test if distribution is normal
#####- Already grouped by condition

VoICE_UD_ShapiroTest <- df[!duplicated(df[df$Values]) & df$Score != c("ZScoreMean","ZScoreCV"),] %>%
  group_by(Variable, Score, Condition, Timepoint)  %>% 
  do(tidy(shapiro.test(.$Values))) %>%
  ungroup() %>% 
  select(-method)

write.csv(VoICE_UD_ShapiroTest, file = paste0(mainDir,"/VoICE_UD_ShapiroTest.csv"), row.names = FALSE)

######- Between group comparisons

#####- F Test to test if variances are equal between asyn and con

VoICE_UD_BG_FTest <- df %>%
  group_by(Variable, Score, Timepoint)  %>% 
  do(tidy(var.test(.$Values ~ .$Condition, alternative = "two.sided", ratio = 1, conf.level = .95))) %>%
  ungroup() %>% 
  select(-method)

write.csv(VoICE_UD_BG_FTest, file = paste0(mainDir,"/VoICE_UD_BG_FTest.csv"), row.names = FALSE)

#####- Wilcoxon Rank Sum Test to test if groups are statistically different

VoICE_UD_BG_WilcoxRankSumTest <- df %>%
  group_by(Variable, Score, Timepoint)  %>% 
  do(tidy(wilcox.test(.$Values ~ .$Condition, alternative = "two.sided", paired = FALSE, conf.level = .95))) %>%
  ungroup() %>% 
  select(-method)

write.csv(VoICE_UD_BG_WilcoxRankSumTest, file = paste0(mainDir,"/VoICE_UD_BG_WilcoxRankSumTest.csv"), row.names = FALSE)

#####- T Test with equal variance to test if groups are statistically different

VoICE_UD_BG_TEqualVarTest <- df[df$Values != 1,] %>%
  group_by(Variable, Score, Timepoint)  %>% 
  do(tidy(t.test(.$Values ~ .$Condition, alternative = "two.sided", 
                 paired = FALSE, conf.level = .95, var.equal = TRUE))) %>%
  ungroup() %>% 
  select(-method)

write.csv(VoICE_UD_BG_TEqualVarTest, file = paste0(mainDir,"/VoICE_UD_BG_TEqualVarTest.csv"), row.names = FALSE)

#####- T Test with unequal variance to test if groups are statistically different

VoICE_UD_TUnequalVarTest <- df[df$Values != 1,] %>%
  group_by(Variable, Score, Timepoint)  %>% 
  do(tidy(t.test(.$Values ~ .$Condition, alternative = "two.sided", 
                 paired = FALSE, conf.level = .95, var.equal = FALSE))) %>%
  ungroup() %>% 
  select(-method)

write.csv(VoICE_UD_TUnequalVarTest, file = paste0(mainDir,"/VoICE_UD_TUneualVarTest.csv"), row.names = FALSE)

######- Within group comparisons

#####- Welch test

my_comparisons <- list((0,1),(0,2),(0,3))

VoICE_UD_WG_WelchTest <- list()

for(i in my_comparisons) {
    
    df  <- df[df$Timepoint %in% my_comparisons[[i]], ]
    
    WelchTest <- df %>%
      group_by(Variable, Score, Condition)  %>% 
      do(tidy(t.test(.$Values ~ .$Timepoint, alternative = "two.sided", 
                     paired = TRUE, conf.level = .95, var.equal = FALSE))) %>%
      ungroup() %>% 
      select(-method)
    
    VoICE_UD_WG_WelchTest [[i]] <- WelchTest
    
}

comparisons <- unique(df$Timepoint)

a <- df[df$Timepoint %in% c(comparisons[1],comparisons[2]), ] %>%
  group_by(Variable, Score, Condition)  %>% 
  do(tidy(t.test(.$Values ~ .$Timepoint, alternative = "two.sided", 
                 paired = TRUE, conf.level = .95, var.equal = FALSE))) %>%
  ungroup() %>% 
  select(-method)

a$Timepoint <- 1

b <- df[df$Timepoint %in% c(comparisons[1],comparisons[3]), ] %>%
  group_by(Variable, Score, Condition)  %>% 
  do(tidy(t.test(.$Values ~ .$Timepoint, alternative = "two.sided", 
                 paired = TRUE, conf.level = .95, var.equal = FALSE))) %>%
  ungroup() %>% 
  select(-method)

b$Timepoint <- 2


c <- df[df$Timepoint %in% c(comparisons[1],comparisons[4]), ] %>%
  group_by(Variable, Score, Condition)  %>% 
  do(tidy(t.test(.$Values ~ .$Timepoint, alternative = "two.sided", 
                 paired = TRUE, conf.level = .95, var.equal = FALSE))) %>%
  ungroup() %>% 
  select(-method)

c$Timepoint <- 3

VoICE_UD_WG_WelchTest <- bind_rows(a,b,c)

TestScore <- c("ZScoreMean","ZScoreCV")
  
VoICE_UD_WG_WelchTest <- VoICE_UD_WG_WelchTest[VoICE_UD_WG_WelchTest$Score %in% TestScore,]

write.csv(VoICE_UD_WG_WelchTest, file = paste0(mainDir,"/VoICE_UD_WG_WelchTest.csv"), row.names = FALSE)


VoICE_UD_WG_WelchTest_P <- VoICE_UD_WG_WelchTest[VoICE_UD_WG_WelchTest$p.value < .05, ]

#####- Resampling Statistics Comparison

####- Bootstrap

VoICE_UD_Bootstrap <- df[df$Values != 1,] %>%
  group_by(Variable, Score, Timepoint)  %>% 
  do(tidy(bootstrap(.$Values ~ .$Condition, alternative = "two.sided", 
                 paired = FALSE, conf.level = .95, var.equal = FALSE))) %>%
  ungroup() %>% 
  select(-method)

write.csv(VoICE_UD_TUnequalVarTest, file = paste0(mainDir,"/VoICE_UD_TUneualVarTest.csv"), row.names = FALSE)

#####- Check occurances for eacg observation

occ <- df %>% 
  group_by(Variable, Condition, Timepoint, Score) %>%
  summarise(no_rows = length(Score))

####- Statistics function to loop over two columns 
####- with X levels and generate a tidy dataframe 
####- made-up of appropriate statistical comparisons 
####- (e.g., wilcoxon rank sum test, unpaired t-test)
####- depending on homogeneity (shapiro-wilks test)
####- and variance of data (bartlett/levene test)
####- Hedges d for effect size

df <- read.table(file = paste0(mainDir,"/VoICE_UD_Table_Birds_2.csv"), 
                 header = TRUE, sep = ",")

Statistics <- function(data, variable, timepoint, score, 
                       condition, condition1, condition2 ) { ## Subset data (e.g., data = data[data$Condition = "asyn", ])
  ## for within group to only asyn or con
  data <- as.data.frame(data)
  variable_name <- distinct(data[ ,variable])
  timepoint_name <- distinct(data[ ,timepoint])
  score_name <- distinct(data[ ,score])
  condition1_name <- data[data$condition = condition1, ]
  condition2_name <- data[data$condition = condition2, ]
  
  StatsFile1 <- list()
  StatsFile2 <- list()
  StatsFile3 <- list()
  
  for(i in seq_along(variable)) {
    
    for(j in seq_along(timepoint) {
      
      a <- as.data.frame(data[data[ ,variable] == variable_name[i], ])
      
      b <- a[a[ ,timepoint] == timepoint_name [j], ]
      
      for(k in 1: length(score)) {
        
        c <- b[b[, score] == score_name[k], ]
        
        paste0(condition1,"_stest") <- shapiro.test(x = data[data[ ,score] == score_name[k], ]))
      
      paste0(condition2,"_stest") <- shapiro.test()
    
    paste0(condition1,"_stest") <- tidy(paste0(condition1,"_stest"))
    
    paste0(condition1_)
    
    if()
      
      }
      
      StatsFile2 [[i]] <- StatsFile1
      
    }
    
  }
  
}

###- Create DF containing annotations (i.e.,Pvalues) 
###- that will be mapped onto graph

Variables_list <- unique(graph$Variable)

Condition_list <- unique(graph$Condition)

Timepoint_list <- unique(graph$Timepoint)

Scores_list <- c("modulationindex.mean","modulationindex.CV")

annotation_graph <- cross_df(list(Variable = Variables_list, Scores = Scores_list, 
                                  x_position = Timepoint_list)) %>%
  mutate(annotations = apply(combn(letters, 2), 2, paste0, collapse = "")[1:n()]) %>%
  group_by(Variable) %>%
  arrange(Variable, x_position) %>%
  filter(Variable != "var.pitch") %>%
  mutate(y_position = 1 + .2 * row_number())

annotation_graph <- annotation_graph %>%
  arrange(x_position, Scores)

names(annotation_graph) [4] <- "p.value"

##- Add custom annotations to faceted plot
##- Use if few comparisons are being run

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
  filter(Variable != "var.pitch") %>%
  spread(Condition, value) %>%
  group_by(Variable, Timepoint, score) %>%
  mutate(p_value = wilcox.test(unlist(asyn), unlist(con), paired = FALSE)$p.value,
         t_value = wilcox.test(unlist(asyn), unlist(con), paired = FALSE)$statistic) 

annotation_table <- annotation_table %>%
  arrange(Timepoint, score)

annotation_graph[,"p.value"] <- annotation_table[, "p_value"]

write.csv(annotation_graph, 
          file = "~/Documents/Julie_Lab/Data/VoICE/Conditions/VoICE_UD_MasterStatisticsTable.csv", 
          row.names = FALSE)
