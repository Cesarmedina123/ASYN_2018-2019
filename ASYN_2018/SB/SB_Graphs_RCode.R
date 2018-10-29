
########## Graph Accuracy and Similarity Score Values ############

#- subset by variable
#- subset further by condition and timepoint
#- to run comparisons using ggpubr or ggsignif

######### Graph using GGPUBR ##### BETTER #####

library(readr)
library(tidyverse)
library(ggsignif)
library(svglite)
library(ggpubr)

# Absolute Scores of Intersyllable-interrendition (Wilcoxon Rank Sum Test)

maindir <- "C:/Users/cesar/Documents/Julie_Lab/Data/SB"

graph <- read.table("~/Documents/Julie_Lab/Data/SB/Conditions/SB_UD_MasterSummaryTable.csv",
                    header = TRUE, sep = ",")

annotation_graph <- read.table("~/Documents/Julie_Lab/Data/SB/Conditions/SB_UD_MasterStatisticsTable.csv",
                               header = TRUE, sep = ",")

selectedfeatures <- c("Accuracy","X.Similarity")

graph <- graph[graph$BirdID.Syll != "Bk286.A", ] # Similarity batch awful (so removed)
graph <- graph[graph$BirdID.Syll != "Wh214.F", ] # Similarity batch awful (so removed)
#graph <- graph[graph$Variable %in% selectedfeatures, ]
graph <- graph[graph$Harmonic == "yes",]

tickdecimal_1 <- function(x) sprintf("%.1f", x)
tickdecimal_2 <- function(x) sprintf("%.2f", x)

# Wilcoxon Rank Sum Test on Normalized CV and Mean scores 
# by timepoint and comparing conditions

plot1 <- ggline(data = graph[graph$Variable %in% selectedfeatures,],
                x = "Timepoint", y = "valuesmean", 
                add = "mean_se",
                color = "Condition", fill = "Condition", shape = "Condition",
                position = position_dodge(0.8),
                facet.by = "Variable",
                ylab = paste("Mean"), point.size = .5) +
  scale_y_continuous(labels = tickdecimal_2) +
  scale_shape_manual(values = c(15,17)) +
  scale_color_manual(values =c("orangered2","royalblue3")) +
  scale_fill_manual(values = c("lightpink1","lightskyblue")) +
  facet_wrap(~ Variable, strip.position = "bottom", nrow = 1) +
  theme_classic2() +
  theme(strip.background = element_blank(), strip.placement = "outside", 
        strip.text = element_blank(), axis.ticks = element_blank(), axis.line.x = 
          element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
        plot.title = element_text("Normalized Values")) 


plot2 <- ggline(data = graph[graph$Variable %in% selectedfeatures,],
                x = "Timepoint", y = "valuesCV", 
                add = "mean_se",
                color = "Condition", fill = "Condition", shape = "Condition",
                position = position_dodge(0.8),
                facet.by = "Variable",
                ylab = paste("CV"), point.size = .5) +
  scale_y_continuous(labels = tickdecimal_2) +
  scale_shape_manual(values = c(15,17)) +  
  scale_color_manual(values =c("orangered2","royalblue3")) +
  scale_fill_manual(values = c("lightpink1","lightskyblue")) +
  facet_wrap(~ Variable, strip.position = "bottom", nrow = 1) +
  theme_classic2() +
  theme(strip.background = element_blank(), strip.placement = "outside") 

multiplot <- ggarrange(plot1, plot2, nrow = 2)

print(multiplot)

ggsave(multiplot, file = paste0(maindir,"/","Conditions/SB.UD.SelectedFeatures.NormalizedScores.MasterFileScores",
                                  ".emf"), scale = 2)

# Normalized Var Scores for Intrasyllable-interrendition 
# by Timepoint and Condition

plot3 <- ggline(data = graph, x = "Timepoint", y = "valuesvar", 
                add = "mean_se",
                color = "Condition", fill = "Condition", shape = "Condition",
                position = position_dodge(0.8),
                facet.by = "Variable",
                ylab = paste("Normalized CV Values (Post/Pre)"), point.size = .5) +
  scale_y_continuous(labels = tickdecimal_2) +
  scale_shape_manual(values = c(15,17)) +
  scale_color_manual(values =c("orangered2","royalblue3")) +
  scale_fill_manual(values = c("lightpink1","lightskyblue")) +
  scale_fill_manual(values = c("black","grey56")) +
  facet_wrap(~ Variable, strip.position = "bottom", nrow = 1) +
  theme_classic2() +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  geom_signif(comparisons = list(c("asyn","gfp")),
              test = 'wilcox.test', 
              test.args = list(alternative = "two.sided",
                               var.equal = FALSE, paired = FALSE), 
              map_signif_level = TRUE,
              step_increase = -2.5, textsize = 3.25) 

print(plot3)

ggsave(plot3, file = paste0(maindir,"/","Conditions/SB.UD.AllFeatures.NormalizedVar.MasterFileScores",
                                ".emf"), scale = 2)
