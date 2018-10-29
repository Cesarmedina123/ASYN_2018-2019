
########## Graph FF Scores from Brainard Day Program ############

#- subset by condition and timepoint since its all pitch data
#- to run comparisons using ggpubr or ggsignif

######### Graph using GGPUBR ##### BETTER #####

library(tidyverse)
library(ggsignif)
library(svglite)
library(ggpubr)
library(readr)

####- Absolute Scores of Intersyllable-interrendition 
####- (Wilcoxon Rank Sum Test)

maindir <- "C:/Users/cesar/Documents/Julie_Lab/Data/FF"

graph <- read.table(file = paste0(maindir,"/Conditions/FF_UD_IntraRenditionScoresTable.csv"), 
                    header = TRUE, sep = ",")

annotation_graph <- read.table(file = paste0(maindir,"/Conditions/FF_UD_MeanPitchAllStatisticsTable.csv"), 
                               header = TRUE, sep = ",")

tickdecimal_1 <- function(x) sprintf("%.1f", x)
tickdecimal_2 <- function(x) sprintf("%.2f", x)

selectedfeature <- "Pitch"

row_sub <- apply(graph, 1, function(row) all(row != 0 ))
graph <- graph[row_sub,]
graph <- graph[graph$Bird.Syll != "Bk286.A" && 
                 graph$Bird.Syll != "Wh214.F", ] # Similarity batch awful (so removed)
graph <- graph[graph$Variable == selectedfeature, ]
# graph <- graph[graph$Timepoint != 1,] # filter by timepoint with example of removing 1 month

###- Lineplot using GGPUBR

##- Mean (normalized or modulation index)

plot1 <- ggline(data = graph[graph$Variable %in% selectedfeature,],
                x = "Timepoint", y = "normmeanCV", 
                add = "mean_se",
                color = "Condition", fill = "Condition", shape = "Condition",
                position = position_dodge(0.8),
                facet.by = "Variable",
                ylab = paste("Mean (Post/Pre)"), point.size = .5) +
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

##- CV (normalized or modulation index)

plot2 <- ggline(data = graph[graph$Variable %in% selectedfeature,],
                x = "Timepoint", y = "normcvCV", 
                add = "mean_se",
                color = "Condition", fill = "Condition", shape = "Condition",
                position = position_dodge(0.8),
                facet.by = "Variable",
                ylab = paste("CV (Post/Pre)"), point.size = .5) +
  scale_y_continuous(labels = tickdecimal_2) +
  scale_shape_manual(values = c(15,17)) +  
  scale_color_manual(values =c("orangered2","royalblue3")) +
  scale_fill_manual(values = c("lightpink1","lightskyblue")) +
  facet_wrap(~ Variable, strip.position = "bottom", nrow = 1) +
  theme_classic2() +
  theme(strip.background = element_blank(), strip.placement = "outside") 

##- Combine both plots

multiplot <- ggarrange(plot1, plot2, nrow = 2)

print(multiplot)

##- Save plot 

ggsave(multiplot, file = paste0(maindir,"/Conditions/FF.UD.NormalizedScores.IntraRenditionCV",
                                ".emf"), scale = 2)

###- Barplot using GGPUBR

#- Redifine limit so direction of errorbar is condition on sign of mean

limits <- aes(
  ymax = graphN$mean + (graph$mean > 0)*graph$se,  
  ymin = graph$mean - (graph$mean < 0)*graph$se)

##- Mean (normalized or modulation index)

plot1 <- ggbarplot(data = graph,
                   x = "Timepoint", y = "modulationindex.meanCV", 
                   add = "mean_se",
                   color = "Condition", fill = "Condition", 
                   position = position_dodge(0.8),
                   facet.by = c("Condition","Variable"),
                   ylab = paste("Intrarendition CV of Pitch
                             (Post - Pre) / (Post + Pre)"),
                   ylim = c(0.3,-0.3)) +
  scale_y_continuous(labels = tickdecimal_2) +
  scale_color_manual(values =c("orangered2","royalblue3")) +
  scale_fill_manual(values = c("lightpink1","lightskyblue")) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ Condition + Variable, strip.position = "bottom", nrow = 1) +
  theme_classic2() +
  theme(strip.background = element_blank(), strip.placement = "outside", 
        strip.text = element_blank(), axis.ticks = element_blank(), axis.line.x = 
          element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())

##- CV (normalized or modulation index)

plot2 <- ggbarplot(data = graph,
                   x = "Timepoint", y = "modulationindex.meanMAD", 
                   add = "mean_se",
                   #error.plot = "upper_errorbar",
                   color = "Condition", fill = "Condition", 
                   position = position_dodge(0.8),
                   facet.by = c("Condition","Variable"),
                   ylab = paste("Intrarendition MAD of Pitch
                            (Post - Pre) / (Post + Pre)"),
                   ylim = c(0.3,-0.3),
                   order = c("1","2","3")) +
  scale_y_continuous(labels = tickdecimal_2) +
  scale_color_manual(values =c("orangered2","royalblue3")) +
  scale_fill_manual(values = c("lightpink1","lightskyblue")) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ Condition + Variable, strip.position = "bottom", nrow = 1) +
  theme_classic2()+
  theme(strip.background = element_blank(), strip.placement = "outside", 
        strip.text = element_blank(), axis.ticks.x = element_blank(), axis.line.x = 
          element_blank(), axis.title.x = element_blank()) 

##- Combine both plots

multiplot <- ggarrange(plot1, plot2, nrow = 2)

print(multiplot)

##- Save plot 

ggsave(multiplot, file = paste0(maindir,"/Conditions/FF.UD.ModulationIndexScores.IntraRenditionScores",
                                ".emf"), scale = 2)
