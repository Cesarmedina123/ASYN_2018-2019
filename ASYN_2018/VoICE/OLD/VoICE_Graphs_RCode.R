########## Graph Individual Acoustic Features ############

#- subset by variable
#- subset further by condition and timepoint
#- to run comparisons using ggpubr or ggsignif

######### Graph using GGPUBR ##### BETTER #####

library(tidyverse)
library(ggpubr)
library(ggsignif)
library(readr)
library(car)
library(broom)
library(svglite)

##- acousticfeature <- "mean.entropy" {graph <- graph[graph$Variable == acousticfeature,]

# Columname to be used later on for plotting

#columname <- as.character(as.vector(graph$Variable[1]))

#columname <- gsub("."," ", columname, fixed = TRUE)

# Capitalize first letter of each word with toupper function
# Will be used for naming in plots

#simpleCap <- function(x) {
#  s <- strsplit(x, " ")[[1]]
#  paste(toupper(substring(s, 1,1)), substring(s, 2),
#        sep="", collapse=" ")
#}

#columname <- sapply(columname,simpleCap) # not necessary to loop over 1 element
#}

# Loop through all rows and identify rows with a "no" and remove them
# this filters down the points plotted to harmonic syllables
# "yes" and "no" are only found in the harmonic column

#row_reduce <- apply(graph, 1, function(row) all(row != "no"))
#
#raph <- graph[row_reduce,]

mainDir <- "C:/Users/cesar/Documents/Julie_Lab/Data/ASYN_2018/VoICE/Conditions/"

df <- read.table(file = paste0(mainDir,"VoICE_UD_Table_Birds.csv"), 
                 header = TRUE, sep = ",")

graph <- read.table(file = paste0(mainDir,"VoICE_UD_Table_Condition_AllSylls.csv"), 
                    header = TRUE, sep = ",")

#annotation_graph <- read.table("~/Documents/Julie_Lab/Data/VoICE/Conditions/VoICE_UD_Statistics.csv",
 #                              header = TRUE, sep = ",")

selectedvars <- c("var.AM","var.entropy","var.FM",
                  "var.mean.freq","var.pitch.goodness") #Exclude vars.pitch

selectedfeatures <- c("mean.AM.2","mean.amplitude","mean.entropy","mean.FM",
                      "mean.mean.freq", "mean.pitch.goodness",
                      "syllable.duration") # exclude "mean.pitch"

graph <- graph[graph$Harmonic == "yes",] # Graph only harmonic syllables
graph <- graph[graph$Timepoint != 0, ] # Graph only 1,2,3 month times

tickdecimal_1 <- function(x) sprintf("%.1f", x)
tickdecimal_2 <- function(x) sprintf("%.2f", x)

#### Acoustic Features for Intersyllable-intrarendition comparisons ####
#### By timepoint and condition ####

###- Graph selected features

plot1 <- ggline(data = graph[graph$Variable %in% selectedvars, ], 
                x = "Timepoint", y = "modulationindex.mean", 
                add = c("mean","se"),
                color = "Condition", fill = "Condition", shape = "Condition",
                position = position_dodge(0.8),
                facet.by = "Variable",
                ylab = paste("Normalized Mean Values (Post/Pre)"), point.size = .5) +
  scale_y_continuous(labels = tickdecimal_2) +
  #scale_x_discrete() +
  scale_shape_manual(values = c(15,17)) +
  scale_color_manual(values =c("orangered2","royalblue3")) +
  scale_fill_manual(values = c("lightpink1","lightskyblue")) + 
  facet_wrap(~ Variable, strip.position = "bottom", nrow = 1) +
  theme_classic2() +
  theme(strip.background = element_blank(), strip.placement = "outside") 

plot1 <- plot1 + geom_text(data = annotation_graph[annotation_graph$Variable %in% selectedvars 
                                                   && annotation_graph$Scores == "modulationindex.mean"
                                                   && annotation_graph$p.value <= .05, ],
                           mapping = aes(label = p.value, x = x_position,y = y_position)) 

print(plot1)

#Use this format if groups to be compared are on x-axis

#plot1 <- plot1 + stat_compare_means(aes(group = c("Variable")), comparisons = c("Condition"),
 #                                   method = "wilcox.test",label = "p.signif", paired = FALSE,
  #                                  hide.ns = TRUE)

###- Save Plot

ggsave(plot1, file = paste0(maindir,"/","Conditions/UD.WilcoxTest.NormalizedMeanVarsScores.MasterFileScores",
                            ".emf"), scale = 2)

#### Acoustic Features for Intrasyllable-interrendition comparisons ####
#### By timepoint and condition ####

###- Normalized mean and CV scores of acoustic features
###- All syllables (noisy and harmonics)


plot1 <- ggline(data = graph[graph$Variable %in% selectedfeatures, ], 
                x = "Timepoint", y = "valuesmean", 
                add = "mean_se",
                color = "Condition", fill = "Condition", shape = "Condition",
                position = position_dodge(0.8),
                facet.by = "Variable",
                ylab = paste("Normalized Mean Values (Post/Pre)"), point.size = .5) +
  scale_shape_manual(values = c(15,17)) +
  scale_color_manual(values =c("orangered2","royalblue3")) +
  scale_fill_manual(values = c("lightpink1","lightskyblue")) +
  facet_wrap(~ Variable, strip.position = "bottom", nrow = 1) +
  theme_classic2() +
  theme(strip.background = element_blank(), strip.placement = "outside", 
        strip.text = element_blank(), axis.ticks = element_blank(), axis.line.x = 
          element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
        plot.title = element_text("Normalized Values")) 

plot1 <- plot1 + geom_text(data = annotation_graph[annotation_graph$Variable %in% selectedfeatures 
                                                   && annotation_graph$Scores == "valuesmean",],
                           mapping = aes(label = p.value, x = x_position,y = y_position)) 

plot2 <- ggline(data = graph[graph$Variable %in% selectedfeatures, ], 
                x = "Timepoint", y = "valuesCV", 
                add = "mean_se",
                color = "Condition", fill = "Condition", shape = "Condition",
                position = position_dodge(0.8),
                facet.by = "Variable",
                ylab = paste("Normalized CV Values (Post/Pre)"), point.size = .5) +
  scale_y_continuous(labels = tickdecimal_2) +
  scale_shape_manual(values = c(15,17)) +
  scale_color_manual(values =c("orangered2","royalblue3")) +
  scale_fill_manual(values = c("lightpink1","lightskyblue")) + 
  facet_wrap(~ Variable, strip.position = "bottom", nrow = 1) +
  theme_classic2() +
  theme(strip.background = element_blank(), strip.placement = "outside")

plot2 <- plot2 + geom_text(data = annotation_graph[annotation_graph$Variable %in% selectedfeatures 
                                                   && annotation_graph$Scores == "valuesCV",],
                           mapping = aes(label = p.value, x = x_position,y = y_position),
                           vjust = -.2)

multiplot_1 <- ggarrange(plot1, plot2, nrow = 2)

print(multiplot_1)

ggsave(multiplot_1, file = paste0(maindir,"/","Conditions/UD.Harmonic.WilcoxTest.NormalizedScores.MasterFileScores",
                                  ".emf"), scale = 2)

###- Barplot using GGPUBR to graph modulation index scores

##- Redifine limit so direction of errorbar is condition on sign of mean

limits <- aes(
  ymax = graphN$mean + (graph$mean > 0)*graph$se,  
  ymin = graph$mean - (graph$mean < 0)*graph$se)

##- Mean (modulation index)

plot1 <- ggbarplot(data = graph[graph$Variable %in% selectedfeatures &
                                  graph$Timepoint != 0,],
                   x = "Condition", y = "modulationindex.mean.mean", 
                   add = c("se"),
                   color = "Condition", fill = "Condition", 
                   position = position_dodge(0.8),
                   facet.by = c("Variable","Timepoint"),
                   ylab = paste("Mean (Post - Pre) / (Post + Pre)"),
                   ylim = c(0.05,-0.05)) +
  scale_y_continuous(labels = tickdecimal_2) +
  scale_color_manual(values =c("orangered2","royalblue3")) +
  scale_fill_manual(values = c("lightpink1","lightskyblue")) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ Variable + Timepoint, strip.position = "bottom", nrow = 1) +
  theme_classic2() +
  theme(strip.background = element_blank(), strip.placement = "outside", 
        strip.text = element_blank(), axis.ticks = element_blank(), axis.line.x = 
          element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())

##- CV (normalized or modulation index)

plot2 <- ggbarplot(data = graph[graph$Variable %in% selectedfeatures&
                                  graph$Timepoint != 0, ],
                   x = "Condition", y = "modulationindex.CV.mean", 
                   add = "mean_se",
                   #error.plot = "pointrange",
                   color = "Condition", fill = "Condition", 
                   position = position_dodge(0.8),
                   facet.by = c("Variable","Timepoint"),
                   ylab = paste("Interrendition Mean
                                (Post - Pre) / (Post + Pre)"),
                   ylim = c(0.3,-0.3),
                   order = c("asyn","con")) +
  scale_y_continuous(labels = tickdecimal_2) +
  scale_color_manual(values =c("orangered2","royalblue3")) +
  scale_fill_manual(values = c("lightpink1","lightskyblue")) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ Variable + Timepoint, strip.position = "bottom", nrow = 1) +
  theme_classic2()+
  theme(strip.background = element_blank(), strip.placement = "outside") 

##- Combine both plots

multiplot <- ggarrange(plot1, plot2, nrow = 2)

print(multiplot)

##- Save plot 

ggsave(multiplot, file = paste0(maindir,"/Conditions/FF.UD.ModulationIndexScores.IntraRenditionScores",
                                ".emf"), scale = 2)

#### Acoustic Features for Inrasyllable-intrarendition comparisons ####
#### For each Bird over multiple timepoints ####

graph <- read.table(file = paste0(maindir,"/","Conditions/UD_VoICE_IntraSyllIntraRend_MasterSummaryTable.csv"), 
                    header = TRUE, sep = ",")

birdID <- "Wh214"

graph <- graph[graph$BirdID == birdID, ]

selectedfeatures <- c("mean.AM.2","mean.amplitude","mean.entropy","mean.FM",
                      "mean.mean.freq", "mean.pitch.goodness", "mean.pitch",
                      "syllable.duration") # exclude 

selectedvars <- c("var.AM","var.entropy","var.FM",
                  "var.mean.freq","var.pitch.goodness") #Exclude vars.pitch

###- Create DF containing annotations (i.e.,Pvalues) 
###- that will be mapped onto graph

annotation_graph <- cross_df(list(Variable = Variables_list, Scores = Scores_list, xmax = Timepoint_list)) %>%
  mutate(xmin = factor(0),
         annotations = apply(combn(letters, 2), 2, paste0, collapse = "")[1:n()]) %>%
  filter(xmin != xmax) %>%
  group_by(Variable) %>%
  arrange(xmax) %>%
  mutate(y_position = 1 + .2 * row_number())

###- Graph selected features

#graph$Syll <- factor(graph$Syll, labels = unique(graph[graph$Syll]))

#tickdecimal_2 <- function(x) sprintf("%.2f", x)

plot1 <- ggline(data = graph[graph$Variable %in% selectedfeatures, ], x = "Timepoint", y = "Score", 
                add = "mean_se",
                color = "Syll", fill = "Syll", shape = "Syll",
                position = position_dodge(0.8),
                facet.by = c("Variable","Syll"),
                ylab = paste("Mean Scores"), point.size = .5) +
  #scale_shape_manual(values = c(15,17)) +
  #scale_color_manual(values =c("orangered2","royalblue3")) +
  #scale_fill_manual(values = c("lightpink1","lightskyblue")) +
  facet_wrap(~ Variable, strip.position = "bottom", scales = "free", nrow = 1) +
  theme_classic() +
  theme(strip.background = element_blank(), strip.placement = "outside")

plot2 <- ggline(data = graph[graph$Variable %in% selectedvars, ], x = "Timepoint", y = "Score", 
                add = "mean_se",
                color = "Syll", fill = "Syll", shape = "Syll",
                #position = position_dodge(0.8),
                facet.by = "Variable",
                ylab = paste("Mean"), point.size = .5) +
  #scale_y_continuous(labels = tickdecimal_2) +
  #scale_shape_manual(values = c(15,17)) +
  #scale_color_manual(values =c("orangered2","royalblue3")) +
  #scale_fill_manual(values = c("lightpink1","lightskyblue")) + 
  facet_wrap(~ Variable, strip.position = "bottom", scales = "free",nrow = 1) +
  theme_classic2() +
  theme(strip.background = element_blank(), strip.placement = "outside") 

multiplot_1 <- ggarrange(plot1, plot2, nrow = 2)

ggsave(multiplot_1, file = paste0(maindir,"/",birdID,"/",paste0("UD.AbsoluteScores.",
                                                            birdID,".emf")), scale = 2)


