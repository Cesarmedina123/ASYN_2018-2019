########## Graph Individual Acoustic Features ############

#- subset by variable
#- subset further by condition and timepoint
#- to run comparisons using ggpubr or ggsignif

install.packages('car')
install.packages('svglite')
install.packages("extrafont")

######### Graph using GGPUBR ##### BETTER #####

library(tidyverse)
library(ggpubr)
library(ggsignif)
library(readr)
library(car)
library(broom)
library(svglite)
library(extrafont)

font_import()
loadfonts(device = "win")

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
#graph <- graph[row_reduce,]

mainDir <- "C:/Users/cesar/Documents/Julie_Lab/Data/ASYN_2018/VoICE/Conditions/"

df <- read.table(file = paste0(mainDir,"VoICE_UD_Table_Birds.csv"), 
                 header = TRUE, sep = ",")

dfBGsummary <- read.table(file = paste0(mainDir,"VoICE_UD_Table_Condition_AllSylls.csv"), 
                    header = TRUE, sep = ",")

dfWGsummary <- read.table(file = paste0(mainDir,"VoICE_UD_WG_WelchTest.csv"), 
                          header = TRUE, sep = ",")

dfWGsummary$y.position <- .5
dfWGsummary$xmin <- 0

selectedvars <- c("var.AM","var.entropy","var.FM",
                  "var.mean.freq","var.pitch.goodness") #Exclude vars.pitch

selectedfeatures <- c("mean.AM.2","mean.amplitude","mean.entropy","mean.FM",
                      "mean.mean.freq", "mean.pitch.goodness",
                      "syllable.duration") # exclude "mean.pitch"

tickdecimal_1 <- function(x) sprintf("%.1f", x)
tickdecimal_2 <- function(x) sprintf("%.2f", x)
tickdecimal_3 <- function(x) sprintf("%.3f", x)

#### Acoustic Features for Intersyllable-intrarendition comparisons ####
#### By timepoint and condition ####

###- Graph selected features

plot1 <- ggline(data = df[f$Variable %in% selectedvars & df$Timepoint != 0, ], 
                x = "Timepoint", y = "ZScoreMean", 
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

#geom_signif(comparisons = c(0,1), map_signif_level = TRUE, 
 #           test = "t.test", test.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE)
  #          ,textsize=6) 

###- Save Plot

ggsave(plot1, file = paste0(maindir,"/","Conditions/UD.WilcoxTest.NormalizedMeanVarsScores.MasterFileScores",
                            ".emf"), scale = 2)

#### Acoustic Features for Intrasyllable-interrendition comparisons ####
#### By timepoint and condition ####

my_comparisons <- list(c("0", "1"),c("0", "2"),c("0", "3"))


plotA1 <- ggline(data = df[df$Variable %in% selectedfeatures &
                             df$Condition == "asyn" & df$Harmonic == "yes", ], 
                 x = "Timepoint", y = "NormMean", 
                 add = "mean_se",
                 error.plot = "pointrange",
                 color = "Condition", fill = "Condition", shape = "Condition",
                 position = position_dodge(0.8),
                 facet.by = "Variable",
                 ylab = paste("Mean (Post/Pre)"),
                 ylim = c(1.075,0.875), point.size = .5) +
  stat_compare_means(comparisons=my_comparisons, method = "t.test",
                     method.args = list(alternative = "two.sided", var.equal = FALSE, paired = TRUE),
                     aes(label=p.signif))


###- Z Scores of mean and CV scores of acoustic features
###- All syllables (noisy and harmonics)

df$Timepoint <- as.factor(df$Timepoint)
Tms <- unique(df$Timepoint)
dfWGsummary$y.position <- as.factor(dfWGsummary$y.position)
dfWGsummary$Timepoint <- as.factor(dfWGsummary$Timepoint)
dfWGsummary$xmin.zscore <- as.factor(dfWGsummary$xmin)


plotA1 <- ggdensity(data = df[df$Variable %in% selectedfeatures & df$Harmonic == "yes" &
                                df$Condition == "asyn", ],
                   x = "ZScoreMean", y = "..density..", rug = TRUE,
                   color = "Timepoint", fill = "Timepoint", 
                   palette = get_palette("Set1",4), alpha = .25,
                   add = "mean", ylim = c(0,2.5),
                   facet.by = c("Variable")) +
  scale_y_continuous(labels = tickdecimal_3) +
  facet_wrap(~ Variable, strip.position = "top", nrow = 4, ncol = 7) +
  theme_classic2() +
  theme(strip.background = element_blank(), strip.placement = "outside", 
        axis.title.x = element_text("top"), legend.position = "none")

plotA1 <- ggviolin(data = df[df$Variable %in% selectedfeatures & df$Harmonic == "yes" &
                               df$Condition == "asyn", ],
                   x = "Timepoint", y = "ZScoreMean",  title = "ASYN",
                   color = "Timepoint", fill = "Timepoint", 
                   palette = c("#E41A1C","#377EB8","#4DAF4A","#984EA3"), ylim = c(-3,5), 
                   add = "boxplot", add.params = list(color = "black"),
                   order = c(0,1,2,3), facet.by = "Variable") +
  scale_y_continuous(labels = tickdecimal_3) +
  facet_wrap(~ Variable, strip.position = "top", nrow = 4, ncol = 7) +
  theme_classic2() +
  theme(strip.background = element_blank(), strip.placement = "outside",axis.ticks = element_blank(),
        axis.line.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
        legend.position = "none", plot.title = element_text(font("title", color = "#E41A1C")),
        plot.background = element_blank())

plotC1 <- ggviolin(data = df[df$Variable %in% selectedfeatures & df$Harmonic == "no" &
                               df$Condition == "con", ],
                   x = "Timepoint", y = "ZScoreMean",  title = "CON",
                   color = "Timepoint", fill = "Timepoint", 
                   palette = c("#E41A1C","#377EB8","#4DAF4A","#984EA3"), ylim = c(-3,5), 
                   add = "boxplot", add.params = list(color = "black"),
                   order = c(0,1,2,3), facet.by = "Variable") +
  scale_y_continuous(labels = tickdecimal_3) +
  facet_wrap(~ Variable, strip.position = "top", nrow = 4, ncol = 7) +
  theme_classic2() +
  theme(strip.background = element_blank(), strip.placement = "outside",axis.ticks = element_blank(),
        axis.line.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank(), plot.title = element_text(font("title", color = "#377EB8")),
        plot.background = element_blank())
                    
plotA2 <- ggline(data = df[df$Variable %in% selectedfeatures &
                             df$Condition == "asyn", ], 
                x = "Timepoint", 
                y = "ZScoreMean", 
                add = "mean_se",
                error.plot = "pointrange",
                color = "Condition", fill = "Condition", shape = "Condition",
                position = position_dodge(0.8),
                facet.by = "Variable",
                ylab = paste("Mean (Post/Pre)"),
                ylim = c(-.5,.75), point.size = .5) +
  scale_y_continuous(labels = tickdecimal_3) +
  scale_shape_manual(values = 15) +
  scale_color_manual(values = "#E41A1C") +
  facet_wrap(~ Variable, strip.position = "bottom", nrow = 1) +
  theme_classic2() +
  theme(strip.background = element_blank(), strip.placement = "outside", 
        strip.text = element_blank(), axis.ticks = element_blank(), axis.line.x =element_blank(), 
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        plot.title = element_text("Mean"), legend.position = "none")

plotA2 <- plotA2 + stat_pvalue_manual(data = dfWGsummary[dfWGsummary$Variable %in% selectedfeatures &
                                                           dfWGsummary$p.value <= .05, ],
                                      label = "p.value", 
                                      xmin = "xmin", 
                                      xmax = "Timepoint", 
                                      y.position = "y.position", 
                                      remove.bracket = TRUE)

plotA2 <- plotA2 + stat_compare_means(data = df[df$Variable %in% selectedfeatures &
                                                         df$Condition == "asyn", ], 
                                      x = "Timepoint", 
                                      y = "ZScoreMean",,
                                      method = "t.test", 
                                      paired =  TRUE, 
                                      method.args = list(alternative = "two.sided"),
                                      ref.group = "0",
                                      label = "p.signif", hide.ns = TRUE,
                                      na.rm = TRUE, inherit.aes = TRUE)

plotC2 <- ggline(data = df[df$Variable %in% selectedfeatures &
                             df$Condition == "con", ], 
                 x = "Timepoint", 
                 y = "ZScoreMean", 
                 add = "mean_se",
                 error.plot = "pointrange",
                 color = "Condition", fill = "Condition", shape = "Condition",
                 position = position_dodge(0.8),
                 facet.by = "Variable",
                 ylab = paste("Mean (Post/Pre)"),
                 ylim = c(-.5,.75), point.size = .5) +
  scale_y_continuous(labels = tickdecimal_3) +
  scale_shape_manual(values = 17) +
  scale_color_manual(values = "#377EB8") +
  facet_wrap(~ Variable, strip.position = "bottom", nrow = 1) +
  theme_classic2() +
  theme(strip.background = element_blank(), strip.placement = "outside", 
        strip.text = element_blank(), axis.ticks = element_blank(), axis.line.x = 
          element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank(), plot.title = element_text("Normalized Values"),
        plot.background = element_blank())

plotC2 <- plotC2 + geom_text(data = annotation_graph[annotation_graph$Variable %in% selectedfeatures 
                                                     && annotation_graph$Scores == "valuesmean",],
                             mapping = aes(label = p.value, x = x_position,y = y_position)) 


plotA3 <- ggline(data = df[df$Variable %in% selectedfeatures &
                             df$Condition == "asyn", ], 
                 x = "Timepoint", y = "ZScoreCV", 
                 add = "mean_se",
                 error.plot = "pointrange",
                 color = "Condition", fill = "Condition", shape = "Condition",
                 position = position_dodge(0.8),
                 facet.by = "Variable",
                 ylab = paste("CV (Post/Pre)"),
                 ylim = c(-.5,.75), point.size = .5) +
  scale_y_continuous(labels = tickdecimal_3) +
  scale_shape_manual(values = 15) +
  scale_color_manual(values = "#E41A1C") + #orangered2
  facet_wrap(~ Variable, strip.position = "bottom", nrow = 1) +
  theme_classic2() +
  theme(strip.background = element_blank(), strip.placement = "outside", axis.title.x = element_blank(),
        legend.position = "none", plot.background = element_blank())

plotA3 <- plotA3 + geom_text(data = annotation_graph[annotation_graph$Variable %in% selectedfeatures 
                                                     && annotation_graph$Scores == "valuesCV",],
                             mapping = aes(label = p.value, x = x_position,y = y_position),
                             vjust = -.2)

plotC3 <- ggline(data = df[df$Variable %in% selectedfeatures &
                             df$Condition == "con", ], 
                 x = "Timepoint", y = "ZScoreCV", 
                 add = "mean_se",
                 error.plot = "pointrange",
                 color = "Condition", fill = "Condition", shape = "Condition",
                 position = position_dodge(0.8),
                 facet.by = "Variable",
                 ylab = paste("CV (Post/Pre)"), 
                 ylim = c(-.5,.75), point.size = .5) +
  scale_y_continuous(labels = tickdecimal_3) +
  scale_shape_manual(values = 17) +
  scale_color_manual(values = "#377EB8") + #royalblue3
  facet_wrap(~ Variable, strip.position = "bottom", nrow = 1) +
  theme_classic2() +
  theme(strip.background = element_blank(), strip.placement = "outside", axis.title.x = element_blank(),
        axis.title.y = element_blank(), panel.spacing = unit(1, "lines"), plot.background = element_blank())

plotC3 <- plotC3 + geom_text(data = annotation_graph[annotation_graph$Variable %in% selectedfeatures 
                                                   && annotation_graph$Scores == "valuesCV",],
                           mapping = aes(label = p.value, x = x_position,y = y_position),
                           vjust = -.2)

multiplot_1 <- ggarrange(plotA1, plotC1, plotA2, plotC2, plotA3, plotC3, nrow = 3, ncol = 2)

print(multiplot_1)

printDir <- "C:/Users/cesar/Documents/Julie_Lab/Data/ASYN_2018/VoICE/Conditions"

ggsave(multiplot_1, file = paste0(printDir, "/VoICE.UD.WG.Noisy.ZScores",".emf"), scale = 2)

###- Barplot using GGPUBR to graph modulation index scores

df$Timepoint <- as.factor(df$Timepoint)

##- Redifine limit so direction of errorbar is condition on sign of mean

limits <- aes(
  ymax = graphN$mean + (graph$mean > 0)*graph$se,  
  ymin = graph$mean - (graph$mean < 0)*graph$se)

##- Inter-renditions comparisons

#- Destribution of mean

plot1 <- ggviolin(data = df[df$Variable %in% JulieFeatures & df$Timepoint == 2, ],
                   x = "Condition", y = "modulationindex.mean",
                   color = "Condition", fill = "Condition",
                   ylab = paste("Mean"), 
                   ylim = c(-.4,.4),
                   position = position_dodge(0.8),
                   palette = c("#DC0000B2","#4DBBD5B2"),  
                   add = "boxplot", add.params = list(color = "black"),
                   order = c("asyn","con"), facet.by = c("Variable","Timepoint")) +
  scale_y_continuous(labels = tickdecimal_3) +
  facet_wrap(~ Variable + Timepoint, strip.position = "top", nrow = 1) +
  theme_classic2() +
  theme(strip.background = element_blank(), strip.placement = "outside",axis.ticks.x = element_blank(),
        axis.line.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
        panel.spacing = unit(1, "lines"), plot.background = element_blank(), 
        text=element_text(size=16,  family="TT Arial")) 

#- Mean

plot2 <- ggbarplot(data = df[df$Variable %in% JulieFeatures & df$Timepoint == 2, ],
                   x = "Condition", y = "modulationindex.mean", 
                   add = c("mean_se"), alpha = 1,
                   error.plot = "pointrange",
                   color = "Condition", fill = "Condition", 
                   position = position_dodge(0.8),
                   facet.by = c("Variable","Timepoint"),
                   ylab = paste("Mean"),
                   ylim = c(-0.04,0.04),
                   order = c("asyn","con")) +
  scale_y_continuous(labels = tickdecimal_3) +
  scale_x_discrete(labels = function(x){paste0(x)}) +
  scale_color_manual(values =c("#E41A1C","#377EB8")) +
  scale_fill_manual(values = c("#E64B35B2","#4DBBD5B2")) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ Variable + Timepoint, strip.position = "bottom", nrow = 1) +
  theme_classic2() +
  theme(strip.background = element_blank(), strip.placement = "outside", 
        strip.text = element_blank(), axis.ticks.x = element_blank(), axis.line.x = 
          element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
        panel.spacing = unit(1, "lines"), plot.background = element_blank(), 
        text=element_text(size=16,  family="TT Arial"))

#- CV Inter-rendition

plot3 <- ggbarplot(data = df[df$Variable %in% selectedfeatures & df$Timepoint != 0, ],
                   x = "Condition", y = "modulationindex.CV", 
                   add = "mean_se", alpha = 1,
                   error.plot = "pointrange",
                   color = "Condition", fill = "Condition", 
                   position = position_dodge(0.8),
                   ylab = paste("CV"),
                   ylim = c(-0.15,0.15),
                   order = c("asyn","con")) +
  scale_y_continuous(labels = tickdecimal_3) +
  scale_color_manual(values =c("#E41A1C","#377EB8")) +
  scale_fill_manual(values = c("#E64B35B2","#4DBBD5B2")) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ Variable + Timepoint, strip.position = "bottom", nrow = 1) +
  geom_signif(comparisons = list(c("asyn","con")),
              test = 'wilcox.test', 
              test.args = list(alternative = "two.sided",
                               var.equal = FALSE, paired = FALSE), 
              map_signif_level = TRUE,
              step_increase = .15, textsize = 3.25) +
  theme_classic2()+
  theme(strip.background = element_blank(), strip.placement = "outside", 
        strip.text = element_blank(), axis.ticks.x = element_blank(), axis.line.x = 
          element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
        panel.spacing = unit(1, "lines"), plot.background = element_blank(), 
        text=element_text(size=16,  family="TT Arial")) 

#- Disctribution of CV

plot4 <- ggviolin(data = df[df$Variable %in% JulieFeatures & df$Timepoint == 2, ],
                  x = "Condition", y = "modulationindex.CV",
                  color = "Condition", fill = "Condition",
                  ylab = paste("CV"), 
                  ylim = c(-1,1),
                  position = position_dodge(0.8),
                  palette = c("#DC0000B2","#4DBBD5B2"),  
                  add = "boxplot", add.params = list(color = "black"),
                  order = c("asyn","con"), facet.by = c("Variable","Timepoint")) +
  scale_y_continuous(labels = tickdecimal_3) +
  facet_wrap(~ Variable + Timepoint, strip.position = "bottom", nrow = 1) +
  theme_classic2() +
  theme(strip.background = element_blank(), strip.placement = "outside", axis.ticks.x = element_blank(),
        axis.line.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
        panel.spacing = unit(1, "lines"), plot.background = element_blank(), 
        text=element_text(size=16,  family="TT Arial")) 

#- Combine both plots

multiplot <- ggarrange(plot1, plot2, plot3, plot4, nrow = 4)

print(multiplot)

ggsave(multiplot, file = paste0(mainDir,"/VoICE.UD.BG.AllSylls.JulieFeatures.InterRenditionScores",
                                ".png"), width = 12, height = 17,units = "cm")

##- Intra-rendition comparisons

#- Disctribution of Vars 

plot5 <- ggviolin(data = df[df$Variable %in% selectedfeatures & df$Timepoint != 0, ],
                  x = "Condition", y = "modulationindex.CV",  title = "All Syllables Inter-Rendition Scores",
                  color = "Condition", fill = "Condition",
                  ylab = paste("Mean"), 
                  ylim = c(-1,1),
                  position = position_dodge(0.8),
                  palette = c("#DC0000B2","#4DBBD5B2"),  
                  add = "boxplot", add.params = list(color = "black"),
                  order = c("asyn","con"), facet.by = c("Variable","Timepoint")) +
  scale_y_continuous(labels = tickdecimal_3) +
  facet_wrap(~ Variable + Timepoint, strip.position = "bottom", nrow = 1) +
  theme_classic2() +
  theme(strip.background = element_blank(), strip.placement = "outside", axis.ticks.x = element_blank(),
        axis.line.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
        panel.spacing = unit(1, "lines"), plot.background = element_blank()) 

##- Vars Intra-rendition

plot6 <- ggbarplot(data = df[df$Variable %in% selectedvars &
                               df$Timepoint != 0, ],
                   x = "Condition", y = "modulationindex.mean", 
                   add = "mean_se", alpha = 1,
                   error.plot = "pointrange",
                   color = "Condition", fill = "Condition", 
                   position = position_dodge(0.8),
                   facet.by = c("Variable","Timepoint"),
                   ylab = paste("CV (Post - Pre) / (Post + Pre)"),
                   ylim = c(0.15,-0.15),
                   order = c("asyn","con")) +
  scale_y_continuous(labels = tickdecimal_3) +
  scale_color_manual(values =c("#E41A1C","#377EB8")) +
  scale_fill_manual(values = c("#E64B35B2","#4DBBD5B2")) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 3) +
  facet_wrap(~ Variable + Timepoint, strip.position = "bottom", nrow = 1) +
  theme_classic2()+
  theme(strip.background = element_blank(), strip.placement = "outside",
        panel.spacing = unit(1, "lines"), plot.background = element_blank()) 

##- Combine both plots

multiplot <- ggarrange(plot5, plot6, nrow = 4)

print(multiplot)

##- Save plot 

ggsave(multiplot, file = paste0(mainDir,"/VoICE.UD.BG.AllSylls.InterRenditionScores",
                                ".pdf"), scale = 2)

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


