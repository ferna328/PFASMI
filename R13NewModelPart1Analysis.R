######################## R13NewModelPart1Analysis.R ############################
# Research Paper: Large-scale Assessment of PFAS Compounds in Drinking Water   #
# Sources through Machine Learning                                             #
# Nicolas Fernandez, A. Pouyan Nejadhashemi, Christian Loveall                 #
# Department of Biosystems and Agricultural Engineering                        #
# Michigan State University, East Lansing, MI, United States                   #
# Code written and curated by Nicolas Fernandez (OrcID 0000-0001-7979-2941)    #
######################## Description ###########################################
# According to the Paper, this code is used to analyze the results after       #
# running the previous code R12NewModelPart1Binary.R                           #
################################################################################
###### 0. IMPORT AND  PREPARE DATA #############################################
# 0.1 Set Working Directory
wd = 'E:/PFASMIproject'
setwd(wd)
# 0.2 Read workspace with results:
dirWS <- paste(wd,"BinarySp8.RData", sep = "/")
load(dirWS)
###### 1. DISTRIBUTION OF PFAS DATA ############################################
# 1.1 Bernoulli distribution ---------------------------------------------------
library(ggplot2)
binwidth = 0.25
histline = "black"
histfill = "white"
plotB1 <- ggplot(dfspat, aes(x=PFBS_B)) + geom_histogram(aes(y=..density..), binwidth=binwidth,colour=histline, fill=histfill) 
plotB2 <- ggplot(dfspat, aes(x=PFHpA_B)) + geom_histogram(aes(y=..density..), binwidth=binwidth,colour=histline, fill=histfill)
plotB3 <- ggplot(dfspat, aes(x=PFHxA_B)) + geom_histogram(aes(y=..density..), binwidth=binwidth,colour=histline, fill=histfill)
plotB4 <- ggplot(dfspat, aes(x=PFHxS_B)) + geom_histogram(aes(y=..density..), binwidth=binwidth,colour=histline, fill=histfill)
plotB5 <- ggplot(dfspat, aes(x=PFOA_B)) + geom_histogram(aes(y=..density..), binwidth=binwidth,colour=histline, fill=histfill)
plotB6 <- ggplot(dfspat, aes(x=PFOA_PFOS_B)) + geom_histogram(aes(y=..density..), binwidth=binwidth,colour=histline, fill=histfill)
plotB7 <- ggplot(dfspat, aes(x=PFOS_B)) + geom_histogram(aes(y=..density..), binwidth=binwidth,colour=histline, fill=histfill)
plotB8 <- ggplot(dfspat, aes(x=Total_PFAS_B)) + geom_histogram(aes(y=..density..), binwidth=binwidth,colour=histline, fill=histfill)
library(gridExtra)
CompactPlotB <- grid.arrange(plotB1,
                            plotB2,
                            plotB3,
                            plotB4,
                            plotB5,
                            plotB6,
                            plotB7,
                            plotB8,nrow=2,ncol=4)
# 1.2 Normal distribution ---------------------------------------------------
# 1.2.1 Filter detected values .........................
library(dplyr)
FiltSp1 <- filter(dfspat,PFBS_B==1)
FiltSp2 <- filter(dfspat,PFHpA_B==1)
FiltSp3 <- filter(dfspat,PFHxA_B==1)
FiltSp4 <- filter(dfspat,PFHxS_B==1)
FiltSp5 <- filter(dfspat,PFOA_B==1)
FiltSp6 <- filter(dfspat,PFOA_PFOS_B==1)
FiltSp7 <- filter(dfspat,PFOS_B==1)
FiltSp8 <- filter(dfspat,Total_PFAS_B==1)
# 1.2.2. Plot detected concentrations..................
histline = "black"
histfill = "white"
densalph = 0.2  
densfill = "#FF6666"
binwidth = 3.5  
plotN1 <- ggplot(FiltSp1, aes(x=PFBS)) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
plotN2 <- ggplot(FiltSp2, aes(x=PFHpA)) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
plotN3 <- ggplot(FiltSp3, aes(x=PFHxA)) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
plotN4 <- ggplot(FiltSp4, aes(x=PFHxS)) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
plotN5 <- ggplot(FiltSp5, aes(x=PFOA)) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
plotN6 <- ggplot(FiltSp6, aes(x=PFOA_PFOS)) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
plotN7 <- ggplot(FiltSp7, aes(x=PFOS)) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
plotN8 <- ggplot(FiltSp8, aes(x=Total_PFAS)) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
CompactPlotN <- grid.arrange(plotN1,
                                plotN2,
                                plotN3,
                                plotN4,
                                plotN5,
                                plotN6,
                                plotN7,
                                plotN8,nrow=2,ncol=4)
# 1.2.3. Plot detected log concentrations..................
binwidth = 0.45  
plotN1log <- ggplot(FiltSp1, aes(x=log(PFBS))) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
plotN2log <- ggplot(FiltSp2, aes(x=log(PFHpA))) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
plotN3log <- ggplot(FiltSp3, aes(x=log(PFHxA))) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
plotN4log <- ggplot(FiltSp4, aes(x=log(PFHxS))) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
plotN5log <- ggplot(FiltSp5, aes(x=log(PFOA))) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
plotN6log <- ggplot(FiltSp6, aes(x=log(PFOA_PFOS))) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
plotN7log <- ggplot(FiltSp7, aes(x=log(PFOS))) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
plotN8log <- ggplot(FiltSp8, aes(x=log(Total_PFAS))) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
CompactPlotNlog <- grid.arrange(plotN1log,
                                   plotN2log,
                                   plotN3log,
                                   plotN4log,
                                   plotN5log,
                                   plotN6log,
                                   plotN7log,
                                   plotN8log,nrow=2,ncol=4)
# 1.2.4. Plot detected log log concentrations..................
binwidth = 0.35  
plotN1loglog <- ggplot(FiltSp1, aes(x=log(log(PFBS)))) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
plotN2loglog <- ggplot(FiltSp2, aes(x=log(log(PFHpA)))) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
plotN3loglog <- ggplot(FiltSp3, aes(x=log(log(PFHxA)))) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
plotN4loglog <- ggplot(FiltSp4, aes(x=log(log(PFHxS)))) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
plotN5loglog <- ggplot(FiltSp5, aes(x=log(log(PFOA)))) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
plotN6loglog <- ggplot(FiltSp6, aes(x=log(log(PFOA_PFOS)))) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
plotN7loglog <- ggplot(FiltSp7, aes(x=log(log(PFOS)))) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
plotN8loglog <- ggplot(FiltSp8, aes(x=log(log(Total_PFAS)))) + 
  geom_histogram(aes(y=..density..), binwidth=binwidth, colour=histline, fill=histfill) +
  geom_density(alpha=densalph, fill=densfill)
CompactPlotNloglog <- grid.arrange(plotN1loglog,
                            plotN2loglog,
                            plotN3loglog,
                            plotN4loglog,
                            plotN5loglog,
                            plotN6loglog,
                            plotN7loglog,
                            plotN8loglog,nrow=2,ncol=4)
# 1.3 Bernoulli & Normal distribution ---------------------------------------------------
histline = "black"
histfill = "white"
densalph = 0.2 
densfill = "#008B8B" #"#FF6666"
binwidth1 = 8.0  
binwidth2 = 0.15 
binwidth3 = 0.25 
#aes(y=..density..)
plotN8 <- ggplot(FiltSp8, aes(x=Total_PFAS)) + 
  geom_histogram(aes(y=..density..),binwidth=binwidth1, colour=histline, fill=histfill) +
  scale_y_continuous(name = "Density") +
  scale_x_continuous(name = "PFAS [ng/L]") +
  geom_density(alpha=densalph, fill=densfill, color = rgb(80/255,0/255,67/255))
plotN8 <- plotN8 + annotate("text",x=185,y= 0.070,label="b)",size=4,fontface="bold")

plotB8 <- ggplot(dfspat, aes(x=Total_PFAS_B)) + 
  geom_histogram(aes(x=Total_PFAS_B),binwidth=binwidth2,colour=histline, fill=histfill) +
  scale_y_continuous(name = "Count") + 
  scale_x_continuous(name = "PFAS detection", breaks = c(0,1))
plotB8 <- plotB8 + annotate("text",x=1.10,y= 2000,label="a)",size=4,fontface="bold")
plotN8loglog <- ggplot(FiltSp8, aes(x=log(log(Total_PFAS)))) +
  geom_histogram(aes(y=..density..),binwidth=binwidth3,color = histline, fill=histfill) +
  scale_y_continuous(name = "Density") + scale_x_continuous(name = "log(log([PFAS]))") +
  geom_density(aes(),alpha=densalph, fill=densfill)
plotN8loglog <- plotN8loglog + annotate("text",x=1.87,y= 0.775,label="c)",size=4,fontface="bold")  
CompactPlot <- grid.arrange(plotB8,plotN8,plotN8loglog,nrow=1,ncol=3)
###### 2. RANDOM FOREST RESULTS ################################################
# READ metricsSp1 ... Sp8 to build table of the best models during tuning
# To get metrics of all models during testing run:
library(tidyverse)
library(tidymodels)
final_fitSp11 %>% collect_metrics()
final_fitSp12 %>% collect_metrics()
final_fitSp21 %>% collect_metrics()
final_fitSp22 %>% collect_metrics()
final_fitSp31 %>% collect_metrics()
final_fitSp32 %>% collect_metrics()
final_fitSp41 %>% collect_metrics()
final_fitSp42 %>% collect_metrics()
final_fitSp51 %>% collect_metrics()
final_fitSp52 %>% collect_metrics()
final_fitSp61 %>% collect_metrics()
final_fitSp62 %>% collect_metrics()
final_fitSp71 %>% collect_metrics()
final_fitSp72 %>% collect_metrics()
final_fitSp81 %>% collect_metrics()
final_fitSp82 %>% collect_metrics()
################ END CODE ######################################################