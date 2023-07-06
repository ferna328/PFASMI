######################## R01ReconstructionModel1.R #############################
# Research Paper: Large-scale Assessment of PFAS Compounds in Drinking Water   #
# Sources using Machine Learning                                               #
# https://doi.org/10.1016/j.watres.2023.120307                                 #
# Nicolas Fernandez, A. Pouyan Nejadhashemi, Christian Loveall                 #
# Department of Biosystems and Agricultural Engineering                        #
# Michigan State University, East Lansing, MI, United States                   #
# Code written and curated by Nicolas Fernandez (OrcID 0000-0001-7979-2941)    #
######################## Description ###########################################
# According to the Paper, this code is used to reconstruct Model 1 following   #
# Hu et al (2016) - https://doi.org/10.1021/acs.estlett.6b00260 - as closely   #
# as possible                                                                  #
################################################################################
###### 0. SETUP AND IMPORT DATA ################################################
# 0.1 Set Working Directory ----------------------------------------------------
wd = 'C:/XXX/YYY/ZZZ/3R'
setwd(wd)
# 0.2 Read Input Information ---------------------------------------------------
# CSV file with dependent variables (PFAS records)
dirDep = 'C:/XXX/YYY/ZZZ/DataExp1V2Dep.csv'
# CSV file with explanatory variables (number of airports etc...)
dirExp = 'C:/XXX/YYY/ZZZ/DataExp1V2Exp.csv'
Dep <- read.csv(file = dirDep) # Dependent Variables
Exp <- read.csv(file = dirExp) # Explanatory Variables
rm(dirDep, dirExp, wd) # clean innecesary variables
###### 1. EXPLORE STATISTICAL DIFFERENCES ######################################
# 1.1 Prepare Input Data -------------------------------------------------------
# 1.1.1 Turn dependent variables into detect/non-detect .............
library(data.table)
DepBin <- copy(Dep) # Make copy of dependent variables
# List of PFAS Species 
PFASspec <- list("Total_PFAS", "PFOA_PFOS_", "PFBA", "PFPeA", "PFHxA", "PFHpA",
                 "PFOA", "PFNA", "PFDA", "PFUnDA", "PFDoDA", "PFTrDA", "PFTeDA",
                 "PFDS", "PFBS", "PFPeS", "PFHxS", "PFHpS", "PFOS", "PFNS",
                 "FOSA", "F4_2_FTS", "F6_2_FTS", "F8_2_FTS", "EtFOSAA", 
                 "MeFOSAA", "F9Cl_PF3ON", "F11Cl_PF3O", "ADONA", "HFPO_DA")
# Loop to convert into binary data
for(var in PFASspec){
  for(i in 1:nrow(DepBin)){
    if(DepBin[[var]][i]==0){DepBin[[var]][i] <- 0}else{DepBin[[var]][i] <- 1}}}
# Loop to convert into categorical
for(var in PFASspec){DepBin[[var]] <- as.factor(DepBin[[var]])}
# 1.1.2 Keep relevant information in dep. and ind. vars .............
# Make compact versions of the variables
library(dplyr)
CDep <- subset(Dep, select = -c(OID_,Join_Count,JOIN_FID,huc8,name))
CDep <- rename(CDep, ID = TARGET_FID)
CDepBin <- subset(DepBin, select = -c(OID_,Join_Count,JOIN_FID,huc8,name))
CDepBin <- rename(CDepBin, ID = TARGET_FID)
CExp <- subset(Exp, select = -c(X,HUC08code,HUC08name))
# Clean unnecessary variables
rm(i, var)
# Delete undetected PFAS or observed less than 10 times
CDep <- subset(CDep, select = -c(ADONA, F11Cl_PF3O, F4_2_FTS, F8_2_FTS, 
                                 F9Cl_PF3ON, FOSA, HFPO_DA, PFBA, PFDA, PFDoDA,
                                 PFDS, PFHpS, PFNS, PFPeA, PFPeS, PFTeDA, 
                                 PFTrDA, PFUnDA, F6_2_FTS, PFNA, EtFOSAA, 
                                 MeFOSAA))
CDepBin <- subset(CDepBin, select = -c(ADONA, F11Cl_PF3O, F4_2_FTS, F8_2_FTS,
                                       F9Cl_PF3ON, FOSA, HFPO_DA, PFBA, PFDA,
                                       PFDoDA, PFDS, PFHpS, PFNS, PFPeA, PFPeS, 
                                       PFTeDA, PFTrDA, PFUnDA, F6_2_FTS, PFNA, 
                                       EtFOSAA, MeFOSAA))
# 1.1.3 Prepare data for boxplots ...................................
# Stack variables
library(tidyr)
CDepStack <- pivot_longer(CDep, cols = 2:9,
                          names_to = "PFASspec", 
                          values_to = "Conc")
CDepBinStack <- pivot_longer(CDepBin, cols = 2:9,
                             names_to = "PFASspec",
                             values_to = "Detect")
# Delete unnecessary columns 
CDepStack <- select(CDepStack, -ID)
CDepStack <- select(CDepStack, -PFASspec)
# Bind columns
DataStack <- bind_cols(CDepBinStack, CDepStack)
# Add counts to data
DataStack <- left_join(DataStack,CExp,by="ID")
### 1.2 Make boxplots ----------------------------------------------------------
library(ggplot2)
DataStack$PFASspec <- as.factor(DataStack$PFASspec)
ggp01 <- ggplot(DataStack, aes(x=PFASspec, y=Coun1In, fill=Detect)) +
  geom_boxplot() + facet_wrap(~PFASspec, scale="free")
ggp02 <- ggplot(DataStack, aes(x=PFASspec, y=Coun2WW, fill=Detect)) + 
  geom_boxplot() + facet_wrap(~PFASspec, scale="free")
ggp03 <- ggplot(DataStack, aes(x=PFASspec, y=Coun3Mi, fill=Detect)) + 
  geom_boxplot() + facet_wrap(~PFASspec, scale="free")
ggp04 <- ggplot(DataStack, aes(x=PFASspec, y=Coun4AP, fill=Detect)) + 
  geom_boxplot() + facet_wrap(~PFASspec, scale="free")
ggp01
ggp02
ggp03
ggp04
### 1.3 Assess significance with t-test ----------------------------------------
Species <- list("PFOA","PFOS","PFHxS","PFHpA","Total_PFAS",
                "PFOA_PFOS_","PFHxA","PFBS")
Counts <- list("Coun1In","Coun2WW","Coun3Mi","Coun4AP")
matPvals1 <- matrix(0,length(Species),length(Counts)) # Matrix P val two sided
matPvals2 <- matrix(0,length(Species),length(Counts)) # Matrix P values greater
matPvals3 <- matrix(0,length(Species),length(Counts)) # Matrix P values less
matLenX <- matrix(0,length(Species),length(Counts)) # Matrix of X lengths
matLenY <- matrix(0,length(Species),length(Counts)) # Matrix of Y lengths
i <- 1 # Loop to fill data lengths and P values of T tests
for(spe in Species){
  j <- 1
  for(cou in Counts){
    X <- subset(DataStack, PFASspec == spe & Detect == 0)[[cou]]
    Y <- subset(DataStack, PFASspec == spe & Detect == 1)[[cou]]
    lenX <- length(X)
    matLenX[i,j] <- lenX
    lenY <- length(Y)
    matLenY[i,j] <- lenY
    tres1 <- t.test(x = X, y = Y, alternative = "two.sided")
    tres2 <- t.test(x = X, y = Y, alternative = "greater")
    tres3 <- t.test(x = X, y = Y, alternative = "less")
    matPvals1[i,j] <- tres1$p.value
    matPvals2[i,j] <- tres2$p.value
    matPvals3[i,j] <- tres3$p.value
    j <- j+1}
  i <- i+1}
# Significance matrix
matSig1 <- matPvals1 # Matrix of significance 2 sided t test
for (r in 1:nrow(matPvals1)){
  for (c in 1:ncol(matPvals1)){
    if(matPvals1[r,c] < 0.05){matSig1[r,c] <- 1} else{matSig1[r,c] <- 0}}}  
matSig2 <- matPvals2 # Matrix of significance 1 sided test 1
for (r in 1:nrow(matPvals2)){
  for (c in 1:ncol(matPvals2)){
    if(matPvals2[r,c] > 0.95){matSig2[r,c] <- 1} else{matSig2[r,c] <- 0}}}
matSig3 <- matPvals3 # Matrix of significance 1 sided test 2
for (r in 1:nrow(matPvals3)){
  for (c in 1:ncol(matPvals3)){
    if(matPvals3[r,c] < 0.05){matSig3[r,c] <- 1} else{matSig3[r,c] <- 0}}}
Significant <- matSig1*matSig2*matSig3
### 1.4 Make boxplots for paper ------------------------------------------------
library(gridExtra)
CompactPlot1 <- ggplot(DataStack, aes(x=PFASspec, y=Coun1In, fill=Detect)) + 
  geom_boxplot() + labs(x = "", y = "M1-1") + ylim(0,15) + theme(legend.position = "right",
    axis.text.x = element_blank(),
    axis.ticks = element_blank()) + 
  scale_fill_discrete(name = "", labels = c("Undetected", "Detected")) +
  annotate("text",x=0.53,y=7.75,label="a)",size=4,fontface="bold") +
  annotate("text",x=1,y=14,label=paste("p =",toString(round(matPvals1[8,1], digits=3))),size=3,fontface="bold") +
  annotate("text",x=2,y=14,label=paste("p =",toString(round(matPvals1[4,1], digits=3))),size=2.7) +
  annotate("text",x=3,y=14,label=paste("p =",toString(round(matPvals1[7,1], digits=3))),size=3,fontface="bold") +
  annotate("text",x=4,y=14,label=paste("p =",toString(round(matPvals1[3,1], digits=3))),size=3,fontface="bold") +
  annotate("text",x=5,y=14,label=paste("p =",toString(round(matPvals1[1,1], digits=3))),size=3,fontface="bold") +
  annotate("text",x=6,y=14,label=paste("p =",toString(round(matPvals1[6,1], digits=3))),size=3,fontface="bold") +
  annotate("text",x=7,y=14,label=paste("p =",toString(round(matPvals1[2,1], digits=3))),size=3,fontface="bold") +
  annotate("text",x=8,y=14,label=paste("p =",toString(round(matPvals1[5,1], digits=3))),size=3,fontface="bold") 
CompactPlot2 <- ggplot(DataStack, aes(x=PFASspec, y=Coun2WW, fill=Detect)) + 
  geom_boxplot() + labs(x = "", y = "M1-2") + ylim(0,80) + theme(legend.position = "right",
    axis.text.x = element_blank(),
    axis.ticks = element_blank()) + 
  scale_fill_discrete(name = "", labels = c("Undetected", "Detected")) +
  annotate("text",x=0.53,y=45,label="b)",size=4,fontface="bold") +
  annotate("text",x=1,y=75,label=paste("p =",toString(round(matPvals1[8,2], digits=4))),size=3,fontface="bold") +
  annotate("text",x=2,y=75,label=paste("p =",toString(round(matPvals1[4,2], digits=3))),size=3,fontface="bold") +
  annotate("text",x=3,y=75,label=paste("p =",toString(round(matPvals1[7,2], digits=3))),size=3,fontface="bold") +
  annotate("text",x=4,y=75,label=paste("p =",toString(round(matPvals1[3,2], digits=3))),size=3,fontface="bold") +
  annotate("text",x=5,y=75,label=paste("p =",toString(round(matPvals1[1,2], digits=4))),size=3,fontface="bold") +
  annotate("text",x=6,y=75,label=paste("p =",toString(round(matPvals1[6,2], digits=4))),size=3,fontface="bold") +
  annotate("text",x=7,y=75,label=paste("p =",toString(round(matPvals1[2,2], digits=3))),size=3,fontface="bold") +
  annotate("text",x=8,y=75,label=paste("p =",toString(round(matPvals1[5,2], digits=4))),size=3,fontface="bold")
CompactPlot3 <- ggplot(DataStack, aes(x=PFASspec, y=Coun3Mi, fill=Detect)) + 
  geom_boxplot() + labs(x = "", y = "M1-3") + ylim(0,8) + theme(legend.position = "right",
    axis.text.x = element_blank(),
    axis.ticks = element_blank()) + 
  scale_fill_discrete(name = "", labels = c("Undetected", "Detected")) +
  annotate("text",x=0.53,y=4.55,label="c)",size=4,fontface="bold") +
  annotate("text",x=1,y=7.5,label=paste("p =",toString(round(matPvals1[8,3], digits=3))),size=2.7) +
  annotate("text",x=2,y=7.5,label=paste("p =",toString(round(matPvals1[4,3], digits=3))),size=2.7) +
  annotate("text",x=3,y=7.5,label=paste("p =",toString(round(matPvals1[7,3], digits=3))),size=2.7) +
  annotate("text",x=4,y=7.5,label=paste("p =",toString(round(matPvals1[3,3], digits=3))),size=2.7) +
  annotate("text",x=5,y=7.5,label=paste("p =",toString(round(matPvals1[1,3], digits=3))),size=2.7) +
  annotate("text",x=6,y=7.5,label=paste("p =",toString(round(matPvals1[6,3], digits=3))),size=2.7) +
  annotate("text",x=7,y=7.5,label=paste("p =",toString(round(matPvals1[2,3], digits=3))),size=2.7) +
  annotate("text",x=8,y=7.5,label=paste("p =",toString(round(matPvals1[5,3], digits=3))),size=2.7)
CompactPlot4 <- ggplot(DataStack, aes(x=PFASspec, y=Coun4AP, fill=Detect)) + 
  geom_boxplot() + labs(x = "PFAS Species", y = "M1-4") +  ylim(0,5) + 
  theme(axis.ticks = element_blank(),legend.position = "right")+ 
  scale_fill_discrete(name = "", labels = c("Undetected", "Detected")) +
  annotate("text",x=0.53,y=2.75,label="d)",size=4,fontface="bold") +
  annotate("text",x=1,y=4.5,label=paste("p =",toString(round(matPvals1[8,4], digits=3))),size=2.7) +
  annotate("text",x=2,y=4.5,label=paste("p =",toString(round(matPvals1[4,4], digits=3))),size=2.7) +
  annotate("text",x=3,y=4.5,label=paste("p =",toString(round(matPvals1[7,4], digits=3))),size=2.7) +
  annotate("text",x=4,y=4.5,label=paste("p =",toString(round(matPvals1[3,4], digits=3))),size=2.7) +
  annotate("text",x=5,y=4.5,label=paste("p =",toString(round(matPvals1[1,4], digits=3))),size=2.7) +
  annotate("text",x=6,y=4.5,label=paste("p =",toString(round(matPvals1[6,4], digits=3))),size=2.7) +
  annotate("text",x=7,y=4.5,label=paste("p =",toString(round(matPvals1[2,4], digits=3))),size=2.7) +
  annotate("text",x=8,y=4.5,label=paste("p =",toString(round(matPvals1[5,4], digits=3))),size=2.7)
CompactPlot <- grid.arrange(CompactPlot1,CompactPlot2,CompactPlot3,CompactPlot4,nrow=4)
###### 2. Spatial Regression ###################################################
### 2.1 See distribution of individual PFAS ------------------------------------
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)
ggp11 <- ggplot(DataStack, aes(x=Conc, fill=PFASspec)) + 
  geom_histogram(alpha = 0.6, binwidth = 2.0) + facet_wrap(~PFASspec,scale="free")
ggp11
ggp12 <- ggplot(DataStack, aes(x=log(Conc), fill=PFASspec)) + 
  geom_histogram(alpha = 0.6, binwidth = 0.3) + facet_wrap(~PFASspec,scale="free")
ggp12
### 2.2 Compute Ordinary Least Squares Regression ------------------------------
# Export data
# Dependent variables in absolute terms
#write.csv(CDep,"C:\\XXX\\YYY\\ZZZ\\DepVarsAbs.csv")
# Dependent variables in binary terms
#write.csv(CDepBin,"C:\\XXX\\YYY\\ZZZ\\DepVarsBin.csv")
# Dependent variables in natural log terms
#CDepLog <- log(CDep)
#CDepLog$ID <- exp(CDepLog$ID)
#write.csv(CDepLog,"C:\\XXX\\YYY\\ZZZ\\DepVarsLog.csv")
# Explanatory variables
#write.csv(CExp,"C:\\XXX\\YYY\\ZZZ\\ExpVarsAbs.csv")
# Once exported, OLS regression was performed in ARCGIS Pro
### 2.3 Compute Moran's I statistic --------------------------------------------
# Morans I was computed in ArcGIS pro.
### 2.4 Implement spatial regression model -------------------------------------
# Spatial regression = OLS because no spatial autocorrelation according to Moran
############################### END CODE #######################################

