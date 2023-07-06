######################## R09AnalysisModel4.R ###################################
# Research Paper: Large-scale Assessment of PFAS Compounds in Drinking Water   #
# Sources using Machine Learning                                               #
# https://doi.org/10.1016/j.watres.2023.120307                                 #
# Nicolas Fernandez, A. Pouyan Nejadhashemi, Christian Loveall                 #
# Department of Biosystems and Agricultural Engineering                        #
# Michigan State University, East Lansing, MI, United States                   #
# Code written and curated by Nicolas Fernandez (OrcID 0000-0001-7979-2941)    #
######################## Description ###########################################
# According to the Paper, once Model 4 is reconstructed following the code     #
# R08ReconstructionModel4.R, this code is used to analyze the results          #
################################################################################
###### 0. IMPORT AND  PREPARE DATA #############################################
# 0.0 Set Working Directory ----------------------------------------------------
wd = 'C:/XXX/YYY/ZZZ/3R'
setwd(wd)
# 0.1 Read workspace with results ----------------------------------------------
dirWS <- paste(wd,"R04Results.RData", sep = "/")
load(dirWS)
# 0.2 Read Dictionary with NAICS code meaning ----------------------------------
DirInfo <- 'C:/XXX/YYY/ZZZ/PFASMIGWdatabaseV3.xlsx'
library('readxl')
DictNAICS <- read_excel(DirInfo, sheet = 'Dictionary')
###### 1. INDIVIDUAL VARIABLE IMPORTANCE PLOTS #################################
# 1.1. Save plots in the order of the paper's plot -----------------------------
library(vip)
library(tidymodels)
final_fit11 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot81 # 1. Total_PFAS -> 8
final_fit12 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot82 
final_fit21 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot61 # 2. PFOA_PFOS -> 6
final_fit22 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot62 
final_fit31 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot31 # 3. PFHxA -> 3
final_fit32 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot32
final_fit41 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot21 # 4. PFHpA -> 2
final_fit42 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot22
final_fit51 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot51 # 5. PFOA -> 5
final_fit52 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot52
final_fit61 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot11 # 6. PFBS -> 1
final_fit62 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot12
final_fit71 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot41 # 7. PFHxS -> 4
final_fit72 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot42
final_fit81 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot71 # 8. PFOS -> 7
final_fit82 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot72
###### 2. COMPACT VARIBLE IMPORTANCE PLOTS #####################################
library(gridExtra)
plot11 <- plot11 + theme(axis.text.x = element_blank(),axis.ticks = element_blank(),axis.title.x=element_blank())
plot12 <- plot12 + theme(axis.text.x = element_blank(),axis.ticks = element_blank(),axis.title.x=element_blank())
plot21 <- plot21 + theme(axis.text.x = element_blank(),axis.ticks = element_blank(),axis.title.x=element_blank())
plot22 <- plot22 + theme(axis.text.x = element_blank(),axis.ticks = element_blank(),axis.title.x=element_blank())
plot31 <- plot31 + theme(axis.text.x = element_blank(),axis.ticks = element_blank(),axis.title.x=element_blank())
plot32 <- plot32 + theme(axis.text.x = element_blank(),axis.ticks = element_blank(),axis.title.x=element_blank())
plot41 <- plot41 + theme(axis.text.x = element_blank(),axis.ticks = element_blank(),axis.title.x=element_blank())
plot42 <- plot42 + theme(axis.text.x = element_blank(),axis.ticks = element_blank(),axis.title.x=element_blank())
plot51 <- plot51 + theme(axis.text.x = element_blank(),axis.ticks = element_blank(),axis.title.x=element_blank())
plot52 <- plot52 + theme(axis.text.x = element_blank(),axis.ticks = element_blank(),axis.title.x=element_blank())
plot61 <- plot61 + theme(axis.text.x = element_blank(),axis.ticks = element_blank(),axis.title.x=element_blank())
plot62 <- plot62 + theme(axis.text.x = element_blank(),axis.ticks = element_blank(),axis.title.x=element_blank())
plot71 <- plot71 + theme(axis.text.x = element_blank(),axis.ticks = element_blank(),axis.title.x=element_blank())
plot72 <- plot72 + theme(axis.text.x = element_blank(),axis.ticks = element_blank(),axis.title.x=element_blank())
plot81 <- plot81 + theme(axis.text.x = element_blank(),axis.ticks = element_blank(),axis.title.x=element_blank())
plot82 <- plot82 + theme(axis.text.x = element_blank(),axis.ticks = element_blank(),axis.title.x=element_blank())
CompactPlot <- grid.arrange(plot11,plot12,
                            plot21,plot22,
                            plot31,plot32,
                            plot41,plot42,
                            plot51,plot52,
                            plot61,plot62,
                            plot71,plot72,
                            plot81,plot82,nrow=8,ncol=2)
###### 3.  Fix variable names according to paper ###############################
TranslateVars <- function(PlotVariables) {
  test <- PlotVariables
  i <- 1
  for (value in test){
    if(value == "DisNeaConSou"){test[i]<-"M4-S1"}
    if(value == "DisNeaLandfil"){test[i]<-"M4-S2"}
    if(value == "DisNeaAirport"){test[i]<-"M4-S3"}
    if(value == "DisNeaFireSt"){test[i]<-"M4-S4"}
    if(value == "Buf101PUA"){test[i]<-"M4-L1"}
    if(value == "DistInd1"){test[i]<-"M4-I1"}
    if(value == "DistInd2"){test[i]<-"M4-I2"}
    if(value == "DistInd3"){test[i]<-"M4-I3"}
    if(value == "DistInd4"){test[i]<-"M4-I4"}
    if(value == "DistInd5"){test[i]<-"M4-I5"}
    if(value == "DistInd6"){test[i]<-"M4-I6"}
    if(value == "DistInd7"){test[i]<-"M4-I7"}
    if(value == "DistInd8"){test[i]<-"M4-I8"}
    if(value == "DistInd9"){test[i]<-"M4-I9"}
    if(value == "DistInd10"){test[i]<-"M4-I10"}
    if(value == "DistInd11"){test[i]<-"M4-I11"}
    if(value == "DistInd12"){test[i]<-"M4-I12"}
    if(value == "DistInd13"){test[i]<-"M4-I13"}
    if(value == "DistInd14"){test[i]<-"M4-I14"}
    if(value == "DistInd15"){test[i]<-"M4-I15"}
    if(value == "DistInd16"){test[i]<-"M4-I16"}
    if(value == "DistInd17"){test[i]<-"M4-I17"}
    if(value == "DistInd18"){test[i]<-"M4-I18"}
    if(value == "DistInd19"){test[i]<-"M4-I19"}
    if(value == "DistInd20"){test[i]<-"M4-I20"}
    if(value == "DistInd21"){test[i]<-"M4-I21"}
    if(value == "DistInd22"){test[i]<-"M4-I22"}
    if(value == "DistInd23"){test[i]<-"M4-I23"}
    if(value == "DistInd24"){test[i]<-"M4-I24"}
    if(value == "DistInd25"){test[i]<-"M4-I25"}
    if(value == "DistInd26"){test[i]<-"M4-I26"}
    if(value == "DistInd27"){test[i]<-"M4-I27"}
    if(value == "DistInd28"){test[i]<-"M4-I28"}
    if(value == "DistInd29"){test[i]<-"M4-I29"}
    if(value == "DistInd30"){test[i]<-"M4-I30"}
    if(value == "DistInd31"){test[i]<-"M4-I31"}
    if(value == "DistInd32"){test[i]<-"M4-I32"}
    if(value == "DistInd33"){test[i]<-"M4-I33"}
    if(value == "DistInd34"){test[i]<-"M4-I34"}
    if(value == "DistInd35"){test[i]<-"M4-I35"}
    if(value == "DistInd36"){test[i]<-"M4-I36"}
    if(value == "DistInd37"){test[i]<-"M4-I37"}
    if(value == "DistInd38"){test[i]<-"M4-I38"}
    if(value == "DistInd39"){test[i]<-"M4-I39"}
    if(value == "DistInd40"){test[i]<-"M4-I40"}
    if(value == "DistInd41"){test[i]<-"M4-I41"}
    if(value == "DistInd42"){test[i]<-"M4-I42"}
    if(value == "DistInd43"){test[i]<-"M4-I43"}
    if(value == "DistInd44"){test[i]<-"M4-I44"}
    i <- i+1 
  }
  return(test)
}
plot11$data$Variable <- TranslateVars(plot11$data$Variable)
plot12$data$Variable <- TranslateVars(plot12$data$Variable)
plot21$data$Variable <- TranslateVars(plot21$data$Variable)
plot22$data$Variable <- TranslateVars(plot22$data$Variable)
plot31$data$Variable <- TranslateVars(plot31$data$Variable)
plot32$data$Variable <- TranslateVars(plot32$data$Variable)
plot41$data$Variable <- TranslateVars(plot41$data$Variable)
plot42$data$Variable <- TranslateVars(plot42$data$Variable)
plot51$data$Variable <- TranslateVars(plot51$data$Variable)
plot52$data$Variable <- TranslateVars(plot52$data$Variable)
plot61$data$Variable <- TranslateVars(plot61$data$Variable)
plot62$data$Variable <- TranslateVars(plot62$data$Variable)
plot71$data$Variable <- TranslateVars(plot71$data$Variable)
plot72$data$Variable <- TranslateVars(plot72$data$Variable)
plot81$data$Variable <- TranslateVars(plot81$data$Variable)
plot82$data$Variable <- TranslateVars(plot82$data$Variable)
CompactPlot <- grid.arrange(plot11,
                            plot21,
                            plot31,
                            plot41,
                            plot51,
                            plot61,
                            plot71,
                            plot81,nrow=2,ncol=4)
CompactPlot <- grid.arrange(plot12,
                            plot22,
                            plot32,
                            plot42,
                            plot52,
                            plot62,
                            plot72,
                            plot82,nrow=2,ncol=4)
###### 4. Make annotations AND FINAL PLOT ######################################
plot12 <- plot12 + annotate("text",x=2.15,y= 0.07,label="a)",size=4,fontface="bold")
plot22 <- plot22 + annotate("text",x=2.15,y= 0.12,label="b)",size=4,fontface="bold")
plot32 <- plot32 + annotate("text",x=2.15,y= 0.08,label="c)",size=4,fontface="bold")
plot42 <- plot42 + annotate("text",x=2.15,y= 0.09,label="d)",size=4,fontface="bold")
plot52 <- plot52 + annotate("text",x=2.15,y= 0.07,label="e)",size=4,fontface="bold")
plot62 <- plot62 + annotate("text",x=2.15,y= 0.10,label="f)",size=4,fontface="bold")
plot72 <- plot72 + annotate("text",x=2.15,y= 0.09,label="g)",size=4,fontface="bold")
plot82 <- plot82 + annotate("text",x=2.15,y= 0.08,label="h)",size=4,fontface="bold")
CompactPlot <- grid.arrange(plot12,
                            plot22,
                            plot32,
                            plot42,
                            plot52,
                            plot62,
                            plot72,
                            plot82,nrow=2,ncol=4)
######################### END CODE #############################################
