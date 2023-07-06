######################## R05AnalysisModel3Clas.R ###############################
# Research Paper: Large-scale Assessment of PFAS Compounds in Drinking Water   #
# Sources using Machine Learning                                               #
# https://doi.org/10.1016/j.watres.2023.120307                                 #
# Nicolas Fernandez, A. Pouyan Nejadhashemi, Christian Loveall                 #
# Department of Biosystems and Agricultural Engineering                        #
# Michigan State University, East Lansing, MI, United States                   #
# Code written and curated by Nicolas Fernandez (OrcID 0000-0001-7979-2941)    #
######################## Description ###########################################
# According to the Paper, once Model 3 is reconstructed following the code     #
# R04ReconstructionModel3Clas.R, this code is used to analyze the results      #
################################################################################
###### 0. IMPORT AND  PREPARE DATA #############################################
# 0.1 Set Working Directory ----------------------------------------------------
wd = 'C:/XXX/YYY/ZZZ/3R'
setwd(wd)
# 0.2 Read workspace with results ----------------------------------------------
dirWS <- paste(wd,"R05Results.RData", sep = "/")
load(dirWS)
###### 1. INDIVIDUAL VARIABLE IMPORTANCE PLOTS #################################
library(vip)
library(tidymodels)
final_fit11 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot11 # 1. Total_PFAS -> 8
final_fit12 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot12 
final_fit21 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot21 # 2. PFOA_PFOS -> 6
final_fit22 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot22 
final_fit31 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot31 # 3. PFHxA -> 3
final_fit32 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot32
final_fit41 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot41 # 4. PFHpA -> 2
final_fit42 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot42
final_fit51 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot51 # 5. PFOA -> 5
final_fit52 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot52
final_fit61 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot61 # 6. PFBS -> 1
final_fit62 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot62
final_fit71 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot71 # 7. PFHxS -> 4
final_fit72 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot72
final_fit81 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot81 # 8. PFOS -> 7
final_fit82 %>% extract_fit_parsnip() %>% vip(num_features = 30) -> plot82
###### 2. COMPACT VARIBLE IMPORTANCE PLOTS #######################################
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
###### 3.  Fix variable names according to paper ####################################
TranslateVars <- function(PlotVariables) {
  test <- PlotVariables
  i <- 1
  for (value in test){
    if(value == "A1"){test[i]<-"M3-A1"}
    if(value == "A2"){test[i]<-"M3-A2"}
    if(value == "A3"){test[i]<-"M3-A3"}
    if(value == "A4"){test[i]<-"M3-A4"}
    if(value == "A5"){test[i]<-"M3-A5"}
    if(value == "A6"){test[i]<-"M3-A6"}
    if(value == "A7"){test[i]<-"M3-A7"}
    if(value == "G1"){test[i]<-"M3-G1"}
    if(value == "G2"){test[i]<-"M3-G2"}
    if(value == "G3"){test[i]<-"M3-G3"}
    if(value == "L1"){test[i]<-"M3-L1"}
    if(value == "L2"){test[i]<-"M3-L2"}
    if(value == "L3"){test[i]<-"M3-L3"}
    if(value == "L4"){test[i]<-"M3-L4"}
    if(value == "L5"){test[i]<-"M3-L5"}
    if(value == "L6"){test[i]<-"M3-L6"}
    if(value == "L7"){test[i]<-"M3-L7"}
    if(value == "L8"){test[i]<-"M3-L8"}
    if(value == "Q1"){test[i]<-"M3-Q1"}
    if(value == "Q2"){test[i]<-"M3-Q2"}
    if(value == "Q3"){test[i]<-"M3-Q3"}
    if(value == "Q4"){test[i]<-"M3-Q4"}
    if(value == "Q5"){test[i]<-"M3-Q5"}
    if(value == "Q6"){test[i]<-"M3-Q6"}
    if(value == "Q7"){test[i]<-"M3-Q7"}
    if(value == "H1"){test[i]<-"M3-H1"}
    if(value == "H2"){test[i]<-"M3-H2"}
    if(value == "H3"){test[i]<-"M3-H3"}
    if(value == "H4"){test[i]<-"M3-H4"}
    if(value == "H5"){test[i]<-"M3-H5"}
    if(value == "H6"){test[i]<-"M3-H6"}
    if(value == "H7"){test[i]<-"M3-H7"}
    if(value == "H8"){test[i]<-"M3-H8"}
    if(value == "H9"){test[i]<-"M3-H9"}
    if(value == "H10"){test[i]<-"M3-H10"}
    if(value == "H11"){test[i]<-"M3-H11"}
    if(value == "H12"){test[i]<-"M3-H12"}
    if(value == "H13"){test[i]<-"M3-H13"}
    if(value == "H14"){test[i]<-"M3-H14"}
    if(value == "H15"){test[i]<-"M3-H15"}
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
###### 4. Make annotations AND get FINAL PLOT ##################################
plot12 <- plot12 + annotate("text",x=2.15,y= 7.07,label="a)",size=4,fontface="bold")
plot22 <- plot22 + annotate("text",x=2.15,y= 0.45,label="b)",size=4,fontface="bold")
plot32 <- plot32 + annotate("text",x=2.15,y= 11.58,label="c)",size=4,fontface="bold")
plot42 <- plot42 + annotate("text",x=2.15,y= 1.09,label="d)",size=4,fontface="bold")
plot52 <- plot52 + annotate("text",x=2.15,y= 9.07,label="e)",size=4,fontface="bold")
plot62 <- plot62 + annotate("text",x=2.15,y= 7.10,label="f)",size=4,fontface="bold")
plot72 <- plot72 + annotate("text",x=2.15,y= 6.59,label="g)",size=4,fontface="bold")
plot82 <- plot82 + annotate("text",x=2.15,y= 27.08,label="h)",size=4,fontface="bold")
CompactPlot <- grid.arrange(plot12,
                            plot22,
                            plot32,
                            plot42,
                            plot52,
                            plot62,
                            plot72,
                            plot82,nrow=2,ncol=4)
###### 5. Compute and print model performances #################################
# 5.0 Collect metrics ----------------------------------------------------------
final_fit11 %>% collect_metrics()
final_fit12 %>% collect_metrics()
final_fit21 %>% collect_metrics()
final_fit22 %>% collect_metrics()
final_fit31 %>% collect_metrics()
final_fit32 %>% collect_metrics()
final_fit41 %>% collect_metrics()
final_fit42 %>% collect_metrics()
final_fit51 %>% collect_metrics()
final_fit52 %>% collect_metrics()
final_fit61 %>% collect_metrics()
final_fit62 %>% collect_metrics()
final_fit71 %>% collect_metrics()
final_fit72 %>% collect_metrics()
final_fit81 %>% collect_metrics()
final_fit82 %>% collect_metrics()
# 5.1 Print metrics ------------------------------------------------------------
# 5.1.1 Species 1 ---------------------------------------------
tunerf_res1 %>% show_best(metric = "accuracy")
tunerf_res1 %>% show_best(metric = "roc_auc")
# 5.1.2 Species 2 ---------------------------------------------
tunerf_res2 %>% show_best(metric = "accuracy")
tunerf_res2 %>% show_best(metric = "roc_auc")
# 5.1.3 Species 3 ---------------------------------------------
tunerf_res3 %>% show_best(metric = "accuracy")
tunerf_res3 %>% show_best(metric = "roc_auc")
# 5.1.4 Species 4 ---------------------------------------------
tunerf_res4 %>% show_best(metric = "accuracy")
tunerf_res4 %>% show_best(metric = "roc_auc")
# 5.1.5 Species 5 ---------------------------------------------
tunerf_res5 %>% show_best(metric = "accuracy")
tunerf_res5 %>% show_best(metric = "roc_auc")
# 5.1.6 Species 6 ---------------------------------------------
tunerf_res6 %>% show_best(metric = "accuracy")
tunerf_res6 %>% show_best(metric = "roc_auc")
# 5.1.7 Species 7 ---------------------------------------------
tunerf_res7 %>% show_best(metric = "accuracy")
tunerf_res7 %>% show_best(metric = "roc_auc")
# 5.1.8 Species 8 ---------------------------------------------
tunerf_res8 %>% show_best(metric = "accuracy")
tunerf_res8 %>% show_best(metric = "roc_auc")
######################### END CODE #############################################
