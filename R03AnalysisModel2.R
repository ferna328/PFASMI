######################## R03AnalysisModel2.R #############################
# Research Paper: Large-scale Assessment of PFAS Compounds in Drinking Water   #
# Sources using Machine Learning                                               #
# https://doi.org/10.1016/j.watres.2023.120307                                 #
# Nicolas Fernandez, A. Pouyan Nejadhashemi, Christian Loveall                 #
# Department of Biosystems and Agricultural Engineering                        #
# Michigan State University, East Lansing, MI, United States                   #
# Code written and curated by Nicolas Fernandez (OrcID 0000-0001-7979-2941)    #
######################## Description ###########################################
# According to the Paper, once Model 2 is reconstructed following the code     #
# R02ReconstructionModel2.R, this code is used to analyze the results          #                                                         #
################################################################################
###### 0. IMPORT AND  PREPARE DATA #############################################
# 0.1 Set Working Directory ----------------------------------------------------
wd = 'C:/XXX/YYY/ZZZ/3R'
setwd(wd)
# 0.2 Read workspace with R02ReconstructionModel2.R results --------------------
dirWS <- paste(wd,"R03Results.RData", sep = "/")
load(dirWS)
###### 1. INDIVIDUAL VARIABLE IMPORTANCE PLOTS #################################
# 1.1 Change plot order according to paper -------------------------------------
library(vip)
library(tidymodels)
final_fit11 %>% extract_fit_parsnip() %>% vip(num_features = 20) -> plot81 # 1. Total_PFAS -> 8
final_fit12 %>% extract_fit_parsnip() %>% vip(num_features = 20) -> plot82 
final_fit21 %>% extract_fit_parsnip() %>% vip(num_features = 20) -> plot61 # 2. PFOA_PFOS -> 6
final_fit22 %>% extract_fit_parsnip() %>% vip(num_features = 20) -> plot62 
final_fit31 %>% extract_fit_parsnip() %>% vip(num_features = 20) -> plot31 # 3. PFHxA -> 3
final_fit32 %>% extract_fit_parsnip() %>% vip(num_features = 20) -> plot32
final_fit41 %>% extract_fit_parsnip() %>% vip(num_features = 20) -> plot21 # 4. PFHpA -> 2
final_fit42 %>% extract_fit_parsnip() %>% vip(num_features = 20) -> plot22
final_fit51 %>% extract_fit_parsnip() %>% vip(num_features = 20) -> plot51 # 5. PFOA -> 5
final_fit52 %>% extract_fit_parsnip() %>% vip(num_features = 20) -> plot52
final_fit61 %>% extract_fit_parsnip() %>% vip(num_features = 20) -> plot11 # 6. PFBS -> 1
final_fit62 %>% extract_fit_parsnip() %>% vip(num_features = 20) -> plot12
final_fit71 %>% extract_fit_parsnip() %>% vip(num_features = 20) -> plot41 # 7. PFHxS -> 4
final_fit72 %>% extract_fit_parsnip() %>% vip(num_features = 20) -> plot42
final_fit81 %>% extract_fit_parsnip() %>% vip(num_features = 20) -> plot71 # 8. PFOS -> 7
final_fit82 %>% extract_fit_parsnip() %>% vip(num_features = 20) -> plot72
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
    if(value == "Hydro1"){test[i]<-"M2-H2"}
    if(value == "Hydro2"){test[i]<-"M2-H1"}
    if(value == "Hydro3"){test[i]<-"M2-H3"}
    if(value == "Hydro4"){test[i]<-"M2-H4"}
    if(value == "Hydro5"){test[i]<-"M2-H5"}
    if(value == "Hydro6"){test[i]<-"M2-H6"}
    if(value == "Soil1"){test[i]<-"M2-S2"}
    if(value == "Soil2"){test[i]<-"M2-S3"}
    if(value == "Soil3"){test[i]<-"M2-S4"}
    if(value == "Soil4"){test[i]<-"M2-S5"}
    if(value == "Soil5"){test[i]<-"M2-S1"}
    if(value == "ImpactFactor1"){test[i]<-"M2-P1"}
    if(value == "ImpactFactor2"){test[i]<-"M2-P2"}
    if(value == "Geol1"){test[i]<-"M2-G1"}
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
plot12 <- plot12 + annotate("text",x=0.99,y= 6,label="a)",size=4,fontface="bold")
plot22 <- plot22 + annotate("text",x=0.99,y= 9,label="b)",size=4,fontface="bold")
plot32 <- plot32 + annotate("text",x=0.99,y= 8,label="c)",size=4,fontface="bold")
plot42 <- plot42 + annotate("text",x=0.99,y= 4,label="d)",size=4,fontface="bold")
plot52 <- plot52 + annotate("text",x=0.99,y= 5,label="e)",size=4,fontface="bold")
plot62 <- plot62 + annotate("text",x=0.99,y= 8,label="f)",size=4,fontface="bold")
plot72 <- plot72 + annotate("text",x=0.99,y= 6,label="g)",size=4,fontface="bold")
plot82 <- plot82 + annotate("text",x=0.99,y=30,label="h)",size=4,fontface="bold")
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
final_fit11 %>% collect_metrics()-> fit81 # 1. Total_PFAS -> 8
final_fit12 %>% collect_metrics()-> fit82 
final_fit21 %>% collect_metrics()-> fit61 # 2. PFOA_PFOS -> 6
final_fit22 %>% collect_metrics()-> fit62
final_fit31 %>% collect_metrics()-> fit31 # 3. PFHxA -> 3
final_fit32 %>% collect_metrics()-> fit32
final_fit41 %>% collect_metrics()-> fit21 # 4. PFHpA -> 2
final_fit42 %>% collect_metrics()-> fit22
final_fit51 %>% collect_metrics()-> fit51 # 5. PFOA -> 5
final_fit52 %>% collect_metrics()-> fit52
final_fit61 %>% collect_metrics()-> fit11 # 6. PFBS -> 1
final_fit62 %>% collect_metrics()-> fit12
final_fit71 %>% collect_metrics()-> fit41 # 7. PFHxS -> 4
final_fit72 %>% collect_metrics()-> fit42
final_fit81 %>% collect_metrics()-> fit71 # 8. PFOS -> 7
final_fit82 %>% collect_metrics()-> fit72
# 5.1 Print PFBS ---------------------------------------------------------------
final_wf61$fit$actions$model$spec$args$mtry
final_wf61$fit$actions$model$spec$args$trees
final_wf61$fit$actions$model$spec$args$min_n 
fit11
final_wf62$fit$actions$model$spec$args$mtry
final_wf62$fit$actions$model$spec$args$trees
final_wf62$fit$actions$model$spec$args$min_n 
fit12
# 5.2 Print PFHpA --------------------------------------------------------------
final_wf41$fit$actions$model$spec$args$mtry
final_wf41$fit$actions$model$spec$args$trees
final_wf41$fit$actions$model$spec$args$min_n 
fit21
final_wf42$fit$actions$model$spec$args$mtry
final_wf42$fit$actions$model$spec$args$trees
final_wf42$fit$actions$model$spec$args$min_n 
fit22
# 5.3 Print PFHxA --------------------------------------------------------------
final_wf31$fit$actions$model$spec$args$mtry
final_wf31$fit$actions$model$spec$args$trees
final_wf31$fit$actions$model$spec$args$min_n 
fit31
final_wf32$fit$actions$model$spec$args$mtry
final_wf32$fit$actions$model$spec$args$trees
final_wf32$fit$actions$model$spec$args$min_n 
fit32
# 5.4 Print PFHxS --------------------------------------------------------------
final_wf71$fit$actions$model$spec$args$mtry
final_wf71$fit$actions$model$spec$args$trees
final_wf71$fit$actions$model$spec$args$min_n 
fit41
final_wf72$fit$actions$model$spec$args$mtry
final_wf72$fit$actions$model$spec$args$trees
final_wf72$fit$actions$model$spec$args$min_n 
fit42
# 5.5 Print PFOA ---------------------------------------------------------------
final_wf51$fit$actions$model$spec$args$mtry
final_wf51$fit$actions$model$spec$args$trees
final_wf51$fit$actions$model$spec$args$min_n 
fit51
final_wf52$fit$actions$model$spec$args$mtry
final_wf52$fit$actions$model$spec$args$trees
final_wf52$fit$actions$model$spec$args$min_n 
fit52
# 5.6 Print PFOA_PFOS ----------------------------------------------------------
final_wf21$fit$actions$model$spec$args$mtry
final_wf21$fit$actions$model$spec$args$trees
final_wf21$fit$actions$model$spec$args$min_n 
fit61
final_wf22$fit$actions$model$spec$args$mtry
final_wf22$fit$actions$model$spec$args$trees
final_wf22$fit$actions$model$spec$args$min_n 
fit62
# 5.7 Print PFOS ---------------------------------------------------------------
final_wf81$fit$actions$model$spec$args$mtry
final_wf81$fit$actions$model$spec$args$trees
final_wf81$fit$actions$model$spec$args$min_n 
fit71
final_wf82$fit$actions$model$spec$args$mtry
final_wf82$fit$actions$model$spec$args$trees
final_wf82$fit$actions$model$spec$args$min_n 
fit72
# 5.8 Print Total_PFAS ---------------------------------------------------------
final_wf11$fit$actions$model$spec$args$mtry
final_wf11$fit$actions$model$spec$args$trees
final_wf11$fit$actions$model$spec$args$min_n 
fit81
final_wf12$fit$actions$model$spec$args$mtry
final_wf12$fit$actions$model$spec$args$trees
final_wf12$fit$actions$model$spec$args$min_n 
fit82
######################## END CODE ##############################################
  
 
 

 
