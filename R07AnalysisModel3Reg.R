######################## R07AnalysisModel3Reg.R ################################
# Research Paper: Large-scale Assessment of PFAS Compounds in Drinking Water   #
# Sources through Machine Learning                                             #
# Nicolas Fernandez, A. Pouyan Nejadhashemi, Christian Loveall                 #
# Department of Biosystems and Agricultural Engineering                        #
# Michigan State University, East Lansing, MI, United States                   #
# Code written and curated by Nicolas Fernandez (OrcID 0000-0001-7979-2941)    #
######################## Description ###########################################
# According to the Paper, once Model 3 is reconstructed following the code     #
# R06ReconstructionModel3Reg.R, this code is used to analyze the results       #
################################################################################
###### 0. IMPORT AND  PREPARE DATA #############################################
# 0.1 Set Working Directory ----------------------------------------------------
wd = 'C:/XXX/YYY/ZZZ/3R'
setwd(wd)
# 0.2 Read workspace with results ----------------------------------------------
dirWS <- paste(wd,"R05ResultsRegLog.RData", sep = "/")
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
###### 4. Make annotations AND FINAL PLOT ######################################
plot12 <- plot12 + annotate("text",x=2.15,y= 7,label="a)",size=4,fontface="bold")
plot22 <- plot22 + annotate("text",x=2.15,y= 0.2,label="b)",size=4,fontface="bold")
plot32 <- plot32 + annotate("text",x=2.15,y= 3.8,label="c)",size=4,fontface="bold")
plot42 <- plot42 + annotate("text",x=2.15,y= 4,label="d)",size=4,fontface="bold")
plot52 <- plot52 + annotate("text",x=2.15,y= 5.2,label="e)",size=4,fontface="bold")
plot62 <- plot62 + annotate("text",x=2.15,y= 9,label="f)",size=4,fontface="bold")
plot72 <- plot72 + annotate("text",x=2.15,y= 7,label="g)",size=4,fontface="bold")
plot82 <- plot82 + annotate("text",x=2.15,y= 20,label="h)",size=4,fontface="bold")
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
tunerf_res1 %>% show_best(metric = "rmse")
tunerf_res1 %>% show_best(metric = "rsq")
# 5.1.2 Species 2 ---------------------------------------------
tunerf_res2 %>% show_best(metric = "rmse")
tunerf_res2 %>% show_best(metric = "rsq")
# 5.1.3 Species 3 ---------------------------------------------
tunerf_res3 %>% show_best(metric = "rmse")
tunerf_res3 %>% show_best(metric = "rsq")
# 5.1.4 Species 4 ---------------------------------------------
tunerf_res4 %>% show_best(metric = "rmse")
tunerf_res4 %>% show_best(metric = "rsq")
# 5.1.5 Species 5 ---------------------------------------------
tunerf_res5 %>% show_best(metric = "rmse")
tunerf_res5 %>% show_best(metric = "rsq")
# 5.1.6 Species 6 ---------------------------------------------
tunerf_res6 %>% show_best(metric = "rmse")
tunerf_res6 %>% show_best(metric = "rsq")
# 5.1.7 Species 7 ---------------------------------------------
tunerf_res7 %>% show_best(metric = "rmse")
tunerf_res7 %>% show_best(metric = "rsq")
# 5.1.8 Species 8 ---------------------------------------------
tunerf_res8 %>% show_best(metric = "rmse")
tunerf_res8 %>% show_best(metric = "rsq")
###### 6. Compute Spearman correlation ############################
##### FOR TEST DATA ###############################################
# 6.1 Species 1 PFBS---------------------------------------------
Pred11 <- pull(final_fit11$.predictions[[1]],var = .pred)
Spear11 <- cor.test(x=DataTest1$PFBS, y=Pred11, method = 'spearman')
print(Spear11$estimate[[1]])
Pred12 <- pull(final_fit12$.predictions[[1]],var = .pred)
Spear12 <- cor.test(x=DataTest1$PFBS, y=Pred12, method = 'spearman')
print(Spear12$estimate[[1]])
# 6.2 Species 2 PFHpA---------------------------------------------
Pred21 <- pull(final_fit21$.predictions[[1]],var = .pred)
Spear21 <- cor.test(x=DataTest2$PFHpA, y=Pred21, method = 'spearman')
print(Spear21$estimate[[1]])
Pred22 <- pull(final_fit12$.predictions[[1]],var = .pred)
Spear22 <- cor.test(x=DataTest2$PFHpA, y=Pred12, method = 'spearman')
print(Spear22$estimate[[1]])
# 6.3 Species 3 PFHxA---------------------------------------------
Pred31 <- pull(final_fit31$.predictions[[1]],var = .pred)
Spear31 <- cor.test(x=DataTest3$PFHxA, y=Pred31, method = 'spearman')
print(Spear31$estimate[[1]])
Pred32 <- pull(final_fit32$.predictions[[1]],var = .pred)
Spear32 <- cor.test(x=DataTest3$PFHxA, y=Pred32, method = 'spearman')
print(Spear32$estimate[[1]])
# 6.4 Species 4 PFHxS---------------------------------------------
Pred41 <- pull(final_fit41$.predictions[[1]],var = .pred)
Spear41 <- cor.test(x=DataTest4$PFHxS, y=Pred41, method = 'spearman')
print(Spear41$estimate[[1]])
Pred42 <- pull(final_fit42$.predictions[[1]],var = .pred)
Spear42 <- cor.test(x=DataTest4$PFHxS, y=Pred42, method = 'spearman')
print(Spear42$estimate[[1]])
# 6.5 Species 5 PFOA---------------------------------------------
Pred51 <- pull(final_fit51$.predictions[[1]],var = .pred)
Spear51 <- cor.test(x=DataTest5$PFOA, y=Pred51, method = 'spearman')
print(Spear51$estimate[[1]])
Pred52 <- pull(final_fit52$.predictions[[1]],var = .pred)
Spear52 <- cor.test(x=DataTest5$PFOA, y=Pred52, method = 'spearman')
print(Spear52$estimate[[1]])
# 6.6 Species 6 PFOA_PFOS_---------------------------------------------
Pred61 <- pull(final_fit61$.predictions[[1]],var = .pred)
Spear61 <- cor.test(x=DataTest6$PFOA_PFOS_, y=Pred61, method = 'spearman')
print(Spear61$estimate[[1]])
Pred62 <- pull(final_fit62$.predictions[[1]],var = .pred)
Spear62 <- cor.test(x=DataTest6$PFOA_PFOS_, y=Pred62, method = 'spearman')
print(Spear62$estimate[[1]])
# 6.7 Species 7 PFOS---------------------------------------------
Pred71 <- pull(final_fit71$.predictions[[1]],var = .pred)
Spear71 <- cor.test(x=DataTest7$PFOS, y=Pred71, method = 'spearman')
print(Spear71$estimate[[1]])
Pred72 <- pull(final_fit72$.predictions[[1]],var = .pred)
Spear72 <- cor.test(x=DataTest7$PFOS, y=Pred72, method = 'spearman')
print(Spear72$estimate[[1]])
# 6.8 Species 8 Total_PFAS---------------------------------------------
Pred81 <- pull(final_fit81$.predictions[[1]],var = .pred)
Spear81 <- cor.test(x=DataTest8$Total_PFAS, y=Pred81, method = 'spearman')
print(Spear81$estimate[[1]])
Pred82 <- pull(final_fit82$.predictions[[1]],var = .pred)
Spear82 <- cor.test(x=DataTest8$Total_PFAS, y=Pred82, method = 'spearman')
print(Spear82$estimate[[1]])
##### FOR TRAINING DATA ###############################################
# 6.1 Species 1 PFBS---------------------------------------------
Pred11 <- pull(predict(final_rf11,DataTrain1),var = .pred)
Spear11 <- cor.test(x=DataTrain1$PFBS, y=Pred11, method = 'spearman')
print(Spear11$estimate[[1]])
Pred12 <- pull(predict(final_rf12,DataTrain1),var = .pred)
Spear12 <- cor.test(x=DataTrain1$PFBS, y=Pred12, method = 'spearman')
print(Spear12$estimate[[1]])
# 6.2 Species 2 PFHpA---------------------------------------------
Pred21 <- pull(predict(final_rf21,DataTrain2),var = .pred)
Spear21 <- cor.test(x=DataTrain2$PFHpA, y=Pred21, method = 'spearman')
print(Spear21$estimate[[1]])
Pred22 <- pull(predict(final_rf22,DataTrain2),var = .pred)
Spear22 <- cor.test(x=DataTrain2$PFHpA, y=Pred22, method = 'spearman')
print(Spear22$estimate[[1]])
# 6.3 Species 3 PFHxA---------------------------------------------
Pred31 <- pull(predict(final_rf31,DataTrain3),var = .pred)
Spear31 <- cor.test(x=DataTrain3$PFHxA, y=Pred31, method = 'spearman')
print(Spear31$estimate[[1]])
Pred32 <- pull(predict(final_rf32,DataTrain3),var = .pred)
Spear32 <- cor.test(x=DataTrain3$PFHxA, y=Pred32, method = 'spearman')
print(Spear32$estimate[[1]])
# 6.4 Species 4 PFHxS---------------------------------------------
Pred41 <- pull(predict(final_rf41,DataTrain4),var = .pred)
Spear41 <- cor.test(x=DataTrain4$PFHxS, y=Pred41, method = 'spearman')
print(Spear41$estimate[[1]])
Pred42 <- pull(predict(final_rf42,DataTrain4),var = .pred)
Spear42 <- cor.test(x=DataTrain4$PFHxS, y=Pred42, method = 'spearman')
print(Spear42$estimate[[1]])
# 6.5 Species 5 PFOA---------------------------------------------
Pred51 <- pull(predict(final_rf51,DataTrain5),var = .pred)
Spear51 <- cor.test(x=DataTrain5$PFOA, y=Pred51, method = 'spearman')
print(Spear51$estimate[[1]])
Pred52 <- pull(predict(final_rf52,DataTrain5),var = .pred)
Spear52 <- cor.test(x=DataTrain5$PFOA, y=Pred52, method = 'spearman')
print(Spear52$estimate[[1]])
# 6.6 Species 6 PFOA_PFOS_---------------------------------------------
Pred61 <- pull(predict(final_rf61,DataTrain6),var = .pred)
Spear61 <- cor.test(x=DataTrain6$PFOA_PFOS_, y=Pred61, method = 'spearman')
print(Spear61$estimate[[1]])
Pred62 <- pull(predict(final_rf62,DataTrain6),var = .pred)
Spear62 <- cor.test(x=DataTrain6$PFOA_PFOS_, y=Pred62, method = 'spearman')
print(Spear62$estimate[[1]])
# 6.7 Species 7 PFOS---------------------------------------------
Pred71 <- pull(predict(final_rf71,DataTrain7),var = .pred)
Spear71 <- cor.test(x=DataTrain7$PFOS, y=Pred71, method = 'spearman')
print(Spear71$estimate[[1]])
Pred72 <- pull(predict(final_rf72,DataTrain7),var = .pred)
Spear72 <- cor.test(x=DataTrain7$PFOS, y=Pred72, method = 'spearman')
print(Spear72$estimate[[1]])
# 6.8 Species 8 Total_PFAS---------------------------------------------
Pred81 <- pull(predict(final_rf81,DataTrain8),var = .pred)
Spear81 <- cor.test(x=DataTrain8$Total_PFAS, y=Pred81, method = 'spearman')
print(Spear81$estimate[[1]])
Pred82 <- pull(predict(final_rf82,DataTrain8),var = .pred)
Spear82 <- cor.test(x=DataTrain8$Total_PFAS, y=Pred82, method = 'spearman')
print(Spear82$estimate[[1]])
######################### END CODE #############################################
