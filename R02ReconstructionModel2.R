######################## R02ReconstructionModel2.R #############################
# Research Paper: Large-scale Assessment of PFAS Compounds in Drinking Water   #
# Sources using Machine Learning                                               #
# https://doi.org/10.1016/j.watres.2023.120307                                 #
# Nicolas Fernandez, A. Pouyan Nejadhashemi, Christian Loveall                 #
# Department of Biosystems and Agricultural Engineering                        #
# Michigan State University, East Lansing, MI, United States                   #
# Code written and curated by Nicolas Fernandez (OrcID 0000-0001-7979-2941)    #
######################## Description ###########################################
# According to the Paper, this code is used to reconstruct Model 2 following   #
# Hu et al (2021) - https://doi.org/10.1021/acs.estlett.1c00264 - as closely   #
# as possible                                                                  #
################################################################################
###### 0. IMPORT AND  PREPARE DATA #############################################
# 0.1 Set Working Directory ----------------------------------------------------
wd = 'C:/XXX/YYY/ZZZ/3R'
setwd(wd)
# 0.2 Read Input Information ---------------------------------------------------
DirInfo <- 'C:/XXX/YYY/ZZZ/PFASMIGWdatabaseV2.xlsx'
library('readxl')
Data <- read_excel(DirInfo, sheet = 'Data')
Dict <- read_excel(DirInfo, sheet = 'Dictionary')
# 0.3 Complete NaNs ------------------------------------------------------------
# 0.3.1 Buf101RCH1 (M2-H3) ............................................. 
for(i in 1:nrow(Data)){
  if(is.na(Data$Buf101RCH1[i])==TRUE){Data$Buf101RCH1[i] = Data$Buf103RCH1[i]}}
for(i in 1:nrow(Data)){
  if(is.na(Data$Buf101RCH1[i])==TRUE){Data$Buf101RCH1[i] = Data$Buf110RCH1[i]}} 
# 0.3.2 Buf101DWT (M2-H4) ..............................................
for(i in 1:nrow(Data)){
  if(is.na(Data$Buf101DWT[i])==TRUE){Data$Buf101DWT[i] = Data$Buf103DWT[i]}}
for(i in 1:nrow(Data)){
  if(is.na(Data$Buf101DWT[i])==TRUE){Data$Buf101DWT[i] = Data$Buf110DWT[i]}}
# 0.3.3 Buf101SGR (M2-H5) ..............................................
for(i in 1:nrow(Data)){
  if(is.na(Data$Buf101SGR[i])==TRUE){Data$Buf101SGR[i] = Data$Buf103SGR[i]}}
# 0.3.4 Buf101SoilOrgC (M2-S5) .........................................
for(i in 1:nrow(Data)){
  if(is.na(Data$Buf101SoilOrgC[i])==TRUE){Data$Buf101SoilOrgC[i] = Data$Buf103SoilOrgC[i]}}
# 0.3.5 Buf101HGR (M2-H6) ..............................................
for(i in 1:nrow(Data)){
  if(is.na(Data$Buf101HGR[i])==TRUE){Data$Buf101HGR[i] = Data$Buf103HGR[i]}}
# 0.4 Convert character records into integers ----------------------------------
# 0.4.1 Total PFAS .....................................................
for(i in 1:nrow(Data)){
  if(grepl("N",Data$Total_PFAS[i])==TRUE){Data$Total_PFAS[i] = 0}}
Data$Total_PFAS <- as.integer(Data$Total_PFAS)
# 0.4.2 Total PFOA_PFOS ................................................
for(i in 1:nrow(Data)){
  if(grepl("N",Data$PFOA_PFOS_[i])==TRUE){Data$PFOA_PFOS_[i] = 0}}
Data$PFOA_PFOS_ <- as.integer(Data$PFOA_PFOS_)
# 0.4.3 PFHxA ..........................................................
for(i in 1:nrow(Data)){
  if(grepl("<",Data$PFHxA[i])==TRUE){Data$PFHxA[i] = 0}}
Data$PFHxA <- as.integer(Data$PFHxA)
# 0.4.4 PFHpA ..........................................................
for(i in 1:nrow(Data)){
  if(grepl("<",Data$PFHpA[i])==TRUE){Data$PFHpA[i] = 0}}
Data$PFHpA <- as.integer(Data$PFHpA)
# 0.4.5 PFOA ...........................................................
for(i in 1:nrow(Data)){
  if(grepl("<",Data$PFOA[i])==TRUE){Data$PFOA[i] = 0}}
Data$PFOA <- as.integer(Data$PFOA)
# 0.4.6 PFBS ...........................................................
for(i in 1:nrow(Data)){
  if(grepl("<",Data$PFBS[i])==TRUE){Data$PFBS[i] = 0}}
Data$PFBS <- as.integer(Data$PFBS)
# 0.4.7 PFHxS ..........................................................
for(i in 1:nrow(Data)){
  if(grepl("<",Data$PFHxS[i])==TRUE){Data$PFHxS[i] = 0}}
Data$PFHxS <- as.integer(Data$PFHxS)
# 0.4.8 PFOS ...........................................................
for(i in 1:nrow(Data)){
  if(grepl("<",Data$PFOS[i])==TRUE){Data$PFOS[i] = 0}}
Data$PFOS <- as.integer(Data$PFOS)
# 0.5 Create dataframe with necessary information ------------------------------
# 0.5.1 Add relevant variables to dataframe ............................
Data4Huetal21 <- data.frame(
  Total_PFAS = Data$Total_PFAS, PFOA_PFOS = Data$PFOA_PFOS_, PFHxA = Data$PFHxA,
  PFHpA = Data$PFHpA, PFOA = Data$PFOA, PFBS = Data$PFBS, PFHxS = Data$PFHxS,
  PFOS = Data$PFOS, ImpactFactor1 = Data$ImpactFactor1, 
  ImpactFactor2 = Data$ImpactFactor2, Geol1 = Data$GW3LGPBuf101Zst_Band_1,
  Hydro1 = Data$Buf101PRELT, Hydro2 = Data$Buf101PREST,Hydro3 = Data$Buf101RCH1,
  Hydro4 = Data$Buf101DWT, Hydro5 = Data$Buf101SGR, Hydro6 = Data$Buf101HGR, 
  Soil1 = Data$Buf101BD, Soil2 = Data$Buf101AWC, Soil3 = Data$Buf101CEC, 
  Soil4 = Data$Buf101SoilOrgC, Soil5 = Data$Buf101KSAT)
# 0.5.2 Modify type of factor variables ................................
Data4Huetal21$Geol1 <- as.factor(Data4Huetal21$Geol1)
# 0.5.3 Delete rows with NaNs ..........................................
Data4Huetal21 <- na.omit(Data4Huetal21)
###### 1. TUNE RANDOM FOREST WITH 10 FOLD CROSS VALIDATION #####################
# 1.0 Prepare Model Tuning for all species -------------------------------------
# 1.0.1 Load libraries .................................................
library(tidyverse)
library(tidymodels)
# 1.0.2 Turn dependent variables into binary factors ...................
for(i in 1:nrow(Data4Huetal21)){if(Data4Huetal21$Total_PFAS[i]>0){Data4Huetal21$Total_PFAS[i]<-1}}
for(i in 1:nrow(Data4Huetal21)){if(Data4Huetal21$PFOA_PFOS[i]>0){Data4Huetal21$PFOA_PFOS[i]<-1}}
for(i in 1:nrow(Data4Huetal21)){if(Data4Huetal21$PFHxA[i]>0){Data4Huetal21$PFHxA[i]<-1}}
for(i in 1:nrow(Data4Huetal21)){if(Data4Huetal21$PFHpA[i]>0){Data4Huetal21$PFHpA[i]<-1}}
for(i in 1:nrow(Data4Huetal21)){if(Data4Huetal21$PFOA[i]>0){Data4Huetal21$PFOA[i]<-1}}
for(i in 1:nrow(Data4Huetal21)){if(Data4Huetal21$PFBS[i]>0){Data4Huetal21$PFBS[i]<-1}}
for(i in 1:nrow(Data4Huetal21)){if(Data4Huetal21$PFHxS[i]>0){Data4Huetal21$PFHxS[i]<-1}}
for(i in 1:nrow(Data4Huetal21)){if(Data4Huetal21$PFOS[i]>0){Data4Huetal21$PFOS[i]<-1}}
Data4Huetal21$Total_PFAS <- as.factor(Data4Huetal21$Total_PFAS)
Data4Huetal21$PFOA_PFOS <- as.factor(Data4Huetal21$PFOA_PFOS)
Data4Huetal21$PFHxA <- as.factor(Data4Huetal21$PFHxA)
Data4Huetal21$PFHpA <- as.factor(Data4Huetal21$PFHpA)
Data4Huetal21$PFOA <- as.factor(Data4Huetal21$PFOA)
Data4Huetal21$PFBS <- as.factor(Data4Huetal21$PFBS)
Data4Huetal21$PFHxS <- as.factor(Data4Huetal21$PFHxS)
Data4Huetal21$PFOS <- as.factor(Data4Huetal21$PFOS)
# 1.0.3 Set Tuning Specifications ......................................
rfTuneSpecs <- rand_forest(mode = "classification",
                           engine = "randomForest", 
                           mtry = tune(),
                           trees = tune(),
                           min_n = tune())
# 1.1 SPECIES 1 TOTAL PFAS -----------------------------------------------------
# 1.1.1 Initial split ..................................................
set.seed(123456)
DataSplit1 <- initial_split(Data4Huetal21, prop = 3/4, strata = Total_PFAS)
DataTrain1 <- training(DataSplit1)
DataTest1  <- testing(DataSplit1)
# 1.1.2 Obtain folds for 10 fold cross validation ......................
folds1 <- vfold_cv(data = DataTrain1,
                   v = 10, repeats = 1,
                   strata = "Total_PFAS",
                   pool = 0.1)
# 1.1.3 Set Tuning grid ................................................
rf_grid1 <- grid_regular(
  finalize(mtry(), subset(DataTrain1, select = ImpactFactor1:Soil5)), 
  trees(), 
  min_n(), 
  levels = 5)
# 1.1.4 Tune model on grid .............................................
tunerf_wf1 <- workflow() %>% add_model(rfTuneSpecs) %>% 
  add_formula(Total_PFAS ~ ImpactFactor1 + ImpactFactor2 + Geol1 + Hydro1 + 
                Hydro2 + Hydro3 + Hydro4 + Hydro5 + Hydro6 + Soil1 + Soil2 + 
                Soil3 + Soil4 + Soil5)
tunerf_res1 <- tunerf_wf1 %>% tune_grid(resamples = folds1, grid = rf_grid1)
# 1.2 SPECIES 2 PFOA_PFOS ------------------------------------------------------
# 1.2.1 Initial split ..................................................
set.seed(123456)
DataSplit2 <- initial_split(Data4Huetal21, prop = 3/4, strata = PFOA_PFOS)
DataTrain2 <- training(DataSplit2)
DataTest2  <- testing(DataSplit2)
# 1.2.2 Obtain folds for 10 fold cross validation ......................
folds2 <- vfold_cv(data = DataTrain2,
                   v = 10, repeats = 1,
                   strata = "PFOA_PFOS",
                   pool = 0.1)
# 1.2.3 Set Tuning grid ................................................
rf_grid2 <- grid_regular(
  finalize(mtry(), subset(DataTrain2, select = ImpactFactor1:Soil5)), 
  trees(), 
  min_n(), 
  levels = 5)
# 1.2.4 Tune model on grid .............................................
tunerf_wf2 <- workflow() %>% add_model(rfTuneSpecs) %>% 
  add_formula(PFOA_PFOS ~ ImpactFactor1 + ImpactFactor2 + Geol1 + Hydro1 + 
                Hydro2 + Hydro3 + Hydro4 + Hydro5 + Hydro6 + Soil1 + Soil2 + 
                Soil3 + Soil4 + Soil5)
tunerf_res2 <- tunerf_wf2 %>% tune_grid(resamples = folds2, grid = rf_grid2)
# 1.3 SPECIES 3 PFHxA ----------------------------------------------------------
# 1.3.1 Initial split ..................................................
set.seed(123456)
DataSplit3 <- initial_split(Data4Huetal21, prop = 3/4, strata = PFHxA)
DataTrain3 <- training(DataSplit3)
DataTest3  <- testing(DataSplit3)
# 1.3.2 Obtain folds for 10 fold cross validation ......................
folds3 <- vfold_cv(data = DataTrain3,
                   v = 10, repeats = 1,
                   strata = "PFHxA",
                   pool = 0.1)
# 1.3.3 Set Tuning grid ................................................
rf_grid3 <- grid_regular(
  finalize(mtry(), subset(DataTrain3, select = ImpactFactor1:Soil5)), 
  trees(), 
  min_n(), 
  levels = 5)
# 1.3.4 Tune model on grid .............................................
tunerf_wf3 <- workflow() %>% add_model(rfTuneSpecs) %>% 
  add_formula(PFHxA ~ ImpactFactor1 + ImpactFactor2 + Geol1 + Hydro1 + 
                Hydro2 + Hydro3 + Hydro4 + Hydro5 + Hydro6 + Soil1 + Soil2 + 
                Soil3 + Soil4 + Soil5)
tunerf_res3 <- tunerf_wf3 %>% tune_grid(resamples = folds3, grid = rf_grid3)
# 1.4 SPECIES 4 PFHpA ----------------------------------------------------------
# 1.4.1 Initial split ..................................................
set.seed(123456)
DataSplit4 <- initial_split(Data4Huetal21, prop = 3/4, strata = PFHpA)
DataTrain4 <- training(DataSplit4)
DataTest4  <- testing(DataSplit4)
# 1.4.2 Obtain folds for 10 fold cross validation ......................
folds4 <- vfold_cv(data = DataTrain4,
                   v = 10, repeats = 1,
                   strata = "PFHpA",
                   pool = 0.1)
# 1.4.3 Set Tuning grid ................................................
rf_grid4 <- grid_regular(
  finalize(mtry(), subset(DataTrain4, select = ImpactFactor1:Soil5)), 
  trees(), 
  min_n(), 
  levels = 5)
# 1.4.4 Tune model on grid .............................................
tunerf_wf4 <- workflow() %>% add_model(rfTuneSpecs) %>% 
  add_formula(PFHpA ~ ImpactFactor1 + ImpactFactor2 + Geol1 + Hydro1 + 
                Hydro2 + Hydro3 + Hydro4 + Hydro5 + Hydro6 + Soil1 + Soil2 + 
                Soil3 + Soil4 + Soil5)
tunerf_res4 <- tunerf_wf4 %>% tune_grid(resamples = folds4, grid = rf_grid4)
# 1.5 SPECIES 5 PFOA ----------------------------------------------------------
# 1.5.1 Initial split ..................................................
set.seed(123456)
DataSplit5 <- initial_split(Data4Huetal21, prop = 3/4, strata = PFOA)
DataTrain5 <- training(DataSplit5)
DataTest5  <- testing(DataSplit5)
# 1.5.2 Obtain folds for 10 fold cross validation ......................
folds5 <- vfold_cv(data = DataTrain5,
                   v = 10, repeats = 1,
                   strata = "PFOA",
                   pool = 0.1)
# 1.5.3 Set Tuning grid ................................................
rf_grid5 <- grid_regular(
  finalize(mtry(), subset(DataTrain5, select = ImpactFactor1:Soil5)), 
  trees(), 
  min_n(), 
  levels = 5)
# 1.5.4 Tune model on grid .............................................
tunerf_wf5 <- workflow() %>% add_model(rfTuneSpecs) %>% 
  add_formula(PFOA ~ ImpactFactor1 + ImpactFactor2 + Geol1 + Hydro1 + 
                Hydro2 + Hydro3 + Hydro4 + Hydro5 + Hydro6 + Soil1 + Soil2 + 
                Soil3 + Soil4 + Soil5)
tunerf_res5 <- tunerf_wf5 %>% tune_grid(resamples = folds5, grid = rf_grid5)
# 1.6 SPECIES 6 PFBS ----------------------------------------------------------
# 1.6.1 Initial split ..................................................
set.seed(123456)
DataSplit6 <- initial_split(Data4Huetal21, prop = 3/4, strata = PFBS)
DataTrain6 <- training(DataSplit6)
DataTest6  <- testing(DataSplit6)
# 1.6.2 Obtain folds for 10 fold cross validation ......................
folds6 <- vfold_cv(data = DataTrain6,
                   v = 10, repeats = 1,
                   strata = "PFBS",
                   pool = 0.1)
# 1.6.3 Set Tuning grid ................................................
rf_grid6 <- grid_regular(
  finalize(mtry(), subset(DataTrain6, select = ImpactFactor1:Soil5)), 
  trees(), 
  min_n(), 
  levels = 5)
# 1.6.4 Tune model on grid .............................................
tunerf_wf6 <- workflow() %>% add_model(rfTuneSpecs) %>% 
  add_formula(PFBS ~ ImpactFactor1 + ImpactFactor2 + Geol1 + Hydro1 + 
                Hydro2 + Hydro3 + Hydro4 + Hydro5 + Hydro6 + Soil1 + Soil2 + 
                Soil3 + Soil4 + Soil5)
tunerf_res6 <- tunerf_wf6 %>% tune_grid(resamples = folds6, grid = rf_grid6)
# 1.7 SPECIES 7 PFHxS ----------------------------------------------------------
# 1.7.1 Initial split ..................................................
set.seed(123456)
DataSplit7 <- initial_split(Data4Huetal21, prop = 3/4, strata = PFHxS)
DataTrain7 <- training(DataSplit7)
DataTest7  <- testing(DataSplit7)
# 1.7.2 Obtain folds for 10 fold cross validation ......................
folds7 <- vfold_cv(data = DataTrain7,
                   v = 10, repeats = 1,
                   strata = "PFHxS",
                   pool = 0.1)
# 1.7.3 Set Tuning grid ................................................
rf_grid7 <- grid_regular(
  finalize(mtry(), subset(DataTrain7, select = ImpactFactor1:Soil5)), 
  trees(), 
  min_n(), 
  levels = 5)
# 1.7.4 Tune model on grid .............................................
tunerf_wf7 <- workflow() %>% add_model(rfTuneSpecs) %>% 
  add_formula(PFHxS ~ ImpactFactor1 + ImpactFactor2 + Geol1 + Hydro1 + 
                Hydro2 + Hydro3 + Hydro4 + Hydro5 + Hydro6 + Soil1 + Soil2 + 
                Soil3 + Soil4 + Soil5)
tunerf_res7 <- tunerf_wf7 %>% tune_grid(resamples = folds7, grid = rf_grid7)
# 1.8 SPECIES 8 PFOS -----------------------------------------------------------
# 1.8.1 Initial split ..................................................
set.seed(123456)
DataSplit8 <- initial_split(Data4Huetal21, prop = 3/4, strata = PFOS)
DataTrain8 <- training(DataSplit8)
DataTest8  <- testing(DataSplit8)
# 1.8.2 Obtain folds for 10 fold cross validation ......................
folds8 <- vfold_cv(data = DataTrain8,
                   v = 10, repeats = 1,
                   strata = "PFOS",
                   pool = 0.1)
# 1.8.3 Set Tuning grid ................................................
rf_grid8 <- grid_regular(
  finalize(mtry(), subset(DataTrain8, select = ImpactFactor1:Soil5)), 
  trees(), 
  min_n(), 
  levels = 5)
# 1.8.4 Tune model on grid .............................................
tunerf_wf8 <- workflow() %>% add_model(rfTuneSpecs) %>% 
  add_formula(PFOS ~ ImpactFactor1 + ImpactFactor2 + Geol1 + Hydro1 + 
                Hydro2 + Hydro3 + Hydro4 + Hydro5 + Hydro6 + Soil1 + Soil2 + 
                Soil3 + Soil4 + Soil5)
tunerf_res8 <- tunerf_wf8 %>% tune_grid(resamples = folds8, grid = rf_grid8)
###### 2. GET TUNING RESULTS FOR ALL SPECIES ###################################
# 2.1 Species 1 ----------------------------------------------------------------
tunerf_res1 %>% show_best(metric = "accuracy")
tunerf_res1 %>% show_best(metric = "roc_auc")
collect_metrics(tunerf_res1) -> metrics1
# 2.2 Species 2 ----------------------------------------------------------------
tunerf_res2 %>% show_best(metric = "accuracy")
tunerf_res2 %>% show_best(metric = "roc_auc")
collect_metrics(tunerf_res2) -> metrics2
# 2.3 Species 3 ----------------------------------------------------------------
tunerf_res3 %>% show_best(metric = "accuracy")
tunerf_res3 %>% show_best(metric = "roc_auc")
collect_metrics(tunerf_res3) -> metrics3
# 2.4 Species 4 ----------------------------------------------------------------
tunerf_res4 %>% show_best(metric = "accuracy")
tunerf_res4 %>% show_best(metric = "roc_auc")
collect_metrics(tunerf_res4) -> metrics4
# 2.5 Species 5 ----------------------------------------------------------------
tunerf_res5 %>% show_best(metric = "accuracy")
tunerf_res5 %>% show_best(metric = "roc_auc")
collect_metrics(tunerf_res5) -> metrics5
# 2.6 Species 6 ----------------------------------------------------------------
tunerf_res6 %>% show_best(metric = "accuracy")
tunerf_res6 %>% show_best(metric = "roc_auc")
collect_metrics(tunerf_res6) -> metrics6
# 2.7 Species 7 ----------------------------------------------------------------
tunerf_res7 %>% show_best(metric = "accuracy")
tunerf_res7 %>% show_best(metric = "roc_auc")
collect_metrics(tunerf_res7) -> metrics7
# 2.8 Species 8 ----------------------------------------------------------------
tunerf_res8 %>% show_best(metric = "accuracy")
tunerf_res8 %>% show_best(metric = "roc_auc")
collect_metrics(tunerf_res8) -> metrics8
###### 3. SELECT BEST MODELS FOR EACH METRIC ###################################
best_rf11 <- tunerf_res1 %>% select_best(metric = "accuracy")
best_rf12 <- tunerf_res1 %>% select_best(metric = "roc_auc")
best_rf21 <- tunerf_res2 %>% select_best(metric = "accuracy")
best_rf22 <- tunerf_res2 %>% select_best(metric = "roc_auc")
best_rf31 <- tunerf_res3 %>% select_best(metric = "accuracy")
best_rf32 <- tunerf_res3 %>% select_best(metric = "roc_auc")
best_rf41 <- tunerf_res4 %>% select_best(metric = "accuracy")
best_rf42 <- tunerf_res4 %>% select_best(metric = "roc_auc")
best_rf51 <- tunerf_res5 %>% select_best(metric = "accuracy")
best_rf52 <- tunerf_res5 %>% select_best(metric = "roc_auc")
best_rf61 <- tunerf_res6 %>% select_best(metric = "accuracy")
best_rf62 <- tunerf_res6 %>% select_best(metric = "roc_auc")
best_rf71 <- tunerf_res7 %>% select_best(metric = "accuracy")
best_rf72 <- tunerf_res7 %>% select_best(metric = "roc_auc")
best_rf81 <- tunerf_res8 %>% select_best(metric = "accuracy")
best_rf82 <- tunerf_res8 %>% select_best(metric = "roc_auc")
###### 4. VALIDATE BEST MODELS WITH TESTING DATA ###############################
# 4.1 Finalize workflowS -------------------------------------------------------
final_wf11 <- tunerf_wf1 %>% finalize_workflow(best_rf11)
final_wf12 <- tunerf_wf1 %>% finalize_workflow(best_rf12)
final_wf21 <- tunerf_wf2 %>% finalize_workflow(best_rf21)
final_wf22 <- tunerf_wf2 %>% finalize_workflow(best_rf22)
final_wf31 <- tunerf_wf3 %>% finalize_workflow(best_rf31)
final_wf32 <- tunerf_wf3 %>% finalize_workflow(best_rf32)
final_wf41 <- tunerf_wf4 %>% finalize_workflow(best_rf41)
final_wf42 <- tunerf_wf4 %>% finalize_workflow(best_rf42)
final_wf51 <- tunerf_wf5 %>% finalize_workflow(best_rf51)
final_wf52 <- tunerf_wf5 %>% finalize_workflow(best_rf52)
final_wf61 <- tunerf_wf6 %>% finalize_workflow(best_rf61)
final_wf62 <- tunerf_wf6 %>% finalize_workflow(best_rf62)
final_wf71 <- tunerf_wf7 %>% finalize_workflow(best_rf71)
final_wf72 <- tunerf_wf7 %>% finalize_workflow(best_rf72)
final_wf81 <- tunerf_wf8 %>% finalize_workflow(best_rf81)
final_wf82 <- tunerf_wf8 %>% finalize_workflow(best_rf82)
# 4.2 Fit final models on full training data and evaluate on test data ---------
final_fit11 <- final_wf11 %>% last_fit(DataSplit1) 
final_fit12 <- final_wf12 %>% last_fit(DataSplit1)
final_fit21 <- final_wf21 %>% last_fit(DataSplit2) 
final_fit22 <- final_wf22 %>% last_fit(DataSplit2)
final_fit31 <- final_wf31 %>% last_fit(DataSplit3) 
final_fit32 <- final_wf32 %>% last_fit(DataSplit3)
final_fit41 <- final_wf41 %>% last_fit(DataSplit4) 
final_fit42 <- final_wf42 %>% last_fit(DataSplit4)
final_fit51 <- final_wf51 %>% last_fit(DataSplit5) 
final_fit52 <- final_wf52 %>% last_fit(DataSplit5)
final_fit61 <- final_wf61 %>% last_fit(DataSplit6) 
final_fit62 <- final_wf62 %>% last_fit(DataSplit6)
final_fit71 <- final_wf71 %>% last_fit(DataSplit7) 
final_fit72 <- final_wf72 %>% last_fit(DataSplit7)
final_fit81 <- final_wf81 %>% last_fit(DataSplit8) 
final_fit82 <- final_wf82 %>% last_fit(DataSplit8)
# 4.3 Collect metrics ----------------------------------------------------------
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
# 4.4 Collect models to predict ------------------------------------------------
final_rf11 <- extract_workflow(final_fit11)
final_rf12 <- extract_workflow(final_fit12)
final_rf21 <- extract_workflow(final_fit21)
final_rf22 <- extract_workflow(final_fit22)
final_rf31 <- extract_workflow(final_fit31)
final_rf32 <- extract_workflow(final_fit32)
final_rf41 <- extract_workflow(final_fit41)
final_rf42 <- extract_workflow(final_fit42)
final_rf51 <- extract_workflow(final_fit51)
final_rf52 <- extract_workflow(final_fit52)
final_rf61 <- extract_workflow(final_fit61)
final_rf62 <- extract_workflow(final_fit62)
final_rf71 <- extract_workflow(final_fit71)
final_rf72 <- extract_workflow(final_fit72)
final_rf81 <- extract_workflow(final_fit81)
final_rf82 <- extract_workflow(final_fit82)
######################## END CODE ##############################################
