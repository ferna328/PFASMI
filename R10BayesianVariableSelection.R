################### R10BayesianVariableSelection.R #############################
# Research Paper: Large-scale Assessment of PFAS Compounds in Drinking Water   #
# Sources through Machine Learning                                             #
# Nicolas Fernandez, A. Pouyan Nejadhashemi, Christian Loveall                 #
# Department of Biosystems and Agricultural Engineering                        #
# Michigan State University, East Lansing, MI, United States                   #
# Code written and curated by Nicolas Fernandez (OrcID 0000-0001-7979-2941)    #
######################## Description ###########################################
# According to the Paper, this code is used to do the Bayesian Variable        #
# Selection process after reconstructing the four models with the previous     #
# codes                                                                        #
################################################################################
###### 0. IMPORT AND  PREPARE DATA #############################################
# 0.1 Set Working Directory ----------------------------------------------------
wd = 'C:/XXX/YYY/ZZZ/PFASMIBVS'
setwd(wd)
# 0.2 Read Input Information ---------------------------------------------------
# 0.2.1 Model 1 (Hu et al 2016) ........................
dirDep = 'C:/XXX/YYY/ZZZ/DataExp1V2Dep.csv'
dirExp = 'C:/XXX/YYY/ZZZ/DataExp1V2Exp.csv'
Dep <- read.csv(file = dirDep) # Dependent Variables
Exp <- read.csv(file = dirExp) # Explanatory Variables
# 0.2.2 Model 2 (Hu et al 2021) ........................
DirInfo <- 'C:/XXX/YYY/ZZZ/PFASMIGWdatabaseV2.xlsx'
library('readxl')
Data2 <- read_excel(DirInfo, sheet = 'Data')
Dict2 <- read_excel(DirInfo, sheet = 'Dictionary')
# 0.2.3 Model 3 (George & Dixit 2021) ..................
DirInfo <- 'C:/XXX/YYY/ZZZ/MasterFile.xlsx'
Data3 <- read_excel(DirInfo)
# 0.2.4 Model 4 (McMahon et al 2022) ...................
DirInfo <- 'C:/XXX/YYY/ZZZ/PFASMIGWdatabaseV3.xlsx'
Data4 <- read_excel(DirInfo, sheet = 'Data')
Dict4 <- read_excel(DirInfo, sheet = 'Dictionary')
rm(dirDep, dirExp, dirin, DirInfo)
# 0.3 Organize Input Information -----------------------------------------------
# 0.3.0 Dependent Variables............................
PFBS <- InputData$PFBS_L
PFBS[which(!is.finite(PFBS))] <- 0 # replace - inf for 0
PFHpA <- InputData$PFHpA_L
PFHpA[which(!is.finite(PFHpA))] <- 0 # replace - inf for 0
PFHxA <- InputData$PFHxA_L
PFHxA[which(!is.finite(PFHxA))] <- 0 # replace - inf for 0
PFHxS <- InputData$PFHxS_L
PFHxS[which(!is.finite(PFHxS))] <- 0 # replace - inf for 0
PFOA <- InputData$PFOA_L
PFOA[which(!is.finite(PFOA))] <- 0 # replace - inf for 0
PFOA_PFOS <- InputData$PFOA_PFOS__L
PFOA_PFOS[which(!is.finite(PFOA_PFOS))] <- 0 # replace - inf for 0
PFOS <- InputData$PFOS_L
PFOS[which(!is.finite(PFOS))] <- 0 # replace - inf for 0
Total_PFAS <- InputData$Total_PFAS_L
Total_PFAS[which(!is.finite(Total_PFAS))] <- 0 # replace - inf for 0
df <- data.frame(PFBS, PFHpA, PFHxA, PFHxS, PFOA, PFOA_PFOS, PFOS, Total_PFAS)
rm(PFBS,PFHpA,PFHxA,PFHxS,PFOA,PFOA_PFOS,PFOS,Total_PFAS)
# 0.3.1 Model 1 .......................................
M1_P1_Buf01 <- InputData$Buf101S5IndCount
M1_P1_Buf03 <- InputData$Buf103S5IndCount
M1_P1_Buf10 <- InputData$Buf110S5IndCount
M1_P2_Buf01 <- InputData$Buf101S4WWCount
M1_P2_Buf03 <- InputData$Buf103S4WWCount
M1_P2_Buf10 <- InputData$Buf110S4WWCount
M1_P3_Buf01 <- InputData$Buf101ConSoCount # Confirmed sources include
M1_P3_Buf03 <- InputData$Buf103ConSoCount # Military fire training areas
M1_P3_Buf10 <- InputData$Buf110ConSoCount # in the study area
M1_P4_Buf01 <- InputData$Buf101S3APCount
M1_P4_Buf03 <- InputData$Buf103S3APCount
M1_P4_Buf10 <- InputData$Buf110S3APCount
df <- cbind(df,
            M1_P1_Buf01, M1_P1_Buf03, M1_P1_Buf10,
            M1_P2_Buf01, M1_P2_Buf03, M1_P2_Buf10,
            M1_P3_Buf01, M1_P3_Buf03, M1_P3_Buf10,
            M1_P4_Buf01, M1_P4_Buf03, M1_P4_Buf10)
rm(M1_P1_Buf01, M1_P1_Buf03, M1_P1_Buf10, M1_P2_Buf01, M1_P2_Buf03, M1_P2_Buf10,
   M1_P3_Buf01, M1_P3_Buf03, M1_P3_Buf10, M1_P4_Buf01, M1_P4_Buf03, M1_P4_Buf10)
# 0.3.2 Model 2 .......................................
# Point Sources
M2_P1 <- Data2$ImpactFactor1 # confirmed sources
M2_P2 <- Data2$ImpactFactor2 # Potential sources
# Geology
M2_G1_Buf01 <- Data2$GW3LGPBuf101Zst_Band_1
M2_G1_Buf03 <- Data2$GW3LGPBuf103Zst_Band_1
M2_G1_Buf10 <- Data2$GW3LGPBuf110Zst_Band_1
# Hydrology
M2_H1_Buf01 <- Data2$Buf101PREST # precipitation short term
M2_H1_Buf03 <- Data2$Buf103PREST
M2_H1_Buf10 <- Data2$Buf110PREST
M2_H2_Buf01 <- Data2$Buf101PRELT # precipitation long term
M2_H2_Buf03 <- Data2$Buf103PRELT
M2_H2_Buf10 <- Data2$Buf110PRELT
M2_H3_Buf01 <- Data2$Buf101RCH1 # Recharge USGS
M2_H3_Buf03 <- Data2$Buf103RCH1
M2_H3_Buf10 <- Data2$Buf110RCH1
M2_H4_Buf01 <- Data2$Buf101DWT # Depth to water table
M2_H4_Buf03 <- Data2$Buf103DWT
M2_H4_Buf10 <- Data2$Buf110DWT
M2_H5_Buf01 <- Data2$Buf101SGR # Slope gradient
M2_H5_Buf03 <- Data2$Buf103SGR
M2_H5_Buf10 <- Data2$Buf110SGR
M2_H6_Buf01 <- Data2$Buf101HGR # Hydrologic Group Dominant Condition
M2_H6_Buf03 <- Data2$Buf103HGR
M2_H6_Buf10 <- Data2$Buf110HGR
# Soil geochemistry
M2_S1_Buf01 <- Data2$Buf101KSAT # Saturated hydraulic conductivity
M2_S1_Buf03 <- Data2$Buf103KSAT
M2_S1_Buf10 <- Data2$Buf110KSAT
M2_S2_Buf01 <- Data2$Buf101BD # Bulk density
M2_S2_Buf03 <- Data2$Buf103BD
M2_S2_Buf10 <- Data2$Buf110BD
M2_S3_Buf01 <- Data2$Buf101AWC # Available Water Capacity
M2_S3_Buf03 <- Data2$Buf103AWC
M2_S3_Buf10 <- Data2$Buf110AWC
M2_S4_Buf01 <- Data2$Buf101CEC # Cation Exchange Capacity
M2_S4_Buf03 <- Data2$Buf103CEC
M2_S4_Buf10 <- Data2$Buf110CEC
M2_S5_Buf01 <- Data2$Buf101SoilOrgC # Soil Organic carbon
M2_S5_Buf03 <- Data2$Buf103SoilOrgC
M2_S5_Buf10 <- Data2$Buf110SoilOrgC
df <- cbind(df,
            M2_P1, M2_P2,
            M2_G1_Buf01, M2_G1_Buf03, M2_G1_Buf10,
            M2_H1_Buf01, M2_H1_Buf03, M2_H1_Buf10,
            M2_H2_Buf01, M2_H2_Buf03, M2_H2_Buf10,
            M2_H3_Buf01, M2_H3_Buf03, M2_H3_Buf10,
            M2_H4_Buf01, M2_H4_Buf03, M2_H4_Buf10,
            M2_H5_Buf01, M2_H5_Buf03, M2_H5_Buf10,
            M2_H6_Buf01, M2_H6_Buf03, M2_H6_Buf10,
            M2_S1_Buf01, M2_S1_Buf03, M2_S1_Buf10,
            M2_S2_Buf01, M2_S2_Buf03, M2_S2_Buf10,
            M2_S3_Buf01, M2_S3_Buf03, M2_S3_Buf10,
            M2_S4_Buf01, M2_S4_Buf03, M2_S4_Buf10,
            M2_S5_Buf01, M2_S5_Buf03, M2_S5_Buf10)
rm(M2_P1, M2_P2, M2_G1_Buf01, M2_G1_Buf03, M2_G1_Buf10, M2_H1_Buf01, 
   M2_H1_Buf03, M2_H1_Buf10, M2_H2_Buf01, M2_H2_Buf03, M2_H2_Buf10, M2_H3_Buf01,
   M2_H3_Buf03, M2_H3_Buf10, M2_H4_Buf01, M2_H4_Buf03, M2_H4_Buf10, M2_H5_Buf01,
   M2_H5_Buf03, M2_H5_Buf10, M2_H6_Buf01, M2_H6_Buf03, M2_H6_Buf10, M2_S1_Buf01,
   M2_S1_Buf03, M2_S1_Buf10, M2_S2_Buf01, M2_S2_Buf03, M2_S2_Buf10, M2_S3_Buf01, 
   M2_S3_Buf03, M2_S3_Buf10, M2_S4_Buf01, M2_S4_Buf03, M2_S4_Buf10, M2_S5_Buf01,
   M2_S5_Buf03, M2_S5_Buf10)
# 0.3.3 Model 3 .......................................
M3_A1 <- Data3$A1
M3_A2 <- Data3$A2
M3_A3 <- Data3$A3
M3_A4 <- Data3$A4
M3_A5 <- Data3$A5
M3_A6 <- Data3$A6
M3_A7 <- Data3$A7
M3_G1 <- Data3$G1
M3_G2 <- Data3$G2
M3_G3 <- Data3$G3
M3_L1 <- Data3$L1
M3_L2 <- Data3$L2
M3_L3 <- Data3$L3
M3_L4 <- Data3$L4
M3_L5 <- Data3$L5
M3_L6 <- Data3$L6
M3_L7 <- Data3$L7
M3_L8 <- Data3$L8
M3_Q1 <- Data3$Q1
M3_Q2 <- Data3$Q2
M3_Q3 <- Data3$Q3
M3_Q4 <- Data3$Q4
M3_Q5 <- Data3$Q5
M3_Q6 <- Data3$Q6
M3_Q7 <- Data3$Q7
M3_H1 <- Data3$H1
M3_H2 <- Data3$H2
M3_H3 <- Data3$H3
M3_H4 <- Data3$H4
M3_H5 <- Data3$H5
M3_H6 <- Data3$H6
M3_H7 <- Data3$H7
M3_H8 <- Data3$H8
M3_H9 <- Data3$H9
M3_H10 <- Data3$H10
M3_H11 <- Data3$H11
M3_H12 <- Data3$H12
M3_H13 <- Data3$H13
M3_H14 <- Data3$H14
M3_H15 <- Data3$H15
df <- cbind(df,
            M3_A1, M3_A2, M3_A3, M3_A4, M3_A5, M3_A6, M3_A7, M3_G1, M3_G2, M3_G3,
            M3_L1, M3_L2, M3_L3, M3_L4, M3_L5, M3_L6, M3_L7, M3_L8, M3_Q1, M3_Q2,
            M3_Q3, M3_Q4, M3_Q5, M3_Q6, M3_Q7, M3_H1, M3_H2, M3_H3, M3_H4, M3_H5,
            M3_H6, M3_H7, M3_H8, M3_H9, M3_H10, M3_H11, M3_H12, M3_H13, M3_H14,
            M3_H15)
rm(M3_A1, M3_A2, M3_A3, M3_A4, M3_A5, M3_A6, M3_A7, M3_G1, M3_G2, M3_G3, M3_L1, 
   M3_L2, M3_L3, M3_L4, M3_L5, M3_L6, M3_L7, M3_L8, M3_Q1, M3_Q2, M3_Q3, M3_Q4, 
   M3_Q5, M3_Q6, M3_Q7, M3_H1, M3_H2, M3_H3, M3_H4, M3_H5, M3_H6, M3_H7, M3_H8,
   M3_H9, M3_H10, M3_H11, M3_H12, M3_H13, M3_H14, M3_H15)
# 0.3.4 Model 4 .......................................
M4_L1 <- Data4$Buf101PUA
M4_S1 <- Data4$DisNeaConSou
M4_S2 <- Data4$DisNeaLandfil
M4_S3 <- Data4$DisNeaAirport
M4_S4 <- Data4$DisNeaFireSt
M4_I1 <- Data4$DistInd1
M4_I2 <- Data4$DistInd2
M4_I3 <- Data4$DistInd3
M4_I4 <- Data4$DistInd4
M4_I5 <- Data4$DistInd5
M4_I6 <- Data4$DistInd6
M4_I7 <- Data4$DistInd7
M4_I8 <- Data4$DistInd8
M4_I9 <- Data4$DistInd9
M4_I10 <- Data4$DistInd10
M4_I11 <- Data4$DistInd11
M4_I12 <- Data4$DistInd12
M4_I13 <- Data4$DistInd13
M4_I14 <- Data4$DistInd14
M4_I15 <- Data4$DistInd15
M4_I16 <- Data4$DistInd16
M4_I17 <- Data4$DistInd17
M4_I18 <- Data4$DistInd18
M4_I19 <- Data4$DistInd19
M4_I20 <- Data4$DistInd20
M4_I21 <- Data4$DistInd21
M4_I22 <- Data4$DistInd22
M4_I23 <- Data4$DistInd23
M4_I24 <- Data4$DistInd24
M4_I25 <- Data4$DistInd25
M4_I26 <- Data4$DistInd26
M4_I27 <- Data4$DistInd27
M4_I28 <- Data4$DistInd28
M4_I29 <- Data4$DistInd29
M4_I30 <- Data4$DistInd30
M4_I31 <- Data4$DistInd31
M4_I32 <- Data4$DistInd32
M4_I33 <- Data4$DistInd33
M4_I34 <- Data4$DistInd34
M4_I35 <- Data4$DistInd35
M4_I36 <- Data4$DistInd36
M4_I37 <- Data4$DistInd37
M4_I38 <- Data4$DistInd38
M4_I39 <- Data4$DistInd39
M4_I40 <- Data4$DistInd40
M4_I41 <- Data4$DistInd41
M4_I42 <- Data4$DistInd42
M4_I43 <- Data4$DistInd43
M4_I44 <- Data4$DistInd44
df <- cbind(df, M4_L1, M4_S1, M4_S2, M4_S3, M4_S4, M4_I1, M4_I2, M4_I3, M4_I4,
            M4_I5, M4_I6, M4_I7, M4_I8, M4_I9, M4_I10, M4_I11, M4_I12, M4_I13,
            M4_I14, M4_I15, M4_I16, M4_I17, M4_I18, M4_I19, M4_I20, M4_I21,
            M4_I22, M4_I23, M4_I24, M4_I25, M4_I26, M4_I27, M4_I28, M4_I29,
            M4_I30, M4_I31, M4_I32, M4_I33, M4_I34, M4_I35, M4_I36, M4_I37,
            M4_I38, M4_I39, M4_I40, M4_I41, M4_I42, M4_I43, M4_I44)  
rm(M4_L1, M4_S1, M4_S2, M4_S3, M4_S4, M4_I1, M4_I2, M4_I3, M4_I4, M4_I5, M4_I6, 
   M4_I7, M4_I8, M4_I9, M4_I10, M4_I11, M4_I12, M4_I13, M4_I14, M4_I15, M4_I16, 
   M4_I17, M4_I18, M4_I19, M4_I20, M4_I21, M4_I22, M4_I23, M4_I24, M4_I25, 
   M4_I26, M4_I27, M4_I28, M4_I29, M4_I30, M4_I31, M4_I32, M4_I33, M4_I34, 
   M4_I35, M4_I36, M4_I37, M4_I38, M4_I39, M4_I40, M4_I41, M4_I42, M4_I43, 
   M4_I44)
# 0.3.5 New Model .......................................
# Note: After revising all input files PFASMIGWdatabaseV3 (Data4 & Dict 4)
# and MasterFile (Data3) contain all variables.
M5_V001 <- Data4$HUC12ID # ID HUC 12
M5_V001 <- as.factor(M5_V001)
M5_V002 <- Data4$DEMFil # Elevation from DEM
M5_V003 <- Data4$DEMFloAcc
M5_V004 <- Data4$DEMFlLeUp
M5_V005 <- Data4$DEMFlLeDo
M5_V006 <- Data4$Buf101AWS
M5_V007 <- Data4$Buf103AWS
M5_V008 <- Data4$Buf110AWS
M5_V009 <- Data4$Buf101DCL
M5_V010 <- Data4$Buf103DCL
M5_V011 <- Data4$Buf110DCL
M5_V012 <- Data4$Buf101PH
M5_V013 <- Data4$Buf103PH
M5_V014 <- Data4$Buf110PH
M5_V015 <- Data4$Buf101RCH2
M5_V016 <- Data4$Buf103RCH2
M5_V017 <- Data4$Buf110RCH2
M5_V018 <- Data4$Buf101LCO
M5_V019 <- Data4$Buf103LCO
M5_V020 <- Data4$Buf110LCO
M5_V018 <- as.factor(M5_V018)
M5_V019 <- as.factor(M5_V019)
M5_V020 <- as.factor(M5_V020)
M5_V021 <- Data4$Buf101S1FireCount
M5_V022 <- Data4$Buf103S1FireCount
M5_V023 <- Data4$Buf110S1FireCount
M5_V024 <- Data4$Buf101S2LFCount
M5_V025 <- Data4$Buf103S2LFCount
M5_V026 <- Data4$Buf110S2LFCount
M5_V027 <- Data4$GW1GLSBuf101Zst_Band_1
M5_V028 <- Data4$GW1GLSBuf103Zst_Band_1
M5_V029 <- Data4$GW1GLSBuf110Zst_Band_1
M5_V030 <- Data4$GW2AGDBuf101Zst_Band_1
M5_V031 <- Data4$GW2AGDBuf103Zst_Band_1
M5_V032 <- Data4$GW2AGDBuf110Zst_Band_1
M5_V027 <- as.factor(M5_V027)
M5_V028 <- as.factor(M5_V028)
M5_V029 <- as.factor(M5_V029)
M5_V030 <- as.factor(M5_V030)
M5_V031 <- as.factor(M5_V031)
M5_V032 <- as.factor(M5_V032)
M5_V033 <- Data4$Pop1BG
M5_V034 <- Data4$Pop2CT
M5_V035 <- Data4$Pop3Co
M5_V036 <- Data4$GDP1BG
M5_V037 <- Data4$GDP2CT
M5_V038 <- Data4$GDP3Co
M5_V039 <- Data4$gGDP1BG
M5_V040 <- Data4$gGDP2CT
M5_V041 <- Data4$gGDP3Co
M5_V042 <- Data4$CSCloDist
M5_V043 <- Data4$CSCloFID
M5_V043 <- as.factor(M5_V043)
M5_V044 <- Data4$LFCloDist
M5_V045 <- Data4$LFCloFID
M5_V045 <- as.factor(M5_V045)
M5_V046 <- Data4$Buf103PUA
M5_V047 <- Data4$Buf110PUA
df <- cbind(df, M5_V001, M5_V002, M5_V003, M5_V004, M5_V005, M5_V006, 
            M5_V007, M5_V008, M5_V009, M5_V010, M5_V011, M5_V012, M5_V013,
            M5_V014, M5_V015, M5_V016, M5_V017, M5_V018, M5_V019, M5_V020,
            M5_V021, M5_V022, M5_V023, M5_V024, M5_V025, M5_V026, M5_V027,
            M5_V028, M5_V029, M5_V030, M5_V031, M5_V032, M5_V033, M5_V034, 
            M5_V035, M5_V036, M5_V037, M5_V038, M5_V039, M5_V040, M5_V041, 
            M5_V042, M5_V043, M5_V044, M5_V045, M5_V046, M5_V047)  
rm(M5_V001, M5_V002, M5_V003, M5_V004, M5_V005, M5_V006, M5_V007, M5_V008, 
   M5_V009, M5_V010, M5_V011, M5_V012, M5_V013, M5_V014, M5_V015, M5_V016, 
   M5_V017, M5_V018, M5_V019, M5_V020, M5_V021, M5_V022, M5_V023, M5_V024, 
   M5_V025, M5_V026, M5_V027, M5_V028, M5_V029, M5_V030, M5_V031, M5_V032, 
   M5_V033, M5_V034, M5_V035, M5_V036, M5_V037, M5_V038, M5_V039, M5_V040, 
   M5_V041, M5_V042, M5_V043, M5_V044, M5_V045, M5_V046, M5_V047)
###### 1. Correlation analysis #################################################
# 1.1 Build Correlation Matrix ......................................
dfnum <-data.frame(df)
i = 1
for(var in dfnum){
    if(class(var)!="numeric"){dfnum[,i] <- as.numeric(dfnum[,i])}
  i = i+1}
CorMat <- cor(dfnum, method = c("pearson", "kendall", "spearman"))
# 1.2 Build dataframe of highly correlated vars .....................
library(dplyr)
cor_df <- as.data.frame(as.table(CorMat))
cor_df %>%  arrange(desc(Freq)) %>% filter(Freq>0.7) -> cor_df
cor_df <- cor_df[-c(1:194),]
dup <- duplicated(t(apply(cor_df, 1, sort)))
dup <- data.frame(dup)
cor_df <- bind_cols(cor_df,dup)
cor_df <- filter(cor_df,dup == TRUE) 
# 1.3 Function to find Variables to delete ..........................
FindVars2Del <- function(Species,Threshold) {
  Vars2Del <- list()
  for(i in 1:nrow(cor_df[])){
    if(cor_df[i,3]>Threshold){
      if(CorMat[Species,cor_df[i,1]]==CorMat[Species,cor_df[i,2]]){
        Vars2Del <- append(Vars2Del,cor_df[i,1])
        Vars2Del <- append(Vars2Del,cor_df[i,2])} else {
          A <- abs(CorMat[Species,cor_df[i,1]])
          B <- abs(CorMat[Species,cor_df[i,2]])
          if(A > B){Vars2Del <- append(Vars2Del,cor_df[i,2])} else{
            Vars2Del <- append(Vars2Del,cor_df[i,1])}}}} 
  Vars2Del <- as.data.frame(Vars2Del)
  Vars2Del <- unique(Vars2Del)
  factor_columns <- sapply(Vars2Del, is.factor)
  Vars2Del[factor_columns] <- lapply(Vars2Del[factor_columns], as.character)
  return(Vars2Del)
}
# 1.4 Run function for the 8 species ...............................
Tresh <- 0.70
Vars2DelSp1 <- FindVars2Del("PFBS",Tresh)
Vars2DelSp2 <- FindVars2Del("PFHpA",Tresh)
Vars2DelSp3 <- FindVars2Del("PFHxA",Tresh)
Vars2DelSp4 <- FindVars2Del("PFHxS",Tresh)
Vars2DelSp5 <- FindVars2Del("PFOA",Tresh)
Vars2DelSp6 <- FindVars2Del("PFOA_PFOS",Tresh)
Vars2DelSp7 <- FindVars2Del("PFOS",Tresh)
Vars2DelSp8 <- FindVars2Del("Total_PFAS",Tresh)
###### 2. Bayesian Variable Selection ##########################################
library("BayesVarSel")
# 2.1 Species 1 PFBS ...............................................
dfSP1 <- data.frame(df[,c(1,9:194)]) 
dfSP1 <- dfSP1[ , !names(dfSP1) %in% Vars2DelSp1$Vars2Del]
dfSP1 = subset(dfSP1, select = -c(M3_A1) )
dfSP1$M3_A2 <- as.factor(dfSP1$M3_A2)
dfSP1$M3_A3 <- as.factor(dfSP1$M3_A3)
dfSP1$M3_A4 <- as.factor(dfSP1$M3_A4)
dfSP1$M3_A5 <- as.factor(dfSP1$M3_A5)
dfSP1$M3_A6 <- as.factor(dfSP1$M3_A6)
dfSP1$M3_A7 <- as.factor(dfSP1$M3_A7)
dfSP1 = subset(dfSP1, select = -c(M3_L3) )
dfSP1 = subset(dfSP1, select = -c(M3_Q4) ) 
dfSP1 = subset(dfSP1, select = -c(M5_V028) )
dfSP1 = subset(dfSP1, select = -c(M5_V045) )
dfSP1 <- na.omit(dfSP1)
BVSsp1.GibbsBvsF <- GibbsBvsF(formula = PFBS ~ ., 
                              data=dfSP1, # 
                              prior.betas="Robust",
                              prior.models="SBSB", n.iter=10000, 
                              init.model="Full", n.burnin=100,  
                              time.test = FALSE)
# 2.2 Species 2 PFHpA ..............................................
dfSP2 <- data.frame(df[,c(2,9:194)]) 
dfSP2 <- dfSP2[ , !names(dfSP2) %in% Vars2DelSp2$Vars2Del]
dfSP2 = subset(dfSP2, select = -c(M3_A1) )
dfSP2$M3_A2 <- as.factor(dfSP2$M3_A2)
dfSP2$M3_A3 <- as.factor(dfSP2$M3_A3)
dfSP2$M3_A4 <- as.factor(dfSP2$M3_A4)
dfSP2$M3_A5 <- as.factor(dfSP2$M3_A5)
dfSP2$M3_A6 <- as.factor(dfSP2$M3_A6)
dfSP2$M3_A7 <- as.factor(dfSP2$M3_A7)
dfSP2 = subset(dfSP2, select = -c(M5_V028) )
dfSP2 = subset(dfSP2, select = -c(M5_V045) )
dfSP2 <- na.omit(dfSP2)
BVSsp2.GibbsBvsF <- GibbsBvsF(formula = PFHpA ~ .,
                              data=dfSP2,
                              prior.betas="Robust",
                              prior.models="SBSB", n.iter=10000, 
                              init.model="Full", n.burnin=100,  
                              time.test = FALSE)
# 2.3 Species 3 PFHxA ..............................................
dfSP3 <- data.frame(df[,c(3,9:194)]) 
dfSP3 <- dfSP3[ , !names(dfSP3) %in% Vars2DelSp3$Vars2Del]
dfSP3 = subset(dfSP3, select = -c(M3_A1) )
dfSP3$M3_A2 <- as.factor(dfSP3$M3_A2)
dfSP3$M3_A3 <- as.factor(dfSP3$M3_A3)
dfSP3$M3_A4 <- as.factor(dfSP3$M3_A4)
dfSP3$M3_A5 <- as.factor(dfSP3$M3_A5)
dfSP3$M3_A6 <- as.factor(dfSP3$M3_A6)
dfSP3$M3_A7 <- as.factor(dfSP3$M3_A7)
dfSP3 = subset(dfSP3, select = -c(M3_L3) )
dfSP3 = subset(dfSP3, select = -c(M5_V028) )
dfSP3 = subset(dfSP3, select = -c(M5_V045) )
dfSP3 <- na.omit(dfSP3)
BVSsp3.GibbsBvsF <- GibbsBvsF(formula = PFHxA ~ .,
                              data=dfSP3,
                              prior.betas="Robust",
                              prior.models="SBSB", n.iter=10000,
                              init.model="Full", n.burnin=100,
                              time.test = FALSE)
# 2.4 Species 4 PFHxS ..............................................
dfSP4 <- data.frame(df[,c(4,9:194)]) 
dfSP4 <- dfSP4[ , !names(dfSP4) %in% Vars2DelSp4$Vars2Del]
dfSP4 = subset(dfSP4, select = -c(M3_A1) )
dfSP4$M3_A2 <- as.factor(dfSP4$M3_A2)
dfSP4$M3_A3 <- as.factor(dfSP4$M3_A3)
dfSP4$M3_A4 <- as.factor(dfSP4$M3_A4)
dfSP4$M3_A5 <- as.factor(dfSP4$M3_A5)
dfSP4$M3_A6 <- as.factor(dfSP4$M3_A6)
dfSP4$M3_A7 <- as.factor(dfSP4$M3_A7)
dfSP4 = subset(dfSP4, select = -c(M3_L3) )
dfSP4$M3_Q1 <- as.factor(dfSP4$M3_Q1)
dfSP4 = subset(dfSP4, select = -c(M5_V028) )
dfSP4 = subset(dfSP4, select = -c(M5_V045) )
dfSP4 <- na.omit(dfSP4)
BVSsp4.GibbsBvsF <- GibbsBvsF(formula = PFHxS ~ .,
                              data=dfSP4,
                              prior.betas="Robust",
                              prior.models="SBSB", n.iter=10000, 
                              init.model="Full", n.burnin=100,
                              time.test = FALSE)
# 2.5 Species 5 PFOA ..............................................
dfSP5 <- data.frame(df[,c(5,9:194)]) 
dfSP5 <- dfSP5[ , !names(dfSP5) %in% Vars2DelSp5$Vars2Del]
dfSP5 = subset(dfSP5, select = -c(M3_A1) )
dfSP5$M3_A2 <- as.factor(dfSP5$M3_A2)
dfSP5$M3_A3 <- as.factor(dfSP5$M3_A3)
dfSP5$M3_A4 <- as.factor(dfSP5$M3_A4)
dfSP5$M3_A5 <- as.factor(dfSP5$M3_A5)
dfSP5$M3_A6 <- as.factor(dfSP5$M3_A6)
dfSP5$M3_A7 <- as.factor(dfSP5$M3_A7)
dfSP5 = subset(dfSP5, select = -c(M3_L3) )
dfSP5$M3_Q1 <- as.factor(dfSP5$M3_Q1)
dfSP5 = subset(dfSP5, select = -c(M5_V028) )
dfSP5 = subset(dfSP5, select = -c(M5_V045) )
dfSP5 <- na.omit(dfSP5)
BVSsp5.GibbsBvsF <- GibbsBvsF(formula = PFOA ~ .,
                              data=dfSP5,
                              prior.betas="Robust",
                              prior.models="SBSB", n.iter=10000,
                              init.model="Full", n.burnin=100,  
                              time.test = FALSE)
# 2.6 Species 6 PFOA + PFOS .......................................
dfSP6 <- data.frame(df[,c(6,9:194)]) 
dfSP6 <- dfSP6[ , !names(dfSP6) %in% Vars2DelSp6$Vars2Del]
dfSP6 = subset(dfSP6, select = -c(M3_A1) )
dfSP6$M3_A2 <- as.factor(dfSP6$M3_A2)
dfSP6$M3_A3 <- as.factor(dfSP6$M3_A3)
dfSP6$M3_A4 <- as.factor(dfSP6$M3_A4)
dfSP6$M3_A5 <- as.factor(dfSP6$M3_A5)
dfSP6$M3_A6 <- as.factor(dfSP6$M3_A6)
dfSP6$M3_A7 <- as.factor(dfSP6$M3_A7)
dfSP6 = subset(dfSP6, select = -c(M3_L3) )
dfSP6$M3_Q1 <- as.factor(dfSP6$M3_Q1)
dfSP6 = subset(dfSP6, select = -c(M5_V028) )
dfSP6 = subset(dfSP6, select = -c(M5_V045) )
dfSP6 <- na.omit(dfSP6)
BVSsp6.GibbsBvsF <- GibbsBvsF(formula = PFOA_PFOS ~ .,
                              data=dfSP6,
                              prior.betas="Robust",
                              prior.models="SBSB", n.iter=10000, 
                              init.model="Full", n.burnin=100,
                              time.test = FALSE)
# 2.7 Species 7 PFOS ..............................................
dfSP7 <- data.frame(df[,c(7,9:194)]) 
dfSP7 <- dfSP7[ , !names(dfSP7) %in% Vars2DelSp7$Vars2Del]
dfSP7 = subset(dfSP7, select = -c(M3_A1) )
dfSP7$M3_A2 <- as.factor(dfSP7$M3_A2)
dfSP7$M3_A3 <- as.factor(dfSP7$M3_A3)
dfSP7$M3_A4 <- as.factor(dfSP7$M3_A4)
dfSP7$M3_A5 <- as.factor(dfSP7$M3_A5)
dfSP7$M3_A6 <- as.factor(dfSP7$M3_A6)
dfSP7$M3_A7 <- as.factor(dfSP7$M3_A7)
dfSP7 = subset(dfSP7, select = -c(M3_L3) )
dfSP7 = subset(dfSP7, select = -c(M3_Q4) ) 
dfSP7$M3_Q1 <- as.factor(dfSP7$M3_Q1)
dfSP7 = subset(dfSP7, select = -c(M5_V028) )
dfSP7 = subset(dfSP7, select = -c(M5_V045) )
dfSP7 <- na.omit(dfSP7)
BVSsp7.GibbsBvsF <- GibbsBvsF(formula = PFOS ~ .,
                              data=dfSP7,
                              prior.betas="Robust",
                              prior.models="SBSB", n.iter=10000, 
                              init.model="Full", n.burnin=100,  
                              time.test = FALSE)
# 2.8 Species 8 Total PFAS ........................................
dfSP8 <- data.frame(df[,c(8,9:194)]) 
dfSP8 <- dfSP8[ , !names(dfSP8) %in% Vars2DelSp8$Vars2Del]
dfSP8 = subset(dfSP8, select = -c(M3_A1) )
dfSP8$M3_A2 <- as.factor(dfSP8$M3_A2)
dfSP8$M3_A3 <- as.factor(dfSP8$M3_A3)
dfSP8$M3_A4 <- as.factor(dfSP8$M3_A4)
dfSP8$M3_A5 <- as.factor(dfSP8$M3_A5)
dfSP8$M3_A6 <- as.factor(dfSP8$M3_A6)
dfSP8$M3_A7 <- as.factor(dfSP8$M3_A7)
dfSP8 = subset(dfSP8, select = -c(M3_L3) )
dfSP8$M3_Q1 <- as.factor(dfSP8$M3_Q1)
dfSP8 = subset(dfSP8, select = -c(M5_V028) )
dfSP8 = subset(dfSP8, select = -c(M5_V045) )
dfSP8 <- na.omit(dfSP8)
BVSsp8.GibbsBvsF <- GibbsBvsF(formula = Total_PFAS ~ .,
                              data=dfSP8,
                              prior.betas="Robust",
                              prior.models="SBSB", n.iter=10000, 
                              init.model="Full", n.burnin=100,  
                              time.test = FALSE)
######################### END CODE #############################################