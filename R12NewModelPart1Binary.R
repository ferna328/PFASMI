######################## R12NewModelPart1Binary.R ##############################
# Research Paper: Large-scale Assessment of PFAS Compounds in Drinking Water   #
# Sources through Machine Learning                                             #
# Nicolas Fernandez, A. Pouyan Nejadhashemi, Christian Loveall                 #
# Department of Biosystems and Agricultural Engineering                        #
# Michigan State University, East Lansing, MI, United States                   #
# Code written and curated by Nicolas Fernandez (OrcID 0000-0001-7979-2941)    #
######################## Description ###########################################
# According to the Paper, this code is used to implement the New Model,        #
# specifically the first part to represent binary data                         #
# (i.e.detection/non-detection of the eigth PFAS species)                      #
################################################################################
###### 0. IMPORT AND  PREPARE DATA #############################################
# 0.0 Set Working Directory
wd = 'E:/PFASMIproject'
setwd(wd)
# 0.1 Read Input Information ---------------------------------------------------
# 0.1.1 Model 1 (Hu et al 2016) ........................
dirDep = 'E:/XXX/YYY/ZZZ/DataExp1V2Dep.csv'
dirExp = 'E:/XXX/YYY/ZZZ/DataExp1V2Exp.csv'
Dep <- read.csv(file = dirDep) # Dependent Variables
Exp <- read.csv(file = dirExp) # Explanatory Variables
# 0.1.2 Model 2 (Hu et al 2021) ........................
DirInfo <- 'E:/XXX/YYY/ZZZ/PFASMIGWdatabaseV2.xlsx'
library('readxl')
Data2 <- read_excel(DirInfo, sheet = 'Data')
Dict2 <- read_excel(DirInfo, sheet = 'Dictionary')
# 0.1.3 Model 3 (George & Dixit 2021) ..................
DirInfo <- 'E:/XXX/YYY/ZZZ/MasterFile.xlsx'
Data3 <- read_excel(DirInfo)
# 0.1.4 Model 4 (McMahon et al 2022) ...................
DirInfo <- 'E:/XXX/YYY/ZZZ/PFASMIGWdatabaseV3.xlsx'
Data4 <- read_excel(DirInfo, sheet = 'Data')
Dict4 <- read_excel(DirInfo, sheet = 'Dictionary')
rm(dirDep, dirExp, dirin, DirInfo)
# 0.2 Organize Input Information -----------------------------------------------
# 0.2.0 Coordinates............................
lonX <- Data3$X_coord
latY <- Data3$Y_Coord 
df <- data.frame(lonX, latY)
rm(lonX, latY)
# 0.2.1 Dependent Variables............................
PFBS <- InputData$PFBS_N
PFBS_B <- InputData$PFBS_B
PFBS_L <- InputData$PFBS_L
PFBS_L[which(!is.finite(PFBS_L))] <- 0 # replace - inf for 0
PFHpA <- InputData$PFHpA_N
PFHpA_B <- InputData$PFHpA_B
PFHpA_L <- InputData$PFHpA_L
PFHpA_L[which(!is.finite(PFHpA_L))] <- 0 # replace - inf for 0
PFHxA <- InputData$PFHxA_N
PFHxA_B <- InputData$PFHxA_B
PFHxA_L <- InputData$PFHxA_L
PFHxA_L[which(!is.finite(PFHxA_L))] <- 0 # replace - inf for 0
PFHxS <- InputData$PFHxS_N
PFHxS_B <- InputData$PFHxS_B
PFHxS_L <- InputData$PFHxS_L
PFHxS_L[which(!is.finite(PFHxS_L))] <- 0 # replace - inf for 0
PFOA <- InputData$PFOA_N
PFOA_B <- InputData$PFOA_B
PFOA_L <- InputData$PFOA_L
PFOA_L[which(!is.finite(PFOA_L))] <- 0 # replace - inf for 0
PFOA_PFOS <- InputData$PFOA_PFOS__N
PFOA_PFOS_B <- InputData$PFOA_PFOS__B
PFOA_PFOS_L <- InputData$PFOA_PFOS__L
PFOA_PFOS_L[which(!is.finite(PFOA_PFOS_L))] <- 0 # replace - inf for 0
PFOS <- InputData$PFOS_N
PFOS_B <- InputData$PFOS_B
PFOS_L <- InputData$PFOS_L
PFOS_L[which(!is.finite(PFOS_L))] <- 0 # replace - inf for 0
Total_PFAS <- InputData$Total_PFAS_N
Total_PFAS_B <- InputData$Total_PFAS_B
Total_PFAS_L <- InputData$Total_PFAS_L
Total_PFAS_L[which(!is.finite(Total_PFAS_L))] <- 0 # replace - inf for 0
df <- cbind(df,
            PFBS, PFHpA, PFHxA, PFHxS, PFOA, PFOA_PFOS, PFOS, Total_PFAS,
            PFBS_B, PFHpA_B, PFHxA_B, PFHxS_B, PFOA_B, PFOA_PFOS_B, PFOS_B, 
            Total_PFAS_B,
            PFBS_L, PFHpA_L, PFHxA_L, PFHxS_L, PFOA_L, PFOA_PFOS_L, PFOS_L, 
            Total_PFAS_L)
rm(PFBS,PFHpA,PFHxA,PFHxS,PFOA,PFOA_PFOS,PFOS,Total_PFAS,PFBS_B, PFHpA_B, 
   PFHxA_B, PFHxS_B, PFOA_B, PFOA_PFOS_B, PFOS_B, Total_PFAS_B, PFBS_L, 
   PFHpA_L, PFHxA_L, PFHxS_L, PFOA_L, PFOA_PFOS_L, PFOS_L, Total_PFAS_L)
# 0.2.2 Model 1 .......................................
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
# 0.2.3 Model 2 .......................................
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
# 0.2.4 Model 3 .......................................
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
# 0.2.5 Model 4 .......................................
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
# 0.2.6 New Model .......................................
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
# 0.2.7 Delete repeated positions ........................
LatLong <- data.frame(X = df$lonX, Y = df$latY)
LatLongUnique <- unique(LatLong)
RowsLatLong <- rownames(LatLong)
RowsLatLongUnique <- rownames(LatLongUnique)
Rows2del <- setdiff(RowsLatLong, RowsLatLongUnique)
dfspat <- subset(df, !(row.names(df) %in% Rows2del))
# 0.2.8 DELETE NAN .......................................
dfspat <- na.omit(dfspat)
# 1. Fit Binary Data ###########################################################
library(tidyverse)
library(tidymodels)
# 1.1 Species 1: PFBS ----------------------------------------------------------
# 1.1.1 Prepare Data ...........................................
dfBinFitSp1 <- dfspat[,c('PFBS_B','M1_P3_Buf01','M2_H4_Buf01', 'M3_A3', 'M4_I43',
                         'M5_V012','M5_V043')]
dfBinFitSp1$PFBS_B <- as.factor(dfBinFitSp1$PFBS_B)
dfBinFitSp1$M1_P3_Buf01 <- as.factor(dfBinFitSp1$M1_P3_Buf01)
dfBinFitSp1$M3_A3 <- as.factor(dfBinFitSp1$M3_A3)
# Reduce number of categorical vars (less than 50 allowed)
dfLinSp1 <- dfspat[,c('PFBS_L','M1_P3_Buf01','M2_H4_Buf01', 'M3_A3', 'M4_I43',
                      'M5_V012','M5_V043')]
formSp1 <- PFBS_L ~ factor(M1_P3_Buf01) + M2_H4_Buf01 + factor(M3_A3) + M4_I43 + 
  M5_V012 + M5_V043
modelSp1 <- lm(formula = formSp1, data = dfLinSp1) # fit linear model
modelSp1Sum <-summary(modelSp1) # sumarize model
modelSp1SumAtr <- attributes(modelSp1Sum$coefficients)
# We keep only significant categories from linear model
Cat2KeepSp1 <- c(21,22,23,25,36,37,40,47,49,52,53,61,64,72,77,83,86,90,93,110,
                 112,113,117,132,133,134,135,136,166,169,171,186,198)
for(i in 1:length(dfBinFitSp1$M5_V043)){
  if(!(dfBinFitSp1$M5_V043[i] %in% Cat2KeepSp1)){
    dfBinFitSp1$M5_V043[i]<-0}}
dfBinFitSp1$M5_V043 <- droplevels(dfBinFitSp1$M5_V043)
# 1.1.2 Set Tuning Specifications ..............................
rfTuneSpecsSp1 <- rand_forest(mode = "classification",
                              engine = "randomForest", 
                              mtry = tune(),
                              trees = tune(),
                              min_n = tune())
# 1.1.3 Set Initial Split ......................................
set.seed(123456)
DataSplitSp1 <- initial_split(dfBinFitSp1, prop = 3/4, strata = PFBS_B)
DataTrainSp1 <- training(DataSplitSp1)
DataTestSp1  <- testing(DataSplitSp1)
# 1.1.4 Set folds for cross validation ........................
foldsSp1 <- vfold_cv(data = DataTrainSp1,
                   v = 10, repeats = 1,
                   strata = "PFBS_B",
                   pool = 0.1)
# 1.1.5 Set Tuning grid .......................................
tune_mtry <- range_set(mtry(), c(2, 10))
tune_min_n <- range_set(min_n(), c(8, 32))
tune_trees <- range_set(trees(), c(300, 1300))
rf_gridSp1 <- grid_regular(
  finalize(mtry(), subset(DataTrainSp1, select = M1_P3_Buf01:M5_V043)), 
  trees(), 
  min_n(), 
  levels = 5)
# 1.1.6 Tune model on grid ....................................
tunerf_wfSp1 <- workflow() %>% add_model(rfTuneSpecsSp1) %>% 
  add_formula(PFBS_B ~ M1_P3_Buf01 + M2_H4_Buf01 + M3_A3 + M4_I43 + M5_V012 + M5_V043)
tunerf_resSp1 <- tunerf_wfSp1 %>% tune_grid(resamples = foldsSp1, grid = rf_gridSp1)
# 1.1.7 Get results ...........................................
tunerf_resSp1 %>% show_best(metric = "accuracy")
tunerf_resSp1 %>% show_best(metric = "roc_auc")
collect_metrics(tunerf_resSp1) -> metricsSp1
# 1.1.8 Select best models ....................................
best_rfSp11 <- tunerf_resSp1 %>% select_best(metric = "accuracy")
best_rfSp12 <- tunerf_resSp1 %>% select_best(metric = "roc_auc")
# 1.1.9 Finalize workflowS ....................................
final_wfSp11 <- tunerf_wfSp1 %>% finalize_workflow(best_rfSp11)
final_wfSp12 <- tunerf_wfSp1 %>% finalize_workflow(best_rfSp12)
# 1.1.10 Fit final models on full training data and evaluate on test data ......
final_fitSp11 <- final_wfSp11 %>% last_fit(DataSplitSp1) 
final_fitSp12 <- final_wfSp12 %>% last_fit(DataSplitSp1)
# 1.1.11 Collect metrics ......................................
final_fitSp11 %>% collect_metrics()
final_fitSp12 %>% collect_metrics()
# 1.1.10 Collect models to predict ............................
final_rfSp11 <- extract_workflow(final_fitSp11)
final_rfSp12 <- extract_workflow(final_fitSp12)
# 1.2 Species 2: PFHpA ---------------------------------------------------------
# 1.2.1 Prepare Data ...........................................
dfBinFitSp2 <- dfspat[,c('PFHpA_B','M2_P1','M3_Q2', 'M5_V009', 'M5_V019',
                         'M5_V020','M5_V034','M5_V043')]
dfBinFitSp2$PFHpA_B <- as.factor(dfBinFitSp2$PFHpA_B)
dfBinFitSp2$M3_Q2 <- as.factor(dfBinFitSp2$M3_Q2)
# Reduce number of categorical vars (less than 50 allowed)
dfLinSp2 <- dfspat[,c('PFHpA_L','M2_P1','M3_Q2', 'M5_V009', 'M5_V019',
                      'M5_V020','M5_V034','M5_V043')]
formSp2 <- PFHpA_L ~ M2_P1 + factor(M3_Q2) + M5_V009 + factor(M5_V019) + 
  factor(M5_V020) + M5_V034 + factor(M5_V043)
modelSp2 <- lm(formula = formSp2, data = dfLinSp2) # fit linear model
modelSp2Sum <-summary(modelSp2) # sumarize model
modelSp2SumAtr <- attributes(modelSp2Sum$coefficients)
# We keep only significant categories from linear model
Cat2KeepSp2 <- c(18,24,28,57,64,107,111,165,182)
for(i in 1:length(dfBinFitSp2$M5_V043)){
  if(!(dfBinFitSp2$M5_V043[i] %in% Cat2KeepSp2)){
    dfBinFitSp2$M5_V043[i]<-0}}
dfBinFitSp2$M5_V043 <- droplevels(dfBinFitSp2$M5_V043)
# 1.2.2 Set Tuning Specifications ..............................
rfTuneSpecsSp2 <- rand_forest(mode = "classification",
                              engine = "randomForest", 
                              mtry = tune(),
                              trees = tune(),
                              min_n = tune())
# 1.2.3 Set Initial Split ......................................
set.seed(123456)
DataSplitSp2 <- initial_split(dfBinFitSp2, prop = 3/4, strata = PFHpA_B)
DataTrainSp2 <- training(DataSplitSp2)
DataTestSp2  <- testing(DataSplitSp2)
# 1.2.4 Set folds for cross validation ........................
foldsSp2 <- vfold_cv(data = DataTrainSp2,
                     v = 10, repeats = 1,
                     strata = "PFHpA_B",
                     pool = 0.1)
# 1.2.5 Set Tuning grid .......................................
tune_mtry <- range_set(mtry(), c(2, 10))
tune_min_n <- range_set(min_n(), c(8, 32))
tune_trees <- range_set(trees(), c(300, 1300))
rf_gridSp2 <- grid_regular(
  finalize(mtry(), subset(DataTrainSp2, select = M2_P1:M5_V043)), 
  trees(), 
  min_n(), 
  levels = 5)
# 1.2.6 Tune model on grid ....................................
tunerf_wfSp2 <- workflow() %>% add_model(rfTuneSpecsSp2) %>% 
  add_formula(PFHpA_B ~ M2_P1 + M3_Q2 + M5_V009 + M5_V019 + M5_V020 + M5_V034 + M5_V043)
tunerf_resSp2 <- tunerf_wfSp2 %>% tune_grid(resamples = foldsSp2, grid = rf_gridSp2)
# 1.2.7 Get results ...........................................
tunerf_resSp2 %>% show_best(metric = "accuracy")
tunerf_resSp2 %>% show_best(metric = "roc_auc")
collect_metrics(tunerf_resSp2) -> metricsSp2
# 1.2.8 Select best models ....................................
best_rfSp21 <- tunerf_resSp2 %>% select_best(metric = "accuracy")
best_rfSp22 <- tunerf_resSp2 %>% select_best(metric = "roc_auc")
# 1.2.9 Finalize workflowS ....................................
final_wfSp21 <- tunerf_wfSp2 %>% finalize_workflow(best_rfSp21)
final_wfSp22 <- tunerf_wfSp2 %>% finalize_workflow(best_rfSp22)
# 1.2.10 Fit final models on full training data and evaluate on test data ......
final_fitSp21 <- final_wfSp21 %>% last_fit(DataSplitSp2) 
final_fitSp22 <- final_wfSp22 %>% last_fit(DataSplitSp2)
# 1.2.11 Collect metrics ......................................
final_fitSp21 %>% collect_metrics()
final_fitSp22 %>% collect_metrics()
# 1.2.10 Collect models to predict ............................
final_rfSp21 <- extract_workflow(final_fitSp21)
final_rfSp22 <- extract_workflow(final_fitSp22)
# 1.3 Species 3: PFHxA ----------------------------------------------------------
# 1.3.1 Prepare Data ...........................................
dfBinFitSp3 <- dfspat[,c('PFHxA_B','M2_P1','M3_Q2', 'M4_S2','M5_V009', 'M5_V029',
                         'M5_V034','M5_V043')]
dfBinFitSp3$PFHxA_B <- as.factor(dfBinFitSp3$PFHxA_B)
dfBinFitSp3$M3_Q2 <- as.factor(dfBinFitSp3$M3_Q2)
# Reduce number of categorical vars (less than 50 allowed)
dfLinSp3 <- dfspat[,c('PFHxA_L','M2_P1','M3_Q2', 'M4_S2','M5_V009', 'M5_V029',
                      'M5_V034','M5_V043')]
formSp3 <- PFHxA_L ~ M2_P1 + factor(M3_Q2) + M4_S2 + M5_V009 + factor(M5_V029) + 
  M5_V034 + factor(M5_V043)
modelSp3 <- lm(formula = formSp3, data = dfLinSp3) # fit linear model
modelSp3Sum <-summary(modelSp3) # sumarize model
modelSp3SumAtr <- attributes(modelSp3Sum$coefficients)
# We keep only significant categories from linear model
Cat2KeepSp3 <- c(18,28,47,64,99,102,111,141,155,165,182,183)
for(i in 1:length(dfBinFitSp3$M5_V043)){
  if(!(dfBinFitSp3$M5_V043[i] %in% Cat2KeepSp3)){
    dfBinFitSp3$M5_V043[i]<-0}}
dfBinFitSp3$M5_V043 <- droplevels(dfBinFitSp3$M5_V043)
# 1.3.2 Set Tuning Specifications ..............................
rfTuneSpecsSp3 <- rand_forest(mode = "classification",
                              engine = "randomForest", 
                              mtry = tune(),
                              trees = tune(),
                              min_n = tune())
# 1.3.3 Set Initial Split ......................................
set.seed(123456)
DataSplitSp3 <- initial_split(dfBinFitSp3, prop = 3/4, strata = PFHxA_B)
DataTrainSp3 <- training(DataSplitSp3)
DataTestSp3  <- testing(DataSplitSp3)
# 1.3.4 Set folds for cross validation ........................
foldsSp3 <- vfold_cv(data = DataTrainSp3,
                     v = 10, repeats = 1,
                     strata = "PFHxA_B",
                     pool = 0.1)
# 1.3.5 Set Tuning grid .......................................
tune_mtry <- range_set(mtry(), c(2, 10))
tune_min_n <- range_set(min_n(), c(8, 32))
tune_trees <- range_set(trees(), c(300, 1300))
rf_gridSp3 <- grid_regular(
  finalize(mtry(), subset(DataTrainSp3, select = M2_P1:M5_V043)), 
  trees(), 
  min_n(), 
  levels = 5)
# 1.3.6 Tune model on grid ....................................
tunerf_wfSp3 <- workflow() %>% add_model(rfTuneSpecsSp3) %>% 
  add_formula(PFHxA_B ~ M2_P1 + M3_Q2 + M4_S2 + M5_V009 + M5_V029 + M5_V034 + 
                M5_V043)
tunerf_resSp3 <- tunerf_wfSp3 %>% tune_grid(resamples = foldsSp3, grid = rf_gridSp3)
# 1.3.7 Get results ...........................................
tunerf_resSp3 %>% show_best(metric = "accuracy")
tunerf_resSp3 %>% show_best(metric = "roc_auc")
collect_metrics(tunerf_resSp3) -> metricsSp3
# 1.3.8 Select best models ....................................
best_rfSp31 <- tunerf_resSp3 %>% select_best(metric = "accuracy")
best_rfSp32 <- tunerf_resSp3 %>% select_best(metric = "roc_auc")
# 1.3.9 Finalize workflowS ....................................
final_wfSp31 <- tunerf_wfSp3 %>% finalize_workflow(best_rfSp31)
final_wfSp32 <- tunerf_wfSp3 %>% finalize_workflow(best_rfSp32)
# 1.3.10 Fit final models on full training data and evaluate on test data ......
final_fitSp31 <- final_wfSp31 %>% last_fit(DataSplitSp3) 
final_fitSp32 <- final_wfSp32 %>% last_fit(DataSplitSp3)
# 1.3.11 Collect metrics .....................................
final_fitSp31 %>% collect_metrics()
final_fitSp32 %>% collect_metrics()
# 1.3.10 Collect models to predict ...........................
final_rfSp31 <- extract_workflow(final_fitSp31)
final_rfSp32 <- extract_workflow(final_fitSp32)
# 1.4 Species 4: PFHxS ---------------------------------------------------------
# 1.4.1 Prepare Data ...........................................
dfBinFitSp4 <- dfspat[,c('PFHxS_B','M2_P1','M3_Q1', 'M5_V029','M5_V043')]
dfBinFitSp4$PFHxS_B <- as.factor(dfBinFitSp4$PFHxS_B)
dfBinFitSp4$M3_Q1 <- as.factor(dfBinFitSp4$M3_Q1)
# Reduce number of categorical vars (less than 50 allowed)
dfLinSp4 <- dfspat[,c('PFHxS_L','M2_P1','M3_Q1', 'M5_V029','M5_V043')]
formSp4 <- PFHxS_L ~ M2_P1 + factor(M3_Q1) + factor(M5_V029) + factor(M5_V043)
modelSp4 <- lm(formula = formSp4, data = dfLinSp4) # fit linear model
modelSp4Sum <-summary(modelSp4) # sumarize model
modelSp4SumAtr <- attributes(modelSp4Sum$coefficients)
# We keep only significant categories from linear model
Cat2KeepSp4 <- c(5,8,21,33,35,38,47,53,55,56,69,70,82,84,86,90,111,112,113,115,
                 116,117,121,129,132,133,134,136,145,151,155,163,164,166,168,
                 169,171,194)
for(i in 1:length(dfBinFitSp4$M5_V043)){
  if(!(dfBinFitSp4$M5_V043[i] %in% Cat2KeepSp4)){
    dfBinFitSp4$M5_V043[i]<-0}}
dfBinFitSp4$M5_V043 <- droplevels(dfBinFitSp4$M5_V043)
# 1.4.2 Set Tuning Specifications ..............................
rfTuneSpecsSp4 <- rand_forest(mode = "classification",
                              engine = "randomForest", 
                              mtry = tune(),
                              trees = tune(),
                              min_n = tune())
# 1.4.3 Set Initial Split ......................................
set.seed(123456)
DataSplitSp4 <- initial_split(dfBinFitSp4, prop = 3/4, strata = PFHxS_B)
DataTrainSp4 <- training(DataSplitSp4)
DataTestSp4  <- testing(DataSplitSp4)
# 1.4.4 Set folds for cross validation ........................
foldsSp4 <- vfold_cv(data = DataTrainSp4,
                     v = 10, repeats = 1,
                     strata = "PFHxS_B",
                     pool = 0.1)
# 1.4.5 Set Tuning grid .......................................
tune_mtry <- range_set(mtry(), c(2, 10))
tune_min_n <- range_set(min_n(), c(8, 32))
tune_trees <- range_set(trees(), c(300, 1300))
rf_gridSp4 <- grid_regular(
  finalize(mtry(), subset(DataTrainSp4, select = M2_P1:M5_V043)), 
  trees(), 
  min_n(), 
  levels = 5)
# 1.4.6 Tune model on grid ....................................
tunerf_wfSp4 <- workflow() %>% add_model(rfTuneSpecsSp4) %>% 
  add_formula(PFHxS_B ~ M2_P1 + M3_Q1 + M5_V029 + M5_V043)
tunerf_resSp4 <- tunerf_wfSp4 %>% tune_grid(resamples = foldsSp4, grid = rf_gridSp4)
# 1.4.7 Get results ...........................................
tunerf_resSp4 %>% show_best(metric = "accuracy")
tunerf_resSp4 %>% show_best(metric = "roc_auc")
collect_metrics(tunerf_resSp4) -> metricsSp4
# 1.4.8 Select best models ....................................
best_rfSp41 <- tunerf_resSp4 %>% select_best(metric = "accuracy")
best_rfSp42 <- tunerf_resSp4 %>% select_best(metric = "roc_auc")
# 1.4.9 Finalize workflowS ....................................
final_wfSp41 <- tunerf_wfSp4 %>% finalize_workflow(best_rfSp41)
final_wfSp42 <- tunerf_wfSp4 %>% finalize_workflow(best_rfSp42)
# 1.4.10 Fit final models on full training data and evaluate on test data ......
final_fitSp41 <- final_wfSp41 %>% last_fit(DataSplitSp4) 
final_fitSp42 <- final_wfSp42 %>% last_fit(DataSplitSp4)
# 1.4.11 Collect metrics ......................................
final_fitSp41 %>% collect_metrics()
final_fitSp42 %>% collect_metrics()
# 1.4.10 Collect models to predict ............................
final_rfSp41 <- extract_workflow(final_fitSp41)
final_rfSp42 <- extract_workflow(final_fitSp42)
# 1.5 Species 5: PFOA ----------------------------------------------------------
# 1.5.1 Prepare Data ...........................................
dfBinFitSp5 <- dfspat[,c('PFOA_B','M2_P1','M2_G1_Buf03','M2_H3_Buf01',
                         'M2_H3_Buf03','M4_I43','M5_V009','M5_V024','M5_V029',
                         'M5_V034','M5_V043')]
dfBinFitSp5$PFOA_B <- as.factor(dfBinFitSp5$PFOA_B)
dfBinFitSp5$M2_G1_Buf03 <- as.factor(dfBinFitSp5$M2_G1_Buf03)
# Reduce number of categorical vars (less than 50 allowed)
dfLinSp5 <- dfspat[,c('PFOA_L','M2_P1','M2_G1_Buf03','M2_H3_Buf01',
                         'M2_H3_Buf03','M4_I43','M5_V009','M5_V024','M5_V029',
                         'M5_V034','M5_V043')]
formSp5 <- PFOA_L ~ M2_P1 + factor(M2_G1_Buf03) + M2_H3_Buf01 + M2_H3_Buf03 + 
  M4_I43 + M5_V009 + M5_V024 + factor(M5_V029) + M5_V034 + factor(M5_V043)
modelSp5 <- lm(formula = formSp5, data = dfLinSp5) # fit linear model
modelSp5Sum <-summary(modelSp5) # sumarize model
modelSp5SumAtr <- attributes(modelSp5Sum$coefficients)
# We keep only significant categories from linear model
Cat2KeepSp5 <- c(11,21,25,4753,57,59,77,82,83,99,133,165,166,171)
for(i in 1:length(dfBinFitSp5$M5_V043)){
  if(!(dfBinFitSp5$M5_V043[i] %in% Cat2KeepSp5)){
    dfBinFitSp5$M5_V043[i]<-0}}
dfBinFitSp5$M5_V043 <- droplevels(dfBinFitSp5$M5_V043)
# 1.5.2 Set Tuning Specifications ..............................
rfTuneSpecsSp5 <- rand_forest(mode = "classification",
                              engine = "randomForest", 
                              mtry = tune(),
                              trees = tune(),
                              min_n = tune())
# 1.5.3 Set Initial Split ......................................
set.seed(123456)
DataSplitSp5 <- initial_split(dfBinFitSp5, prop = 3/4, strata = PFOA_B)
DataTrainSp5 <- training(DataSplitSp5)
DataTestSp5  <- testing(DataSplitSp5)
# 1.5.4 Set folds for cross validation ........................
foldsSp5 <- vfold_cv(data = DataTrainSp5,
                     v = 10, repeats = 1,
                     strata = "PFOA_B",
                     pool = 0.1)
# 1.5.5 Set Tuning grid .......................................
tune_mtry <- range_set(mtry(), c(2, 10))
tune_min_n <- range_set(min_n(), c(8, 32))
tune_trees <- range_set(trees(), c(300, 1300))
rf_gridSp5 <- grid_regular(
  finalize(mtry(), subset(DataTrainSp5, select = M2_P1:M5_V043)), 
  trees(), 
  min_n(), 
  levels = 5)
# 1.5.6 Tune model on grid ....................................
tunerf_wfSp5 <- workflow() %>% add_model(rfTuneSpecsSp5) %>% 
  add_formula(PFOA_B ~ M2_P1 + M2_G1_Buf03 + M2_H3_Buf01 + M2_H3_Buf03 + 
                M4_I43 + M5_V009 + M5_V024 + M5_V029 + M5_V034 + M5_V043)
tunerf_resSp5 <- tunerf_wfSp5 %>% tune_grid(resamples = foldsSp5, grid = rf_gridSp5)
# 1.5.7 Get results ...........................................
tunerf_resSp5 %>% show_best(metric = "accuracy")
tunerf_resSp5 %>% show_best(metric = "roc_auc")
collect_metrics(tunerf_resSp5) -> metricsSp5
# 1.5.8 Select best models ....................................
best_rfSp51 <- tunerf_resSp5 %>% select_best(metric = "accuracy")
best_rfSp52 <- tunerf_resSp5 %>% select_best(metric = "roc_auc")
# 1.5.9 Finalize workflowS ....................................
final_wfSp51 <- tunerf_wfSp5 %>% finalize_workflow(best_rfSp51)
final_wfSp52 <- tunerf_wfSp5 %>% finalize_workflow(best_rfSp52)
# 1.5.10 Fit final models on full training data and evaluate on test data ......
final_fitSp51 <- final_wfSp51 %>% last_fit(DataSplitSp5) 
final_fitSp52 <- final_wfSp52 %>% last_fit(DataSplitSp5)
# 1.5.11 Collect metrics ......................................
final_fitSp51 %>% collect_metrics()
final_fitSp52 %>% collect_metrics()
# 1.5.10 Collect models to predict ............................
final_rfSp51 <- extract_workflow(final_fitSp51)
final_rfSp52 <- extract_workflow(final_fitSp52)
# 1.6 Species 6: PFOA + PFOS ----------------------------------------------------
# 1.6.1 Prepare Data ...........................................
dfBinFitSp6 <- dfspat[,c('PFOA_PFOS_B','M2_P1','M2_G1_Buf03','M5_V009',
                         'M5_V029','M5_V034','M5_V043')]
dfBinFitSp6$PFOA_PFOS_B <- as.factor(dfBinFitSp6$PFOA_PFOS_B)
dfBinFitSp6$M2_G1_Buf03 <- as.factor(dfBinFitSp6$M2_G1_Buf03)
# Reduce number of categorical vars (less than 50 allowed)
dfLinSp6 <- dfspat[,c('PFOA_PFOS_L','M2_P1','M2_G1_Buf03','M5_V009',
                      'M5_V029','M5_V034','M5_V043')]
formSp6 <- PFOA_PFOS_L ~ M2_P1 + factor(M2_G1_Buf03) + M5_V009 + 
  factor(M5_V029) + M5_V034 + factor(M5_V043)
modelSp6 <- lm(formula = formSp6, data = dfLinSp6) # fit linear model
modelSp6Sum <-summary(modelSp6) # sumarize model
modelSp6SumAtr <- attributes(modelSp6Sum$coefficients)
# We keep only significant categories from linear model
Cat2KeepSp6 <- c(2,5,9,11,33,38,47,48,55,56,57,61,71,77,81,82,85,86,90,95,99,
                 103,109,111,112,113,116,117,118,121,133,145,150,162,164,165,
                 168,169,171,173,181,183,192)
for(i in 1:length(dfBinFitSp6$M5_V043)){
  if(!(dfBinFitSp6$M5_V043[i] %in% Cat2KeepSp6)){
    dfBinFitSp6$M5_V043[i]<-0}}
dfBinFitSp6$M5_V043 <- droplevels(dfBinFitSp6$M5_V043)
# 1.6.2 Set Tuning Specifications ..............................
rfTuneSpecsSp6 <- rand_forest(mode = "classification",
                              engine = "randomForest", 
                              mtry = tune(),
                              trees = tune(),
                              min_n = tune())
# 1.6.3 Set Initial Split ......................................
set.seed(123456)
DataSplitSp6 <- initial_split(dfBinFitSp6, prop = 3/4, strata = PFOA_PFOS_B)
DataTrainSp6 <- training(DataSplitSp6)
DataTestSp6  <- testing(DataSplitSp6)
# 1.6.4 Set folds for cross validation ........................
foldsSp6 <- vfold_cv(data = DataTrainSp6,
                     v = 10, repeats = 1,
                     strata = "PFOA_PFOS_B",
                     pool = 0.1)
# 1.6.5 Set Tuning grid .......................................
tune_mtry <- range_set(mtry(), c(2, 10))
tune_min_n <- range_set(min_n(), c(8, 32))
tune_trees <- range_set(trees(), c(300, 1300))
rf_gridSp6 <- grid_regular(
  finalize(mtry(), subset(DataTrainSp6, select = M2_P1:M5_V043)), 
  trees(), 
  min_n(), 
  levels = 5)
# 1.6.6 Tune model on grid ....................................
tunerf_wfSp6 <- workflow() %>% add_model(rfTuneSpecsSp6) %>% 
  add_formula(PFOA_PFOS_B ~ M2_P1 + M2_G1_Buf03 + M5_V009 + M5_V029 + M5_V034 
              + M5_V043)
tunerf_resSp6 <- tunerf_wfSp6 %>% tune_grid(resamples = foldsSp6, grid = rf_gridSp6)
# 1.6.7 Get results ...........................................
tunerf_resSp6 %>% show_best(metric = "accuracy")
tunerf_resSp6 %>% show_best(metric = "roc_auc")
collect_metrics(tunerf_resSp6) -> metricsSp6
# 1.6.8 Select best models ....................................
best_rfSp61 <- tunerf_resSp6 %>% select_best(metric = "accuracy")
best_rfSp62 <- tunerf_resSp6 %>% select_best(metric = "roc_auc")
# 1.6.9 Finalize workflowS ....................................
final_wfSp61 <- tunerf_wfSp6 %>% finalize_workflow(best_rfSp61)
final_wfSp62 <- tunerf_wfSp6 %>% finalize_workflow(best_rfSp62)
# 1.6.10 Fit final models on full training data and evaluate on test data ......
final_fitSp61 <- final_wfSp61 %>% last_fit(DataSplitSp6) 
final_fitSp62 <- final_wfSp62 %>% last_fit(DataSplitSp6)
# 1.6.11 Collect metrics ......................................
final_fitSp61 %>% collect_metrics()
final_fitSp62 %>% collect_metrics()
# 1.6.10 Collect models to predict ............................
final_rfSp61 <- extract_workflow(final_fitSp61)
final_rfSp62 <- extract_workflow(final_fitSp62)
# 1.7 Species 7: PFOS ----------------------------------------------------------
# 1.7.1 Prepare Data ...........................................
dfBinFitSp7 <- dfspat[,c('PFOS_B','M2_P1','M2_H4_Buf01','M2_S1_Buf03','M5_V024',
                         'M5_V043')]
dfBinFitSp7$PFOS_B <- as.factor(dfBinFitSp7$PFOS_B)
# Reduce number of categorical vars (less than 50 allowed)
dfLinSp7 <- dfspat[,c('PFOS_L','M2_P1','M2_H4_Buf01','M2_S1_Buf03','M5_V024',
                      'M5_V043')]
formSp7 <- PFOS_L ~ M2_P1 + M2_H4_Buf01 + M2_S1_Buf03 + M5_V024 + factor(M5_V043)
modelSp7 <- lm(formula = formSp7, data = dfLinSp7) # fit linear model
modelSp7Sum <-summary(modelSp7) # sumarize model
modelSp7SumAtr <- attributes(modelSp7Sum$coefficients)
# We keep only significant categories from linear model
Cat2KeepSp7 <- c(22, 23,55, 57,64,80,87,105,128,152,165,179,188)
for(i in 1:length(dfBinFitSp7$M5_V043)){
  if(!(dfBinFitSp7$M5_V043[i] %in% Cat2KeepSp7)){
    dfBinFitSp7$M5_V043[i]<-0}}
dfBinFitSp7$M5_V043 <- droplevels(dfBinFitSp7$M5_V043)
# 1.7.2 Set Tuning Specifications ..............................
rfTuneSpecsSp7 <- rand_forest(mode = "classification",
                              engine = "randomForest", 
                              mtry = tune(),
                              trees = tune(),
                              min_n = tune())
# 1.7.3 Set Initial Split ......................................
set.seed(123456)
DataSplitSp7 <- initial_split(dfBinFitSp7, prop = 3/4, strata = PFOS_B)
DataTrainSp7 <- training(DataSplitSp7)
DataTestSp7  <- testing(DataSplitSp7)
# 1.7.4 Set folds for cross validation ........................
foldsSp7 <- vfold_cv(data = DataTrainSp7,
                     v = 10, repeats = 1,
                     strata = "PFOS_B",
                     pool = 0.1)
# 1.7.5 Set Tuning grid .......................................
tune_mtry <- range_set(mtry(), c(2, 10))
tune_min_n <- range_set(min_n(), c(8, 32))
tune_trees <- range_set(trees(), c(300, 1300))
rf_gridSp7 <- grid_regular(
  finalize(mtry(), subset(DataTrainSp7, select = M2_P1:M5_V043)), 
  trees(), 
  min_n(), 
  levels = 5)
# 1.7.6 Tune model on grid ....................................
tunerf_wfSp7 <- workflow() %>% add_model(rfTuneSpecsSp7) %>% 
  add_formula(PFOS_B ~ M2_P1 + M2_H4_Buf01 + M2_S1_Buf03 + M5_V024 + M5_V043)
tunerf_resSp7 <- tunerf_wfSp7 %>% tune_grid(resamples = foldsSp7, grid = rf_gridSp7)
# 1.7.7 Get results ...........................................
tunerf_resSp7 %>% show_best(metric = "accuracy")
tunerf_resSp7 %>% show_best(metric = "roc_auc")
collect_metrics(tunerf_resSp7) -> metricsSp7
# 1.7.8 Select best models ....................................
best_rfSp71 <- tunerf_resSp7 %>% select_best(metric = "accuracy")
best_rfSp72 <- tunerf_resSp7 %>% select_best(metric = "roc_auc")
# 1.7.9 Finalize workflowS ....................................
final_wfSp71 <- tunerf_wfSp7 %>% finalize_workflow(best_rfSp71)
final_wfSp72 <- tunerf_wfSp7 %>% finalize_workflow(best_rfSp72)
# 1.7.10 Fit final models on full training data and evaluate on test data ......
final_fitSp71 <- final_wfSp71 %>% last_fit(DataSplitSp7) 
final_fitSp72 <- final_wfSp72 %>% last_fit(DataSplitSp7)
# 1.7.11 Collect metrics ......................................
final_fitSp71 %>% collect_metrics()
final_fitSp72 %>% collect_metrics()
# 1.6.10 Collect models to predict ............................
final_rfSp71 <- extract_workflow(final_fitSp71)
final_rfSp72 <- extract_workflow(final_fitSp72)
# 1.8 Species 8: Total PFAS ----------------------------------------------------
# 1.8.1 Prepare Data ...........................................
dfBinFitSp8 <- dfspat[,c('Total_PFAS_B','M2_P1','M2_H3_Buf01','M2_H3_Buf03',
                         'M5_V009','M5_V029','M5_V034','M5_V038','M5_V043')]
dfBinFitSp8$Total_PFAS_B <- as.factor(dfBinFitSp8$Total_PFAS_B)
# Reduce number of categorical vars (less than 50 allowed)
dfLinSp8 <- dfspat[,c('Total_PFAS_L','M2_P1','M2_H3_Buf01','M2_H3_Buf03',
                      'M5_V009','M5_V029','M5_V034','M5_V038','M5_V043')]
formSp8 <- Total_PFAS_L ~ M2_P1 + M2_H3_Buf01 + M2_H3_Buf03 + M5_V009 +
  factor(M5_V029) + M5_V034 + M5_V038 + factor(M5_V043)
modelSp8 <- lm(formula = formSp8, data = dfLinSp8) # fit linear model
modelSp8Sum <-summary(modelSp8) # sumarize model
modelSp8SumAtr <- attributes(modelSp8Sum$coefficients)
# We keep only significant categories from linear model
Cat2KeepSp8 <- c(1,2,3,8,12,13,21,24,25,36,37,47,52,53,55,57,59,64,69,71,77,81,
                 82,83,85,93,94,100,107,117,118,120,133,136,141,150,151,157,
                 162,163,165,166,167,171,173,181,186,195)
for(i in 1:length(dfBinFitSp8$M5_V043)){
  if(!(dfBinFitSp8$M5_V043[i] %in% Cat2KeepSp8)){
    dfBinFitSp8$M5_V043[i]<-0}}
dfBinFitSp8$M5_V043 <- droplevels(dfBinFitSp8$M5_V043)
# 1.8.2 Set Tuning Specifications ..............................
rfTuneSpecsSp8 <- rand_forest(mode = "classification",
                              engine = "randomForest", 
                              mtry = tune(),
                              trees = tune(),
                              min_n = tune())
# 1.8.3 Set Initial Split ......................................
set.seed(123456)
DataSplitSp8 <- initial_split(dfBinFitSp8, prop = 3/4, strata = Total_PFAS_B)
DataTrainSp8 <- training(DataSplitSp8)
DataTestSp8  <- testing(DataSplitSp8)
# 1.8.4 Set folds for cross validation ........................
foldsSp8 <- vfold_cv(data = DataTrainSp8,
                     v = 10, repeats = 1,
                     strata = "Total_PFAS_B",
                     pool = 0.1)
# 1.8.5 Set Tuning grid .......................................
tune_mtry <- range_set(mtry(), c(2, 10))
tune_min_n <- range_set(min_n(), c(8, 32))
tune_trees <- range_set(trees(), c(300, 1300))
rf_gridSp8 <- grid_regular(
  finalize(mtry(), subset(DataTrainSp8, select = M2_P1:M5_V043)), 
  trees(), 
  min_n(), 
  levels = 5)
# 1.8.6 Tune model on grid ....................................
tunerf_wfSp8 <- workflow() %>% add_model(rfTuneSpecsSp8) %>% 
  add_formula(Total_PFAS_B ~ M2_P1 + M2_H3_Buf01 + M2_H3_Buf03 + M5_V009 +
                M5_V029 + M5_V034 + M5_V038 + M5_V043)
tunerf_resSp8 <- tunerf_wfSp8 %>% tune_grid(resamples = foldsSp8, grid = rf_gridSp8)
# 1.8.7 Get results ...........................................
tunerf_resSp8 %>% show_best(metric = "accuracy")
tunerf_resSp8 %>% show_best(metric = "roc_auc")
collect_metrics(tunerf_resSp8) -> metricsSp8
# 1.8.8 Select best models ....................................
best_rfSp81 <- tunerf_resSp8 %>% select_best(metric = "accuracy")
best_rfSp82 <- tunerf_resSp8 %>% select_best(metric = "roc_auc")
# 1.8.9 Finalize workflowS ....................................
final_wfSp81 <- tunerf_wfSp8 %>% finalize_workflow(best_rfSp81)
final_wfSp82 <- tunerf_wfSp8 %>% finalize_workflow(best_rfSp82)
# 1.8.10 Fit final models on full training data and evaluate on test data ......
final_fitSp81 <- final_wfSp81 %>% last_fit(DataSplitSp8) 
final_fitSp82 <- final_wfSp82 %>% last_fit(DataSplitSp8)
# 1.8.11 Collect metrics.......................................
final_fitSp81 %>% collect_metrics()
final_fitSp82 %>% collect_metrics()
# 1.8.10 Collect models to predict ............................
final_rfSp81 <- extract_workflow(final_fitSp81)
final_rfSp82 <- extract_workflow(final_fitSp82)
################ END CODE ######################################################