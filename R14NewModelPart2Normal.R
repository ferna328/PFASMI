######################## R14NewModelPart2Normal.R ##############################
# Research Paper: Large-scale Assessment of PFAS Compounds in Drinking Water   #
# Sources using Machine Learning                                               #
# https://doi.org/10.1016/j.watres.2023.120307                                 #
# Nicolas Fernandez, A. Pouyan Nejadhashemi, Christian Loveall                 #
# Department of Biosystems and Agricultural Engineering                        #
# Michigan State University, East Lansing, MI, United States                   #
# Code written and curated by Nicolas Fernandez (OrcID 0000-0001-7979-2941)    #
######################## Description ###########################################
# According to the Paper, this code is used to implement the New Model,        #
# specifically the second part to represent normally distributed data          #
# (i.e.loglog transform of detected PFAS concentrations)                       #
################################################################################
###### 0. IMPORT AND  PREPARE DATA #############################################
# 0.0 Set Working Directory ----------------------------------------------------
wd = 'E:/PFASMIproject'
setwd(wd)
# 0.1 Read Input Information ---------------------------------------------------
# 0.1.1 Model 1 (Hu et al 2016) ........................
dirDep = 'E:/XXX/YYY/ZZZ/DataExp1V2Dep.csv'
dirExp = 'E:/XXX/YYY/ZZZ/DataExp1V2Exp.csv'
Dep <- read.csv(file = dirDep) # Dependent Variables
Exp <- read.csv(file = dirExp) # Explanatory Variables
# 0.2.2 Model 2 (Hu et al 2021) ........................
DirInfo <- 'E:/XXX/YYY/ZZZ/PFASMIGWdatabaseV2.xlsx'
library('readxl')
Data2 <- read_excel(DirInfo, sheet = 'Data')
Dict2 <- read_excel(DirInfo, sheet = 'Dictionary')
# 0.2.3 Model 3 (George & Dixit 2021) ..................
DirInfo <- 'E:/XXX/YYY/ZZZ/MasterFile.xlsx'
Data3 <- read_excel(DirInfo)
# 0.2.4 Model 4 (McMahon et al 2022) ...................
DirInfo <- 'E:/XXX/YYY/ZZZ/PFASMIGWdatabaseV3.xlsx'
Data4 <- read_excel(DirInfo, sheet = 'Data')
Dict4 <- read_excel(DirInfo, sheet = 'Dictionary')
rm(dirDep, dirExp, dirin, DirInfo)
# 0.3 Organize Input Information -----------------------------------------------
# 0.3.-1 Coordinates............................
lonX <- Data3$X_coord
latY <- Data3$Y_Coord 
df <- data.frame(lonX, latY)
rm(lonX, latY)
# 0.3.0 Dependent Variables............................
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
# 0.3.6 Delete repeated positions ........................
LatLong <- data.frame(X = df$lonX, Y = df$latY)
LatLongUnique <- unique(LatLong)
RowsLatLong <- rownames(LatLong)
RowsLatLongUnique <- rownames(LatLongUnique)
Rows2del <- setdiff(RowsLatLong, RowsLatLongUnique)
dfspat <- subset(df, !(row.names(df) %in% Rows2del))
# 1. Fit Normal Data ###########################################################
# 1.1 Filter detected values and relevant vars ---------------------------------
library(dplyr)
dfspat %>% select(lonX,latY,PFBS_B,
                  PFBS,
                  M1_P3_Buf01,
                  M2_H4_Buf01,
                  M3_A3,
                  M4_I43,
                  M5_V012,
                  M5_V043) %>% filter(PFBS_B==1) %>% select(-PFBS_B) -> FiltSp1
dfspat %>% select(lonX,latY,PFHpA_B,
                  PFHpA,
                  M2_P1,
                  M3_Q2,
                  M5_V009,
                  M5_V019,
                  M5_V020,
                  M5_V034,
                  M5_V043) %>% filter(PFHpA_B==1)%>% select(-PFHpA_B) -> FiltSp2
dfspat %>% select(lonX,latY,PFHxA_B,
                  PFHxA,
                  M2_P1,
                  M3_Q2,
                  M4_S2,
                  M5_V009,
                  M5_V029,
                  M5_V034,
                  M5_V043) %>% filter(PFHxA_B==1)%>% select(-PFHxA_B) -> FiltSp3
dfspat %>% select(lonX,latY,PFHxS_B,
                  PFHxS,
                  M2_P1,
                  M3_Q1,
                  M5_V029,
                  M5_V043) %>% filter(PFHxS_B==1)%>% select(-PFHxS_B) -> FiltSp4
dfspat %>% select(lonX,latY,PFOA_B,
                  PFOA,
                  M2_P1,
                  M2_G1_Buf03,
                  M2_H3_Buf01,
                  M2_H3_Buf03,
                  M4_I43,
                  M5_V009,
                  M5_V024,
                  M5_V029,
                  M5_V034,
                  M5_V043) %>% filter(PFOA_B==1)%>% select(-PFOA_B) -> FiltSp5
dfspat %>% select(lonX,latY,PFOA_PFOS_B,
                  PFOA_PFOS,
                  M2_P1,
                  M2_G1_Buf03,
                  M5_V009,
                  M5_V029,
                  M5_V034,
                  M5_V043) %>% filter(PFOA_PFOS_B==1)%>% select(-PFOA_PFOS_B) -> FiltSp6
dfspat %>% select(lonX,latY,PFOS_B,
                  PFOS,
                  M2_P1,
                  M2_H4_Buf01,
                  M2_S1_Buf03,
                  M5_V024,
                  M5_V043) %>% filter(PFOS_B==1)%>% select(-PFOS_B) -> FiltSp7
dfspat %>% select(lonX,latY,Total_PFAS_B,
                  Total_PFAS,
                  M2_P1,
                  M2_H3_Buf01,
                  M2_H3_Buf03,
                  M5_V009,
                  M5_V029,
                  M5_V034,
                  M5_V038,
                  M5_V043) %>% filter(Total_PFAS_B==1)%>% select(-Total_PFAS_B) -> FiltSp8
### 1.2 Manage NAN and vars ----------------------------------------------------
library(GGally)
### 1.2.1 Species 1 PFBS .........................................
# PFBS~factor(M1_P3_Buf01)+M2_H4_Buf01+factor(M3_A3)+M4_I43+M5_V012+factor(M5_V043)
FiltSp1 <- na.omit(FiltSp1)
FiltSp1$PFBS <- log(log(FiltSp1$PFBS))
FiltSp1$M1_P3_Buf01 <- as.factor(FiltSp1$M1_P3_Buf01)
FiltSp1$M3_A3 <- as.factor(FiltSp1$M3_A3)
FiltSp1$M5_V043 <- as.factor(FiltSp1$M5_V043)
ggpairs(select(FiltSp1,-c(M5_V043,lonX,latY)))
### 1.2.2 Species 2 PFHpA .........................................
# PFHpA ~ M2_P1 + factor(M3_Q2) + M5_V009 + factor(M5_V019) + factor(M5_V020) + M5_V034 + factor(M5_V043)
FiltSp2 <- na.omit(FiltSp2)
FiltSp2$PFHpA <- log(log(FiltSp2$PFHpA))
FiltSp2$M3_Q2 <- as.factor(FiltSp2$M3_Q2)
FiltSp2$M5_V019 <- as.factor(FiltSp2$M5_V019)
FiltSp2$M5_V020 <- as.factor(FiltSp2$M5_V020)
FiltSp2$M5_V043 <- as.factor(FiltSp2$M5_V043)
ggpairs(select(FiltSp2,-c(M5_V043,lonX,latY)))
### 1.2.3 Species 3 PFHxA .........................................
#formSp3 <- PFHxA ~ M2_P1 + factor(M3_Q2) + M4_S2 + M5_V009 + factor(M5_V029) + 
#  M5_V034 + factor(M5_V043)
FiltSp3 <- na.omit(FiltSp3)
FiltSp3$PFHxA <- log(log(FiltSp3$PFHxA))
FiltSp3$M3_Q2 <- as.factor(FiltSp3$M3_Q2)
FiltSp3$M5_V029<- as.factor(FiltSp3$M5_V029)
FiltSp3$M5_V043<- as.factor(FiltSp3$M5_V043)
ggpairs(select(FiltSp3,-c(M5_V043,lonX,latY)))
### 1.2.4 Species 4 PFHxS .........................................
FiltSp4 <- na.omit(FiltSp4)
FiltSp4$PFHxS <- log(log(FiltSp4$PFHxS))
FiltSp4$M3_Q1 <- as.factor(FiltSp4$M3_Q1)
ggpairs(select(FiltSp4,-c(M5_V043,lonX,latY)))
### 1.2.5 Species 5 PFOA ..........................................
FiltSp5 <- na.omit(FiltSp5)
FiltSp5$PFOA <- log(log(FiltSp5$PFOA))
FiltSp5$M2_G1_Buf03 <- as.factor(FiltSp5$M2_G1_Buf03)
ggpairs(select(FiltSp5,-c(M5_V043,lonX,latY)))
### 1.2.6 Species 6 PFOA_PFOS .....................................
FiltSp6 <- na.omit(FiltSp6)
FiltSp6$PFOA_PFOS <- log(log(FiltSp6$PFOA_PFOS))
FiltSp6$M2_G1_Buf03 <- as.factor(FiltSp6$M2_G1_Buf03)
ggpairs(select(FiltSp6,-c(M5_V043,lonX,latY)))
### 1.2.7 Species 7 PFOS ..........................................
FiltSp7 <- na.omit(FiltSp7)
FiltSp7$PFOS <- log(log(FiltSp7$PFOS))
ggpairs(select(FiltSp7,-c(M5_V043,lonX,latY)))
### 1.2.8 Species 8 Total PFAS ....................................
FiltSp8 <- na.omit(FiltSp8)
FiltSp8$Total_PFAS <- log(log(FiltSp8$Total_PFAS))
ggpairs(select(FiltSp8,-c(M5_V043,lonX,latY)))
### 1.3 Fit Linear Models ------------------------------------------------------
### 1.3.1 Establish formulas ................................
formSp1 <- PFBS ~ factor(M1_P3_Buf01) + M2_H4_Buf01 + factor(M3_A3) + M4_I43 + 
  M5_V012 + factor(M5_V043)
formSp2 <- PFHpA ~ M2_P1 + factor(M3_Q2) + M5_V009 + factor(M5_V019) + 
  factor(M5_V020) + M5_V034 + factor(M5_V043)
formSp3 <- PFHxA ~ M2_P1 + factor(M3_Q2) + M4_S2 + M5_V009 + factor(M5_V029) + 
  M5_V034 + factor(M5_V043)
formSp4 <- PFHxS ~ M2_P1 + factor(M3_Q1) + factor(M5_V029) + factor(M5_V043)
formSp5 <- PFOA ~ M2_P1 + factor(M2_G1_Buf03) + M2_H3_Buf01 + M2_H3_Buf03 + 
  M4_I43 + M5_V009 + M5_V024 + factor(M5_V029) + M5_V034 + factor(M5_V043)
formSp6 <- PFOA_PFOS ~ M2_P1 + factor(M2_G1_Buf03) + M5_V009 + 
  factor(M5_V029) + M5_V034 + factor(M5_V043)
formSp7 <- PFOS ~ M2_P1 + M2_H4_Buf01 + M2_S1_Buf03 + M5_V024 + factor(M5_V043)
formSp8 <- Total_PFAS ~ M2_P1 + M2_H3_Buf01 + M2_H3_Buf03 + M5_V009 +
  factor(M5_V029) + M5_V034 + M5_V038 + factor(M5_V043)
### 1.3.2 Initial Fit .......................................
lmodelSp1 <- lm(formula = formSp1, data = FiltSp1) 
lmodelSp2 <- lm(formula = formSp2, data = FiltSp2)
lmodelSp3 <- lm(formula = formSp3, data = FiltSp3)
lmodelSp4 <- lm(formula = formSp4, data = FiltSp4)
lmodelSp5 <- lm(formula = formSp5, data = FiltSp5)
lmodelSp6 <- lm(formula = formSp6, data = FiltSp6)
lmodelSp7 <- lm(formula = formSp7, data = FiltSp7)
lmodelSp8 <- lm(formula = formSp8, data = FiltSp8)
lmodelSumSp1 <-summary(lmodelSp1)
lmodelSumSp2 <-summary(lmodelSp2)
lmodelSumSp3 <-summary(lmodelSp3)
lmodelSumSp4 <-summary(lmodelSp4)
lmodelSumSp5 <-summary(lmodelSp5)
lmodelSumSp6 <-summary(lmodelSp6)
lmodelSumSp7 <-summary(lmodelSp7)
lmodelSumSp8 <-summary(lmodelSp8)
lmodelSumAtrSp1 <- attributes(lmodelSumSp1$coefficients)
lmodelSumAtrSp2 <- attributes(lmodelSumSp2$coefficients)
lmodelSumAtrSp3 <- attributes(lmodelSumSp3$coefficients)
lmodelSumAtrSp4 <- attributes(lmodelSumSp4$coefficients)
lmodelSumAtrSp5 <- attributes(lmodelSumSp5$coefficients)
lmodelSumAtrSp6 <- attributes(lmodelSumSp6$coefficients)
lmodelSumAtrSp7 <- attributes(lmodelSumSp7$coefficients)
lmodelSumAtrSp8 <- attributes(lmodelSumSp8$coefficients)
### 1.3 Check Spatial Autocorrelation ------------------------------------------
### 1.3.1 Compute & see Voronoi polygons ...............................
library(deldir) # Library for computing Thyessen polygons
library(animalEKF)
GetTilesNShape <- function(FiltSpX){
  tesselation <- deldir(FiltSpX$lonX, FiltSpX$latY) # compute tesselation
  tiles <- tile.list(tesselation) # compute tiles
  shp <- tess2spat(tesselation, idvec=NULL)
  TilesNShape <- list(tiles,shp)
  return(TilesNShape)}
TilesNShapeSp1 <- GetTilesNShape(FiltSp1)
TilesNShapeSp2 <- GetTilesNShape(FiltSp2)
TilesNShapeSp3 <- GetTilesNShape(FiltSp3)
TilesNShapeSp4 <- GetTilesNShape(FiltSp4)
TilesNShapeSp5 <- GetTilesNShape(FiltSp5)
TilesNShapeSp6 <- GetTilesNShape(FiltSp6)
TilesNShapeSp7 <- GetTilesNShape(FiltSp7)
TilesNShapeSp8 <- GetTilesNShape(FiltSp8)
plot(TilesNShapeSp1[[1]], pch = 19) # Plot tiles
plot(TilesNShapeSp1[[2]]) #Plot shape
plot(TilesNShapeSp2[[1]], pch = 19) # Plot tiles
plot(TilesNShapeSp2[[2]]) #Plot shape
plot(TilesNShapeSp3[[1]], pch = 19) # Plot tiles
plot(TilesNShapeSp3[[2]]) #Plot shape
plot(TilesNShapeSp4[[1]], pch = 19) # Plot tiles
plot(TilesNShapeSp4[[2]]) #Plot shape
plot(TilesNShapeSp5[[1]], pch = 19) # Plot tiles
plot(TilesNShapeSp5[[2]]) #Plot shape
plot(TilesNShapeSp6[[1]], pch = 19) # Plot tiles
plot(TilesNShapeSp6[[2]]) #Plot shape
plot(TilesNShapeSp7[[1]], pch = 19) # Plot tiles
plot(TilesNShapeSp7[[2]]) #Plot shape
plot(TilesNShapeSp8[[1]], pch = 19) # Plot tiles
plot(TilesNShapeSp8[[2]]) #Plot shape
### 1.3.2 Run Moran's test ..............................................
library(spdep)
RunMoran <- function(shp,FiltSpX,lmodelSpX,nsim){
  W.nb <- poly2nb(pl = shp, row.names = rownames(FiltSpX))
  W.list <- nb2listw(W.nb, style = "B") #style can take values “W”, “B”, “C”, “U”, “minmax” and “S”
  MoranResults <- moran.mc(x = residuals(lmodelSpX), listw = W.list, nsim = nsim)
  results <- list(W.nb,W.list,MoranResults)
  return(results)}
MoranResultsSp1 <- RunMoran(TilesNShapeSp1[[2]],FiltSp1,lmodelSp1,10000)
MoranResultsSp2 <- RunMoran(TilesNShapeSp2[[2]],FiltSp2,lmodelSp2,10000)
MoranResultsSp3 <- RunMoran(TilesNShapeSp3[[2]],FiltSp3,lmodelSp3,10000)
MoranResultsSp4 <- RunMoran(TilesNShapeSp4[[2]],FiltSp4,lmodelSp4,10000)
MoranResultsSp5 <- RunMoran(TilesNShapeSp5[[2]],FiltSp5,lmodelSp5,10000)
MoranResultsSp6 <- RunMoran(TilesNShapeSp6[[2]],FiltSp6,lmodelSp6,10000)
MoranResultsSp7 <- RunMoran(TilesNShapeSp7[[2]],FiltSp7,lmodelSp7,10000)
MoranResultsSp8 <- RunMoran(TilesNShapeSp8[[2]],FiltSp8,lmodelSp8,10000)
# 2. Fit Conditional model (Bayesian approach) #################################
#see: https://cran.r-project.org/web/packages/CARBayes/vignettes/CARBayes.pdf
# 2.1 Bayesian Linear Models ---------------------------------------------------
# 2.1.1 Function 1: Run 3 markov chains ......................
library(CARBayes)
Run3MChains <- function(form,FiltSpX,burning,samples,progress){
  Chain1 <- S.glm(formula = form, 
                  formula.omega=NULL, # Offsets
                  family = "gaussian",
                  data=FiltSpX, 
                  trials=NULL, # if binomial or multinomial
                  burnin=burning, # MCMC samples discarded
                  n.sample=samples, # MCMC samples to generate
                  thin=1, # if <1 it thins, meaning that temporal autocorrelation of MCMC samples are reduced
                  prior.mean.beta=NULL, # priors for the mean of regression pars
                  prior.var.beta=NULL, # priors for the variances of regression pars
                  prior.nu2=NULL, # priors for shape and scale of inverse gamma nu 2
                  prior.mean.delta=NULL, # priors for multinomial delta mean
                  prior.var.delta=NULL, # priors for multinomial delta var
                  MALA=TRUE, # when true uses Metropolis adjusted, when false random walk
                  verbose=progress) # true if I want to see progress
  Chain2 <- S.glm(formula = form, 
                  formula.omega=NULL, # Offsets
                  family = "gaussian", # it can be also "binomial", "multinomial", "poisson" or "zip"
                  data=FiltSpX, 
                  trials=NULL, # if binomial or multinomial number of trials for each data point
                  burnin=burning, # MCMC samples discarded
                  n.sample=samples, # MCMC samples to generate
                  thin=1, # if <1 it thins, meaning that temporal autocorrelation of MCMC samples are reduced
                  prior.mean.beta=NULL, # priors for the mean of regression pars
                  prior.var.beta=NULL, # priors for the variances of regression pars
                  prior.nu2=NULL, # priors for shape and scale of inverse gamma nu 2
                  prior.mean.delta=NULL, # priors for multinomial delta mean
                  prior.var.delta=NULL, # priors for multinomial delta var
                  MALA=TRUE, # when true uses Metropolis adjusted, when false random walk
                  verbose=progress) # true if I want to see progress
  Chain3 <- S.glm(formula = form, 
                  formula.omega=NULL, # Offsets
                  family = "gaussian", # it can be also "binomial", "multinomial", "poisson" or "zip"
                  data=FiltSpX, 
                  trials=NULL, # if binomial or multinomial number of trials for each data point
                  burnin=burning, # MCMC samples discarded
                  n.sample=samples, # MCMC samples to generate
                  thin=1, # if <1 it thins, meaning that temporal autocorrelation of MCMC samples are reduced
                  prior.mean.beta=NULL, # priors for the mean of regression pars
                  prior.var.beta=NULL, # priors for the variances of regression pars
                  prior.nu2=NULL, # priors for shape and scale of inverse gamma nu 2
                  prior.mean.delta=NULL, # priors for multinomial delta mean
                  prior.var.delta=NULL, # priors for multinomial delta var
                  MALA=TRUE, # when true uses Metropolis adjusted, when false random walk
                  verbose=progress) # true if I want to see progress
  Chains <- list(Chain1, Chain2, Chain3)
  return(Chains)}
# 2.1.2 Function 2: Summarize and Infer ......................
library(coda)
SummNInfer <- function(ChainsSpX){
  # Summarize results --------------------------------
  print(ChainsSpX[[1]])
  summary(ChainsSpX[[1]])
  summary(ChainsSpX[[1]]$samples)
  # Evaluate if enough for inference -----------------
  beta.samples <- mcmc.list(ChainsSpX[[1]]$samples$beta, 
                            ChainsSpX[[2]]$samples$beta, 
                            ChainsSpX[[3]]$samples$beta)
  beta.samples <- mcmc.list(ChainsSpX[[1]]$samples$beta,
                            ChainsSpX[[2]]$samples$beta, 
                            ChainsSpX[[3]]$samples$beta)
  plot(beta.samples[ ,1:3])
  print(gelman.diag(beta.samples))
  # Make inference -----------------------------------
  beta.samples.matrix <- rbind(ChainsSpX[[1]]$samples$beta, 
                               ChainsSpX[[2]]$samples$beta,
                               ChainsSpX[[3]]$samples$beta)
  colnames(beta.samples.matrix) <- colnames(ChainsSpX[[1]]$X)
  round(t(rbind(apply(beta.samples.matrix, 2, mean),
                apply(beta.samples.matrix, 2, quantile, c(0.025, 0.975)))), 5)
}
# 2.1.3 Run Functions on Species ......................
### SPECIES 1 PFBS ~ factor(M1_P3_Buf01) + M2_H4_Buf01 + factor(M3_A3) + M4_I43 + 
# M5_V012 + factor(M5_V043)
ChainsSp1 <- Run3MChains(formSp1,FiltSp1,3000,10000,TRUE)
### SPECIES 2: PFHpA ~ M2_P1 + factor(M3_Q2) + M5_V009 + factor(M5_V019) + factor(M5_V020) + 
#M5_V034 + factor(M5_V043)
Cat2KeepSp2 <- c(3,18,22,24,57,111)
for(i in 1:length(FiltSp2$M5_V043)){
  if(!(FiltSp2$M5_V043[i] %in% Cat2KeepSp2)){
    FiltSp2$M5_V043[i]<-0}}
FiltSp2$M5_V043 <- droplevels(FiltSp2$M5_V043)
Cat2KeepSp2 <- c(41)
for(i in 1:length(FiltSp2$M5_V020)){
  if(!(FiltSp2$M5_V020[i] %in% Cat2KeepSp2)){
    FiltSp2$M5_V020[i]<-0}}
FiltSp2$M5_V020 <- droplevels(FiltSp2$M5_V020)
formSp2 <- PFHpA ~ M2_P1 + factor(M3_Q2) + M5_V009 + factor(M5_V020) + M5_V034 + factor(M5_V043)
Cat2KeepSp2 <- c(22,41,82)
for(i in 1:length(FiltSp2$M5_V019)){
  if(!(FiltSp2$M5_V019[i] %in% Cat2KeepSp2)){
    FiltSp2$M5_V019[i]<-11}}
FiltSp2$M5_V019 <- droplevels(FiltSp2$M5_V019)
ChainsSp2 <- Run3MChains(formSp2,FiltSp2,3000,10000,TRUE)
### SPECIES 3: PFHxA ~ M2_P1 + factor(M3_Q2) + M4_S2 + M5_V009 + factor(M5_V029) + 
#M5_V034 + factor(M5_V043)
Cat2DelSp3 <- c(173,192,196)
for(i in 1:length(FiltSp3$M5_V043)){
  if(FiltSp3$M5_V043[i] %in% Cat2DelSp3){
    FiltSp3$M5_V043[i]<-0}}
FiltSp3$M5_V043 <- droplevels(FiltSp3$M5_V043)
ChainsSp3 <- Run3MChains(formSp3,FiltSp3,3000,10000,TRUE)
### SPECIES 4: PFHxS ~ M2_P1 + factor(M3_Q1) + factor(M5_V029) + factor(M5_V043)
Cat2DelSp4 <- c(3,173,186,188,192,194,196)
for(i in 1:length(FiltSp4$M5_V043)){
  if(FiltSp4$M5_V043[i] %in% Cat2DelSp4){
    FiltSp4$M5_V043[i]<-0}}
FiltSp4$M5_V043 <- droplevels(FiltSp4$M5_V043)
ChainsSp4 <- Run3MChains(formSp4,FiltSp4,3000,10000,TRUE)
### SPECIES 5: PFOA ~ M2_P1 + factor(M2_G1_Buf03) + M2_H3_Buf01 + M2_H3_Buf03 + 
#M4_I43 + M5_V009 + M5_V024 + factor(M5_V029) + M5_V034 + factor(M5_V043)
formSp5 <- PFOA ~ M2_P1 + factor(M2_G1_Buf03) + M2_H3_Buf01 + M2_H3_Buf03 + M4_I43 + M5_V009 + factor(M5_V029) + M5_V034 + factor(M5_V043)
Cat2DelSp5 <- c(3,84,107,141,196)
for(i in 1:length(FiltSp5$M5_V043)){
  if(FiltSp5$M5_V043[i] %in% Cat2DelSp5){
    FiltSp5$M5_V043[i]<-0}}
FiltSp5$M5_V043 <- droplevels(FiltSp5$M5_V043)
ChainsSp5 <- Run3MChains(formSp5,FiltSp5,3000,10000,TRUE)
### SPECIES 6: PFOA_PFOS ~ M2_P1 + factor(M2_G1_Buf03) + M5_V009 + factor(M5_V029) + 
#M5_V034 + factor(M5_V043)
Cat2DelSp6 <- c(84,107,141,196)
for(i in 1:length(FiltSp6$M5_V043)){
  if(FiltSp6$M5_V043[i] %in% Cat2DelSp6){
    FiltSp6$M5_V043[i]<-0}}
FiltSp6$M5_V043 <- droplevels(FiltSp6$M5_V043)
ChainsSp6 <- Run3MChains(formSp6,FiltSp6,3000,10000,TRUE)
### SPECIES 7: PFOS ~ M2_P1 + M2_H4_Buf01 + M2_S1_Buf03 + M5_V024 + factor(M5_V043)
formSp7 <- PFOS ~ M2_P1 + M2_H4_Buf01 + M2_S1_Buf03 + factor(M5_V043)
ChainsSp7 <- Run3MChains(formSp7,FiltSp7,30,100,TRUE)
### SPECIES 8: Total_PFAS ~ M2_P1 + M2_H3_Buf01 + M2_H3_Buf03 + M5_V009 + factor(M5_V029) + 
#M5_V034 + M5_V038 + factor(M5_V043)
Cat2DelSp8 <- c(173,196)
for(i in 1:length(FiltSp8$M5_V043)){
  if(FiltSp8$M5_V043[i] %in% Cat2DelSp8){
    FiltSp8$M5_V043[i]<-0}}
FiltSp8$M5_V043 <- droplevels(FiltSp8$M5_V043)
ChainsSp8 <- Run3MChains(formSp8,FiltSp8,3000,10000,TRUE)
SummNInfer(ChainsSp1) # good for inference
SummNInfer(ChainsSp2) # good for inference
SummNInfer(ChainsSp3) # good for inference
SummNInfer(ChainsSp4) # good for inference
SummNInfer(ChainsSp5) # good for inference
SummNInfer(ChainsSp6) # good for inference
SummNInfer(ChainsSp7) # good for inference
SummNInfer(ChainsSp8) # good for inference
# 2.1.4 Extract fitness metrics ......................
ExtractMetrics <- function(ChainsSpX,FiltSpX){
  SpCor <-  cor(ChainsSpX[[1]]$fitted.values,
                FiltSpX[,3], method = "spearman")
  model <- lm(FiltSpX[,3] ~ ChainsSpX[[1]]$fitted.values)
  rsq <- summary(model)$r.squared
  metrics <- list(SpCor,rsq)
  return(metrics)}
ExtractMetrics(ChainsSp1,FiltSp1)
ExtractMetrics(ChainsSp2,FiltSp2)
ExtractMetrics(ChainsSp3,FiltSp3)
ExtractMetrics(ChainsSp4,FiltSp4)
ExtractMetrics(ChainsSp5,FiltSp5)
ExtractMetrics(ChainsSp6,FiltSp6)
ExtractMetrics(ChainsSp7,FiltSp7)
ExtractMetrics(ChainsSp8,FiltSp8)
# 2.2 Leroux and Besag CAR Models ----------------------------------------------
# 2.2.1 Function 1: Run 3 markov chains ......................
Run3MChainsLB <- function(MoranResultsSpX, formSpX, FiltSpX, burning, samples, rho, progress){
  W <- nb2mat(MoranResultsSpX[[1]], style="B")
  Chain1 <-S.CARleroux(formula = formSpX,
                       formula.omega=NULL, 
                       family = "gaussian", 
                       data = FiltSpX,
                       trials=NULL, 
                       W = W,
                       burnin = burning, 
                       n.sample = samples, 
                       thin=1, 
                       prior.mean.beta=NULL, 
                       prior.var.beta=NULL,
                       prior.nu2=NULL, 
                       prior.tau2=NULL, 
                       prior.mean.delta=NULL, 
                       prior.var.delta=NULL, 
                       rho=rho, #  Independent Leroux random effects rho=0, intrinsic CAR model Besag rho=1
                       MALA=TRUE, 
                       verbose=progress)
  Chain2 <-S.CARleroux(formula = formSpX,
                       formula.omega=NULL, 
                       family = "gaussian", 
                       data = FiltSpX,
                       trials=NULL, 
                       W = W,
                       burnin = burning, 
                       n.sample = samples, 
                       thin=1, 
                       prior.mean.beta=NULL, 
                       prior.var.beta=NULL,
                       prior.nu2=NULL, 
                       prior.tau2=NULL, 
                       prior.mean.delta=NULL, 
                       prior.var.delta=NULL, 
                       rho=rho, #  Independent Leroux random effects rho=0, intrinsic CAR model Besag rho=1
                       MALA=TRUE, 
                       verbose=progress)
  Chain3 <-S.CARleroux(formula = formSpX,
                       formula.omega=NULL, 
                       family = "gaussian", 
                       data = FiltSpX,
                       trials=NULL, 
                       W = W,
                       burnin = burning, 
                       n.sample = samples, 
                       thin=1, 
                       prior.mean.beta=NULL, 
                       prior.var.beta=NULL,
                       prior.nu2=NULL, 
                       prior.tau2=NULL, 
                       prior.mean.delta=NULL, 
                       prior.var.delta=NULL, 
                       rho=rho, #  Independent Leroux random effects rho=0, intrinsic CAR model Besag rho=1
                       MALA=TRUE, 
                       verbose=progress)
  Chains <- list(Chain1, Chain2, Chain3)
  return(Chains)}
# 2.1.3 Run Functions on Species (Leroux Model)......................
ChainsSp1L <- Run3MChainsLB(MoranResultsSp1, formSp1, FiltSp1, 3000, 10000, 0, TRUE)
ChainsSp2L <- Run3MChainsLB(MoranResultsSp2, formSp2, FiltSp2, 3000, 10000, 0, TRUE)
ChainsSp3L <- Run3MChainsLB(MoranResultsSp3, formSp3, FiltSp3, 3000, 10000, 0, TRUE)
ChainsSp4L <- Run3MChainsLB(MoranResultsSp4, formSp4, FiltSp4, 3000, 10000, 0, TRUE)
ChainsSp5L <- Run3MChainsLB(MoranResultsSp5, formSp5, FiltSp5, 3000, 10000, 0, TRUE)
ChainsSp6L <- Run3MChainsLB(MoranResultsSp6, formSp6, FiltSp6, 3000, 10000, 0, TRUE)
ChainsSp7L <- Run3MChainsLB(MoranResultsSp7, formSp7, FiltSp7, 3000, 10000, 0, TRUE)
ChainsSp8L <- Run3MChainsLB(MoranResultsSp8, formSp8, FiltSp8, 3000, 10000, 0, TRUE)
SummNInfer(ChainsSp1L)
SummNInfer(ChainsSp2L)
SummNInfer(ChainsSp3L)
SummNInfer(ChainsSp4L)
SummNInfer(ChainsSp5L)
SummNInfer(ChainsSp6L)
SummNInfer(ChainsSp7L)
SummNInfer(ChainsSp8L)
ExtractMetrics(ChainsSp1L,FiltSp1)
ExtractMetrics(ChainsSp2L,FiltSp2)
ExtractMetrics(ChainsSp3L,FiltSp3)
ExtractMetrics(ChainsSp4L,FiltSp4)
ExtractMetrics(ChainsSp5L,FiltSp5)
ExtractMetrics(ChainsSp6L,FiltSp6)
ExtractMetrics(ChainsSp7L,FiltSp7)
ExtractMetrics(ChainsSp8L,FiltSp8)
# 2.1.4 Run Functions on Species (Besag Model)......................
ChainsSp1B <- Run3MChainsLB(MoranResultsSp1, formSp1, FiltSp1, 3000, 10000, 1, TRUE)
ChainsSp2B <- Run3MChainsLB(MoranResultsSp2, formSp2, FiltSp2, 3000, 10000, 1, TRUE)
ChainsSp3B <- Run3MChainsLB(MoranResultsSp3, formSp3, FiltSp3, 3000, 10000, 1, TRUE)
ChainsSp4B <- Run3MChainsLB(MoranResultsSp4, formSp4, FiltSp4, 3000, 10000, 1, TRUE)
ChainsSp5B <- Run3MChainsLB(MoranResultsSp5, formSp5, FiltSp5, 3000, 10000, 1, TRUE)
ChainsSp6B <- Run3MChainsLB(MoranResultsSp6, formSp6, FiltSp6, 3000, 10000, 1, TRUE)
ChainsSp7B <- Run3MChainsLB(MoranResultsSp7, formSp7, FiltSp7, 3000, 10000, 1, TRUE)
ChainsSp8B <- Run3MChainsLB(MoranResultsSp8, formSp8, FiltSp8, 3000, 10000, 1, TRUE)
SummNInfer(ChainsSp1B)
SummNInfer(ChainsSp2B)
SummNInfer(ChainsSp3B)
SummNInfer(ChainsSp4B)
SummNInfer(ChainsSp5B)
SummNInfer(ChainsSp6B)
SummNInfer(ChainsSp7B)
SummNInfer(ChainsSp8B)
ExtractMetrics(ChainsSp1B,FiltSp1)
ExtractMetrics(ChainsSp2B,FiltSp2)
ExtractMetrics(ChainsSp3B,FiltSp3)
ExtractMetrics(ChainsSp4B,FiltSp4)
ExtractMetrics(ChainsSp5B,FiltSp5)
ExtractMetrics(ChainsSp6B,FiltSp6)
ExtractMetrics(ChainsSp7B,FiltSp7)
ExtractMetrics(ChainsSp8B,FiltSp8)
# 2.1.5 Run Functions on Species (Hybrid Model)......................
ChainsSp1H <- Run3MChainsLB(MoranResultsSp1, formSp1, FiltSp1, 3000, 10000, 0.5, TRUE)
ChainsSp2H <- Run3MChainsLB(MoranResultsSp2, formSp2, FiltSp2, 3000, 10000, 0.5, TRUE)
ChainsSp3H <- Run3MChainsLB(MoranResultsSp3, formSp3, FiltSp3, 3000, 10000, 0.5, TRUE)
ChainsSp4H <- Run3MChainsLB(MoranResultsSp4, formSp4, FiltSp4, 3000, 10000, 0.5, TRUE)
ChainsSp5H <- Run3MChainsLB(MoranResultsSp5, formSp5, FiltSp5, 3000, 10000, 0.5, TRUE)
ChainsSp6H <- Run3MChainsLB(MoranResultsSp6, formSp6, FiltSp6, 3000, 10000, 0.5, TRUE)
ChainsSp7H <- Run3MChainsLB(MoranResultsSp7, formSp7, FiltSp7, 3000, 10000, 0.5, TRUE)
ChainsSp8H <- Run3MChainsLB(MoranResultsSp8, formSp8, FiltSp8, 3000, 10000, 0.5, TRUE)
SummNInfer(ChainsSp1H)
SummNInfer(ChainsSp2H)
SummNInfer(ChainsSp3H)
SummNInfer(ChainsSp4H)
SummNInfer(ChainsSp5H)
SummNInfer(ChainsSp6H)
SummNInfer(ChainsSp7H)
SummNInfer(ChainsSp8H)
ExtractMetrics(ChainsSp1H,FiltSp1)
ExtractMetrics(ChainsSp2H,FiltSp2)
ExtractMetrics(ChainsSp3H,FiltSp3)
ExtractMetrics(ChainsSp4H,FiltSp4)
ExtractMetrics(ChainsSp5H,FiltSp5)
ExtractMetrics(ChainsSp6H,FiltSp6)
ExtractMetrics(ChainsSp7H,FiltSp7)
ExtractMetrics(ChainsSp8H,FiltSp8)
### 3. Fit & Cross Validate Marginal Model 1: linear Bayesian ##################
# 3.1 Set Marginal Formulas ----------------------------------------------------
MformSp1 <- PFBS ~ M2_H4_Buf01 + M4_I43 + M5_V012
MformSp2 <- PFHpA ~ M2_P1 + M5_V009 + M5_V034
MformSp3 <- PFHxA ~ M2_P1 + M4_S2 + M5_V009 + M5_V034
MformSp4 <- PFHxS ~ M2_P1
MformSp5 <- PFOA ~ M2_P1 + M2_H3_Buf01 + M2_H3_Buf03 + M4_I43 + M5_V009 + M5_V034
MformSp6 <- PFOA_PFOS ~ M2_P1 + M5_V009 + M5_V034
MformSp7 <- PFOS ~ M2_P1 + M2_H4_Buf01 + M2_S1_Buf03
MformSp8 <- Total_PFAS ~ M2_P1 + M2_H3_Buf01 + M2_H3_Buf03 + M5_V009 + M5_V034 + M5_V038
# 3.2 Create folds -------------------------------------------------------------
library(caret)
cv_foldSp1 <- createFolds(FiltSp1$PFBS, k = 10)
cv_foldSp2 <- createFolds(FiltSp2$PFHpA, k = 10)
cv_foldSp3 <- createFolds(FiltSp3$PFHxA, k = 10)
cv_foldSp4 <- createFolds(FiltSp4$PFHxS, k = 10)
cv_foldSp5 <- createFolds(FiltSp5$PFOA, k = 10)
cv_foldSp6 <- createFolds(FiltSp6$PFOA_PFOS, k = 10)
cv_foldSp7 <- createFolds(FiltSp7$PFOS, k = 10)
cv_foldSp8 <- createFolds(FiltSp8$Total_PFAS, k = 10)
# 3.3 Loop to fit linear models with caret folds -------------------------------
allfolds <- list(cv_foldSp1, cv_foldSp2, cv_foldSp3, cv_foldSp4, cv_foldSp5, 
                 cv_foldSp6, cv_foldSp7, cv_foldSp8)
alldata <- list(FiltSp1, FiltSp2, FiltSp3, FiltSp4, FiltSp5, FiltSp6, FiltSp7,
                FiltSp8)
allforms <- list(MformSp1, MformSp2, MformSp3, MformSp4, MformSp5, MformSp6, 
                 MformSp7, MformSp8)
allLmodels <- list(list(),list(),list(),list(),list(),list(),list(),list())
for(i in 1:length(allfolds)){
  lmodel <- lapply(allfolds[[i]], function(fold) {
    # Get the training and validation data for this fold
    train_data <- alldata[[i]][-fold, ]
    validation_data <- alldata[[i]][fold, ]
    # Fit linear model
    lmodel <- lm(formula = allforms[[i]], data = train_data)
    return(lmodel)
    })
  allLmodels[[i]] <- append(allLmodels[[i]],lmodel)}
# 3.4 Train Models for all species and folds -----------------------------------
Run1MChain <- function(form,FiltSpX,burning,samples,progress){
  Chain <- S.glm(formula = form, 
                  formula.omega=NULL, # Offsets
                  family = "gaussian",
                  data=FiltSpX, 
                  trials=NULL, # if binomial or multinomial
                  burnin=burning, # MCMC samples discarded
                  n.sample=samples, # MCMC samples to generate
                  thin=1, # if <1 it thins, meaning that temporal autocorrelation of MCMC samples are reduced
                  prior.mean.beta=NULL, # priors for the mean of regression pars
                  prior.var.beta=NULL, # priors for the variances of regression pars
                  prior.nu2=NULL, # priors for shape and scale of inverse gamma nu 2
                  prior.mean.delta=NULL, # priors for multinomial delta mean
                  prior.var.delta=NULL, # priors for multinomial delta var
                  MALA=TRUE, # when true uses Metropolis adjusted, when false random walk
                  verbose=progress) # true if I want to see progress
  return(Chain)}
allCARLmodels <- list(list(),list(),list(),list(),list(),list(),list(),list())
for(i in 1:length(allfolds)){
  print(i)
  lmodelCAR <- lapply(allfolds[[i]], function(fold) {
    # Get the training data for this fold
    train_data <- alldata[[i]][-fold, ]
    # Fit CAR model
    CARmodel <- Run1MChain(allforms[[i]],train_data,5000,15000,TRUE)
    return(CARmodel)})
  allCARLmodels[[i]] <- append(allCARLmodels[[i]],lmodelCAR)}
# 3.5 Get Training & Testing Data ----------------------------------------------
alltrainCAR <- list(list(),list(),list(),list(),list(),list(),list(),list())
for(i in 1:length(allfolds)){
  print(i)
  train <- lapply(allfolds[[i]], function(fold) {
    # Get the training data for this fold
    test_data <- alldata[[i]][-fold, ]
    return(test_data)})
  alltrainCAR[[i]] <- append(alltrainCAR[[i]],train)}
alltestsCAR <- list(list(),list(),list(),list(),list(),list(),list(),list())
for(i in 1:length(allfolds)){
  print(i)
  tests <- lapply(allfolds[[i]], function(fold) {
    # Get the testing data for this fold
    test_data <- alldata[[i]][fold, ]
    return(test_data)})
  alltestsCAR[[i]] <- append(alltestsCAR[[i]],tests)}
# 3.6 Test models for all species and folds ------------------------------------
allSpCor <- list(list(),list(),list(),list(),list(),list(),list(),list())
allrsq <- list(list(),list(),list(),list(),list(),list(),list(),list())
for(i in 1:length(alltestsCAR)){
  print(i)
  for(j in 1:length(alltestsCAR[[i]])){
    print(j)
    # Fit linear model
    lmodel <- lm(formula = allforms[[i]], data = alltrainCAR[[i]][[j]])
    # Enter coefficients of CAR model into linear model
    lmodel$coefficients <- coef(allCARLmodels[[i]][[j]])
    # Make predictions using the validation data
    predicted <- predict(lmodel, newdata = alltestsCAR[[i]][[j]])
    # Compute metrics
    SpCor <-  cor(predicted,
                  alltestsCAR[[i]][[j]][[3]], method = "spearman")
    model <- lm(alltestsCAR[[i]][[j]][[3]] ~ predicted)
    rsq <- summary(model)$r.squared
    print(SpCor)
    allSpCor[[i]][[j]] <- SpCor
    print(rsq)
    allrsq[[i]][[j]] <- rsq
    } 
  }
# 3.7 Extract metrics ----------------------------------------------------------
# Best R2
max(unlist(allrsq[[1]]))
max(unlist(allrsq[[2]]))
max(unlist(allrsq[[3]]))
max(unlist(allrsq[[4]]))
max(unlist(allrsq[[5]]))
max(unlist(allrsq[[6]]))
max(unlist(allrsq[[7]]))
max(unlist(allrsq[[8]]))
# Corresponding spearman
allSpCor[[1]][[which.max(unlist(allrsq[[1]]))]]
allSpCor[[2]][[which.max(unlist(allrsq[[2]]))]]
allSpCor[[3]][[which.max(unlist(allrsq[[3]]))]]
allSpCor[[4]][[which.max(unlist(allrsq[[4]]))]]
allSpCor[[5]][[which.max(unlist(allrsq[[5]]))]]
allSpCor[[6]][[which.max(unlist(allrsq[[6]]))]]
allSpCor[[7]][[which.max(unlist(allrsq[[7]]))]]
allSpCor[[8]][[which.max(unlist(allrsq[[8]]))]]
# 3.8 5 fold for species with less than 50 vars --------------------------------
# CREATE FOLDS
cv5_foldSp2 <- createFolds(FiltSp2$PFHpA, k = 5)
cv5_foldSp7 <- createFolds(FiltSp7$PFOS, k = 5)
# LOOP TO FIT LINEAR MODELS
allfolds5 <- list(cv5_foldSp2, cv5_foldSp7)
alldata5 <- list(FiltSp2,FiltSp7)
allforms5 <- list(MformSp2, MformSp7)
allLmodels5 <- list(list(),list())
for(i in 1:length(allfolds5)){
  lmodel <- lapply(allfolds5[[i]], function(fold) {
    # Get the training and validation data for this fold
    train_data <- alldata5[[i]][-fold, ]
    validation_data <- alldata5[[i]][fold, ]
    # Fit linear model
    lmodel <- lm(formula = allforms5[[i]], data = train_data)
    return(lmodel)
  })
  allLmodels5[[i]] <- append(allLmodels5[[i]],lmodel)}
# TRAIN MODELS FOR SPECIES AND FOLDS
allCARLmodels5 <- list(list(),list())
for(i in 1:length(allfolds5)){
  print(i)
  lmodelCAR <- lapply(allfolds5[[i]], function(fold) {
    # Get the training data for this fold
    train_data <- alldata5[[i]][-fold, ]
    # Fit CAR model
    CARmodel <- Run1MChain(allforms5[[i]],train_data,5000,15000,TRUE)
    return(CARmodel)})
  allCARLmodels5[[i]] <- append(allCARLmodels5[[i]],lmodelCAR)}
# GET TRAINING AND TESTING DATA
alltrainCAR5 <- list(list(),list())
for(i in 1:length(allfolds5)){
  print(i)
  train <- lapply(allfolds5[[i]], function(fold) {
    # Get the training data for this fold
    test_data <- alldata5[[i]][-fold, ]
    return(test_data)})
  alltrainCAR5[[i]] <- append(alltrainCAR5[[i]],train)}
alltestsCAR5 <- list(list(),list())
for(i in 1:length(allfolds5)){
  print(i)
  tests <- lapply(allfolds5[[i]], function(fold) {
    # Get the testing data for this fold
    test_data <- alldata5[[i]][fold, ]
    return(test_data)})
  alltestsCAR5[[i]] <- append(alltestsCAR5[[i]],tests)}
# TEST MODELS FOR SPECIES AND FOLDS
allSpCor5 <- list(list(),list())
allrsq5 <- list(list(),list())
for(i in 1:length(alltestsCAR5)){
  print(i)
  for(j in 1:length(alltestsCAR5[[i]])){
    print(j)
    # Fit linear model
    lmodel <- lm(formula = allforms5[[i]], data = alltrainCAR5[[i]][[j]])
    # Enter coefficients of CAR model into linear model
    lmodel$coefficients <- coef(allCARLmodels5[[i]][[j]])
    # Make predictions using the validation data
    predicted <- predict(lmodel, newdata = alltestsCAR5[[i]][[j]])
    # Compute metrics
    SpCor <-  cor(predicted,
                  alltestsCAR5[[i]][[j]][[3]], method = "spearman")
    model <- lm(alltestsCAR5[[i]][[j]][[3]] ~ predicted)
    rsq <- summary(model)$r.squared
    print(SpCor)
    allSpCor5[[i]][[j]] <- SpCor
    print(rsq)
    allrsq5[[i]][[j]] <- rsq
  } 
}
# EXTRACT METRICS
# Best R2
max(unlist(allrsq5[[1]]))
max(unlist(allrsq5[[2]]))
# Corresponding spearman
allSpCor5[[1]][[which.max(unlist(allrsq5[[1]]))]]
allSpCor5[[2]][[which.max(unlist(allrsq5[[2]]))]]
### 4. Fit & Cross Validate Marginal Model 2: CAR rho = 0;1;0.5 ################
allfolds <- list(cv_foldSp1, cv_foldSp2, cv_foldSp3, cv_foldSp4, cv_foldSp5, 
                 cv_foldSp6, cv_foldSp7, cv_foldSp8)
alldata <- list(FiltSp1, FiltSp2, FiltSp3, FiltSp4, FiltSp5, FiltSp6, FiltSp7,
                FiltSp8)
allforms <- list(MformSp1, MformSp2, MformSp3, MformSp4, MformSp5, MformSp6, 
                 MformSp7, MformSp8)
# 4.1 Compute Shapefiles for all folds -----------------------------------------
allshapes <- list(list(),list(),list(),list(),list(),list(),list(),list())
for(i in 1:length(allfolds)){
  print(i)
  shape <- lapply(allfolds[[i]], function(fold) {
    # Get the train data for this fold
    train_data <- alldata[[i]][-fold, ]
    # Get tiles and shape for train data
    TilesNShape <- GetTilesNShape(train_data)
    ## Get Neighbor List
    W.nb <- poly2nb(pl = TilesNShape[[2]], row.names = rownames(train_data))
    W.list <- nb2listw(W.nb, style = "B") #style can take values “W”, “B”, “C”, “U”, “minmax” and “S”
    #MoranResults <- RunMoran(TilesNShape,train_data,allLmodels[[1]][[1]],10000)
    return(W.list)})
  allshapes[[i]] <- append(allshapes[[i]],shape)}
# 4.2 Function to fit CAR models -----------------------------------------------
Run1MChainLB <- function(Shape, formSpX, FiltSpX, burning, samples, rho, progress){
  W <- nb2mat(Shape, style="B")
  Chain <-S.CARleroux(formula = formSpX,
                      formula.omega=NULL, 
                      family = "gaussian", 
                      data = FiltSpX,
                      trials=NULL, 
                      W = W,
                      burnin = burning, 
                      n.sample = samples, 
                      thin=1, 
                      prior.mean.beta=NULL, 
                      prior.var.beta=NULL,
                      prior.nu2=NULL, 
                      prior.tau2=NULL, 
                      prior.mean.delta=NULL, 
                      prior.var.delta=NULL, 
                      rho=rho, #  Independent Leroux random effects rho=0, intrinsic CAR model Besag rho=1
                      MALA=TRUE, 
                      verbose=progress)
  return(Chain)}
# 4.3 Fit CAR models with function ---------------------------------------------
allmodelsCARp0 <- list(list(),list(),list(),list(),list(),list(),list(),list())
for(i in 1:length(allmodelsCARp0)){
  for(j in 1:10){
    train_data <- alldata[[i]][-allfolds[[i]][[j]], ]
    CARmodel <- Run1MChainLB(allshapes[[i]][[j]][[2]], allforms[[i]],train_data,5000,15000,0,TRUE)
    allmodelsCARp0[[i]] <- append(allmodelsCARp0[[i]],list(CARmodel))
    }
}
allmodelsCARp1 <- list(list(),list(),list(),list(),list(),list(),list(),list())
for(i in 1:length(allmodelsCARp1)){
  for(j in 1:10){
    train_data <- alldata[[i]][-allfolds[[i]][[j]], ]
    CARmodel <- Run1MChainLB(allshapes[[i]][[j]][[2]], allforms[[i]],train_data,5000,15000,1,TRUE)
    allmodelsCARp1[[i]] <- append(allmodelsCARp1[[i]],list(CARmodel))
  }
}
allmodelsCARp05 <- list(list(),list(),list(),list(),list(),list(),list(),list())
for(i in 1:length(allmodelsCARp05)){
  for(j in 1:10){
    train_data <- alldata[[i]][-allfolds[[i]][[j]], ]
    CARmodel <- Run1MChainLB(allshapes[[i]][[j]][[2]], allforms[[i]],train_data,5000,15000,0.5,TRUE)
    allmodelsCARp05[[i]] <- append(allmodelsCARp05[[i]],list(CARmodel))
  }
}
# 4.4 Test CAR models rho 0 ----------------------------------------------------
# TEST MODELS FOR SPECIES AND FOLDS
alltestsCAR <- list(list(),list(),list(),list(),list(),list(),list(),list())
for(i in 1:length(allfolds)){
  print(i)
  tests <- lapply(allfolds[[i]], function(fold) {
    # Get the testing data for this fold
    test_data <- alldata[[i]][fold, ]
    return(test_data)})
  alltestsCAR[[i]] <- append(alltestsCAR[[i]],tests)}
allSpCorCARp0 <- list(list(),list(),list(),list(),list(),list(),list(),list())
allrsqCARp0 <- list(list(),list(),list(),list(),list(),list(),list(),list())
for(i in 1:length(allmodelsCARp0)){
  print(i)
  for(j in 1:10){
    print(j)
    # Fit linear model
    lmodel <- lm(formula = allforms[[i]], data = alldata[[i]][-allfolds[[i]][[j]], ])
    # Enter coefficients of CAR model into linear model
    lmodel$coefficients <- coef(allmodelsCARp0[[i]][[j]])
    # Make predictions using the validation data
    predicted <- predict(lmodel, newdata = alldata[[i]][allfolds[[i]][[j]], ])
    # Compute metrics
    SpCor <-  cor(predicted,
                  alltestsCAR[[i]][[j]][[3]], method = "spearman")
    model <- lm(alltestsCAR[[i]][[j]][[3]] ~ predicted)
    rsq <- summary(model)$r.squared
    print(SpCor)
    allSpCorCARp0[[i]][[j]] <- SpCor
    print(rsq)
    allrsqCARp0[[i]][[j]] <- rsq
  } 
}
# EXTRACT METRICS
# Best R2
max(unlist(allrsqCARp0[[1]]))
max(unlist(allrsqCARp0[[2]]))
max(unlist(allrsqCARp0[[3]]))
max(unlist(allrsqCARp0[[4]]))
max(unlist(allrsqCARp0[[5]]))
max(unlist(allrsqCARp0[[6]]))
max(unlist(allrsqCARp0[[7]]))
max(unlist(allrsqCARp0[[8]]))
# Corresponding spearman
allSpCorCARp0[[1]][[which.max(unlist(allrsqCARp0[[1]]))]]
allSpCorCARp0[[2]][[which.max(unlist(allrsqCARp0[[2]]))]]
allSpCorCARp0[[3]][[which.max(unlist(allrsqCARp0[[3]]))]]
allSpCorCARp0[[4]][[which.max(unlist(allrsqCARp0[[4]]))]]
allSpCorCARp0[[5]][[which.max(unlist(allrsqCARp0[[5]]))]]
allSpCorCARp0[[6]][[which.max(unlist(allrsqCARp0[[6]]))]]
allSpCorCARp0[[7]][[which.max(unlist(allrsqCARp0[[7]]))]]
allSpCorCARp0[[8]][[which.max(unlist(allrsqCARp0[[8]]))]]
# 4.5 Test CAR models rho 1 ----------------------------------------------------
# TEST MODELS FOR SPECIES AND FOLDS
allSpCorCARp1 <- list(list(),list(),list(),list(),list(),list(),list(),list())
allrsqCARp1 <- list(list(),list(),list(),list(),list(),list(),list(),list())
for(i in 1:length(allmodelsCARp1)){
  print(i)
  for(j in 1:10){
    print(j)
    # Fit linear model
    lmodel <- lm(formula = allforms[[i]], data = alldata[[i]][-allfolds[[i]][[j]], ])
    # Enter coefficients of CAR model into linear model
    lmodel$coefficients <- coef(allmodelsCARp1[[i]][[j]])
    # Make predictions using the validation data
    predicted <- predict(lmodel, newdata = alltestsCAR[[i]][[j]])
    # Compute metrics
    SpCor <-  cor(predicted,
                  alltestsCAR[[i]][[j]][[3]], method = "spearman")
    model <- lm(alltestsCAR[[i]][[j]][[3]] ~ predicted)
    rsq <- summary(model)$r.squared
    print(SpCor)
    allSpCorCARp1[[i]][[j]] <- SpCor
    print(rsq)
    allrsqCARp1[[i]][[j]] <- rsq
  } 
}
# EXTRACT METRICS
# Best R2
max(unlist(allrsqCARp1[[1]]))
max(unlist(allrsqCARp1[[2]]))
max(unlist(allrsqCARp1[[3]]))
max(unlist(allrsqCARp1[[4]]))
max(unlist(allrsqCARp1[[5]]))
max(unlist(allrsqCARp1[[6]]))
max(unlist(allrsqCARp1[[7]]))
max(unlist(allrsqCARp1[[8]]))
# Corresponding spearman
allSpCorCARp1[[1]][[which.max(unlist(allrsqCARp1[[1]]))]]
allSpCorCARp1[[2]][[which.max(unlist(allrsqCARp1[[2]]))]]
allSpCorCARp1[[3]][[which.max(unlist(allrsqCARp1[[3]]))]]
allSpCorCARp1[[4]][[which.max(unlist(allrsqCARp1[[4]]))]]
allSpCorCARp1[[5]][[which.max(unlist(allrsqCARp1[[5]]))]]
allSpCorCARp1[[6]][[which.max(unlist(allrsqCARp1[[6]]))]]
allSpCorCARp1[[7]][[which.max(unlist(allrsqCARp1[[7]]))]]
allSpCorCARp1[[8]][[which.max(unlist(allrsqCARp1[[8]]))]]
# 4.6 Test CAR models rho 0.5 --------------------------------------------------
# TEST MODELS FOR SPECIES AND FOLDS
allSpCorCARp05 <- list(list(),list(),list(),list(),list(),list(),list(),list())
allrsqCARp05 <- list(list(),list(),list(),list(),list(),list(),list(),list())
for(i in 1:length(allmodelsCARp05)){
  print(i)
  for(j in 1:10){
    print(j)
    # Fit linear model
    lmodel <- lm(formula = allforms[[i]], data = alldata[[i]][-allfolds[[i]][[j]], ])
    # Enter coefficients of CAR model into linear model
    lmodel$coefficients <- coef(allmodelsCARp05[[i]][[j]])
    # Make predictions using the validation data
    predicted <- predict(lmodel, newdata = alltestsCAR[[i]][[j]])
    # Compute metrics
    SpCor <-  cor(predicted,
                  alltestsCAR[[i]][[j]][[3]], method = "spearman")
    model <- lm(alltestsCAR[[i]][[j]][[3]] ~ predicted)
    rsq <- summary(model)$r.squared
    print(SpCor)
    allSpCorCARp05[[i]][[j]] <- SpCor
    print(rsq)
    allrsqCARp05[[i]][[j]] <- rsq
  } 
}
# EXTRACT METRICS
# Best R2
max(unlist(allrsqCARp05[[1]]))
max(unlist(allrsqCARp05[[2]]))
max(unlist(allrsqCARp05[[3]]))
max(unlist(allrsqCARp05[[4]]))
max(unlist(allrsqCARp05[[5]]))
max(unlist(allrsqCARp05[[6]]))
max(unlist(allrsqCARp05[[7]]))
max(unlist(allrsqCARp05[[8]]))
# Corresponding spearman
allSpCorCARp05[[1]][[which.max(unlist(allrsqCARp05[[1]]))]]
allSpCorCARp05[[2]][[which.max(unlist(allrsqCARp05[[2]]))]]
allSpCorCARp05[[3]][[which.max(unlist(allrsqCARp05[[3]]))]]
allSpCorCARp05[[4]][[which.max(unlist(allrsqCARp05[[4]]))]]
allSpCorCARp05[[5]][[which.max(unlist(allrsqCARp05[[5]]))]]
allSpCorCARp05[[6]][[which.max(unlist(allrsqCARp05[[6]]))]]
allSpCorCARp05[[7]][[which.max(unlist(allrsqCARp05[[7]]))]]
allSpCorCARp05[[8]][[which.max(unlist(allrsqCARp05[[8]]))]]
# 5. 5 fold cross validation for less than 50 observations #####################
# Compute Shapefiles for 5 folds -----------------------------------------
allshapes5 <- list(list(),list())
for(i in 1:length(allfolds5)){
  print(i)
  shape <- lapply(allfolds5[[i]], function(fold) {
    # Get the train data for this fold
    train_data <- alldata5[[i]][-fold, ]
    # Get tiles and shape for train data
    TilesNShape <- GetTilesNShape(train_data)
    ## Get Neighbor List
    W.nb <- poly2nb(pl = TilesNShape[[2]], row.names = rownames(train_data))
    W.list <- nb2listw(W.nb, style = "B") #style can take values “W”, “B”, “C”, “U”, “minmax” and “S”
    #MoranResults <- RunMoran(TilesNShape,train_data,allLmodels[[1]][[1]],10000)
    return(W.list)})
  allshapes5[[i]] <- append(allshapes5[[i]],shape)}
# Fit CAR models with function ---------------------------------------------
allmodelsCAR5p0 <- list(list(),list())
for(i in 1:length(allmodelsCAR5p0)){
  for(j in 1:5){
    train_data <- alldata5[[i]][-allfolds5[[i]][[j]], ]
    CARmodel <- Run1MChainLB(allshapes5[[i]][[j]][[2]], allforms5[[i]],train_data,5000,15000,0,TRUE)
    allmodelsCAR5p0[[i]] <- append(allmodelsCAR5p0[[i]],list(CARmodel))
  }
}
allmodelsCAR5p1 <- list(list(),list())
for(i in 1:length(allmodelsCAR5p1)){
  for(j in 1:5){
    train_data <- alldata5[[i]][-allfolds5[[i]][[j]], ]
    CARmodel <- Run1MChainLB(allshapes5[[i]][[j]][[2]], allforms5[[i]],train_data,5000,15000,1,TRUE)
    allmodelsCAR5p1[[i]] <- append(allmodelsCAR5p1[[i]],list(CARmodel))
  }
}
allmodelsCAR5p05 <- list(list(),list())
for(i in 1:length(allmodelsCAR5p05)){
  for(j in 1:5){
    train_data <- alldata5[[i]][-allfolds5[[i]][[j]], ]
    CARmodel <- Run1MChainLB(allshapes5[[i]][[j]][[2]], allforms5[[i]],train_data,5000,15000,0.5,TRUE)
    allmodelsCAR5p05[[i]] <- append(allmodelsCAR5p05[[i]],list(CARmodel))
  }
}
# TEST 1: Rho 0
alltestsCAR5 <- list(list(),list())
for(i in 1:length(allfolds5)){
  print(i)
  tests <- lapply(allfolds5[[i]], function(fold) {
    # Get the testing data for this fold
    test_data <- alldata5[[i]][fold, ]
    return(test_data)})
  alltestsCAR5[[i]] <- append(alltestsCAR5[[i]],tests)}
allSpCorCAR5p0 <- list(list(),list())
allrsqCAR5p0 <- list(list(),list())
for(i in 1:length(allmodelsCAR5p0)){
  print(i)
  for(j in 1:5){
    print(j)
    # Fit linear model
    lmodel <- lm(formula = allforms5[[i]], data = alldata5[[i]][-allfolds5[[i]][[j]], ])
    # Enter coefficients of CAR model into linear model
    lmodel$coefficients <- coef(allmodelsCAR5p0[[i]][[j]])
    # Make predictions using the validation data
    predicted <- predict(lmodel, newdata = alldata5[[i]][allfolds5[[i]][[j]], ])
    # Compute metrics
    SpCor <-  cor(predicted,
                  alltestsCAR5[[i]][[j]][[3]], method = "spearman")
    model <- lm(alltestsCAR5[[i]][[j]][[3]] ~ predicted)
    rsq <- summary(model)$r.squared
    print(SpCor)
    allSpCorCAR5p0[[i]][[j]] <- SpCor
    print(rsq)
    allrsqCAR5p0[[i]][[j]] <- rsq
  } 
}
# EXTRACT METRICS
# Best R2
max(unlist(allrsqCAR5p0[[1]]))
max(unlist(allrsqCAR5p0[[2]]))
# TEST 2: Rho 1
alltestsCAR5 <- list(list(),list())
for(i in 1:length(allfolds5)){
  print(i)
  tests <- lapply(allfolds5[[i]], function(fold) {
    # Get the testing data for this fold
    test_data <- alldata5[[i]][fold, ]
    return(test_data)})
  alltestsCAR5[[i]] <- append(alltestsCAR5[[i]],tests)}
allSpCorCAR5p1 <- list(list(),list())
allrsqCAR5p1 <- list(list(),list())
for(i in 1:length(allmodelsCAR5p1)){
  print(i)
  for(j in 1:5){
    print(j)
    # Fit linear model
    lmodel <- lm(formula = allforms5[[i]], data = alldata5[[i]][-allfolds5[[i]][[j]], ])
    # Enter coefficients of CAR model into linear model
    lmodel$coefficients <- coef(allmodelsCAR5p1[[i]][[j]])
    # Make predictions using the validation data
    predicted <- predict(lmodel, newdata = alldata5[[i]][allfolds5[[i]][[j]], ])
    # Compute metrics
    SpCor <-  cor(predicted,
                  alltestsCAR5[[i]][[j]][[3]], method = "spearman")
    model <- lm(alltestsCAR5[[i]][[j]][[3]] ~ predicted)
    rsq <- summary(model)$r.squared
    print(SpCor)
    allSpCorCAR5p1[[i]][[j]] <- SpCor
    print(rsq)
    allrsqCAR5p1[[i]][[j]] <- rsq
  } 
}
# EXTRACT METRICS
# Best R2
max(unlist(allrsqCAR5p1[[1]]))
max(unlist(allrsqCAR5p1[[2]]))
# TEST 2: Rho 0.5
alltestsCAR5 <- list(list(),list())
for(i in 1:length(allfolds5)){
  print(i)
  tests <- lapply(allfolds5[[i]], function(fold) {
    # Get the testing data for this fold
    test_data <- alldata5[[i]][fold, ]
    return(test_data)})
  alltestsCAR5[[i]] <- append(alltestsCAR5[[i]],tests)}
allSpCorCAR5p05 <- list(list(),list())
allrsqCAR5p05 <- list(list(),list())
for(i in 1:length(allmodelsCAR5p05)){
  print(i)
  for(j in 1:5){
    print(j)
    # Fit linear model
    lmodel <- lm(formula = allforms5[[i]], data = alldata5[[i]][-allfolds5[[i]][[j]], ])
    # Enter coefficients of CAR model into linear model
    lmodel$coefficients <- coef(allmodelsCAR5p05[[i]][[j]])
    # Make predictions using the validation data
    predicted <- predict(lmodel, newdata = alldata5[[i]][allfolds5[[i]][[j]], ])
    # Compute metrics
    SpCor <-  cor(predicted,
                  alltestsCAR5[[i]][[j]][[3]], method = "spearman")
    model <- lm(alltestsCAR5[[i]][[j]][[3]] ~ predicted)
    rsq <- summary(model)$r.squared
    print(SpCor)
    allSpCorCAR5p05[[i]][[j]] <- SpCor
    print(rsq)
    allrsqCAR5p05[[i]][[j]] <- rsq
  } 
}
# EXTRACT METRICS
# Best R2
max(unlist(allrsqCAR5p05[[1]]))
max(unlist(allrsqCAR5p05[[2]]))
####################### END CODE ###############################################
