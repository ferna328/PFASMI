################### R11BVSAnalysis.R ###########################################
# Research Paper: Large-scale Assessment of PFAS Compounds in Drinking Water   #
# Sources through Machine Learning                                             #
# Nicolas Fernandez, A. Pouyan Nejadhashemi, Christian Loveall                 #
# Department of Biosystems and Agricultural Engineering                        #
# Michigan State University, East Lansing, MI, United States                   #
# Code written and curated by Nicolas Fernandez (OrcID 0000-0001-7979-2941)    #
######################## Description ###########################################
# Once R10BayesianVariableSelection.R is run to obtain all variable selection  #
# results, this code is intended to analyze said results                       #
################################################################################
###### 0. IMPORT AND  PREPARE DATA #############################################
# 0.0 Set Working Directory ----------------------------------------------------
wd = 'E:/XXX/YYY/ZZZ/PFASMIproject'
setwd(wd)
# 0.1 Read workspace with results ----------------------------------------------
dirWS <- paste(wd,"Model5/BVS10000iterSP8.RData", sep = "/")
load(dirWS)
###### 1. EXPLORE RESULTS ######################################################
# The following functions were used to produce the final variable selection
# table presented in the table
# more functions can be explored in the BayesVarSel Package
# https://cran.r-project.org/web/packages/BayesVarSel/BayesVarSel.pdf
BVSsp1.GibbsBvsF$time # Time running in seconds
BVSsp1.GibbsBvsF$lmfull # Coefficients of full model
BVSsp1.GibbsBvsF$lmnull # Coefficients of null model
BVSsp1.GibbsBvsF$variables # List of variables
BVSsp1.GibbsBvsF$n # number of observations
BVSsp1.GibbsBvsF$p # number of explanatory vars
BVSsp1.GibbsBvsF$k # number of fixed vars
BVSsp1.GibbsBvsF$HPMbin # Most probable model (Bin)
BVSsp1.GibbsBvsF$inclprob # Probability of inclusion
BVSsp1.GibbsBvsF$jointinclprob # joint prob of inclusion
BVSsp1.GibbsBvsF$postprobdim # Posterior probability of inclusion
BVSsp1.GibbsBvsF$modelslogBF
BVSsp1.GibbsBvsF$modelswllogBF
BVSsp1.GibbsBvsF$call
BVSsp1.GibbsBvsF$C
BVSsp1.GibbsBvsF$positions
BVSsp1.GibbsBvsF$positionsx
BVSsp1.GibbsBvsF$prior.betas
BVSsp1.GibbsBvsF$prior.models
BVSsp1.GibbsBvsF$method
print(BVSsp1.GibbsBvsF)
summary(BVSsp1.GibbsBvsF) ################ this summary has all values needed
BMAcoeff(BVSsp1.GibbsBvsF, n.sim = 1000, method = "svd")
Jointness(BVSsp1.GibbsBvsF, covariates = "All")
plot(BVSsp1.GibbsBvsF, option = "dimension")
plot(BVSsp1.GibbsBvsF, option = "joint")
plot(BVSsp1.GibbsBvsF, option = "not")
predict(BVSsp1.GibbsBvsF, dfSP1, n.sim = 100)
print(Jointness(BVSsp1.GibbsBvsF, covariates = "All"))
####################### END CODE ###############################################



