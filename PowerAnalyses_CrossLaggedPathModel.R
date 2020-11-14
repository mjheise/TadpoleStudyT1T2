## CODE DESCRIPTION

# This code simulates a population with significant paths
# from 3 explicit ToM-relevant precursors (visual perspective, pretense
# and penny hiding/ early knowledge understanding) and spontaneous
# belief understanding (true and false belief) at Time 1, and computes a power analysis (Beta)
# for each of these predictors to theory of mind at Time 2. This model
# also computes a power analysis (Beta) for a significant covariance between
# each of the 5 ToM-relevant precursors.

# The simulated data will have the following parameters: 

# Time 1:
# Beta =.1 for paths controlling for language, age, and EF
#   There is little empirical evidence in toddlerhood for the contribution of language, age
#   and EF to ToM.
# Covariance = .1 between distal constructs (e.g., language and EF)
# Covariance = .3 between related constructs (e.g., visual perspective taking
#   & penny hiding/early knowledge)

# Time 2:
# Beta =.3 for paths controlling for covariates (e.g., language, age and EF)
#   There is strong evidence in preschool for the contribution of language, age
#   and EF to ToM. (see meta-analyses: Devine & Hughes, 2014; Milligan, Astington & Dack, 2007)
# Covariance = .1 between covariates (e.g., age, language, EF)
# Covariance = .3 between covariates across Time 1 and Time 2 (e.g., T1 age to T2 age)


# Edited by MJH, 11/13/2020

# Requires:
# Population-level parameters, number of iterations, sample size
# packages: lavaan

# Outputs: 
# -A dataframe with the beta coefficients from each sample

library(lavaan)

#------------------------------------------------------------------------
#set.seed(25)

sampleN = 124
iterations = 1000
div = 10 # iterations/div=100

#------------------------------------------------------------------------
# 1. Population Model

PopMod <- '# control for T1 age, language, EF, and WM
          t1_spontFB_scale ~ .1*t1_age_scale + .1*t1_macarthur_scale + .1*t1_workingmemory_scale + .1*t1_objectstroop_scale + .1*t1_shapestroop_scale
          t1_spontTB_scale ~ .1*t1_age_scale + .1*t1_macarthur_scale + .1*t1_workingmemory_scale + .1*t1_objectstroop_scale + .1*t1_shapestroop_scale
          t1_penny_scale ~ .1*t1_age_scale + .1*t1_macarthur_scale + .1*t1_workingmemory_scale + .1*t1_objectstroop_scale + .1*t1_shapestroop_scale
          t1_visperspective_scale ~ .1*t1_age_scale + .1*t1_macarthur_scale + .1*t1_workingmemory_scale + .1*t1_objectstroop_scale + .1*t1_shapestroop_scale
          t1_pretense_scale ~ .1*t1_age_scale + .1*t1_macarthur_scale + .1*t1_workingmemory_scale + .1*t1_objectstroop_scale + .1*t1_shapestroop_scale
          
          # control for T2 age, EF, and language
          t2_TOM_scale ~ .1*t2_age_scale + .1*t2_ppvt_scale + .1*t2_EF_scale
          
          # model
          t2_TOM_scale ~ .3*t1_spontFB_scale + .3*t1_spontTB_scale + .3*t1_penny_scale + .3*t1_visperspective_scale + .3*t1_pretense_scale
          
          # covariances between t1 covariates
          t1_age_scale ~~ .1*t1_macarthur_scale
          t1_age_scale ~~ .1*t1_objectstroop_scale
          t1_age_scale ~~ .1*t1_shapestroop_scale
          t1_age_scale ~~ .1*t1_workingmemory_scale
          
          t1_macarthur_scale ~~ .1*t1_objectstroop_scale
          t1_macarthur_scale ~~ .1*t1_shapestroop_scale
          t1_macarthur_scale ~~ .1*t1_workingmemory_scale
          
          t1_objectstroop_scale ~~ .1*t1_workingmemory_scale
          t1_shapestroop_scale ~~ .1*t1_workingmemory_scale

          # covariances between t1 predictors
          t1_spontFB_scale ~~ .3*t1_spontTB_scale
          t1_spontFB_scale ~~ .3*t1_penny_scale
          t1_spontFB_scale ~~ .3*t1_visperspective_scale
          t1_spontFB_scale ~~ .3*t1_pretense_scale
          
          t1_spontTB_scale ~~ .3*t1_penny_scale
          t1_spontTB_scale ~~ .3*t1_visperspective_scale
          t1_spontTB_scale ~~ .3*t1_pretense_scale
          
          t1_penny_scale ~~ .3*t1_visperspective_scale
          t1_penny_scale ~~ .3*t1_pretense_scale
          t1_visperspective_scale ~~ .3*t1_pretense_scale
          
          # covariances between t2 covariates
          t2_age_scale ~~ .1*t2_ppvt_scale
          t2_age_scale ~~ .1*t2_EF_scale
          t2_ppvt_scale ~~ .1*t2_EF_scale
          
          # covariances between t1 and t2
          t1_age_scale ~~ .3*t2_age_scale
          t1_macarthur_scale ~~ .3*t2_ppvt_scale'

#------------------------------------------------------------------------
# 2. Fit Model

FitMod <- '# control for T1 age, language, EF, and WM
t1_spontFB_scale ~ t1_age_scale + t1_macarthur_scale + t1_workingmemory_scale + t1_objectstroop_scale + t1_shapestroop_scale
t1_spontTB_scale ~ t1_age_scale + t1_macarthur_scale + t1_workingmemory_scale + t1_objectstroop_scale + t1_shapestroop_scale
t1_penny_scale ~ t1_age_scale + t1_macarthur_scale + t1_workingmemory_scale + t1_objectstroop_scale + t1_shapestroop_scale
t1_visperspective_scale ~ t1_age_scale + t1_macarthur_scale + t1_workingmemory_scale + t1_objectstroop_scale + t1_shapestroop_scale
t1_pretense_scale ~ t1_age_scale + t1_macarthur_scale + t1_workingmemory_scale + t1_objectstroop_scale + t1_shapestroop_scale

# control for T2 age, EF, and language
t2_TOM_scale ~ t2_age_scale + t2_ppvt_scale + t2_EF_scale

# model
t2_TOM_scale ~ t1_spontFB_scale + t1_spontTB_scale + t1_penny_scale + t1_visperspective_scale + t1_pretense_scale

# covariances between t1 covariates
t1_age_scale ~~ t1_macarthur_scale
t1_age_scale ~~ t1_objectstroop_scale
t1_age_scale ~~ t1_shapestroop_scale
t1_age_scale ~~ t1_workingmemory_scale

t1_macarthur_scale ~~ t1_objectstroop_scale
t1_macarthur_scale ~~ t1_shapestroop_scale
t1_macarthur_scale ~~ t1_workingmemory_scale

t1_objectstroop_scale ~~ t1_workingmemory_scale
t1_shapestroop_scale ~~ t1_workingmemory_scale

# covariances between t1 predictors
t1_spontFB_scale ~~ t1_spontTB_scale
t1_spontFB_scale ~~ t1_penny_scale
t1_spontFB_scale ~~ t1_visperspective_scale
t1_spontFB_scale ~~ t1_pretense_scale

t1_spontTB_scale ~~ t1_penny_scale
t1_spontTB_scale ~~ t1_visperspective_scale
t1_spontTB_scale ~~ t1_pretense_scale

t1_penny_scale ~~ t1_visperspective_scale
t1_penny_scale ~~ t1_pretense_scale
t1_visperspective_scale ~~ t1_pretense_scale

# covariances between t2 covariates
t2_age_scale ~~ t2_ppvt_scale
t2_age_scale ~~ t2_EF_scale
t2_ppvt_scale ~~ t2_EF_scale

# covariances between t1 and t2
t1_age_scale ~~ t2_age_scale
t1_macarthur_scale ~~ t2_ppvt_scale'

#------------------------------------------------------------------------
# 3. Simulate data

results <- NULL

for (i in 1:iterations) {
  data <- simulateData(PopMod, sample.nobs = sampleN)
  data <- as.data.frame(data)
  fit <- sem(FitMod, data)
  results <- rbind(results, parameterEstimates(fit))
}

#------------------------------------------------------------------------
# 4. Print power & sample parameter estimates: T2 ToM onto T1 ToM-relevant precursors

predictors = c('t1_spontFB_scale', 't1_spontTB_scale', 't1_penny_scale', 
               't1_visperspective_scale', 't1_pretense_scale')
dependent = 't2_TOM_scale'

PowerEst <- NULL
Power <- NULL
Means <- NULL

for (i in 1:length(predictors)) {
  sampleSig <- results[which(results$rhs == predictors[i] & results$lhs =='t2_TOM_scale'), ]
  PowerEst$sig <- ifelse(sampleSig$pvalue<.05, 1, 0)

  mean <- mean(results$est)
  CIlower <- (mean-(1.96*sd(results$est)))
  CIupper <- (mean+(1.96*sd(results$est)))
  
  Power[i] <- paste("Power to detect an effect between ToM and ", predictors[i], "is", sum(PowerEst$sig)/div)
  Means[i] <- paste("Mean Beta coefficient from ToM onto", predictors[i], "is", round(mean, digits=3), 
              ", 95% Ci [", round(CIlower, digits=3), ",", round(CIupper, digits=3), "].")

}

Power
Means

#------------------------------------------------------------------------
# 4. Print power & sample parameter estimates: T2 ToM onto T1 ToM-relevant precursors

covA = c('t1_spontFB_scale', 't1_spontFB_scale', 't1_spontFB_scale', 
         't1_spontTB_scale', 't1_spontTB_scale', 't1_spontTB_scale')
covB = c('t1_penny_scale', 't1_visperspective_scale', 't1_pretense_scale',
         't1_penny_scale', 't1_visperspective_scale', 't1_pretense_scale')

PowerEst <- NULL
Power <- NULL
Means <- NULL

for (i in 1:length(covA)) {
  sampleSig <- results[which(results$lhs == covA[i] & results$rhs ==covB[i]), ]
  PowerEst$sig <- ifelse(sampleSig$pvalue<.05, 1, 0)
  
  mean <- mean(results$est)
  CIlower <- (mean-(1.96*sd(results$est)))
  CIupper <- (mean+(1.96*sd(results$est)))
  
  Power[i] <- paste("Power to detect an effect between ", covA[i]," and ", covB[i], "is", sum(PowerEst$sig)/div)
  Means[i] <- paste("Mean covariance between", covA[i], " and ", covB[i], "is", round(mean, digits=3), 
                    ", 95% Ci [", round(CIlower, digits=3), ",", round(CIupper, digits=3), "].")
  
}

Power
Means

#------------------------------------------------------------------------

