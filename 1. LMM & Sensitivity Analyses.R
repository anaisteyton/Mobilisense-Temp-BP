# MOBILISENSE ANALYSIS 
# LINEAR MIXED MODELS (Line 12) + SENSITIVITY ANALYSES (Line 95)
# PUBLISHED ON 17/09/2025, MODIFIED 12/05/2026
# Code written by Anais Teyton

library(nlme)  
library(splines)

# Note: Input data (shown in script as YOURDATA)
# Note: 5 minute model shown only; simply replace model variables for other time windows (i.e., 10/ 15/ 30 min, or 1/ 2 hours)

#########################################################
# 5 MINUTE MODELS
#########################################################

fit_mixed_model <- function(response, predictor) {
  formula <- as.formula(paste(response, "~", predictor, "+ ns(time_trend, df = 7) + sex + age + education_level + employment_level + income_household_per_member + five_min_vm + Five_domicile + Five_motorized + Five_nonMotorized + weekend + wave + Residence + season"))
  model <- lme(formula, random = list(IDNUMBER1 = pdDiag(form = ~ 1)), correlation = corAR1(form = ~ time | IDNUMBER1), data = YOURDATA)
  return(model)
}

print_coefficients <- function(model) {
  coef_summary <- coef(summary(model))
  print(coef_summary)
}

# Mixed-effects models fit for each combination of BP/ Temp variables
model_SBP_temp <- fit_mixed_model("SBPbr", "five_min_temp")
 AIC(model_SBP_temp)
model_SBP_temp_sd <- fit_mixed_model("SBPbr", "five_min_temp_sddir")
model_DBP_temp <- fit_mixed_model("DBPbr", "five_min_temp")
 AIC(model_DBP_temp)
model_DBP_temp_sd <- fit_mixed_model("DBPbr", "five_min_temp_sddir")

# Print results
print("SBP - five_min_temp")
print_coefficients(model_SBP_temp)

print("SBP - five_min_temp_dtv")
print_coefficients(model_SBP_temp_sd)

print("DBP - five_min_temp")
print_coefficients(model_DBP_temp)

print("DBP - five_min_temp_dtv")
print_coefficients(model_DBP_temp_sd)

# SBP 5 Minutes
summary<-data.frame(summary(model_SBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Systolic Blood Pressure"
exposure <- "5 Min"
summary <- cbind(outcome, exposure, summary)  
estimates_SBP <- summary

summary<-data.frame(summary(model_SBP_temp_sd)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Systolic Blood Pressure"
exposure <- "5 Min SD"
summary <- cbind(outcome, exposure, summary)  # Add outcome and exposure columns
estimates_SBP<-rbind(estimates_SBP, summary)

###

# DBP 5 Minutes
summary<-data.frame(summary(model_DBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Diastolic Blood Pressure"
exposure <- "5 Min"
summary <- cbind(outcome, exposure, summary)  
estimates_DBP<-summary

summary<-data.frame(summary(model_DBP_temp_sd)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Diastolic Blood Pressure"
exposure <- "5 Min SD"
summary <- cbind(outcome, exposure, summary)  
estimates_DBP<-rbind(estimates_DBP, summary)

write.csv(estimates_DBP, file = "YOUR DBP FILE LOCATION HERE .csv", row.names = FALSE)
write.csv(estimates_SBP, file = "YOUR SBP FILE LOCATION HERE .csv", row.names = FALSE)





##########################################################
# SENSITIVITY ANALYSIS
##########################################################

####################
# SENSITIVITY #1
# ADDING COUNTRY OF BIRTH FOR THE PARTICIPANT
####################

fit_mixed_model <- function(response, predictor) {
  formula <- as.formula(paste(response, "~", predictor, "+ ns(time_trend, df = 7) + sex + age + education_level + employment_level + income_household_per_member + five_min_vm + Five_domicile + Five_motorized + Five_nonMotorized + weekend+ wave +Residence + COB_participant1"))
  model <- lme(formula, random = list(IDNUMBER1 = pdDiag(form = ~ 1)), correlation = corAR1(form = ~ time | IDNUMBER1), data = YOURDATA)
  return(model)
}

print_coefficients <- function(model) {
  coef_summary <- coef(summary(model))
  print(coef_summary)
}

model_SBP_temp <- fit_mixed_model("SBPbr", "five_min_temp")
model_DBP_temp <- fit_mixed_model("DBPbr", "five_min_temp")

# Print results
print("SBP - five_min_temp")
print_coefficients(model_SBP_temp)
print("DBP - five_min_temp")
print_coefficients(model_DBP_temp)

# SBP 5 Minutes
summary<-data.frame(summary(model_SBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Systolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Country of Birth (Participant only)"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary) 
estimates_SBP_sensitivity <- summary_sensitivity

###
# DBP 5 Minutes
summary<-data.frame(summary(model_DBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Diastolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Country of Birth (Participant only)"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary)  
estimates_DBP_sensitivity<-summary_sensitivity

####################
# SENSITIVITY #2
# ADDING COUNTRY OF BIRTH, CONSIDERING PARTICIPANT+PARENT COB
####################
fit_mixed_model <- function(response, predictor) {
  formula <- as.formula(paste(response, "~", predictor, "+ ns(time_trend, df = 7) + sex + age + education_level + employment_level + income_household_per_member + five_min_vm + Five_domicile + Five_motorized + Five_nonMotorized + weekend+ wave +Residence + COB_all"))
  model <- lme(formula, random = list(IDNUMBER1 = pdDiag(form = ~ 1)), correlation = corAR1(form = ~ time | IDNUMBER1), data = YOURDATA)
  return(model)
}

print_coefficients <- function(model) {
  coef_summary <- coef(summary(model))
  print(coef_summary)
}

model_SBP_temp <- fit_mixed_model("SBPbr", "five_min_temp")
model_DBP_temp <- fit_mixed_model("DBPbr", "five_min_temp")

# Print results
print("SBP - five_min_temp")
print_coefficients(model_SBP_temp)
print("DBP - five_min_temp")
print_coefficients(model_DBP_temp)

# SBP 5 Minutes
summary<-data.frame(summary(model_SBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Systolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Country of Birth (Participant & Parents)"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary)  
estimates_SBP_sensitivity<-rbind(estimates_SBP_sensitivity, summary_sensitivity)

###
# DBP 5 Minutes
summary<-data.frame(summary(model_DBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Diastolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Country of Birth (Participant & Parents)"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary)
estimates_DBP_sensitivity<-rbind(estimates_DBP_sensitivity, summary_sensitivity)

####################
# SENSITIVITY #3
# uSING ALTERNATIVE CHARACTERIZATION FOR EDUCATION
####################
fit_mixed_model <- function(response, predictor) {
  formula <- as.formula(paste(response, "~", predictor, "+ ns(time_trend, df = 7) + sex + age + education_level1 + employment_level + income_household_per_member + five_min_vm + Five_domicile + Five_motorized + Five_nonMotorized + weekend+ wave +Residence"))
  model <- lme(formula, random = list(IDNUMBER1 = pdDiag(form = ~ 1)), correlation = corAR1(form = ~ time | IDNUMBER1), data = YOURDATA)
  return(model)
}

print_coefficients <- function(model) {
  coef_summary <- coef(summary(model))
  print(coef_summary)
}

model_SBP_temp <- fit_mixed_model("SBPbr", "five_min_temp")
model_DBP_temp <- fit_mixed_model("DBPbr", "five_min_temp")

# Print results
print("SBP - five_min_temp")
print_coefficients(model_SBP_temp)
print("DBP - five_min_temp")
print_coefficients(model_DBP_temp)

# SBP 5 Minutes
summary<-data.frame(summary(model_SBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Systolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Alternative Education"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary) 
estimates_SBP_sensitivity<-rbind(estimates_SBP_sensitivity, summary_sensitivity)

###
# DBP 5 Minutes
summary<-data.frame(summary(model_DBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Diastolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Alternative Education"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary) 
estimates_DBP_sensitivity<-rbind(estimates_DBP_sensitivity, summary_sensitivity)

####################
# SENSITIVITY #4
# uSING ALTERNATIVE CHARACTERIZATION FOR EMPLOYMENT
####################
fit_mixed_model <- function(response, predictor) {
  formula <- as.formula(paste(response, "~", predictor, "+ ns(time_trend, df = 7) + sex + age + education_level + employment_level1 + income_household_per_member + five_min_vm + Five_domicile + Five_motorized + Five_nonMotorized + weekend+ wave +Residence"))
  model <- lme(formula, random = list(IDNUMBER1 = pdDiag(form = ~ 1)), correlation = corAR1(form = ~ time | IDNUMBER1), data = YOURDATA)
  return(model)
}

print_coefficients <- function(model) {
  coef_summary <- coef(summary(model))
  print(coef_summary)
}

model_SBP_temp <- fit_mixed_model("SBPbr", "five_min_temp")
model_DBP_temp <- fit_mixed_model("DBPbr", "five_min_temp")

# Print results
print("SBP - five_min_temp")
print_coefficients(model_SBP_temp)
print("DBP - five_min_temp")
print_coefficients(model_DBP_temp)

# SBP 5 Minutes
summary<-data.frame(summary(model_SBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Systolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Alternative Employment"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary)
estimates_SBP_sensitivity<-rbind(estimates_SBP_sensitivity, summary_sensitivity)

###
# DBP 5 Minutes
summary<-data.frame(summary(model_DBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Diastolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Alternative Employment"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary) 
estimates_DBP_sensitivity<-rbind(estimates_DBP_sensitivity, summary_sensitivity)

####################
# SENSITIVITY #5
# uSING ALTERNATIVE CHARACTERIZATION FOR INCOME (#1)
####################
fit_mixed_model <- function(response, predictor) {
  formula <- as.formula(paste(response, "~", predictor, "+ ns(time_trend, df = 7) + sex + age + education_level + employment_level + income_household_per_member1 + five_min_vm + Five_domicile + Five_motorized + Five_nonMotorized + weekend+ wave +Residence"))
  model <- lme(formula, random = list(IDNUMBER1 = pdDiag(form = ~ 1)), correlation = corAR1(form = ~ time | IDNUMBER1), data = YOURDATA)
  return(model)
}

print_coefficients <- function(model) {
  coef_summary <- coef(summary(model))
  print(coef_summary)
}

model_SBP_temp <- fit_mixed_model("SBPbr", "five_min_temp")
model_DBP_temp <- fit_mixed_model("DBPbr", "five_min_temp")

# Print results
print("SBP - five_min_temp")
print_coefficients(model_SBP_temp)
print("DBP - five_min_temp")
print_coefficients(model_DBP_temp)

# SBP 5 Minutes
summary<-data.frame(summary(model_SBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Systolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Alternative Income (Continuous)"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary)
estimates_SBP_sensitivity<-rbind(estimates_SBP_sensitivity, summary_sensitivity)

###
# DBP 5 Minutes
summary<-data.frame(summary(model_DBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Diastolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Alternative Income (Continuous)"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary) 
estimates_DBP_sensitivity<-rbind(estimates_DBP_sensitivity, summary_sensitivity)


####################
# SENSITIVITY #6
# uSING ALTERNATIVE CHARACTERIZATION FOR INCOME (#2)
####################
fit_mixed_model <- function(response, predictor) {
  formula <- as.formula(paste(response, "~", predictor, "+ ns(time_trend, df = 7) + sex + age + education_level + employment_level + income1 + five_min_vm + Five_domicile + Five_motorized + Five_nonMotorized + weekend+ wave +Residence"))  model <- lme(formula, random = list(IDNUMBER1 = pdDiag(form = ~ 1)), correlation = corAR1(form = ~ time | IDNUMBER1), data = YOURDATA)
  return(model)
}

print_coefficients <- function(model) {
  coef_summary <- coef(summary(model))
  print(coef_summary)
}

model_SBP_temp <- fit_mixed_model("SBPbr", "five_min_temp")
model_DBP_temp <- fit_mixed_model("DBPbr", "five_min_temp")

# Print results
print("SBP - five_min_temp")
print_coefficients(model_SBP_temp)
print("DBP - five_min_temp")
print_coefficients(model_DBP_temp)

# SBP 5 Minutes
summary<-data.frame(summary(model_SBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Systolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Alternative Income (Categorical)"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary)  
estimates_SBP_sensitivity<-rbind(estimates_SBP_sensitivity, summary_sensitivity)

###
# DBP 5 Minutes
summary<-data.frame(summary(model_DBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Diastolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Alternative Income (Categorical)"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary) 
estimates_DBP_sensitivity<-rbind(estimates_DBP_sensitivity, summary_sensitivity)

####################
# SENSITIVITY #7
# uSING ALTERNATIVE CHARACTERIZATION FOR TRANSPORTATION TYPE
####################
fit_mixed_model <- function(response, predictor) {
  formula <- as.formula(paste(response, "~", predictor, "+ ns(time_trend, df = 7) + sex + age + education_level + employment_level + income1 + five_min_vm +  Five_transportmajority + weekend+ wave +Residence"))
  model <- lme(formula, random = list(IDNUMBER1 = pdDiag(form = ~ 1)), correlation = corAR1(form = ~ time | IDNUMBER1), data = YOURDATA)
  return(model)
}

print_coefficients <- function(model) {
  coef_summary <- coef(summary(model))
  print(coef_summary)
}

model_SBP_temp <- fit_mixed_model("SBPbr", "five_min_temp")
model_DBP_temp <- fit_mixed_model("DBPbr", "five_min_temp")

# Print results
print("SBP - five_min_temp")
print_coefficients(model_SBP_temp)
print("DBP - five_min_temp")
print_coefficients(model_DBP_temp)

# SBP 5 Minutes
summary<-data.frame(summary(model_SBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Systolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Transportation (Majority)"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary) 
estimates_SBP_sensitivity<-rbind(estimates_SBP_sensitivity, summary_sensitivity)

###
# DBP 5 Minutes
summary<-data.frame(summary(model_DBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Diastolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Transportation (Majority)"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary) 
estimates_DBP_sensitivity<-rbind(estimates_DBP_sensitivity, summary_sensitivity)

####################
# SENSITIVITY #8
# REMOVING ONLY PARTICIPANTS WHO HAVE HYPERTENSION AND DO NOT TAKE MEDICATION 
####################
fit_mixed_model <- function(response, predictor) {
  formula <- as.formula(paste(response, "~", predictor, "+ ns(time_trend, df = 7) + sex + age + education_level + employment_level + income1 + five_min_vm +  Five_transportmajority + weekend+ wave +Residence"))
  YOURDATA <- YOURDATA[YOURDATA$medication_hypertension != "No_medication", ]
  model <- lme(formula, random = list(IDNUMBER1 = pdDiag(form = ~ 1)), correlation = corAR1(form = ~ time | IDNUMBER1), data = YOURDATA)
  return(model)
}

print_coefficients <- function(model) {
  coef_summary <- coef(summary(model))
  print(coef_summary)
}

model_SBP_temp <- fit_mixed_model("SBPbr", "five_min_temp")
model_DBP_temp <- fit_mixed_model("DBPbr", "five_min_temp")

# Print results
print("SBP - five_min_temp")
print_coefficients(model_SBP_temp)
print("DBP - five_min_temp")
print_coefficients(model_DBP_temp)

# SBP 5 Minutes
summary<-data.frame(summary(model_SBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Systolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Removing hypertensive individuals, no medication"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary)
estimates_SBP_sensitivity<-rbind(estimates_SBP_sensitivity, summary_sensitivity)

###
# DBP 5 Minutes
summary<-data.frame(summary(model_DBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Diastolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Removing hypertensive individuals, no medication"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary)  
estimates_DBP_sensitivity<-rbind(estimates_DBP_sensitivity, summary_sensitivity)

####################
# SENSITIVITY #8
# REMOVING PARTICIPANTS WHO HAVE HYPERTENSION, BOTH TAKING OR NOT TAKING MEDICATION 
####################
fit_mixed_model <- function(response, predictor) {
  formula <- as.formula(paste(response, "~", predictor, "+ ns(time_trend, df = 7) + sex + age + education_level + employment_level + income1 + five_min_vm +  Five_transportmajority + weekend+ wave +Residence"))
  YOURDATA <- YOURDATA[!(YOURDATA$medication_hypertension %in% c("No_medication", "Regular_medication")), ]
  model <- lme(formula, random = list(IDNUMBER1 = pdDiag(form = ~ 1)), correlation = corAR1(form = ~ time | IDNUMBER1), data = YOURDATA)
  return(model)
}

print_coefficients <- function(model) {
  coef_summary <- coef(summary(model))
  print(coef_summary)
}

model_SBP_temp <- fit_mixed_model("SBPbr", "five_min_temp")
model_DBP_temp <- fit_mixed_model("DBPbr", "five_min_temp")

# Print results
print("SBP - five_min_temp")
print_coefficients(model_SBP_temp)
print("DBP - five_min_temp")
print_coefficients(model_DBP_temp)

# SBP 5 Minutes
summary<-data.frame(summary(model_SBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Systolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Removing hypertensive individuals, with & without medication"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary)  
estimates_SBP_sensitivity<-rbind(estimates_SBP_sensitivity, summary_sensitivity)

###
# DBP 5 Minutes
summary<-data.frame(summary(model_DBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Diastolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Removing hypertensive individuals, with & without medication"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary)  
estimates_DBP_sensitivity<-rbind(estimates_DBP_sensitivity, summary_sensitivity)

####################
# SENSITIVITY #9
# REMOVING ONLY PARTICIPANTS WITH HYPERTENSION TAKING MEDICATION 
####################
fit_mixed_model <- function(response, predictor) {
  formula <- as.formula(paste(response, "~", predictor, "+ ns(time_trend, df = 7) + sex + age + education_level + employment_level + income1 + five_min_vm +  Five_transportmajority + weekend+ wave +Residence"))
  YOURDATA <- YOURDATA[!(YOURDATA$medication_hypertension %in% c("Regular_medication")), ]
  model <- lme(formula, random = list(IDNUMBER1 = pdDiag(form = ~ 1)), correlation = corAR1(form = ~ time | IDNUMBER1), data = YOURDATA)
  return(model)
}

print_coefficients <- function(model) {
  coef_summary <- coef(summary(model))
  print(coef_summary)
}

model_SBP_temp <- fit_mixed_model("SBPbr", "five_min_temp")
model_DBP_temp <- fit_mixed_model("DBPbr", "five_min_temp")

# Print results
print("SBP - five_min_temp")
print_coefficients(model_SBP_temp)
print("DBP - five_min_temp")
print_coefficients(model_DBP_temp)

# SBP 5 Minutes
summary<-data.frame(summary(model_SBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Systolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Removing hypertensive individuals, with medication"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary) 
estimates_SBP_sensitivity<-rbind(estimates_SBP_sensitivity, summary_sensitivity)

###
# DBP 5 Minutes
summary<-data.frame(summary(model_DBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Diastolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Removing hypertensive individuals, with medication"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary) 
estimates_DBP_sensitivity<-rbind(estimates_DBP_sensitivity, summary_sensitivity)

####################
# SENSITIVITY #10
# INCLUDING ONLY THOSE WHO HAVE HYPERTENSION WHO ARE TAKING MEDICATION 
####################
fit_mixed_model <- function(response, predictor) {
  formula <- as.formula(paste(response, "~", predictor, "+ ns(time_trend, df = 7) + sex + age + education_level + employment_level + income1 + five_min_vm +  Five_transportmajority + weekend+ wave +Residence"))
  YOURDATA <- YOURDATA[(YOURDATA$medication_hypertension %in% c("Regular_medication")), ]
  model <- lme(formula, random = list(IDNUMBER1 = pdDiag(form = ~ 1)), correlation = corAR1(form = ~ time | IDNUMBER1), data = YOURDATA)
  return(model)
}

print_coefficients <- function(model) {
  coef_summary <- coef(summary(model))
  print(coef_summary)
}

model_SBP_temp <- fit_mixed_model("SBPbr", "five_min_temp")
model_DBP_temp <- fit_mixed_model("DBPbr", "five_min_temp")

# Print results
print("SBP - five_min_temp")
print_coefficients(model_SBP_temp)
print("DBP - five_min_temp")
print_coefficients(model_DBP_temp)

# SBP 5 Minutes
summary<-data.frame(summary(model_SBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Systolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Hypertensive individuals, with medication only"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary)  
estimates_SBP_sensitivity<-rbind(estimates_SBP_sensitivity, summary_sensitivity)

###
# DBP 5 Minutes
summary<-data.frame(summary(model_DBP_temp)[["tTable"]])
summary<-summary[2, ]
summary$"95%-CI"<-paste0(sprintf("%.2f", round(summary$Value-1.96*summary$Std.Error, 2)), ", ",
                         sprintf("%.2f", round(summary$Value +1.96*summary$Std.Error, 2)))
outcome <- "Diastolic Blood Pressure"
exposure <- "5 Min"
sensitivity <-"Hypertensive individuals, with medication only"
summary_sensitivity <- cbind(outcome, exposure, sensitivity, summary) 
estimates_DBP_sensitivity<-rbind(estimates_DBP_sensitivity, summary_sensitivity)

write.csv(estimates_DBP_sensitivity, file = "YOUR DBP SENSITIVITY FILE LOCATION HERE .csv", row.names = FALSE)
write.csv(estimates_SBP_sensitivity, file = "YOUR SBP SENSITIVITY FILE LOCATION HERE .csv", row.names = FALSE)
