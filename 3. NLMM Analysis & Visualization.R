# MOBILISENSE ANALYSIS 
# NLMM ANALYSIS (LINE 17) & VISUALIZATION (LINE 157)
# PUBLISHED ON 17/09/2025
# Code written by Anais Teyton

library(nlme) 
library(splines)
library(effects)
library(ggplot2)
library(ggpubr)

# Note: Input data (shown in script as YOURDATA)
# Note: 5 minute model shown only; simply replace model variables for other time windows (i.e., 10/ 15/ 30 min, or 1/ 2 hours)



###########################################
#AVERAGE TEMPERATURE MODELS
###########################################
#5 MINUTE SBP 
###########################################
#Examining nonlinear structure (ex. ns/ poly, df's, etc.) 
results <- data.frame(Df = numeric(0), AIC = numeric(0))
for(df_new in 2:10) {
  formula_nl <- formula(paste("SBPbr ~ ns(five_min_temp, df =", df_new, ") + ns(time_trend, df = 7) + sex + age + education_level + employment_level + income_household_per_member + five_min_vm + Five_domicile + Five_motorized + Five_nonMotorized + weekend + wave + Residence"))
  model_ns_i <- lme(formula_nl, random = list(IDNUMBER1 = pdDiag(form = ~ 1)), 
                    correlation = corAR1(form = ~ time | IDNUMBER1), data = YOURDATA)
  aic_i <- AIC(model_ns_i)
  results <- rbind(results, data.frame(Df = df_new, AIC = aic_i))
  cat(paste("AIC for model with df =", df_new, "is", round(aic_i, 2), "\n"))
}

# Running model
formula_nl <- formula("SBPbr ~ ns(five_min_temp, df = 4) + ns(time_trend, df = 7) + sex + age + education_level + employment_level+ income_household_per_member + five_min_vm + Five_domicile + Five_motorized + Five_nonMotorized + weekend + wave + Residence")
model_ns <- lme(formula_nl, random = list(IDNUMBER1 = pdDiag(form = ~ 1)), correlation = corAR1(form = ~ time | IDNUMBER1), data = YOURDATA)
quantile1<-unname(quantile(YOURDATA[, "five_min_temp"], probs=0.01))
quantile99<-unname(quantile(YOURDATA[, "five_min_temp"], probs=0.99))
seqx<-seq.int(quantile1,quantile99, 0.1)
eff.tmp <- Effect("five_min_temp", model_ns, xlevels=list(five_min_temp=seqx))

# Creating plot
df.plot = data.frame(Temp = eff.tmp$variables$five_min_temp$levels, SBP = eff.tmp$fit, se = eff.tmp$se)

five_SBP_NL<- ggplot(df.plot, aes(x = Temp, y = SBP)) +
geom_line(aes(x=Temp, y= SBP), color="black", size=1) +
geom_ribbon(aes(ymin=SBP - se*1.96, ymax=SBP + se*1.96), colour=NA, alpha=0.3) +
  scale_x_continuous(limits = c(10,32)) +
  scale_y_continuous(limits = c(115,130)) +
  labs(y="Systolic Blood Pressure",x="Five Minute Average Temperature")+
  theme_bw()
five_SBP_NL
ggsave("SAVE PNG LOCATION HERE.png")

#############################################
#5 MINUTE DBP 
###########################################
#Examining nonlinear structure (ex. ns/ poly, df's, etc.) 
results <- data.frame(Df = numeric(0), AIC = numeric(0))
for(df_new in 2:10) {
  formula_nl <- formula(paste("DBPbr ~ ns(five_min_temp, df =", df_new, ") + ns(time_trend, df = 7) + sex + age + education_level + employment_level + income_household_per_member + five_min_vm + Five_domicile + Five_motorized + Five_nonMotorized + weekend + wave + Residence"))
  model_ns_i <- lme(formula_nl, random = list(IDNUMBER1 = pdDiag(form = ~ 1)), correlation = corAR1(form = ~ time | IDNUMBER1), data = YOURDATA)
  aic_i <- AIC(model_ns_i)
  results <- rbind(results, data.frame(Df = df_new, AIC = aic_i))
  cat(paste("AIC for model with df =", df_new, "is", round(aic_i, 2), "\n"))
}

# Running model
formula_nl <- formula("DBPbr ~ ns(five_min_temp, df = 2) + ns(time_trend, df = 7) + sex + age + education_level + employment_level + income_household_per_member + five_min_vm + Five_domicile + Five_motorized + Five_nonMotorized + weekend + wave + Residence")
model_ns <- lme(formula_nl, random = list(IDNUMBER1 = pdDiag(form = ~ 1)), correlation = corAR1(form = ~ time | IDNUMBER1), data = YOURDATA)
quantile1<-unname(quantile(YOURDATA[, "five_min_temp"], probs=0.01))
quantile99<-unname(quantile(YOURDATA[, "five_min_temp"], probs=0.99))
seqx<-seq.int(quantile1,quantile99, 0.1)
eff.tmp <- Effect("five_min_temp", model_ns, xlevels=list(five_min_temp=seqx))

# Creating plot
df.plot = data.frame(Temp = eff.tmp$variables$five_min_temp$levels, DBP = eff.tmp$fit, se = eff.tmp$se)

five_DBP_NL<- ggplot(df.plot, aes(x = Temp, y = DBP)) +
  geom_line(aes(x=Temp, y= DBP), color="black", size=1) +
  geom_ribbon(aes(ymin=DBP - se*1.96, ymax=DBP + se*1.96), colour=NA, alpha=0.3) +
  scale_x_continuous(limits = c(10,32)) +
  scale_y_continuous(limits = c(65,75)) +
  labs(y="Diastolic Blood Pressure",x="Five Minute Average Temperature")+
  theme_bw()
five_DBP_NL
ggsave("SAVE PNG LOCATION HERE.png")

###########################################
#TEMPERATURE VARIABILITY MODELS
###########################################
#5 MINUTE SBP NLMM
###########################################
#Examining nonlinear structure (ex. ns/ poly, df's, etc.) 
results <- data.frame(Df = numeric(0), AIC = numeric(0))
for(df_new in 2:10) {
  formula_nl <- formula(paste("SBPbr ~ ns(five_min_temp_sddir, df =", df_new, ") + ns(time_trend, df = 7) + sex + age + education_level + employment_level + income_household_per_member + five_min_vm + Five_domicile + Five_motorized + Five_nonMotorized + weekend + wave + Residence"))
  model_ns_i <- lme(formula_nl, random = list(IDNUMBER1 = pdDiag(form = ~ 1)), 
                    correlation = corAR1(form = ~ time | IDNUMBER1), data = YOURDATA)
  aic_i <- AIC(model_ns_i)
  results <- rbind(results, data.frame(Df = df_new, AIC = aic_i))
  cat(paste("AIC for model with df =", df_new, "is", round(aic_i, 2), "\n"))
}

# Running model
formula_nl <- formula("SBPbr ~ ns(five_min_temp_sddir, df = 3) + ns(time_trend, df = 7) + sex + age + education_level + employment_level+ income_household_per_member + five_min_vm + Five_domicile + Five_motorized + Five_nonMotorized + weekend + wave + Residence")
model_ns <- lme(formula_nl, random = list(IDNUMBER1 = pdDiag(form = ~ 1)), correlation = corAR1(form = ~ time | IDNUMBER1), data = YOURDATA)
quantile1<-unname(quantile(YOURDATA[, "five_min_temp_sddir"], probs=0.01))
quantile99<-unname(quantile(YOURDATA[, "five_min_temp_sddir"], probs=0.99))
seqx<-seq.int(quantile1,quantile99, 0.1)
eff.tmp <- Effect("five_min_temp_sddir", model_ns, xlevels=list(five_min_temp_sddir=seqx))

# Creating plot
df.plot = data.frame(Temp = eff.tmp$variables$five_min_temp_sddir$levels, SBP = eff.tmp$fit, se = eff.tmp$se)

five_SBP_NL_SD<- ggplot(df.plot, aes(x = Temp, y = SBP)) +
  geom_line(aes(x=Temp, y= SBP), color="black", size=1) +
  geom_ribbon(aes(ymin=SBP - se*1.96, ymax=SBP + se*1.96), colour=NA, alpha=0.3) +
  labs(y="Systolic Blood Pressure",x="Five Minute Temperature Variability")+
  theme_bw()
five_SBP_NL_SD
ggsave("SAVE PNG LOCATION HERE.png")

#############################################
#5 MINUTE DBP NLMM
###########################################
#Examining nonlinear structure (ex. ns/ poly, df's, etc.) 
results <- data.frame(Df = numeric(0), AIC = numeric(0))
for(df_new in 2:10) {
  formula_nl <- formula(paste("DBPbr ~ ns(five_min_temp_sddir, df =", df_new, ") + ns(time_trend, df = 7) + sex + age + education_level + employment_level + income_household_per_member + five_min_vm + Five_domicile + Five_motorized + Five_nonMotorized + weekend + wave + Residence"))
  model_ns_i <- lme(formula_nl, random = list(IDNUMBER1 = pdDiag(form = ~ 1)), correlation = corAR1(form = ~ time | IDNUMBER1), data = YOURDATA)
  aic_i <- AIC(model_ns_i)
  results <- rbind(results, data.frame(Df = df_new, AIC = aic_i))
  cat(paste("AIC for model with df =", df_new, "is", round(aic_i, 2), "\n"))
}

# Running model
formula_nl <- formula("DBPbr ~ ns(five_min_temp_sddir, df = 3) + ns(time_trend, df = 7) + sex + age + education_level + employment_level + income_household_per_member + five_min_vm + Five_domicile + Five_motorized + Five_nonMotorized + weekend + wave + Residence")
model_ns <- lme(formula_nl, random = list(IDNUMBER1 = pdDiag(form = ~ 1)), correlation = corAR1(form = ~ time | IDNUMBER1), data = YOURDATA)
quantile1<-unname(quantile(YOURDATA[, "five_min_temp_sddir"], probs=0.01))
quantile99<-unname(quantile(YOURDATA[, "five_min_temp_sddir"], probs=0.99))
seqx<-seq.int(quantile1,quantile99, 0.1)
eff.tmp <- Effect("five_min_temp_sddir", model_ns, xlevels=list(five_min_temp_sddir=seqx))

# Creating plot
df.plot = data.frame(Temp = eff.tmp$variables$five_min_temp_sddir$levels, DBP = eff.tmp$fit, se = eff.tmp$se)

five_DBP_NL_SD<- ggplot(df.plot, aes(x = Temp, y = DBP)) +
  geom_line(aes(x=Temp, y= DBP), color="black", size=1) +
  geom_ribbon(aes(ymin=DBP - se*1.96, ymax=DBP + se*1.96), colour=NA, alpha=0.3) +
  labs(y="Diastolic Blood Pressure",x="Five Minute Temperature Variability")+
  theme_bw()
five_DBP_NL_SD
ggsave("SAVE PNG LOCATION HERE.png")



###########################################
# COMPILED VISUALIZATION (ONCE ANALYSES FOR EACH TIME WINDOW ARE RUN)
###########################################
nlmm_sd_results1<-ggarrange(five_SBP_NL_SD, ten_SBP_NL_SD, 
                         fifteen_SBP_NL_SD, thirty_SBP_NL_SD,
                         onehr_SBP_NL_SD,twohour_SBP_NL_SD,
                         labels=c("A","B","C","D","E","F"),
                         ncol=2,nrow=3)
nlmm_sd_results1

nlmm_sd_results2<-ggarrange(five_DBP_NL_SD, ten_DBP_NL_SD,
                         fifteen_DBP_NL_SD, thirty_DBP_NL_SD,
                         onehr_DBP_NL_SD, twohr_DBP_NL_SD,
                         labels=c("A","B","C","D","E","F"),
                         ncol=2,nrow=3)
nlmm_sd_results2

