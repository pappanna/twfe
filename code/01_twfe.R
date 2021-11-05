#**************************************************************
# ** TWFE / DiD Literature Exploration
# ** Based on Goodman-Bacon (2021), Sun and Abraham (2020), Callaway and Sant'Anna (2021), Borusyak et al. (2021)
#** Other coding sources: 
#**** https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html
#**** https://cran.r-project.org/web/packages/bacondecomp/vignettes/bacon.html
#**** https://causalinf.substack.com/p/callaway-and-santanna-dd-estimator
#**** https://rdrr.io/cran/didimputation/man/did_imputation.html
#**************************************************************
#** ap3907@columbia.edu - November 2021
#** Last modified: 11/05/21
#**************************************************************

################## 00 - Setup ########################################

# clear
rm(list = ls())

# themes
theme_set(theme_bw())

# set working directory for project
setwd("/Users/annapapp/Desktop/twfe")

# libraries 
#install.packages("bacondecomp")
#install.packages("fixest")
#install.packages("did")
#install.packages("didimputation")
#install.packages("did2s")
library(dplyr)
library(data.table)
library(ggplot2)
library(bacondecomp) # bacon decomp package
library(fixest) # Sun and Abraham package
library(did) # Callaway and Sant'anna package
library(didimputation) # Borusyak et al package 
library(did2s)

#### file and chart names ####
# Stevenson and Wolfers (2006): stevensonwolfers2006
# Cheng and Hoekstra (2013): chenghoekstra2013
filename <- "stevensonwolfers2006"
#filename <- "chenghoekstra2013"

#### variable names ####
# for Stevenson and Wolfers 2006:
outcomevar <- "asmrs"
postvar <- "post"
unitvar <- "stfips"
timevar <- "year"
timetreatvar <- "X_nfd"
# for Cheng and Hoekstra 2013:
#outcomevar <- "l_homicide"
#postvar <- "post"
#unitvar <- "sid"
#timevar <- "year"
#timetreatvar <- "effyear"

################## 01 - TWFE #########################################

data <- read.csv(file=paste("data/", filename, ".csv", sep=""))
data <- data.table(data)

# twfe model 
fit_tw <- lm(as.formula(paste(outcomevar, "~", postvar, "+ factor(", unitvar, ") + factor(",timevar,")")), 
             data = data)
coef_twfe <- as.numeric(fit_tw$coefficients[2])
se_twfe <- as.numeric(sqrt(diag(vcov(fit_tw)))[2])
print(paste("Two-way FE estimate =", round(coef_twfe, 4)))

################## 02 - Bacon Decomp (2021) ##########################

data <- read.csv(file=paste("data/", filename, ".csv", sep=""))
data <- data.table(data)

# run bacon decomp 
df_bacon <- bacon(as.formula(paste(outcomevar, "~", postvar)),
                  data = data,
                  id_var = unitvar,
                  time_var = timevar)

# can check that this is equal to TWFE estimate
coef_bacon <- sum(df_bacon$estimate * df_bacon$weight)
print(paste("Weighted sum of decomposition =", round(coef_bacon, 4)))

# plot decomposition 
ggplot(df_bacon) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  geom_point() + 
  ggtitle("Bacon Decomposition") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
ggsave(file=paste("output/", filename, "_bacondecomp_R.png", sep=""))

################## 03 - Event Study ###################################

data <- read.csv(file=paste("data/", filename, ".csv", sep=""))
data <- data.table(data)

# indicator of which states received treatment
data <- rename(data, timetreatvar = paste("",timetreatvar,"", sep=""))
data[, treat := ifelse(is.na(`timetreatvar`), 0, 1)]

# create a "time_to_treatment" variable for each state; for never-treated units set to 0
data[, time_to_treat := ifelse(treat==1, year - `timetreatvar`, 0)]

# run fe ols 
mod_twfe = feols(as.formula(paste(outcomevar, " ~ i(time_to_treat, treat, ref = -1) | ",
                              unitvar, "+", timevar)), cluster = as.formula(paste("~", unitvar)), data = data)

# plot TWFE event study
png(file=paste("output/", filename, "_eventstudy_R.png", sep=""),width=900, height=550)
iplot(mod_twfe, 
      xlab = 'Time to Treatment',
      main = 'Event Study: Staggered Treatment (TWFE)')
dev.off()

################## 04 - Event Study, Sun and Abraham (2021) ###########

data <- read.csv(file=paste("data/", filename, ".csv", sep=""))
data <- data.table(data)

# indicator of which states received treatment
data <- rename(data, timetreatvar = paste("",timetreatvar,"", sep=""))
data[, treat := ifelse(is.na(`timetreatvar`), 0, 1)]

# create a "time_to_treatment" variable for each state; for never-treated units set to 0
data[, time_to_treat := ifelse(treat==1, year - `timetreatvar`, 0)]

# run fe ols 
mod_twfe = feols(as.formula(paste(outcomevar, " ~ i(time_to_treat, treat, ref = -1) | ",
                                  unitvar, "+", timevar)), 
                 cluster = as.formula(paste("~", unitvar)), 
                 data = data)

# give never-treated units a fake "treatment" date far outside the relevant study period.
data[, year_treated := ifelse(treat==0, 10000, `timetreatvar`)]

# now run Sun and Abraham method model 
mod_sa = feols(as.formula(paste(outcomevar, "~ sunab(year_treated,", timevar,") |", unitvar, "+", timevar)),
               cluster = as.formula(paste("~", unitvar)), 
               data = data)

# plot with event-study 
png(file=paste("output/", filename, "_eventstudy_sunabraham_R.png", sep=""),width=900, height=550)
iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1, 
      xlab = 'Time to Treatment',
      main = 'Event Study: Staggered Treatment')
        legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
        legend = c("TWFE", "Sun & Abraham (2021)"))
dev.off()

################## 05 - Callaway and Sant'anna (2021) #######################

data <- read.csv(file=paste("data/", filename, ".csv", sep=""))
data <- data.table(data)

# set untreated units to have effective year of 0
data <- rename(data, timetreatvar = paste("",timetreatvar,"", sep=""))
data$timetreatvar[is.na(data$timetreatvar)] <- 0 

# estimate cs, first with never treated control 
atts_never <- att_gt(yname = outcomevar, # LHS variable
               tname = timevar, # time variable
               idname = unitvar, # id variable
               gname = "timetreatvar", # first treatment period variable
               data = data, # data
               xformla = NULL, # no covariates
               #xformla = ~ , # with covariates
               est_method = "drimp", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "nevertreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 1000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = unitvar, # cluster level
               panel = TRUE) # whether the data is panel or repeated cross-sectional
# now with not yet treated 
atts_notyet <- att_gt(yname = outcomevar, # LHS variable
               tname = timevar, # time variable
               idname = unitvar, # id variable
               gname = "timetreatvar", # first treatment period variable
               data = data, # data
               xformla = NULL, # no covariates
               #xformla = ~ , # with covariates
               est_method = "drimp", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "notyettreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 1000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = unitvar, # cluster level
               panel = TRUE)  # whether the data is panel or repeated cross-sectional
# aggregate ATT
agg_effects_never <- aggte(atts_never, type = "group")
coef_cs_never <- agg_effects_never$overall.att
se_cs_never <- agg_effects_never$overall.se
agg_effects_notyet <- aggte(atts_notyet, type = "group")
coef_cs_notyet <- agg_effects_notyet$overall.att
se_cs_notyet <- agg_effects_notyet$overall.se
  
# event-study
agg_effects_es_never <- aggte(atts_never, type = "dynamic")
agg_effects_es_notyet <- aggte(atts_notyet, type = "dynamic")

# plot event-study coefficients
png(file=paste("output/", filename, "_eventstudy_cs_never_R.png", sep=""),width=900, height=550)
ggdid(agg_effects_es_never)
dev.off()
png(file=paste("output/", filename, "_eventstudy_cs_notyet_R.png", sep=""),width=900, height=550)
ggdid(agg_effects_es_notyet)
dev.off()

################## 06 - Borusyak et al. (2021) ##############################

data <- read.csv(file=paste("data/", filename, ".csv", sep=""))
data <- data.table(data)

# estimate borusyak
borusyak <- did_imputation(data = data, yname = outcomevar, gname = timetreatvar, tname = timevar, idname = unitvar,
  first_stage = NULL,
  weights = NULL,
  wtr = NULL,
  horizon = NULL,
  pretrends = NULL)
coef_borusyak <- borusyak$estimate
se_borusyak <- borusyak$std.error

# event study, having trouble getting this to work?
#borusyak_es <- did_imputation(data = data, yname = outcomevar, gname = timetreatvar, tname = timevar, idname = unitvar,horizon = TRUE)

