
# Data and libraries ----------------------------------------------------

library(car)  
library(DataCombine)  
library(plyr)  
library(dplyr)  
library(foreign) 
library(formattable)
library(gdata)  
library(ggplot2) 
library(gridExtra) 
library(haven) 
library(kableExtra) 
library(lavaan)
library(LittleHelpers) 
library(MIE) 
library(readxl) 
library(reshape2)
library(stringr) 
library(tidyr) 
source("WVS_functions.R") # own functions


# Indicators: 
## V9 - For each of the following, indicate how important it is
## in your life: Religion 
## V25 - For each organization, could you tell me whether you
## are….? Church or religious organization: active / inactive member / do not belong 
## V108 - For each one, could you tell me how much confidence you have in them. The Churches
## V144 - Do you belong to a religion or religious denomination?  
## V145 - Apart from weddings and funerals, about how often do you attend religious services 
## these days?
# V146 - Apart from weddings and funerals, about how often do you pray?  
## V147 - Would you say you are... religious / not a religious person / an atheist 
## V148 - Do you believe in God?  
## V149 - Do you believe in hell?  
## V152 - How important is God in your life?  
## V154 - Please tell us if you strongly agree...: The only acceptable religion is
## my religion


WVS6 <- read.spss(
  "WV6_Data_Spss_v20180912.sav", use.value.labels = TRUE, to.data.frame = TRUE
)
rel_data <- select(
  WVS6, c(V2, V9, V25, V108, V144:V149, V152, V154))

## Add country code
country_abb <- read_excel("Country_abb.xlsx")
rel_data <- merge(
  rel_data, country_abb[, 1:2], 
  by.x = c("V2"), by.y = c("country"),
  all.x = TRUE
)

colnames(rel_data) <- c("country", "imprel", "member", "confidence", "belong", "attend", "pray",
  "person", "bgod", "bhell", "impgod", "fundament", "code")


# Recode indicators ----------------------------------------------------

for (item in c("imprel", "member", "confidence", "attend", "pray", "person", "bgod",
  "bhell", "impgod", "fundament")) {
  rel_data[, item] <- as.numeric(rel_data[, item])
}

for (item in c("imprel", "fundament", "confidence")) {
  rel_data[, item] <- Recode(
    rel_data[, item], rec = "1=4; 2=3; 3=2; 4=1; else=NA"
)
}

for (item in c("bhell", "bgod")) {
  rel_data[, item] <- Recode(
    rel_data[, item], rec = "1=2; 2=1; else=NA")
}

rel_data$attend <- Recode(
  rel_data$attend, rec = "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; else=NA"
)
rel_data$pray <- Recode(
  rel_data$pray, rec = "1=8; 2=7; 3=6; 4=5; 5=4; 6=3; 7=2; 8=1; else=NA"
)
rel_data$person <- Recode(
  rel_data$person, rec = "1=2; 2=1; 3=1; else=NA"
)

# binary belonging
rel_data$belonging <- as.numeric(rel_data$belong)
rel_data$belonging <- ifelse(rel_data$belonging == "1", 1, 2)
rel_data <- rel_data[, -5]

rel_data$country <- droplevels(rel_data$country)

# Clean
rm(item)




# CFA ----------------------------------------------------

## Specify ordered items
ord_items <- c("person", "imprel", "confidence", "bgod", "bhell", "belonging", "member",
  "fundament")

# TABLE 15 in Supplementary Materials
## Initial two-factor model with 6 indicators 

## Drop countries with omitted questions
rel_data_full <- subset(
  rel_data, subset = !(rel_data$country %in% c("Kuwait", "Morocco", "Qatar", "Egypt"))
)
rel_data_full$country <- droplevels(rel_data_full$country)

model_initial <- "Feelings =~ imprel + person + confidence + impgod;
Practices =~ pray + attend;
"

## Fit measures - show only countries with good fit
groupwiseCFA.modified(
  model_initial, data = rel_data_full, group = "country", ordered = ord_items,
  estimator = "WLSMV", missing = "listwise", out = c("goodfit")
) # change the out argument to “fit” to see the results for all countries

## Note. There are negative correlations of indicators in:
## Algeria, Haiti, Jordan, Kyrgyzstan, Libya, Malaysia, Nigeria, Pakistan, 
## Palestine, Philippines, Rwanda, Thailand, and Yemen


# TABLE 16 in Supplementary Materials
## Model 1: one-factor model with 6 indicators

model1 <- "Religiosity =~ imprel + person + confidence + pray + attend + impgod;
pray ~~ attend;
impgod ~~ pray;
"
cfa_model1 <- groupwiseCFA.modified(
  model1, data = rel_data_full, group = "country", ordered = ord_items,
  estimator = "WLSMV", missing = "listwise", out = c("goodfit")
)

## Ignore the warnings, they do not affect the sample with acceptable model fit


# TABLE 17 in Supplementary Materials
## Model 2: one-factor model with 6 indicators

## Drop countries with omitted questions or empty response categories
rel_data_full1 <- subset(
  rel_data, subset = !(rel_data$country %in% c(
    "Kuwait", "Morocco", "Qatar", "Egypt", "Algeria", "Haiti", "Iraq", 
    "Jordan", "Lebanon", "Libya", "Pakistan", "Palestine", "Tunisia", "Yemen")))
rel_data_full1$country <- droplevels(rel_data_full1$country)

model2 <- "Religiosity =~ imprel + person + confidence + pray + attend + belonging;
pray ~~ attend;
"
cfa_model2 <- groupwiseCFA.modified(
  model2, data = rel_data_full1, group = "country", ordered = ord_items,
  estimator = "WLSMV", missing = "listwise", out = c("goodfit"))

## Note. There are negative correlations of indicators in:
##Kyrgyzstan, Malaysia, Nigeria, Philippines, Rwanda

## Ignore the warnings, they do not affect the sample with acceptable model fit
# except Poland - correlation between variables belonging and person is (nearly) 1.0


# TABLE 18 in Supplementary Materials
## Model 3: one-factor model with 7 indicators

model3 <- "Religiosity =~ imprel + person + confidence + pray + attend + belonging + bgod;
pray ~~ attend;
"
cfa_model3 <- groupwiseCFA.modified(
  model3, data = rel_data_full1, group = "country", ordered = ord_items,
  estimator = "WLSMV", missing = "listwise", out = c("goodfit"))

## Note. There are negative correlations of indicators in:
## Ghana, Kyrgyzstan, Malaysia, Nigeria, Philippines, Rwanda, Thailand, Turkey

## Ignore the warnings, they do not affect the sample with acceptable model fit


# TABLE 19 in Supplementary Materials
## Model 4: one-factor model with 8 indicators

model4 <- "Religiosity =~ imprel + person + confidence + pray + attend + belonging + bgod + member;
pray ~~ attend;
"
cfa_model4 <- groupwiseCFA.modified(
  model4, data = rel_data_full1, group = "country", ordered = ord_items,
  estimator = "WLSMV", missing = "listwise", out = c("goodfit"))

## Note. There are negative correlations of indicators in:
## Armenia, Georgia, Ghana, Kazakhstan, Kyrgyzstan, Malaysia, Nigeria, Peru, 
## Philippines, Rwanda, Thailand, Turkey, Uzbekistan, India

## Ignore the warnings, they do not affect the sample with acceptable model fit




# MGCFA ----------------------------------------------------

# Model 1

## Select countries with acceptable fit
data_model1 <- subset(
  rel_data, subset = rel_data$country %in% rownames(cfa_model1))
data_model1$country <- droplevels(data_model1$country)

mgcfa_model1 <- globalMI.modified.ord(
  model1, data = data_model1, group = "country", ordered = ord_items,
  estimator = "WLSMV", missing = "listwise")

# Model 2

## Select countries with acceptable fit
## Drop Poland - correlation between variables belonging and person is (nearly) 1.0
data_model2 <- subset(
  rel_data, subset = rel_data$country %in% rownames(cfa_model2)[-17])
data_model2$country <- droplevels(data_model2$country)

mgcfa_model2 <- globalMI.modified.ord(
  model2, data = data_model2, group = "country", ordered = ord_items,
  estimator = "WLSMV", missing = "listwise")

# Model 3

## Select countries with acceptable fit
## Drop Poland and Ghana - negative correlations of indicators
data_model3 <- subset(
  rel_data, subset = rel_data$country %in% rownames(cfa_model3)[-c(9, 14)])
data_model3$country <- droplevels(data_model3$country)

mgcfa_model3 <- globalMI.modified.ord(
  model3, data = data_model3, group = "country", ordered = ord_items,
  estimator = "WLSMV", missing = "listwise")

# Model 4

## Select countries with acceptable fit
## Drop Ghana - negative correlations of indicators
data_model4 <- subset(
  rel_data, subset = rel_data$country %in% rownames(cfa_model4)[-7])
data_model4$country <- droplevels(data_model4$country)

mgcfa_model4 <- globalMI.modified.ord(
  model4, data = data_model4, group = "country", ordered = ord_items,
  estimator = "WLSMV", missing = "listwise")

## TABLE 1
table_mgcfa <- rbind(mgcfa_model1, mgcfa_model2, mgcfa_model3, mgcfa_model4)
colnames(table_mgcfa) <- c(
  "Model", "CFI", "Δ CFI", "RMSEA", "Δ RMSEA", "χ2", "df", "SRMR")
table_mgcfa[is.na(table_mgcfa)] <- "--"

kable(table_mgcfa) %>%
    group_rows("Model 1 (30 countries)", 1, 2) %>%
    group_rows("Model 2 (28 countries)", 3, 4) %>%
    group_rows("Model 3 (24 countries)", 5, 6) %>%
    group_rows("Model 4 (20 countries)", 7, 8) %>%
    footnote(general = "CFI = comparative fit index, 
                        RMSEA = root mean square error of approximation, 
                        χ2 = chi-square, df = degrees of freedom, 
                        SRMR = standardized root mean residual.
                        The list of countries for each model could be found in 
                        Table 20 in the Supplementary Materials.")



# Pairwise comparisons ----------------------------------------------------

## scalar and configural models Δ cutoff fit measures - the sum of Δ metric and Δ scalar
## Rutkowski and Svetina, 2017: Δ CFI 0.008; Δ RMSEA 0.06 - very large, not relevant

# Model 1

incrfit_model1 <- incrementalFit(
  "Religiosity =~ imprel + person + confidence + pray + attend + impgod;
   pray ~~ attend;
   impgod ~~ pray;",
  data_model1, missing = "listwise", ordered = ord_items, estimator = "WLSMV", 
  group = "country", level = "at.once")

plotCutoff.modified(incrfit_model1, fit.index = "cfi.scaled", cutoff = 0.008)

## countries with scalar invariance:
## New Zealand + Australia, Australia + USA, Germany + Australia

# Model 2

incrfit_model2 <- incrementalFit(
  "Religiosity =~ imprel + person + confidence + pray + attend + belonging;
   pray ~~ attend;",
  data_model2, missing = "listwise", ordered = ord_items, estimator = "WLSMV", 
  group = "country", level = "at.once")

plotCutoff.modified(incrfit_model2, fit.index = "cfi.scaled", cutoff = 0.008)

## countries with scalar invariance:
## South Africa + Zimbabwe, USA + Peru, Australia + USA, Peru + Netherlands Australia +
## Netherlands + Germany, Australia + New Zealand + Germany


# Model 3

incrfit_model3 <- incrementalFit(
  "Religiosity =~ imprel + person + confidence + pray + attend + belonging + bgod;
   pray ~~ attend;",
  data_model3, missing = "listwise", ordered = ord_items, estimator = "WLSMV", 
  group = "country", level = "at.once")

plotCutoff.modified(incrfit_model3, fit.index = "cfi.scaled", cutoff = 0.008)

## countries with scalar invariance:
## Russia + Ukraine, USA + Uruguay, USA + Australia + New Zealand 
## New Zealand + Australia + Germany, Germany + Argentina


# Model 4

incrfit_model4 <- incrementalFit(
  "Religiosity =~ imprel + person + confidence + pray + attend + belonging + bgod + member;
   pray ~~ attend;",
  data_model4, missing = "listwise", ordered = ord_items, estimator = "WLSMV", 
  group = "country", level = "at.once")

plotCutoff.modified(incrfit_model4, fit.index = "cfi.scaled", cutoff = 0.008)

## countries with scalar invariance:
## Russia + Ukraine, USA + Uruguay, New Zealand + USA




# Alignment ----------------------------------------------------

## Create folders for each model
model_names <- c("Model 1", "Model 2", "Model 3", "Model 4")
for (i in 1:length(model_names)) {
  directory <- paste0("add your path to the folder here/", model_names[i])
  folder <- dir.create(directory)
}


# Model 1

setwd("add your path to the folder here/Model 1")
runAlignment.modified(
  "Religiosity BY imprel confidence pray attend person impgod;
  pray WITH attend;
  impgod WITH pray;",
  group = "code", dat = data_model1, estim = "mlr", 
  categorical = c("person", "imprel", "confidence"),
  sim.samples = c(100, 500, 1000, 1500), sim.reps = 500, 
  Mplus_com = "/Applications/Mplus/mplus", 
  summaries = FALSE
)

## extract alignment results
align_model1 <- extractAlignment("fixed.out", silent = TRUE)

## extract standard errors
se_model1 <- extractSE.WVS("fixed.out")
se_model1 <- as.data.frame(
  cbind(
    unique(data_model1$code),
    se_model1
    )
)

## extract simulations results
sim_model1 <- extractAlignmentSim(
  c("sim100.out", "sim500.out", "sim1000.out", "sim1500.out"),
  silent = TRUE
)

# Model 2

setwd("add your path to the folder here/Model 2")
runAlignment.modified(
  "Religiosity BY imprel person confidence pray attend belonging;
   pray WITH attend;",
  group = "code", dat = data_model2, estim = "mlr", 
  categorical = c("person", "imprel", "confidence", "belonging"),
  sim.samples = c(100, 500, 1000, 1500), sim.reps = 500, 
  Mplus_com = "/Applications/Mplus/mplus", summaries = FALSE
)

## Do not extract alignment results due to the low correlation of estimated and 
## simulated factor means

## extract simulations results
sim_model2 <- extractAlignmentSim(
  c("sim100.out", "sim500.out", "sim1000.out", "sim1500.out"),
  silent = TRUE
)

# Model 3

setwd("add your path to the folder here/Model 3")
runAlignment.modified(
  "Religiosity BY imprel person confidence pray attend belonging bgod;
   pray WITH attend;",
  group = "code", dat = data_model3, estim = "mlr", 
  categorical = c("person", "imprel", "confidence", "belonging", "bgod"),
  sim.samples = c(100, 500, 1000, 1500), sim.reps = 500, 
  Mplus_com = "/Applications/Mplus/mplus", summaries = FALSE
)

## Do not extract alignment results due to the low correlation of estimated and 
## simulated factor means

## extract simulations results
sim_model3 <- extractAlignmentSim(
  c("sim100.out", "sim500.out", "sim1000.out", "sim1500.out"),
  silent = TRUE
)

# Model 4

setwd("add your path to the folder here/Model 4")
runAlignment.modified(
  "Religiosity BY imprel person confidence pray attend belonging bgod member;
   pray WITH attend",
  group = "code", dat = data_model4, estim = "mlr", 
  categorical = c("person", "imprel", "confidence", "belonging", "bgod", "member"),
  sim.samples = c(100, 500, 1000, 1500), sim.reps = 500, 
  Mplus_com = "/Applications/Mplus/mplus", summaries = FALSE
)

## extract alignment results
align_model4 <- extractAlignment("fixed.out", silent = TRUE)

## extract standard errors
se_model4 <- extractSE.WVS("fixed.out")
se_model4 <- as.data.frame(
  cbind(
    unique(data_model4$code),
    se_model4
    )
)

## extract simulations results
sim_model4 <- extractAlignmentSim(
  c("sim100.out", "sim500.out", "sim1000.out", "sim1500.out"),
  silent = TRUE
)


# See simulation results
sim_models_list <- list(sim_model1, sim_model2, sim_model3, sim_model4)
for (i in 1:length(sim_models_list)) {
  sim_models_list[[i]] <- cbind(
    sim_models_list[[i]][[1]][[1]][[1]], sim_models_list[[i]][[2]][[1]][[1]],
    sim_models_list[[i]][[3]][[1]][[1]], sim_models_list[[i]][[4]][[1]][[1]]
  )
  sim_models_list[[i]] <- round(sim_models_list[[i]], 3)
}

table.sim <- cbind(
  model_names, rbind(
    sim_models_list[[1]], sim_models_list[[2]], sim_models_list[[3]],
    sim_models_list[[4]]
  )
)
colnames(table.sim) <- c("Model", "100", "500", "1000", "1500")
kable(table.sim) %>%
  add_header_above(c(` ` = 1, `Number of observations per group` = 4))



# Reduce samples for Model 2 and Model 3
data_model2_red <- subset(
  data_model2, subset = !(data_model2$country %in% c("Ghana", "Turkey"))
)
data_model2_red$country <- droplevels(data_model2_red$country)

data_model3_red <- subset(
  data_model3, subset = !(data_model3$country %in% c("Trinidad and Tobago"))
)
data_model3_red$country <- droplevels(data_model3_red$country)


model_names_red <- c("Model 2 reduced", "Model 3 reduced")
for (i in 1:length(model_names_red)) {
  directory <- paste0("add your path to the folder here/", model_names_red[i])
  folder <- dir.create(directory)
}

# Model 2 reduced
setwd("add your path to the folder here/Model 2 reduced")
runAlignment.modified(
  "Religiosity BY imprel person confidence pray attend belonging;
   pray WITH attend;",
  group = "code", dat = data_model2_red, estim = "mlr", 
  categorical = c("person", "imprel", "confidence", "belonging"),
  sim.samples = c(100, 500, 1000, 1500), sim.reps = 500, 
  Mplus_com = "/Applications/Mplus/mplus", summaries = FALSE
)

## extract alignment results
align_model2_red <- extractAlignment("fixed.out", silent = TRUE)

## extract standard errors
se_model2 <- extractSE.WVS("fixed.out")
se_model2 <- as.data.frame(
  cbind(
    unique(data_model2_red$code),
    se_model2
  )
)

## extract simulations results
sim_model2_red <- extractAlignmentSim(
  c("sim100.out", "sim500.out", "sim1000.out", "sim1500.out"),
  silent = TRUE
)

# Model 3 reduced
setwd("add your path to the folder here/Model 3 reduced")
runAlignment.modified(
  "Religiosity BY imprel person confidence pray attend belonging bgod;
   pray WITH attend;",
  group = "code", dat = data_model3_red, estim = "mlr", 
  categorical = c("person", "imprel", "confidence", "belonging", "bgod"),
  sim.samples = c(100, 500, 1000, 1500), sim.reps = 500, 
  Mplus_com = "/Applications/Mplus/mplus", summaries = FALSE
)

## extract alignment results
align_model3_red <- extractAlignment("fixed.out", silent = TRUE)

## extract standard errors
se_model3 <- extractSE.WVS("fixed.out")
se_model3 <- as.data.frame(
  cbind(
    unique(data_model3_red$code),
    se_model3
  )
)

## extract simulations results
sim_model3_red <- extractAlignmentSim(
  c("sim100.out", "sim500.out", "sim1000.out", "sim1500.out"),
  silent = TRUE
)



# TABLE 2

sim_models_list_red <- list(sim_model1, sim_model2_red, sim_model3_red, sim_model4)
for (i in 1:length(sim_models_list_red)) {
  sim_models_list_red[[i]] <- cbind(
    sim_models_list_red[[i]][[1]][[1]][[1]], sim_models_list_red[[i]][[2]][[1]][[1]],
    sim_models_list_red[[i]][[3]][[1]][[1]], sim_models_list_red[[i]][[4]][[1]][[1]]
  )
  sim_models_list_red[[i]] <- round(sim_models_list_red[[i]], 3)
}

table_sim_red <- cbind(
  c("Model 1 (30 countries)", "Model 2  (26 countries)", "Model 3  (23 countries)",
    "Model 4  (20 countries)"),
  rbind(
    sim_models_list_red[[1]], sim_models_list_red[[2]], sim_models_list_red[[3]],
    sim_models_list_red[[4]]
  )
)
colnames(table_sim_red) <- c("Model", "100", "500", "1000", "1500")
kable(table_sim_red) %>%
  add_header_above(c(` ` = 1, `Number of observations per group` = 4))



# Make abbreviations for country codes in alignment tables
align_models_list <- list(align_model1, align_model2_red, align_model3_red, align_model4)
for (i in 1:length(align_models_list)) {
  align_models_list[[i]] <- align_models_list[[i]][[3]][, c(1, 3, 6, 4, 5)]
  colnames(align_models_list[[i]]) <- c(
    "", "R-Square", "Fit Contribution", "Invariant Groups", "Non-invariant Groups"
  )
  align_models_list[[i]] <- FindReplace(
    data = align_models_list[[i]], Var = "Invariant Groups", 
    replaceData = country_abb,
    from = "code", to = "abbreviation", 
    exact = FALSE, vector = FALSE
  )
  align_models_list[[i]] <- FindReplace(
    data = align_models_list[[i]], Var = "Non-invariant Groups", 
    replaceData = country_abb,
    from = "code", to = "abbreviation", 
    exact = FALSE, vector = FALSE
  )
}


# TABLE 24 in Supplementary Materials
## Share of non-invariant countries
share_ni <- align_models_list
for (i in 1:length(share_ni)) {
  ni.countries <- sapply(share_ni[[i]][5], function(x) strsplit(x, " ")) %>%
    lapply(., length) %>%
    do.call("rbind", .)
  
  total.countries <- sapply(share_ni[[i]][4], function(x) strsplit(x, " ")) %>%
    lapply(., length) %>%
    do.call("rbind", .)
  total.countries <- sum(ni.countries[1] + total.countries[1])
  
  share_ni[[i]] <- cbind(share_ni[[i]][1], ni.countries)
  share_ni[[i]][2] <- round(share_ni[[i]][2]/total.countries, 2)
  
  names(share_ni[[i]])[names(share_ni[[i]]) == "Var.1"] <- "Parameter"
  names(share_ni[[i]])[names(share_ni[[i]]) == "ni.countries"] <- model_names[i]
}

share_ni <- Reduce(
  function(x, y) merge(x, y, by = "Parameter", all = TRUE),
  share_ni
)


## mean across models for each parameter
share_ni$Mean <- round(
  rowMeans(share_ni[, 2:5], na.rm = TRUE), 
  2
)

share_ni[nrow(share_ni) +  7, ] <- NA
share_ni[24:26, 1] <- "Total"
share_ni[27, 1] <- "Mean"


## mean across models - mean threshold of confidence, importance of religion,
## and membership separately
share_ni[28, 1] <- "Threshold CONFIDEN total"
share_ni[28, 2:6] <- round(
  colMeans(share_ni[15:17, 2:6], na.rm = TRUE),
  2
)

share_ni[29, 1] <- "Threshold IMPREL total"
share_ni[29, 2:6] <- round(
  colMeans(share_ni[18:20, 2:6], na.rm = TRUE),
  2
)

share_ni[30, 1] <- "Threshold MEMBER total"
share_ni[30, 2:6] <- round(
  colMeans(share_ni[21:22, 2:6], na.rm = TRUE),
  2
)

## mean across models - intercepts
share_ni[24, 6] <- round(
  mean(share_ni[1:3, 6]),
  2
)

## mean across models - loadings
share_ni[25, 6] <- round(
  mean(share_ni[4:12, 6]),
  2
)

## mean across models -  thresholds
share_ni[26, 6] <- round(
  mean(share_ni[c(13:14, 28:30, 23), 6]),
  2
)


## mean across indicators for each model
share_ni[27, 2:5] <- round(
  colMeans(share_ni[c(1:14, 23, 28:30), 2:5], na.rm = TRUE),
  2
)


share_ni <- rbind(
  share_ni[1:3, ], share_ni[24, ], share_ni[4:12, ], share_ni[25, ],
  share_ni[13:17, ], share_ni[28, ], share_ni[18:20, ], share_ni[29, ], 
  share_ni[21:22, ], share_ni[30, ], share_ni[23, ],
  share_ni[26, ], share_ni[27, ]
)



# TABLE 25 in Supplementary Materials
## R-square
rsquare <- align_models_list
for (i in 1:length(rsquare)) {
  rsquare[[i]] <- rsquare[[i]][, 1:2]
  names(rsquare[[i]])[names(rsquare[[i]]) == "R-Square"] <- model_names[i]
  names(rsquare[[i]])[1] <- "Parameter"
  rsquare[[i]][, 2] <- as.numeric(as.character(rsquare[[i]][, 2]))
}
rsquare <- Reduce(
  function(x, y) merge(x, y, by = "Parameter", all = TRUE),
  rsquare
)

## mean across models for each parameter
rsquare$Mean <- round(rowMeans(rsquare[, 2:5], na.rm = TRUE), 3)

rsquare[nrow(rsquare) +  7, ] <- NA
rsquare[24:26, 1] <- "Total"
rsquare[27, 1] <- "Mean"


## mean across models - mean threshold of confidence, importance of religion,
## and membership separately
rsquare[28, 1] <- "Threshold CONFIDEN total"
rsquare[28, 2:6] <- round(
  colMeans(rsquare[15:17, 2:6], na.rm = TRUE),
  3
)

rsquare[29, 1] <- "Threshold IMPREL total"
rsquare[29, 2:6] <- round(
  colMeans(rsquare[18:20, 2:6], na.rm = TRUE),
  3
)

rsquare[30, 1] <- "Threshold MEMBER total"
rsquare[30, 2:6] <- round(
  colMeans(rsquare[21:22, 2:6], na.rm = TRUE),
  3
)

## mean across models - intercepts
rsquare[24, 6] <- round(
  mean(rsquare[1:3, 6]),
  3
)

## mean across models - loadings
rsquare[25, 6] <- round(
  mean(rsquare[4:12, 6]),
  3
)

## mean across models -  thresholds 
rsquare[26, 6] <- round(
  mean(rsquare[c(13:14, 28:30, 23), 6]),
  3
)

## mean across indicators for each model
rsquare[27, 2:5] <- round(
  colMeans(rsquare[c(1:14, 23, 28:30), 2:5], na.rm = TRUE),
  3
)

rsquare <- rbind(
  rsquare[1:3, ], rsquare[24, ], rsquare[4:12, ], rsquare[25, ],
  rsquare[13:17, ], rsquare[28, ], rsquare[18:20, ], rsquare[29, ], 
  rsquare[21:22, ], rsquare[30, ], rsquare[23, ],
  rsquare[26, ], rsquare[27, ]
)


# TABLE 26 in Supplementary Materials
## Fit function contribution
fit_cont <- align_models_list
for (i in 1:length(fit_cont)) {
  fit_cont[[i]] <- fit_cont[[i]][, c(1, 3)]
  names(fit_cont[[i]])[names(fit_cont[[i]]) == "Fit Contribution"] <- model_names[i]
  names(fit_cont[[i]])[1] <- "Parameter"
  fit_cont[[i]][, 2] <- as.numeric(as.character(fit_cont[[i]][, 2]))
}
fit_cont <- Reduce(
  function(x, y) merge(x, y, by = "Parameter", all = TRUE),
  fit_cont
)
fit_cont[nrow(fit_cont) + 4, ] <- NA


## mean threshold of confidence, importance of religion,
## and membership separately
fit_cont[25, 1] <- "Threshold CONFIDEN total"
fit_cont[25, 2:5] <- round(
  colMeans(fit_cont[15:17, 2:5], na.rm = TRUE), 
  3)

fit_cont[26, 1] <- "Threshold IMPREL total"
fit_cont[26, 2:5] <- round(
  colMeans(fit_cont[18:20, 2:5], na.rm = TRUE), 
  3)

fit_cont[27, 1] <- "Threshold MEMBER total"
fit_cont[27, 2:5] <- round(
  colMeans(fit_cont[21:22, 2:5], na.rm = TRUE), 
  3)

## mean for each model
fit_cont[24, 1] <- "Mean"
fit_cont[24, 2:5] <- round(
  colMeans(fit_cont[c(1:14, 23, 25:27), 2:5], na.rm = TRUE),
  3
)


fit_cont <- rbind(
  fit_cont[1:17, ], fit_cont[25, ], fit_cont[18:20, ], fit_cont[26, ], 
  fit_cont[21:22,], fit_cont[27, ], fit_cont[23, ], fit_cont[24, ]
)



## Show tables

## TABLE 3
alignment_table <- data.frame(
  cbind(
    c("Frequency of attendance", "Importance of God", "Frequency of praying",
      "Belonging to a denomination", "Belief in God", "Confidence in institutions", 
      "Importance of religion", "Membership in an organization", 
      "Identification as religious person"),
    rbind(
      round(mean(share_ni[c(1, 5), 6]), 2),
      round(mean(share_ni[c(2, 9), 6]), 2),
      round(mean(share_ni[c(3, 13), 6]), 2),
      round(mean(share_ni[c(6, 15), 6]), 2),
      round(mean(share_ni[c(7, 16), 6]), 2),
      round(mean(share_ni[c(8, 20), 6]), 2),
      round(mean(share_ni[c(10, 24), 6]), 2),
      round(mean(share_ni[c(11, 27), 6]), 2),
      round(mean(share_ni[c(12, 28), 6]), 2)
    ),
    rbind(
      round(mean(rsquare[c(1, 5), 6]), 3),
      round(mean(rsquare[c(2, 9), 6]), 3),
      round(mean(rsquare[c(3, 13), 6]), 3),
      round(mean(rsquare[c(6, 15), 6]), 3),
      round(mean(rsquare[c(7, 16), 6]), 3),
      round(mean(rsquare[c(8, 20), 6]), 3),
      round(mean(rsquare[c(10, 24), 6]), 3),
      round(mean(rsquare[c(11, 27), 6]), 3),
      round(mean(rsquare[c(12, 28), 6]), 3)
    ),
    rbind(
      round(colMeans(fit_cont[c(1, 4), 2:5], na.rm = TRUE), 3),
      round(colMeans(fit_cont[c(2, 8), 2:5], na.rm = TRUE), 3),
      round(colMeans(fit_cont[c(3, 12), 2:5], na.rm = TRUE), 3),
      round(colMeans(fit_cont[c(5, 13), 2:5], na.rm = TRUE), 3),
      round(colMeans(fit_cont[c(6, 14), 2:5], na.rm = TRUE), 3),
      round(colMeans(fit_cont[c(7, 18), 2:5], na.rm = TRUE), 3),
      round(colMeans(fit_cont[c(9, 22), 2:5], na.rm = TRUE), 3),
      round(colMeans(fit_cont[c(10, 25), 2:5], na.rm = TRUE), 3),
      round(colMeans(fit_cont[c(11, 26), 2:5], na.rm = TRUE), 3)
    )
  )
)

alignment_table[nrow(alignment_table) + 1, ] <- NA
alignment_table[10, 1] <- "Mean"
alignment_table[10, 4:7] <- fit_cont[27, 2:5]

colnames(alignment_table) <- c("Indicator", "Share NI countries", "R-square",
                               "Model 1", "Model 2", "Model 3", "Model 4")

kable(alignment_table, row.names = FALSE) %>%
  add_header_above(c(" " = 3, "Fit contribution" = 4))




## TABLE 24 in Supplementary Materials
share_ni[is.na(share_ni)] <- "--"
kable(share_ni, row.names = FALSE) %>%
  group_rows("Intercepts", 1, 4) %>%
  group_rows("Loadings", 5, 14) %>%
  group_rows("Thresholds", 15, 29) %>%
  group_rows("Mean", 30, 30)

# TABLE 25 in Supplementary Materials
rsquare[is.na(rsquare)] <- "--"
kable(rsquare, row.names = FALSE) %>%
  group_rows("Intercepts", 1, 4) %>%
  group_rows("Loadings", 5, 14) %>%
  group_rows("Thresholds", 15, 29) %>%
  group_rows("Mean", 30, 30)

# TABLE 26 in Supplementary Materials
fit_cont[is.na(fit_cont)] <- "--"
kable(fit_cont, row.names = FALSE) %>%
  group_rows("Intercepts", 1, 3) %>%
  group_rows("Loadings", 4, 12) %>%
  group_rows("Thresholds", 13, 26) %>%
  group_rows("Mean", 27, 27)


# TABLE 27 in Supplementary Materials
## Share of countries for each model, by zone
share_zones <- align_models_list
for (i in 1:length(share_zones)) {
  share_zones[[i]] <- FindReplace(
    data = share_zones[[i]], Var = "Invariant Groups", 
    replaceData = country_abb,
    from = "abbreviation", to = "zone", 
    exact = FALSE, vector = FALSE
  )
  share_zones[[i]] <- FindReplace(
    data = share_zones[[i]], Var = "Non-invariant Groups", 
    replaceData = country_abb,
    from = "abbreviation", to = "zone", 
    exact = FALSE, vector = FALSE
  )
  
  # invariant countries, by zone
  zones <- sapply(share_zones[[i]][4], function(x) strsplit(x, " "))
  LA_inv <- str_count(zones, "LA")
  West_inv <- str_count(zones, "West")
  Sinic_inv <- str_count(zones, "Sinic")
  Orthodox_inv <- str_count(zones, "Orthodox")
  Africa_inv <- str_count(zones, "Africa")
  Islamic_inv <- str_count(zones, "Islamic")
  Indic_inv <- str_count(zones, "Indic")
  
  # total N of countries, by zone
  zones_total <- strsplit(share_zones[[i]][1, 5], " ")
  LA_total <- LA_inv[1] + str_count(zones_total, "LA")[1]
  West_total <- West_inv[1] + str_count(zones_total, "West")[1]
  Sinic_total <- Sinic_inv[1] + str_count(zones_total, "Sinic")[1]
  Orthodox_total <- Orthodox_inv[1] + str_count(zones_total, "Orthodox")[1]
  Africa_total <- Africa_inv[1] + str_count(zones_total, "Africa")[1]
  Islamic_total <- Islamic_inv[1] + str_count(zones_total, "Islamic")[1]
  Indic_total <- Indic_inv[1] + str_count(zones_total, "Indic")[1]
  
  # share of invariant countries, by zone
  LA_inv <- round(LA_inv/LA_total, 2)
  West_inv <- round(West_inv/West_total, 2)
  Sinic_inv <- round(Sinic_inv/Sinic_total, 2)
  Orthodox_inv <- round(Orthodox_inv/Orthodox_total, 2)
  Africa_inv <- round(Africa_inv/Africa_total, 2)
  Islamic_inv <- round(Islamic_inv/Islamic_total, 2)
  Indic_inv <- round(Indic_inv/Indic_total, 2)
  
  share_zones[[i]] <- cbind(
    share_zones[[i]][1], LA_inv, West_inv, Sinic_inv, Orthodox_inv,
    Africa_inv, Islamic_inv, Indic_inv
  )
  colnames(share_zones[[i]]) <- c(
    "Parameter", paste0("Latin America_", model_names[[i]]),
    paste0("Western_", model_names[[i]]),
    paste0("Sinic East_", model_names[[i]]),
    paste0("Orthodox East_", model_names[[i]]),
    paste0("Sub-Saharan Africa_", model_names[[i]]),
    paste0("Islamic East_", model_names[[i]]),
    paste0("Indic East_", model_names[[i]])
  )
}

## Ignore the warnings, they are related to the arguments' format only


share_zones <- Reduce(
  function(x, y) merge(x, y, by = "Parameter", all = TRUE),
  share_zones
)

share_zones <- share_zones %>%
  select(sort(names(.)))
share_zones <- share_zones[, c(21, 1:20, 22:29)]
share_zones <- share_zones %>%
  gather(variable, value, -Parameter) %>%
  mutate(
    location = sub(".*(Model 1|Model 2|Model 3|Model 4).*", "\\1", variable),
    variable = sub("_?(Model 1|Model 2|Model 3|Model 4)_?", "", variable)
  ) %>%
  spread(variable, value)

share_zones$Parameter[duplicated(share_zones$Parameter)] <- NA
share_zones[, 2:8][is.na(share_zones[, 2:8])] <- "no"
share_zones[share_zones == 0] <- "--"

kable(share_zones, row.names = FALSE) %>%
  add_header_above(c(` ` = 2, `Cultural zones` = 7)) %>%
  group_rows("Intercepts", 1, 12) %>%
  group_rows("Loadings", 13, 48) %>%
  group_rows("Thresholds", 49, 92) %>%
  footnote(
    general = "-- = no invariant countries, no = countries did not participate in survey."
  )



# FIGURE 5
## Factor means across models
align.models.means <- list(
  align_model1, align_model2_red, align_model3_red, align_model4)
model_names <- c("Model 1", "Model 2", "Model 3", "Model 4")

se_models <- list(se_model1, se_model2, se_model3, se_model4)
for (i in 1:length(se_models)) {
  names(se_models[[i]])[1] <- "code"
  names(se_models[[i]])[2] <- "se"
}

for (i in 1:length(align.models.means)) {
  align.models.means[[i]] <- align.models.means[[i]][[4]][[1]][, 3:4]
  align.models.means[[i]] <- na.omit(align.models.means[[i]])
  names(align.models.means[[i]])[names(align.models.means[[i]]) ==
                                   "Factor.mean"] <- model_names[i]
  names(align.models.means[[i]])[names(align.models.means[[i]]) ==
                                   "Group.value"] <- "code"
  align.models.means[[i]] <- merge(
    align.models.means[[i]], country_abb[, 1:2], by = c("code"),
    all.x = TRUE
  )
  align.models.means[[i]][, 2] <- formattable(align.models.means[[i]][, 2], 
                                              digits = 2, format = "f")
  align.models.means[[i]] <- merge(
    align.models.means[[i]], se_models[[i]], by = c("code"),
    all.x = TRUE
  )
  align.models.means[[i]][, 4] <- as.numeric(as.character(align.models.means[[i]][, 4]))
}

plot1 <- ggmeans(align.models.means[[1]])
plot2 <- ggmeans(align.models.means[[2]])
plot3 <- ggmeans(align.models.means[[3]])
plot4 <- ggmeans(align.models.means[[4]])

grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)






# Other analyses and tables included in Supplementary Materials (except alignment results for WVS6)

# Table 1 in Supplementary Materials ----------------------------------------------------
## Data: Brown, D., & James, P. Religious Characteristics of States Dataset
## Project - Demographics v. 2.0 (RCS-Dem 2.0), COUNTRIES ONLY.
rcs_data <- read.spss("https://osf.io/xtvf5/download", 
                      use.value.labels = TRUE, to.data.frame = TRUE)
## select observations for 2010 year as the beginning of WVS6 data collection 
## ABBREV - 3-letter abbreviation assigned by RCS, generally following COW 
## CATPC - Percentage of Catholics 
## PRTPC - Percentage of Protestants 
## QPRTPC - Percentage of Quasi-Protestants - Combined Anglican & Pentecostal
## ORTPC - Percentage of Orthodox 
## CSYNPC - Percentage of Christian Syncretics 
### (Mostly African and New World African spiritist and spiritualist denominations) 
## MUSPC - Percentage of Muslims 
## HINPC - Percentage of Hindus 
## BUDPC - Percentage of Buddhists 
## East Asian Religions: 
### SHNPC - Percentage of Shintoists 
### CNFPC - Percentage of Confucianists 
### TAOPC - Percentage of Taoists 
### CHFPC - Percentage of Chinese Folk Religionists 
## NREPC - Percentage of Not Religious 
## UNKPC - Percentage of Unknown

rcs_data <- rcs_data[rcs_data$YEAR == 2010, ]
rcs_data <- select(
  rcs_data, c(
    ABBREV, CATPC, PRTPC, QPRTPC, ORTPC, CSYNPC, MUSPC, HINPC, BUDPC,
    SHNPC, CNFPC, TAOPC, CHFPC, NREPC, UNKPC
  )
)


## Correlates of War country codes
cow_codes <- read.csv("https://correlatesofwar.org/data-sets/cow-country-codes/cow-country-codes")

## Merge data frames
rel_compos <- merge(
  rcs_data, cow_codes[, c(1, 3)],
  by.x = c("ABBREV"),
  by.y = c("StateAbb"),
  all.x = TRUE
)
rel_compos$StateNme <- as.character(rel_compos$StateNme)
rel_compos$StateNme[rel_compos$StateNme == "United States of America"] <- "United States"

## Add Palestine - GAZ in Religious Characteristics of States Dataset 
## Add Hong Kong - HKO
missing <- subset(rcs_data, subset = (rcs_data$ABBREV %in% c("GAZ", "HKO")))
missing$StateNme <- c("Hong Kong", "Palestine")

rel_compos <- rbind(rel_compos, missing)
rel_compos <- subset(
  rel_compos, subset = rel_compos$StateNme %in% levels(rel_data$country)
)
rel_compos <- rel_compos[!duplicated(rel_compos$StateNme),]

## Combine Eastern religions
rel_compos$East <- rowSums(
  rel_compos[, c("SHNPC", "CNFPC", "TAOPC", "CHFPC")],
  na.rm = TRUE
)
## Combine Protestants
rel_compos$PRTPC <- rowSums(
  rel_compos[, c("PRTPC", "QPRTPC")],
  na.rm = TRUE
)

rel_compos[, c("SHNPC", "CNFPC", "TAOPC", "CHFPC", "QPRTPC")] <- NULL

## Add sample size
nobs <- data.frame(table(rel_data$country))
rel_compos <- merge(
  rel_compos, nobs, by.x = c("StateNme"),
  by.y = 1, all.x = TRUE
)

rel_compos <- cbind(
  rel_compos[, 1], rel_compos[, 13], rel_compos[, 3:9], rel_compos[, 12], 
  rel_compos[, 10:11]
)

colnames(rel_compos) <- c("Country", "N obs", "Catholics", "Protestants", "Orthodox", "Christ Syncr",
  "Muslims", "Hindus", "Buddhists", "East Asian", "Not Religious", "Unknown")

rel_compos[, 3:12] <- round(rel_compos[, 3:12], 0)
rel_compos[, 3:12][rel_compos[, 3:12] < 10 & rel_compos[, 3:12] != 0] <- "<10"
rel_compos[is.na(rel_compos)] <- "--"
df_to_viewer(rel_compos, rownames = FALSE)



# TABLES 3-12 in Supplementary Materials ----------------------------------------------------
## Distribution of indicators, by country

WVS6$belong.bin <- ifelse(WVS6$V144 == "None", "No", "Yes")
WVS6 <- WVS6 %>%
  rename_at(
    vars(
      c("V2", "V9", "V25", "V108", "V145", "V146", "V147", "V148",
        "V149", "V152", "V154")
    ),
    ~c("country", "imprel", "member", "confidence", "attend", "pray",
      "person", "bgod", "bhell", "impgod", "fundament")
  )
WVS6$country <- droplevels(WVS6$country)

sapply(
  c("imprel", "member", "confidence", "belong.bin", "attend", "pray",
    "person", "bgod", "bhell", "impgod", "fundament"),
  function(x) {
    crosstab("country", x, WVS6, margin = "row")
  }
)


## Mean and sd for continuous indicators
meandsd <- rel_data %>%
  group_by(country) %>%
  summarise_at(
    vars(attend, pray, impgod),
    funs(
      mean(., na.rm = TRUE),
      sd(., na.rm = TRUE)
    )
  )
meandsd[, 2:7] <- round(meandsd[, 2:7], 2)


# TABLE 14 in Supplementary Materials ----------------------------------------------------
## Cross-country correlations of indicators: mean, sd 
## Use countries with no omitted questions
correlations.list <- lavCor(
  rel_data_full1[, -12], group = "country", ordered = ord_items, missing = "listwise",
  output = "cor"
)

corr.mean <- aaply(
  laply(correlations.list, as.matrix),
  c(2, 3),
  mean
)
corr.sd <- aaply(
  laply(correlations.list, as.matrix),
  c(2, 3),
  sd
)
corr.mean.sd <- matrix(
  paste(
    round(corr.mean, 2),
    round(corr.sd, 2),
    sep = ", "
  ),
  nrow = nrow(corr.mean),
  dimnames = dimnames(corr.mean)
) %>%
  df_to_viewer()



# TABLES 21-23 in Supplementary Materials ----------------------------------------------------
## Fit measures for models with 'importance of god' included in all models

## Model with 7 indicators
model_god1 <- "Religiosity =~ imprel + person + confidence + pray + attend + belonging + impgod;
pray ~~ attend;
impgod ~~ pray;
"
groupwiseCFA.modified(
  model_god1, data = rel_data_full1, group = "country", ordered = ord_items,
  estimator = "WLSMV", missing = "listwise", out = c("fit"))

## Drop Poland 
## Note. There are negative correlations of indicators in:
## Ghana, Kyrgyzstan, Malaysia, Nigeria, Philippines, Rwanda, Thailand


# Model with 8 indicators
model_god2 <- "Religiosity =~ imprel + person + confidence + pray + attend + belonging + bgod + impgod;
pray ~~ attend;
impgod ~~ pray;
"
groupwiseCFA.modified(
  model_god2, data = rel_data_full1, group = "country", ordered = ord_items,
  estimator = "WLSMV", missing = "listwise", out = c("fit"))

## Drop Poland 
## Note. There are negative correlations of indicators in:
## Ghana, Kyrgyzstan, Malaysia, Nigeria, Philippines, Rwanda, Thailand, Turkey


# Model with 9 indicators
model_god3 <- "Religiosity =~ imprel + person + confidence + pray + attend + belonging + bgod + member + impgod;
pray ~~ attend;
impgod ~~ pray;
"
groupwiseCFA.modified(
  model_god3, data = rel_data_full1, group = "country", ordered = ord_items,
  estimator = "WLSMV", missing = "listwise", out = c("fit"))

## Note. There are negative correlations of indicators in:
## Armenia, Azerbaijan, Georgia, Ghana, Kazakhstan, Kyrgyzstan, Malaysia, Nigeria, 
## Peru, Philippines, Rwanda, Turkey, Thailand, Uzbekistan, India



# Robustness analyses on WVS 7 data ----------------------------------------------------
## Use EVS_WVS_Cross-National_Wave_7_joint_core_spss_v1_1 version, 
## as in the latter version(s) the membership in organization indicator was recoded 
## into a binary variable for all countries

WVS7 <- read.spss(
  "EVS_WVS_Cross-National_Wave_7_joint_core_spss_v1_1.sav", use.value.labels = TRUE,
  to.data.frame = TRUE
)
rel_data_WVS7 <- select(
  WVS7, c(B_COUNTRY, Q6, Q94, Q64, Q289, Q171, Q172, Q173, Q165, Q164))

## add Ukraine and Singapore data from WVS
rel_data_WVS7_added <- read.spss(
  "WVS_Cross-National_Wave_7_sav_v2_0.sav", use.value.labels = TRUE, 
  to.data.frame = TRUE
)
rel_data_WVS7_added <- select(
  rel_data_WVS7_added, c(B_COUNTRY, Q6, Q94, Q64, Q289, Q171, Q172, Q173, Q165, Q164)
)

rel_data_WVS7_added <- subset(
  rel_data_WVS7_added, subset = rel_data_WVS7_added$B_COUNTRY %in% c("Ukraine", "Singapore")
)

rel_data_WVS7 <- rbind(rel_data_WVS7, rel_data_WVS7_added)


colnames(rel_data_WVS7) <- c("country", "imprel", "member", "confidence", "belong", "attend", "pray",
  "person", "bgod", "impgod")

for (item in c("imprel", "member", "confidence", "attend", "pray", "person", "bgod",
  "impgod")) {
  rel_data_WVS7[, item] <- as.numeric(rel_data_WVS7[, item])
}

for (item in c("imprel", "confidence")) {
  rel_data_WVS7[, item] <- Recode(
    rel_data_WVS7[, item], rec = "1=4; 2=3; 3=2; 4=1; else=NA")
}

rel_data_WVS7$bgod <- Recode(
  rel_data_WVS7$bgod, rec = "1=2; 2=1; else=NA")
rel_data_WVS7$attend <- Recode(
  rel_data_WVS7$attend, rec = "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; else=NA"
)
rel_data_WVS7$person <- Recode(
  rel_data_WVS7$person, rec = "1=2; 2=1; 3=1; else=NA")

rel_data_WVS7$belonging <- as.numeric(rel_data_WVS7$belong)
rel_data_WVS7$belonging <- ifelse(rel_data_WVS7$belonging == "1", 1, 2)


## Due to the different coding for the frequency of praying and
## membership in organization indicators in WVS and EVS, 
## harmonize the coding for specific countries

EVS <- subset(
  rel_data_WVS7, subset = rel_data_WVS7$country %in% c(
    "Armenia", "Belarus", "Netherlands", "Poland", "Slovenia", "Spain",
    "Sweden"
  )
)
rel_data_WVS7 <- subset(
  rel_data_WVS7, subset = !(rel_data_WVS7$country %in% c(
    "Armenia", "Belarus", "Netherlands", "Poland", "Slovenia", "Spain",
    "Sweden"
  ))
)

rel_data_WVS7$pray <- Recode(
  rel_data_WVS7$pray, rec = "1=7; 2=7; 3=6; 4=5; 5=4; 6=3; 7=2; 8=1; else=NA"
)
rel_data_WVS7$member <- Recode(
  rel_data_WVS7$member, rec = "1=1; 2=2; 3=2; else=NA")
EVS$pray <- Recode(
  EVS$pray, rec = "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; else=NA")

rel_data_WVS7 <- rbind(rel_data_WVS7, EVS)

# set the same country names as in WVS6:
levels(rel_data_WVS7$country)[levels(rel_data_WVS7$country) == "Taiwan ROC"] <- "Taiwan"
levels(rel_data_WVS7$country)[levels(rel_data_WVS7$country) == "Hong Kong SAR"] <- "Hong Kong"


rm(rel_data_WVS7_added, EVS, WVS7)



# CFA

ord_items <- c("person", "imprel", "confidence", "bgod", "belonging", "member")

# TABLE 28 in Supplementary Materials
## Model 1 
## Run on the same sample as WVS6 except Uruguay, Uzbekistan, South Africa, 
## Trinidad and Tobago
data_model1_WVS7 <- subset(
  rel_data_WVS7, subset = (rel_data_WVS7$country %in% levels(data_model1$country))
)
data_model1_WVS7$country <- droplevels(data_model1_WVS7$country)

cfa_model1_WVS7 <- groupwiseCFA.modified(
  model1, data = data_model1_WVS7, group = "country", ordered = ord_items,
  estimator = "WLSMV", missing = "listwise", out = c("goodfit")
)


# TABLE 29 in Supplementary Materials
## Model 2 
## Run on the same sample as WVS6 except  Ghana, Trinidad and Tobago
## South Africa, Zimbabwe - empty category of 'belonging to denomination';
## Turkey - the same, but caused by the listwise deletion of missings

data_model2_WVS7 <- subset(
  rel_data_WVS7, subset = (rel_data_WVS7$country %in% levels(data_model2$country))
)
data_model2_WVS7 <- subset(
  data_model2_WVS7, subset = !(data_model2_WVS7$country %in% c("Zimbabwe", "Turkey"))
)
data_model2_WVS7$country <- droplevels(data_model2_WVS7$country)

cfa_model2_WVS7 <- groupwiseCFA.modified(
  model2, data = data_model2_WVS7, group = "country", ordered = ord_items,
  estimator = "WLSMV", missing = "listwise", out = c("goodfit")
)

# TABLE 30 in Supplementary Materials
## Model 3 
## Run on the same sample as WVS6 except Trinidad and Tobago, South Africa, 
## Uruguay
data_model3_WVS7 <- subset(
  rel_data_WVS7, subset = (rel_data_WVS7$country %in% levels(data_model3$country))
)
data_model3_WVS7$country <- droplevels(data_model3_WVS7$country)

cfa_model3_WVS7 <- groupwiseCFA.modified(
  model3, data = data_model3_WVS7, group = "country", ordered = ord_items,
  estimator = "WLSMV", missing = "listwise", out = c("goodfit")
)

## treat only 15 countries as acceptable:
## without Ecuador, as it had non-acceptable fit without recoding
## drop Russia - correlation between variables bgod and person is (nearly) 1.0


# TABLE 31 in Supplementary Materials
## Model 4 
## Run on the same sample as WVS6 except Uruguay, Trinidad and Tobago,
## South Africa

data_model4_WVS7 <- subset(
  rel_data_WVS7, subset = (rel_data_WVS7$country %in% levels(data_model4$country))
)
data_model4_WVS7$country <- droplevels(data_model4_WVS7$country)

cfa_model4_WVS7 <- groupwiseCFA.modified(
  model4, data = data_model4_WVS7, group = "country", ordered = ord_items,
  estimator = "WLSMV", missing = "listwise", out = c("goodfit")
)

## treat only 10 countries as acceptable:
## without Ecuador, New Zealand, Cyprus, as they had non-acceptable fit without recoding 
## drop Russia - correlation between variables bgod and person is (nearly) 1.0


# MGCFA

## Model 1

data_model1_WVS7 <- subset(
  rel_data_WVS7, subset = rel_data_WVS7$country %in% rownames(cfa_model1_WVS7)
)
data_model1_WVS7$country <- droplevels(data_model1_WVS7$country)

mgcfa_model1_WVS7 <- globalMI.modified.ord(
  model1, data = data_model1_WVS7, group = "country", ordered = ord_items,
  estimator = "WLSMV", missing = "listwise"
)

## Model 2

data_model2_WVS7 <- subset(
  rel_data_WVS7, subset = rel_data_WVS7$country %in% rownames(cfa_model2_WVS7)
)
data_model2_WVS7$country <- droplevels(data_model2_WVS7$country)

mgcfa_model2_WVS7 <- globalMI.modified.ord(
  model2, data = data_model2_WVS7, group = "country", ordered = ord_items,
  estimator = "WLSMV", missing = "listwise"
)

## Model 3

data_model3_WVS7 <- subset(
  rel_data_WVS7, subset = rel_data_WVS7$country %in% rownames(cfa_model3_WVS7)[-c(2, 13)]
)
data_model3_WVS7$country <- droplevels(data_model3_WVS7$country)

mgcfa_model3_WVS7 <- globalMI.modified.ord(
  model3, data = data_model3_WVS7, group = "country", ordered = ord_items,
  estimator = "WLSMV", missing = "listwise"
)

## Model 4

data_model4_WVS7 <- subset(
  rel_data_WVS7, subset = rel_data_WVS7$country %in% rownames(cfa_model4_WVS7)[-c(1:3, 9)]
)
data_model4_WVS7$country <- droplevels(data_model4_WVS7$country)

mgcfa_model4_WVS7 <- globalMI.modified.ord(
  model4, data = data_model4_WVS7, group = "country", ordered = ord_items,
  estimator = "WLSMV", missing = "listwise"
)


# TABLE 32 in Supplementary Materials

table_mgcfa_WVS7 <- rbind(
  mgcfa_model1_WVS7, mgcfa_model2_WVS7, mgcfa_model3_WVS7, mgcfa_model4_WVS7
)
colnames(table_mgcfa_WVS7) <- c("Model", "CFI", "Δ CFI", "RMSEA", "Δ RMSEA", "χ2", "df", "SRMR")
table_mgcfa_WVS7[is.na(table_mgcfa_WVS7)] <- "--"

kable(table_mgcfa_WVS7) %>%
  group_rows("Model 1 (18 countries)", 1, 2) %>%
  group_rows("Model 2 (18 countries)", 3, 4) %>%
  group_rows("Model 3 (14 countries)", 5, 6) %>%
  group_rows("Model 4 (9 countries)", 7, 8) %>%
  footnote(
    general = "CFI = comparative fit index, 
               RMSEA = root mean square error of approximation, 
               χ2 = chi-square, df = degrees of freedom, 
               SRMR = standardized root mean residual"
)



# Alignment

## Do not estimate for Model 4, as there are only 10 countries with acceptable fit

model_names_WVS7 <- c("Model 1 WVS7", "Model 2 WVS7", "Model 3 WVS7")
for (i in 1:length(model_names_WVS7)) {
  directory <- paste0("add your path to the folder here/Alignment/", model_names_WVS7[i])
  folder <- dir.create(directory)
}

# Model 1

setwd("add your path to the folder here/Model 1 WVS7")
runAlignment.modified(
  "Religiosity BY imprel confidence pray attend person impgod;
  pray WITH attend;
  impgod WITH pray;",
  group = "country", dat = data_model1_WVS7, estim = "mlr", 
  categorical = c("person", "imprel", "confidence"),
  sim.samples = c(100, 500, 1000, 1500), sim.reps = 500, 
  Mplus_com = "/Applications/Mplus/mplus", summaries = FALSE
)

## extract alignment results
align_model1_WVS7 <- extractAlignment("fixed.out", silent = TRUE)

## extract simulation results
sim_model1_WVS7 <- extractAlignmentSim(
  c("sim100.out", "sim500.out", "sim1000.out", "sim1500.out"),
  silent = TRUE
)

# Model 2

setwd("add your path to the folder here/Model 2 WVS7")
runAlignment.modified(
  "Religiosity BY imprel person confidence pray attend belonging;
  pray WITH attend;",
  group = "country", dat = data_model2_WVS7, estim = "mlr", 
  categorical = c("person", "imprel", "confidence", "belonging"),
  sim.samples = c(100, 500, 1000, 1500), sim.reps = 500, 
  Mplus_com = "/Applications/Mplus/mplus", summaries = FALSE
)

## extract alignment results
align_model2_WVS7 <- extractAlignment("fixed.out", silent = TRUE)

## extract simulation results
sim_model2_WVS7 <- extractAlignmentSim(
  c("sim100.out", "sim500.out", "sim1000.out", "sim1500.out"),
  silent = TRUE
)

# Model 3

setwd("add your path to the folder here/Model 3 WVS7")
runAlignment.modified(
  "Religiosity BY imprel person confidence pray attend belonging bgod;
  pray WITH attend;",
  group = "country", dat = data_model3_WVS7, estim = "mlr", 
  categorical = c("person", "imprel", "confidence", "belonging", "bgod"),
  sim.samples = c(100, 500, 1000, 1500), sim.reps = 500, 
  Mplus_com = "/Applications/Mplus/mplus", summaries = FALSE
)

## extract alignment results
align_model3_WVS7 <- extractAlignment("fixed.out", silent = TRUE)

## extract simulation results
sim_model3_WVS7 <- extractAlignmentSim(
  c("sim100.out", "sim500.out", "sim1000.out", "sim1500.out"),
  silent = TRUE
)


# TABLE 33 in Supplementary Materials

sim_models_list_WVS7 <- list(sim_model1_WVS7, sim_model2_WVS7, sim_model3_WVS7)
for (i in 1:length(sim_models_list_WVS7)) {
  sim_models_list_WVS7[[i]] <- cbind(
    sim_models_list_WVS7[[i]][[1]][[1]][[1]], sim_models_list_WVS7[[i]][[2]][[1]][[1]],
    sim_models_list_WVS7[[i]][[3]][[1]][[1]], sim_models_list_WVS7[[i]][[4]][[1]][[1]]
  )
  sim_models_list_WVS7[[i]] <- round(sim_models_list_WVS7[[i]], 3)
}

table_sim_WVS7 <- cbind(
  c("Model 1", "Model 2", "Model 3"),
  rbind(
    sim_models_list_WVS7[[1]], sim_models_list_WVS7[[2]], sim_models_list_WVS7[[3]]
  )
)
colnames(table_sim_WVS7) <- c("Model", "100", "500", "1000", "1500")
kable(table_sim_WVS7) %>%
  add_header_above(c(` ` = 1, `Number of observations per group` = 4))



## correlation of factor means across two survey waves

data_models_WVS7 <- list(data_model1_WVS7, data_model2_WVS7, data_model3_WVS7)
names(data_models_WVS7) <- c("Model 1 WVS7", "Model 2 WVS7", "Model 3 WVS7")
codes_list <- lapply(
  data_models_WVS7, function(data_models_WVS7) as.data.frame(levels(data_models_WVS7[, 1]))
)
for (i in 1:length(codes_list)) {
  codes_list[[i]] <- cbind(
    1:nrow(codes_list[[i]]),
    codes_list[[i]]
  )
  names(codes_list[i][[1]])[1] <- "number"
  names(codes_list[i][[1]])[2] <- names(codes_list[i])
}

codes_list <- Reduce(
  function(x, y) merge(x, y, by = "number", all = TRUE),
  codes_list
)
codes_list <- rbind(codes_list[10:18, ], codes_list[1:9, ])

align_models_WVS7 <- list(align_model1_WVS7, align_model2_WVS7, align_model3_WVS7)

for (i in 1:length(align_models_WVS7)) {
  align_models_WVS7[[i]] <- align_models_WVS7[[i]][[4]][[1]][, 3:4]
  align_models_WVS7[[i]] <- na.omit(align_models_WVS7[[i]])
  names(align_models_WVS7[[i]])[names(align_models_WVS7[[i]]) ==
                                  "Factor.mean"] <- names(data_models_WVS7[i])
  names(align_models_WVS7[[i]])[names(align_models_WVS7[[i]]) ==
                                  "Group.value"] <- "Code"
  code_WVS7 <- select(codes_list, c(number, names(data_models_WVS7[i][1])))
  align_models_WVS7[[i]] <- merge(
    align_models_WVS7[[i]], code_WVS7, by.x = c("Code"),
    by.y = c("number"),
    all.x = TRUE
  )
  names(align_models_WVS7[[i]])[2] <- names(data_models_WVS7[i][i])
  names(align_models_WVS7[[i]])[3] <- "country"
  align_models_WVS7[[i]] <- align_models_WVS7[[i]][, -1]
  align_models_WVS7[[i]][1] <- round(align_models_WVS7[[i]][1], 2)
  align_models_WVS7[[i]] <- merge(
    align_models_WVS7[[i]], align.models.means[[i]], 
    by = c("country")) # merge two waves
  
}

## Model 1
cor(align_models_WVS7[[1]][2], align_models_WVS7[[1]][4]) %>% 
  round(., 2)
## Model 2 - drop Slovenia
cor(align_models_WVS7[[2]][2][-15, ], align_models_WVS7[[2]][4][-15, ]) %>% 
  round(., 2)
## Model 3
cor(align_models_WVS7[[3]][2], align_models_WVS7[[3]][4]) %>% 
  round(., 2)





# Session info ----------------------------------------------------

sessionInfo(package = NULL)
# R version 4.0.5 (2021-03-31) Platform: x86_64-apple-darwin17.0 (64-bit) Running under:
# macOS Big Sur 10.16

# attached base packages: stats graphics grDevices utils datasets methods base

# other attached packages: formatR_1.11 stargazer_5.2.2 formattable_0.2.1
# gridExtra_2.3 gdata_2.18.0 tidyr_1.1.3 stringr_1.4.0 reshape2_1.4.4 
# readxl_1.3.1 DataCombine_0.2.21 MIE_0.5-3 shinyWidgets_0.6.0 shinyjs_2.0.0
# igraph_1.2.6 DT_0.17 shiny_1.6.0 ggforce_0.3.3 ggrepel_0.9.1 kableExtra_1.3.4
# LittleHelpers_0.5-10 lme4_1.1-26 Matrix_1.3-2 magrittr_2.0.1 ggplot2_3.3.3
# lavaan_0.6-7 dplyr_1.0.5 plyr_1.8.6 haven_2.3.1 foreign_0.8-81 car_3.0-10
# carData_3.0-4


