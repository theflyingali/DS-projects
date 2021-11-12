### CLEAR WORKSPACE
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
### DATA MANIPULATION ###
library("haven")        
library("dplyr")      
library("psych")
library('stringr')
library('missForest') # impute data

### MODELING ###
library("lavaan")
library("semTools")

### VISUALIZATION ###
library("corrplot")     
library("tidySEM")
library("semPlot")

slovenia_youth_2013_raw <- read_sav('Slovenian Youth 2013.sav')
gender_age_grouping <- slovenia_youth_2013_raw$A0_gender_age
write.csv(slovenia_youth_2013_raw,'slovenia_youth_2013_raw.csv') # to deal with errors relating to haven label
rm(slovenia_youth_2013_raw)

slovenia_youth_2013 <- read.csv('slovenia_youth_2013_raw.csv')
variables <- c('A0_gender_age','D3','A20','A21','A22','A9','A10','A18',
               'A17','I2_1','I2_2','I2_3','I2_4','I2_5','I2_6','I2_7',
               'I2_8','I2_9','I1_1','I1_2','I1_3','E2','E3','E5',
               'A3_13','B1_8','B9','B10','B11','C13_1','F9_8','F9_1','F9_2',
               'F9_3','F9_4','F9_5','F9_6','F9_7','F9_9','F9_13','F10','D3')
df <- select(slovenia_youth_2013,variables)
rm(variables)

# Before answering any questions, I will identify manifest variables that I
# want to study as well as create latent variables that I want to study

# Latent variables:
# dietary_health: A20 A21 A22
# lifestyle_health: A9 A10 A18
# wealth: I2_1 I2_2 I2_3 I2_4 I2_5 I2_6 I2_7 I2_8 I2_9
# parents_education: I1_2 I1_3
# religosity: ...
# trust in ps: ...

# manifest variables:
# desire_to_leave_slovenia (D2)
# health (A17)

##############################################################################
####### Exploration of variables, for multicollinearity and to determine what 
####### needs to be treated as a categorical variable
##############################################################################
# Multicollinearity first:
# Q1 variables: 
Q1_cov <- cov(df[,c('A20','A21','A22','A9','A10','A18','A17')], 
              use = "pairwise.complete.obs")
Q1_cor <- cov2cor(Q1_cov)
corrplot(Q1_cor, 
         is.corr = TRUE,       # whether is a correlation matrix 
         method = "circle",     # magnitude of covariances as circles 
         type = "upper",        # remove the bottom of the covariance matrix
         addCoef.col = "black"  # add to the plot the coefficients
) 
# Looks okay
Q2_cov <- cov(df[,c('I2_1','I2_2','I2_3','I2_4','I2_5','I2_6','I2_7',
                    'I2_8','I2_9')],use = "pairwise.complete.obs")
Q2_cor <- cov2cor(Q2_cov)
corrplot(Q2_cor, 
         is.corr = TRUE,       # whether is a correlation matrix 
         method = "circle",     # magnitude of covariances as circles 
         type = "upper",        # remove the bottom of the covariance matrix
         addCoef.col = "black"  # add to the plot the coefficients
) 
# Looks okay
Q3_cov <- cov(df[,c('A3_13','B1_8','B9','B10','B11','C13_1','F9_8','F9_1',
                    'F9_2','F9_3','F9_4','F9_5','F9_6','F9_7','F9_9','F9_13',
                    'F10','D3')],
              use = "pairwise.complete.obs")
Q3_cor <- cov2cor(Q3_cov)
corrplot::corrplot(Q3_cor, 
                   is.corr = TRUE,       # whether is a correlation matrix 
                   method = "circle",     # magnitude of covariances as circles 
                   type = "upper",        # remove the bottom of the covariance matrix
                   addCoef.col = "black"  # add to the plot the coefficients
)
# We see some concerns regarding multicolinearity with the variables:
# F9_1 F9_2 F9_3, will keep F9_1
# F9_5 F9_7 F9_9, will keep F9_5
# B9 B10 B11, will keep B9

# Before dealing with NA values, we will eliminate the variables mentioned
# and determine which variables should be treated as categorical
df <- select(df,-c(F9_2,F9_3,F9_7,F9_9,B10,B11))
# Determining which are categorical will be a determined with the following rules:
# 1. levels of variables < 5
# 2. level of variable is < 10 but >= 5, with high skewness (abs(skew) > 1)
names <- names(df)
df_explore <- df
df_explore[names] <- sapply(df[names],ordered)
df_explore[sapply(df_explore, is.character)] <- 
  lapply(df_explore[sapply(df_explore, is.character)],as.factor)

list_categories <- sapply(df_explore,summary)
count_categories <- sapply(list_categories,length)
count_categories[count_categories < 5] # A9
# Some are counted to have more than5 because NA is taken as a category,
# so lets visually check
list_categories[names(count_categories[count_categories < 6])]
# D3, F9_8, F9_1,F9_4,F9_5,F9_6,F9_13

# Till now we convert the following to categorical: 
# A9, D3, F9_8, F9_1,F9_4,F9_5,F9_6,F9_13

as.data.frame(sapply(list_categories,describe))[11,]
# I2_1, I2_2, I2_5, I2_9, I1_3, B1_8, B9 are all highly skewed
list_categories[names(count_categories[count_categories > 12])]
# I2_1,I2_2, I2_9 have too many categories, so will not be treated as categorical
rm(names,list_categories,df_explore)


# Final list of variables to order:
# A9,D3,F9_8,F9_1,F9_4,F9_5,F9_6,F9_13, I2_1, I2_2, I2_5, I1_3, B1_8,B9

ordered_variables <- c('A9','D3','F9_8','F9_1','F9_4','F9_5','F9_6','F9_13', 
                       'I2_5','I1_3','B1_8','B9')

df[ordered_variables] <-
  sapply(df[ordered_variables],ordered)
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)],as.factor)
sapply(df,class)

# In order to deal with NA variables, we need to decide on whether a missing 
# value would be considered MCAR, MAR, or MNAR. Given that this is a survey
# that was filled entirely by the respondents, it is unlikely for missing 
# values to be MCAR or MAR, but in fact to be missing not at random, therefore
# the method we will to deal with any missing values is imputation

# Amelia package cant deal with categorical variables, so we will use missForest
# for imputation purposes

df_missforest <- missForest(df)
df_imputed <- data.frame(df_missforest$ximp)

df_imputed[ordered_variables] <- sapply(df_imputed[ordered_variables],ordered)


###############################################################################
####### Question 1A: Are the people following healthy lifestyles similar
####### to the people following healthy diets?
###############################################################################

dietary_lifestyle_correlation_model <- 'dietary_health =~ A20 + A21 + A22
                                        lifestyle_health =~ A9 + A10 + A18
                                        dietary_health ~~ lifestyle_health'

fit_dietary_lifestyle_model <- cfa(dietary_lifestyle_correlation_model,
                                   ordered = c('A9'),
                                   data = df_imputed)
summary(fit_dietary_lifestyle_model)
fitMeasures(fit_dietary_lifestyle_model)[c('srmr','rmsea','cfi','tli')]
# SRMR = 0.055 < 0.08 acceptable
# RMSEA = 0.074 > 0.05 not acceptable [0.054,0.094] so even CI 
# CFI = 0.935 < 0.95 not acceptable
# TLI = 0.877 < 0.95 not acceptable

modificationindices(fit_dietary_lifestyle_model,sort = TRUE)

# Model fit seems to be poor, we look at modificationindices and evaluate if
# any addition may make intuitive sense. 
# We see the suggestion to add A22 ~~ A18, which when we consider the 
# low correlation that exists between the latent variables, makes sense,
# since people who tend to be active physically (working on their fitness,etc...),
# tend to also be monitoring overall healthiness of their diet.

dietary_lifestyle_correlation_model_2 <- 'dietary_health =~ A20 + A21 + A22
                                        lifestyle_health =~ A9 + A10 + A18
                                        dietary_health ~~ lifestyle_health
                                        A18 ~~ A22'

fit_dietary_lifestyle_model_2 <- cfa(dietary_lifestyle_correlation_model_2,
                                     ordered = c('A9'),
                                     data = df_imputed)
summary(fit_dietary_lifestyle_model_2,standardized = T)
fitmeasures(fit_dietary_lifestyle_model_2)[c('srmr','rmsea','cfi','tli')]
# srmr      rmsea        cfi        tli 
# 0.03664001 0.04748746 0.97673132 0.95013855



semPaths(fit_dietary_lifestyle_model_2,what="paths",whatLabels = "est", layout = "tree2",
         curvature = 3, curve = 0.5, nCharNodes =0,residuals = F,
         sizeLat = 8,sizeLat2 =6 ,title = FALSE, intercepts  = FALSE,
         edge.label.cex =1,node.width = 1.8,sizeMan=3,freeStyle = c("black",3))

# A10, A18 seem inapparorpiate for lifestyle_health, but I still think A18 will 
# be relevant for health.

dietary_lifestyle_correlation_model_3 <- 'dietary_health =~ A20 + A21 + A22
                                        lifestyle_health =~ A9 + A18
                                        dietary_health ~~ lifestyle_health
                                        A18 ~~ A22'
fit_dietary_lifestyle_correlation_3 <- cfa(dietary_lifestyle_correlation_model_3,
                                                 ordered = c('A9'),
                                                 data = df_imputed)
summary(fit_dietary_lifestyle_correlation_3,standardized = T)
fitmeasures(fit_dietary_lifestyle_correlation_3)[c('srmr','rmsea','cfi','tli')]
# srmr      rmsea        cfi        tli 
# 0.03101428 0.04781467 0.98897088 0.96323627 
semPaths(fit_dietary_lifestyle_correlation_3,what="paths",whatLabels = "est", layout = "tree2",
         curvature = 3, curve = 0.5, nCharNodes =0,residuals = F,
         sizeLat = 8,sizeLat2 =6 ,title = FALSE, intercepts  = FALSE,
         edge.label.cex =1,node.width = 1.8,sizeMan=3,freeStyle = c("black",3))

###############################################################################
####### Question 1B: Do people who are overall following healthy dietary and lifestyle habits
####### have good health (latent variables regressed on one observed variable)
###############################################################################
# New variable is A17

overall_health_model <- '
  A17 ~ dietary_health + lifestyle_health
  dietary_health =~ A20 + A21 + A22
  lifestyle_health =~ A9  + A18
  A18 ~~ A22
  '
fit_overall_health <- cfa(overall_health_model,data = df_imputed,
                          ordered = c('A9'))
summary(fit_overall_health, standardized = T)

fitmeasures(fit_overall_health)[c('srmr','rmsea','cfi','tli')]
# srmr      rmsea        cfi        tli 
# 0.04408654 0.07124679 0.95861194 0.89652985
modificationindices(fit_overall_health,sort = TRUE)
# No theoretically sound addition can be made

###############################################################################
####### Question 2: Does the education within a household have a significant 
####### effect on the respondent's wealth
###############################################################################
# We will not consider the education of the respondent as their education at
# this point will not affect their wealth positively

wealth_education_model <- '
                          wealth =~ I2_1 + I2_2 + I2_3 + I2_4 + I2_5 + I2_6 + I2_7 + I2_8 + I2_9 
                          parents_education =~ I1_2 + I1_3
                          wealth ~ parents_education
                          '
# Since we have imputed the data, we can run a regular cfa now

fit_wealth_education_model <- cfa(wealth_education_model,data = df_imputed,
                                  ordered = c('I2_5','I1_3'))
summary(fit_wealth_education_model)

fitmeasures(fit_wealth_education_model)[c('srmr','rmsea','cfi','tli')]
# srmr      rmsea        cfi        tli 
# 0.06630370 0.07146349 0.95001929 0.93607119
modificationindices(fit_wealth_education_model,sort = TRUE)
# According to MI we would benefit from adding I2_3 ~~ I2_4
# and I2_6 ~~ I2_9, the first clearly makes sense since its about TV's
# the second makes some sense when you think about the size of the house

wealth_education_model_2 <- '
                          wealth =~ I2_1 + I2_2 + I2_3+ I2_4 + I2_5 + I2_6 + I2_7 + I2_8 + I2_9 
                          parents_education =~ I1_2 + I1_3
                          wealth ~ parents_education
                          I2_3 ~~ I2_4
                          I2_6 ~~ I2_9
                          '
fit_wealth_education_model_2 <- cfa(wealth_education_model_2,data = df_imputed,
                                    ordered = c('I2_5','I1_3'))
summary(fit_wealth_education_model_2,standardized = T)
fitmeasures(fit_wealth_education_model_2)[c('srmr','rmsea','cfi','tli')]
# srmr      rmsea        cfi        tli 
# 0.05355250 0.05713077 0.96954284 0.95914284

semPaths(fit_wealth_education_model_2,what="paths",whatLabels = "est", layout = "tree2",
         curvature = 3, curve = 0.5, nCharNodes =0,residuals = F,
         sizeLat = 8,sizeLat2 =6 ,title = FALSE, intercepts  = FALSE,
         edge.label.cex =1,node.width = 1.8,sizeMan=3,freeStyle = c("black",3))


###############################################################################
####### Question 3a: Are religious people similar to people who trust
####### public services institutions?
###############################################################################

#D3 leaving slovenia
# We see some concerns regarding multicolinearity with the variables:
# F9_1 F9_2 F9_3, will keep F9_1
# F9_5 F9_7 F9_9, will keep F9_5
# B9 B10 B11, will keep B10

religosity_trust_model_1  <- 'religosity =~ A3_13 + B1_8 + B9 + C13_1 + F9_8
                            trust_ps =~ F9_1 + F9_4 + F9_5 + F9_6 + F9_13
                            religosity ~~ trust_ps
                           '
fit_religosity_trust_model_1 <- cfa(religosity_trust_model_1, data = df_imputed,
                                  ordered = c('F9_8','F9_1','F9_4','F9_5',
                                              'F9_6','F9_13','B1_8','B9'))
summary(fit_religosity_trust_model_1,standardized = T)
fitmeasures(fit_religosity_trust_model_1)[c('srmr','rmsea','cfi','tli')]
# srmr      rmsea        cfi        tli 
# 0.05766951 0.07138732 0.97827223 0.97124266
modificationindices(fit_religosity_trust_model_1,sort = TRUE)
# Add some correlations

religosity_trust_model_2  <- 'religosity =~ A3_13 + B1_8 + B9 + C13_1 + F9_8
                            trust_ps =~ F9_1 + F9_4 + F9_5 + F9_6 + F9_13
                            religosity ~~ trust_ps
                            F9_5 ~~ F9_13
                            B1_8 ~~ B9
                           '
fit_religosity_trust_model_2 <- cfa(religosity_trust_model_2, data = df_imputed,
                                    ordered = c('F9_8','F9_1','F9_4','F9_5',
                                                'F9_6','F9_13','B1_8','B9'))
summary(fit_religosity_trust_model_2,standardized = T)
fitmeasures(fit_religosity_trust_model_2,sort = TRUE)[c('srmr','rmsea','cfi','tli')]
# srmr      rmsea        cfi        tli 
# 0.04413120 0.05022556 0.98981978 0.98568407

semPaths(fit_religosity_trust_model_2,what="paths",whatLabels = "est", layout = "tree2",
         curvature = 3, curve = 0.5, nCharNodes =0,residuals = F,
         sizeLat = 8,sizeLat2 =6 ,title = FALSE, intercepts  = FALSE,
         edge.label.cex =1,node.width = 1.8,sizeMan=3,freeStyle = c("black",3))

# Remove the two variables that are not good indicators

religosity_trust_model_3  <- 'religosity =~  B1_8 + B9  + F9_8
                            trust_ps =~ F9_1 + F9_4 + F9_5 + F9_6 + F9_13
                            trust_ps ~~ religosity
                            F9_5 ~~ F9_13
                            B1_8 ~~ B9'
fit_religosity_trust_model_3 <- cfa(religosity_trust_model_3, data = df_imputed,
                                    ordered = c('F9_8','F9_1','F9_4','F9_5',
                                                'F9_6','F9_13','B1_8','B9'))
fitmeasures(fit_religosity_trust_model_3,sort = TRUE)[c('srmr','rmsea','cfi','tli')]
# srmr      rmsea        cfi        tli 
# 0.03783239 0.04858185 0.99462646 0.99114947
summary(fit_religosity_trust_model_3,standardized = T)
semPaths(fit_religosity_trust_model_3,what="paths",whatLabels = "est", layout = "tree2",
         curvature = 3, curve = 0.5, nCharNodes =0,residuals = F,
         sizeLat = 8,sizeLat2 =6 ,title = FALSE, intercepts  = FALSE,
         edge.label.cex =1,node.width = 1.8,sizeMan=3,freeStyle = c("black",3))

###############################################################################
####### Question 3b: Does religiosity and trust in public services institutions
####### affect the desire for an individual to leave Slovenia
###############################################################################

leave_slovenia_model_1 <- 'D3 ~ religosity + trust_ps
                           religosity =~  B1_8 + B9  + F9_8
                           trust_ps =~ F9_1 + F9_4 + F9_5 + F9_6 + F9_13
                           religosity ~~ trust_ps 
                           F9_5 ~~ F9_13
                           B1_8 ~~ B9
                           '
fit_leave_slovenia_model_1 <- cfa(leave_slovenia_model_1,data = df_imputed,
                                  ordered = c('F9_8','F9_1','F9_4','F9_5','D3',
                                              'F9_6','F9_13','B1_8','B9'))
summary(fit_leave_slovenia_model_1, standardized = T)
fitmeasures(fit_leave_slovenia_model_1,sort = TRUE)[c('srmr','rmsea','cfi','tli')]
# srmr      rmsea        cfi        tli 
# 0.04318239 0.04536246 0.98983120 0.98601790 

semPaths(fit_leave_slovenia_model_1,what="paths",whatLabels = "est", layout = "tree2",
         curvature = 3, curve = 0.5, nCharNodes =0,residuals = F,
         sizeLat = 8,sizeLat2 =6 ,title = FALSE, intercepts  = FALSE,
         edge.label.cex =1,node.width = 1.8,sizeMan=3,freeStyle = c("black",3))



###############################################################################
####### Multigroup modelling a
###############################################################################
gender_age_grouping
# we only want to compare genders
# so we should create a new gender variable based on these labels
# Male: 1,3,5
# Female:, 2,4,6

gender <- ifelse(gender_age_grouping%%2 == 0, "female","male" )
df_imputed["gender"] <- gender
df_imputed <- select(df_imputed, -c("A0_gender_age"))

# Now we take the final models we ended up with for questions 1 and 3 as 
# we want to test for ...

df_imputed['F9_1'] <- slovenia_youth_2013$F9_1 # to resolve problem of a group frequency
# being 0


religosity_trust_model_3


fit_religosity_trust_model_3_configural <-
  cfa(religosity_trust_model_3, 
      data = df_imputed, 
      ordered = c('F9_8','F9_4','F9_5','F9_6','F9_13','B1_8','B9'),
      group = "gender")


fit_religosity_trust_model_3_metric <-
  cfa(religosity_trust_model_3,
      data = df_imputed, 
      ordered = c('F9_8','F9_4','F9_5','F9_6','F9_13','B1_8','B9'),
      group = "gender",
      group.equal = c("loadings"))

fit_religosity_trust_model_3_scalar <-
  cfa(religosity_trust_model_3, 
      data = df_imputed, 
      ordered = c('F9_8','F9_4','F9_5','F9_6','F9_13','B1_8','B9'),
      group = "gender",
      group.equal = c("loadings","intercepts"))

fit_religosity_trust_model_3_strict <-
  cfa(religosity_trust_model_3, 
      data = df_imputed, 
      ordered = c('F9_8','F9_4','F9_5','F9_6','F9_13','B1_8','B9'),
      group = "gender",
      group.equal = c("loadings","intercepts","residuals"))

fit_religosity_trust_model_3_structural <-
  cfa(religosity_trust_model_3,
      data = df_imputed, 
      ordered = c('F9_8','F9_4','F9_5','F9_6','F9_13','B1_8','B9'),
      group = "gender",
      group.equal = c("loadings","intercepts","residuals",
                                       "lv.variances","lv.covariances"))
# function to extract fit indices
model_fit <-  function(lavobject) {
  vars <- c("df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "srmr")
  return(fitmeasures(lavobject)[vars] %>% data.frame() %>% round(2) %>% t())
}
library(purrr)

table_relogisty_trust <- 
  list(model_fit(fit_religosity_trust_model_3_configural), 
       model_fit(fit_religosity_trust_model_3_metric), 
       model_fit(fit_religosity_trust_model_3_scalar), 
       model_fit(fit_religosity_trust_model_3_strict),
       model_fit(fit_religosity_trust_model_3_structural)) %>% 
  reduce(rbind)
rownames(table_relogisty_trust) <- c("Configural", "Metric", "Scalar","Strict","Structural")
table_fit_dietary_lifestyle

table_anova_fit_relogisty_trust <- list(anova(fit_religosity_trust_model_3_configural, 
                                                fit_religosity_trust_model_3_metric),
                    anova(fit_religosity_trust_model_3_metric, 
                          fit_religosity_trust_model_3_scalar),
                    anova(fit_religosity_trust_model_3_scalar, 
                          fit_religosity_trust_model_3_strict),
                    anova(fit_religosity_trust_model_3_strict,
                          fit_religosity_trust_model_3_structural)) %>%  
  reduce(rbind) %>% 
  .[-c(3, 5, 7),]

table_anova_fit_relogisty_trust
# according to our results no level of invariance has been achieved
lavTestScore(fit_religosity_trust_model_3_structural)
# We fail to reject the multivariate score test, so there is nothing we can free 
# to achieve structural invariance

###############################################################################
####### Multigroup modelling b
###############################################################################
leave_slovenia_model_1

fit_leave_slovenia_model_1_configural <-
  cfa(leave_slovenia_model_1, 
      data = df_imputed, 
      ordered = c('F9_8','F9_4','F9_5','F9_6','F9_13','B1_8','B9','D3'),
      group = "gender")


fit_leave_slovenia_model_1_metric <-
  cfa(leave_slovenia_model_1,
      data = df_imputed, 
      ordered = c('F9_8','F9_4','F9_5','F9_6','F9_13','B1_8','B9','D3'),
      group = "gender",
      group.equal = c("loadings"))

fit_leave_slovenia_model_1_scalar <-
  cfa(leave_slovenia_model_1, 
      data = df_imputed, 
      ordered = c('F9_8','F9_4','F9_5','F9_6','F9_13','B1_8','B9','D3'),
      group = "gender",
      group.equal = c("loadings","intercepts"))

fit_leave_slovenia_model_1_strict <-
  cfa(leave_slovenia_model_1, 
      data = df_imputed, 
      ordered = c('F9_8','F9_4','F9_5','F9_6','F9_13','B1_8','B9','D3'),
      group = "gender",
      group.equal = c("loadings","intercepts","residuals"))

fit_leave_slovenia_model_1_structural <-
  cfa(leave_slovenia_model_1,
      data = df_imputed, 
      ordered = c('F9_8','F9_4','F9_5','F9_6','F9_13','B1_8','B9','D3'),
      group = "gender",
      group.equal = c("loadings","intercepts","residuals",
                      "lv.variances","lv.covariances"))

table_leave_sloveniat <- 
  list(model_fit(fit_leave_slovenia_model_1_configural), 
       model_fit(fit_leave_slovenia_model_1_metric), 
       model_fit(fit_leave_slovenia_model_1_scalar), 
       model_fit(fit_leave_slovenia_model_1_strict),
       model_fit(fit_leave_slovenia_model_1_structural)) %>% 
  reduce(rbind)
rownames(table_leave_sloveniat) <- c("Configural", "Metric", "Scalar","Strict","Structural")

table_anova_fit_leave_slovenia <- list(anova(fit_leave_slovenia_model_1_configural, 
                                              fit_leave_slovenia_model_1_metric),
                                        anova(fit_leave_slovenia_model_1_metric, 
                                              fit_leave_slovenia_model_1_scalar),
                                        anova(fit_leave_slovenia_model_1_scalar, 
                                              fit_leave_slovenia_model_1_strict),
                                        anova(fit_leave_slovenia_model_1_strict,
                                              fit_leave_slovenia_model_1_structural)) %>%  
  reduce(rbind) %>% 
  .[-c(3, 5, 7),]

table_anova_fit_leave_slovenia
lavTestScore(fit_leave_slovenia_model_1_structural)
# same thing

