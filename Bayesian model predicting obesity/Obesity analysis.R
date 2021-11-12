# clears workspace:  
rm(list=ls())

library(R2jags)
library(dplyr)
library(tidyr)
library(stringr) # subset of a string
library(hablar) # convert function

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Educaiton annd obesity data data from http://appsso.eurostat.ec.europa.eu/nui/submitViewTableAction.do

# read data files and retain: BMI, GEO, Value
Total_total <- read.csv("data/education/Total Levels Total.csv") %>% select(BMI, GEO,Value)
Total_female <- read.csv("data/education/Total Levels Female.csv") %>% select(Value)
Total_male <- read.csv("data/education/Total Levels Male.csv") %>% select(Value)
Level02_female <- read.csv("data/education/Levels 0-2 Female.csv") %>% select(Value)
Level02_male <- read.csv("data/education/Levels 0-2 Male.csv") %>% select(Value)
Level02_total <- read.csv("data/education/Levels 0-2 Total.csv") %>% select(Value)
Level34_female <- read.csv("data/education/Levels 3-4 Female.csv") %>% select(Value)
Level34_male <- read.csv("data/education/Levels 3-4 Male.csv")%>% select(Value)
Level34_total <- read.csv("data/education/Levels 3-4 Total.csv") %>% select(Value)
Level58_female <- read.csv("data/education/Levels 5-8 Female.csv")%>% select(Value)
Level58_male <- read.csv("data/education/Levels 5-8 Male.csv") %>% select(Value)
Level58_total <- read.csv("data/education/Levels 5-8 Total.csv") %>% select(Value)

education_data_obesity <- cbind(Total_female,"Total Average" = Total_total[,1], "Total Male" = Total_male[,1], 
  "Level 0-2 Female" = Level02_female[,1],"Level 0-2 Male" = Level02_male[,1], 
  "Level 3-4 Female" = Level34_female[,1],"Level 3-4 Male" = Level34_male[,1],
  "Level 5-8 Female" = Level58_female[,1],"Level 5-8 Male" = Level58_male[,1])

# We want to estimate the % of people who are obese and % who are underweight
# 1. Generic obesity/overweight predictions in a european country by considering 
#    all the countries in the list first 
# 2. depending on education level
# 3. 2 + gender
# 4. 3 but using regression

################################# Question 1 #################################
## 1. We will only consider total data sets at the moment (not gender split)##
# Total_total has the base line numbers, I will get rid of first 6 rows
# (aggregate european union)
# We will also convert the %'s into per 10,000 data
q1_data <- Total_total[-1:-6,]
sapply(q1_data,class)
q1_data$Value <- as.numeric(q1_data$Value)
q1_data$Value <- q1_data$Value*100  # /100 * 10000

# Get rid of NA rows
q1_data <- q1_data[is.na(q1_data$Value) == FALSE,]

# now split data into underweight, normal and obese
q1_data_underweight <- q1_data[q1_data$BMI == "Underweight",]
q1_data_normal <- q1_data[q1_data$BMI == "Normal",]
q1_data_obese <- q1_data[q1_data$BMI == "Obese",]
  
# We make the most basic model for obese people now 
model1_obesity <- 
  "model {
    for (i in 1:n){ 
      obese[i] ~ dbin(theta,num_people[i])
    }
      theta ~ dbeta( 1 , 1 ) 
}"
writeLines(model1_obesity,"model1_obesity.txt")

# Data
obese <- round(q1_data_obese$Value)
num_people <- rep(10000,length(obese))
n <- dim(q1_data_obese)[1]

data <- list(
  obese = obese,
  num_people = num_people,
  n=n
) # to be passed on to JAGS

myinits=NULL

# parameters to be monitored:	
parameters <- c("theta")

samples_q1 <- jags(data, inits=myinits, parameters,
                model.file = "model1_obesity.txt", n.chains=1, n.iter=10000, 
                n.burnin=500, n.thin=1, DIC=F,set.seed(0857263))
# Inspect output
theta      <- samples_q1$BUGSoutput$sims.list$theta
quantile(theta,c(.025,.975)) 
#   2.5%        97.5% 
#   0.1771368   0.1798546 

plot(density(theta),main = 'Density of theta',xlab='theta',ylab='')
abline(v = quantile(theta,.025),lty = 2, col = 'red')
abline(v = quantile(theta,.975),lty = 2, col = 'red')
abline(v = mean(theta),lty = 2, col = 'blue')

################################# Question 2 #################################
q2_data <- cbind(Total_total,"Level02" = Level02_total[,1],"Level34" = Level34_total[,1],
                 "Level58" = Level58_total[,1])[-1:-6,]
colnames(q2_data)[3] = "Average"
q2_data <- pivot_longer(q2_data,3:6,names_to = "Education level",
                        values_to = "Value")
q2_data <- as.data.frame(q2_data)
q2_data$Value <- as.numeric(q2_data$Value)
q2_data$Value <- q2_data$Value*100  # /100 * 10000

# Get rid of NA rows
q2_data <- q2_data[is.na(q2_data$Value) == FALSE,]

q2_data_obese <- q2_data[q2_data$BMI == "Obese",]
q2_data_underweight <- q2_data[q2_data$BMI == "Underweight",]
q2_data_normal <- q2_data[q2_data$BMI == "Normal",]



model2_obesity <- 
  "model {
    for (i in 1:n){ 
      obese[i] ~ dbin(theta[eid[i]],num_people[i])
    }
    for (g in 1:eg){
      theta[g] ~ dbeta( 1 , 1 ) 
    }
}"
writeLines(model2_obesity,"model2_obesity.txt")

obese <- round(q2_data_obese$Value)
num_people <- rep(10000,length(obese))
n <- dim(q2_data_obese)[1]
ed_id <- ifelse(q2_data_obese$`Education level` == "Average", 1 ,
                ifelse(q2_data_obese$`Education level` == "Level02",2,
                       ifelse(q2_data_obese$`Education level` == "Level34",3,4))) 
eg <- 4

data <- list(
  obese = obese,
  num_people = num_people,
  eid=ed_id,
  eg=eg,
  n=n
) # to be passed on to JAGS

myinits=NULL

# parameters to be monitored:	
parameters <- c("theta")

samples_q2 <- jags(data, inits=myinits, parameters,
                model.file = "model2_obesity.txt", n.chains=1, n.iter=10000, 
                n.burnin=500, n.thin=1, DIC=F,set.seed(0857263))
### Inspect output
theta_q2      <- samples_q2$BUGSoutput$sims.list$theta
quantile(theta_q2[,1],c(.025,.975)) # Average 
#   2.5%        97.5% 
#   0.1770761   0.1798327 
quantile(theta_q2[,2],c(.025,.975)) # Education level 0-2
#   2.5%        97.5% 
#   0.1986353   0.2014674 
quantile(theta_q2[,3],c(.025,.975)) # Education level 3-4
#   2.5%        97.5% 
#   0.1846941   0.1874830 
quantile(theta_q2[,4],c(.025,.975)) # Education level 5-8
#   2.5%        97.5% 
#   0.1292326   0.1315976

plot(density(theta_q2[,1]),main ='' ,xlab='',ylab='')
abline(v = quantile(theta_q2[,1],.025),lty = 2, col = 'red')
abline(v = quantile(theta_q2[,1],.975),lty = 2, col = 'red')
abline(v = mean(theta_q2[,1]),lty = 2, col = 'blue')

plot(density(theta_q2[,2]),main ='' ,xlab='',ylab='')
abline(v = quantile(theta_q2[,2],.025),lty = 2, col = 'red')
abline(v = quantile(theta_q2[,2],.975),lty = 2, col = 'red')
abline(v = mean(theta_q2[,2]),lty = 2, col = 'blue')

plot(density(theta_q2[,3]),main ='' ,xlab='',ylab='')
abline(v = quantile(theta_q2[,3],.025),lty = 2, col = 'red')
abline(v = quantile(theta_q2[,3],.975),lty = 2, col = 'red')
abline(v = mean(theta_q2[,3]),lty = 2, col = 'blue')

plot(density(theta_q2[,4]),main ='' ,xlab='',ylab='')
abline(v = quantile(theta_q2[,4],.025),lty = 2, col = 'red')
abline(v = quantile(theta_q2[,4],.975),lty = 2, col = 'red')
abline(v = mean(theta_q2[,4]),lty = 2, col = 'blue')

Level02_diff <- theta_q2[,1] - theta_q2[,2]
mean(Level02_diff) #-0.02156169
quantile(Level02_diff,c(.025,.975))
#   2.5%        97.5% 
#   -0.02354293 -0.01958965 

Level34_diff <- theta_q2[,1] - theta_q2[,3]
mean(Level34_diff) #-0.007599899
quantile(Level34_diff,c(.025,.975))
#   2.5%          97.5% 
#   -0.009590799  -0.005663713 

Level58_diff <- theta_q2[,1] - theta_q2[,4]
mean(Level58_diff) #0.04806023
quantile(Level58_diff,c(.025,.975))
#   2.5%        97.5% 
#   0.04626238  0.04988437 

##### NEXT WE ADD GENDER VARIABLE
################################# Question 3 #################################
q3_data <- cbind(Total_total,"Average" = Total_female[,1],"Average" = Total_male[,1],"Level02" = Level02_total[,1], "Level02" = Level02_female[,1],
                 "Level02" = Level02_male[,1],"Level34" = Level34_total[,1],
                 "Level34" = Level34_female[,1], "Level34" = Level34_male[,1],
                 "Level58" = Level58_total[,1], "Level58" = Level58_female[,1],
                 "Level58" = Level58_male[,1])[-1:-6,]
colnames(q3_data)[3] = "Average"
q3_data <- pivot_longer(q3_data,3:6,names_to = "Education level",
                        values_to = "Value")
q3_data <- as.data.frame(q3_data)
q3_data <- cbind(q3_data,Gender = rep(c("Total","Female","Male"),nrow(q3_data)/3))
q3_data$Value <- as.numeric(q3_data$Value)
q3_data$Value <- q3_data$Value*100  # /100 * 10000

# Get rid of NA rows
q3_data <- q3_data[is.na(q3_data$Value) == FALSE,]

q3_data_obese <- q3_data[q3_data$BMI == "Obese",]
q3_data_underweight <- q3_data[q3_data$BMI == "Underweight",]
q3_data_normal <- q3_data[q3_data$BMI == "Normal",]

q4_data_obese <- q3_data_obese

model3_obesity <- 
  "model {
    for (i in 1:n){ 
      obese[i] ~ dbin(theta[ed_id[i],gen_id[i]],num_people[i])
    }
    for (x in 1:ed){
      for(y in 1:gd){
        theta[x,y] ~ dbeta( 1 , 1 ) 
      }
    }
}"
writeLines(model3_obesity,"model3_obesity.txt")

obese <- round(q3_data_obese$Value)
num_people <- rep(10000,length(obese))
n <- dim(q3_data_obese)[1]
ed_id <- as.factor(ifelse(q3_data_obese$`Education level` == "Average", 1 ,
                ifelse(q3_data_obese$`Education level` == "Level02",2,
                       ifelse(q3_data_obese$`Education level` == "Level34",3,4))))
gen_id <- as.factor(ifelse(q3_data_obese$Gender == "Total",1,
                 ifelse(q3_data_obese$Gender =="Female",2,3)))
ed <- length(levels(ed_id))
gd <- length(levels(gen_id))

data <- list(
  obese = obese,
  num_people = num_people,
  ed_id = ed_id,
  gen_id = gen_id,
  ed = ed,
  gd = gd,
  n = n
) # to be passed on to JAGS

myinits=NULL

# parameters to be monitored:	
parameters <- c("theta")

samples_q3 <- jags(data, inits=myinits, parameters,
                   model.file = "model3_obesity.txt", n.chains=1, n.iter=10000, 
                   n.burnin=500, n.thin=1, DIC=F)
### Inspect output
theta_q3      <- samples_q3$BUGSoutput$sims.matrix
# theta_q3:
# - theta[1,1] is average education and both genders
# - theta[1,2] is average education and female
# - theta[1,3] is average education and male
# - theta[2,1] is level02 and both genders
# - theta[2,2] is level02 and female
# - theta[2,3] is level02 and male
# - theta[3,1] is level34 and both genders
# - theta[3,2] ...
# - theta[3,3] ...
# - theta[4,1] is level58 and both genders
# - theta[4,2] ...
# - theta[4,3] ...
quantile(theta_q3[,"theta[1,1]"],c(.025,.975)) # Average 
#   2.5%        97.5% 
#   0.1747616   0.1774504
quantile(theta_q3[,"theta[2,1]"],c(.025,.975)) # level02 both genders
#   2.5%        97.5% 
#   0.1986165   0.2014653
quantile(theta_q3[,"theta[3,1]"],c(.025,.975)) # level34 both genders
#   2.5%        97.5% 
#   0.1846764   0.1874758
quantile(theta_q3[,"theta[4,1]"],c(.025,.975)) # level58 both genders
#   2.5%        97.5% 
#   0.129218    0.131633

# Now we look at the gender separations that couldn't be seen in previous questions
quantile(theta_q3[,"theta[1,2]"],c(.025,.975)) # Average education Female
#   2.5%        97.5% 
#   0.1747616   0.1774504 
quantile(theta_q3[,"theta[1,3]"],c(.025,.975)) # Average education Male
#   2.5%        97.5% 
#   0.1794751   0.1822299

quantile(theta_q3[,"theta[2,2]"],c(.025,.975)) # level02 education Female
#   2.5%        97.5% 
#   0.2119456   0.2148923 
quantile(theta_q3[,"theta[2,3]"],c(.025,.975)) # level02 education Male
#   2.5%        97.5% 
#   0.1798193   0.1825991 
quantile(theta_q3[,"theta[3,2]"],c(.025,.975)) # level34 education Female
#   2.5%        97.5% 
#   0.1773743   0.1800335 
quantile(theta_q3[,"theta[3,3]"],c(.025,.975)) # level34 education Male
#   2.5%        97.5% 
#   0.1908330   0.1936487
quantile(theta_q3[,"theta[4,2]"],c(.025,.975)) # level58 education Female
#   2.5%        97.5% 
#   0.1159259   0.1182713 
quantile(theta_q3[,"theta[4,3]"],c(.025,.975)) # level58 education Male
#   2.5%        97.5% 
#   0.1433989   0.1459569 


# Exploring differences in females depending on education level
Level02_average_female_diff <- theta_q3[,"theta[2,2]"] - theta_q3[,"theta[1,2]"]
Level34_average_female_diff <- theta_q3[,"theta[3,2]"] - theta_q3[,"theta[1,2]"]
Level58_average_female_diff <- theta_q3[,"theta[4,2]"] - theta_q3[,"theta[1,2]"]

Level02_average_male_diff <- theta_q3[,"theta[2,3]"] - theta_q3[,"theta[1,3]"]
Level34_average_male_diff <- theta_q3[,"theta[3,3]"] - theta_q3[,"theta[1,3]"]
Level58_average_male_diff <- theta_q3[,"theta[4,3]"] - theta_q3[,"theta[1,3]"]

# Female
mean(Level02_average_female_diff) # 0.03733227
mean(Level34_average_female_diff) # 0.002620921
mean(Level58_average_female_diff) # -0.05897876
quantile(Level02_average_female_diff,c(.025,.975)) #[0.03527368,0.039232043]
quantile(Level34_average_female_diff,c(.025,.975)) #[0.0006711643,0.0045210645]
quantile(Level58_average_female_diff,c(.025,.975)) #[-0.06077669, -0.05719929]

# Male
mean(Level02_average_male_diff) # 0.0003696049
mean(Level34_average_male_diff) # 0.01139339
mean(Level58_average_male_diff) # -0.03616932
quantile(Level02_average_male_diff,c(.025,.975)) #[-0.001597924,0.002341649]
quantile(Level34_average_male_diff,c(.025,.975)) #[0.00941258,0.01336321]
quantile(Level58_average_male_diff,c(.025,.975)) #[-0.03800425,-0.03425814]


#### Maybe i should try to have a model that is a regression and compare ####
# Will try a linear regression
# % obese = B0 + B1 * Level of education + B2 * Gender
# I want to try both an uniformative prior and an informative prior
# Informative prior will only be for level of education, assuming 
# that the higher the level of education the lower the rate of obesity

#### Uninformative prior

# The mean for the regression should look like this
# mu[i] = B0 + B1 * edu1 + B2 * edu2 + B3 * edu3 + B4 * Gender1 + B4 * Gender2
# The dummy variables will be so that edu1=edu2=edu3=0 implies average
# Similiarly for Gender1/2

q4_data_obese
# Create dummy variables
education_dummy <- data.frame(matrix(ncol = 3,nrow=nrow(q4_data_obese)))
colnames(education_dummy) <- c("edu1","edu2","edu3")

education_dummy$edu1  <- ifelse(q4_data_obese$`Education level` == "Average",0,1)
education_dummy$edu2  <- ifelse(q4_data_obese$`Education level` == "Average",0,
                                ifelse(q4_data_obese$`Education level` == "Level02",0,1))
education_dummy$edu3  <- ifelse(q4_data_obese$`Education level` != "Level58",0,1)

gender_dummy <- data.frame(matrix(ncol = 2,nrow=nrow(q4_data_obese)))
colnames(gender_dummy) <- c("gender1","gender2")

gender_dummy$gender1 <- ifelse(q4_data_obese$Gender == "Total",0,1)
gender_dummy$gender2 <- ifelse(q4_data_obese$Gender == "Male",1,0) # 11 is male 10 is female
# Now adjust dataset to include these columns and remove the other ones

q4_data_obese <- mutate(q4_data_obese,education_dummy,gender_dummy)
q4_data_obese <- q4_data_obese[-c(3,5)]

model3_regression_obesity <- 
  "model {
    for(i in 1:n){
      obese[i] ~ dbin(theta[ed_id[i],gen_id[i]],num_people[i]) # likelihood
    }
    # priors
    for(x in 1:4){
      for(y in 1:3){
        theta[x,y] ~ dnorm(mu[x,y],tau) 
        mu[x,y] <- B0 + B1 * edu1[x] + B2 * edu2[x] + 
                   B3 * edu3[x] + B4 * gender1[y] + B5 * gender2[y]
      }
    }
    # priors for regression
    B0 ~ dunif(0,1)
    B1 ~ dnorm(0,0.01)
    B2 ~ dnorm(0,0.01)
    B3 ~ dnorm(0,0.01)
    B4 ~ dnorm(0,0.01)
    B5 ~ dnorm(0,0.01)
    sigma ~ dunif(0,100)
    tau <- 1/(sigma*sigma) # precision, i.e. 1/variance
}"
writeLines(model3_regression_obesity,"model3_regression_obesity.txt")

obese <- round(q4_data_obese$Value)
num_people <- rep(10000,length(obese))
n <- dim(q4_data_obese)[1]
# dont need to change this since its the same...
ed_id <- as.factor(ifelse(q3_data_obese$`Education level` == "Average", 1 ,
                          ifelse(q3_data_obese$`Education level` == "Level02",2,
                                 ifelse(q3_data_obese$`Education level` == "Level34",3,4))))
gen_id <- as.factor(ifelse(q3_data_obese$Gender == "Total",1,
                           ifelse(q3_data_obese$Gender =="Female",2,3)))
ed <- length(levels(ed_id))
gd <- length(levels(gen_id))
edu1 <- c(0,1,1,1)
edu2 <- c(0,0,1,1)
edu3 <- c(0,0,0,1)
gender1 <- c(0,1,1)
gender2 <- c(0,0,1)

data <- list(
  obese = obese,
  num_people = num_people,
  ed_id = ed_id,
  gen_id = gen_id,
  #ed = ed,
  #gd = gd,
  edu1=edu1,
  edu2=edu2,
  edu3=edu3,
  gender1=gender1,
  gender2=gender2,
  n = n
) # to be passed on to JAGS

myinits=NULL

parameters <- c("theta","mu","sigma","B0","B1","B2","B3","B4","B5")

samples_q4 <- jags(data, inits=myinits, parameters,
                   model.file = "model3_regression_obesity.txt", n.chains=1, n.iter=10000, 
                   n.burnin=500, n.thin=1, DIC=F)
samples_q4$BUGSoutput$summary
