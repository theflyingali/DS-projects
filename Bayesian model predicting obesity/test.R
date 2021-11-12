# clears workspace:  
rm(list=ls())

library(R2jags)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

obesity <- read.csv("common-health-problems-of-students-examined-obesity-annual/common-health-problems-of-students-examined-overweight-annual.csv")

# aggregate student data so we first look generically, then look at the age groups
agg_obesity <- aggregate(obesity$per_10000_examined,by = list(Year = obesity$year,Gender = obesity$gender),FUN = mean)
colnames(agg_obesity)[3] <- "per_10000"


aggregate(agg_obesity$per_10000,by = list(agg_obesity$Gender),FUN = mean) # 1113 female, 1422 male
# this tells us that young males are 30% more likely to be obese

# Lets do some bayesian analysis

# Model
model1 <- 
"model {
    for (i in 1:n){ 
      obese[i] ~ dbin(theta[gid[i]],num_people[i])
    }
    for (g in 1:ng){ 
      theta[g] ~ dbeta( 1 , 1 ) 
    }
}"
writeLines(model1,"model1_obesity.txt")

# Data
obese <- round(agg_obesity$per_10000)
num_people <- rep(10000,length(obese))
gen_id <- ifelse( agg_obesity$Gender=="Male" , 1 , 2 ) 
n <- dim(agg_obesity)[1]
ng <- 2

data <- list(
  obese = obese,
  num_people = num_people,
  gid=gen_id,
  ng=ng,
  n=n
) # to be passed on to JAGS

myinits=NULL

# parameters to be monitored:	
parameters <- c("theta")

samples <- jags(data, inits=myinits, parameters,
                model.file = "model1_obesity.txt", n.chains=1, n.iter=10000, 
                n.burnin=500, n.thin=1, DIC=F)

# Now the values for the monitored parameters are in the "samples" object, 
# ready for inspection.
theta      <- samples$BUGSoutput$sims.list$theta

probdiff=theta[,1]-theta[,2]
mean(probdiff)
quantile(probdiff,c(.025,.975))


## This is a good start now I should continue with looking at the sessions and what is covered
## then try to build on this and use world obesity data