# set working directory to current file location in R-Studio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# if you use another IDE, you can try the following:
# setwd(getSrcDirectory(function(){})[1])

# include getData function from script
if(!exists("getData", mode="function")) source("../common/data_filtered.R")

# data frame excludes variables with near zero variance and with NAs
# You can specify the proportion of allowed NAs with a parameter 
# eg. getData(na_proportion = 0.1)
data <- getData()

mice::md.pattern(data)

plot(sort(data$time), pch='.', type='o', 
     col='blue', lwd=2 , 
     main = 'MIDUS Data \nTime to death')


# Random Forest Survival
#
# install.packages('ranger')
library(ranger)
# install.packages('survival')
library(survival)


survival_formula <- formula(
  paste('Surv(', 'time', ',', 'censor', ') ~ ',
        paste(
          colnames(
            data[,!(colnames(data) %in% 
                      c("M2ID", "time", "censor"))
            ]
        ), 
        collapse = '+')
  )
)

survival_model <- ranger(survival_formula,
                         data = data,  
                         seed = 1234,
                         importance = 'permutation',
                         mtry = 2,
                         verbose = TRUE,
                         num.trees = 50,
                         write.forest=TRUE)

summary(survival_model)

sort(survival_model$variable.importance)

plot(survival_model$unique.death.times, survival_model$survival[1,], type='l', col='orange', ylim=c(0.4,1))
lines(survival_model$unique.death.times, survival_model$survival[56,], col='blue')
