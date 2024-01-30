library(caret)
library(pec)
library(survival)
library(survminer)
library(glmnet)
library(bnstruct)
library(mice)
library(janitor)
library(hdnom)
library(tidyverse)

if(!exists("ggforest_custom", mode="function")) source("./ggforest.R")

# include getData function from script
# if(!exists("getData", mode="function")) source("../common/data_filtered.R")
# if(!exists("getData", mode="function")) source("./data_filtered.R")

# data frame excludes variables with near zero variance and with NAs
# You can specify the proportion of allowed NAs with a parameter 
# eg. getData(na_proportion = 0.1)
# We use 0.001 here to illustrate imputation, but with few features for fast
# computation
# data <- getData(na_proportion = 0.1)

# load(
#   file = "./data_final_analysis.rda"
# )

data <- data_final_analysis

sum(data$censor)

# remove "admin" vars
#c("SAMPLMAJ", "A1STATUS", "A1PAGE_M2", "A1PBYEAR", "A1PBYEAR_2019", "M1BYEAR_FLAG", "A1PI_MO", "A1PI_YR")
# data <- data %>% select(-SAMPLMAJ, -A1STATUS, -A1PAGE_M2, -A1PBYEAR, -A1PBYEAR_2019, -M1BYEAR_FLAG, -A1PI_MO, -A1PI_YR)
# data <- data %>% select(
#   -any_of(
#     c(
#       "SAMPLMAJ", 
#       "A1STATUS", 
#       "A1PAGE_M2", 
#       "A1PBYEAR", 
#       "A1PBYEAR_2019", 
#       "M1BYEAR_FLAG", 
#       "A1PI_MO", 
#       "A1PI_YR",
#       "A1PSXEDAG",
#       "A1PCHILD13",
#       "A1PCHILD15",
#       "A1PCHILD16",
#       "A1PCHILD18",
#       "A1PC06",
#       "A1PC713",
#       "A1PC1417",
#       "A1PC18",
#       "A1PD4",
#       "A1PD5",
#       "A1PD6",
#       "A1PD7",
#       "A1SG1A",
#       "A1SG1B",
#       "A1SG1C",
#       "A1SG1D",
#       "A1SG1E",
#       "A1SG1F",
#       "A1SG1G",
#       "A1SG1H",
#       "A1SG1I",
#       "A1SG1J",
#       "A1SG1K",
#       "A1SG1L",
#       "A1SG1M",
#       "A1SG1N",
#       "A1SG1O",
#       "A1SG1P",
#       "A1SG1Q",
#       "A1SG1R",
#       "A1SG2A",
#       "A1SG2B",
#       "A1SG2C",
#       "A1SG2D",
#       "A1SG2E",
#       "A1SG2F",
#       "A1SG2G",
#       "A1SG2H",
#       "A1SG2I",
#       "A1SG2J",
#       "A1SG2K",
#       "A1SG2L",
#       "A1SG2M",
#       "A1SG2N",
#       "A1SG2O",
#       "A1SG2P",
#       "A1SG2Q",
#       "A1SG2R",
#       "A1SG2S",
#       "A1SG2T",
#       "A1SG2U",
#       "A1SG3",
#       "A1SG4",
#       "A1SG5",
#       "A1SG6",
#       "A1SG7",
#       "A1SG8",
#       "A1SS6_1",
#       "A1SS6_2",
#       "A1SS7"
#       )
#     )
#   )


# invert censor variable into status
# 0 = event not observed; 1 = event observed
data <- data %>% mutate(censor = !censor)

# data_bu <- data

recode <- function(d) {
  d %>% mutate(
    A1PA13 = ifelse(
      is.na(A1PA13), NA, 
        ifelse(
          A1PA13 == 2,
          -1,
          ifelse(
            A1PA13 == 3,
            0,
            A1PA13
          )
    )),
    NO_RELIGIOUS_PREF = ifelse(
      is.na(A1SR1), NA,
        ifelse(
          (A1SR1 == 31 | A1SR1 == 32),
          1,
          0
    )),
    OTHER_LANGUAGE = ifelse(
      is.na(A1SE5), NA,
        ifelse(
          A1SE5 >= 3,
          1,
          0
    )),
    MARRIED = ifelse(
      is.na(A1PB17), NA,
        ifelse(
          A1PB17 > 1,
          0,
          A1PB17
    )),
    A1PA80 = ifelse(
      is.na(A1PA80), NA, c(1,-1,0,0,0,-2)[A1PA80]
    ),
    A1PANGIN = ifelse(
      is.na(A1PANGIN), NA, c(3,2,1,0,0,4)[A1PANGIN]
    ),
    A1PA37 = ifelse(
      is.na(A1PA37), NA, c(1,-1,0)[A1PA37]
    )
  )
}

data <- recode(data)

data <- data %>% select(
  -any_of(
    c(
      "A1SR1",  # Religious preference 1st choice
      "A1SE5",  # Language spoken
      "A1PB17", # Marital status currently
      "A1SR3"   # Explore different religions or stick to one
    )
  )
)

recode_dichotomous <- function(x) {
  return(ifelse(x == 2, 0, x))
}

data <- data %>% mutate_at(
  vars(
    c(
      "A1PRSEX",
      "A1PA11",
      "A1PA16",
      "A1PA17",
      "A1PA18",
      "A1PA23",
      "A1PA29",
      "A1PA33",
      "A1PA34",
      "A1PA36",
      "A1PA87",
      "A1PB3A",
      "A1PB3B",
      "A1PB3E",
      "A1PB3F",
      "A1PC1",
      "A1SA38A",
      "A1SALTER",
      "A1SA44A",
      "A1SE3",
      "A1SE4"
    )
  ), 
  ~recode_dichotomous(.)
)

# factorize dichotomous variables
for(variable in c(
  "A1PRSEX",
  "A1PA11",
  "A1PA16",
  "A1PA17",
  "A1PA18",
  "A1PA23",
  "A1PA29",
  "A1PA33",
  "A1PA34",
  "A1PA36",
  "A1PA87",
  "A1PB3A",
  "A1PB3B",
  "A1PB3E",
  "A1PB3F",
  "A1PC1",
  "A1SA38A",
  "A1SALTER",
  "A1SA44A",
  "A1SE3",
  "A1SE4",
  "MARRIED",
  "OTHER_LANGUAGE",
  "NO_RELIGIOUS_PREF"
)) {
  data[,variable] <- as.factor(data[,variable])
}


# data <- data %>% filter(A1PRSEX == 1) %>% select(-A1PRSEX) # filter for males only
# data <- data %>% filter(A1PRSEX == 0) %>% select(-A1PRSEX) # filter for females only

# We have to remove all labels from the spss .sav file or MICE throws wierd
# errors.
library(sjlabelled)
data <- remove_all_labels(data)

# skimr::skim(data)

# INFO:
# Multinomial logistic regression imputation with 
# imputeMulti::multinomial_impute gives good results but is computational 
# heavy and can only handle around 10 categorical variables at once. 
#  
# data.factorCols <- data %>% select(where(is.factor))
# imputedFactors <- imputeMulti::multinomial_impute(data.factorCols[,1:10])
# skimr::skim(imputeMulti::get_parameters(imputedFactors))
# View(imputeMulti::get_parameters(imputedFactors) %>% select(-theta_y))


# ---- Descriptive Stats ----
pdf(file = "./output/plots_descriptive.pdf")

plot(sort(data$time), pch='.', type='o', 
     col='blue', lwd=2 , 
     main = 'MIDUS Data \nTime to death')


# histogram of survival times shows a somewhat symmetric distribution
hist(data$time)

# survival function
fit.surv <- survfit(Surv(time, censor) ~ 1,
                    data = data)
ggsurvplot(fit.surv,conf.int=FALSE, data = data)
ggsave("./output/plots_descriptive_survplot.pdf", device = "pdf")

summary(fit.surv, times = c(50,60,70,80,90,100))

dev.off()

# ---- Regularized Cox Regression ----
use_cache    <- TRUE
max_times    <- max(data$time)
num_splits   <- 5    # number of splits for the outer loop
num_mult_imp <- 1    # number of multiple imputations 
cox_nfolds   <- 5    # number of folds for inner cross validation 
enet_alpha   <- 0.05 # elastic net mixing parameter
form <- as.formula(  # formula used for dummy coding
  paste(
    "~", 
    paste(
      colnames(data %>% select(-time, -censor)), 
      collapse = " + "), 
    sep = " "))

set.seed(1234)

# outer resampling
trainIndices.outer <- createDataPartition(
  pull(data, "time"),
  p = .7,
  list = FALSE,
  times = num_splits
)

preProcessSplit <- function(d) {
  # d <- data
  data_imp.surv   <- d %>% select( time,  censor)
  data_imp        <- d %>% select(-time, -censor)
  
  # impute missings
  data_imp   <- complete(
    mice(data_imp, m = num_mult_imp, method = "pmm", printFlag = FALSE)
  )
  
  # scale numeric variables
  data_imp <- data_imp %>% mutate(across(where(is.numeric), scale))
  
  # data frame to matrix
  data_mx <- model.matrix(form, data_imp)
  data_mx <- clean_names(data_mx[,-1])
  
  data_imp <- as.data.frame(data_mx)
  
  data_imp$time   <- data_imp.surv$time
  data_imp$censor <- data_imp.surv$censor
  
  return(
    list(
      "df" = data_imp,
      "mx" = data_mx
    )
  )
}

outerLoopIndex <- 1 # for testing only

results.outer <- lapply(1:num_splits, function(outerLoopIndex) {
  print(paste0("starting outer loop: ", outerLoopIndex))
  file_path <- paste0("./cache/loop_", outerLoopIndex, "-imputed_data.rda")
  file_path_fw <- paste0("./cache/loop_", outerLoopIndex, "-forward_selection.rda")
  
  if(use_cache & file.exists(file_path)) {
    load(
      file = file_path
    )
  } else {
    data.train <- data[ trainIndices.outer[, outerLoopIndex], ]
    data.test  <- data[-trainIndices.outer[, outerLoopIndex], ]
    
    print("starting preprocessing training set")
    ls.train <- preProcessSplit(data.train)
    print("starting preprocessing testing set")
    ls.test  <- preProcessSplit(data.test)
    print("finished preprocessing")
    
    save(list = c("ls.train", "ls.test"), file = file_path)
  }
  
  data.train <- ls.train[["df"]]
  data.test  <- ls.test[["df"]]
  
  data.train.mx <- ls.train[["mx"]]
  data.test.mx  <- ls.test[["mx"]]

  train.surv <- Surv(data.train$time, data.train$censor)
  
  print("starting regularized cox regression")
  
  fit.coxnet <- glmnet(
    data.train.mx, 
    train.surv, 
    family = "cox", 
    alpha = enet_alpha
  )
  
  pdf(file = paste0("./output/plots_loop_", outerLoopIndex, ".pdf") )
  
  plot(fit.coxnet, xvar = "lambda")
  
  cv.coxnet <- cv.glmnet(
    data.train.mx,
    train.surv,
    family="cox",
    type.measure="C",
    nfolds = cox_nfolds,
    alpha=enet_alpha
  )
  # tune alpha value
  # aenet <- fit_aenet(
  #     data.train.mx,
  #     train.surv,
  #     nfolds = cox_nfolds,
  #     rule = "lambda.1se", seed = c(5, 7), parallel = TRUE
  # )
  
  plot(cv.coxnet)
  
  print("finished regularized cox regression")
  
  
  # Variable Importance
  # All numerical variables are scaled, so we should be able to just
  # order the absolute coefficient values, right?
  coefList <- coef(cv.coxnet, s = "lambda.1se")
  coefList <- data.frame(coefList@Dimnames[[1]][coefList@i+1],coefList@x)
  names(coefList) <- c("var", "val")
  coefList <- coefList %>% arrange(-abs(val))
  
  # linear predictor
  lp <- predict(fit.coxnet,
                newx=data.test.mx,
                s=cv.coxnet$lambda.1se,
                type="link")
  data.test$prognosis <- ifelse(lp>0,"poor","good")
  # data.test$prognosis <- ifelse(data.test$a1prsex1,"Männer","Frauen")
  fit.surv <- survfit(Surv(time, censor) ~ 1, 
                      data = data.test)
  ggsurvplot(fit.surv, data = data.test, conf.int = TRUE)
  # ggsurvplot(fit.surv, data = data.test, conf.int = TRUE, add.all = TRUE, palette = c("black", "red", "blue"))
  ggsave(paste0("./output/plots_loop_", outerLoopIndex, "_ggsurvplot.pdf"), device = "pdf")
  
  # C-Index
  print('C-Index: Train')
  print(
  Cindex(predict(fit.coxnet,
                 newx=data.train.mx,
                 s=cv.coxnet$lambda.1se,
                 type="link"), train.surv)
  )
  
  test.surv <- Surv(data.test$time, data.test$censor)
  print('C-Index: Test')
  print(
  Cindex(lp, test.surv)
  )
  
  
  # ---- Brier Score ----
  print("starting brier score calculations")
  
  fit.lo <- coxph(Surv(time,censor)~1,data=data.train,
                  x=TRUE,y=TRUE)
  
  up <- as.formula(paste("~", 
                         paste(colnames(data.train.mx), 
                               collapse="+")))
  
  if(use_cache & file.exists(file_path_fw)) {
    load(
      file = file_path_fw
    )
  } else {
    fit.fw <- MASS::stepAIC(fit.lo,
                      scope=list(lower=fit.lo,
                                 upper=up),
                      direction="both",
                      trace=FALSE)
    save(fit.fw, file = file_path_fw)
  }
  # print(as.data.frame(fit.fw$anova))
  
  
  beta.1se <- coef(cv.coxnet,s=cv.coxnet$lambda.1se)
  vars.1se <- rownames(beta.1se)[as.numeric(beta.1se)!=0]
  # vars.1se <- vars.1se[vars.1se != "a1peducp_4_graduated_college_to_doctorate_or_professional_degree"]
  fm.1se <- as.formula(
    paste0(
      "Surv(time,censor)~", 
      paste0(vars.1se, collapse="+")
    )
  )
  
  fit.1se <- coxph(fm.1se,data=data.train, x=TRUE, y=TRUE)
  
  # source("./final_ggforest.R")
  # Forest Plot ----
  ggforest_custom(fit.1se, data = data.test)
  ggsave(
    paste0("./output/plots_loop_", outerLoopIndex, "_ggforest.pdf"), 
    device = "pdf", 
    width = 21, 
    height = 4 + (length(fit.1se[["coefficients"]]) * 0.8), 
    units = "cm"
  )
  
  fit.pec.train <- pec(
    object=list("cox.fw"=fit.fw,
                "cox.1se"=fit.1se), 
    data = data.train, 
    formula = Surv(time, censor) ~ 1, 
    splitMethod = "none")
  
  
  fit.pec.test <- pec(
    object=list("cox.fw"=fit.fw,
                "cox.1se"=fit.1se), 
    data = data.test, 
    formula = Surv(time, censor) ~ 1, 
    splitMethod = "none")
  
  par(mfrow=c(1,2))
  
  plot(fit.pec.train, main="training data")
  plot(fit.pec.test,  main="test data")
  
  par(mfrow=c(1,1))
  
  dev.off()
  
  return(
    list(
      "index" = outerLoopIndex,
      "training" = fit.pec.train,
      "testing" = fit.pec.test,
      "glmnet" = cv.coxnet,
      "fit.1se" = fit.1se
    )
  )

})

save(results.outer, file = "./output/results_outer.rda")

meanBrierScore <- function(r) {
  result <- tibble(
    time = numeric(),
    test = numeric(),
    train = numeric(),
    ref = numeric()
  )
  
  probabilities_test <- vector(mode='list', length = max_times + 1)
  probabilities_train <- vector(mode='list', length = max_times + 1)
  probabilities_ref <- vector(mode='list', length = max_times + 1)
  
  for(loop_result in r) {
    for(i in 1:length(loop_result[["testing"]][["time"]])) {
      time <- loop_result[["testing"]][["time"]][i] + 1
      probabilities_test[[time]] <- append(
        probabilities_test[[time]],
        loop_result[["testing"]][["AppErr"]][["cox.1se"]][i]
      )
      probabilities_ref[[time]] <- append(
        probabilities_ref[[time]],
        loop_result[["testing"]][["AppErr"]][["Reference"]][i]
      )
    }
    for(i in 1:length(loop_result[["training"]][["time"]])) {
      time <- loop_result[["training"]][["time"]][i] + 1
      probabilities_train[[time]] <- append(
        probabilities_train[[time]],
        loop_result[["training"]][["AppErr"]][["cox.1se"]][i]
      )
    }
  }
  for(i in 1:(max_times + 1)) {
    # print(i)
    # print(mean(probabilities[[i]]))
    result <- result %>% add_row(
      time = (i - 1), 
      test = mean(probabilities_test[[i]]), 
      train = mean(probabilities_train[[i]]), 
      ref = mean(probabilities_ref[[i]])
    )
  }
  return(result)
}

ggplot(data = meanBrierScore(results.outer), aes(x = time)) + 
  geom_line(aes(y = ref), color="black", size = 1)   + 
  geom_line(aes(y = train), color = "steelblue", size = 1) + 
  geom_line(aes(y = test), color="darkgreen", size = 1)

ggsave("./output/plots_outer_brier.pdf", device = "pdf")

# selectCoxfw <- function(formula,data,steps=100,direction="both")
# {
#   require(prodlim)
#   fmlo <- reformulate("1",formula[[2]])
#   fitlo <- coxph(fmlo,data=data,x=TRUE,y=TRUE)
#   fwfit <- MASS::stepAIC(fitlo,
#                    scope=list(lower=fitlo,
#                               upper=formula),
#                    direction=direction,
#                    steps=steps,
#                    trace=FALSE)
#   if (fwfit$formula[[3]]==1){
#     newform <- reformulate("1",formula[[2]])
#     newfit <- prodlim(newform,
#                       data=data)
#   }else{
#     newform <-fwfit$formula
#     newfit <- coxph(newform,data=data,x=TRUE,y=TRUE)
#   }
#   out <- list(fit=newfit,
#               In=attr(terms(newfit$formula),which = "term.labels"))
#   out$call <-match.call()
#   class(out) <- "selectCoxfw"
#   
#   out
# }
# 
# predictSurvProb.selectCoxfw <- function(object,newdata,times,...){
#   predictSurvProb(object[[1]],newdata=newdata,times=times,...)
# }
# 
# # imp_dat <- mice(data %>% select(-M2ID, -A1PBYEAR_2019), m = 5, method = "pmm") # Impute missing values
# imp_dat <- mice(data %>% select(-M2ID), m = 5, method = "pmm") # Impute missing values
# data.full <- complete(imp_dat)
# skimr::skim(data.full)
# 
# data.full.surv <- data.full %>% select(time, censor)
# data.full <- data.full %>% mutate(across(where(is.numeric), scale))
# data.full$time <- data.full.surv$time
# data.full$censor <- data.full.surv$censor
# 
# data.full.mx <- model.matrix(as.formula(paste("~", paste(colnames(data.full  %>% select(-time, -censor)), collapse = " + "), sep = " ")), data.full)
# 
# data.full.mx <- clean_names(data.full.mx[,-1])
# 
# data.full <- as.data.frame(data.full.mx)
# data.full$time <- data.full.surv$time
# data.full$censor <- data.full.surv$censor
# 
# fm <- as.formula(paste("Surv(time, censor) ~ ", 
#                        paste(colnames((data.full %>% select(-time, -censor))), 
#                              collapse="+")))
# fit.coxfw <- selectCoxfw(fm,data=data.full,
#                          direction="forward")
# 
# fit.cforest <- pecCforest(fm, data =data.full, 
#                                control = party::cforest_classical(ntree = 100))
# View(fit.cforest[["forest"]])
# party::varimp(fit.cforest[["forest"]], mincriterion = 0.95)
# 
# 
# pec.cv <- pec::pec(
#   object=list("cox.fw"=fit.coxfw,"cforest"=fit.cforest), 
#   data = data.full, 
#   formula = Surv(time, censor) ~ 1, 
#   splitMethod = "cv5")
# plot(pec.cv)
# 
# ggforest(fit.coxfw, data = data.full)

# [1] "starting outer loop: 1"
# [1] "starting preprocessing training set"
# Error in solve.default(xtx + diag(pen)) :
#   system is computationally singular: reciprocal condition number = 1.70904e-18
# Error: evaluation nested too deeply: infinite recursion / options(expressions=)?

# ls.test[["df"]]
# merged_data <- merge(ls.test[["df"]], ls.train["df"])
# library(corrplot)
# 
# merged_data.cor = cor(merged_data, method = c("spearman"))

