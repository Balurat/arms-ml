
# if(!exists("getPlottingData", mode="function")) source("./final_ggforest.R")
source("./final_ggforest.R")

load(file = "./cache/all_final/loop_3-imputed_data.rda")
d_all.test <- ls.test
d_all.train <- ls.train

load(file = "./cache/male_final/loop_3-imputed_data.rda")
d_male.test <- ls.test
d_male.train <- ls.train

load(file = "./cache/female_final/loop_3-imputed_data.rda")
d_female.test <- ls.test
d_female.train <- ls.train

pd_all <- getPlottingData(d_all.test, d_all.train)
pd_male <- getPlottingData(d_male.test, d_male.train, "Männer")
pd_female <- getPlottingData(d_female.test, d_female.train, "Frauen")

unique(pd_female$var)

# pd_total <- rbind(pd_all, pd_male, pd_female)
# pd_total$group <- factor(pd_total$group, levels = c('Männer', 'Alle', 'Frauen'))
pd_total <- rbind(pd_male, pd_female)
pd_total$group <- factor(pd_total$group, levels = c('Männer', 'Frauen'))
pd_total <- cleanData(pd_total, grouped = TRUE)


ggforest_total(pd_total, fontsize = 11)
# write.csv(pd_total, "./pd_total.csv", row.names=TRUE)

ggsave(
  paste0("./_forestplot_total.pdf"), 
  device = "pdf", 
  width = 21, 
  height = 4 + (length(unique(pd_total$var)) * 1.5), 
  units = "cm",
  limitsize = FALSE
)



# plotSurvCurve(d_all.train, d_all.test, d_male.train, d_male.test, d_female.train, d_female.test)

fit.surv_all <- survfit(Surv(time, censor) ~ a1prsex1,
                        data = d_all.test[["df"]])
ggsurvplot(fit.surv_all, d_all.test[["df"]], conf.int = TRUE)[["plot"]] + 
  # xlim(50, 100)
  
  coord_cartesian(xlim = c(40, 100)) +
  scale_x_continuous(expand = expansion( add = 2) ,name = "Zeit") +
  scale_y_continuous(name = "Überlebenswahrscheinlichkeit")

ggsave(paste0("./output/_plots_total_ggsurvplot.pdf"), device = "pdf")




# All female loops combined

load(file = "./cache/female_final/loop_1-imputed_data.rda")
d1.test <- ls.test
d1.train <- ls.train

load(file = "./cache/female_final/loop_2-imputed_data.rda")
d2.test <- ls.test
d2.train <- ls.train

load(file = "./cache/female_final/loop_3-imputed_data.rda")
d3.test <- ls.test
d3.train <- ls.train

load(file = "./cache/female_final/loop_4-imputed_data.rda")
d4.test <- ls.test
d4.train <- ls.train

load(file = "./cache/female_final/loop_5-imputed_data.rda")
d5.test <- ls.test
d5.train <- ls.train

pd1 <- getPlottingData(d1.test, d1.train, 1)
pd2 <- getPlottingData(d2.test, d2.train, 2)
pd3 <- getPlottingData(d3.test, d3.train, 3)
pd4 <- getPlottingData(d4.test, d4.train, 4)
pd5 <- getPlottingData(d5.test, d5.train, 5)

pd_total <- rbind(pd1, pd2, pd3, pd4, pd5)
pd_total$group <- factor(pd_total$group, levels = c(1, 2, 3, 4, 5))
pd_total <- cleanData(pd_total)

# source("./final_ggforest.R")
# ggforest_loops(pd_total, fontsize = 11,
#                dotCOLS = c("#f9b282"),
#                barCOLS = c("#de6b35", "#b2562a", "#854020", "#592b15", "#2c150b"),
#                shape = 21)

ggforest_total(pd_total, fontsize = 11,
               dotCOLS = c("#f9b282"),
               barCOLS = c("#de6b35", "#b2562a", "#854020", "#592b15", "#2c150b"),
               shape = 21)

ggsave(
  paste0("./_forestplot_females.pdf"), 
  device = "pdf", 
  width = 21, 
  height = 4 + (length(unique(pd_total$var)) * 1.5), 
  units = "cm",
  limitsize = FALSE
)


# All male loops combined

load(file = "./cache/male_final/loop_1-imputed_data.rda")
d1.test <- ls.test
d1.train <- ls.train

load(file = "./cache/male_final/loop_2-imputed_data.rda")
d2.test <- ls.test
d2.train <- ls.train

load(file = "./cache/male_final/loop_3-imputed_data.rda")
d3.test <- ls.test
d3.train <- ls.train

load(file = "./cache/male_final/loop_4-imputed_data.rda")
d4.test <- ls.test
d4.train <- ls.train

load(file = "./cache/male_final/loop_5-imputed_data.rda")
d5.test <- ls.test
d5.train <- ls.train

pd1 <- getPlottingData(d1.test, d1.train, 1)
pd2 <- getPlottingData(d2.test, d2.train, 2)
pd3 <- getPlottingData(d3.test, d3.train, 3)
pd4 <- getPlottingData(d4.test, d4.train, 4)
pd5 <- getPlottingData(d5.test, d5.train, 5)

pd_total <- rbind(pd1, pd2, pd3, pd4, pd5)
pd_total$group <- factor(pd_total$group, levels = c(1, 2, 3, 4, 5))
pd_total <- cleanData(pd_total)


ggforest_total(pd_total, fontsize = 11,
               dotCOLS = c("#f9b282"),
               barCOLS = c("#008fd5", "#0072aa", "#005680", "#003955", "#001d2b"),
               shape = 25)

ggsave(
  paste0("./_forestplot_males.pdf"), 
  device = "pdf", 
  width = 21, 
  height = 4 + (length(unique(pd_total$var)) * 1.5), 
  units = "cm",
  limitsize = FALSE
)

# females and males combined

pd_total$loop <- pd_total$group
pd_total_f <- pd_total %>% mutate(group = "Frauen")

pd_total$loop <- pd_total$group
pd_total_m <- pd_total %>% mutate(group = "Männer")

pd_total <- rbind(pd_total_f, pd_total_m)

unique(pd_total$var)


# All general loops combined
source("./final_ggforest.R")

load(file = "./cache/all_final/loop_1-imputed_data.rda")
d1.test <- ls.test
d1.train <- ls.train

load(file = "./cache/all_final/loop_2-imputed_data.rda")
d2.test <- ls.test
d2.train <- ls.train

load(file = "./cache/all_final/loop_3-imputed_data.rda")
d3.test <- ls.test
d3.train <- ls.train

pd1 <- getPlottingData(d1.test, d1.train, 1)
pd2 <- getPlottingData(d2.test, d2.train, 2)
pd3 <- getPlottingData(d3.test, d3.train, 3)

pd_total <- rbind(pd1, pd2, pd3)
pd_total$group <- factor(pd_total$group, levels = c(1, 2, 3))
pd_total <- cleanData(pd_total)


ggforest_total(pd_total, fontsize = 11,
               dotCOLS = c("#f9b282"),
               barCOLS = c("#969696", "#707070", "#4b4b4b"),
               shape = 22)

ggsave(
  paste0("./_forestplot_all.pdf"), 
  device = "pdf", 
  width = 21, 
  height = 4 + (length(unique(pd_total$var)) * 1.5), 
  units = "cm",
  limitsize = FALSE
)


load(file = "./cache/all_final/loop_3-imputed_data.rda")
d_all.test <- ls.test
d_all.train <- ls.train

data <- rbind(d_all.test[["df"]], d_all.train[["df"]])

library(corrplot)
corr_simple <- function(data=data_final_analysis,sig=0.8, dim = 6){
  pdf(file = paste0("./_plots_corr.pdf"), width = dim, height = dim )
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
  dev.off()
}
corr_simple(data = data)



cox_nfolds   <- 5    # number of folds for inner cross validation 
enet_alpha   <- 0.05 # elastic net mixing parameter
noDigits <- 2
refLabel <- "reference"
main <- "Hazard ratio" 
cpositions <- c(0.02, 0.22, 0.4)
fontsize <- 0.7
set.seed(1234)

train.surv <- Surv(d_male.train[["df"]]$time, d_male.train[["df"]]$censor)

cv.coxnet <- cv.glmnet(
  d_male.train[["mx"]],
  train.surv,
  family="cox",
  type.measure="C",
  nfolds = cox_nfolds,
  alpha=enet_alpha
)

Cindex(predict(cv.coxnet,
               newx=d_male.train[['mx']],
               s=cv.coxnet$lambda.1se,
               type="link"), train.surv)

test.surv <- Surv(d_male.test[["df"]]$time, d_male.test[["df"]]$censor)
Cindex(predict(cv.coxnet,
               newx=d_male.test[['mx']],
               s=cv.coxnet$lambda.1se,
               type="link"), test.surv)

beta.1se <- coef(cv.coxnet,s=cv.coxnet$lambda.1se)
vars.1se <- rownames(beta.1se)[as.numeric(beta.1se)!=0]
# vars.1se <- vars.1se[vars.1se != "a1peducp_4_graduated_college_to_doctorate_or_professional_degree"]
fm.1se <- as.formula(
  paste0(
    "Surv(time,censor)~", 
    paste0(vars.1se, collapse="+")
  )
)

model <- coxph(fm.1se,data=d_male.test[["df"]], x=TRUE, y=TRUE)

summary(model)

conf.high <- conf.low <- estimate <- NULL
stopifnot(inherits(model, "coxph"))


terms <- attr(model$terms, "dataClasses")[-1]

coef <- as.data.frame(tidy(model, conf.int = TRUE))
gmodel <- glance(model)
summary(gmodel)
