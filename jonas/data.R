library(haven)
library(tidyverse)
library(caret)


getCorrelationEstimate <- function(col, otherCol) {
  tryCatch(
    {
      corResult <- cor.test( 
        as.numeric(da02760.0001[[col]]), 
        as.numeric(da02760.0001[[otherCol]]), 
        na.action = "na.exclude"
      )
      return(corResult)
    },
    error = function(cond) {
      return(NULL)
    }
  )
}

getHighCorrelations <- function(col) {
  corList <- list()
  
  for(otherCol in columns) {
    if(otherCol == col) next
    corResult <- getCorrelationEstimate(col, otherCol)
    if(is.null(corResult) || is.na(corResult[['p.value']])) next
    if(corResult[['p.value']] < 0.05 && (corResult[['estimate']] > 0.7 || corResult[['estimate']] < -0.7)) {
      value <- corResult[['estimate']]
      names(value) <- otherCol
      corList <- append(corList, value)
    }
  }
  return(corList)
}

load(
  file = "../common/MIDUS/MIDUS-1_ICPSR_02760/DS0001_main/02760-0001-Data.rda"
)
# da02760.0001

savData <- read_sav("../common/MIDUS/MIDUS-1_ICPSR_02760/DS0001_main/02760-0001-Data.sav")

columns <- names(da02760.0001)

varList <- tibble(
  UID = character(),
  Lbl = character(),
  Var = numeric(),
  NoF = integer(),
  NAs = numeric(),
  Cor = list()
)

for( col in columns ) {
  variable <- da02760.0001[,col]
  
  varList <- varList %>% add_row(
    UID = col,
    Lbl = attr(savData[[col]], 'label'),
    Var = round( ifelse(is.factor(variable), sd( as.numeric(variable), na.rm=TRUE ), sd(variable, na.rm=TRUE)), digits = 2 ),
    NoF = nlevels( variable ),
    NAs = round( sum(is.na(variable)) / 7108, digits = 3 ),
    Cor = getHighCorrelations(col) # This takes about an hour to compute
  )
}

#save(varList, file = "varList.rda")

# varListWithCorSorted <- varListWithCorBU %>% mutate(CID = names(Cor), Cor = as.numeric(Cor)) %>% arrange(ordered(UID, unique(UID)), Cor)
# 
# # varListWithCor <- varListWithCorBU %>% mutate(cID = names(Cor), Cor = round(as.numeric(Cor), 3)) %>% nest(Cor, cID, .key = "Cor")
# varListWithCor <- varListWithCorSorted %>% mutate(Cor = paste(CID, paste0("(", round(as.numeric(Cor), 3), ")"), sep = " ")) %>% select(-CID) %>% nest(Cor, .key = "Cor") %>% unnest_wider(Cor)
# varListWithCor <- varListWithCor %>% mutate(NoC = length(Cor[[]]))
# varListWithCor$NoC <- lengths(varListWithCor$Cor)
# varListWithCorBU <- varList
# 
# varList <- varList %>% select(-Cor)
# varList$id <- 1:2098
# varListMerged <- merge(varList, varListWithCor, all.x = TRUE)
# varListMerged <- varListMerged %>% arrange(id) %>% select(-id)
# 
# getHighCorrelations("A1PBYEAR_2019")
# 
# cor.test(da02760.0001[["A1PBYEAR_2019"]], as.numeric(da02760.0001[["A1PRSEX"]]), use="pairwise.complete.obs")[['p.value']]
# # Check for variance / usefullness
# 
# attr(savData[["A1STATUS"]], 'label')
# 
# 
# save(varListMerged, file = "varListMerged.rda")
# save(varListWithCorBU, file = "varListBU.rda")

nzv <- nearZeroVar(da02760.0001, saveMetrics= TRUE)
nzvList <- nzv %>% filter(nzv == TRUE)

varListCSV <- varListMerged %>% mutate(Use = TRUE, Row = 1:2098, .before = UID)

varListCSV <- varListCSV %>% filter(!(UID %in% row.names(nzvList)))
varListCSV <- varListCSV %>% filter(!(UID %in% c("M2FAMNUM", "A1PRAGE_2019")))

# varListCSV <- varListCSV %>% filter(NAs < 0.10)
varListCSV <- varListCSV %>% filter(NAs == 0.0)

varListFiltered <- varListCSV

varListCSV$Cor <- lapply(varListCSV$Cor, function(x) {paste(x, collapse = ', ')})
varListCSV <- apply(varListCSV,2,as.character)
write.csv(varListCSV, "VariableList_Filtered.csv", row.names = FALSE)


mortality <- read_sav("../common/MIDUS/MIDUS_Core_MortalityCauseData_N2459_20230531.sav")

data <- da02760.0001[,varListFiltered$UID]
data <- merge(data, mortality %>% select(M2ID, DOD_Y), by = "M2ID", all.x = TRUE)

data <- data %>% mutate(time = ifelse(is.na(DOD_Y), 2023 - A1PBYEAR_2019, DOD_Y - A1PBYEAR_2019), censor = is.na(DOD_Y)) # Age at Death in Years

plot(sort(data$time), pch='.', type='o', 
     col='blue', lwd=2 , 
     main = 'MIDUS Data \nTime to death')


# Random Forest Survival
# install.packages('ranger')
library(ranger)
# install.packages('survival')
library(survival)


survival_formula <- formula(paste('Surv(', 'time', ',', 'censor', ') ~ ',paste(colnames(data[,!(colnames(data) %in% c("M2ID", "time", "censor", "DOD_Y", "A1PBYEAR_2019", "A1PRSEX", "A1PB35"))]), collapse = '+')))

data_surv <- data %>% select(-DOD_Y, -M2ID, -A1PBYEAR_2019, -A1PRSEX, -A1PB35)

mice::md.pattern(data_surv)


data_surv <- data_surv %>% filter(!is.na(time))

survival_model <- ranger(survival_formula,
                         data = data_surv,  
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



