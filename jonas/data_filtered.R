
# returns a data frame with all variables that haven't near zero variance
# and which have less NAs than the specified na_proportion (default = 0.0)
getData <- function(na_proportion = 0.0) {
  # na_proportion <- 0.1
  require(haven)
  require(tidyverse)
  require(caret)
  
  allowed_na_count <- 7108 * na_proportion
  
  # estimates the correlation of two variables
  # in case of an error the return value is NULL
  getCorrelationEstimate <- function(col, otherCol) {
    tryCatch(
      {
        corResult <- cor.test( 
          as.numeric(dat[[col]]), 
          as.numeric(dat[[otherCol]]), 
          na.action = "na.exclude"
        )
        return(corResult)
      },
      error = function(cond) {
        return(NULL)
      }
    )
  }
  
  # goes through all possible variable pairs for the given variable 
  # and checks their correlation
  # returns a list of all correlations greater than +- .7
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
  
  # da02760.0001 is the variable that contains the MIDUS data
  load(
    file = "../common/MIDUS/MIDUS-1_ICPSR_02760/DS0001_main/02760-0001-Data.rda"
  )
  
  mortality <- read_sav("../common/MIDUS/MIDUS_Core_MortalityCauseData_N2459_20230531.sav")
  
  # contains the descriptions of all variables
  # we combine those with the data from the rda file
  savData <- read_sav("../common/MIDUS/MIDUS-1_ICPSR_02760/DS0001_main/02760-0001-Data.sav")
  
  # remove all rows with missing birth year (n - 3)
  # dat <- da02760.0001 %>% filter(!is.na(A1PBYEAR_2019))
  dat <- savData %>% filter(!is.na(A1PBYEAR_2019))
  
  columns <- names(dat)
  
  varList <- tibble(
    UID = character(),
    # Lbl = character(),
    # Var = numeric(),
    # NoF = integer(),
    NAs = numeric(),
    # Cor = list()
  )
  
  for( col in columns ) {
    variable <- dat[,col]
    
    varList <- varList %>% add_row(
      UID = col,
      # Lbl = attr(savData[[col]], 'label'),
      # Var = round( ifelse(is.factor(variable), sd( as.numeric(variable), na.rm=TRUE ), sd(variable, na.rm=TRUE)), digits = 2 ),
      # NoF = nlevels( variable ),
      NAs = sum(is.na(variable)),
      # This takes about an hour to compute, so uncomment on own risk ;)
      # Cor = getHighCorrelations(col) 
      # A saved list is prepared in file "varList.rda"
    )
  }
  
  # filter varList
  varList <- varList %>% 
    # we don't want the family id and the age, since we use the birth year
    filter(!(UID %in% c("M2FAMNUM", "A1PRAGE_2019"))) %>% 
    filter(NAs <= allowed_na_count)
  
  # remove near zero variance variables
  nzvList <- nearZeroVar(dat[,varList$UID], saveMetrics= TRUE) %>% 
    filter(nzv == TRUE)
  varList <- varList %>% filter(!(UID %in% row.names(nzvList)))
  
  # only use variables from filtered varList
  data <- dat[,varList$UID]
  
  # merge mortality data into the data frame
  # and compute survival indices (time & censor)
  return(
    merge(
      data, 
      mortality %>% select(M2ID, DOD_Y), 
      by = "M2ID", 
      all.x = TRUE
    ) %>% 
    # calculate time and censor variable
    mutate(
      # True if still alive
      censor = is.na(DOD_Y),
      # Age at Death in Years
      time = ifelse(censor, 2023 - A1PBYEAR_2019, DOD_Y - A1PBYEAR_2019)
    ) %>% 
    # remove year of death since it contains many missings
    select(-DOD_Y) 
  )
}