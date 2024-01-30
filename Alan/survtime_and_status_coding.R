#### Data preperation ####

#### creating a survival_time variable for dead participants ####

setwd("C:/Users/alan-/Desktop/statsdata")

install.packages("haven")
library(haven)
library(lubridate)
library(mice)
library(psych)

# Midus 1 and mortality data

wave_1 <- read_dta("02760-0001-Data.dta")

mortality <- read_sav("MIDUS_Core_MortalityCauseData_N2459_20230531.sav")

# merge two dataframes

merged_data <- merge(wave_1, mortality, by = "M2ID", all = TRUE)

#create interview_date and daeth_date variables 

merged_data$interview_date <- as.Date(paste(merged_data$A1PI_YR, merged_data$A1PI_MO, "01", sep = "-"), format = "%Y-%m-%d")
merged_data$death_date <- as.Date(paste(merged_data$DOD_Y, merged_data$DOD_M, "01", sep = "-"), format = "%Y-%m-%d")

# as Date-objects require day number, every date is noted with "01" for "day"

#calculate the time interval from interview to death in months 

merged_data$survival_time <- as.numeric(interval(merged_data$interview_date, merged_data$death_date) %/% months(1))

#### creating status variable for survival analysis #### 

# check if sum of NAs in death date and interview date add up to NAs in survival time 

NAs_in_interview_date <- sum(is.na(merged_data$interview_date))
NAs_in_death_date <- sum(is.na(merged_data$death_date))
NAs_in_survival_time <- sum(is.na(merged_data$survival_time))
NAs_in_death_date + NAs_in_interview_date == NAs_in_survival_time

# ascribe value "0" to every participant with "NA" in death date in new "status" variable 

merged_data$status <- ifelse(is.na(merged_data$death_date), 0,1)

# investigate which participants have missings in interview date 

subset <- merged_data[is.na(merged_data$interview_date), "M2ID"]
print(subset)

# participants with NAs in interview date are participants from other waves than MIDUS 1 core 

# exclude these participants from analysis? These cases have NAs in all of the predictor variables 

filtered_data <- merged_data[!is.na(merged_data$interview_date), ]

# explore distributions of variables relevant to survival analysis

describe(filtered_data$survival_time)
hist(filtered_data$survival_time)

describe(filtered_data$status)
hist(filtered_data$status)





