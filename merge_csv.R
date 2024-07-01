install.packages("tidyr")
install.packages("dplyr")
install.packages("readr")
install.packages("purrr")
library(tidyr)
library(dplyr)
library(readr)
library(purrr)

setwd("C:/Users/student/Desktop/MasterthesisData/rawdata")
files <- list.files(pattern="*.csv")

#==============================================================================#
############################ read erroneous files ##############################
#==============================================================================#

read_and_convert <- function(file) {
  if(file == "CHBpain_3624_Session_2_Bloc_2.csv" | 
     file == "CHBpain_3624_Session_2_Bloc_6.csv" | 
     file == "CHBpain_3690_Session_2_Bloc_2.csv" |
     file == "CHBpain_1604_Session_2_Bloc_9.csv") {
    df <- read.csv(file, sep = ",")
    df$subj_idx <- as.character(df$subj_idx)
    return(df)
  } else{
    df <- read.csv(file, sep = "")
    df$subj_idx <- as.character(df$subj_idx)
    return(df)
  }
}

#==============================================================================#
################################## for HDDM ####################################
#==============================================================================#

mainDF <- files %>% map_dfr(read_and_convert)

colnames(mainDF)[colnames(mainDF) == 'stim_choice'] <- 'response'

mainDF$response <- ifelse(mainDF$response %in% c(3, 4), NA,
                          ifelse(mainDF$response == 2, 1,
                                 ifelse(mainDF$response == 1, 0, mainDF$response)))

mainDF <- mainDF %>%
  arrange(subj_idx, session, bloc, trial_id) %>%
  group_by(subj_idx, session) %>%
  mutate(
    response_lagged = lag(response, 1)
  )

mainDF <- mainDF %>%
  group_by(subj_idx) %>%
  mutate(response_lagged = ifelse(session == 2 & bloc == 1 & trial_id == 1, NA, response_lagged))

mainDF <- mainDF[complete.cases(mainDF$response), ]

write.csv(mainDF, "CHBpain_HDDM.csv", row.names = FALSE)

#==============================================================================#
######################### for inferential statistics ###########################
#==============================================================================#

mainDF <- files %>% map_dfr(read_and_convert)
write.csv(mainDF, "CHBpain_R.csv", row.names = FALSE)

