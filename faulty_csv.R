setwd("C:/Users/student/Desktop/MasterthesisData/rawdata")

#==============================================================================#
######################### correct erroneous names ##############################
#==============================================================================#

data <- read.csv("CHBpain_00003624_Session_2_Bloc_2.csv", sep="")
data$subj_idx <- "3624"
write.csv(data, "CHBpain_3624_Session_2_Bloc_2.csv", row.names = FALSE)

data <- read.csv("CHBpain_00003624_Session_2_Bloc_6.csv", sep="")
data$subj_idx <- "3624"
write.csv(data, "CHBpain_3624_Session_2_Bloc_6.csv", row.names = FALSE)

data <- read.csv("CHBpain_36900347_Session_2_Bloc_2.csv", sep="")
data$subj_idx <- "3690"
write.csv(data, "CHBpain_3690_Session_2_Bloc_2.csv", row.names = FALSE)

data <- read.csv("CHBpain_1604_Session_2_Bloc_9.csv", sep="")
data$subj_idx <- "1604"
data$bloc <- "9"
write.csv(data, "CHBpain_1604_Session_2_Bloc_9.csv", row.names = FALSE, sep="")
