###############trialseq 1###############

repeat{
  rndstim <- rbinom(300, 1, 0.5)
  rndstim.1 <- 1:(length(rndstim)-1)
  rndstim.2 <- 2:length(rndstim)
  if(mean(rndstim) == 0.5 & mean(rndstim[rndstim.1] == rndstim[rndstim.2]) == 149/299){
    break
  }
}

repeat{
  trials1 <- rndstim
  trials1[trials1 == 0] <-  runif(sum(trials1 == 0), min = 1.5, max = 6.5)  
  trials1 <- round(trials1, 0)
  if(length(trials1[trials1 == 2]) == 30 & length(trials1[trials1 == 3]) == 30 & length(trials1[trials1 == 4]) == 30 & length(trials1[trials1 == 5]) == 30 & length(trials1[trials1 == 6]) == 30){
    break
  }
}

repeat{
  trials1[trials1 == 1 | trials1 == 7 | trials1 == 8 | trials1 == 9 | trials1 == 10 | trials1 == 11] <-  runif(sum(trials1 == 1 | trials1 == 7 | trials1 == 8 | trials1 == 9 | trials1 == 10 | trials1 == 11), min = 6.5, max = 11.5)  
  trials1 <- round(trials1, 0)
  if(length(trials1[trials1 == 7]) == 30 & length(trials1[trials1 == 8]) == 30 & length(trials1[trials1 == 9]) == 30 & length(trials1[trials1 == 10]) == 30 & length(trials1[trials1 == 11]) == 30){
    break
  }
}

df_trials1 <- as.data.frame(trials1)

###############trialseq 2###############

repeat{
  rndstim_2 <- rbinom(300, 1, 0.5)
  rndstim_2.1 <- 1:(length(rndstim_2)-1)
  rndstim_2.2 <- 2:length(rndstim_2)
  if(mean(rndstim_2) == 0.5 & mean(rndstim_2[rndstim_2.1] == rndstim_2[rndstim_2.2]) == 150/299){
    break
  }
}

repeat{
  trials2 <- rndstim_2
  trials2[trials2 == 0] <-  runif(sum(trials2 == 0), min = 1.5, max = 6.5)  
  trials2 <- round(trials2, 0)
  if(length(trials2[trials2 == 2]) == 30 & length(trials2[trials2 == 3]) == 30 & length(trials2[trials2 == 4]) == 30 & length(trials2[trials2 == 5]) == 30 & length(trials2[trials2 == 6]) == 30){
    break
  }
}

repeat{
  trials2[trials2 == 1 | trials2 == 7 | trials2 == 8 | trials2 == 9 | trials2 == 10 | trials2 == 11] <-  runif(sum(trials2 == 1 | trials2 == 7 | trials2 == 8 | trials2 == 9 | trials2 == 10 | trials2 == 11), min = 6.5, max = 11.5)  
  trials2 <- round(trials2, 0)
  if(length(trials2[trials2 == 7]) == 30 & length(trials2[trials2 == 8]) == 30 & length(trials2[trials2 == 9]) == 30 & length(trials2[trials2 == 10]) == 30 & length(trials2[trials2 == 11]) == 30){
    break
  }
}

df_trials2 <- as.data.frame(trials2)

