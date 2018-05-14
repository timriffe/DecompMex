### Based on version 1.
#######################################
# IMPORTANT: impute zeros for missing ages, causes, states, years
# otherwise BP is too high...
rm(list=ls(all=TRUE))

if (system("hostname",intern=TRUE) == "ADM-108625") {
  setwd("C:/Users/jmaburto/Documents/GitHub/DecompMex/DecompMex")
} else {
  if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
    # if I'm on the laptop
    setwd("/home/tim/git/DecompMex/DecompMex")
  } else {
    # in that case I'm on Berkeley system, and other people in the dept can run this too
    setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/DecompMex/DecompMex"))
  }}



# Load data and group causes according to paper ---------------------------

#sex=1 <- males
load('Data/Counts&Rates_1990-2015Mex.RData')


library(data.table)
library(reshape2)
library(MortalitySmooth)


Counts <- Data_Counts[,1:4,with=F]
Counts$g1 <- rowSums(Data_Counts[, c(6,7,8,9,11),with=F])
Counts$g2 <- Data_Counts[,10,with=F]
Counts$g3 <- Data_Counts[,12,with=F]
Counts$g4 <- Data_Counts[,13,with=F]
Counts$g5 <- Data_Counts[,15,with=F]
Counts$g6 <- Data_Counts[,16,with=F]
Counts$g7 <- Data_Counts[,17,with=F]
Counts$g8 <- Data_Counts[,18,with=F]
Counts$g9 <- Data_Counts[,14,with=F]
Counts$g10 <- rowSums(Data_Counts[,19:21,with=F])
Counts$g11 <- rowSums(Counts[,5:14,with=F])
Counts$Pop <- Data_Counts$Pop

# just in very high ages
Counts[Pop < g11]$g1 <- Counts[Pop < g11]$g2 <- Counts[Pop < g11]$g3 <- Counts[Pop < g11]$g4 <- Counts[Pop < g11]$g5 <- Counts[Pop < g11]$g6 <- Counts[Pop < g11]$g7 <- Counts[Pop < g11]$g8 <- Counts[Pop < g11]$g9 <- Counts[Pop < g11]$g10 <- 0 
Counts[Pop < g11]$g11 <- Counts[Pop < g11]$Pop

gdata::keep(Counts, sure= T)
source("R/Functions.R")


causes   <- 5:15

# State specfic smoothing and CIs-----------------------------------------------
#example
#D <- Counts[Counts$sex == 1 & Counts$state==1]
CI_function <- function(D =.SD, i =5){
  ages  		<- 0:109
  years 		<- 1990:2015
  cause   <- colnames(D)[i-2]
  DX      <- acast(D[,c(1:4,i-2,ncol(D)),with=F], age~year, value.var = colnames(D)[i-2], fill = 0)
  EX      <- acast(D[,c(1:4,i-2,ncol(D)),with=F], age~year, value.var = "Pop", fill = 0)
  
  W     		<- EX
  W[W > 0] 	<- 1
  W[DX < 1 & W == 1] 	<- .3
  
  fit  <- Mort2Dsmooth(
    x = ages, 
    y = years, 
    Z = DX,
    offset = log(EX), 
    W = W,
    control=list(MAX.IT=300))
  
  pre <- predict(fit, se.fit=TRUE)
  
  CI1 <- exp(pre$fit - 2*pre$se.fit)
  CI2 <- exp(pre$fit + 2*pre$se.fit)
  mxs <- exp(pre$fit)
  
  D <- D[,1:4]    
  D$mxs <- mxs
  D$CI1 <- CI1
  D$CI2 <- CI2
  D$cause <- cause
  
  D
  
}

Restuls_CI <- NULL
for (i in causes){
  Mxs_CI     <- Counts[, CI_function(.SD,i),by=list(state,sex)]
  Results_CI <- rbind(Restuls_CI,Mxs_CI)
}


unique(Results_CI$cause)


Results_CI       <- Results_CI[with(Results_CI,order(year,sex,state,cause,age)),]

save(Results_CI, file = 'Data/State_CIs.RData')


