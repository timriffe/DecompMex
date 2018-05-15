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
Counts.melt <- melt.data.table(data = Counts,id.vars = c('year','sex','state','age','Pop'),
                               variable.name = 'Cause',value.name = 'Deaths')

gdata::keep(Counts.melt, sure= T)

# State specfic smoothing and CIs-----------------------------------------------
#example
#D <- Counts[Counts$sex == 1 & Counts$state==1]
#g1 <- D[D$year == 1990,]$g1
#Pop <- D[D$year == 1990,]$Pop
Counts.melt <- Counts.melt[Counts.melt$Cause == 'g11',]

CI_function <- function(D=.SD,upper.age = 90,lower.age=5,range.smooth = c(10^0 , 10^8)){
  DX <- D$Deaths
  EX <- D$Pop
  
  ages  		     <- lower.age:upper.age
  W     		     <- EX*0
  W[EX != 0] 	   <- 1
  
  fit  <- Mort1Dsmooth(
    x = ages, 
    y = DX[(lower.age+1):(upper.age+1)], 
    offset = log(EX[(lower.age+1):(upper.age+1)]), 
    w = W[(lower.age+1):(upper.age+1)],
    control = list(RANGE=range.smooth))
  
  pre <- predict(object = fit,newdata = lower.age:109,se.fit = TRUE)
  
  Mx <- DX/EX
  CI1 <- c(Mx[1:(lower.age)],exp(pre$fit - 2*pre$se.fit))
  CI2 <- c(Mx[1:(lower.age)],exp(pre$fit + 2*pre$se.fit))
  mxs <- c(Mx[1:(lower.age)],exp(pre$fit))
  
  D$CI1 <- CI1
  D$CI2 <- CI2
  D$mxs <- mxs
 
  D
}

#CI_function(g1,Pop)
 #i <- 5

Results_CI <- Counts.melt[,CI_function(D=.SD), by = list(year,sex,state,Cause)]
Results_CI       <- Results_CI[with(Results_CI,order(year,sex,state,Cause,age)),]

save(Results_CI, file = 'Data/State_CIs_1D.RData')


