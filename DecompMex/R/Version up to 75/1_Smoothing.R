### Based on version 1.
#######################################
# IMPORTANT: impute zeros for missing ages, causes, states, years
# otherwise BP is too high...

setwd("C:/Users/jmaburto/Documents/GitHub/DecompMex/DecompMex")

if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
  # if I'm on the laptop
  setwd("/home/tim/git/DecompMex/DecompMex")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/DecompMex/DecompMex"))
}



# Load data and group causes according to paper ---------------------------

#sex=1 <- males
#### 1. Infectious and respiratory diseases, 2. Cancers, 3. Circulatory, 4. Birth, 5. Diabetes, 6. Other Medical Care AM
#### 7. IHD, 8. HIV, 9. Suicide, 10. Lung Cancer, 11. Cirrhosis, 12. Homicide, 13. Road traffic accidents, 
#### 14. other heart diseases, 15. Ill-defined causes, 16. All other Non-AM
#### Note: these data do not contain Not Specified categories

load('Data/Counts&Rates_1990-2015Mex.RData')


#Groups used in the article:
# 1. Amenable to meical service: 1+2+3+4+6
# 2. Diabetes: 5
# 3. IHD:7
# 4. HIV:8
# 5. Lung cancer: 10
# 6. Cirrhosis: 11
# 7. Homicide: 12
# 8. Road traffic: 13 
# 9. Suicide: 9
# 10. All other causes: 14+15+16

#group accordingly
Counts <- Data_Counts[,1:4]
Counts <- cbind(Counts, g1=rowSums(Data_Counts[, c(6,7,8,9,11)]))
Counts <- cbind(Counts, g2=Data_Counts[,10])
Counts <- cbind(Counts, g3=Data_Counts[,12])
Counts <- cbind(Counts, g4=Data_Counts[,13])
Counts <- cbind(Counts, g5=Data_Counts[,15])
Counts <- cbind(Counts, g6=Data_Counts[,16])
Counts <- cbind(Counts, g7=Data_Counts[,17])
Counts <- cbind(Counts, g8=Data_Counts[,18])
Counts <- cbind(Counts, g9=Data_Counts[,14])
Counts <- cbind(Counts, g10=rowSums(Data_Counts[,19:21]))
Counts <- cbind(Counts, g11=rowSums(Counts[,5:14]))
Counts$Pop <- Data_Counts$Pop

Rates <- Data_rates[,c(1:4,21)]
gdata::keep(Counts,Rates, sure = T)
library(data.table)
Counts <- data.table(Counts)
Rates <- data.table(Rates)

# Cause-specific smoothing -----------------------------------------------

#First smooth with Camarda's method. We smooth cause-specific deaths and then constrain to the unsmoothed rates

source("R/Functions.R")

library(reshape2)
library(MortalitySmooth)

causes   <- 5:15
Counts2  <- data.table(cbind(Counts[,1:4,with=F],g1= rowSums(Counts[,5:14,with=F]),Counts[,15,with=F]))

sm.rates <- data.table(as.matrix(Counts)[,1:4])
sm.rates2 <- data.table(as.matrix(Counts2)[,1:4])
sm.rates2  <- sm.rates2[order(year,sex,state,age)]
sm.rates  <- sm.rates[order(year,sex,state,age)]

#i <- 5
#Dx <- Counts[sex==1 & state ==1 ]
#DX      <- acast(Dx, age~year, value.var = colnames(Counts)[i], fill = 0)
#EX      <- acast(Dx, age~year, value.var = "Pop", fill = 0)

#r <- DX/EX
#image(log(r))

#r.s <- sm.mat(DX,EX)
#r.s2 <- acast(r.s, age~year, value.var = "mxs")
#image((log(r.s2)-log(r)))


# melt so that we just use data.table

for (i in causes){
  Mxs      <- Counts[,sm.chunk(.SD,i),by=list(state,sex)]
  Mxs  <- Mxs[order(year,sex,state,age)]
  sm.rates[,paste0("g",i-4)] <- Mxs$mxs
  print(i)
 # cbind(sm.rates,i= Mxs$mxs)
}

#smooth total mortality rates
Counts2  <- data.table(cbind(Counts[,1:4,with=F],g1= rowSums(Counts[,5:14,with=F]),Counts[,16,with=F]))

Counts2$g1[Counts2$Pop == 0] <- 0
Counts2$g1[Counts2$Pop < Counts2$g1] <- Counts2$Pop[Counts2$Pop < Counts2$g1]

Mxs2      <- Counts2[,sm.chunk(.SD,5),by=list(state,sex)]
Mxs2 <- Mxs2[order(year,sex,state,age)]


plot(Mxs2$mxs[Mxs2$state==1 & Mxs2$sex==1 & Mxs2$year==1990])

sm.rates2[,"Tot"] <- Mxs2$mxs




#Order all datasets
sm.rates <- sm.rates[order(year,sex,state,age)]
Rates <- Rates[order(year,sex,state,age)]


#now constrain to original ones
sm.r    <- sm.rates[,5:14,with=F]
sm.tot  <- rowSums(sm.r)
sm.prop <- sm.r/sm.tot
sm.prop2 <- sm.prop*Rates$total
smooth.rates <- cbind(as.matrix(Rates)[,1:4],mx=rowSums(sm.prop2),sm.prop2)
smooth.rates <- as.data.table(smooth.rates)

#now constrain to smoothed ones
sm.prop3 <- sm.prop*sm.rates2$Tot
smooth.rates2 <- cbind(as.matrix(Rates)[,1:4],mx=rowSums(sm.prop3),sm.prop3)
smooth.rates2 <- as.data.table(smooth.rates2)

#wihout constrain in smoothing by cause
sm.rates$mx <- sm.tot
colnames(sm.rates)
sm.rates <- sm.rates[,-15, with=F]
setcolorder(sm.rates,c("year","sex","state","age","mx","g1","g2","g3","g4","g5","g6","g7","g8","g9","g10"))
head(sm.rates)

# This file contains smooth rates constraint to the original ones
save(smooth.rates,file = "Data/smoothed rates_ConstraintOrig.RData")
save(smooth.rates2,file = "Data/smoothed rates_ConstraintSmooth.RData")
# This file contains smooth rates NOT constraint to the original ones
save(sm.rates,file = "Data/smoothed rates_No Constraint.RData")

save(sm.rates2,file = "Data/Total_Smooth.RData")

