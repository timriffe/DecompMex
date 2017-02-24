
setwd("C:/Users/jmaburto/Documents/GitHub/DecompMex/DecompMex")

if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
  # if I'm on the laptop
  setwd("/home/tim/git/DecompMex/DecompMex")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/DecompMex/DecompMex"))
}

source("R/Functions.R")

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

library(data.table)
library(reshape2)

## minimum mortality rates by state, year, sex, cause
#constrain to the original ones
#data       <- local(get(load("Data/smoothed rates_Constraint.RData")))
#constrain to the smoothed total ones
#data       <- local(get(load("Data/smoothed rates_ConstraintSmooth.RData")))
# No constrain, sum over smoother causes
#data       <- local(get(load("Data/smoothed rates_No Constraint.RData")))

#data <- local(get(load("Data/Total_Smooth.RData")))


Mxsc       <- data[,-c(5),with=F]
Mxsc       <- melt.data.table(Mxsc,id.vars = 1:4,variable.name = "cause",value.name = "mx")
Mxscmin    <- Mxsc[,min(mx), by = list(year, sex, age, cause)]
Mxsmin     <- Mxscmin[,sum(V1),by=list(year, sex, age)]
setnames(Mxsmin,4,"mx_min")
# object with minimum death rates for every year by sex and age
head(Mxsmin)


#######################################
# now get best practive lx and Lx, don't really need BP e0
#BPe0 <- Mxsmin[,myLT(mx_min, sex), by = list(year,sex)]

BPlx <- Mxsmin[,lx:=myLTlx(mx_min, sex), by = list(year,sex)]
BPlx[,Lx:=lx2Lx(lx),by = list(year,sex)]

# get temp e0 for BP
bpe0_14       <- BPlx[,getTempe0(.SD),by=list(year, sex)]
bpe0_14$state <- 33
bpe0_14$age.g <- 1

bpe15_49 <- BPlx[,getTempe0(.SD,15,49),by=list(year, sex)]
bpe15_49$state <- 33
bpe15_49$age.g <- 2

bpe50_84 <- BPlx[,getTempe0(.SD,50,84),by=list(year, sex)]
bpe50_84$state <- 33
bpe50_84$age.g <- 3

BP_temp <- rbind(bpe0_14,bpe15_49,bpe50_84)
setnames(BP_temp,"V1","temp_e0")

#######################################
# now do calcs for states
#######################################
#gdata:: keep(Mxsc,data,BP_temp,sure=T)
Mxs         <- data[,1:5,with=F]
#setnames(Mxs,"Tot","mx")

Stateslx    <- Mxs[,lx:=myLTlx(mx, sex), by = list(state,year, sex)]
Stateslx    <- Stateslx[,Lx := lx2Lx(lx), by = list(state,year, sex)]
head(Stateslx)
# now get temp e0 for states
ste0_14     <- Stateslx[,getTempe0(.SD,lowera = 0, uppera = 14),by=list(state,year, sex)]
ste0_14$age.g <-1
ste15_49    <- Stateslx[,getTempe0(.SD,lowera=15,uppera=49),by=list(state,year, sex)]
ste15_49$age.g <-2
ste50_84    <- Stateslx[,getTempe0(.SD,lowera=50,uppera=84),by=list(state,year, sex)]
ste50_84$age.g <-3

ste_temp <- rbind(ste0_14,ste15_49,ste50_84)
setnames(ste_temp,"V1","temp_e0")
head(ste_temp)


####### Now find vanguard temporary life expectancy
van_temp <- ste_temp[,max(temp_e0), by = list(year,sex,age.g)]
van_temp$state <- 34
setnames(van_temp,"V1","temp_e0")
head(van_temp)

#### create an object with all temp data

temp.data <- rbind(BP_temp,ste_temp,van_temp)
temp.data <- temp.data[order(year,sex,state,age.g)]

save(data,temp.data,Mxsc,file = "Data/Temp_e0_results.RData")

