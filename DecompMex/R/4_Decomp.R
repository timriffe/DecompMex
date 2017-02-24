# Author: tim
###############################################################################

setwd("C:/Users/jmaburto/Documents/GitHub/DecompMex/DecompMex")

if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
  # if I'm on the laptop
  setwd("/home/tim/git/DecompMex/DecompMex")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/DecompMex/DecompMex"))
}


library(data.table)
library(reshape2)

load("Data/Temp_e0_results.RData")


# Arrange data objects ----------------------------------------------------


# 1) get the minimum for each cause/age/sex/year
Mins   <- Mxsc[,min(mx),by=list(year, sex, age, cause)]
setnames(Mins,"V1","mxcmin")
head(Mins)
# iterator for whoile script:
years  <- 1990:2015

# make sure we're uniform here...
ages   <- 0:109
Causes <- unique(Mxsc$cause) 
#Causes <- 1:10
Empty  <- matrix(0,nrow=length(ages),ncol=length(Causes), dimnames=list(ages,Causes))

# ok, now we have Age~Cause matrices for the best practices. Each list element is a year.
FBPL <- lapply(years, function(yr,Mins,Empty){
  Mat <- acast(Mins[Mins$year == yr & Mins$sex == 2, ], age~cause, value.var = "mxcmin")
  Empty[rownames(Mat),colnames(Mat)] <- Mat
  Empty
}, Mins = Mins,Empty=Empty)

MBPL <- lapply(years, function(yr,Mins,Empty){
  Mat <- acast(Mins[Mins$year == yr & Mins$sex == 1, ], age~cause, value.var = "mxcmin")
  Empty[rownames(Mat),colnames(Mat)] <- Mat
  Empty
}, Mins = Mins, Empty=Empty)


MST <-  lapply(years, function(yr,LTC, Empty){
  YR <- LTC[LTC$year == yr & LTC$sex == 1, ]
  STlist <- lapply(unique(YR$state), function(st, YR, Empty){
    Mat <- as.matrix(YR[YR$state == st, 6:15,with=F])
    rownames(Mat)<-0:109
    Empty[rownames(Mat),colnames(Mat)] <- Mat
    Empty
  }, YR = YR, Empty = Empty)
  names(STlist) <- unique(YR$state)
  STlist
}, LTC = data, Empty=Empty)

FST <-  lapply(years, function(yr,LTC, Empty){
  YR <- LTC[LTC$year == yr & LTC$sex == 2, ]
  STlist <- lapply(unique(YR$state), function(st, YR, Empty){
    Mat <- as.matrix(YR[YR$state == st, 6:15,with=F])
    rownames(Mat)<-0:109
    Empty[rownames(Mat),colnames(Mat)] <- Mat
    Empty
  }, YR = YR, Empty = Empty)
  names(STlist) <- unique(YR$state)
  STlist
}, LTC = data, Empty=Empty)


names(MST)  <- 1990:2015
names(FST)  <- 1990:2015
names(FBPL) <- 1990:2015
names(MBPL) <- 1990:2015

source("R/Functions.R")

# decomposition of life expectancy (not in the paper) ---------------------


# next
# now for a decomposable function, we need a function that gives
# e0 based on a vector of mxc
mxc      <- c(FST$`2005`$`9`)
mxcvec   <- c(FBPL[[1]])
e0frommxc <- function(mxcvec,sex){
  dim(mxcvec) <- c(110,length(mxcvec)/110)
  mx          <- rowSums(mxcvec)
  LTuniformvecminimal(mx,sex)
}
e0frommxc(mxc ,"f")
e0frommxc(mxcvec,"f")


#library(DecompHoriuchi)
#DecompContinuousOrig

# Males, takes a long time
system.time(contrib2 <- mydecomp(
  func = e0frommxc, 
  rates1 = mxc,
  rates2 = mxcvec,
  N = 20,
  sex = "f"))

15 * 51 * length(1959:2004)
(35190 / 60) / 60

#################################################################
# this will take a long time, so best prepare for a server run. #
#################################################################
library(parallel)

# takes a long time!
Females <- list()
Males   <- list()

# ca 2 hours on 4 cores
# mclapply does not work on windows, 
#install_github('nathanvan/parallelsugar')
library(parallelsugar)

############### Decomp for life expectancy

for (yr in 1990:2015){
  cat("\nYear\n")
  Females[[as.character(yr)]] <- mclapply(FST[[as.character(yr)]], function(YRST, YRBP, e0frommxc){
    contrib <- mydecomp(
      func = e0frommxc, 
      rates1 = c(YRST),
      rates2 = c(YRBP),
      N = 20,
      sex = "f"
    )
    dim(contrib)        <- dim(YRST)
    dimnames(contrib)   <- dimnames(YRST)
    contrib
  }, YRBP = FBPL[[as.character(yr)]], e0frommxc = e0frommxc, mc.cores = 4)
  gc()
  # repeat for males
  Males[[as.character(yr)]] <- mclapply(MST[[as.character(yr)]], function(YRST, YRBP, e0frommxc){
    contrib <- mydecomp(
      func = e0frommxc, 
      rates1 = c(YRST),
      rates2 = c(YRBP),
      N = 20,
      sex = "m"
    )
    dim(contrib)        <- dim(YRST)
    dimnames(contrib)   <- dimnames(YRST)
    contrib
  }, YRBP = MBPL[[as.character(yr)]], e0frommxc = e0frommxc, mc.cores = 4)
  gc()
}

save(Females, file = "Data/fcontrib.Rdata")
save(Males, file = "Data/mcontrib.Rdata")

# Decomposition of temporary life expectancy 0-14, 15-39, 40-74 -------------------------

mxc      <- c(FST$`2005`$`9`)
mxcvec   <- c(FBPL[[1]])

mcx2etemp(mxc ,"f",0,14)
mcx2etemp(mxcvec,"f",0,14)
mcx2etemp(mxc ,"f",15,49)
mcx2etemp(mxcvec,"f",15,49)
mcx2etemp(mxc ,"f",50,84)
mcx2etemp(mxcvec,"f",50,84)



#library(DecompHoriuchi)
#DecompContinuousOrig

# Males, takes a long time
system.time(contrib2 <- mydecomp(
  func = mcx2etemp, 
  rates1 = mxc,
  rates2 = mxcvec,
  N = 20,
  sex = "f",
  lowera=0,
  uppera=14))




library(parallel)
library(parallelsugar)




Females <- list()
Males   <- list()

for (yr in 1990:2015){
  cat("\nYear\n",yr)
  Females[[as.character(yr)]] <- mclapply(FST[[as.character(yr)]], function(YRST, YRBP, mcx2etemp){
    contrib <- mydecomp(
      func = mcx2etemp, 
      rates1 = c(YRST),
      rates2 = c(YRBP),
      N = 40,
      sex = "f",
      lowera=0,
      uppera=14
    )
    dim(contrib)        <- dim(YRST)
    dimnames(contrib)   <- dimnames(YRST)
    contrib
  }, YRBP = FBPL[[as.character(yr)]], mcx2etemp = mcx2etemp, mc.cores = 4)
  gc()
  # repeat for males
  Males[[as.character(yr)]] <- mclapply(MST[[as.character(yr)]], function(YRST, YRBP, mcx2etemp){
    contrib <- mydecomp(
      func = mcx2etemp, 
      rates1 = c(YRST),
      rates2 = c(YRBP),
      N = 40,
      sex = "m",
      lowera=0,
      uppera=14
    )
    dim(contrib)        <- dim(YRST)
    dimnames(contrib)   <- dimnames(YRST)
    contrib
  }, YRBP = MBPL[[as.character(yr)]], mcx2etemp = mcx2etemp, mc.cores = 4)
  gc()
}

save(Females, file = "Data/ContribFemales0_14.Rdata")
save(Males, file = "Data/ContribMales0_14.Rdata")



###########################################################

Females <- list()
Males   <- list()

for (yr in 1990:2015){
  cat("\nYear\n",yr)
  Females[[as.character(yr)]] <- mclapply(FST[[as.character(yr)]], function(YRST, YRBP, mcx2etemp){
    contrib <- mydecomp(
      func = mcx2etemp, 
      rates1 = c(YRST),
      rates2 = c(YRBP),
      N = 40,
      sex = "f",
      lowera=15,
      uppera=49
    )
    dim(contrib)        <- dim(YRST)
    dimnames(contrib)   <- dimnames(YRST)
    contrib
  }, YRBP = FBPL[[as.character(yr)]], mcx2etemp = mcx2etemp, mc.cores = 4)
  gc()
  # repeat for males
  Males[[as.character(yr)]] <- mclapply(MST[[as.character(yr)]], function(YRST, YRBP, mcx2etemp){
    contrib <- mydecomp(
      func = mcx2etemp, 
      rates1 = c(YRST),
      rates2 = c(YRBP),
      N = 40,
      sex = "m",
      lowera=15,
      uppera=49
    )
    dim(contrib)        <- dim(YRST)
    dimnames(contrib)   <- dimnames(YRST)
    contrib
  }, YRBP = MBPL[[as.character(yr)]], mcx2etemp = mcx2etemp, mc.cores = 4)
  gc()
}

save(Females, file = "Data/ContribFemales15_49.Rdata")
save(Males, file = "Data/ContribMales15_49.Rdata")


Females <- list()
Males   <- list()

for (yr in 1990:2015){
  cat("\nYear\n",yr)
  Females[[as.character(yr)]] <- mclapply(FST[[as.character(yr)]], function(YRST, YRBP, mcx2etemp){
    contrib <- mydecomp(
      func = mcx2etemp, 
      rates1 = c(YRST),
      rates2 = c(YRBP),
      N = 40,
      sex = "f",
      lowera=50,
      uppera=84
    )
    dim(contrib)        <- dim(YRST)
    dimnames(contrib)   <- dimnames(YRST)
    contrib
  }, YRBP = FBPL[[as.character(yr)]], mcx2etemp = mcx2etemp, mc.cores = 4)
  gc()
  # repeat for males
  Males[[as.character(yr)]] <- mclapply(MST[[as.character(yr)]], function(YRST, YRBP, mcx2etemp){
    contrib <- mydecomp(
      func = mcx2etemp, 
      rates1 = c(YRST),
      rates2 = c(YRBP),
      N = 40,
      sex = "m",
      lowera=50,
      uppera=84
    )
    dim(contrib)        <- dim(YRST)
    dimnames(contrib)   <- dimnames(YRST)
    contrib
  }, YRBP = MBPL[[as.character(yr)]], mcx2etemp = mcx2etemp, mc.cores = 4)
  gc()
}

save(Females, file = "Data/ContribFemales50_84.Rdata")
save(Males, file = "Data/ContribMales50_84.Rdata")
