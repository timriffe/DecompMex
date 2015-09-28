# Author: tim
###############################################################################

setwd('C:\\Users\\aburtoflores\\Desktop\\Working papers\\Aburto & Riffe 2015')

if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/DecompMex/DecompMex")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/DecompMex/DecompMex"))
}

MxscSm    <- local(get(load("Data/MxscSm.Rdata")))

# prior to this, smoothing code run on Berkeley server

# 1) get the minimum for each cause/age/sex/year
Mins   <- MxscSm[,min(mxcs2),by=list(Year, Sex, Age, Cause)]
setnames(Mins,"V1","mxcmin")

# iterator for whoile script:
years  <- 1990:2010

# make sure we're uniform here...
ages   <- 0:74
Causes <- unique(MxscSM$Cause) # Causes <- 1:10
Empty  <- matrix(0,nrow=length(ages),ncol=length(Causes), dimnames=list(ages,Causes))

# ok, now we have Age~Cause matrices for the best practices. Each list element is a year.
FBPL <- lapply(years, function(yr,Mins,Empty){
			Mat <- acast(Mins[Mins$Year == yr & Mins$Sex == 2, ], Age~Cause, value.var = "mxcmin")
			Empty[rownames(Mat),colnames(Mat)] <- Mat
			Empty
		}, Mins = Mins)
MBPL <- lapply(years, function(yr,Mins,Empty){
			Mat <- acast(Mins[Mins$Year == yr & Mins$Sex == 1, ], Age~Cause, value.var = "mxcmin")
			Empty[rownames(Mat),colnames(Mat)] <- Mat
			Empty
		}, Mins = Mins)

MST <-  lapply(years, function(yr,LTC, Empty){
			YR <- LTC[LTC$Year == yr & LTC$Sex == 1, ]
			STlist <- lapply(unique(YR$State), function(state, YR, Empty){
						Mat <- acast(YR[YR$State == state, ], Age~Cause, value.var = "Mxcsm")
						Empty[rownames(Mat),colnames(Mat)] <- Mat
						Empty
					}, YR = YR, Empty = Empty)
			names(STlist) <- unique(YR$State)
			STlist
		}, LTC = LTC, Empty)
FST <-  lapply(1990:2010, function(yr,LTC, Empty){
			YR <- LTC[LTC$Year == yr & LTC$Sex == 2, ]
			STlist <- lapply(unique(YR$State), function(state, YR, Empty){
						Mat <- acast(YR[YR$State == state, ], Age~Cause, value.var = "Mxcsm")
						Empty[rownames(Mat),colnames(Mat)] <- Mat
						Empty
					}, YR = YR, Empty = Empty)
			names(STlist) <- unique(YR$State)
			STlist
		}, LTC = LTC, Empty)
names(MST)  <- 1959:2004
names(FST)  <- 1959:2004
names(FBPL) <- 1959:2004
names(MBPL) <- 1959:2004
# next
# now for a decomposable function, we need a function that gives
# e0 based on a vector of mxc
mxc      <- MST[[1]][[1]]
mxcvec   <- c(FBPL[[1]])
e0frommxc <- function(mxcvec,sex){
	dim(mxcvec) <- c(111,length(mxcvec)/111)
	mx          <- rowSums(mxcvec)
	LTuniformvecminimal(mx,sex)
}
e0frommxc(c(FBPL[[1]]),"f")
e0frommxc(c(FST[[1]][[1]]),"f")


library(DecompHoriuchi)
DecompContinuousOrig

# Males, takes a long time


system.time(contrib2 <- mydecomp(
				func = e0frommxc, 
				rates1 = c(FST[[1]][[1]]),
				rates2 = c(FBPL[[1]]),
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
for (yr in 1959:2004){
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
