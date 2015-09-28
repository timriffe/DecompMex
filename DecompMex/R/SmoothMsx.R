# check to see if it's necessary to smooth mx by state,sex
# Author: tim
###############################################################################


if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/DecompMex/DecompMex")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/DecompMex/DecompMex"))
}
library(data.table)
library(reshape2)
library(MortalitySmooth)

sm.mat <- function(DX, EX){
	ages  		<- as.integer(rownames(DX))
	years 		<- as.integer(colnames(EX))
	W     		<- EX
	W[W > 0] 	<- 1
	W[DX < 1 & W == 1] 	<- .3
	fit   <- Mort2Dsmooth(
			x = ages, 
			y = years, 
			Z = DX,
			offset = log(EX), 
			W = W,
			control=list(MAX.IT=200))
	mxs <- exp(fit$logmortality)
	mxs <- melt(mxs, varnames=c("Age","Year"), value.name = "mxs")
	mxs
}

# the iterator function for data.table
sm.chunk <- function(.SD){
	# using the Dx that came from the HMD smoothed
	# mx, keeps upper ages under control a bit more.
	DX      <- acast(.SD, Age~Year, value.var = "Dx", fill = 0)
	EX      <- acast(.SD, Age~Year, value.var = "Exposure", fill = 0)
	
	mxs     <- sm.mat(DX,EX)
	
	.SD$mxs <- mxs$mxs
	
	# still need to normalize exposure?
	.SD
}

Mxs <- local(get(load("Data/Mxs.Rdata")))

Mxs <- Mxs[,sm.chunk(.SD),by=list(State,Sex)]
head(Mxs)
source("R/Functions.R")
source("R/LTuniform.R")

Statese0    <- Mxs[,myLT(mxs, Sex), by = list(State,Year,Sex)]
setnames(Statese0, "V1","e0")
Stateslx    <- Mxs[,lx:=myLTlx(mxs, Sex), by = list(State,Year,Sex)]
Stateslx[,Lx := lx2Lx(lx), by = list(State,Year,Sex)]
# now get temp e0 for states
ste0_14     <- Stateslx[,getTempe0(.SD,lowera = 0, uppera = 14),by=list(State,Year, Sex)]
ste15_39    <- Stateslx[,getTempe0(.SD,lowera=15,uppera=39),by=list(State,Year, Sex)]
ste40_74    <- Stateslx[,getTempe0(.SD,lowera=40,uppera=74),by=list(State,Year, Sex)]

# this is how far we calculated stuff. Next decomposing and plotting.


setnames(ste40_74,"V1","e0")
setnames(ste15_39,"V1","e0")
setnames(ste0_14,"V1","e0")
van0_14  <- ste0_14[, max(e0), by = list(Sex, Year)]
van15_39 <- ste15_39[, max(e0), by = list(Sex, Year)]
van40_74 <- ste40_74[, max(e0), by = list(Sex, Year)]
setnames(van0_14,"V1","e0")
setnames(van15_39,"V1","e0")
setnames(van40_74,"V1","e0")

# print and save figures
pdf("Figures/et0_14s.pdf",width=6,height=5)
print(Fig0_14)
dev.off()
pdf("Figures/et15_39s.pdf",width=6,height=5)
print(Fig15_39)
dev.off()
pdf("Figures/et40_74s.pdf",width=6,height=5)
print(Fig40_74)
dev.off()













