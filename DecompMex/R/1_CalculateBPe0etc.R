setwd('C:\\Users\\aburtoflores\\Desktop\\Working papers\\Aburto & Riffe 2015')

if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/DecompMex/DecompMex")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/DecompMex/DecompMex"))
}

library(data.table)
library(reshape2)
source("R/Functions.R")
# load in data made in DataPrep.R
Mxs        <- local(get(load("Data/Mxs.Rdata")))
Mxsc       <- local(get(load("Data/Mxsc.Rdata")))


#########################################
# npow get BP Mx
Mxsc$Mx    <- Mxsc$Dx/Mxsc$Exposure
Mxsc       <- as.data.table(Mxsc)
Mxscmin    <- Mxsc[,min(Mx), by = list(Year, Sex, Age, AM.Group)]
setnames(Mxscmin, "V1","Mx")
Mxsmin     <- Mxscmin[,sum(Mx),by=list(Year, Sex, Age)]
setnames(Mxsmin, "V1","Mx")
Mxsmin     <- as.data.frame(Mxsmin)
Mxsmin     <- Mxsmin[with(Mxsmin, order(Year,Sex,Age)),]
Mxsmin     <- data.table(Mxsmin)

#######################################
# now get lifetable results for BP
BPe0 <- Mxsmin[,myLT(Mx, Sex), by = list(Year,Sex)]
setnames(BPe0, "V1","e0")
BPlx <- Mxsmin[,lx:=myLTlx(Mx, Sex), by = list(Year,Sex)]
BPlx[,Lx:=lx2Lx(lx),by = list(Year,Sex)]

#######################################
# get temp e0 for BP
bpe0_14 <- BPlx[,getTempe0(.SD),by=list(Year, Sex)]
bpe15_39 <- BPlx[,getTempe0(.SD,15,39),by=list(Year, Sex)]
bpe40_74 <- BPlx[,getTempe0(.SD,40,74),by=list(Year, Sex)]

#######################################
# now do calcs for states
#######################################

# get e0 and lx columsn for states
#Deaths.PopM <- Mxs[,sum(Mx),by=list(State,Year,Sex,Age)]
#setnames(Deaths.PopM, "V1","Mx")

Statese0    <- Mxs[,myLT(Mx, Sex), by = list(State,Year,Sex)]
setnames(Statese0, "V1","e0")
Stateslx    <- Mxs[,lx:=myLTlx(Mx, Sex), by = list(State,Year,Sex)]
Stateslx[,Lx := lx2Lx(lx), by = list(State,Year,Sex)]
# now get temp e0 for states
ste0_14     <- Stateslx[,getTempe0(.SD,lowera = 0, uppera = 14),by=list(State,Year, Sex)]
ste15_39    <- Stateslx[,getTempe0(.SD,lowera=15,uppera=39),by=list(State,Year, Sex)]
ste40_74    <- Stateslx[,getTempe0(.SD,lowera=40,uppera=74),by=list(State,Year, Sex)]

van0_14  <- ste0_14[, max(e0), by = list(Sex, Year)]
van15_39 <- ste15_39[, max(e0), by = list(Sex, Year)]
van40_74 <- ste40_74[, max(e0), by = list(Sex, Year)]



# this is how far we calculated stuff. Next decomposing and plotting.
setnames(ste40_74,"V1","e0")
setnames(ste15_39,"V1","e0")
setnames(ste0_14,"V1","e0")
setnames(bpe0_14,"V1","e0")
setnames(bpe15_39,"V1","e0")
setnames(bpe40_74,"V1","e0")

setnames(van0_14,"V1","e0")
setnames(van15_39,"V1","e0")
setnames(van40_74,"V1","e0")











