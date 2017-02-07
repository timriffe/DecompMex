setwd("/Users/josemanuelaf/Documents/DecompMex/DecompMex")

if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/DecompMex/DecompMex")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/DecompMex/DecompMex"))
}


library(lattice)
library(latticeExtra)
# some functions from JM
makeTransparent<-function(someColor, alpha=100) {
	newColor<-col2rgb(someColor)
	apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
						blue=curcoldata[3],alpha=alpha, maxColorValue=255)}) }


my.settings <- list(
		strip.background=list(col="grey"),
		strip.border=list(col="black")
)

# custom figure function for this run
# al JM majic here
et_trends <- function(states,van,bp,mtitle,ylim = range(pretty(c(states$e0,van$e0,bp$e0))),
                      fig.key,ylab1,at1){
	
	states$Sex <- factor(states$Sex,levels=c(1,2),labels=c("Males", "Females"))
	van$Sex <- factor(van$Sex,levels=c(1,2),labels=c("Males", "Females"))
	bp$Sex <- factor(bp$Sex,levels=c(1,2),labels=c("Males", "Females"))
	
	Fig1<-xyplot(e0~Year|Sex,data=states,groups=State,type="l",lwd=2,between=list(x=2),
			xlim=c(1990,2010),main=mtitle,ylim=ylim,ylab=ylab1,
      layout=c(1,2),strip.left=T,strip=F,
			col=makeTransparent("black",alpha=65),par.settings=my.settings,xlab="Period", 
			key=fig.key,
			scales=list(alternating=1,x=list(cex=.75,at=c(seq(1990,2010,5))),
					y=list(cex=.75,at=at1,alternating=1)),                        
			panel = function(x, y, ...){                        
				panel.abline(v=c(seq(1990,2010,5)),col='dark grey',lty=3)				
				panel.xyplot(x, y,lty=1,...)	
			}
			)
	
	
	Fig2<-xyplot(e0~Year|Sex,data=van,type="l",lwd=2,between=list(x=2),
			xlim=c(1990,2010),main=mtitle,ylim=ylim,ylab=ylab1,
			layout=c(1,2),strip.left=T,strip=F,
			col="red",par.settings=my.settings,xlab="Period"
	)
	
	
	Fig3<-xyplot(e0~Year|Sex,data=bp,type="l",lwd=2,between=list(x=2),
			xlim=c(1990,2010),main=mtitle,ylim=ylim,ylab=ylab1,
			layout=c(1,2),strip.left=T,strip=F,
			col="blue",par.settings=my.settings,xlab="Period"
)


	Fig4  <- Fig1 + Fig2 + Fig3
	return(Fig4)
}

source("R/1_CalculateBPe0etc.R")
source("R/SmoothMsx.R")

Fig0_14  <- et_trends(ste0_14,van0_14,bpe0_14,"a) Young",c(14,15),
                      fig.key=list(space="bottom",background="transparent",
                                   text=list(c('State','Record holder','Low benchmark'),
                                             col="white"),cex=1.2,
                                   lines=list(lty=1,lwd=2,col=c("white","white","white"))),
                      ylab1="Temporary life expectancy",at1=seq(14,14.8,.2))

Fig15_39 <- et_trends(ste15_39,van15_39,bpe15_39,"b) Young adults",c(22.5,25),
                      fig.key=list(space="bottom",background="transparent",
                      text=list(c('State','Record holder','Low benchmark')),cex=1.2,
                      lines=list(lty=1,lwd=2,col=c("black","red","blue"))),
                      ylab1=" ",at1=seq(22.5,24.5,.5))

Fig40_74 <- et_trends(ste40_74,van40_74,bpe40_74,"c) Older adults",c(28,35),
                      fig.key=list(space="bottom",background="transparent",
                       text=list(c('State','Record holder','Low benchmark'),
                      col="white"),cex=1.2,
                      lines=list(lty=1,lwd=2,col=c("white","white","white"))),
                      ylab1=" ",at1=seq(28,34,1))


require(gridExtra)
pdf(file="Figures/Temp_fig.pdf",width=12,height=7,pointsize=12)
grid.arrange(Fig0_14,Fig15_39,Fig40_74, ncol=3)
dev.off()



# print and save figures
pdf("Figures/et0_14.pdf",width=6,height=5)
print(Fig0_14)
dev.off()
pdf("Figures/et15_39.pdf",width=6,height=5)
print(Fig15_39)
dev.off()
pdf("Figures/et40_74.pdf",width=6,height=5)
print(Fig40_74)
dev.off()


###############
Mxsc       <- local(get(load("Data/Mxsc.Rdata")))
library(xtable)
tab <- tapply(Mxsc$Dx,list(Mxsc$Sex, Mxsc$AM.Group), sum)
t(100 * tab / rowSums(tab))
print(xtable(t(100 * tab / rowSums(tab))))
