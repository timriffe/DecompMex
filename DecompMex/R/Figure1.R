setwd('C:\\Users\\aburtoflores\\Desktop\\Working papers\\Aburto & Riffe 2015')

if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/DecompMex/DecompMex")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/DecompMex/DecompMex"))
}

source("R/1_CalculateBPe0etc.R")
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
et_trends <- function(states,van,bp,ylim = range(pretty(c(states$e0,van$e0,bp$e0)))){
	
	
	
	
	states$Sex <- factor(states$Sex,levels=c(1,2),labels=c("Males", "Females"))
	van$Sex <- factor(van$Sex,levels=c(1,2),labels=c("Males", "Females"))
	bp$Sex <- factor(bp$Sex,levels=c(1,2),labels=c("Males", "Females"))
	
	Fig1<-xyplot(e0~Year|Sex,data=states,groups=State,type="l",lwd=2,between=list(x=2),
			xlim=c(1990,2010),main="",ylim=ylim,ylab="Life Expectancy",
			col=makeTransparent("black",alpha=65),par.settings=my.settings,xlab="Period", 
			key=list(space="bottom",background="white",text=list(c('Life Expectancy','VG Life Expectancy','BP Life Expectancy')),cex=.8,
					lines=list(lty=1,lwd=2,col=c(makeTransparent("black",alpha=65),makeTransparent("red",alpha=100),
									makeTransparent("blue",alpha=100)))),
			scales=list(alternating=1,x=list(cex=.75,at=c(seq(1990,2010,5))),
					y=list(cex=.75,at=pretty(ylim),alternating=1)),                        
			#panel = function(x, y, ...){                        
			#	panel.abline(v=c(seq(1990,2010,1)),col='dark grey',lty=3)
			#	panel.abline(v=c(seq(1990,2010,5)),col='dark grey',lty=1)
			#	panel.abline(h=c(seq(0,90,5)),col='dark grey',lty=3)
			#	panel.xyplot(x, y,lty=1,...)	
			#}
			)
	
	
	Fig2<-xyplot(e0~Year|Sex,data=van,type="l",lwd=2,between=list(x=2),
			xlim=c(1990,2010),main="Life expectancy experiment",ylim=ylim,ylab="Life Expectancy",
			col=makeTransparent("red",alpha=100),par.settings=my.settings,xlab="Period"#,
#			key=list(space="bottom",background="white",text=list(c('Life Expectancy','VG Life Expectancy','BP Life Expectancy')),cex=.8,
#					lines=list(lty=1,lwd=2,col=c(makeTransparent("black",alpha=65),makeTransparent("red",alpha=100),
#									makeTransparent("blue",alpha=100)))),
#			scales=list(alternating=1,x=list(cex=.75,at=c(seq(1990,2010,2))),
#					y=list(cex=.75,at=c(seq(55,77,5)),alternating=1)),                        
#			panel = function(x, y, ...){                        
#				panel.abline(v=c(seq(1990,2010,1)),col='dark grey',lty=3)
#				panel.abline(v=c(seq(1990,2010,5)),col='dark grey',lty=1)
#				panel.abline(h=c(seq(0,90,5)),col='dark grey',lty=3)
#				panel.xyplot(x, y,lty=1,...)
#				
#			}
	)
	
	
	Fig3<-xyplot(e0~Year|Sex,data=bp,type="l",lwd=2,between=list(x=2),
			xlim=c(1990,2010),main="Life expectancy experiment",ylim=ylim,ylab="Life Expectancy",
			col=makeTransparent("blue",alpha=100),par.settings=my.settings,xlab="Period"#,
#			key=list(space="bottom",background="white",text=list(c('Life Expectancy','VG Life Expectancy','BP Life Expectancy')),cex=.8,
#					lines=list(lty=1,lwd=2,col=c(makeTransparent("black",alpha=65),makeTransparent("red",alpha=100),
#									makeTransparent("blue",alpha=100)))),
#			scales=list(alternating=1,x=list(cex=.75,at=c(seq(1990,2010,2))),
#					y=list(cex=.75,at=c(seq(55,77,5)),alternating=1)),                        
#			panel = function(x, y, ...){                        
#				panel.abline(v=c(seq(1990,2010,1)),col='dark grey',lty=3)
#				panel.abline(v=c(seq(1990,2010,5)),col='dark grey',lty=1)
#				panel.abline(h=c(seq(0,90,5)),col='dark grey',lty=3)
#				panel.xyplot(x, y,lty=1,...)
#				
#			}
)

	Fig4  <- Fig1 + Fig2 + Fig3
	return(Fig4)
}


Fig0_14 <- et_trends(ste0_14,van0_14,bpe0_14,c(14,15))
Fig15_39 <- et_trends(ste15_39,van15_39,bpe15_39,c(22.5,25))
Fig40_74 <- et_trends(ste40_74,van40_74,bpe40_74,c(28,35))

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
