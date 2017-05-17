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


load("Data/cv_figure.RData")

#### a function to get the differenes with the low mortality benchmark

my.settings <- list(
  strip.background=list(col="grey"),
  strip.border=list(col="black")
)

cv <- function(x,z){ y <- sd(x)/z[1]
y}

Data           <- ste_temp
BP_temp$new    <- BP_temp$temp_e0
Data           <- Data[BP_temp, lmb := new, on = c(year='year',sex='sex',age.g='age.g')]
Data$dif       <- Data$lmb - Data$temp_e0
  



cv.states <- Data[,cv(dif,lmb), by = list(year,sex,age.g)]


cv.states$sex <- factor(cv.states$sex,levels=c(1,2),labels=c("Males", "Females"))


Greens           <- brewer.pal(5,"Blues")[2:5]

cv.fig <-xyplot(V1~year|sex,data=cv.states,groups=age.g,type="l",lwd=2,between=list(x=1),
                xlim=c(1990,2015),main="Survival inequality",
                #ylim=c(0,.6),
                ylab="Coefficient of variation",
                layout=c(2,1),strip.left=F,strip=T,
                col=Greens[-4],par.settings=my.settings,xlab="Year",
                key=list(x=.2,y=.9,background="transparent",
                         text=list(c('Young (0-14)','Young adults(15-49)','Older adults(50-84)'),
                                   col="black"),cex=1,
                         lines=list(lty=c(1,1,1),lwd=2,col=Greens[-4])),
                scales=list(x=list(cex=.75,at=c(seq(1990,2015,5))),
                            y=list(cex=.75,at=seq(0,.05,.005),alternating=1)),
                panel = function(x, y,...){
                  panel.abline(v=c(seq(1990,2015,5)),col='dark grey',lty=3)
                  panel.abline(h=c(seq(0,.05,.005)),col='dark grey',lty=3)
                  panel.xyplot(x, y,lty=c(1,1,1,2),...)
                })
cv.fig
pdf(file="Appendix Figures/CVfig.pdf",width=11,height=7,pointsize=12)
print(cv.fig)
dev.off()


Greens           <- brewer.pal(5,"Blues")[2:5]

cv.fig2 <-xyplot(V1~year,data=cv.states[sex=="Males"],groups=age.g,type="l",between=list(x=1),
                 xlim=c(1990,2015),main="Disparities between states",
                 ylim = c(0,.04),
                 ylab="Coefficient of variation",
                 strip.left=F,strip=T,
                 col=Greens[-4],par.settings=my.settings,xlab="Year", 
                 #  key=list(x=.2,y=.9,background="transparent",
                 #   text=list(c('Young (0-14)','Young adults(15-49)','Older adults(50-84)'),
                 #            col="black"),cex=1,
                 #    lines=list(lty=c(1,1,1),lwd=2,col=Greens[-4])),
                 scales=list(x=list(cex=1,at=c(seq(1990,2015,5))),
                             y=list(cex=1,at=seq(0,.05,.005),alternating=1)),                                       
                 panel = function(x, y,...){           
                   panel.abline(v=c(seq(1990,2015,5)),col='dark grey',lty=3)
                   panel.abline(h=c(seq(0,.05,.005)),col='dark grey',lty=3)
                   panel.xyplot(x, y,lwd = 4,...)  
                 })
cv.fig2      

pdf(file="Paper Figures/CVfig_males.pdf",width=5,height=4,pointsize=12)
print(cv.fig2)
dev.off()
