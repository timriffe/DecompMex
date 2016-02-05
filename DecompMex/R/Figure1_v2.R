#Replace F1
#for JM
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
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}
my.settings <- list(
  strip.background=list(col="grey"),
  strip.border=list(col="black")
)

#State names
  Statenames <- NULL
  Statenames$ID <- 1:32
  name <- c("Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila","Colima","Chiapas",
  "Chihuahua","Distrito Federal","Durango","Guanajuato","Guerrero","Hidalgo","Jalisco","México","Michoacán",
  "Morelos","Nayarit","Nuevo León","Oaxaca","Puebla","Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora",
  "Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatán","Zacatecas")
  Statenames$name <- name
  Statenames  <- as.data.frame(Statenames)
      
  
# custom figure function for this run
# al JM majic here
  source("R/1_CalculateBPe0etc.R")
  source("R/SmoothMsx.R")

et_trends <- function(states,van,bp,mtitle,ylim = range(pretty(c(states$e0,van$e0,bp$e0))),
                      fig.key,ylab1,at1,arrowx11,arrowy1,arrowx21,text1x,text1,arrowy12,
                      arrowx12,arrowy2,arrowx22,text2x,text2,arrowy22,arrowx13,arrowy3,
                      arrowx23,text3x,text3,arrowy32,pnumber){
  
  states$Sex <- factor(states$Sex,levels=c(1,2),labels=c("Males", "Females"))
  van$Sex    <- factor(van$Sex,levels=c(1,2),labels=c("Males", "Females"))
  bp$Sex     <- factor(bp$Sex,levels=c(1,2),labels=c("Males", "Females"))
  
  Fig1<-xyplot(e0~Year|Sex,data=states,groups=State,type="l",lwd=2,between=list(x=2),
               xlim=c(1990,2010),main=mtitle,ylim=ylim,ylab=ylab1,
               layout=c(1,2),strip.left=T,strip=F,
               col=makeTransparent("black",alpha=65),par.settings=my.settings,xlab="Period", 
               key=fig.key,
               scales=list(alternating=1,x=list(cex=.75,at=c(seq(1990,2010,5))),
                           y=list(cex=.75,at=at1,alternating=1)),                                       
               panel = function(x, y,...){           
                 panel.abline(v=c(seq(1990,2010,5)),col='dark grey',lty=3)
                 if (panel.number()==pnumber){                 
                 # 1st state                 
                 panel.arrows(arrowx11,arrowy1,arrowx21,arrowy12,length=.1,col="black")
                 panel.text(text1x,arrowy12-.02,text1,cex=.7)
                 # 2nd state                 
                 panel.arrows(arrowx12,arrowy2,arrowx22,arrowy22,length=.1,col="black")  
                 panel.text(text2x,arrowy22-.02,text2,cex=.7)
                 # 3rd state
                 panel.arrows(arrowx13,arrowy3,arrowx23,arrowy32,length=.1,col="black")  
                 panel.text(text3x,arrowy32-.02,text3,cex=.7)                 }
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

  
Fig0_14  <- et_trends(ste0_14,van0_14,bpe0_14,"a) Young",c(14,15),
                      fig.key=list(space="bottom",background="transparent",
                                   text=list(c('State','Record holder','Low benchmark'),
                                             col="white"),cex=1.2,
                                   lines=list(lty=1,lwd=2,col=c("white","white","white"))),
                      ylab1="Temporary life expectancy",at1=seq(14,14.8,.2),
                      arrowx11=1995,
                      arrowy1=min(ste0_14$e0[ste0_14$Year==1995]),
                      arrowx21=1995,
                      text1x = 1996,
                      text1 = Statenames$name[Statenames$ID==ste0_14$State[ste0_14$e0==min(ste0_14$e0[ste0_14$Year==1995])]],
                      arrowy12=min(ste0_14$e0[ste0_14$Year==1995])-.1,                                            
                                                                                    
                      arrowx12=2000,
                      arrowy2=min(subset(ste0_14,Year==2000 & Sex==1 & e0 != min(subset(ste0_14,Year==2000 & Sex==1)$e0))$e0),
                      arrowx22=2000,
                      text2x =2001 ,
                      text2 =Statenames$name[Statenames$ID==ste0_14$State[ste0_14$e0==min(subset(ste0_14,Year==2000 & Sex==1 & e0 != min(subset(ste0_14,Year==2000 & Sex==1)$e0))$e0)]],
                      arrowy22=min(subset(ste0_14,Year==2000 & Sex==1 & e0 != min(subset(ste0_14,Year==2000 & Sex==1)$e0))$e0)-.1,
                      
                      arrowx13=2005,
                      arrowy3=min(subset(ste0_14, Year==2005 & Sex==1 & e0 !=min(ste0_14$e0[ste0_14$Year==2005 & ste0_14$Sex==1]) & 
                                           e0 != min(subset(ste0_14,Year==2005 & Sex==1 & e0 != min(subset(ste0_14,Year==2005 & Sex==1)$e0))$e0))$e0),
                      arrowx23=2005,
                      text3x = 2006,
                      text3 =Statenames$name[Statenames$ID==ste0_14$State[ste0_14$e0==min(subset(ste0_14, Year==2005 & Sex==1 & e0 !=min(ste0_14$e0[ste0_14$Year==2005 & ste0_14$Sex==1]) & 
                                                                    e0 != min(subset(ste0_14,Year==2005 & Sex==1 & e0 != min(subset(ste0_14,Year==2005 & Sex==1)$e0))$e0))$e0)]],
                      arrowy32=min(subset(ste0_14, Year==2005 & Sex==1 & e0 !=min(ste0_14$e0[ste0_14$Year==2005 & ste0_14$Sex==1]) & 
                                            e0 != min(subset(ste0_14,Year==2005 & Sex==1 & e0 != min(subset(ste0_14,Year==2005 & Sex==1)$e0))$e0))$e0)-.1,
                      
                      pnumber=1)

  
    
  
Fig15_39 <- et_trends(ste15_39,van15_39,bpe15_39,"b) Young adults",c(22.5,25),
                      fig.key=list(space="bottom",background="transparent",
                                   text=list(c('State','Record holder','Low benchmark')),cex=1.2,
                                   lines=list(lty=1,lwd=2,col=c("black","red","blue"))),
                      ylab1=" ",at1=seq(22.5,24.5,.5),
                      arrowx11=1995,
                      arrowy1=min(ste15_39$e0[ste15_39$Year==1995]),
                      arrowx21=1995,
                      text1x = 1996,
                      text1 = Statenames$name[Statenames$ID==ste15_39$State[ste15_39$e0==min(ste15_39$e0[ste15_39$Year==1995])]],
                      arrowy12=min(ste15_39$e0[ste15_39$Year==1995])-.5,                                            
                      
                      arrowx12=2000,
                      arrowy2=min(subset(ste15_39,Year==2000 & Sex==1 & e0 != min(subset(ste15_39,Year==2000 & Sex==1)$e0))$e0),
                      arrowx22=2000,
                      text2x =2001 ,
                      text2 =Statenames$name[Statenames$ID==ste15_39$State[ste15_39$e0==min(subset(ste15_39,Year==2000 & Sex==1 & e0 != min(subset(ste15_39,Year==2000 & Sex==1)$e0))$e0)]],
                      arrowy22=min(subset(ste15_39,Year==2000 & Sex==1 & e0 != min(subset(ste15_39,Year==2000 & Sex==1)$e0))$e0)-.5,
                      
                      arrowx13=2005,
                      arrowy3=min(subset(ste15_39, Year==2005 & Sex==1 & e0 !=min(ste15_39$e0[ste15_39$Year==2005 & ste15_39$Sex==1]) & 
                                           e0 != min(subset(ste15_39,Year==2005 & Sex==1 & e0 != min(subset(ste15_39,Year==2005 & Sex==1)$e0))$e0))$e0),
                      arrowx23=2005,
                      text3x = 2006,
                      text3 =Statenames$name[Statenames$ID==ste15_39$State[ste15_39$e0==min(subset(ste15_39, Year==2005 & Sex==1 & e0 !=min(ste15_39$e0[ste15_39$Year==2005 & ste15_39$Sex==1]) & 
                                                                                                   e0 != min(subset(ste15_39,Year==2005 & Sex==1 & e0 != min(subset(ste15_39,Year==2005 & Sex==1)$e0))$e0))$e0)]],
                      arrowy32=min(subset(ste15_39, Year==2005 & Sex==1 & e0 !=min(ste15_39$e0[ste15_39$Year==2005 & ste15_39$Sex==1]) & 
                                            e0 != min(subset(ste15_39,Year==2005 & Sex==1 & e0 != min(subset(ste15_39,Year==2005 & Sex==1)$e0))$e0))$e0)-.5,
                      
                      pnumber=1)

Fig40_74 <- et_trends(ste40_74,van40_74,bpe40_74,"c) Older adults",c(28,35),
                      fig.key=list(space="bottom",background="transparent",
                                   text=list(c('State','Record holder','Low benchmark'),
                                             col="white"),cex=1.2,
                                   lines=list(lty=1,lwd=2,col=c("white","white","white"))),
                      ylab1=" ",at1=seq(28,34,1),
                      arrowx11=1995,
                      arrowy1=min(ste40_74$e0[ste40_74$Year==1995]),
                      arrowx21=1995,
                      text1x = 1996,
                      text1 = Statenames$name[Statenames$ID==ste40_74$State[ste40_74$e0==min(ste40_74$e0[ste40_74$Year==1995])]],
                      arrowy12=min(ste40_74$e0[ste40_74$Year==1995])-.5,                                            
                      
                      arrowx12=2000,
                      arrowy2=min(subset(ste40_74,Year==2000 & Sex==1 & e0 != min(subset(ste40_74,Year==2000 & Sex==1)$e0))$e0),
                      arrowx22=2000,
                      text2x =2001 ,
                      text2 =Statenames$name[Statenames$ID==ste40_74$State[ste40_74$e0==min(subset(ste40_74,Year==2000 & Sex==1 & e0 != min(subset(ste40_74,Year==2000 & Sex==1)$e0))$e0)]],
                      arrowy22=min(subset(ste40_74,Year==2000 & Sex==1 & e0 != min(subset(ste40_74,Year==2000 & Sex==1)$e0))$e0)-.5,
                      
                      arrowx13=2005,
                      arrowy3=min(subset(ste40_74, Year==2005 & Sex==1 & e0 !=min(ste40_74$e0[ste40_74$Year==2005 & ste40_74$Sex==1]) & 
                                           e0 != min(subset(ste40_74,Year==2005 & Sex==1 & e0 != min(subset(ste40_74,Year==2005 & Sex==1)$e0))$e0))$e0),
                      arrowx23=2005,
                      text3x = 2006,
                      text3 =Statenames$name[Statenames$ID==ste40_74$State[ste40_74$e0==min(subset(ste40_74, Year==2005 & Sex==1 & e0 !=min(ste40_74$e0[ste40_74$Year==2005 & ste40_74$Sex==1]) & 
                                                                                                     e0 != min(subset(ste40_74,Year==2005 & Sex==1 & e0 != min(subset(ste40_74,Year==2005 & Sex==1)$e0))$e0))$e0)]],
                      arrowy32=min(subset(ste40_74, Year==2005 & Sex==1 & e0 !=min(ste40_74$e0[ste40_74$Year==2005 & ste40_74$Sex==1]) & 
                                            e0 != min(subset(ste40_74,Year==2005 & Sex==1 & e0 != min(subset(ste40_74,Year==2005 & Sex==1)$e0))$e0))$e0)-.5,
                      
                      pnumber=1)


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


  
      
  
  min(subset(ste0_14, ste0_14$e0 != min(ste0_14$e0[ste0_14$Year==2000 & ste0_14$Sex==1]))$e0[ste0_14$Year==2000
                                                                                             & ste0_14$Sex==1],na.rm = T)
  
  
  
  