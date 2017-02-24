#Replace F1
#for JM
  #setwd("/Users/josemanuelaf/Documents/DecompMex/DecompMex")
  setwd("/Users/josemanuelaf/Desktop/JM&TIM_beta")

library(lattice)
library(latticeExtra)

source("R/Data_Figure1_v2.R")  

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

et_trends <- function(states,van,bp,mtitle,ylim = range(pretty(c(states$e0,van$e0,bp$e0))),
                      fig.key,ylab1,at1,arrowx11,arrowy1,arrowx21,text1x,text1,arrowy12,
                      arrowx12,arrowy2,arrowx22,text2x,text2,arrowy22,arrowx13,arrowy3,
                      arrowx23,text3x,text3,arrowy32,arrowx11v,arrowy1v,arrowx21v,arrowy12v,text1xv,
                      textv,pnumber){
  
  states$Sex <- factor(states$Sex,levels=c(1,2),labels=c("Males", "Females"))
  van$Sex    <- factor(van$Sex,levels=c(1,2),labels=c("Males", "Females"))
  bp$Sex     <- factor(bp$Sex,levels=c(1,2),labels=c("Males", "Females"))
  
  Fig1<-xyplot(e0~Year|Sex,data=states,groups=State,type="l",lwd=2,between=list(y=1),
               xlim=c(1990,2010),main=mtitle,ylim=ylim,ylab=ylab1,
               layout=c(1,2),strip.left=T,strip=F,
               col=makeTransparent("black",alpha=65),par.settings=my.settings,xlab="Year", 
               key=fig.key,
               scales=list(alternating=1,x=list(cex=.75,at=c(seq(1990,2010,5))),
                           y=list(cex=.75,at=at1,alternating=1)),                                       
               panel = function(x, y,...){           
                 panel.abline(v=c(seq(1990,2010,5)),col='dark grey',lty=3)
                 if (panel.number()==pnumber){                 
                 # 1st state                 
                 panel.arrows(arrowx11,arrowy1,arrowx21,arrowy12,length=.1,col="black")
                 panel.text(text1x,arrowy12-.08,text1,cex=1)
                 # 2nd state                 
                 panel.arrows(arrowx12,arrowy2,arrowx22,arrowy22,length=.1,col="black")  
                 panel.text(text2x,arrowy22-.08,text2,cex=1)
                 # 3rd state
                 panel.arrows(arrowx13,arrowy3,arrowx23,arrowy32,length=.1,col="black")  
                 panel.text(text3x,arrowy32-.08,text3,cex=1)}                 
                 # Vanguard state
                 #panel.arrows(arrowx11v,arrowy1v,arrowx21v,arrowy12v,length=.05,col="red")
                 #panel.text(text1xv,arrowy12v-.08,textv,cex=1)}
                 panel.xyplot(x, y,lty=1,...)  
                 }
               
  )
  
  
  Fig2<-xyplot(e0~Year|Sex,data=van,type="l",lwd=2,between=list(y=1),
               xlim=c(1990,2010),main=mtitle,ylim=ylim,ylab=ylab1,
               layout=c(1,2),strip.left=T,strip=F,
               col="red",par.settings=my.settings,xlab="Year"
  )
  
  
  Fig3<-xyplot(e0~Year|Sex,data=bp,type="l",lwd=2,between=list(y=1),
               xlim=c(1990,2010),main=mtitle,ylim=ylim,ylab=ylab1,
               layout=c(1,2),strip.left=T,strip=F,
               col="blue",par.settings=my.settings,xlab="Year"
  )
  
  
  Fig4  <- Fig1 + Fig2 + Fig3
  return(Fig4)
}

  
Fig0_14  <- et_trends(ste0_14,van0_14,bpe0_14,"a) Young (0-14)",c(14,15),
                      fig.key=list(space="bottom",background="transparent",
                                   text=list(c('State','Record holder','Low benchmark'),
                                             col="white"),cex=1.2,
                                   lines=list(lty=1,lwd=2,col=c("white","white","white"))),
                      ylab1="Temporary life expectancy",at1=seq(14,15,.2),
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

Fig0_14  
    
   
   
Fig15_39 <- et_trends(ste15_39,van15_39,bpe15_39,"b) Young adults (15-39)",c(22.5,25),
                      fig.key=list(space="bottom",background="transparent",
                                   text=list(c('State','Record holder','Low benchmark')),cex=1.2,
                                   lines=list(lty=1,lwd=2,col=c("black","red","blue"))),
                      ylab1=" ",at1=seq(22.5,25,.5),
                      arrowx11=1995,
                      arrowy1=min(ste15_39$e0[ste15_39$Year==1995]),
                      arrowx21=1995,
                      text1x = 1996,
                      text1 = Statenames$name[Statenames$ID==ste15_39$State[ste15_39$e0==min(ste15_39$e0[ste15_39$Year==1995])]],
                      arrowy12=min(ste15_39$e0[ste15_39$Year==1995])-.7,                                            
                      
                      arrowx12=2000,
                      arrowy2=min(subset(ste15_39,Year==2000 & Sex==1 & e0 != min(subset(ste15_39,Year==2000 & Sex==1)$e0))$e0),
                      arrowx22=2000,
                      text2x =2001 ,
                      text2 =Statenames$name[Statenames$ID==ste15_39$State[ste15_39$e0==min(subset(ste15_39,Year==2000 & Sex==1 & e0 != min(subset(ste15_39,Year==2000 & Sex==1)$e0))$e0)]],
                      arrowy22=min(subset(ste15_39,Year==2000 & Sex==1 & e0 != min(subset(ste15_39,Year==2000 & Sex==1)$e0))$e0)-.7,
                      
                      #arrowx13=2005,
                      #arrowy3=min(subset(ste15_39, Year==2005 & Sex==1 & e0 !=min(ste15_39$e0[ste15_39$Year==2005 & ste15_39$Sex==1]) & 
                                     #      e0 != min(subset(ste15_39,Year==2005 & Sex==1 & e0 != min(subset(ste15_39,Year==2005 & Sex==1)$e0))$e0))$e0),
                      #arrowx23=2005,
                      #text3x = 2006,
                      #text3 =Statenames$name[Statenames$ID==ste15_39$State[ste15_39$e0==min(subset(ste15_39, Year==2005 & Sex==1 & e0 !=min(ste15_39$e0[ste15_39$Year==2005 & ste15_39$Sex==1]) & 
                       #                                                                            e0 != min(subset(ste15_39,Year==2005 & Sex==1 & e0 != min(subset(ste15_39,Year==2005 & Sex==1)$e0))$e0))$e0)]],
                      #arrowy32=min(subset(ste15_39, Year==2005 & Sex==1 & e0 !=min(ste15_39$e0[ste15_39$Year==2005 & ste15_39$Sex==1]) & 
                      #                      e0 != min(subset(ste15_39,Year==2005 & Sex==1 & e0 != min(subset(ste15_39,Year==2005 & Sex==1)$e0))$e0))$e0)-.4,
                      
                      
                      
                      arrowx13=2007,
                      arrowy3=min(ste15_39$e0[ste15_39$Year==2007]),
                      arrowx23=2007,
                      text3x = 2005,
                      text3 =Statenames$name[Statenames$ID==ste15_39$State[ste15_39$e0==min(ste15_39$e0[ste15_39$Year==2007])]],
                      arrowy32=min(ste15_39$e0[ste15_39$Year==2007])-.7,

                      
                      pnumber=1)
Fig15_39


Fig40_74 <- et_trends(ste40_74,van40_74,bpe40_74,"c) Older adults (40-74)",c(28,35),
                      fig.key=list(space="bottom",background="transparent",
                                   text=list(c('State','Record holder','Low benchmark'),
                                             col="white"),cex=1.2,
                                   lines=list(lty=1,lwd=2,col=c("white","white","white"))),
                      ylab1=" ",at1=seq(28,35,1),
                      arrowx11=1995,
                      arrowy1=min(ste40_74$e0[ste40_74$Year==1995]),
                      arrowx21=1995,
                      text1x = 1996,
                      text1 = Statenames$name[Statenames$ID==ste40_74$State[ste40_74$e0==min(ste40_74$e0[ste40_74$Year==1995])]],
                      arrowy12=min(ste40_74$e0[ste40_74$Year==1995])-.7,                                            
                      
                      arrowx12=2000,
                      arrowy2=min(subset(ste40_74,Year==2000 & Sex==1 & e0 != min(subset(ste40_74,Year==2000 & Sex==1)$e0))$e0),
                      arrowx22=2000,
                      text2x =2001 ,
                      text2 =Statenames$name[Statenames$ID==ste40_74$State[ste40_74$e0==min(subset(ste40_74,Year==2000 & Sex==1 & e0 != min(subset(ste40_74,Year==2000 & Sex==1)$e0))$e0)]],
                      arrowy22=min(subset(ste40_74,Year==2000 & Sex==1 & e0 != min(subset(ste40_74,Year==2000 & Sex==1)$e0))$e0)-.7,
                      
                      arrowx13=2005,
                      arrowy3=min(subset(ste40_74, Year==2005 & Sex==1 & e0 !=min(ste40_74$e0[ste40_74$Year==2005 & ste40_74$Sex==1]) & 
                                           e0 != min(subset(ste40_74,Year==2005 & Sex==1 & e0 != min(subset(ste40_74,Year==2005 & Sex==1)$e0))$e0))$e0),
                      arrowx23=2005,
                      text3x = 2006,
                      text3 =Statenames$name[Statenames$ID==ste40_74$State[ste40_74$e0==min(subset(ste40_74, Year==2005 & Sex==1 & e0 !=min(ste40_74$e0[ste40_74$Year==2005 & ste40_74$Sex==1]) & 
                                                                                                     e0 != min(subset(ste40_74,Year==2005 & Sex==1 & e0 != min(subset(ste40_74,Year==2005 & Sex==1)$e0))$e0))$e0)]],
                      arrowy32=min(subset(ste40_74, Year==2005 & Sex==1 & e0 !=min(ste40_74$e0[ste40_74$Year==2005 & ste40_74$Sex==1]) & 
                                            e0 != min(subset(ste40_74,Year==2005 & Sex==1 & e0 != min(subset(ste40_74,Year==2005 & Sex==1)$e0))$e0))$e0)-.6,

                      
                      pnumber=1)
Fig40_74

require(gridExtra)
pdf(file="Temp_fig.pdf",width=12,height=7,pointsize=12)
grid.arrange(Fig0_14,Fig15_39,Fig40_74, ncol=3)
dev.off()



############### table
Mxsc       <- local(get(load("Data/Mxsc.Rdata")))
library(xtable)
am.names <- c("Causes amenable to medical service","Diabetes","Ischemic heart diseases",
              "HIV/AIDS","Lung cancer","Cirrhosis","Homicide","Road traffic accidents",
              "Suicide","Other causes")
tab <- tapply(Mxsc$Dx,list(Mxsc$Sex, Mxsc$AM.Group), sum)
tab1<- round(tab/1000,0)
per <-   round(t(100 * tab/ rowSums(tab)),1)
t1  <- paste(per[,1],"%",sep=" ")
t2  <- paste(per[,2],"%",sep=" ")
ta  <- cbind(Category=am.names,Males =t1,MalesPercent=tab1[1,],Females=t2,FemalesPercent=tab1[2,])
    
print(xtable(prettyNum(ta,big.mark=","),
             caption = "Avoidable Mortality classification, 
             with crude percentages below age 75.",
             label="tab:causes"), include.rownames=F,caption.placement = "top")


  
######### Sensitivity analysis of classification
am.names <- c("AMS","Diabetes","Ischemic heart diseases",
              "HIV/AIDS","Lung cancer","Cirrhosis","Homicide","Road traffic accidents",
              "Suicide","Other causes")
tabClass <- tapply(Mxsc$Dx,list(Mxsc$Year, Mxsc$AM.Group), sum)
colnames(tabClass) <- am.names
Class    <- melt(tabClass)

ClassFig <- xyplot(value~Var1|Var2,group=Var2, data=Class, type="l",
                   ylab="Death counts", xlab="Year",
       scales=list(alternating=1,x=list(cex=.75,at=c(seq(1990,2010,5))),
                   y=list(relation="free")),                                       
       panel = function(x, y,...){           
         panel.abline(v=c(seq(1990,2010,5)),col='dark grey',lty=3)
         panel.abline(v=1998,col='red',lty=1)
         panel.grid(h=-1,v=0,col='dark grey',lty=3)           
         panel.xyplot(x, y,lty=1,...)  
       })

pdf(file="Outcomes/Class_fig.pdf",width=10,height=7,pointsize=12)
print(ClassFig)
dev.off()



############### Gini analysis
giniSimple <- function(x){
  N          <- length(x)
  S          <- sum(x)
  xlorenz    <- cumsum(sort(x)) / S
  xequal     <- cumsum(rep(S / N, N)) / S
  2 * sum(xequal - xlorenz)
}

steTotal <- ste0_14
steTotal$e0 <- ste0_14$e0 + ste15_39$e0 + ste40_74$e0



gini.Young <- ste0_14[,giniSimple(e0), by = list(Year,Sex)]
gini.YAdul <- ste15_39[,giniSimple(e0), by = list(Year,Sex)]
gini.OldAl <- ste40_74[,giniSimple(e0), by = list(Year,Sex)]
gini.Total <- steTotal[,giniSimple(e0), by = list(Year,Sex)]

gini.Young$Age <- 1
gini.YAdul$Age <- 2
gini.OldAl$Age <- 3
gini.Total$Age <- 4

gini.data <- rbind(gini.Young,gini.YAdul,gini.OldAl,gini.Total)
gini.data$Sex <- factor(gini.data$Sex,levels=c(1,2),labels=c("Males", "Females"))



Gini.fig <-xyplot(V1~Year|Sex,data=gini.data,groups=Age,type="l",lwd=2,between=list(y=1),
             xlim=c(1990,2010),ylim=c(0,.5),ylab="Gini coefficient",
             layout=c(1,2),strip.left=T,strip=F,
             col=c("green","blue","red","black"),par.settings=my.settings,xlab="Year", 
             key=list(x=.1,y=.99,background="transparent",
                      text=list(c('Young (0-14)','Young adults(15-39)','Older adults(40-74)','Total (0-74)'),
                                col="black"),cex=1,
                      lines=list(lty=c(1,1,1,2),lwd=2,col=c("green","blue","red","black"))),
             scales=list(alternating=1,x=list(cex=.75,at=c(seq(1990,2010,5))),
                         y=list(cex=.75,at=seq(0,.5,.1),alternating=1)),                                       
             panel = function(x, y,...){           
               panel.abline(v=c(seq(1990,2010,5)),col='dark grey',lty=3)
               panel.abline(h=c(seq(0,.5,.1)),col='dark grey',lty=3)
               panel.xyplot(x, y,lty=c(1,1,1,2),...)  
             })
Gini.fig             

pdf(file="Outcomes/Gini_fig.pdf",width=5,height=9,pointsize=12)
print(Gini.fig)
dev.off()


mean(ste40_74$e0[ste40_74$Sex==1]) - mean(bpe40_74$e0[bpe40_74$Sex==1])
mean(ste40_74$e0[ste40_74$Sex==2]) - mean(bpe40_74$e0[bpe40_74$Sex==2])
35 - mean(bpe40_74$e0[bpe40_74$Sex==1])
35 - mean(bpe40_74$e0[bpe40_74$Sex==2])