
#library(LifeTable)
# one year, and one sex at a time
#LT(Nx = Exposure, Dx = Deaths, Mx = Dx/Nx, ages = 0:(length(Mx) - 1), axmethod = "midpoint", sex = "female", mxsmooth = FALSE, 
#		axsmooth = FALSE, radix = 1, verbose = FALSE)

########## Program to analyze sex mortality differences in Avoidable Mortality
setwd("//Users//josemanuelaf//Desktop//Aburto 2015 gender disparities//Data")
setwd('C:\\Users\\aburtoflores\\Desktop\\Working papers\\Riffe & Aburto 2015\\Data')

#install.packages('reshape')
#install.packages('gdata')
#install.packages('latticeExtra')
#install.packages('ggplot2')
#(install.packages("cause.decomp_1.0.zip", repos = NULL))
#install.packages("devtools",dependencies=TRUE)
#install.packages('data.table')

library(ggplot2)
library(reshape)
library(gdata)
library(latticeExtra)
library(cause.decomp)
library(data.table)
library(devtools)

# Perform Analyses --------------------------------------------------------

getwd()
Mex.Data<-local(get(load("AM_Data&Population1990-2010.RData")))
Mex.Data<-local(get(load("/media/tim/USB_JMAF/Riffe & Aburto 2015/Data/AM_Data&Population1990-2010.RData")))

#############################################
# The function needs 5 arguments:           
# 1. Age specific death rates 
# 2. radix for the life table
# 3. Oldest age in the mort rates
# 4. Sex (i.e., male/female death rates
# 5. Length of time mort is assumed constant when closing the life table
#############################################

## This first part is only by age, I aggregate data by AM

lifetab <- function(nMx, Rdx=1, max.age=100, sex="male", length.const.mort=10)   {
  
  
  x = c(0,1,seq(5,max.age,by=5))
  row<-which(x==max.age)
  
  NMx <- matrix(0,row,1)
  kk<-as.matrix(nMx)
  
  NMx[,1] <- kk[1:row,]
  dime<-dim(NMx)
  
  n <-c(1,4, rep(5,row-3), 0)
  nax<-rep(0,row)
  
  if(nMx[1] >=0.107 & sex=="male")   {nax[1]<-0.33 ; nax[2]<-1.352}
  if(nMx[1] < 0.107 & sex=="male")   {nax[1]<-0.045+2.684*nMx[1]; nax[2]<-1.651-2.816*nMx[1]}
  if(nMx[1] >=0.107 & sex=="female") {nax[1]<-0.35; nax[1]<-1.361}
  if(nMx[1] < 0.107 & sex=="female") {nax[1]<-0.053+2.8*nMx[1]; nax[2]<-1.522-1.518*nMx[1]}
  
  nAx <- matrix(0,row,5)
  nAx[,1] <-nax
  
  
  nQx <-matrix(0,dime[1],5)
  for(i in 1:dime[1]-1) {
    nQx[i,1]= n[i]*NMx[i,1]/(1+(n[i]-nAx[i,1])*NMx[i,1])
    nQx[dime[1],1]=1
  }
  
  nPx <-matrix(0,dime[1],5)
  nPx[,1] <- 1-nQx[,1]
  
  Lx <-matrix(Rdx,dime[1],5)
  for(i in 2:dime[1]) {
    Lx[i,1] = nPx[i-1,1]*Lx[i-1,1]
  }
  
  nDx <-matrix(0,dime[1],5)
  nDx[,1]<- nQx[,1]*Lx[,1]
  
  NLx <-matrix(0,dime[1],5)
  for(i in 1:dime[1]-1) {
    NLx[i,1] = n[i]*Lx[i+1,1]+nAx[i,1]*nDx[i,1]
    NLx[dime[1],1]=Lx[dime[1],1]/NMx[dime[1],1]    
  }
  
  Txx <- matrix(0,dime[1],5)
  Txx[,1] <- rev(cumsum(rev(NLx[,1])))
  
  Ex <-matrix(0,dime[1],5)
  Ex[,1] <- Txx[,1]/Lx[,1]
  
  
  for(j in 2:5) {
    nAx[1,j]=nax[1]
    nAx[2,j]=nax[2]
    #nAx[3,j]=a5
  }
  
  for(j in 2:5) {
    
    for(k in 3:dime[1]-1) {
      nAx[k,j] = (-n[k]/24*nDx[k-1,j-1]+n[k]/2*nDx[k,j-1]+n[k]/24*nDx[k+1,j-1])/nDx[k,j-1]
    }
    
    for(k in 1:dime[1]-1) {
      nQx[k,j]= n[k]*NMx[k,1]/(1+(n[k]-nAx[k,j])*NMx[k,1])
      nQx[dime[1],j]=1
    }
    
    for(k in 1:dime[1]) {
      nPx[k,j] <- 1-nQx[k,j]
    } 
    
    Lx[,j]<-rep(Rdx,dime[1])
    for(k in 2:dime[1]) {
      Lx[k,j] = nPx[k-1,j]*Lx[k-1,j]
    }
    
    for(k in 1:dime[1]) {
      nDx[k,j] <- nQx[k,j]*Lx[k,j]
    }
    
    
    for(i in 1:dime[1]) {
      if(i<dime[1]) {
        NLx[i,j] = n[i]*Lx[i+1,j]+nAx[i,j]*nDx[i,j]
      }
      if(i==dime[1]) {
        if(length.const.mort==0) { NLx[i,j]=Lx[i,j]/NMx[i,1]     }
        if(length.const.mort>0) {NLx[i,j]=Lx[i,j]*(1-exp(-length.const.mort*NMx[i,1]))/NMx[i,1]}
      }
    }
    
    Txx[,j]<-rev(cumsum(rev(NLx[,j])))
    
    for(k in 1:dime[1]) {
      Ex[k,j]=Txx[k,j]/Lx[k,j]
    }
  }
  
  
  lt5<-data.frame(x=x, nax=nAx[,5],
                  nMx = NMx[,1],
                  npx = nPx[,5],
                  lx = Lx[,5],
                  ndx = nDx[,5],
                  nLx = NLx[,5],
                  Tx = Txx[,5],
                  ex = Ex[,5] )            
  return(lt5$ex[1])
  
}

Mx.Data <- aggregate.data.frame(Mex.Data$Spcf.DRate, by=list(Mex.Data$Year,Mex.Data$Sex,Mex.Data$State,
                                                           Mex.Data$Age.Group),FUN=sum)
Mx.Data <- rename.vars(Mx.Data,c("Group.1","Group.2","Group.3","Group.4","x"),
                          c("Year","Sex","State","Age.Group","Mx"),info=TRUE)
Mx.Data <- Mx.Data[with(Mx.Data,order(Year,Sex,Age.Group,State)),]


#e0.vG <-  vanguard e0

Mx.Data.Males <- subset(Mx.Data, Sex==1)
Mx.Data.Females <- subset(Mx.Data, Sex==2)

Mx.Data.Males<-as.data.table(Mx.Data.Males)
Mx.Data.Females<-as.data.table(Mx.Data.Females)

e0.Males  <- Mx.Data.Males[,lifetab(Mx,1,95,"male",10), by = list(Year, Sex, State)]
e0.Females  <- Mx.Data.Females[,lifetab(Mx,1,100,"female",10), by = list(Year, Sex, State)]

e0.Males.VG      <- e0.Males[,max(V1), by = list(Year)]
e0.Females.VG      <- e0.Females[,max(V1), by = list(Year)]

e0.Males.VG$Sex <- 1
e0.Females.VG$Sex <- 2

#e0.BP <- Best Practices
Mx.BP.Males <- Mx.Data.Males[,min(Mx), by = list(Year, Sex, Age.Group)]
Mx.BP.Females <- Mx.Data.Females[,min(Mx), by = list(Year, Sex, Age.Group)]

e0.BP.Males  <- Mx.BP.Males[,lifetab(V1,1,ifelse(length(V1)==22,100,95),"male",10), by = list(Year, Sex)]
e0.BP.Females  <- Mx.BP.Females[,lifetab(V1,1,100,"female",10), by = list(Year, Sex)]


e0.Males.VG			<- as.data.frame(e0.Males.VG)
e0.Females.VG		<- as.data.frame(e0.Females.VG)
e0.Males			<- as.data.frame(e0.Males)
e0.Females			<- as.data.frame(e0.Females)
e0.BP.Males			<- as.data.frame(e0.BP.Males)
e0.BP.Females		<- as.data.frame(e0.BP.Females)

e0.Males.VG$e0.class 	<- 1
e0.Females.VG$e0.class 	<- 1
e0.Males$e0.class 		<- 2
e0.Females$e0.class 	<- 2
e0.BP.Males$e0.class 	<- 3
e0.BP.Females$e0.class 	<- 3
# 1 VG. 2 e0. 3 BP

e0.Graph <- rbind(e0.Males,e0.Females)
VG.Graph <- rbind(e0.Males.VG,e0.Females.VG)
BP.Graph <- rbind(e0.BP.Males,e0.BP.Females)

keep(e0.Graph,VG.Graph,BP.Graph,Mx.Data.Males,Mx.Data.Females,sure=T)


### Make figure

makeTransparent<-function(someColor, alpha=100) {
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)}) }


my.settings <- list(
  strip.background=list(col="grey"),
  strip.border=list(col="black")
)
e0.Graph$Sex<-factor(e0.Graph$Sex,levels=c(1,2),labels=c("Males", "Females"))
BP.Graph$Sex<-factor(BP.Graph$Sex,levels=c(1,2),labels=c("Males", "Females"))
VG.Graph$Sex<-factor(VG.Graph$Sex,levels=c(1,2),labels=c("Males", "Females"))

Fig1<-xyplot(V1~Year|Sex,data=e0.Graph,groups=State,type="l",lwd=2,between=list(x=2),
            xlim=c(1990,2010),main="Life expectancy experiment",ylim=c(60,90),ylab="Life Expectancy",
            col=makeTransparent("black",alpha=65),par.settings=my.settings,xlab="Period", 
            key=list(x=.05,y=.9,background="white",text=list(c('Life Expectancy','VG Life Expectancy','BP Life Expectancy')),cex=.8,
                                                            points=list(pch=19,col=c(makeTransparent("black",alpha=65),makeTransparent("red",alpha=100),
                                                                                     makeTransparent("blue",alpha=100)))),
            scales=list(alternating=1,x=list(cex=.75,at=c(seq(1990,2010,2))),
                        y=list(cex=.75,at=c(seq(60,90,5)),alternating=1)),                        
            panel = function(x, y, ...){                        
              panel.abline(v=c(seq(1990,2010,1)),col='dark grey',lty=3)
              panel.abline(v=c(seq(1990,2010,5)),col='dark grey',lty=1)
              panel.abline(h=c(seq(0,90,5)),col='dark grey',lty=3)
              panel.xyplot(x, y,lty=1,...)
              
            })
Fig1

Fig2<-xyplot(V1~Year|Sex,data=VG.Graph,type="l",lwd=2,between=list(x=2),
             xlim=c(1990,2010),main="Life expectancy experiment",ylim=c(60,90),ylab="Life Expectancy",
             col=makeTransparent("red",alpha=100),par.settings=my.settings,xlab="Period",
             key=list(x=.05,y=.9,background="white",text=list(c('Life Expectancy','VG Life Expectancy','BP Life Expectancy')),cex=.8,
                      points=list(pch=19,col=c(makeTransparent("black",alpha=65),makeTransparent("red",alpha=100),
                                               makeTransparent("blue",alpha=100)))),
             scales=list(alternating=1,x=list(cex=.75,at=c(seq(1990,2010,2))),
                         y=list(cex=.75,at=c(seq(60,90,5)),alternating=1)),                        
             panel = function(x, y, ...){                        
               panel.abline(v=c(seq(1990,2010,1)),col='dark grey',lty=3)
               panel.abline(v=c(seq(1990,2010,5)),col='dark grey',lty=1)
               panel.abline(h=c(seq(0,90,5)),col='dark grey',lty=3)
               panel.xyplot(x, y,lty=1,...)
               
             })
Fig2

Fig3<-xyplot(V1~Year|Sex,data=BP.Graph,type="l",lwd=2,between=list(x=2),
             xlim=c(1990,2010),main="Life expectancy experiment",ylim=c(60,90),ylab="Life Expectancy",
             col=makeTransparent("blue",alpha=100),par.settings=my.settings,xlab="Period",
             key=list(x=.05,y=.9,background="white",text=list(c('Life Expectancy','VG Life Expectancy','BP Life Expectancy')),cex=.8,
                      points=list(pch=19,col=c(makeTransparent("black",alpha=65),makeTransparent("red",alpha=100),
                                               makeTransparent("blue",alpha=100)))),
             scales=list(alternating=1,x=list(cex=.75,at=c(seq(1990,2010,2))),
                         y=list(cex=.75,at=c(seq(60,90,5)),alternating=1)),                        
             panel = function(x, y, ...){                        
               panel.abline(v=c(seq(1990,2010,1)),col='dark grey',lty=3)
               panel.abline(v=c(seq(1990,2010,5)),col='dark grey',lty=1)
               panel.abline(h=c(seq(0,90,5)),col='dark grey',lty=3)
               panel.xyplot(x, y,lty=1,...)
               
             })
Fig3


Fig4<-Fig1+Fig2+Fig3



pdf(file="Life Expectancy Experiment.pdf",width=12,height=8,pointsize=4)
print(Fig4)
dev.off()
Fig4

which(e0.Graph$V1==min(e0.Graph$V1))
e0.Graph[648,] #Chihuahua


################### Analysis by age
lifetab.1 <- function(nMx, Rdx, max.age, sex, length.const.mort)   {
  
  
  x = c(0,1,seq(5,max.age,by=5))
  row<-which(x==max.age)
  
  NMx <- matrix(0,row,1)
  kk<-as.matrix(nMx)
  
  NMx[,1] <- kk[1:row,]
  dime<-dim(NMx)
  
  n <-c(1,4, rep(5,row-3), 0)
  nax<-rep(0,row)
  
  if(nMx[1] >=0.107 & sex=="male")   {nax[1]<-0.33 ; nax[2]<-1.352}
  if(nMx[1] < 0.107 & sex=="male")   {nax[1]<-0.045+2.684*nMx[1]; nax[2]<-1.651-2.816*nMx[1]}
  if(nMx[1] >=0.107 & sex=="female") {nax[1]<-0.35; nax[1]<-1.361}
  if(nMx[1] < 0.107 & sex=="female") {nax[1]<-0.053+2.8*nMx[1]; nax[2]<-1.522-1.518*nMx[1]}
  
  nAx <- matrix(0,row,5)
  nAx[,1] <-nax
  
  
  nQx <-matrix(0,dime[1],5)
  for(i in 1:dime[1]-1) {
    nQx[i,1]= n[i]*NMx[i,1]/(1+(n[i]-nAx[i,1])*NMx[i,1])
    nQx[dime[1],1]=1
  }
  
  nPx <-matrix(0,dime[1],5)
  nPx[,1] <- 1-nQx[,1]
  
  Lx <-matrix(Rdx,dime[1],5)
  for(i in 2:dime[1]) {
    Lx[i,1] = nPx[i-1,1]*Lx[i-1,1]
  }
  
  nDx <-matrix(0,dime[1],5)
  nDx[,1]<- nQx[,1]*Lx[,1]
  
  NLx <-matrix(0,dime[1],5)
  for(i in 1:dime[1]-1) {
    NLx[i,1] = n[i]*Lx[i+1,1]+nAx[i,1]*nDx[i,1]
    NLx[dime[1],1]=Lx[dime[1],1]/NMx[dime[1],1]    
  }
  
  Txx <- matrix(0,dime[1],5)
  Txx[,1] <- rev(cumsum(rev(NLx[,1])))
  
  Ex <-matrix(0,dime[1],5)
  Ex[,1] <- Txx[,1]/Lx[,1]
  
  
  for(j in 2:5) {
    nAx[1,j]=nax[1]
    nAx[2,j]=nax[2]
    #nAx[3,j]=a5
  }
  
  for(j in 2:5) {
    
    for(k in 3:dime[1]-1) {
      nAx[k,j] = (-n[k]/24*nDx[k-1,j-1]+n[k]/2*nDx[k,j-1]+n[k]/24*nDx[k+1,j-1])/nDx[k,j-1]
    }
    
    for(k in 1:dime[1]-1) {
      nQx[k,j]= n[k]*NMx[k,1]/(1+(n[k]-nAx[k,j])*NMx[k,1])
      nQx[dime[1],j]=1
    }
    
    for(k in 1:dime[1]) {
      nPx[k,j] <- 1-nQx[k,j]
    } 
    
    Lx[,j]<-rep(Rdx,dime[1])
    for(k in 2:dime[1]) {
      Lx[k,j] = nPx[k-1,j]*Lx[k-1,j]
    }
    
    for(k in 1:dime[1]) {
      nDx[k,j] <- nQx[k,j]*Lx[k,j]
    }
    
    
    for(i in 1:dime[1]) {
      if(i<dime[1]) {
        NLx[i,j] = n[i]*Lx[i+1,j]+nAx[i,j]*nDx[i,j]
      }
      if(i==dime[1]) {
        if(length.const.mort==0) { NLx[i,j]=Lx[i,j]/NMx[i,1]     }
        if(length.const.mort>0) {NLx[i,j]=Lx[i,j]*(1-exp(-length.const.mort*NMx[i,1]))/NMx[i,1]}
      }
    }
    
    Txx[,j]<-rev(cumsum(rev(NLx[,j])))
    
    for(k in 1:dime[1]) {
      Ex[k,j]=Txx[k,j]/Lx[k,j]
    }
  }
  
  
  lt5<-data.frame(x=x, nax=nAx[,5],
                  nMx = NMx[,1],
                  npx = nPx[,5],
                  lx = Lx[,5],
                  ndx = nDx[,5],
                  nLx = NLx[,5],
                  Tx = Txx[,5],
                  ex = Ex[,5] )            
  return(lt5)
  
}

LF.Males  <- Mx.Data.Males[,lifetab.1(Mx,1,95,"male",10), by = list(Year, Sex, State)]
LF.Females  <- Mx.Data.Females[,lifetab.1(Mx,1,100,"female",10), by = list(Year, Sex, State)]


e0.Males.VG      <- e0.Males[,max(V1), by = list(Year)]
e0.Females.VG      <- e0.Females[,max(V1), by = list(Year)]

e0.Males.VG$Sex <- 1
e0.Females.VG$Sex <- 2

#e0.BP <- Best Practices
Mx.BP.Males <- Mx.Data.Males[,min(Mx), by = list(Year, Sex, Age.Group)]
Mx.BP.Females <- Mx.Data.Females[,min(Mx), by = list(Year, Sex, Age.Group)]

e0.BP.Males  <- Mx.BP.Males[,lifetab.1(V1,1,ifelse(length(V1)==22,100,95),"male",10), by = list(Year, Sex)]
e0.BP.Females  <- Mx.BP.Females[,lifetab.1(V1,1,100,"female",10), by = list(Year, Sex)]



getTempe0 <- function(LT,lowera = 0, uppera = 10){
	stopifnot(all(c(lowera,uppera) %in% LT$x))

	sum(LT$nLx[LT$x >= lowera & LT$x <= uppera] ) / LT$lx[LT$x == lowera]
}
head(e0.BP.Males)
etemp0_14BPm  <- e0.BP.Males[,getTempe0(.SD),by=list(Year)]
etemp15_50BPm <- e0.BP.Males[,getTempe0(.SD,15,45),by=list(Year)]
etemp50_74BPm <- e0.BP.Males[,getTempe0(.SD,50,70),by=list(Year)]
etemp0_14BPf  <- e0.BP.Females[,getTempe0(.SD),by=list(Year)]
etemp15_50BPf <- e0.BP.Females[,getTempe0(.SD,15,45),by=list(Year)]
etemp50_74BPf <- e0.BP.Females[,getTempe0(.SD,50,70),by=list(Year)]
etemp75_100BPf <- e0.BP.Females[,getTempe0(.SD,75,100),by=list(Year)]
etemp75_100BPm <- e0.BP.Males[,getTempe0(.SD,75,100),by=list(Year)]

etemp75_99BPfstates <- LF.Females[,getTempe0(.SD,75,95),by=list(Year,State)]
etemp75_99BPmstates <- LF.Males[,getTempe0(.SD,75,95),by=list(Year,State)]

etemp0_74BPf <- e0.BP.Females[,getTempe0(.SD,0,70),by=list(Year)]
etemp0_74BPm <- e0.BP.Males[,getTempe0(.SD,0,70),by=list(Year)]
etemp0_74BPfstates <- LF.Females[,getTempe0(.SD,0,70),by=list(Year,State)]
etemp0_74BPmstates <- LF.Males[,getTempe0(.SD,0,70),by=list(Year,State)]



fmat <- acast(etemp0_74BPfstates, Year~State,value.var = "V1")
mmat <- acast(etemp0_74BPmstates, Year~State,value.var = "V1")
par(mfrow=c(1,2))
matplot(1990:2010,fmat,
		type = 'l', lty =1,col="#00000050",ylim=c(62,73))
lines(1990:2010, etemp0_74BPf$V1,col="red")
lines(1990:2010, fmat[,"7"],col = "blue")
lines(1990:2010, fmat[,"8"],col = "magenta")
lines(1990:2010, fmat[,"9"],col = "green")

matplot(1990:2010,mmat,
		type = 'l', lty =1,col="#00000050",ylim=c(62,73))
lines(1990:2010, etemp0_74BPm$V1,col="red")
lines(1990:2010, mmat[,"7"],col = "blue")
lines(1990:2010, mmat[,"8"],col = "magenta")
lines(1990:2010, mmat[,"9"],col = "green")

df <- gdata::read.xls("/home/tim/Data/MEX/SOMEDE/Poblacion y eventos estatales 1990_2011.xls", 
		sheet = 2, header=TRUE,encoding="latin1")
NAs <- is.na(as.matrix(df))
ind <- rowSums(!NAs) == 1
table(diff(ind))
sum(df[,1] == 0 & !is.na(df[,1]))
sum(df[,1] == 109 & !is.na(df[,1]))

sum(df[,1] == "Hombres" & !is.na(df[,1]))
sum(df[,1] == "Mujeres" & !is.na(df[,1]))

states <- df[,1][!is.na(df[,1]) & !df[,1] %in% 0:109 & df[,1]!="Hombres" & df[,1]!="Mujeres"][-1]

'%==%' <- function(x,y){
	x == y & !is.na(x) & !is.na(y)
}
'%QQ%' <- function(x,y){
	x %in% y & !is.na(x)
}
ind2 <- cumsum(df[,1] %QQ% states)
library(reshape2)
chunks <- split(df,ind2)
chunk <- chunks[[2]]
splitsexes <- function(chunk){
	MI <- which(chunk[,1] %==% "Hombres")
	FI <-  which(chunk[,1] %==% "Mujeres")
	

	maxA <- max(suppressWarnings(as.integer(chunk[,1])), na.rm = TRUE)

	Males <- as.matrix(chunk[MI + 0:maxA + 1, -1])
	dimnames(Males) <- list(Age = 0:maxA,Year = 1990:2010)

	Males <- melt(Males)
	Males$Sex <- "m"
	
	Females <- as.matrix(chunk[FI + 0:maxA + 1, -1])
	dimnames(Females) <- list(Age = 0:maxA,Year = 1990:2010)
	
	Females <- melt(Females)
	Females$Sex <- "f"
	
	Dat <- rbind(Females, Males)
	Dat$State <- chunk[1,1]
	colnames(Dat)[colnames(Dat) == "value"] <- "Exposure"
    Dat
}

Exposures <- do.call(rbind, lapply(chunks[-1], splitsexes))

save(Exposures, file = "/home/tim/git/DecompMex/DecompMex/Data/StateExpSingle.Rdata")

library(reshape2)
matplot(1990:2010,acast(etemp75_99BPfstates, Year~State,value.var = "V1"),
		type = 'l', lty =1,col="#00000050")

max(e0.BP.Males$x)

plot(1990:2010, etemp0_14BPm$V1, type= 'l', ylim = c(14,15))
lines(1990:2010, etemp0_14BPf$V1, col = "red")

plot(1990:2010, etemp15_50BPm$V1, type= 'l',ylim=c(34,35))
lines(1990:2010, etemp15_50BPf$V1, col = "red")

plot(1990:2010, etemp50_74BPm$V1, type= 'l',ylim=c(22,24))
lines(1990:2010, etemp50_74BPf$V1, col = "red")

plot(1990:2010, etemp75_100BPm$V1, type= 'l',ylim=c(15,21))
lines(1990:2010, etemp75_100BPf$V1, col = "red")


lm(V1~Year, data=etemp50_74BPm)$coef[2] * 10

e0.Males.VG				<-as.data.frame(e0.Males.VG)
e0.Females.VG			<-as.data.frame(e0.Females.VG)
e0.Males				<-as.data.frame(e0.Males)
e0.Females				<-as.data.frame(e0.Females)
e0.BP.Males				<-as.data.frame(e0.BP.Males)
e0.BP.Females			<-as.data.frame(e0.BP.Females)

e0.Males.VG$e0.class <- 1
e0.Females.VG$e0.class <- 1
e0.Males$e0.class <- 2
e0.Females$e0.class <- 2
e0.BP.Males$e0.class <- 3
e0.BP.Females$e0.class <- 3
# 1 VG. 2 e0. 3 BP

e0.Graph <- rbind(e0.Males,e0.Females)
VG.Graph <- rbind(e0.Males.VG,e0.Females.VG)
BP.Graph <- rbind(e0.BP.Males,e0.BP.Females)








#### 1. Infectious and respiratory diseases, 2. Cancers, 3. Circulatory, 4. Birth, 5. Diabetes, 6. Other Medical Care AM
#### 7. IHD, 8. HIV, 9. Suicide, 10. Lung Cancer, 11. Cirrhosis, 12. Homicide, 13. Road traffic accidents, 
#### 14. other heart diseases, 15. Ill-defined causes, 16. All other Non-AM
#### Note: these data do not contain Not Specified categories
