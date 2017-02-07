############# Decomposition analyses for Mexican mortality 1990-2010
############# Results provided by Tim
setwd("/Users/josemanuelaf/Desktop/JM&TIM_beta")
library(latticeExtra)
dir()
###### for ages 0 - 14 ##############################################################
#####################################################################################
### For females
DecomF0_14 <- local(get(load("Data/ContribFemales0_14.Rdata")))
DF0_14 <- NULL
require(reshape2)
for( i in 1:21){
        D        <- do.call(rbind.data.frame,DecomF0_14[[i]])
        D$Age    <- seq(0,75,1)
        D$State  <- rep(1:32, each=76)
        D        <- subset(D, Age< 15)
        D1       <- aggregate(x = D, by = list(D$State), FUN = sum)
        D1       <- D1[1:11]
        D2       <- melt(as.data.frame(D1), id="Group.1")
        D3       <- as.data.frame(cbind(Year= i+1989,State=D2$Group.1, AMCategory = D2$variable, Contribution = D2$value))  
        DF0_14   <- as.data.frame(rbind(DF0_14, D3))
}
## Data frame with results for females

## For males
DecomM0_14 <- local(get(load("Data/ContribMales0_14.Rdata")))
DM0_14 <- NULL
for( i in 1:21){
  D        <- do.call(rbind.data.frame,DecomM0_14[[i]])
  D$Age    <- seq(0,75,1)
  D$State  <- rep(1:32, each=76)
  D        <- subset(D, Age< 15)
  D1       <- aggregate(x = D, by = list(D$State), FUN = sum)
  D1       <- D1[1:11]
  D2       <- melt(as.data.frame(D1), id="Group.1")
  D3       <- as.data.frame(cbind(Year= i+1989,State=D2$Group.1, AMCategory = D2$variable, Contribution = D2$value))  
  DM0_14   <- as.data.frame(rbind(DM0_14, D3))
}
## Data frame with results for males
#####################################################################################

###### for ages 15 - 39 ##############################################################
#####################################################################################
### For females
DecomF15_39 <- local(get(load("Data/ContribFemales15_39.Rdata")))
DF15_39 <- NULL
for( i in 1:21){
  D        <- do.call(rbind.data.frame,DecomF15_39[[i]])
  D$Age    <- seq(0,75,1)
  D$State  <- rep(1:32, each=76)
  D        <- subset(D, Age > 14 & Age < 40)
  D1       <- aggregate(x = D, by = list(D$State), FUN = sum)
  D1       <- D1[1:11]
  D2       <- melt(as.data.frame(D1), id="Group.1")
  D3       <- as.data.frame(cbind(Year= i+1989,State=D2$Group.1, AMCategory = D2$variable, Contribution = D2$value))  
  DF15_39   <- as.data.frame(rbind(DF15_39, D3))
}
## Data frame with results for females

## For males
DecomM15_39 <- local(get(load("Data/ContribMales15_39.Rdata")))
DM15_39 <- NULL
for( i in 1:21){
  D        <- do.call(rbind.data.frame,DecomM15_39[[i]])
  D$Age    <- seq(0,75,1)
  D$State  <- rep(1:32, each=76)
  D        <- subset(D, Age > 14 & Age < 40)
  D1       <- aggregate(x = D, by = list(D$State), FUN = sum)
  D1       <- D1[1:11]
  D2       <- melt(as.data.frame(D1), id="Group.1")
  D3       <- as.data.frame(cbind(Year= i+1989,State=D2$Group.1, AMCategory = D2$variable, Contribution = D2$value))  
  DM15_39   <- as.data.frame(rbind(DM15_39, D3))
}
## Data frame with results for males
#####################################################################################

###### for ages 40 - 74 ##############################################################
#####################################################################################
### For females
DecomF40_74 <- local(get(load("Data/ContribFemales40_74.Rdata")))
DF40_74 <- NULL
for( i in 1:21){
  D        <- do.call(rbind.data.frame,DecomF40_74[[i]])
  D$Age    <- seq(0,75,1)
  D$State  <- rep(1:32, each=76)
  D        <- subset(D, Age > 39 & Age < 74)
  D1       <- aggregate(x = D, by = list(D$State), FUN = sum)
  D1       <- D1[1:11]
  D2       <- melt(as.data.frame(D1), id="Group.1")
  D3       <- as.data.frame(cbind(Year= i+1989,State=D2$Group.1, AMCategory = D2$variable, Contribution = D2$value))  
  DF40_74   <- as.data.frame(rbind(DF40_74, D3))
}
## Data frame with results for females

## For males
DecomM40_74 <- local(get(load("Data/ContribMales40_74.Rdata")))
DM40_74 <- NULL
for( i in 1:21){
  D        <- do.call(rbind.data.frame,DecomM40_74[[i]])
  D$Age    <- seq(0,75,1)
  D$State  <- rep(1:32, each=76)
  D        <- subset(D, Age > 39 & Age < 74)
  D1       <- aggregate(x = D, by = list(D$State), FUN = sum)
  D1       <- D1[1:11]
  D2       <- melt(as.data.frame(D1), id="Group.1")
  D3       <- as.data.frame(cbind(Year= i+1989,State=D2$Group.1, AMCategory = D2$variable, Contribution = D2$value))  
  DM40_74   <- as.data.frame(rbind(DM40_74, D3))
}
## Data frame with results for males
gdata::keep(DF0_14,DM0_14,DF15_39,DM15_39,DF40_74,DM40_74,sure=T)
#####################################################################################
#####################################################################################

#####################################################################################
#Start Graph analysis################################################################
#####################################################################################
#1.Causes amenable to medical service
#2.Diabetes
#3.Ischemic heart diseases
#4.HIV/AIDS
#5.Lung cancer
#6.Cirrhosis
#7.Homicide
#8.Road traffic accidents
#9.Suicide
#10.Other causes
#####################################################################################
####graphs parameters
my.settings <- list(  
  strip.background=list(col="grey"),
  strip.border=list(col="black"),
  auto.key = F
)
makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}
#####################################################################################

DM0_14$AMLabel <- factor(DM0_14$AMCategory,levels=c(1:10),labels=c("AMS","Diabetes","IHD","HIV","Lung cancer","Cirrhosis","Homicide","RTA","Suicide","RC"))
DF0_14$AMLabel <- factor(DF0_14$AMCategory,levels=c(1:10),labels=c("AMS","Diabetes","IHD","HIV","Lung cancer","Cirrhosis","Homicide","RTA","Suicide","RC"))
DF15_39$AMLabel <- factor(DF15_39$AMCategory,levels=c(1:10),labels=c("AMS","Diabetes","IHD","HIV","Lung cancer","Cirrhosis","Homicide","RTA","Suicide","RC"))
DM15_39$AMLabel <- factor(DM15_39$AMCategory,levels=c(1:10),labels=c("AMS","Diabetes","IHD","HIV","Lung cancer","Cirrhosis","Homicide","RTA","Suicide","RC"))
DF40_74$AMLabel <- factor(DF40_74$AMCategory,levels=c(1:10),labels=c("AMS","Diabetes","IHD","HIV","Lung cancer","Cirrhosis","Homicide","RTA","Suicide","RC"))
DM40_74$AMLabel <- factor(DM40_74$AMCategory,levels=c(1:10),labels=c("AMS","Diabetes","IHD","HIV","Lung cancer","Cirrhosis","Homicide","RTA","Suicide","RC"))

################### from 0 to 14
#################### The only category that worths mentioning is the Amenable to medical service
# wchich is a succes story. And maybe the residual causes categoru
#females
DF0_14$Sex <- "Females"
DM0_14$Sex <- "Males"
data1 <-rbind(DF0_14,DM0_14)
F1 <- xyplot(Contribution~Year|Sex,data=data1[data1$AMCategory<2,],groups=State,
             col=makeTransparent("black", alpha=150),between=list(x=.7),
             par.settings=my.settings,type="l",ylim=c(0,.6),main="Amenable to medical service",
             scales=list(alternating=1,x=list(cex=.75,at=c(seq(1990,2010,5))),
                         y=list(cex=.75,at=c(seq(0,.6,.1)),alternating=3)),
             panel = function(x, y, ...){                        
             panel.abline(v=c(seq(1990,2010,2.5)),col='dark grey',lty=3)
             panel.abline(h=c(seq(0,.6,.1)),col='dark grey',lty=3)
             panel.xyplot(x, y,lty=1,...)
             })
F1

pdf(file="AMS_0_14.pdf",width=12,height=5,pointsize=4)
print(F1)
dev.off()

##################################################################################

F1.1 <-  useOuterStrips(xyplot(Contribution~Year|AMLabel+Sex,data=data1,groups=State,
                             col=makeTransparent("black", alpha=150),ylim=c(0,1.6),
                             par.settings=my.settings,type="l",
                             scales=list(alternating=1,x=list(cex=.75,at=c(1995,2005)),
                                         y=list(cex=.75,at=c(seq(0,2,.25)),alternating=3)),
                             panel = function(x, y, ...){                        
                               panel.abline(v=c(seq(1990,2010,5)),col='dark grey',lty=3)
                               panel.abline(h=c(.2),col='red',lty=1)
                               panel.abline(h=c(.5),col='red',lty=1)
                               panel.abline(h=c(.1),col='blue',lty=1)
                               panel.abline(h=c(seq(0,1.5,.25)),col='dark grey',lty=3)
                               panel.xyplot(x, y,lty=1,...)
                             }),strip.left=T)
F1.1

pdf(file="AM_0_14.pdf",width=12,height=5,pointsize=4)
print(F1.1)
dev.off()




#######################################################################################################
################### from 15 to 39
#################### The only category that worths mentioning is Homicide
#females
DF15_39$Sex <- "Females"
DM15_39$Sex <- "Males"
data2 <-rbind(DF15_39,DM15_39)

F2 <-  useOuterStrips(xyplot(Contribution~Year|AMLabel+Sex,data=data2,groups=State,
       col=makeTransparent("black", alpha=150),ylim=c(0,1.6),
       par.settings=my.settings,type="l",
       scales=list(alternating=1,x=list(cex=.75,at=c(1995,2005)),
                   y=list(cex=.75,at=c(seq(0,2,.25)),alternating=3)),
       panel = function(x, y, ...){                        
         panel.abline(v=c(seq(1990,2010,5)),col='dark grey',lty=3)
         panel.abline(h=c(.2),col='red',lty=1)
         panel.abline(h=c(.5),col='red',lty=1)
         panel.abline(h=c(.1),col='blue',lty=1)
         panel.abline(h=c(seq(0,1.5,.25)),col='dark grey',lty=3)
         panel.xyplot(x, y,lty=1,...)
       }),strip.left=T)
F2

pdf(file="AM_15_39.pdf",width=12,height=5,pointsize=4)
print(F2)
dev.off()


#######################################################################################################
################### from 40 to 74
#################### 
DF40_74$Sex <- "Females"
DM40_74$Sex <- "Males"
data3 <-rbind(DF40_74,DM40_74)

F3 <-  useOuterStrips(xyplot(Contribution~Year|AMLabel+Sex,data=data3,groups=State,
                             col=makeTransparent("black", alpha=150),ylim=c(0,1.6),
                             par.settings=my.settings,type="l",
                             scales=list(alternating=1,x=list(cex=.75,at=c(1995,2005)),
                                         y=list(cex=.75,at=c(seq(0,2,.25)),alternating=3)),
                             panel = function(x, y, ...){                        
                               panel.abline(v=c(seq(1990,2010,5)),col='dark grey',lty=3)
                               panel.abline(h=c(.2),col='red',lty=1)
                               panel.abline(h=c(.5),col='red',lty=1)
                               panel.abline(h=c(.1),col='blue',lty=1)
                               panel.abline(h=c(seq(0,1.5,.25)),col='dark grey',lty=3)
                               panel.xyplot(x, y,lty=1,...)
                             }),strip.left=T)
F3

pdf(file="AM_40_74.pdf",width=12,height=5,pointsize=4)
print(F3)
dev.off()

require(gridExtra)
pdf(file="TLE_Decomp.pdf",width=12,height=15,pointsize=2)
grid.arrange(F1.1, F2,F3, nrow=3)
dev.off()


#################### Get into the Hex Mex
source("R/HexTest.R")
#data1 <-rbind(DF0_14,DM0_14)
#data2 <-rbind(DF15_39,DM15_39)
#data3 <-rbind(DF40_74,DM40_74)

HexMex_Cause <- function(Data1,Year,Category,breaks){
 data           <- as.data.frame(Data1)[Data1$Year == Year & Data1$AMCategory == Category,]
 rownames(data) <- data$State
 data           <- subset(data,select=c("State","Year","Contribution"))
 return(HexMex(data,value.name = "Contribution",version =3,breaks=breaks))
}

require(gridExtra)
pdf(file="HexMex_Homicide_15-39.pdf",width=6,height=4,pointsize=2)
for(i in 1990:2010){
  HexMex_Cause(Data1=DM15_39,Year=i,Category=7,breaks=NULL)  
}
dev.off()


pdf(file="HexMex_maps.pdf",width=12,height=8,pointsize=2)
par(mfrow=c(2,2))
par(mar = c(1,1,1,1))
HexMex_Cause(Data1=DM15_39,Year=1990,Category=7,breaks=NULL)  
text(10,8,"main")
HexMex_Cause(Data1=DM15_39,Year=1995,Category=7,breaks=NULL)  
HexMex_Cause(Data1=DM15_39,Year=2000,Category=7,breaks=NULL)  
HexMex_Cause(Data1=DM15_39,Year=2005,Category=7,breaks=NULL)  
dev.off()

pdf(file="HexMex_AMS_0-14.pdf",width=6,height=4,pointsize=2)
for(i in 1990:2010){
HexMex_Cause(Data1=DM0_14,Year=i,Category=1,breaks=c(.01,.02,.05,.06,.07,.08,.09,.1,.2,.25,.3,.35,.4,.5)) 
}
dev.off()

