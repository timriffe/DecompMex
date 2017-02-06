
library(RColorBrewer)
library(latticeExtra)
library(xtable)
library(reshape2)
library(data.table)

setwd("C:/Users/jmaburto/Documents/GitHub/DecompMex/DecompMex")

if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
  # if I'm on the laptop
  setwd("/home/tim/git/DecompMex/DecompMex")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/DecompMex/DecompMex"))
}


###### for ages 0 - 14 ##############################################################
#####################################################################################
### For females
DecomF0_14 <- local(get(load("Data/ContribFemales0_14.Rdata")))
DF0_14 <- NULL
for( i in 1:26){
  D        <- do.call(rbind.data.frame,DecomF0_14[[i]])
  D$Age    <- seq(0,109,1)
  D$State  <- rep(1:32, each=110)
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
for( i in 1:26){
  D        <- do.call(rbind.data.frame,DecomM0_14[[i]])
  D$Age    <- seq(0,109,1)
  D$State  <- rep(1:32, each=110)
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
for( i in 1:26){
  D        <- do.call(rbind.data.frame,DecomF15_39[[i]])
  D$Age    <- seq(0,109,1)
  D$State  <- rep(1:32, each=110)
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
for( i in 1:26){
  D        <- do.call(rbind.data.frame,DecomM15_39[[i]])
  D$Age    <- seq(0,109,1)
  D$State  <- rep(1:32, each=110)
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
for( i in 1:26){
  D        <- do.call(rbind.data.frame,DecomF40_74[[i]])
  D$Age    <- seq(0,109,1)
  D$State  <- rep(1:32, each=110)
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
for( i in 1:26){
  D        <- do.call(rbind.data.frame,DecomM40_74[[i]])
  D$Age    <- seq(0,109,1)
  D$State  <- rep(1:32, each=110)
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
#Groups used in the article:
# 1. Amenable to medical service: 1+2+3+4+6
# 2. Diabetes: 5
# 3. IHD:7
# 4. HIV:8
# 5. Lung cancer: 10
# 6. Cirrhosis: 11
# 7. Homicide: 12
# 8. Road traffic: 13 
# 9. Suicide: 9
# 10. All other causes: 14+15+16

DM0_14$AMLabel <- factor(DM0_14$AMCategory,levels=c(1:10),labels=c("AMS","Diabetes","IHD","HIV","Lung cancer","Cirrhosis","Homicide","RTA","Suicide","RC"))
DF0_14$AMLabel <- factor(DF0_14$AMCategory,levels=c(1:10),labels=c("AMS","Diabetes","IHD","HIV","Lung cancer","Cirrhosis","Homicide","RTA","Suicide","RC"))
DF15_39$AMLabel <- factor(DF15_39$AMCategory,levels=c(1:10),labels=c("AMS","Diabetes","IHD","HIV","Lung cancer","Cirrhosis","Homicide","RTA","Suicide","RC"))
DM15_39$AMLabel <- factor(DM15_39$AMCategory,levels=c(1:10),labels=c("AMS","Diabetes","IHD","HIV","Lung cancer","Cirrhosis","Homicide","RTA","Suicide","RC"))
DF40_74$AMLabel <- factor(DF40_74$AMCategory,levels=c(1:10),labels=c("AMS","Diabetes","IHD","HIV","Lung cancer","Cirrhosis","Homicide","RTA","Suicide","RC"))
DM40_74$AMLabel <- factor(DM40_74$AMCategory,levels=c(1:10),labels=c("AMS","Diabetes","IHD","HIV","Lung cancer","Cirrhosis","Homicide","RTA","Suicide","RC"))

DM0_14$Ages  <- 1
DF0_14$Ages  <- 1
DF15_39$Ages <- 2
DM15_39$Ages <- 2
DF40_74$Ages <- 3
DM40_74$Ages <- 3

DM0_14$Sex  <- 2
DF0_14$Sex  <- 1
DF15_39$Sex <- 1
DM15_39$Sex <- 2
DF40_74$Sex <- 1
DM40_74$Sex <- 2

Data <- rbind(DM0_14,DF0_14,DM15_39,DF15_39,DM40_74,DF40_74)
gdata::keep(Data,my.settings,makeTransparent, sure=T)



###########################################heat map exercise
Data$Statenom<- "a"
attach(Data)
Data$Statenom[Data$State==1] <- "Aguascalientes"
Data$Statenom[Data$State==2] <- "Baja California"
Data$Statenom[Data$State==3] <- "Baja California Sur"
Data$Statenom[Data$State==4] <- "Campeche"
Data$Statenom[Data$State==5] <- "Coahuila"
Data$Statenom[Data$State==6] <- "Colima"
Data$Statenom[Data$State==7] <- "Chiapas"
Data$Statenom[Data$State==8] <- "Chihuahua"
Data$Statenom[Data$State==9] <- "Mexico City"
Data$Statenom[Data$State==10] <- "Durango"
Data$Statenom[Data$State==11] <- "Guanajuato"
Data$Statenom[Data$State==12] <- "Guerrero"
Data$Statenom[Data$State==13] <- "Hidalgo"
Data$Statenom[Data$State==14] <- "Jalisco"
Data$Statenom[Data$State==15] <- "Mexico State"
Data$Statenom[Data$State==16] <- "Michoacan"
Data$Statenom[Data$State==17] <- "Morelos"
Data$Statenom[Data$State==18] <- "Nayarit"
Data$Statenom[Data$State==19] <- "Nuevo Leon"
Data$Statenom[Data$State==20] <- "Oaxaca"
Data$Statenom[Data$State==21] <- "Puebla"
Data$Statenom[Data$State==22] <- "Queretaro"
Data$Statenom[Data$State==23] <- "Quintana Roo"
Data$Statenom[Data$State==24] <- "San Luis Potosi"
Data$Statenom[Data$State==25] <- "Sinaloa"
Data$Statenom[Data$State==26] <- "Sonora"
Data$Statenom[Data$State==27] <- "Tabasco"
Data$Statenom[Data$State==28] <- "Tamaulipas"
Data$Statenom[Data$State==29] <- "Tlaxcala"
Data$Statenom[Data$State==30] <- "Veracruz"
Data$Statenom[Data$State==31] <- "Yucatan"
Data$Statenom[Data$State==32] <- "Zacatecas"
detach(Data)

Data$region<-0
attach(Data)
Data$region[Data$State==1] <- 2
Data$region[Data$State==2] <- 3
Data$region[Data$State==3] <- 3
Data$region[Data$State==4] <- 1
Data$region[Data$State==5] <- 3
Data$region[Data$State==6] <- 2
Data$region[Data$State==7] <- 1
Data$region[Data$State==8] <- 3
Data$region[Data$State==9] <- 2
Data$region[Data$State==10] <- 3
Data$region[Data$State==11] <- 2
Data$region[Data$State==12] <- 1
Data$region[Data$State==13] <- 2
Data$region[Data$State==14] <- 2
Data$region[Data$State==15] <- 2
Data$region[Data$State==16] <- 2
Data$region[Data$State==17] <- 1
Data$region[Data$State==18] <- 2
Data$region[Data$State==19] <- 3
Data$region[Data$State==20] <- 1
Data$region[Data$State==21] <- 1
Data$region[Data$State==22] <- 2
Data$region[Data$State==23] <- 1
Data$region[Data$State==24] <- 3
Data$region[Data$State==25] <- 3
Data$region[Data$State==26] <- 3
Data$region[Data$State==27] <- 1
Data$region[Data$State==28] <- 3
Data$region[Data$State==29] <- 2
Data$region[Data$State==30] <- 1
Data$region[Data$State==31] <- 1
Data$region[Data$State==32] <- 3
detach(Data)
Data$region<-factor(Data$region,levels=c(1,2,3),labels=c("South", "Central", "North"))
Data$Ages<-factor(Data$Ages,levels=c(1,2,3),labels=c("0-14", "15-39", "40-74"))
Data$Sex<-factor(Data$Sex,levels=c(1,2),labels=c("Females","Males"))


#### Distance data
Data <- data.table(Data)
Dist.data <- Data[,sum(Contribution), by = list(Year,State,Ages,Sex)]
setnames(Dist.data,"V1","Distance")

ref.order <- subset(Dist.data, Year==2015 & Sex == "Males" & Ages == "40-74")
ref.order <- data.table(ref.order)
setnames(ref.order, "Distance", "Order")
ref.order <- ref.order[,c(2,5),with=F]
head(ref.order)


Dist.data$Statenom<- "a"
attach(Dist.data)
Dist.data$Statenom[Dist.data$State==1] <- "Aguascalientes"
Dist.data$Statenom[Dist.data$State==2] <- "Baja California"
Dist.data$Statenom[Dist.data$State==3] <- "Baja California Sur"
Dist.data$Statenom[Dist.data$State==4] <- "Campeche"
Dist.data$Statenom[Dist.data$State==5] <- "Coahuila"
Dist.data$Statenom[Dist.data$State==6] <- "Colima"
Dist.data$Statenom[Dist.data$State==7] <- "Chiapas"
Dist.data$Statenom[Dist.data$State==8] <- "Chihuahua"
Dist.data$Statenom[Dist.data$State==9] <- "Mexico City"
Dist.data$Statenom[Dist.data$State==10] <- "Durango"
Dist.data$Statenom[Dist.data$State==11] <- "Guanajuato"
Dist.data$Statenom[Dist.data$State==12] <- "Guerrero"
Dist.data$Statenom[Dist.data$State==13] <- "Hidalgo"
Dist.data$Statenom[Dist.data$State==14] <- "Jalisco"
Dist.data$Statenom[Dist.data$State==15] <- "Mexico State"
Dist.data$Statenom[Dist.data$State==16] <- "Michoacan"
Dist.data$Statenom[Dist.data$State==17] <- "Morelos"
Dist.data$Statenom[Dist.data$State==18] <- "Nayarit"
Dist.data$Statenom[Dist.data$State==19] <- "Nuevo Leon"
Dist.data$Statenom[Dist.data$State==20] <- "Oaxaca"
Dist.data$Statenom[Dist.data$State==21] <- "Puebla"
Dist.data$Statenom[Dist.data$State==22] <- "Queretaro"
Dist.data$Statenom[Dist.data$State==23] <- "Quintana Roo"
Dist.data$Statenom[Dist.data$State==24] <- "San Luis Potosi"
Dist.data$Statenom[Dist.data$State==25] <- "Sinaloa"
Dist.data$Statenom[Dist.data$State==26] <- "Sonora"
Dist.data$Statenom[Dist.data$State==27] <- "Tabasco"
Dist.data$Statenom[Dist.data$State==28] <- "Tamaulipas"
Dist.data$Statenom[Dist.data$State==29] <- "Tlaxcala"
Dist.data$Statenom[Dist.data$State==30] <- "Veracruz"
Dist.data$Statenom[Dist.data$State==31] <- "Yucatan"
Dist.data$Statenom[Dist.data$State==32] <- "Zacatecas"
detach(Dist.data)

Dist.data$region<-0
attach(Dist.data)
Dist.data$region[Dist.data$State==1] <- 2
Dist.data$region[Dist.data$State==2] <- 3
Dist.data$region[Dist.data$State==3] <- 3
Dist.data$region[Dist.data$State==4] <- 1
Dist.data$region[Dist.data$State==5] <- 3
Dist.data$region[Dist.data$State==6] <- 2
Dist.data$region[Dist.data$State==7] <- 1
Dist.data$region[Dist.data$State==8] <- 3
Dist.data$region[Dist.data$State==9] <- 2
Dist.data$region[Dist.data$State==10] <- 3
Dist.data$region[Dist.data$State==11] <- 2
Dist.data$region[Dist.data$State==12] <- 1
Dist.data$region[Dist.data$State==13] <- 2
Dist.data$region[Dist.data$State==14] <- 2
Dist.data$region[Dist.data$State==15] <- 2
Dist.data$region[Dist.data$State==16] <- 2
Dist.data$region[Dist.data$State==17] <- 1
Dist.data$region[Dist.data$State==18] <- 2
Dist.data$region[Dist.data$State==19] <- 3
Dist.data$region[Dist.data$State==20] <- 1
Dist.data$region[Dist.data$State==21] <- 1
Dist.data$region[Dist.data$State==22] <- 2
Dist.data$region[Dist.data$State==23] <- 1
Dist.data$region[Dist.data$State==24] <- 3
Dist.data$region[Dist.data$State==25] <- 3
Dist.data$region[Dist.data$State==26] <- 3
Dist.data$region[Dist.data$State==27] <- 1
Dist.data$region[Dist.data$State==28] <- 3
Dist.data$region[Dist.data$State==29] <- 2
Dist.data$region[Dist.data$State==30] <- 1
Dist.data$region[Dist.data$State==31] <- 1
Dist.data$region[Dist.data$State==32] <- 3
detach(Dist.data)
Dist.data <- Dist.data[Year==2005 | Year==2010 | Year==2015]
Dist.data <- as.data.frame(Dist.data)
Dist.data$region<-factor(Dist.data$region,levels=c(1,2,3),labels=c("South", "Central", "North"))


Dist.data <- merge(Dist.data,ref.order, by = c("State"),all.y = T)

Dist.data$Statenom<-with(Dist.data,reorder(reorder(Statenom,Order),as.numeric(region)))

my.pch<-c(17:19)  
my.fill<-c("green","blue","red")  
col.line<-list(c("black"))
txt.legend<-c("2005","2010","2015")
my.pch1<-c(17:19) 
my.fill1<-c("green","blue","red")  
my.settings <- list(  
  strip.background=list(col="darkgrey"),
  strip.border=list(col="black")
)

### one plot fpr the supplemental material
f1.y.data <- subset(Dist.data,Ages=="0-14")
f1.y <- useOuterStrips( dotplot(Statenom ~ Distance|Sex+region,aspect = c(0.9),ylab= list("State",cex=1),
              #layout=c(3,6),
              strip=T,
              xlab=list("Distance from benchmark survival",cex=1),main=FALSE,cex=1,
              data=f1.y.data, groups=Year,pch=my.pch, col=my.fill,col.line=col.line,lin=line,par.settings=my.settings,
              #xlim=c(0,4),
              scales=list(alternating=1,x=list(cex=1),
                          y=list(relation="free")),
              key=list(position="top",background="white",
                       title="Year",text=list(txt.legend),points=list(pch=my.pch1,col=my.fill1,cex=1)),                       
              panel=function(x,y,lin,col.line,...){    
                panel.abline(v=seq(0,.5,.1),lwd=1,lty=3,col="darkgrey")
                panel.dotplot(x,y,col.line="grey",lwd=1,lty=1,...)                     
              }),strip.left=T)
f1.y
pdf(file="Distance_y.pdf",width=8,height=11)
print(f1.y)
dev.off()

f1.ya.data <- subset(Dist.data,Ages=="15-39")
f1.ya <- useOuterStrips( dotplot(Statenom ~ Distance|Sex+region,aspect = c(0.9),ylab= list("State",cex=1),
                                #layout=c(3,6),
                                strip=T,
                                xlab=list("Distance from benchmark survival",cex=1),main=FALSE,cex=1,
                                data=f1.ya.data, groups=Year,pch=my.pch, col=my.fill,col.line=col.line,lin=line,par.settings=my.settings,
                                #xlim=c(0,4),
                                scales=list(alternating=1,x=list(cex=1),
                                            y=list(relation="free")),
                                key=list(position="top",background="white",
                                         title="Year",text=list(txt.legend),points=list(pch=my.pch1,col=my.fill1,cex=1)),                       
                                panel=function(x,y,lin,col.line,...){    
                                  panel.abline(v=seq(0,2,.5),lwd=1,lty=3,col="darkgrey")
                                  panel.dotplot(x,y,col.line="grey",lwd=1,lty=1,...)                     
                                }),strip.left=T)
f1.ya
pdf(file="Distance_ya.pdf",width=8,height=11)
print(f1.ya)
dev.off()


f1.oa.data <- subset(Dist.data,Ages=="40-74")
f1.oa <- useOuterStrips( dotplot(Statenom ~ Distance|Sex+region,aspect = c(0.9),ylab= list("State",cex=1),
                                 #layout=c(3,6),
                                 strip=T,
                                 xlab=list("Distance from benchmark survival",cex=1),main=FALSE,cex=1,
                                 data=f1.oa.data, groups=Year,pch=my.pch, col=my.fill,col.line=col.line,lin=line,par.settings=my.settings,
                                 #xlim=c(0,4),
                                 scales=list(alternating=1,x=list(cex=1),
                                             y=list(relation="free")),
                                 key=list(position="top",background="white",
                                          title="Year",text=list(txt.legend),points=list(pch=my.pch1,col=my.fill1,cex=1)),                       
                                 panel=function(x,y,lin,col.line,...){    
                                   panel.abline(v=seq(0,4,.5),lwd=1,lty=3,col="darkgrey")
                                   panel.dotplot(x,y,col.line="grey",lwd=1,lty=1,...)                     
                                 }),strip.left=T)
f1.oa
pdf(file="Distance_oa.pdf",width=8,height=11)
print(f1.oa)
dev.off()





### one plot for the paper



fig.data <- subset(Dist.data,Sex=="Males" & Ages == "40-74")

  f2 <- dotplot(Statenom ~ Distance|region,aspect = c(0.9),ylab= list("State",cex=1.3),strip.left=T,layout=c(1,3),strip=F,
            xlab=list("Distance from benchmark survival",cex=1.3),main=FALSE,cex=1,
            data=fig.data, groups=Year,pch=my.pch, col=my.fill,col.line=col.line,lin=line,par.settings=my.settings,
            xlim=c(0,4),between=list(y=.5),
            scales=list(alternating=1,x=list(cex=1,at=seq(0,4,.5), 
                                             labels=as.character(seq(0,4,.5))),
                        y=list(relation="free")),
            key=list(space="right",background="white",
                     title="Year",text=list(txt.legend),points=list(pch=my.pch1,col=my.fill1,cex=1)),                       
            panel=function(x,y,lin,col.line,...){    
              panel.abline(v=seq(.5,3.5,1),lwd=1,lty=3,col="darkgrey")
              panel.abline(v=seq(0,4,1),lwd=1,lty=2,col="black")
              #panel.abline(v=lin[[1]],col=col.line[[1]],lwd=1,lty=2)
              panel.dotplot(x,y,col.line="grey",lwd=1,lty=1,...)                     
            })
  f2







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

  
#### calculate proportions
  Data <- data.table(Data)
  my.prop <- function(c){
    z <- c/sum(c)
    z
  }
  Data <- Data[,Prop:= my.prop(Contribution), by = list(Year,State,Ages,Sex)]
  
  
  f2.cause <- Data[Ages=="40-74" & Year==2015 & Sex== "Males"]
  

myColours1 <- c("pink","blue","deepskyblue3", "lightgrey","darkblue","green","red", "orange",  "gray69","gray69")
#myColours1 <- c("cornflowerblue","red","chocolate2","yellow", "orange", "coral", "lightgrey","blue","darkorchid1","black","grey")
fig.labels <- c("1.Causes amenable to medical service",
"2.Diabetes",
"3.Ischemic heart diseases",
"4.HIV/AIDS",
"5.Lung cancer",
"6.Cirrhosis",
"7.Homicide",
"8.Road traffic accidents",
"9. Other causes")

my.settings1 <- list(
  superpose.polygon=list(col=myColours1[], border="transparent"),
  strip.background=list(col="grey"),
  strip.border=list(col="black")
)


f2.c <- merge(f2.cause,ref.order, by = c("State"),all.y = T)

f2.c$Statenom<-with(f2.c,reorder(reorder(Statenom,Order),as.numeric(region)))



f2.c2 <-  barchart(Statenom ~ Prop |region, data=f2.c,
                   #aspect = c(.9),
                   #aspect = "xy",
                      groups=AMCategory,  ylab="",xlab=list("Proportion explained by cause of death",cex=1.3),
           layout=c(1,3),strip.left=F,strip=F,
                      stack=TRUE,
                      between = list(y = .5),
           scales=list(alternating=1,x=list(cex=1,at=seq(0,4,.5), 
                                            labels=as.character(seq(0,4,.5))),
                       y=list(relation="free")),
                      par.settings=my.settings1,
                      key = list(space="right", title="Cause of death",background="white",
                                 text=list(fig.labels)
                                 ,cex=.9,
                                 points=list(pch=19,col=myColours1[-10])))


require(gridExtra)
pdf(file="Figure 4.pdf",width=15,height=7,pointsize=12)
grid.arrange(f2,f2.c2,ncol=2)
dev.off()






