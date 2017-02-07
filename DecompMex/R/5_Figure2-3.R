
library(RColorBrewer)
library(latticeExtra)
library(xtable)
library(reshape2)

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

Data$Statenom<-with(Data,reorder(reorder(Statenom,State),as.numeric(region)))


##### Figure function
Figure.heatmap <- function(SubData,Age.group){
  useOuterStrips(levelplot(Contribution ~ Year*Statenom|AMLabel+region,                           
                           data=SubData,                       
                           main=F,
                           #main=paste("Cause-specific contributions to deviations from low mortality benchmark,",Age.group, sep = " "),
                           ylab.right = list("(Years)",cex=1.2),par.settings=my.settings,
                           xlab=list("Year",cex=1.2),ylab=list("State",cex=1.2),
                           #colorkey=T,
                           colorkey = list(at = seq(0,2,.25), labels=list(at=seq(0,2,.25))),
                           ylim= c(rep(list(c(1,10)),7),rep(list(c(11,21)),7),rep(list(c(22,32)),7)),
                           between = list(x = -8.5),scales=list(x=list(cex=1.3,rot=45,at=seq(1990,2015,5)),
                                                                y=list(relation="free",at = c(rep(list(c(1:32)),21)),
                                                                       labels=list(levels(Data$Statenom)[],NULL,NULL,NULL,NULL,NULL,NULL,
                                                                                   levels(Data$Statenom)[],NULL,NULL,NULL,NULL,NULL,NULL,
                                                                                   levels(Data$Statenom)[],NULL,NULL,NULL,NULL,NULL,NULL),
                                                                       tck=c(0,1),cex=1.2)),par.strip.text=list(cex=1.4),
                           #at=c(do.breaks(c(0,0.25),20), 
                           #    do.breaks(c(0.2500001,0.5),20),
                           #   do.breaks(c(0.5000001,1),20), 
                           #  do.breaks(c(1.0000001,2),20)),                               
                           #col.regions=colorRampPalette(c("white","royalblue","red","black")),
                           #Tim suggestion
                           at=c(do.breaks(c(0,0.25),20), 
                                do.breaks(c(0.2500001,0.5),20),
                                do.breaks(c(0.5000001,1),20), 
                                do.breaks(c(1.0000001,2),20)),                                                   
                           col.regions=colorRampPalette(brewer.pal(9,"YlOrRd")[c(1,4,7,9)]),
                           #col.regions=colorRampPalette(brewer.pal(9,"YlOrBr")[c(1,4,7,9)]),
                           
  ),strip.left=T)
}


#Male Adults
Heat.map1 <- Figure.heatmap(SubData=subset(Data,Sex=="Males" & Ages=="40-74" &
                                             AMCategory!=10&AMCategory!=4 & AMCategory!=9),
                            Age.group="male adults")

Heat.map1
pdf(file="Adult_Male_heatmap.pdf",width=20,height=11,pointsize=12)
print(Heat.map1)
dev.off()


#Female Adults
Heat.map2 <- Figure.heatmap(SubData=subset(Data,Sex=="Females" & Ages=="40-74" &
                                             AMCategory!=10&AMCategory!=4 & AMCategory!=9),
                            Age.group="female adults")
pdf(file="Adult_Female_heatmap.pdf",width=20,height=11,pointsize=12)
print(Heat.map2)
dev.off()


#Male young-adults
Heat.map3 <- Figure.heatmap(SubData=subset(Data,Sex=="Males" & Ages=="15-39" &
                                             AMCategory!=10&AMCategory!=4 & AMCategory!=9),
                            Age.group="male young-adults")
pdf(file="YoungAdult_Male_heatmap.pdf",width=20,height=11,pointsize=12)
print(Heat.map3)
dev.off()

#Female young-adults
Heat.map4 <- Figure.heatmap(SubData=subset(Data,Sex=="Females" & Ages=="15-39" &
                                             AMCategory!=10&AMCategory!=4 & AMCategory!=9),
                            Age.group="female young-adults")
pdf(file="YoungAdult_Female_heatmap.pdf",width=20,height=11,pointsize=12)
print(Heat.map4)
dev.off()


#Male young
Heat.map5 <- Figure.heatmap(SubData=subset(Data,Sex=="Males" & Ages=="0-14" &
                                             AMCategory!=10&AMCategory!=4 & AMCategory!=9),
                            Age.group="male young")
pdf(file="Young_Male_heatmap.pdf",width=20,height=11,pointsize=12)
print(Heat.map5)
dev.off()

#Female young
Heat.map6 <- Figure.heatmap(SubData=subset(Data,Sex=="Females" & Ages=="0-14" &
                                             AMCategory!=10&AMCategory!=4 & AMCategory!=9),
                            Age.group="female young")
pdf(file="Young_Female_heatmap.pdf",width=20,height=11,pointsize=12)
print(Heat.map6)
dev.off()

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

########################## Results for table (males)
Table.Data <- subset(Data,Year==2000|Year==2005|Year==2010)
Table.Data$Statenom<-with(Table.Data,reorder(reorder(Statenom,State),-as.numeric(region)))
Table.Data$Contribution1 <- round(Table.Data$Contribution,2)

Table <- cast(subset(Table.Data, Ages=="40-74" & Sex=="Males" & AMCategory!=10 & AMCategory!=9 &
                       AMCategory!=8 & AMCategory!= 4),
              Statenom ~ AMLabel +Year, fun = sum, value = "Contribution1")
Years <- as.character(c("State",rep(seq(2000,2010,5),(dim(Table)[2]-1)/3)))
Cause <- c("","",levels(Table.Data$AMLabel)[1],"",
           "",levels(Table.Data$AMLabel)[2],"",
           "",levels(Table.Data$AMLabel)[3],"",
           "",levels(Table.Data$AMLabel)[5],"",
           "",levels(Table.Data$AMLabel)[6],"",
           "",levels(Table.Data$AMLabel)[7],"")
Table1 <- rbind(Cause,Years, Table)
Region <- c(rep("",2),"North",rep("",10),"Central",rep("",10),"South",rep("",9))
Table1 <- cbind(Region,Table1)

print(xtable(Table1),include.rownames=FALSE, include.colnames = FALSE)



########################## Results for table (females)
Table <- cast(subset(Table.Data, Ages=="40-74" & Sex=="Females" & AMCategory!=10 & AMCategory!=9 &
                       AMCategory!=8 & AMCategory!= 4),
              Statenom ~ AMLabel +Year, fun = sum, value = "Contribution1")
Years <- as.character(c("State",rep(seq(2000,2010,5),(dim(Table)[2]-1)/3)))
Cause <- c("","",levels(Table.Data$AMLabel)[1],"",
           "",levels(Table.Data$AMLabel)[2],"",
           "",levels(Table.Data$AMLabel)[3],"",
           "",levels(Table.Data$AMLabel)[5],"",
           "",levels(Table.Data$AMLabel)[6],"",
           "",levels(Table.Data$AMLabel)[7],"")
Table1 <- rbind(Cause,Years, Table)
Region <- c(rep("",2),"North",rep("",10),"Central",rep("",10),"South",rep("",9))
Table1 <- cbind(Region,Table1)

print(xtable(Table1),include.rownames=FALSE, include.colnames = FALSE)

#################### some averages
Data1 <- as.data.frame(Data)
Data2 <- subset(Data1, Ages=="15-39" & Sex=="Males" & AMLabel=="Homicide" & Year==2010)
Data3 <- subset(Data1, Ages=="15-39" & Sex=="Males" & AMLabel=="Homicide" & Year==2005)
mean(Data2$Contribution)/mean(Data3$Contribution)



