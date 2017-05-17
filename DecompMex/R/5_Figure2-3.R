
library(RColorBrewer)
library(latticeExtra)
library(xtable)
library(reshape2)
library(data.table)

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


source("R/Functions.R")


###### for ages 0 - 14 ##############################################################
#####################################################################################
### For females
DecomF0_14 <- local(get(load("Data/ContribFemales0_14.Rdata")))
DecomM0_14 <- local(get(load("Data/ContribMales0_14.Rdata")))

# TR: rather than growing a data.frame in a loop, do it in an lapply()
DF0_14 <- do.call(rbind,
                  lapply(1:length(DecomF0_14), 
                         FUN = my_reshape.function, 
                         DecompIn = DecomF0_14, 
                         lower = 0, 
                         upper = 14))
DM0_14 <- do.call(rbind,
                  lapply(1:length(DecomM0_14), 
                         FUN = my_reshape.function, 
                         DecompIn = DecomM0_14, 
                         lower = 0, 
                         upper = 14))

###### for ages 15 - 49 ##############################################################
#####################################################################################
DecomF15_49 <- local(get(load("Data/ContribFemales15_49.Rdata")))
DecomM15_49 <- local(get(load("Data/ContribMales15_49.Rdata")))
# then do the aggregate-reshape
DF15_49 <- do.call(rbind,
                   lapply(1:length(DecomF15_49), 
                          FUN = my_reshape.function, 
                          DecompIn = DecomF15_49, 
                          lower = 15, 
                          upper = 49))
DM15_49 <- do.call(rbind,
                   lapply(1:length(DecomM15_49), 
                          FUN = my_reshape.function, 
                          DecompIn = DecomM15_49, 
                          lower = 15, 
                          upper = 49))

###### for ages 40 - 74 ##############################################################
#####################################################################################
### For females
DecomF50_84 <- local(get(load("Data/ContribFemales50_84.Rdata")))
DecomM50_84 <- local(get(load("Data/ContribMales50_84.Rdata")))
DF50_84     <- do.call(rbind,
                       lapply(1:length(DecomF50_84), 
                              FUN = my_reshape.function, 
                              DecompIn = DecomF50_84, 
                              lower = 50, 
                              upper = 84))
DM50_84     <- do.call(rbind,
                       lapply(1:length(DecomM50_84), 
                              FUN = my_reshape.function, 
                              DecompIn = DecomM50_84, 
                              lower = 50, 
                              upper = 84))

## Data frame with results for males
gdata::keep(DF0_14,DM0_14,DF15_49,DM15_49,DF50_84,DM50_84,sure=TRUE)
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

# TR: I simplified this by making a recode vec here too,
# but it seems this could have happened in my_reshape.function() too, no?
cause.labels <- c("AMS","Diabetes","IHD","HIV","Lung cancer","Cirrhosis","Homicide",
                  "RTA","Suicide","RC")

lev.am <- levels(DM0_14$AMCategory)
DM0_14$AMLabel  <- factor(DM0_14$AMCategory, levels =lev.am, labels = cause.labels)
DF0_14$AMLabel  <- factor(DF0_14$AMCategory, levels = lev.am, labels = cause.labels)
DF15_49$AMLabel <- factor(DF15_49$AMCategory, levels =lev.am, labels = cause.labels)	
DM15_49$AMLabel <- factor(DM15_49$AMCategory, levels = lev.am, labels = cause.labels)	
DF50_84$AMLabel <- factor(DF50_84$AMCategory, levels = lev.am, labels = cause.labels)
DM50_84$AMLabel <- factor(DM50_84$AMCategory, levels = lev.am, labels = cause.labels)

# Age group codes
DM0_14$Ages  <- 1
DF0_14$Ages  <- 1
DF15_49$Ages <- 2
DM15_49$Ages <- 2
DF50_84$Ages <- 3
DM50_84$Ages <- 3

# Sex codes
DM0_14$Sex  <- 2
DF0_14$Sex  <- 1
DF15_49$Sex <- 1
DM15_49$Sex <- 2
DF50_84$Sex <- 1
DM50_84$Sex <- 2

Data <- rbind(DM0_14,DF0_14,DM15_49,DF15_49,DM50_84,DF50_84)
gdata::keep(Data, sure = TRUE)

###########################################
# TR: make two recode vectors:

# states codes and names
state.code.recvec <- 
  c("Aguascalientes","Baja California","Baja California Sur","Campeche",
    "Coahuila","Colima","Chiapas","Chihuahua","Mexico City","Durango",
    "Guanajuato","Guerrero","Hidalgo","Jalisco","Mexico State","Michoacan",
    "Morelos","Nayarit","Nuevo Leon","Oaxaca","Puebla","Queretaro",
    "Quintana Roo","San Luis Potosi","Sinaloa","Sonora","Tabasco","Tamaulipas",
    "Tlaxcala","Veracruz","Yucatan","Zacatecas")
names(state.code.recvec) <- 1:32

# state and region codes
region.recvec            <- c(2,3,3,1,3,2,1,3,2,3,2,1,2,2,2,
                              2,1,2,3,1,1,2,1,3,3,3,1,3,2,1,1,3)
names(region.recvec)     <- 1:32

# adding recode columns is then easy
Data$Statenom            <- state.code.recvec[as.character(Data$State)]
Data$region              <- region.recvec[as.character(Data$State)] 

# Turn region, Ages, Sex into levelled/ordered factors
Data$region <- factor(Data$region, 
                      levels = c(1, 2, 3), 
                      labels = c("South", "Central", "North"))
Data$Ages   <- factor(Data$Ages, 
                      levels = c(1, 2, 3), 
                      labels = c("0-14", "15-49", "50-84"))
Data$Sex    <- factor(Data$Sex, 
                      levels = c(1, 2), 
                      labels = c("Females", "Males"))

load("Data/order.RData")

Data <- merge(Data,ref.order, by = c("State"),all.y = T)

Data$Statenom<-with(Data,reorder(reorder(Statenom,Order),as.numeric(region)))


##### Figure function
source("R/Functions_fig.R")
brksl <- seq(0,1.5,by=.25)
brksval <- seq(0,1.5,by=.25/10)
Figure.heatmap <- function(SubData,Age.group,brksl,brksval){
  useOuterStrips(levelplot(Contribution ~ Year*Statenom|AMLabel+region,                           
                           data=SubData,                       
                           main=F,
                           ylab.right = list("(Years)",cex=1.2),par.settings=my.settings,
                           xlab=list("Year",cex=1.2),ylab=list("State",cex=1.2),
                           #colorkey=T,
                           colorkey = list(at = brksl, labels=list(at=brksl)),
                           ylim= c(rep(list(c(1,10)),7),rep(list(c(11,21)),7),rep(list(c(22,32)),7)),
                           between = list(x = -8.5),scales=list(x=list(cex=1.3,rot=45,at=seq(1990,2015,5)),
                                                                y=list(relation="free",at = c(rep(list(c(1:32)),21)),
                                                                       labels=list(levels(Data$Statenom)[],NULL,NULL,NULL,NULL,NULL,NULL,
                                                                                   levels(Data$Statenom)[],NULL,NULL,NULL,NULL,NULL,NULL,
                                                                                   levels(Data$Statenom)[],NULL,NULL,NULL,NULL,NULL,NULL),
                                                                       tck=c(0,1),cex=1.2)),par.strip.text=list(cex=1.4),
                           at=brksval,    
                           col.regions=colorRampPalette(brewer.pal(9,"YlOrRd"),space = "Lab"),
                           #at=c(do.breaks(c(0,0.2),20), 
                            #    do.breaks(c(0.200001,0.6),20),
                            #    do.breaks(c(0.6000001,1.2),20), 
                            #    do.breaks(c(1.2000001,2.6),20)),                                                   
                           #col.regions=colorRampPalette(brewer.pal(9,"YlOrRd")[c(1,4,6,9)]),
                           
  ),strip.left=T)
}



#Male Adults
Heat.map1 <- Figure.heatmap(SubData=Data[Sex=="Males" & Ages=="50-84" &
                                          AMCategory!="g10" & AMCategory!="g4" & AMCategory!="g9"],
                            Age.group="male adults",brksl,brksval)

Heat.map1
pdf(file="Paper Figures/Adult_Male_heatmap.pdf",width=20,height=11,pointsize=12)
print(Heat.map1)
dev.off()


#Male Adults (same scale as appendic)
brksl <- seq(0,2.75,by=.25)
brksval <- seq(0,2.75,by=.25/10)
Heat.map2 <- Figure.heatmap(SubData=Data[Sex=="Males" & Ages=="50-84" &
                                           AMCategory!="g10" & AMCategory!="g4" & AMCategory!="g9"],
                            Age.group="female adults",brksl,brksval)
Heat.map2
pdf(file="Figures/Adult_Male_heatmap.pdf",width=20,height=11,pointsize=12)
print(Heat.map2)
dev.off()


#Female Adults
brksl <- seq(0,2.75,by=.25)
brksval <- seq(0,2.75,by=.25/10)
Heat.map2 <- Figure.heatmap(SubData=Data[Sex=="Females" & Ages=="50-84" &
                                           AMCategory!="g10" & AMCategory!="g4" & AMCategory!="g9"],
                            Age.group="female adults",brksl,brksval)
Heat.map2
pdf(file="Appendix Figures/Adult_Female_heatmap.pdf",width=20,height=11,pointsize=12)
print(Heat.map2)
dev.off()


#Male young-adults
setorder(Data[Sex=="Males" & Ages=="15-49" & AMCategory=="g7" & Year == 2015],Contribution)[]



brksl <- seq(0,2.75,by=.25)
brksval <- seq(0,2.75,by=.25/10)
Heat.map3 <- Figure.heatmap(SubData=Data[Sex=="Males" & Ages=="15-49" &
                                           AMCategory!="g10" & AMCategory!="g4" & AMCategory!="g9"],
                            Age.group="male young-adults",brksl,brksval)
Heat.map3
pdf(file="Appendix Figures/YoungAdult_Male_heatmap.pdf",width=20,height=11,pointsize=12)
print(Heat.map3)
dev.off()

#Female young-adults
Heat.map4 <- Figure.heatmap(SubData=Data[Sex=="Females" & Ages=="15-49" &
                                           AMCategory!="g10" & AMCategory!="g4" & AMCategory!="g9"],
                            Age.group="female young-adults",brksl,brksval)
Heat.map4
pdf(file="Appendix Figures/YoungAdult_Female_heatmap.pdf",width=20,height=11,pointsize=12)
print(Heat.map4)
dev.off()


#Male young

Heat.map5 <- Figure.heatmap(SubData=Data[Sex=="Males" & Ages=="0-14" &
                                           AMCategory!="g10" & AMCategory!="g4" & AMCategory!="g9"],
                            Age.group="male young",brksl,brksval)
Heat.map5
pdf(file="Appendix Figures/Young_Male_heatmap.pdf",width=20,height=11,pointsize=12)
print(Heat.map5)
dev.off()

#Female young
Heat.map6 <- Figure.heatmap(SubData=Data[Sex=="Females" & Ages=="0-14" &
                                           AMCategory!="g10" & AMCategory!="g4" & AMCategory!="g9"],
                            Age.group="female young",brksl,brksval)
Heat.map6
pdf(file="Appendix Figures/Young_Female_heatmap.pdf",width=20,height=11,pointsize=12)
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



