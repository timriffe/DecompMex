### Based on version 1.
#######################################
# IMPORTANT: impute zeros for missing ages, causes, states, years
# otherwise BP is too high...
rm(list=ls(all=TRUE))

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



# Load data and group causes according to paper ---------------------------
load('Data/State_CIs_1D.RData')
Results_CI<- Results_CI[,c('year','state','sex','age','mxs','CI1','CI2')]

library(data.table)
library(reshape2)

source('R/Functions.R')

##################### Now for all the states
mxs.states  <- Results_CI

# now get temp e0 for states mxs
Stateslx    <- mxs.states[,lx:=myLTlx(mxs, sex), by = list(state,year, sex)]
Stateslx    <- Stateslx[,Lx := lx2Lx(lx), by = list(state,year, sex)]
head(Stateslx)
ste0_14     <- Stateslx[,getTempe0(.SD,lowera = 0, uppera = 14),by=list(state,year, sex)]
ste0_14$age.g <-1
ste15_49    <- Stateslx[,getTempe0(.SD,lowera=15,uppera=49),by=list(state,year, sex)]
ste15_49$age.g <-2
ste50_84    <- Stateslx[,getTempe0(.SD,lowera=50,uppera=84),by=list(state,year, sex)]
ste50_84$age.g <-3
ste_temp <- rbind(ste0_14,ste15_49,ste50_84)
setnames(ste_temp,"V1","temp_e0")
head(ste_temp)


# now get temp e0 for states CI1
Stateslx    <- mxs.states[,lx:=myLTlx(CI1, sex), by = list(state,year, sex)]
Stateslx    <- Stateslx[,Lx := lx2Lx(lx), by = list(state,year, sex)]
head(Stateslx)
ste0_14     <- Stateslx[,getTempe0(.SD,lowera = 0, uppera = 14),by=list(state,year, sex)]
ste0_14$age.g <-1
ste15_49    <- Stateslx[,getTempe0(.SD,lowera=15,uppera=49),by=list(state,year, sex)]
ste15_49$age.g <-2
ste50_84    <- Stateslx[,getTempe0(.SD,lowera=50,uppera=84),by=list(state,year, sex)]
ste50_84$age.g <-3
ste_tempCI1 <- rbind(ste0_14,ste15_49,ste50_84)
setnames(ste_tempCI1,"V1","temp_e0")
head(ste_tempCI1)


# now get temp e0 for states CI2
Stateslx    <- mxs.states[,lx:=myLTlx(CI2, sex), by = list(state,year, sex)]
Stateslx    <- Stateslx[,Lx := lx2Lx(lx), by = list(state,year, sex)]
head(Stateslx)
ste0_14     <- Stateslx[,getTempe0(.SD,lowera = 0, uppera = 14),by=list(state,year, sex)]
ste0_14$age.g <-1
ste15_49    <- Stateslx[,getTempe0(.SD,lowera=15,uppera=49),by=list(state,year, sex)]
ste15_49$age.g <-2
ste50_84    <- Stateslx[,getTempe0(.SD,lowera=50,uppera=84),by=list(state,year, sex)]
ste50_84$age.g <-3
ste_tempCI2 <- rbind(ste0_14,ste15_49,ste50_84)
setnames(ste_tempCI2,"V1","temp_e0")
head(ste_tempCI2)

ste_temp$CI1 <- ste_tempCI1$temp_e0
ste_temp$CI2 <- ste_tempCI2$temp_e0


CI_Data <- ste_temp

# states codes and names
state.code.recvec <- c("Aguascalientes","Baja California","Baja California Sur","Campeche",
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
CI_Data$Statenom            <- state.code.recvec[as.character(CI_Data$state)]
CI_Data$region              <- region.recvec[as.character(CI_Data$state)] 

# Turn region, Ages, Sex into levelled/ordered factors
CI_Data$region <- factor(CI_Data$region, 
                      levels =rev( c(1, 2, 3)), 
                      labels = rev(c("South", "Central", "North")))
CI_Data$age.g   <- factor(CI_Data$age.g, 
                      levels = c(1, 2, 3), 
                      labels = c("0-14", "15-49", "50-84"))
CI_Data$sex    <- factor(CI_Data$sex, 
                      levels = c(1, 2), 
                      labels = c("Females", "Males"))

save(CI_Data,file = 'Data/CI_Data_1D.RData')
gdata::keep(CI_Data,sure=T)



## Plot a table with 
library(ggplot2)
figdata1 <- CI_Data[CI_Data$sex=='Males' & CI_Data$age.g == '0-14' & CI_Data$year %in% c(2005,2010,2015),]
  
etemp.CI.males1 <- ggplot(figdata1) +
  ggtitle(bquote('Temporary life expectancy for young '~(0-14)), subtitle = 'by year')+
  geom_errorbarh(data = figdata1, aes(xmax = CI1, xmin = CI2, y = reorder(Statenom,temp_e0), x= temp_e0,height =.5),show.legend = F)+
  geom_point(data = figdata1, aes(round(temp_e0,3), reorder(Statenom,temp_e0),col=factor(year)),size = 3,show.legend = F) +
  facet_grid(region ~ factor(year), scales = "free") +
  theme_light()+
  xlab('Years')+
  scale_color_manual(values=c('green','blue','red'))+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 10, angle = 00))+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black",angle = 00))+
  theme(strip.text.y = element_text(colour = "black"))
  

etemp.CI.males1

figdata2 <- CI_Data[CI_Data$sex=='Males' & CI_Data$age.g == '15-49' & CI_Data$year %in% c(2005,2010,2015),]

etemp.CI.males2 <- ggplot(figdata2) +
  ggtitle(bquote('Temporary life expectancy for young adults '~(15-49)), subtitle = 'by year')+
  geom_errorbarh(data = figdata2, aes(xmax = CI1, xmin = CI2, y = reorder(Statenom,temp_e0), x= temp_e0,height =.5),show.legend = F)+
  geom_point(data = figdata2, aes(round(temp_e0,3), reorder(Statenom,temp_e0),col=factor(year)),size = 3,show.legend = F) +
  facet_grid(region ~ factor(year), scales = "free") +
  theme_light()+
  xlab('Years')+
  scale_color_manual(values=c('green','blue','red'))+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 10, angle = 00))+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black",angle = 00))+
  theme(strip.text.y = element_text(colour = "black"))


etemp.CI.males2


figdata3 <- CI_Data[CI_Data$sex=='Males' & CI_Data$age.g == '50-84' & CI_Data$year %in% c(2005,2010,2015),]

etemp.CI.males3 <- ggplot(figdata3) +
  ggtitle(bquote('Temporary life expectancy for older adults '~(50-84)), subtitle = 'by year')+
  geom_errorbarh(data = figdata3, aes(xmax = CI1, xmin = CI2, y = reorder(Statenom,temp_e0), x= temp_e0,height =.5),show.legend = F)+
  geom_point(data = figdata3, aes(round(temp_e0,3), reorder(Statenom,temp_e0),col=factor(year)),size = 3,show.legend = F) +
  facet_grid(region ~ factor(year), scales = "free") +
  theme_light()+
  xlab('Years')+
  scale_color_manual(values=c('green','blue','red'))+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 10, angle = 00))+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black",angle = 00))+
  theme(strip.text.y = element_text(colour = "black"))


etemp.CI.males3

pdf(file="BMJ Open Revise and Resubmit/Revision/Temp_e0_CIs_1D.pdf",width=7,height=7,pointsize=12)
etemp.CI.males1
etemp.CI.males2
etemp.CI.males3
dev.off()


#################################################

figdata1 <- CI_Data[CI_Data$sex=='Males' &  CI_Data$year %in% c(2005,2010,2015),]

etemp.CI.males1 <- ggplot(figdata1) +
  ggtitle(bquote('Temporary life expectancy '), subtitle = 'by year')+
  geom_errorbarh(data = figdata1, aes(xmax = CI1, xmin = CI2, y = reorder(Statenom,temp_e0), x= temp_e0,height =.2,col=factor(year)),show.legend = F)+
  geom_point(data = figdata1, aes(round(temp_e0,3), reorder(Statenom,temp_e0),col=factor(year)),size = 1.5,show.legend = T) +
  facet_grid(region ~ age.g, scales = "free") +
  theme_light()+
  xlab('Years')+
  scale_color_manual(values=c('green','blue','red'))+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 10, angle = 00))+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black",angle = 00))+
  theme(strip.text.y = element_text(colour = "black"))


etemp.CI.males1

pdf(file="BMJ Open Revise and Resubmit/Revision/Temp_e0_CIs_1D_2.pdf",width=10,height=9,pointsize=12)
etemp.CI.males1
dev.off()



