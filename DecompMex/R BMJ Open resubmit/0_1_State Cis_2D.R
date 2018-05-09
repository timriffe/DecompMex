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
load('Data/State_CIs.RData')
Results_CI<- Results_CI[,c('year','state','sex','cause','age','mxs','CI1','CI2')]

library(data.table)
library(reshape2)

#Groups used in the article:
# 1. Amenable to meical service: 1+2+3+4+6
# 2. Diabetes: 5
# 3. IHD:7
# 4. HIV:8
# 5. Lung cancer: 10
# 6. Cirrhosis: 11
# 7. Homicide: 12
# 8. Road traffic: 13 
# 9. Suicide: 9
# 10. All other causes: 14+15+16

Min_Dta        <- Results_CI[Results_CI$cause!='g11',]
Mxscmin        <- Min_Dta[,list(mxs=min(mxs)), by = list(year,cause,sex, age)]
Mxscmin$CI1    <- Min_Dta[,list(CI1=CI1[which.min(mxs)]), by = list(year,cause,sex, age)]$CI1
Mxscmin$CI2    <- Min_Dta[,list(CI2=CI2[which.min(mxs)]), by = list(year,cause,sex, age)]$CI2
Mxsmin         <- Mxscmin[,list(mxs=sum(mxs),CI1=sum(CI1),CI2=sum(CI2)),by=list(year, sex, age)]


# object with minimum death rates for every year by sex and age
head(Mxsmin)


#######################################
# now get best practive lx and Lx, don't really need BP e0
#BPe0 <- Mxsmin[,myLT(mx_min, sex), by = list(year,sex)]
source('R/Functions.R')

#for mxx
BPlx <- Mxsmin[,lx:=myLTlx(mxs, sex), by = list(year,sex)]
BPlx[,Lx:=lx2Lx(lx),by = list(year,sex)]
# get temp e0 for BP
bpe0_14        <- BPlx[,getTempe0(.SD,0,14),by=list(year, sex)]
bpe0_14$state  <- 33
bpe0_14$age.g  <- 1
bpe15_49       <- BPlx[,getTempe0(.SD,15,49),by=list(year, sex)]
bpe15_49$state <- 33
bpe15_49$age.g <- 2
bpe50_84       <- BPlx[,getTempe0(.SD,50,84),by=list(year, sex)]
bpe50_84$state <- 33
bpe50_84$age.g <- 3
BP_temp        <- rbind(bpe0_14,bpe15_49,bpe50_84)
setnames(BP_temp,"V1","temp_e0")

#for CI1
#for mxx
BPlx <- Mxsmin[,lx:=myLTlx(CI1, sex), by = list(year,sex)]
BPlx[,Lx:=lx2Lx(lx),by = list(year,sex)]
# get temp e0 for BP
bpe0_14        <- BPlx[,getTempe0(.SD,0,14),by=list(year, sex)]
bpe0_14$state  <- 33
bpe0_14$age.g  <- 1
bpe15_49       <- BPlx[,getTempe0(.SD,15,49),by=list(year, sex)]
bpe15_49$state <- 33
bpe15_49$age.g <- 2
bpe50_84       <- BPlx[,getTempe0(.SD,50,84),by=list(year, sex)]
bpe50_84$state <- 33
bpe50_84$age.g <- 3
BP_tempCI1        <- rbind(bpe0_14,bpe15_49,bpe50_84)
setnames(BP_tempCI1,"V1","temp_e0")

#for CI2
BPlx <- Mxsmin[,lx:=myLTlx(CI2, sex), by = list(year,sex)]
BPlx[,Lx:=lx2Lx(lx),by = list(year,sex)]
# get temp e0 for BP
bpe0_14        <- BPlx[,getTempe0(.SD,0,14),by=list(year, sex)]
bpe0_14$state  <- 33
bpe0_14$age.g  <- 1
bpe15_49       <- BPlx[,getTempe0(.SD,15,49),by=list(year, sex)]
bpe15_49$state <- 33
bpe15_49$age.g <- 2
bpe50_84       <- BPlx[,getTempe0(.SD,50,84),by=list(year, sex)]
bpe50_84$state <- 33
bpe50_84$age.g <- 3
BP_tempCI2        <- rbind(bpe0_14,bpe15_49,bpe50_84)
setnames(BP_tempCI2,"V1","temp_e0")

BP_temp$CI1 <- BP_tempCI1$temp_e0
BP_temp$CI2 <- BP_tempCI2$temp_e0

##################### Now for all the states
mxs.states  <- Results_CI[Results_CI$cause=='g11',]


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

ste_temp <- ste_temp[,colnames(BP_temp),with=F]

CI_Data <- rbind(ste_temp,BP_temp)

# states codes and names
state.code.recvec <- c("Aguascalientes","Baja California","Baja California Sur","Campeche",
    "Coahuila","Colima","Chiapas","Chihuahua","Mexico City","Durango",
    "Guanajuato","Guerrero","Hidalgo","Jalisco","Mexico State","Michoacan",
    "Morelos","Nayarit","Nuevo Leon","Oaxaca","Puebla","Queretaro",
    "Quintana Roo","San Luis Potosi","Sinaloa","Sonora","Tabasco","Tamaulipas",
    "Tlaxcala","Veracruz","Yucatan","Zacatecas", "Benchmark")
names(state.code.recvec) <- 1:33

# state and region codes
region.recvec            <- c(2,3,3,1,3,2,1,3,2,3,2,1,2,2,2,
                              2,1,2,3,1,1,2,1,3,3,3,1,3,2,1,1,3,3)
names(region.recvec)     <- 1:33

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
                      labels = c("Males", "Females"))


Distance <- function(.SD){
  benchmark <- .SD[.SD$Statenom== 'Benchmark',]$temp_e0
  Distance  <- benchmark - .SD$temp_e0
  .SD$Distance <- Distance
  .SD
}

CI_Data <- CI_Data[,Distance(.SD), by = list(year,sex,age.g)]

save(CI_Data,file = 'Data/CI_Data.RData')
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

pdf(file="BMJ Open Revise and Resubmit/Revision/Temp_e0_CIs.pdf",width=7,height=7,pointsize=12)
etemp.CI.males1
etemp.CI.males2
etemp.CI.males3
dev.off()



## Plot a table with 

figdata4 <- CI_Data[CI_Data$sex=='Males' & CI_Data$year %in% c(2000,2010,2015),]


etemp.CI.males <- ggplot(figdata4) +
  ggtitle(bquote('Temporary life expectancy '), subtitle = 'by age group')+
  geom_pointrange(data = figdata4, aes(ymax = CI1, ymin = CI2, x = reorder(Statenom,temp_e0), y= temp_e0,col=factor(year)),
             show.legend = T,position = position_jitter(width=.2)) +
  facet_grid(region ~ age.g, scales = "free") +
  theme_light()+
  xlab('Years')+
  scale_color_manual(values=c('gray83','gray49','gray28'))+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 10, angle = 00))+
  theme(text = element_text(size=10),
        strip.text.x = element_text(size = 10, colour = "black",angle = 00))+
  theme(strip.text.y = element_text(colour = "black"))+
  coord_flip()
etemp.CI.males

pdf(file="BMJ Open Revise and Resubmit/Revision/Temp_e0_CIs_RR.pdf",width=11,height=10,pointsize=12,useDingbats = F)
etemp.CI.males
dev.off()



