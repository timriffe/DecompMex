
setwd("C:/Users/jmaburto/Documents/GitHub/DecompMex/DecompMex")

if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
  # if I'm on the laptop
  setwd("/home/tim/git/DecompMex/DecompMex")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/DecompMex/DecompMex"))
}


library(latticeExtra)
library(data.table)

load("Data/Temp_e0_results.RData")
source("R/Functions_fig.R")

fig1.data <- temp.data
fig1.data$age.g <- factor(fig1.data$age.g,levels = 1:3, labels = c("Young (0-14)","Young adults (15-39)","Older adults (40-74)"))
fig1.data$sex<- factor(fig1.data$sex,levels = 1:2, labels = c("Males","Females"))

Fig1 <- useOuterStrips(xyplot(temp_e0 ~ year|age.g+sex,groups = state,data = fig1.data,type="l",
       col= c(rep(makeTransparent("black",alpha=65),32),"blue","red"),lwd=2,between=list(x=1,y=1),
       key=list(space="bottom",background="transparent",text=list(c('State','Record holder','Low benchmark'),
                              col="black"),cex=1,lines=list(lty=1,lwd=2,col=c("black","red","blue"))),
       xlab="",ylab = "Temporary life expectancy",par.settings=my.settings,par.strip.text=list(cex=1.3),
       scales = list(alternating=1,x=list(cex=.9,at=c(seq(1990,2015,5))),y = list(relation = "free",cex=.9),tck=c(1,0)),
       ylim = list(range(pretty(fig1.data[fig1.data$age.g=="Young (0-14)"]$temp_e0)),
                    range(pretty(fig1.data[fig1.data$age.g=="Young adults (15-39)"]$temp_e0)),
                    range(pretty(fig1.data[fig1.data$age.g=="Older adults (40-74)"]$temp_e0))),
       panel = function(x, y, ...){                        
         panel.abline(v=c(seq(1990,2015,5)),col='dark grey',lty=3)				
         panel.xyplot(x, y,lty=1,...)	
       }),strip.left = T)

Fig1

# print and save figures
pdf("Figures/Figure 1_NoConstrain smoothed.pdf",width=14,height=8)
print(Fig1)
dev.off()


### some querys for the paper
fig1.data$Statenom <- fig1.data$state
fig1.data$Statenom  <- factor(fig1.data$Statenom ,levels=c(1:34),
                              labels=c("Aguascalientes","Baja California","Baja California Sur",
                                       "Campeche","Coahuila","Colima","Chiapas","Chihuahua",
                                       "Mexico City","Durango","Guanajuato","Guerrero",
                                       "Hidalgo","Jalisco","Mexico State","Michoacan","Morelos",
                                       "Nayarit","Nuevo Leon","Oaxaca","Puebla","Queretaro",
                                       "Quintana Roo","San Luis Potosi","Sinaloa","Sonora",
                                       "Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatan",
                                       "Zacatecas","Benchmark","Record"))


setorder(fig1.data[year == 2015 & age.g=="Young (0-14)" & sex=="Females"],temp_e0)[]





for (j in 1:32){
  plot(setorder(fig1.data[state == j & age.g=="Young (0-14)" & sex=="Females"],year)$temp_e0, type="l")
  print(j)
Sys.sleep(2)
}

