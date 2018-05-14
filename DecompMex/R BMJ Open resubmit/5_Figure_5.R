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
                         upper = 15))
DM0_14 <- do.call(rbind,
                  lapply(1:length(DecomM0_14), 
                         FUN = my_reshape.function, 
                         DecompIn = DecomM0_14, 
                         lower = 0, 
                         upper = 15))

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
                          upper = 50))
DM15_49 <- do.call(rbind,
                   lapply(1:length(DecomM15_49), 
                          FUN = my_reshape.function, 
                          DecompIn = DecomM15_49, 
                          lower = 15, 
                          upper = 50))
		 # DM15_49[DM15_49$AMCategory=="g7"&DM15_49$Year==2015,]
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
                              upper = 85))
DM50_84     <- do.call(rbind,
                       lapply(1:length(DecomM50_84), 
                              FUN = my_reshape.function, 
                              DecompIn = DecomM50_84, 
                              lower = 50, 
                              upper = 85))

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
                      labels = c("Males", "Females"))

#### Distance data
# TR: how about we call it a 'gap'?
load('Data/CI_Data.RData')
Dist.data   <- CI_Data
Dist.data$region              <- region.recvec[as.character(Dist.data$state)] 

# Turn region, Ages, Sex into levelled/ordered factors
Dist.data$region <- factor(Dist.data$region, 
                      levels = c(1, 2, 3), 
                      labels = c("South", "Central", "North"))

ref.order   <- subset(Dist.data, year==2015 & sex == "Males" & age.g == "50-84")

setnames(ref.order, "Distance", "Order")
# select State and Order columns
ref.order   <- ref.order[,c(8,10),with=FALSE]
ref.order <- ref.order[order(-Order),]
ref.order <- ref.order[ref.order$Statenom!='Benchmark',]
head(ref.order)
save(ref.order, file= "Data/Order.Rdata")

Dist.data          <- merge(Dist.data, ref.order, by = c("Statenom"), all.y = TRUE)

Dist.data$Statenom <- with(Dist.data, reorder(reorder(Statenom, Order), as.numeric(region)))

# lez make some graphs!
#####################################################################################
# graphical parameters
my.settings <- list(  
  strip.background = list(col = "grey"),
  strip.border = list(col = "black"),
  auto.key = FALSE
)
makeTransparent<-function(someColor, alpha = 100)
{
  newColor <- col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){
    rgb(red = curcoldata[1], 
        green = curcoldata[2],
        blue = curcoldata[3], 
        alpha = alpha, 
        maxColorValue = 255)})
}

# TR: changing year color codes to sequential blues
blues           <- brewer.pal(5,"Blues")[3:5]

my.pch			<- c(17:19)  
my.fill			<- blues 
col.line		<- list(c("black"))
txt.legend		<- c("2005", "2010", "2015")
my.pch1			<- c(17:19) 
my.fill1		<- blues
my.settings 	<- list(  
  strip.background = list(col = "darkgrey"),
  strip.border = list(col = "black")
)

main.label <- "Gap with benchmark life expectancy (years)"




fig.data <- subset(Dist.data,sex=="Males" & age.g == "50-84")


fig.data2<- subset(fig.data,year==2015)
f2.1 <- dotplot(Statenom ~ Distance | region,
              aspect = c(0.9),
              ylab = "", 
              strip.left = TRUE, 
              layout = c(1, 3), 
              strip = FALSE,
              xlab = list(main.label, cex=1),
              main = FALSE, 
              cex = 1,
              data = fig.data2,
              groups = year, 
              pch = my.pch[3], 
              col = my.fill[3],
              col.line = col.line,
              lin = line,
              par.settings = my.settings,
              xlim = c(0,4),
              between = list(y = .5),
              scales = list(alternating = 1,
                            x = list(cex = 1, at = seq(0,4,1), labels = as.character(seq(0,4,1))),
                            y = list(relation = "free")),
              panel=function(x, y, lin, col.line,...){    
                #panel.abline(v=seq(.5,3.5,1),lwd=1,lty=3,col="darkgrey")
                panel.abline(v=seq(0,4,1),lwd=1,lty=1,col="#A1A1A150")
                #panel.abline(v=lin[[1]],col=col.line[[1]],lwd=1,lty=2)
                panel.dotplot(x,y,col.line="#A1A1A150",lwd=1,lty=1,...)                     
              })
f2.1


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
Data$col.label <- 8
Data[AMCategory=="g1"]$col.label <- 1
Data[AMCategory=="g2"]$col.label <- 2
Data[AMCategory=="g3"]$col.label <- 3
Data[AMCategory=="g5"]$col.label <- 4
Data[AMCategory=="g6"]$col.label <- 5
Data[AMCategory=="g7"]$col.label <- 6
Data[AMCategory=="g8"]$col.label <- 7

Data.fig <- Data[,sum(Contribution), by = list(Year,State,Ages,Sex,col.label)]
setnames(Data.fig,"V1","Contribution")
head(Data.fig)  
  
my.prop <- function(c){
  z <- c/sum(c)
  z
}

Data.fig <- Data.fig[,Prop:= my.prop(Contribution), by = list(Year,State,Ages,Sex)]
Data.fig$Statenom <- state.code.recvec[as.character(Data.fig$State)]
Data.fig$region   <- region.recvec[as.character(Data.fig$State)] 
# labels according to figure

f2.cause <- Data.fig[Ages=="50-84" & Year==2015 & Sex== "Males"]



#myColours1 <- c("pink","blue","deepskyblue3", "lightgrey","darkblue","green","red", "orange",  "gray69","gray69")
#myColours1 <- c("pink","darkblue","blue","deepskyblue3","lightblue1","red", "orange",  "lightgrey","lightgrey","lightgrey")


# palette generated here:https://bl.ocks.org/cmpolis/fbc368f79c06c1912b8c04c795f592a8
# settings = 7 colors, saturation 73%, lightness 65%, 246 degrees base hue
# then maniupulate, always taking every 4th, to maximize distance:
# by hand:
base2 <- toupper(c("#7265e7", "#e265e7", "#e7657d", "#e7bc65", "#a2e765", "#65e797", "#65c7e7"))
#plot(1:7,1:7,col=toupper(base2),pch=16,cex=5)
# determine order by eyeballing colors to causes (HT J. Schoeley)
base2 <- base2[c(5,4,2,1,6,3,7)]

#myColours1 <- c("pink","darkblue","blue","deepskyblue3","lightblue1","red", 
#		"orange",  "lightgrey","lightgrey","lightgrey")
myColours1 <- c(toupper(base2), rep(gray(.7), 1))

fig.labels <- c("Causes amenable to medical service",
                "Diabetes",
                "Ischemic heart diseases",
                "Lung cancer",
                "Cirrhosis",
                "Homicide",
                "Road traffic accidents",
                "Other causes")


my.settings1 <- list(
  superpose.polygon=list(col=myColours1, border=gray(.5)),
  strip.background=list(col="grey"),
  strip.border=list(col="black")
)


f2.c           <- merge(f2.cause,ref.order, by = c("Statenom"),all.y = T)


f2.c$Statenom  <- with(f2.c,reorder(reorder(Statenom,Order),as.numeric(region)))

# TR:
# assign Other to all causes under 10%
#f2.c$AMCategory[f2.c$Prop < .1] <- 'g10'


# TR: would like to order causes descending within states.
# i.e. not always the same order. Then the message is to each state
# what they can work on.


f2.c2 <-  barchart(Statenom ~ Prop |region, data=f2.c,
                   groups = col.label,  ylab="",xlab=list("Gap composition by cause of death in 2015",cex=1),
                   layout=c(1,3),strip.left=F,strip=F,
                   stack=TRUE,box.ratio=5,
                   between = list(y = .5),
                   scales=list(alternating=1,x=list(cex=1,at=seq(0,1,.25), 
                                                    labels=as.character(seq(0,1,.25))),
                               y=list(relation="free")),
                   par.settings=my.settings1,
                   key = list(space="right", title="Cause of death",background="white",
                              points=list(pch=19,col=myColours1),
                              text=list(fig.labels)
                              ,cex=.9),
                   panel=function(x,y,lin,col.line,...){    
                     panel.barchart(x,y,...)                     
                     panel.abline(v=seq(0,1,.25),lwd=1,lty=2,col="darkgrey")
                   })
f2.c2

library(gridExtra)
library(grid)
blank<-rectGrob(gp=gpar(col="white"))

# pdf(file="Figure 4.pdf",width=15,height=7,pointsize=12)
# grid.arrange(f2,blank,f2.c2,ncol=3,widths=c(0.7,-.15, 0.9))
# dev.off()

pdf(file="BMJ Open Revise and Resubmit/Figures/Gap_Prop_Fig.pdf",width=13,height=7,pointsize=12,useDingbats = F)
grid.arrange(f2.1,blank,f2.c2,ncol=3,widths=c(0.7,-.15, 0.8))
dev.off()






