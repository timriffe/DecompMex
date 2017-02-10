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

# TR:
# Aggregator / reshaper function:
# TR: this function replaces the for loops. Now everything
# a little more robust and explicit.

my_reshape.function <- function(i,DecompIn, lower = 0, upper = 15){
  Z        <- DecompIn[[i]]
  # and I'm going to append Age and State before
  # rbind.list.ing
  
  Z        <- lapply(1:length(Z), function(ii,Z){
    XX       <- as.data.frame(Z[[ii]])
    XX$State <- ii
    XX$Age   <- 0:109
    XX
  }, Z = Z)
  # now stick it together
  D        <- do.call(rbind.data.frame, Z)
  # select age range
  D        <- subset(D, Age >= lower & Age < upper)
  # toggle to long
  
  DT       <- as.data.table(melt(D, 
                                 id.vars = list("State","Age"),
                                 variable.name = "AMCategory"))
  # replaces aggregate()
  DT       <- DT[, list(Contribution = sum(value)), by = list(State,AMCategory)]
  # add year
  DT$Year  <- 1989 + i
  DT
}

# now apply this function the the crazy list objects containing 
# decomposition results.

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

###### for ages 15 - 39 ##############################################################
#####################################################################################
DecomF15_39 <- local(get(load("Data/ContribFemales15_39.Rdata")))
DecomM15_39 <- local(get(load("Data/ContribMales15_39.Rdata")))
# then do the aggregate-reshape
DF15_39 <- do.call(rbind,
                   lapply(1:length(DecomF15_39), 
                          FUN = my_reshape.function, 
                          DecompIn = DecomF15_39, 
                          lower = 15, 
                          upper = 40))
DM15_39 <- do.call(rbind,
                   lapply(1:length(DecomM15_39), 
                          FUN = my_reshape.function, 
                          DecompIn = DecomM15_39, 
                          lower = 15, 
                          upper = 40))

###### for ages 40 - 74 ##############################################################
#####################################################################################
### For females
DecomF40_74 <- local(get(load("Data/ContribFemales40_74.Rdata")))
DecomM40_74 <- local(get(load("Data/ContribMales40_74.Rdata")))
DF40_74     <- do.call(rbind,
                       lapply(1:length(DecomF40_74), 
                              FUN = my_reshape.function, 
                              DecompIn = DecomF40_74, 
                              lower = 40, 
                              upper = 75))
DM40_74     <- do.call(rbind,
                       lapply(1:length(DecomM40_74), 
                              FUN = my_reshape.function, 
                              DecompIn = DecomM40_74, 
                              lower = 40, 
                              upper = 75))

## Data frame with results for males
gdata::keep(DF0_14,DM0_14,DF15_39,DM15_39,DF40_74,DM40_74,sure=TRUE)
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
DM0_14$AMLabel  <- factor(DM0_14$AMCategory, levels = c(1:10), labels = cause.labels)
DF0_14$AMLabel  <- factor(DF0_14$AMCategory, levels = c(1:10), labels = cause.labels)
DF15_39$AMLabel <- factor(DF15_39$AMCategory, levels = c(1:10), labels = cause.labels)	
DM15_39$AMLabel <- factor(DM15_39$AMCategory, levels = c(1:10), labels = cause.labels)	
DF40_74$AMLabel <- factor(DF40_74$AMCategory, levels = c(1:10), labels = cause.labels)
DM40_74$AMLabel <- factor(DM40_74$AMCategory, levels = c(1:10), labels = cause.labels)

# Age group codes
DM0_14$Ages  <- 1
DF0_14$Ages  <- 1
DF15_39$Ages <- 2
DM15_39$Ages <- 2
DF40_74$Ages <- 3
DM40_74$Ages <- 3

# Sex codes
DM0_14$Sex  <- 2
DF0_14$Sex  <- 1
DF15_39$Sex <- 1
DM15_39$Sex <- 2
DF40_74$Sex <- 1
DM40_74$Sex <- 2

Data <- rbind(DM0_14,DF0_14,DM15_39,DF15_39,DM40_74,DF40_74)
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
                      labels = c("0-14", "15-39", "40-74"))
Data$Sex    <- factor(Data$Sex, 
                      levels = c(1, 2), 
                      labels = c("Females", "Males"))

#### Distance data
# TR: how about we call it a 'gap'?
Data        <- data.table(Data)
Dist.data   <- Data[,sum(Contribution), by = list(Year, State, Ages, Sex)]
setnames(Dist.data,"V1","Distance")


ref.order   <- subset(Dist.data, Year==2015 & Sex == "Males" & Ages == "40-74")
ref.order   <- data.table(ref.order)
setnames(ref.order, "Distance", "Order")
# select State and Order columns
ref.order   <- ref.order[,c(2,5),with=FALSE]
head(ref.order)
save(ref.order, file= "Data/Order.Rdata")

# get names & regions for current data chunk
Dist.data$Statenom <- state.code.recvec[as.character(Dist.data$State)]
Dist.data$region   <- region.recvec[as.character(Data$State)] 

# select years, factor region
Dist.data          <- Dist.data[Year == 2005 | Year == 2010 | Year == 2015]
Dist.data          <- as.data.frame(Dist.data)
Dist.data$region   <- factor(Dist.data$region, 
                             levels = c(1, 2, 3), 
                             labels = c("South", "Central", "North"))


Dist.data          <- merge(Dist.data, ref.order, by = c("State"), all.y = TRUE)

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

main.label <- "Gap with benchmark survival (years)"


### one plot fpr the supplemental material
f1.y.data <- subset(Dist.data,Ages=="0-14")
f1.y <- useOuterStrips( dotplot(Statenom ~ Distance|Sex+region,aspect = c(0.9),ylab= list("State",cex=1),
                                #layout=c(3,6),
                                strip=TRUE,
                                # TR: changed to gap
                                xlab=list(main.label,cex=1),main=FALSE,cex=1,
                                data=f1.y.data, groups=Year,pch=my.pch, col=my.fill,col.line=col.line,lin=line,par.settings=my.settings,
                                #xlim=c(0,4),
                                scales=list(alternating=1,x=list(cex=1),
                                            y=list(relation="free")),
                                key=list(position="top",background="white",
                                         title="Year",text=list(txt.legend),points=list(pch=my.pch1,col=my.fill1,cex=1)),                       
                                panel=function(x,y,lin,col.line,...){    
                                  panel.abline(v=seq(0,.5,.1),lwd=1,lty=1,col="#A1A1A150")
                                  panel.dotplot(x,y,col.line="#A1A1A150",lwd=1,lty=1,...)                     
                                }),strip.left=T)
f1.y
pdf(file="Distance_y.pdf",width=8,height=11)
print(f1.y)
dev.off()

f1.ya.data <- subset(Dist.data,Ages=="15-39")
f1.ya <- useOuterStrips( dotplot(Statenom ~ Distance|Sex+region,aspect = c(0.9),ylab= list("State",cex=1),
                                 #layout=c(3,6),
                                 strip=T,
                                 xlab=list(main.label,cex=1),main=FALSE,cex=1,
                                 data=f1.ya.data, groups=Year,pch=my.pch, col=my.fill,col.line=col.line,lin=line,par.settings=my.settings,
                                 #xlim=c(0,4),
                                 scales=list(alternating=1,x=list(cex=1),
                                             y=list(relation="free")),
                                 key=list(position="top",background="white",
                                          title="Year",text=list(txt.legend),points=list(pch=my.pch1,col=my.fill1,cex=1)),                       
                                 panel=function(x,y,lin,col.line,...){    
                                   panel.abline(v=seq(0,2,.5),lwd=1,lty=1,col="#A1A1A150")
                                   panel.dotplot(x,y,col.line="#A1A1A150",lwd=1,lty=1,...)                     
                                 }),strip.left=T)
f1.ya
pdf(file="Distance_ya.pdf",width=8,height=11)
print(f1.ya)
dev.off()


f1.oa.data <- subset(Dist.data,Ages=="40-74")
f1.oa <- useOuterStrips( dotplot(Statenom ~ Distance|Sex+region,aspect = c(0.9),ylab= list("State",cex=1),
                                 #layout=c(3,6),
                                 strip=T,
                                 xlab=list(main.label,cex=1),main=FALSE,cex=1,
                                 data=f1.oa.data, groups=Year,pch=my.pch, col=my.fill,col.line=col.line,lin=line,par.settings=my.settings,
                                 #xlim=c(0,4),
                                 scales=list(alternating=1,x=list(cex=1),
                                             y=list(relation="free")),
                                 key=list(position="top",background="white",
                                          title="Year",text=list(txt.legend),points=list(pch=my.pch1,col=my.fill1,cex=1)),                       
                                 panel=function(x, y, lin, col.line,...){    
                                   #panel.abline(v=seq(.5,3.5,1),lwd=1,lty=3,col="darkgrey")
                                   panel.abline(v=seq(0,4,1),lwd=1,lty=1,col="#A1A1A150")
                                   #panel.abline(v=lin[[1]],col=col.line[[1]],lwd=1,lty=2)
                                   panel.dotplot(x,y,col.line="#A1A1A150",lwd=1,lty=1,...)                     
                                 }),strip.left=T)
f1.oa
pdf(file="Distance_oa.pdf",width=8,height=11)
print(f1.oa)
dev.off()





### one plot for the paper



fig.data <- subset(Dist.data,Sex=="Males" & Ages == "40-74")


# TR: changing x ticks and grids
f2 <- dotplot(Statenom ~ Distance | region,
              aspect = c(0.9),
              ylab = list("State", cex = 1.3), 
              strip.left = TRUE, 
              layout = c(1, 3), 
              strip = FALSE,
              xlab = list(main.label, cex=1.3),
              main = FALSE, 
              cex = 1,
              data = fig.data, 
              groups = Year, 
              pch = my.pch, 
              col = my.fill,
              col.line = col.line,
              lin = line,
              par.settings = my.settings,
              xlim = c(0,4),
              between = list(y = .5),
              scales = list(alternating = 1,
                            x = list(cex = 1, at = seq(0,4,1), labels = as.character(seq(0,4,1))),
                            y = list(relation = "free")),
              key=list(space = "right", 
                       background="white",
                       title = "Year", 
                       text = list(txt.legend), 
                       points = list(pch = my.pch1, col = my.fill1, cex = 1)),                       
              panel=function(x, y, lin, col.line,...){    
                #panel.abline(v=seq(.5,3.5,1),lwd=1,lty=3,col="darkgrey")
                panel.abline(v=seq(0,4,1),lwd=1,lty=1,col="#A1A1A150")
                #panel.abline(v=lin[[1]],col=col.line[[1]],lwd=1,lty=2)
                panel.dotplot(x,y,col.line="#A1A1A150",lwd=1,lty=1,...)                     
              })
f2







#1.Causes amenable to medical service
#2.Diabetes
#3.Ischemic heart diseases
#5.Lung cancer
#6.Cirrhosis
#7.Homicide
#8.Road traffic accidents
#9.Suicide
#10.Other causes
#11.HIV/AIDS


#### calculate proportions
Data <- data.table(Data)
my.prop <- function(c){
  z <- c/sum(c)
  z
}
Data <- Data[,Prop:= my.prop(Contribution), by = list(Year,State,Ages,Sex)]
# recode HIV
Data[AMCategory == 4]$AMCategory <- 11

f2.cause <- Data[Ages=="40-74" & Year==2015 & Sex== "Males"]



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
myColours1 <- c(toupper(base2), rep(gray(.7), 3))

fig.labels <- c("1. Causes amenable to medical service",
                "2. Diabetes",
                "3. Ischemic heart diseases",
                "4. Lung cancer",
                "5. Cirrhosis",
                "6. Homicide",
                "7. Road traffic accidents",
                "8. Other causes")

fig.labels <- c("1.Causes amenable to medical service",
                "2.Diabetes",
                "3.Ischemic heart diseases",
                "4.Lung cancer",
                "5.Cirrhosis",
                "6.Homicide",
                "7.Road traffic accidents",
                "8. Other causes")

my.settings1 <- list(
  superpose.polygon=list(col=myColours1[], border=gray(.5)),
  strip.background=list(col="grey"),
  strip.border=list(col="black")
)


f2.c           <- merge(f2.cause,ref.order, by = c("State"),all.y = T)
f2.c$Statenom  <- with(f2.c,reorder(reorder(Statenom,Order),as.numeric(region)))

# TR:
# assign Other to all causes under 10%
f2.c$AMCategory[f2.c$Prop < .1] <- 'g10'


# TR: would like to order causes descending within states.
# i.e. not always the same order. Then the message is to each state
# what they can work on.

axis.yout<- function(side, ...) { 
  if(side %in% c("left", "right")) {
    if (panel.number() %% 2 == which(c("right","left")==side)-1) {
      panel.axis(side = side, outside =TRUE)
    }
  } else {
    axis.default(side = side, ...)
  }
  
}

f2.c2 <-  barchart(Statenom ~ Prop |region, data=f2.c,
                   groups = AMCategory,  ylab="",xlab=list("Gap composition by cause of death in 2015",cex=1),
                   layout=c(1,3),strip.left=F,strip=F,
                   stack=TRUE,
                   between = list(y = .5),
                   scales=list(alternating=1,x=list(cex=1,at=seq(0,1,.25), 
                                                    labels=as.character(seq(0,1,.25))),
                               y=list(relation="free")),
                   par.settings=my.settings1,
                   key = list(space="right", title="Cause of death",background="white",
                              text=list(fig.labels)
                              ,cex=.9,
                              points=list(pch=19,col=myColours1[-c(9,10)])),
                   panel=function(x,y,lin,col.line,...){    
                     panel.barchart(x,y,...)                     
                     panel.abline(v=seq(0,1,.25),lwd=1,lty=2,col="darkgrey")
                   })
f2.c2




library(gridExtra)
library(grid)
blank<-rectGrob(gp=gpar(col="white"))

pdf(file="Figure 4.pdf",width=15,height=7,pointsize=12)
grid.arrange(f2,blank,f2.c2,ncol=3,widths=c(0.7,-.15, 0.9))
dev.off()



################ Supplemental figures 


sup.data <- Data[Year == 2005 | Year == 2010 | Year == 2015]

sup.data2 <- merge(sup.data,ref.order, by = c("State"),all.y = T)

sup.data2$Statenom<-with(sup.data2,reorder(reorder(Statenom,Order),as.numeric(region)))
sup.data2$Year <- factor(sup.data2$Year,levels=c(2005,2010,2015),labels=c("2005","2010","2015"))


#supplemental figure for young

prop.ym <-  useOuterStrips(barchart(Statenom ~ Prop |Year+region, data=subset(sup.data2,Ages=="0-14" & Sex=="Males"),
                                    groups=AMCategory,  ylab="",xlab=list("Proportion explained by cause of death",cex=1.3),
                                    stack=TRUE,main="Young Males",
                                    between = list(y = .5),
                                    scales=list(alternating=1,x=list(cex=1,at=seq(0,4,.5), 
                                                                     labels=as.character(seq(0,4,.5))),
                                                y=list(relation="free")),
                                    par.settings=my.settings1,
                                    key = list(space="bottom", title="Cause of death",background="white",
                                               text=list(fig.labels)
                                               ,cex=.9,
                                               points=list(pch=19,col=myColours1[-10]))),strip.left=T)
prop.ym


library(gridExtra)
library(grid)
blank<-rectGrob(gp=gpar(col="white"))

pdf(file="Figure 4.pdf",width=15,height=7,pointsize=12)
grid.arrange(f2,blank,f2.c2,ncol=3,widths=c(0.7,-.15, 0.9))

require(gridExtra)
pdf(file="Figure_prop_ym.pdf",width=14,height=10,pointsize=12)
prop.ym

dev.off()


prop.yf <-  useOuterStrips(barchart(Statenom ~ Prop |Year+region, data=subset(sup.data2,Ages=="0-14" & Sex=="Females"),
                                    groups=AMCategory,  ylab="",xlab=list("Proportion explained by cause of death",cex=1.3),
                                    stack=TRUE,main="Young Females",
                                    between = list(y = .5),
                                    scales=list(alternating=1,x=list(cex=1,at=seq(0,4,.5), 
                                                                     labels=as.character(seq(0,4,.5))),
                                                y=list(relation="free")),
                                    par.settings=my.settings1,
                                    key = list(space="bottom", title="Cause of death",background="white",
                                               text=list(fig.labels)
                                               ,cex=.9,
                                               points=list(pch=19,col=myColours1[-10]))),strip.left=T)
prop.yf


require(gridExtra)
pdf(file="Figure_prop_yf.pdf",width=14,height=10,pointsize=12)
prop.yf
dev.off()




prop.yam <-  useOuterStrips(barchart(Statenom ~ Prop |Year+region, data=subset(sup.data2,Ages=="15-39" & Sex=="Males"),
                                     groups=AMCategory,  ylab="",xlab=list("Proportion explained by cause of death",cex=1.3),
                                     stack=TRUE,main="Young adult males",
                                     between = list(y = .5),
                                     scales=list(alternating=1,x=list(cex=1,at=seq(0,4,.5), 
                                                                      labels=as.character(seq(0,4,.5))),
                                                 y=list(relation="free")),
                                     par.settings=my.settings1,
                                     key = list(space="bottom", title="Cause of death",background="white",
                                                text=list(fig.labels)
                                                ,cex=.9,
                                                points=list(pch=19,col=myColours1[-10]))),strip.left=T)
prop.yam


require(gridExtra)
pdf(file="Figure_prop_yam.pdf",width=14,height=10,pointsize=12)
prop.yam
dev.off()


prop.yaf <-  useOuterStrips(barchart(Statenom ~ Prop |Year+region, data=subset(sup.data2,Ages=="15-39" & Sex=="Females"),
                                     groups=AMCategory,  ylab="",xlab=list("Proportion explained by cause of death",cex=1.3),
                                     stack=TRUE,main="Young adult females",
                                     between = list(y = .5),
                                     scales=list(alternating=1,x=list(cex=1,at=seq(0,4,.5), 
                                                                      labels=as.character(seq(0,4,.5))),
                                                 y=list(relation="free")),
                                     par.settings=my.settings1,
                                     key = list(space="bottom", title="Cause of death",background="white",
                                                text=list(fig.labels)
                                                ,cex=.9,
                                                points=list(pch=19,col=myColours1[-10]))),strip.left=T)
prop.yaf


require(gridExtra)
pdf(file="Figure_prop_yaf.pdf",width=14,height=10,pointsize=12)
prop.yaf
dev.off()



prop.oam <-  useOuterStrips(barchart(Statenom ~ Prop |Year+region, data=subset(sup.data2,Ages=="40-74" & Sex=="Males"),
                                     groups=AMCategory,  ylab="",xlab=list("Proportion explained by cause of death",cex=1.3),
                                     stack=TRUE,main="Older adult males",
                                     between = list(y = .5),
                                     scales=list(alternating=1,x=list(cex=1,at=seq(0,4,.5), 
                                                                      labels=as.character(seq(0,4,.5))),
                                                 y=list(relation="free")),
                                     par.settings=my.settings1,
                                     key = list(space="bottom", title="Cause of death",background="white",
                                                text=list(fig.labels)
                                                ,cex=.9,
                                                points=list(pch=19,col=myColours1[-10]))),strip.left=T)
prop.oam


require(gridExtra)
pdf(file="Figure_prop_oam.pdf",width=14,height=10,pointsize=12)
prop.oam
dev.off()




prop.oaf <-  useOuterStrips(barchart(Statenom ~ Prop |Year+region, data=subset(sup.data2,Ages=="40-74" & Sex=="Females"),
                                     groups=AMCategory,  ylab="",xlab=list("Proportion explained by cause of death",cex=1.3),
                                     stack=TRUE,main="Older adult females",
                                     between = list(y = .5),
                                     scales=list(alternating=1,x=list(cex=1,at=seq(0,4,.5), 
                                                                      labels=as.character(seq(0,4,.5))),
                                                 y=list(relation="free")),
                                     par.settings=my.settings1,
                                     key = list(space="bottom", title="Cause of death",background="white",
                                                text=list(fig.labels)
                                                ,cex=.9,
                                                points=list(pch=19,col=myColours1[-10]))),strip.left=T)
prop.oaf


require(gridExtra)
pdf(file="Figure_prop_oaf.pdf",width=14,height=10,pointsize=12)
prop.oam
dev.off()










