
me <- system("hostname",intern=TRUE)
if (me == "ADM-108625") {
	setwd("C:/Users/jmaburto/Documents/GitHub/DecompMex/DecompMex")
} else {
	if (me %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
		# if I'm on the laptop
		setwd("/home/tim/git/DecompMex/DecompMex")
	} else {
		# in that case I'm on Berkeley system, and other people in the dept can run this too
		setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/DecompMex/DecompMex"))
	}}

library(reshape2)
library(latticeExtra)
library(data.table)
library(RColorBrewer)
library(plotrix)

load("Data/Temp_e0_results_smooth.RData")
source("R/Functions_fig.R")
source("R/Color.R")

### standardize temporary life expectancy with the maximum survival
dat <- temp.data[state < 33]

# single out just needed e0 year ranges

dat <- dat[dat$year >= 2010 & dat$state <= 32, ]
# get avg for 6 years starting 2010
dat <- data.table(dat)
dat <- dat[, list(mean_temp_e0 = mean(temp_e0)), by=list(sex,state,age.g)]

# get regions
state.code.recvec <- 
		c("Aguascalientes","Baja California","Baja California Sur","Campeche",
				"Coahuila","Colima","Chiapas","Chihuahua","Mexico City","Durango",
				"Guanajuato","Guerrero","Hidalgo","Jalisco","Mexico State","Michoacan",
				"Morelos","Nayarit","Nuevo Leon","Oaxaca","Puebla","Queretaro",
				"Quintana Roo","San Luis Potosi","Sinaloa","Sonora","Tabasco","Tamaulipas",
				"Tlaxcala","Veracruz","Yucatan","Zacatecas")
names(state.code.recvec) <- 1:32

state.abbrev.recvec <- c("AG", 
				"BN", "BS", "CM", "CA", "CL", "CP", "CH", "DF", "DU", "GT", "GR", 
				"HI", "JA", "MX", "MC", "MR", "NA", "NL", "OA", "PU", "QE", "QR", 
				"SL", "SI", "SO", "TB", "TM", "TL", "VE", "YU", "ZA")
names(state.abbrev.recvec)<- 1:32

region.recvec            <- c(2,3,3,1,3,2,1,3,2,3,2,1,2,2,2,
		2,1,2,3,1,1,2,1,3,3,3,1,3,2,1,1,3)
names(region.recvec)     <- 1:32
# order based on left column, which will have state labels. This refers
# to young age group, ergo, we need within
males        <- dat[dat$sex == 1]
females      <- dat[dat$sex == 2]

youngmales   <- males[males$age.g == 1, ]
youngmales   <- youngmales[order(youngmales$mean_temp_e0), ]
youngfemales <- females[females$age.g == 1, ]
youngfemales <- youngfemales[order(youngfemales$mean_temp_e0), ]

# this matrix is no longer ordered.
m.mat   		  <- acast(males, age.g ~ state, value.var = "mean_temp_e0")
f.mat   		  <- acast(females, age.g ~ state, value.var = "mean_temp_e0")


# how about state labels proper?
#colnames(m.mat)   <- state.code.recvec[colnames(m.mat)]
#colnames(f.mat)   <- state.code.recvec[colnames(f.mat)]

m.rank            <- apply(m.mat,1,rank)
f.rank            <- apply(f.mat,1,rank)

m.rank            <- m.rank[order(m.rank[,1]),]
f.rank            <- f.rank[order(f.rank[,1]),]

y.f               <- f.rank - 1
y.m               <- m.rank - 1

Nrank             <- nrow(y.f)
# ys               <- abs(Nrank - rankmat)
laby              <- ys[1,]
#ys[mat < 1e-5] <- NA
xs                <- 1:3

labels            <- state.code.recvec[rownames(y.m)]
codes             <- state.abbrev.recvec[rownames(y.m)]

codewpar          <- paste0(labels," (", codes,")")

deduct            <- y.m * 0
deduct[,1]        <- .08

Purp4 <- RColorBrewer::brewer.pal(9,"Purples")[c(3,5,7,9)]
Gree4 <- RColorBrewer::brewer.pal(9,"Greens")[c(3,5,7,9)]

GS <- c("Sinaloa","Nayarit","Michoacan","Morelos","Zacatecas","Guerrero")
PS <- c("Yucatan","Hidalgo","Queretaro","Mexico City","Puebla")

colors <- rep(gray(seq(.8,.3,length=4)),8)
names(colors) <- labels

ivec <- 1:32 %% 4 
i1 <- ivec == 1
i2 <- ivec == 2
i3 <- ivec == 3
i4 <- ivec == 0
colors[i1 & labels %in% GS] <- Gree4[1]
colors[i2 & labels %in% GS] <- Gree4[2]
colors[i3 & labels %in% GS] <- Gree4[3]
colors[i4 & labels %in% GS] <- Gree4[4]
colors[i1 & labels %in% PS] <- Purp4[1]
colors[i2 & labels %in% PS] <- Purp4[2]
colors[i3 & labels %in% PS] <- Purp4[3]
colors[i4 & labels %in% PS] <- Purp4[4]



#show.pal(Gree4,F)
#graphics.off()
#dev.new(width=6,height=8)

# male rank plot (main text Figure 3)
pdf("BMJ Open Revise and Resubmit/Figures/Figure_3.pdf",width=6,height=8)

par(mai=c(.2,1.8,.2,.7))
plot(NULL, 
		type = "n", 
		xlim = range(xs), 
		ylim = c(0, Nrank + 1), 
		axes = FALSE,
		xlab = "",
		ylab = "",
		panel.first=
				list(
						rect(rep(.8,16),seq(1,31,by=2)-.5,rep(3.4,16),seq(2,32,by=2)-.5,
								col=gray(.92),border=NA,xpd=TRUE))
)

# archaic way of making sure thin lines render on top of fat ones
matplot(t(y.m[i1,]) ,type='l',lty=1,ylab="",xlab="",col=colors[i1],lwd=4,axes=FALSE,add=TRUE)
matplot(t(y.m[i2,]) ,type='l',lty=1,ylab="",xlab="",col=colors[i2],lwd=3,axes=FALSE,add=TRUE)
matplot(t(y.m[i3,]) ,type='l',lty=1,ylab="",xlab="",col=colors[i3],lwd=2,axes=FALSE,add=TRUE)
matplot(t(y.m[i4,]) ,type='l',lty=1,ylab="",xlab="",col=colors[i4],lwd=1,axes=FALSE,add=TRUE)

boxed.labels(col(y.m)-deduct,y.m,codes,cex=.8,border=FALSE)
text(.85,y.m[,1],labels[rownames(y.m)],pos=2,xpd=TRUE)
text(3.3,0:31,32:1,xpd=TRUE,cex=.8)
text(1:3,33.5,c("Young (0-14)","Middle (15-49)","Older (50-84)"),xpd=TRUE)
text(3.3,32.4,"Rank",xpd=TRUE)
dev.off()

# females rank plot (additional file)
pdf("BMJ Open Revise and Resubmit/Additional File/Figures/RankFemales.pdf",width=6,height=8)
par(mai=c(.2,1.8,.2,.7))
plot(NULL, 
		type = "n", 
		xlim = range(xs), 
		ylim = c(0, Nrank + 1), 
		axes = FALSE,
		xlab = "",
		ylab = "",
		panel.first=
				list(
						rect(rep(.8,16),seq(1,31,by=2)-.5,rep(3.4,16),seq(2,32,by=2)-.5,
								col=gray(.92),border=NA,xpd=TRUE))
)

#matplot(t(y.f) ,type='l',lty=1,ylab="",xlab="",col=gray(.2),lwd=1.2,axes=FALSE,add=TRUE)
matplot(t(y.f[i1,]) ,type='l',lty=1,ylab="",xlab="",col=gray(.6),lwd=3,axes=FALSE,add=TRUE)
matplot(t(y.f[i2,]) ,type='l',lty=1,ylab="",xlab="",col=gray(.4),lwd=2,axes=FALSE,add=TRUE)
matplot(t(y.f[i3,]) ,type='l',lty=1,ylab="",xlab="",col=gray(.2),lwd=1.5,axes=FALSE,add=TRUE)
matplot(t(y.f[i4,]) ,type='l',lty=1,ylab="",xlab="",col=gray(0),lwd=1,axes=FALSE,add=TRUE)


boxed.labels(col(y.f)-deduct,y.f,codes,cex=.8,border=FALSE)
text(.85,y.f[,1],state.code.recvec[rownames(y.f)],pos=2,xpd=TRUE)
text(3.3,0:31,32:1,xpd=TRUE,cex=.8)
text(1:3,33.5,c("Young (0-14)","Middle (15-49)","Older (50-84)"),xpd=TRUE)
text(3.3,32.4,"Rank",xpd=TRUE)
dev.off()
