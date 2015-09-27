# Author: tim
###############################################################################

source("R/colourtools.R")

Hexagon <- function (x, y, ...) {
	xcoords <- c(-sqrt(3)/2,-sqrt(3)/2,0,sqrt(3)/2,sqrt(3)/2,0)
	ycoords <- c(.5,-.5,-1,-.5,.5,1)
	polygon(xcoords+x,ycoords+y,...)
}

HexMex <- function(data,value.name = "e0",breaks = NULL, 
		ramp = colorRampPalette(RColorBrewer::brewer.pal(9,"OrRd"),space="Lab"),labels = TRUE,...){
	s3  <- sqrt(3)
	s32 <- s3/2
	
	# leave states in this code format, so it's easier to move hexagons around.
	# this is just a first draft.
	states <- data.frame(
			x=c(1-s32-s3,1-s32,1+s32,1+s32+s3,           # row 1
					1-s3,1,1+s3,1+2*s3,1+3*s3,           # row 2
					1+s32,1+s32+s3,1+s32+2*s3,           # row 3
					1+1*s3,1+2*s3,1+3*s3,                # row 4
					1+s32+s3,1+s32+2*s3,1+s32+3*s3,1+s32+4*s3,1+s32+7*s3,   # row 5
					1+2*s3,1+3*s3,1+4*s3,1+5*s3,1+6*s3,1+7*s3,1+8*s3,       # row 6
					1+s32+2*s3,1+s32+3*s3,1+s32+4*s3,1+s32+5*s3,1+s32+6*s3  # row 7
			),               
			y=c(8.5,8.5,8.5,8.5,                         # row 1
					7,7,7,7,7,                           # row 2
					5.5,5.5,5.5,                         # row 3
					4,4,4,                               # row 4
					2.5,2.5,2.5,2.5, 2.5,                # row 5
					1,1,1,1,1,1,1,                       # row 6
					-.5,-.5,-.5,-.5,-.5                  # row 7
			),                        
			name=c("BN","SO","CH","CA",                  # row 1
					"BS","SI","DU","NL","TM",            # row 2
					"NA","ZA","SL",                      # row 3
					"JA","AG","QE",                      # row 4
					"CL","GT","HI","VE", "YU",           # row 5
					"MC","MX","DF","TL","TB","CM","QR",  # row 6
					"GR","MR","PU","OA","CP"             # row 7
			
			),value=NA)
	# this just needed to be coded once.
	StatesLookup <- structure(list(Name = c("Aguascalientes", "Baja California", 
							"Baja California Sur", "Campeche", "Coahuila", "Colima", "Chiapas", 
							"Chihuahua", "Distrito Federal", "Durango", "Guanajuato", "Guerrero", 
							"Hidalgo", "Jalisco", "México", "Michoacán", "Morelos", "Nayarit", 
							"Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", 
							"San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
							"Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"), lettercode = c("AG", 
							"BN", "BS", "CM", "CA", "CL", "CP", "CH", "DF", "DU", "GT", "GR", 
							"HI", "JA", "MX", "MC", "MR", "NA", "NL", "OA", "PU", "QE", "QR", 
							"SL", "SI", "SO", "TB", "TM", "TL", "VE", "YU", "ZA"), code = 1:32), .Names = c("Name", 
					"lettercode", "code"), row.names = c("AG", "BN", "BS", "CM", 
					"CA", "CL", "CP", "CH", "DF", "DU", "GT", "GR", "HI", "JA", "MX", 
					"MC", "MR", "NA", "NL", "OA", "PU", "QE", "QR", "SL", "SI", "SO", 
					"TB", "TM", "TL", "VE", "YU", "ZA"), class = "data.frame")
	states$Name      <- StatesLookup[states$name,"Name"]
	states$code      <- StatesLookup[states$name,"code"]
	rownames(states) <- states$code
	# OK, now data can be matched.
# data <- as.data.frame(ste0_14)[ste0_14$Sex==1&ste0_14$Year == 2008,]

rownames(data) <- data$State
states$value   <- data[rownames(states),value.name]

if (is.null(breaks)){
	breaks <- pretty(states$value,n=10)
}
states$color <- as.character(cut(states$value,breaks=breaks,labels=ramp(length(breaks)-1)))
 
states$labcol<- ifelse(colorspace::hex2RGB(to.grey(states$color))@coords[, 1] < .5,gray(.9),gray(.3))

	plot(NULL, type = "n", xlim = c(-3,16), ylim = c(-2,10),asp=1)
	for (i in 1:nrow(states)){
		Hexagon(states$x[i],states$y[i],col = states$color[i], border = "white")
		if (labels){
			text(states$x[i],states$y[i],states$name[i],col=states$labcol[i])	
		}
		#
	}
}

labels <- TRUE

states$Name <- StatesLookup[states$name,"Name"]
states$code <-StatesLookup[states$name,"code"]


if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/DecompMex/DecompMex")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/DecompMex/DecompMex"))
}

library(data.table)
library(reshape2)
source("R/Functions.R")
source("R/LTuniform.R")
# load in data made in DataPrep.R

dput(StatesLookup)
rownames(StatesLookup)<- StatesLookup$lettercode
StatesLookup <- structure(list(Name = c("Aguascalientes", "Baja California", 
						"Baja California Sur", "Campeche", "Coahuila", "Colima", "Chiapas", 
						"Chihuahua", "Distrito Federal", "Durango", "Guanajuato", "Guerrero", 
						"Hidalgo", "Jalisco", "México"   , "Michoacán"   , "Morelos", 
						"Nayarit", "Nuevo León"   , "Oaxaca", "Puebla", "Querétaro"   , 
						"Quintana Roo", "San Luis Potosí"   , "Sinaloa", "Sonora", "Tabasco", 
						"Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán"   , "Zacatecas"
				), lettercode = c("AG", "BN", "BS", "CM", "CA", "CL", "CP", "CH", 
						"DF", "DU", "GT", "GR", "HI", "JA", "MX", "MC", "MR", "NA", "NL", 
						"OA", "PU", "QE", "QR", "SL", "SI", "SO", "TB", "TM", "TL", "VE", 
						"YU", "ZA"), code = 1:32), .Names = c("Name", "lettercode", "code"
		), row.names = c(NA, -32L), class = "data.frame")

source("R/1_CalculateBPe0etc.R")

ste0_14

data <- ste
