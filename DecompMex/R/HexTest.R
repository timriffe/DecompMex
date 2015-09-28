# Author: tim
###############################################################################

source("R/colourtools.R")

Hexagon <- function (x, y, ...) {
	xcoords <- c(-sqrt(3)/2,-sqrt(3)/2,0,sqrt(3)/2,sqrt(3)/2,0)
	ycoords <- c(.5,-.5,-1,-.5,.5,1)
	polygon(xcoords+x,ycoords+y,...)
}


ramp <- colorRampPalette(rev(RColorBrewer::brewer.pal(9,"OrRd")),space="Lab")

# matches using numeric codes. data must be a single year/sex, such that each state
# is only present once, and we use state numeric codes 1:32 to match to hexagons.

HexMex <- function(data,value.name = "e0",version = 3, breaks = NULL, 
		ramp = colorRampPalette(RColorBrewer::brewer.pal(9,"OrRd"),space="Lab"),
		labels = TRUE,add=FALSE,...){
	s3  <- sqrt(3)
	s32 <- s3/2
	
	# leave states in this code format, so it's easier to move hexagons around.
	# this is just a first draft.
	if (version == 1){
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
	}
	if (version == 2){
		states <- data.frame(
				x=c(0:3 * s3,              # row 1
						0:4*s3+s32,           # row 2
						1:5*s3,7:8*s3,    # row 3
						1:7*s3+s32,    # row 4
						2:7*s3, # row 5
						3:5*s3+s32         # row 6
				),               
				y=c(    rep(7.5,4),                            # row 1
						rep(6,5),                              # row 2
						rep(4.5,7),                            # row 3
						rep(3,7),	                           # row 4
						rep(1.5,6),	                           # row 5
						rep(0,3)                               # row 6
				),                        
				name=c("BN","SO","CH","CA",                             # row 1
						"BS","SI","DU","NL","TM",                      # row 2
						"NA","ZA","SL","HI","VE",    "YU","QR",      # row 3
						"JA","AG","GT","QE","TL","TB","CM",        # row 4
						"CL","MX","DF","MR","PU","CP",           # row 5
						"MC","GR","OA"                         # row 6
				),value=NA)
		
	}
	if (version == 3){
		states <- data.frame(
				x=c(0:3 * s3,              # row 1
						-1:3*s3+s32,           # row 2
						0:4*s3,7:8*s3,    # row 3
						0:7*s3+s32,    # row 4
						2:7*s3, # row 5
						3:4*s3+s32         # row 6
				),               
				y=c(    rep(7.5,4),                            # row 1
						rep(6,5),                              # row 2
						rep(4.5,7),                            # row 3
						rep(3,8),	                           # row 4
						rep(1.5,6),	                           # row 5
						rep(0,2)                               # row 6
				),                        
				name=c("BCN","SON","CHH","COA",                             # row 1
						"BCS","SIN","DUR","NL","TAM",                      # row 2
						"NAY","ZAC","SLP","QUE","HID",     "YUC","QROO",      # row 3
						"JAL","AGU","GUA","DF","TLA","VER","TAB","CAM",        # row 4
						"COL","MEX","MOR","PUE","OAX","CHP",           # row 5
						       "MIC","GRO"                         # row 6
				),value=NA)
		
	}
	
	
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
							"SL", "SI", "SO", "TB", "TM", "TL", "VE", "YU", "ZA"), code = 1:32, 
					Name3 = c("AGU", "BCN", "BCS", "CAM",  "COA", "COL","CHP",
							"CHH", "DF", "DUR", "GUA", "GRO", "HID", "JAL", "MEX", "MIC", 
							"MOR", "NAY", "NL", "OAX", "PUE", "QUE", "QROO", "SLP", "SIN", 
							"SON", "TAB", "TAM", "TLA", "VER", "YUC", "ZAC")), .Names = c("Name", 
					"lettercode", "code", "Name3"), row.names = c("AGU", "BCN", "BCS", "CAM",  "COA", "COL","CHP",
					"CHH", "DF", "DUR", "GUA", "GRO", "HID", "JAL", "MEX", "MIC", 
					"MOR", "NAY", "NL", "OAX", "PUE", "QUE", "QROO", "SLP", "SIN", 
					"SON", "TAB", "TAM", "TLA", "VER", "YUC", "ZAC"), class = "data.frame")
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
	if (!add){
		xlim <- range(states$x) + c(-.5,.5)
		ylim <- range(states$y) + c(-.5,.5)
		plot(NULL, type = "n", xlim = xlim, ylim = ylim,asp=1)
	}
	
	# this avoids errors if per chance we only want the states object back.
	if (length(dev.list()) > 0){
		for (i in 1:nrow(states)){
			Hexagon(states$x[i],states$y[i],col = states$color[i], border = "white")
			if (labels){
				text(states$x[i],states$y[i],states$name[i],col=states$labcol[i])	
			}
			# labels = TRUE
		}
	}
	invisible(states)
}





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


source("R/1_CalculateBPe0etc.R")

ste0_14

data <- as.data.frame(ste40_74)[ste40_74$Sex==1&ste40_74$Year == 2008,]
rownames(data) <- data$State


HexMex(data, version =1)
dev.new()
HexMex(data, version =2)
HexMex(data, version =3)