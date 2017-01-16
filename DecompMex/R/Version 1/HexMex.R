
# Author: tim



###############################################################################
# file copied from https://raw.githubusercontent.com/spatstat/spatstat/master/R/colourtools.R
# couldn't install package due to R version...

#
#  colourtools.R
#
#   $Revision: 1.17 $   $Date: 2015/07/08 10:23:36 $
#
dependencies <- TRUE

if (dependencies){

rgb2hex <- function(v, maxColorValue=255) {
	stopifnot(is.numeric(v))
	if(!is.matrix(v))
		v <- matrix(v, nrow=1)
	if(ncol(v) %in% c(3, 4)) {
		out <- rgb(v, maxColorValue=maxColorValue)
		return(out)
	} 
	stop("v should be a vector of length 3 or 4, or a matrix with 3 or 4 columns")
}

rgb2hsva <- function(red, green=NULL, blue=NULL, alpha=NULL,
		maxColorValue=255) {
	if(is.null(green) && is.null(blue) && is.null(alpha)) {
		## red should be a 3-row matrix of RGB values
		## or a 4-row matrix of RGBA values 
		if(!is.matrix(red))
			red <- matrix(red, ncol=1)
		## check for an alpha channel
		if(nrow(red) == 4) {
			alpha <- red[4,]
			red <- red[-4, , drop=FALSE]
		}
	}
	y <- rgb2hsv(red, green, blue, maxColorValue=maxColorValue)
	if(!is.null(alpha))
		y <- rbind(y, alpha=alpha/maxColorValue)
	return(y)
}

col2hex <- function(x) {
	# convert to RGBA
	y <- col2rgb(x, alpha=TRUE)
	# remove alpha channel if all colours are opaque
	if(all(y["alpha", ] == 255))
		y <- y[1:3, , drop=FALSE]
	# convert to hex 
	z <- rgb2hex(t(y))
	return(z)
}

paletteindex <- function(x) {
	x <- col2hex(x)
	p <- col2hex(palette())
	m <- match(x, p)
	return(m)
}

is.colour <- function(x) {
	if(length(x) == 0) return(FALSE)
	cx <- try(col2rgb(x), silent=TRUE)
	bad <- inherits(cx, "try-error")
	return(!bad)
}

samecolour <- function(x, y) { col2hex(x) == col2hex(y) }

complementarycolour <- function(x) {
	if(is.null(x)) return(NULL)
	if(inherits(x, "colourmap")) {
		colouroutputs(x) <- complementarycolour(colouroutputs(x))
		return(x)
	}
	# convert to RGBA
	y <- col2rgb(x, alpha=TRUE)
	# complement of R, G, B
	y[1:3, ] <- 255 - y[1:3, ]
	# convert to colours
	z <- rgb2hex(t(y))
	return(z)
}

is.grey <- function(x) {
	if(inherits(x, "colourmap")) x <- colouroutputs(x)
	if(is.function(x)) return(NA)
	y <- rgb2hsva(col2rgb(x, alpha=TRUE))
	sat <- y["s", ]
	alp <- y["alpha", ]
	return(sat == 0 & alp == 1)
}

to.opaque <- function(x) {
	if(all(!is.na(paletteindex(x))))
		return(x) # preserve palette colours
	rgb(t(col2rgb(x)), maxColorValue=255)
}

to.transparent <- function(x, fraction) {
	if(all(fraction == 1))
		return(to.opaque(x))
	rgb(t(col2rgb(x))/255, alpha=fraction, maxColorValue=1)
}

to.grey <- function(x, weights=c(0.299, 0.587, 0.114), transparent=FALSE) {
	if(is.null(x)) return(NULL)
	if(inherits(x, "colourmap")) {
		colouroutputs(x) <- to.grey(colouroutputs(x),
				weights=weights, transparent=transparent)
		return(x)
	}
	if(is.function(x)) {
		f <- x
		g <- function(...) to.grey(f(...), weights=weights, transparent=transparent)
		return(g)
	}
	## preserve palette indices, if only using black/grey
	if(all(!is.na(paletteindex(x))) && all(is.grey(x)))
		return(x)
	if(!transparent) {
		y <- col2rgb(x)
		g <- (weights %*% y)/(255 * sum(weights))
		z <- grey(g)
	} else {
		yy <- col2rgb(x, alpha=TRUE)
		y <- yy[1:3, , drop=FALSE]
		g <- (weights %*% y)/(255 * sum(weights))
		z <- grey(g, alpha=y[4,])
	}
	return(z)
}

is.col.argname <- function(x) {
	return(nzchar(x) & ((x == "col") | (substr(x, 1, 4) == "col.")))
}

col.args.to.grey <- function(x, ...) {
	if(any(hit <- is.col.argname(names(x))))
		x[hit] <- lapply(x[hit], to.grey, ...)
	return(x)
}

# versions of rgb() and hsv() that work with NA values

rgbNA <- function(red, green, blue, alpha=NULL, maxColorValue=1) {
	df <- if(is.null(alpha)) data.frame(red=red, green=green, blue=blue) else
				data.frame(red=red, green=green, blue=blue, alpha=alpha)
	result <- rep(NA_character_, nrow(df))
	ok <- complete.cases(df)
	result[ok] <- if(is.null(alpha)) {
				with(df, rgb(red[ok], green[ok], blue[ok],
								maxColorValue=maxColorValue))
			} else {
				with(df, rgb(red[ok], green[ok], blue[ok], alpha[ok],
								maxColorValue=maxColorValue))
			}
	return(result)
}

hsvNA <- function(h, s, v, alpha=NULL) {
	df <- if(is.null(alpha)) data.frame(h=h, s=s, v=v) else
				data.frame(h=h, s=s, v=v, alpha=alpha)
	result <- rep(NA_character_, nrow(df))
	ok <- complete.cases(df)
	result[ok] <- if(is.null(alpha)) {
				with(df, hsv(h[ok], s[ok], v[ok]))
			} else {  
				with(df, hsv(h[ok], s[ok], v[ok], alpha[ok]))
			}
	return(result)
}
}
################################################3
# author: Tim Riffe
# here begin the custom functions for the hex map of Mexico

#' @title draw a regular hexagon, pointy side up
#' @description a non-scalable hexagon drawer
#' 
#' @param x center x coord
#' @param y center y coord
#' @param ... arguments passed to \code{polygon()}
#' 
#' @export

Hexagon <- function (x, y, ...) {
	xcoords <- c(-sqrt(3)/2,-sqrt(3)/2,0,sqrt(3)/2,sqrt(3)/2,0)
	ycoords <- c(.5,-.5,-1,-.5,.5,1)
	polygon(xcoords+x,ycoords+y,...)
}

#' @title draw an equal area hexagon map of Mexico
#' @description a non-scalable map. This isn't yet flexible. Function returns the modified states object with colors, label colors, etc, as well as coordinates. This object can be modified by the user to position the map in a particular place in a device, by shifting x and y, etc.
#' 
#' @param data \code{data.frame} requires \code{State} column with numeric code, as well as a column with a numeric value to plot
#' @param value.name name of numeric column to plot (\code{character}).
#' @param breaks value breaks. Detected using \code{pretty()} if not given. Best to provide these if making maps to compare
#' @param ramp a color ramp function
#' @param labels logical. should state labels be drawn? 
#' @param add logical. Should the plot be added to a device (\code{TRUE}), or a new plot drawn?
#' 
#' @value states a \code{data.frame} has various ID columns, the value to be plotted, colors for the choropleth, as well as label colors.
#' 
#' @export

HexMex <- function(data, value.name = "e0", breaks = NULL, 
		ramp = colorRampPalette(RColorBrewer::brewer.pal(9,"OrRd"),space="Lab"),
		labels = TRUE,add=FALSE){
	s3  <- sqrt(3)
	s32 <- s3/2

		states <- data.frame(
				x=c(0:3 * s3,              							# row 1
						-1:3*s3+s32,           						# row 2
						0:4*s3,7:8*s3,    							# row 3
						0:7*s3+s32,    								# row 4
						2:7*s3,                                    	# row 5
						3:4*s3+s32         							# row 6
				),               
				y=c(    rep(7.5,4),                            		# row 1
						rep(6,5),                              		# row 2
						rep(4.5,7),                            		# row 3
						rep(3,8),	                           		# row 4
						rep(1.5,6),	                           		# row 5
						rep(0,2)                               		# row 6
				),                        
				name=c("BCN","SON","CHH","COA",                             # row 1
						"BCS","SIN","DUR","NL","TAM",                      	# row 2
						"NAY","ZAC","SLP","QUE","HID",     "YUC","QROO",    # row 3
						"JAL","AGU","GUA","DF","TLA","VER","TAB","CAM",     # row 4
						"COL","MEX","MOR","PUE","OAX","CHP",           		# row 5
						"MIC","GRO"                         				# row 6
				), value = NA)
		
	
	
	
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
	
	# now attach data to object with coordinates:
	rownames(data) <- data$State
	states$value   <- data[rownames(states),value.name]
	
	if (is.null(breaks)){
		breaks <- pretty(states$value, n = 10)
	}
	states$color <- as.character(cut(states$value, breaks = breaks, labels = ramp(length(breaks) - 1)))
	
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


