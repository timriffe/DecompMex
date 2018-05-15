
# Author: tim
###############################################################################

# some color functions ripped from a once-off data viz that Tim did.
# these help equalize colors in grayscale and optimize palettes for
# that space.

library(colorspace)
library(spatstat)

show.pal <- function(colorvec, square = TRUE){
	n <- length(colorvec)
	if (square){
		nside   <- ceiling(sqrt(n))
		colorvec <- c(colorvec, rep(NA,nside^2 - n))
		colsmat <- matrix(colorvec,nrow=nside,byrow=TRUE)
		plot(0:nside,0:nside,type= "n",asp=1,axes=FALSE,xlab="",ylab="")
		rect(row(colsmat)-1,
				col(colsmat)-1,
				row(colsmat),
				col(colsmat),
				col = colsmat,
				border = "white", asp = 1)
	} else {
		plot(0:n,type= "n",ylim=c(0,1),xlim=c(0,n),axes=FALSE,xlab="",ylab="")
		rect(0:(n-1),0,1:n,1,col=colorvec,border="white")
	}
}

# darken by changing luminance
darkenhex <- function(hexcols,fac=.3){
	# from @Roland:
	#https://stackoverflow.com/questions/30219738/is-there-a-way-to-programmatically-darken-the-color-given-rgb-values
	require(colorspace)
	zz   <- textConnection(hexcols)
	cols <- readhex(file = zz,	class = "RGB")
	close(zz)
	# transform to hue/lightness/saturation colorspace
	cols <- as(cols, "HLS")
	cols@coords[, "L"] <- pmax(0, cols@coords[, "L"] - fac)
	cols <- as(cols, "RGB")
	hex(cols)
}

# a gives a number from 0 to 1. 0 is black, 1 is white. vectorized
howdark <- function(col){
	suppressMessages(colorspace::hex2RGB(spatstat::to.grey(col))@coords[, 1])
}

howsat <- function(colvec){
	rgb2hsv(col2rgb(colvec))[2, ]
}
howdark2 <- function(colvec){
	rgb2hsv(col2rgb(colvec))[3, ]
}

# satdarkmin <- function(pars, h, starget = .5, darktarget = .5){
# 	col <- hsv(h, pars["s"], pars["v"])
# 	dk  <- howdark(col)
# 	st  <- howsat(col)
# 	sqrt(((st-starget)^2+(dk-darktarget)^2)/2)
# }
eqspace2 <- function(colvec,dfrom,dto,sfrom,sto,show=TRUE){
	darkvals   <- howdark(colvec)
	colvec     <- colvec[order(darkvals)]
	darkvals   <- sort(darkvals)
	n          <- length(darkvals)
	targetdark <- darkvals - lm(darkvals~I(1:n))$res
	
	if (missing(dfrom)){
		dfrom  <- targetdark[1]
	}
	if (missing(dto)){
		dto    <- targetdark[n]
	}
	if (missing(sfrom)){
		sfrom  <- .8
	}
	if (missing(sto)){
		sto    <- .6
	}
	
	
	if (dfrom < 0){
		dfrom <- .01
	}
	if (dto > 1){
		dto <- .99
	}
	
	targets    <- seq(sfrom, sto, length = n)
	targetdark <- seq(dfrom, dto, length = n)
	colvec2 <- colvec
	for (i in 1:n){
		col1  <- suppressMessages(darken.to(colvec[i], targetdark[i]))
		col2 <- to.saturated(colvec[i],targets[i])
		proposal1 <- blend(col1,col2)
		col1  <- suppressMessages(darken.to(proposal1, targetdark[i]))
		col2 <- to.saturated(proposal1,targets[i])
		colvec2[i] <- blend(col1,col2)
	}
	colvec2 <- colvec2[colvec2 != "#000000"]
	
	if (show){
		show.pal(colvec2)
	}
	colvec2
}

blend <- function(col1, col2){
	rgb1   <- col2rgb(col1)
	rgb2   <- col2rgb(col2)
	rgbnew <- sqrt((rgb1^2+rgb2^2)/2)
	rgb2hex(c(rgbnew))
}


# darkens a color to a specific grayscale target darkness. not vectorized
darken.to <- function(hexcol,target=.2){
	require(colortools)
	require(spatstat)
	
	# a function to optimize the ideal darkening factor
	mintarg <- function(fac,col,target){
		hexcol <- darkenhex(hexcol,fac)
		abs(target - howdark(hexcol))
	}
	# get darkening factor
	fac    <- optimize(mintarg,c(-.8,.8),col=hexcol,target=target)$minimum
	# now darken by that much...
	colout <- darkenhex(hexcol,fac)
	colout
}

#to.saturated(colvec2, s=seq(.9,.6,length=n))

# will use this for surface ramps.
my.palette <- function(col,n,from=.2,to=.9){
	fromcol <- darken.to(col,from)
	tocol   <- darken.to(col,to)
	colorRampPalette(c(fromcol,tocol),space="Lab")(n)
}

# we can sort colors by darkness, but what if we want them in equally spaced
# dark levels, with minimal purturbation. Here we go!
eqspace <- function(colvec,from,to,show=TRUE,sateq=TRUE){
	darkvals   <- howdark(colvec)
	colvec     <- colvec[order(darkvals)]
	darkvals   <- sort(darkvals)
	n          <- length(darkvals)
	targetdark <- darkvals - lm(darkvals~I(1:n))$res
	
	if (missing(from)){
		from <- targetdark[1]
	}
	if (missing(to)){
		to <- targetdark[n]
	}
	if (from < 0){
		from <- .01
	}
	if (to > 1){
		to <- .99
	}
	targetdark <- seq(from, to, length = n)
	colvec2 <- colvec
	for (i in 1:length(colvec)){
		colvec2[i] <- suppressMessages(darken.to(colvec[i], targetdark[i]))
	}
	
	colvec2 <- colvec2[colvec2 != "#000000"]
	
	
	if (show){
		show.pal(colvec2)
		#	show.pal(to.saturated(colvec2, s=seq(.9,.5,length=n)))
	}
	
	colvec2
}
