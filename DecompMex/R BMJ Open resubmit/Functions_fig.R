# some functions from JM
makeTransparent<-function(someColor, alpha=100) {
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}
my.settings <- list(
  strip.background=list(col="grey"),
  strip.border=list(col="black")
)


#State names
Statenames <- NULL
Statenames$ID <- 1:32
name <- c("Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila","Colima","Chiapas",
          "Chihuahua","Mexico City","Durango","Guanajuato","Guerrero","Hidalgo","Jalisco","Mexico","Michoacan",
          "Morelos","Nayarit","Nuevo Leon","Oaxaca","Puebla","Queretaro","Quintana Roo","San Luis Potosi","Sinaloa",
          "Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatan","Zacatecas")
Statenames$name <- name
Statenames  <- as.data.frame(Statenames)
