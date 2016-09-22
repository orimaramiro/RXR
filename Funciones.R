mi_suma <- function(a, b) {
  suma <- a + b
  return(suma)
}
mi_suma(a= 5, b=4)
kelvin_a_fahr <- function(temp) {
  fahr <- (temp - 273.15) * (9/5) + 32
  return(fahr)
}

celsius_a_kelvin <- function(temp){
  kelvin <- temp + 273.15
  return(kelvin)
}
temp = 15
kelvin_a_fahr(temp)
celsius_a_kelvin(450)
celsius_a_Fahr = function (temp) {
  CelFahr= (temp) * (9/5) +32
  return(CelFahr)
}
Cel_a_Fahr = function (temp){
  primera= celsius_a_kelvin(temp)
  segunda= kelvin_a_fahr (primera)
  return(segunda)
} 
Cel_a_Fahr (20)
gapminder
head (gapminder)

calPBI= function (dat, Año = NULL, País,)
  if(!is.null(Año)){
              data = dat
  }
calcPBI <- function(dat, Año=NULL, País=NULL) {
  if(!is.null(Año)) {
    dat <- dat[dat$Año %in% Año, ]
  }
  if (!is.null(País)) {
    dat <- dat[dat$País %in% País,]
  }
  gdp <- dat$Pobación * dat$PBI
  new <- cbind(dat, gdp = gdp)
  return(new)
}
new  
head (calcPBI)
calcPBI(head(gapminder))
head(calcPBI(gapminder, Año = 2007))
head(calcPBI(gapminder, País = "Argentina"))
head(calcPBI(gapminder, País = "New Zealand", Año = 1987))
head(calcPBI(gapminder, País = "New Zealand", Año = c(1952,1987)))
vallar(salio)
vallar salio

#' Title
#'
#' @param texto 
#' @param envoltura 
#'
#' @return
#' @export
#'
#' @examples
vallar= function(texto, envoltura = "***") {
     
     texto = paste0(envoltura, paste(texto, collapse =" "),envoltura)
     return (texto)}
vallar(mejores_practicas)
vallar(mejores_practicas)
library(ggplot2)
pdf("Life_Exp_vs_time.pdf", width=12, height=12, onefile = TRUE)
print(ggplot(data=gapminder, aes(x=Año, y=Expect, colour=País)) +
  geom_line()+
  facet_grid(continente ~ .)
dev.off()
pdf("Mi_grafico.pdf", width = 12, height = 8, onefile = TRUE)
for(continente in levels(gapminder$Continente)){
  print(ggplot(data=
                 gapminder[gapminder$Continente == continente, ],
               aes(x=Año, y=Expect, colour=País)) +
          geom_line())
}
dev.off()
pdf("Life_Exp_vs_time.pdf", width=12, height=12, onefile = TRUE)
ggplot(data=gapminder, aes(x=Año, y=Expect, colour=País)) +
  geom_line() + facet_grid(Continente ~ .)+
  theme(legend.position = "none")

# You then have to make sure to turn off the pdf device!

dev.off()
head (gapminder-aus.csv)
a1990_subset = gapminder[gapmider$Año >= 1990,]
write.table(aust_subset,
            file = "cleaned-data/gapminder-aus.csv",
            sep = ",", quote = FALSE, row.names = FALSE
)
write.table(1990_subset,
            file = "cleaned-data/gapminder-1990.csv",gapminder$Año == 1990,
            sep = ",", quote = FALSE, row.names = FALSE
)
write.table(1990_subset,
            file = "cleaned-data/gapminder-1990.csv",
            sep = ",", quote = FALSE, row.names = FALSE
)