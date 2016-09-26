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
conPBI = calcPBI(gapminder)
ddply(
  .data = conPBI,
  .variables = "Continente",
  .fun = function(dat) mean(dat$PBI)
)
names(gapminder).
conPBI = calcPBI(gapminder)
ddply(
  .data = conPBI,
  .variables = c( "Continente", "Año"),
  .fun = function(dat) mean(dat$PBI))
conPBI = calcPBI(gapminder)
MediaVida= ddply(
  .data = conPBI,
  .variables = c( "Continente"),
  .fun = function(dat) mean(dat$Expect))
MediaVida
MediaVidaAño1= ddply(
  .data = conPBI[conPBI$Año == 1952 ,],
  .variables = c( "Continente"),
  .fun = function(dat) mean(dat$Expect))
MediaVidaAño1
"Año" == 2007
MediaVidaAño2= ddply(
  .data = conPBI[conPBI$Año == 2007 ,],
  .variables = c( "Continente"),
  .fun = function(dat) mean(dat$Expect))
MediaVidaAño2
MediaVidaAño3
REsta= cbind(MediaVidaAño1, anio_2007= MediaVidaAño2$V1,resta=MediaVidaAño3)
names REsta
REsta names
names(Resta)
lebels(REsta)
REsta
names(REsta)
names(REsta)[2]= "anio_1952"
REsta
ddply(gapminder, .(Continente), summarise,
      Expect = mean(Expect),
      sd_Exoect = sd(Expect))
library(dplyr)
install.packages('dplyr')
Africa
gdp_bycontinents <- gapminder %>%
  group_by(Continente) %>%
  summarize(mean_PBI = mean(PBI))
gdp_bycontinents
mediaIspa <- gapminder %>%
  group_by(País) %>%
  summarize(mean_Expect = mean(Expect))
mediaIspa[which.min(mediaIspa$mean_Expect),]
mediaIspa[which.max(mediaIspa$mean_Expect),]
pipa
MediaVidaAño2002= ddply(
  .data = gapminder[gapminder$Año == 2002 ,],
  .variables = c( "País"),
  .fun = function(dat) mean(dat$Expect))
MediaVidaAño2002
############# clase del viernes############
install.packages("tidyr")
library("tidyr")
str (gapminder)
library(ggplot2)
library(GGally)
install.packages("GGally")
gap_wide <- read.csv("http://bit.ly/gap_wide", stringsAsFactors = FALSE)
str(gap_wide)
View(gap_wide)
gap_long <- gap_wide %>%
  gather(obstype_year,obs_values, starts_with('pop'),
         starts_with('lifeExp'), starts_with('gdpPercap'))
str(gap_long)
library("dplyr")
View (gap_long)
gap_long <- gap_wide %>% gather(obstype_year,obs_values,-continent,-country)
str(gap_long)
View(gap_long)
gap_long <- gap_long %>% 
  separate(obstype_year,
           into = c('obs_type','year'), sep = "_")
gap_long$year <- as.integer(gap_long$year)
View (gap_long)
mediaIspa <- gap_long %>%
  group_by(continent, obs_type) %>%
  summarize(mean_Expect = mean(obs_values))
mediaIspa[which.min(mediaIspa$mean_Expect),]
mediaIspa[which.max(mediaIspa$mean_Expect),]
head (gap_long)
gap_normal <- gap_long %>% spread(obs_type, obs_values)
dim(gap_normal)
dim(gapminder)
head (gapminder, gap_normal)
head(gap_normal)
names(gapminder)
names(gap_normal)
gap_normal <- gap_normal[,names(gapminder)]
all.equal(gap_normal,gapminder)
gap_normal <- gap_normal[,names(gapminder)]
all.equal(gap_normal,gapminder)
gap_temp <- gap_long %>% 
  unite(var_ID, continent, country, sep = "_")
View(gap_temp)
gap_temp <- gap_long %>%
  unite(ID_var,continent,country,sep = "_") %>%
  unite(var_names, obs_type, year, sep = "_")
str(gap_temp)
View(gap_temp)
gap_wide_new <- gap_long %>%
  unite(ID_var, continent, country, sep = "_") %>%
  unite(var_names, obs_type, year, sep = "_") %>%
  spread(var_names, obs_values)
gap_wide_new <- gap_long %>%
  unite(ID_var, continent, country, sep = "_") %>%
  unite(var_names, obs_type, year, sep = "_") %>%
  spread(var_names, obs_values)
View(gap_wide_new)
gap_ridiculamente_ancho <- gap_long %>% 
  unite(var_names, country, obs_type, year, sep = "_") %>% 
  spread(var_names, obs_values)
dim(gap_ridiculamente_ancho)
View(gap_long)
install.packages(c("jsonlite", "base64enc"))
