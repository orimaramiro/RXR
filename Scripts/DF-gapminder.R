gapminder= read.csv("data/gapminder-FiveYearData.csv")
source(gapminder)
source("scripts/DF-gapminder.R")
str(gapminder)
head(gapminder)
colnames(gapminder)
dim(gapminder)
names(DF-gapminder)= c("País", "Año" , "Pobación", "Continente", "Expect", "PBI")
gapminder
names(gapminder)= c("País", "Año" , "Pobación", "Continente", "Expect", "PBI")
head (gapminder)
x <- c(5.4, 6.2, 7.1, 4.8, 7.5)
names(x) <- c('a', 'b', 'c', 'd', 'e')
x
X[c(-1,-5)]
x
x(-1,-5)
x[-1,-5]
x[c(-1, -5)]
x[c(2, 3, 4)]
x[-c(1, 5)]
head(gapminder)
gapminder[c([1],"argentina")]
gapminder=="Argentina", TRUE
gapminder[which(names(País) == "Argentina")]
x <- c(5.4, 6.2, 7.1, 4.8, 7.5)
names(x) <- c('a', 'b', 'c', 'd', 'e')
print(x)
x[-which(names(x) == "g")]
names(x) =="g"
gapminder[which(gapminder$País) == "Argentina"]
gapminder[which(names(gapminder) == c("País", "Argentina")]
gapminder[gapminder$País = "Argentina",]
x [x > 4 & x < 7]
x47 = x[x > 4 & x < 7]
x47
levels (x47)
if (any (gapminder$Año == 2012)){ print("hay 2002 loco") 
} else print("no hay che")
for(i in seq(from= 1, to= 10, by= 2-1)){print (i)}
MatrixSalida= matrix(nrow =5, ncol=5)
vector_j = c("a", "b", "c", "d", "e")
for (i in 1:5){
  for(j in 1:5){
    valor_j_temp =vector_j[j]
    salida_temp =paste(i, valor_j_temp)
    MatrixSalida[j,i] = salida_temp
  }
}
  
  MatrixSalida
  vector_salida = as.vector(MatrixSalida)
  vector_salida
  vector_salida2 <- vector(mode = "character")
  for (i in 1:5){
    for (j in c('a', 'b', 'c', 'd', 'e')){
      salida_temporal <- paste(i, j)
      vector_salida2 <- c(vector_salida2, salida_temporal)
    }
  }
  vector_salida2
  vector_salida
gapminder[gapminder$Continente=="Asia",mean "Expect", ]
mean(gapminder$Expect[gapminder$Continente=="Asia"])
media_asia= mean(gapminder$Expect[gapminder$Continente=="Asia"])
media_america = mean(gapminder$Expect[gapminder$Continente=="Americas"])
media_africa = mean(gapminder$Expect[gapminder$Continente=="Africa"])
media_europa = mean(gapminder$Expect[gapminder$Continente=="Europa"])
media_america
continentes= levels(gapminder$Continente)
for (continente in continentes){
  media= mean(gapminder$Expect[gapminder$Continente== continente])
 if (media > 70) {
print(paste("en",continente,"lamedia es mayor a 70"))
}  else {print(paste("en",continente,"lamedia es menor a 70"))
}
}
paises= levels(gapminder$País)

for (paises2 in paises){
  media= mean(gapminder$Expect[gapminder$País== paises2])
  if (media < 50) {
    print(paste("En",paises2, "la media es menor a 50"))
  } else if (media > 50 & media < 70) {
    print(paste("En",paises2, "la media es mayor a 50 y menor que 70"))
  } else print(paste("En",paises2, "la media es mayor a 70"))
} 
 library(ggplot2)
ggplot(gapminder,aes(x=PBI, y=Expect)) + geom_point()
head (gapminder)
ggplot(data = gapminder, aes(x = Año, y = Expect,by = País)) +
  geom_line()
  ejercicio2 + geom_line(aes(color=Continente, by=País))
  ejercicio2 + scale_x_log10()
  ejercicio2 =ggplot(data = gapminder, aes(x = Año, y = Expect,by = País)) +
    geom_line
  ggplot(gapminder,aes(x=PBI, y=Expect)) + geom_point()+scale_x_log10()
  ggplot(gapminder,aes(x=PBI, y=Expect)) + geom_point(alpha = 0.1,color="red")+scale_x_log10()  
  ggplot(gapminder,aes(x=PBI, y=Expect)) + geom_point(alpha = 0.1,color="red")+scale_x_log10()+geom_smooth()+geom_smooth(method = "lm",color = "green")
  ?loess
  ggplot(gapminder,aes(x=PBI, y=Expect, color = Continente)) + geom_point(size = 2,alpha = 0.5)+scale_x_log10()+geom_smooth()+geom_smooth(method = "lm")
  ggplot(gapminder,aes(x=PBI, y=Expect)) + geom_point(alpha = 0.1,color="red")+scale_x_log10()+geom_smooth()+geom_smooth(method = "lm",color = "green")
  starts.with <- substr(gapminder$País, start = 1, stop = 1)
  az.País <- gapminder[starts.with %in% c("A", "Z"), ]
  ggplot(data = az.País, 
         aes(x = Año, y = Expect, color = Continente)) +
    geom_line() + facet_wrap( ~ País)
  ggplot(data = az.País, 
         aes(x = Año, y = Expect, color = Continente)) +
    geom_line() + 
    facet_wrap( ~ País) +
    xlab("Year") +
    ylab("Life expectancy") + 
    ggtitle("Figura 1") +
    scale_colour_discrete( name = "Continente") +
    theme(strip.text = element_text(size = 13)) + theme_dark()
  ggplot(data = az.País, 
         aes(x = Año, y = Expect, color = Continente)) +
    geom_line() + 
    facet_wrap( ~ País) +
    xlab("Year") +
    ylab("Life expectancy") + 
    ggtitle("Figura 1") +
    scale_colour_manual( name = "Continente",
                         values =
                           c(Africa = "red",
                             Americas = "blue",
                             Asia = "yellow",
                             Oceania = "orange",
                             Europa = "purple"))+ 
    theme(axis.text.x=
            element_text(angle = 90, hjust = 1))
pop= (gapminder$Pobación/1000000)
pop
gapminder = cbind(gapminder,pop)
head (gapminder)
ggplot(gapminder,aes(x=Año, y=pop, color = Continente)) + geom_point()
head(gapminder)
m= matrix(1:12, nrow = 3, ncol =4)
m
m2= m*5
m2
m3=m%*% 5
m^-1
m* c(1,0,-1)
m> c(0,20)
m *c(1,0,-1,2)
vector1 = 1:100
vector2 = 1:10000
vectorsuma= sum (vector1**-2)
vectrosuma    
vectorsuma
vecsum2= sum (vector2**-2)
vecsum2
ddply(
  .data = calcPBI(gapminder),
  .variables = "continent",
  .fun = function(x) mean(x$gdp)
)
