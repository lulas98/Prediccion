

library(car)
library(fBasics)
library(gvlma)
library(leaps)
library(MASS)
library(readr)
library(corrplot)
library(PerformanceAnalytics)
library(tidyverse)

## CARGAMOS LOS DATOS Y LOS VISUALIZAMOS


nba <- read_csv("nba.csv")
attach(nba)
view(nba)
#quitamos los % de los titulos que generan problemas
nba <- rename_with(nba, ~ tolower(gsub("%", "", .x, fixed = TRUE)))
#tenemos que limpiar los datos de NaN y duplicados

summarise_all(nba, funs(sum(is.na(.))))
 #tenemos NaN en algunas columnas, por lo que hemos de borrarlas
nba <- na.omit(nba)
#tambien tenemos que limpiar los duplicados
nba <- nba[!duplicated(nba$Player), ]
nba

#ya tenemos el dataset listo para realizar nuestro EDA

#analizamos cada variable respecto del salario, que va a ser nuestra variable dependiente. Quitamos las variables de Player, Country y Team ya que no son numéricas.

corrplot(cor(nba %>% 
               select_at(vars(-(c(Player, NBA_Country, Tm)))), 
             use = "complete.obs"), 
         method = "circle")

#observamos que las variables G, MP, OWS, DWS, WS y VORP estan altamente relacionadas con el salario.

#empezamos nuestro modelo de regresion lineal
model1 <- lm(log(Salary) ~ . - (Player + NBA_Country + Tm), data= nba)
summary(model1)
 #nos aparecen como altamente significativas el numero de draft, Age, Games, MP y USG%

#vemos nuestro modelo en un grafico QQ-plot es un gráfico de puntos que muestra los cuantiles. Cuanto mas unidos los puntos y la linea azul mejor
qqPlot(model1, labels=row.names(nba), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

residplot <- function(model1, nbreaks=10) {
  z <- rstudent(model1)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}
residplot(model1)
vResid=resid(model1)

#con Jarque-Bera podemos ver el contraste de normalidad del modelo
jbTest(vResid)         
  
#El test de Shapiro-Wilk permite comprobar si una muestra ha sido generada por un distribución normal.
shapiro.test(vResid)

#Se grafican los valores ajustados con respecto a los predictores, si no hay 
#problemas de linealidad se obtiene un recta sobre las que se representan los puntos.
crPlots(model1)

#Test de Varianza no constante (ncvtest)
ncvTest(model1)

#También podemos representar los residuos estandarizados absolutos versus
#los valores ajustados, donde se superpone la mejor linea recta que ajusta los datos.
spreadLevelPlot(model1)

#Validacion global
gvmodel <- gvlma(model1) 
summary(gvmodel)
 
#solo se da la heteroscedasticidad en nuestro modelo

#Detección de la multicolinealidad
vif(model1)

#Para cualquier regresor la raíz del VIF indica cuantas veces es la varianza del
#estimador es mayor que la que se obtendría si no hubiera correlación entre los
#regresores. Cuando VIF√>2 se considera que hay problemas de multicolinealidad.

sqrt(vif(model1)) > 2

#tenemos problemas de colinealidad en algunas variables. Unas dependen de otras

#Outliers
outlierTest(model1)

#determinar valores extremos.
hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(model1)

#Valores influyentes
cutoff <- 4/(nrow(mData)-length(model1$coefficients)-2)
plot(model1, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

avPlots(model1, ask=FALSE, id.method="identify")


#seleccionar el mejor modelo
stepAIC(model1, direction="both")

#gracias a StepAIC seleccionamos un nuevo modelo con menor AIC y mas simple. 
modelmejorado <- lm(formula = log(Salary) ~ NBA_DraftNumber + Age + MP + PER + 
                      `TS%` + `TRB%` + `AST%` + `TOV%` + `USG%` + DWS + `WS/48` + 
                      OBPM + BPM, data = nba)

#prediccion
set.seed(1234)
n <- 10 
x <- sample(1:nrow(nba), n, replace = FALSE)
datostest <- nba[x,]
datostest <- data.frame(datostest)
datostest

predict(modelmejorado, newdata = datostest)
