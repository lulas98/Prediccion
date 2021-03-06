
library(tidyverse)
library(broom) # modelos en df
library(flextable) # Tablas formateadas
library(mgcv) # estimar gam
library(reshape2) # melt
library(magrittr)
library(janitor)
library(imputeTS)
library(corrplot)

raw_data <- read_csv("../datos/pisasci2006.csv")

### Limpieza de datos

 #variables clave:
 
#- Overall Science Score (average score for 15 year olds)
#- Interest in science
#- Support for scientific inquiry
#- Income Index
#- Health Index
#- Education Index
#- Human Development Index (composed of the Income index, Health Index, and Education Index)

raw_data <- raw_data %>% clean_names()  
colnames(raw_data)
 
raw_data <- raw_data %>% select(country, overall, interest, support, income, health, edu, hdi)

# Eliminamos duplicados y NA
raw_data <- raw_data%>% distinct(country, .keep_all = T)
raw_data <- na_mean(raw_data)


#Una vez tenemos el dataset limpio podemos empezar a trabajar
pisa <- raw_data
attach(pisa)



# Correlaciones

vars <- c("country")

corrplot(cor(pisa %>% 
               select_at(vars(-vars)), 
             use = "complete.obs"), 
         method = "circle",type = "upper")

#smooth spline, aqui sacamos los grados de libertad optimos de cada variable respecto al overall.

spline_interest <- smooth.spline(x = interest, y = overall, cv = TRUE)
spline_support <- smooth.spline(x = support, y = overall, cv = TRUE)
spline_income <- smooth.spline(x = income, y = overall, cv = TRUE)
spline_health <- smooth.spline(x = health, y = overall, cv = TRUE)
spline_edu <- smooth.spline(x = edu, y = overall, cv = TRUE)
spline_hdi <- smooth.spline(x = hdi, y = overall, cv = TRUE)


spline_interest$df
spline_support$df
spline_income$df
spline_health$df
spline_edu$df
spline_hdi$df

#Modelo GAM
gam_mod1 <- gam(overall ~ s(interest) + s(support) + s(income) + s(health) + s(edu) + s(hdi), data = pisa)
coef(gam_mod1)
par(mfrow = c(2, 3))
plot(gam_mod1, se = TRUE, col = 'black', lwd = 2)



#calculamos nuestro modelo con los splines obtenidos anteriormente
gam_mod2 <- gam(overall ~ s(interest, k= 4.75) + s(support, k= 2) + s(income, k= 4.24) + 
                      s(health, k= 2) + s(edu, k= 2) + s(hdi, k= 8.60), data = pisa)

# Visualizamos
par(mfrow = c(2, 3))
plot(gam_mod2, residuals = TRUE, pch = 1)
summary(gam_mod2)
gam.check(gam_mod2)

#viendo esto podemos observar que tanto la educacion como hdi podian ser lineales, por lo que adaptamos el modelo

gam_mod3 <- gam(overall ~ s(interest, k= 4.750171) + s(support, k= 2.001243) + s(income, k= 4.244952) + 
                  s(health, k= 2) + edu + hdi, data = pisa)
par(mfrow = c(2, 3))
plot(gam_mod3, residuals = TRUE, pch = 1)
summary(gam_mod3)
gam.check(gam_mod3)




# AIC 
AIC(gam_mod1, gam_mod2, gam_mod3)

# BIC
BIC(gam_mod1, gam_mod2, gam_mod3)

#comparamos modelos
anova(gam_mod1, gam_mod2, gam_mod3)


#Escogemos el modelo 3, pues es el mas significativo

