
rm(list = ls(all.names = TRUE))
gc()


library(dplyr)     # Para el manejo de datos
library(ggplot2)    # Para realizar gráficas
library(kableExtra) # Para un mejor manejo de tablas
library(GGally)     # Para realizar análisis descriptivo fácilmente
library(multcomp)   # Para pruebas de hipótesis
library(openxlsx)
library(MASS) #Para seleccion de variables

##############################
####Cargas los datos ########
#############################


alfabetas <- read.xlsx("Bases de datos /Alfabetas.xlsx") %>%
  rename(PobAlfabeta = Suma.de.Alfabeta ,  PobAnalfabeta = Suma.de.Analfabeta )%>%
  filter(Etiquetas.de.fila != "Total general")

defunciones <- read.xlsx("Bases de datos /Defunciones.xlsx") %>%
  rename(defuncionesH = Suma.de.Hombres ,  defuncionesM = Suma.de.Mujeres ) %>%
  filter(Etiquetas.de.fila != "Total general")

discapacidad <- read.xlsx("Bases de datos /Discapacidad_.xlsx")%>%
  rename(discapacidadH = Suma.de.Hombres ,  discapacidadM = Suma.de.Mujeres ) %>%
  filter(Etiquetas.de.fila != "Total general")


divorcios <- read.xlsx("Bases de datos /Divorcios.xlsx")%>%
  rename(divadmin  = Suma.de.Administrativo, divjud=Suma.de.Judicial) %>%
  filter(Etiquetas.de.fila != "Total general")


migracion <- read.xlsx("Bases de datos /Inmigrantes y Emigrantes.xlsx")%>%
  rename(inmigrantes= Suma.de.Inmigrante3, emigrantes = Suma.de.Emigrante3,
         saldoneto = Suma.de.Saldo.neto3) %>%
  filter(Etiquetas.de.fila != "Total general")

migracion2 <- read.xlsx("Bases de datos /Inmigrantes y Emigrantes 2.xlsx")%>%
  rename(inmigrantes= Suma.de.Inmigrante3, emigrantes = Suma.de.Emigrante3,
         saldoneto = Suma.de.Saldo.neto3)%>%
  filter(Etiquetas.de.fila != "Total general")

licenciatura <- read.xlsx("Bases de datos /Licenciatura.xlsx")%>%
  rename(licenciatura = Suma.de.Licenciatura.o.equivalente)%>%
  filter(Etiquetas.de.fila != "Total general")


matrimonios <- read.xlsx("Bases de datos /Matrimonios.xlsx")%>%
  rename(matrimonios = Suma.de.2020 )%>%
  filter(Etiquetas.de.fila != "Total general")

nacimientos <- read.xlsx("Bases de datos /Nacimientos.xlsx")%>%
  rename(nacimientosH = Suma.de.Hombres, nacimientosM =  Suma.de.Mujeres)%>%
  filter(Etiquetas.de.fila != "Total general")

accidentes <- read.xlsx("Bases de datos /Numero de victimas.xlsx")%>%
  rename(victimasAcc = Suma.de.2020, Etiquetas.de.fila = Total.de.Victimas)%>%
  filter(Etiquetas.de.fila != "Total general")

pobreza <- read.xlsx("Bases de datos /Pobreza.xlsx")%>%
  rename(pobrezaModerada = Suma.de.Pobreza.moderada2,
         pobrezaExtrema = Suma.de.Pobreza.extrema2) %>%
  transmute(Etiquetas.de.fila,pobrezaModerada = pobrezaModerada*1000, #Porque está
            pobrezaExtrema = pobrezaExtrema*1000) %>%#en unidades miles
                                                            #de personas
  filter(Etiquetas.de.fila != "Total general")

religionc <- read.xlsx("Bases de datos /Religion catolica.xlsx")
religionc <- religionc %>%
  rename_with(~ as.character(unlist(religionc[1, ]))) %>%
  slice(-1) %>%
  rename(religionCatH = `Suma de Hombres3`, religionCatM = `Suma de Mujeres3`, 
         Etiquetas.de.fila = `Etiquetas de fila` )%>%
  mutate(religionCatH = as.numeric(religionCatH), religionCatM = as.numeric(religionCatM))%>%
  filter(Etiquetas.de.fila != "Total general")

religion <- read.xlsx("Bases de datos /Religion.xlsx")
religion <- religion %>%
  rename_with(~ as.character(unlist(religion[1, ]))) %>%
  slice(-1) %>%
  rename(religionH = `Suma de Hombres3`, religionM = `Suma de Mujeres3`, 
         Etiquetas.de.fila = `Etiquetas de fila`)%>%
  mutate(religionH = as.numeric(religionH), religionM = as.numeric(religionM))%>%
  filter(Etiquetas.de.fila != "Total general")


salud <- read.xlsx("Bases de datos /Servicios de Salud.xlsx")%>%
  filter(Etiquetas.de.fila != "Total general")


transtornos <- read.xlsx(
  "Bases de datos /Tasa de casos nuevos de transtornos.xlsx"
) 

transtornos <- transtornos %>%
  rename_with(~ as.character(unlist(transtornos[1, ]))) %>%
  slice(-1) %>%
  rename(transtornosH = `Suma de Hombres`, transtornosM = `Suma de Mujeres`,
         Etiquetas.de.fila = `Etiquetas de fila`)%>%
  mutate(
    transtornosH = as.numeric(transtornosH),  
    transtornosM = as.numeric(transtornosM)   
  ) %>%
  mutate(
    transtornosH = round(transtornosH, 3),
    transtornosM = round(transtornosM, 3)
  ) %>%
  filter(Etiquetas.de.fila != "Total general")


poblacion <- read.xlsx("Bases de datos /Poblacion.xlsx")
poblacion <- poblacion %>%
  rename_with(~ as.character(unlist(poblacion[1, ]))) %>%
  slice(-1) %>%
  rename(poblacionH = `Suma de Hombres3`, poblacionM = `Suma de Mujeres3`, 
         poblacionT = `Suma de Total3`,
         Etiquetas.de.fila = `Etiquetas de fila`) %>%
  transmute(Etiquetas.de.fila, poblacionT = as.numeric(poblacionT))%>%
  filter(Etiquetas.de.fila != "Total general")

str(poblacion)
head(poblacion)
poblacion$Etiquetas.de.fila


############################################
###Unir los datos###########################
############################################

dataframes_list <- list(poblacion, alfabetas, defunciones, discapacidad, divorcios,
                        migracion, licenciatura,
                        matrimonios, nacimientos, accidentes, pobreza, religionc, 
                        religion, salud, transtornos)


#Unimos los dataframes
datos <- purrr::reduce(
  dataframes_list, 
  ~ inner_join(.x, .y, by = "Etiquetas.de.fila")
)

head(datos)

Datos <- datos %>%
  transmute(estado = Etiquetas.de.fila, poblacionT, PobAlfabeta, 
            defunciones = defuncionesH + defuncionesM, 
         discapacidad = discapacidadH+discapacidadM, divorcios = divjud + divadmin, 
        inmigrantes, 
         emigrantes, licenciatura, matrimonios, nacimientos =
           nacimientosH + nacimientosM, victimasAcc, pobreza = pobrezaModerada + 
           pobrezaExtrema, religion = religionCatH + religionCatM + 
           religionH + religionM, afiliadosSalud = AfiliadoHombre + AfiliadoMujer,
         transtornos = transtornosH + transtornosM)

tail(Datos)
str(Datos)



##################################
###Análisis Descriptivo ##########
##################################
DatosAD <- Datos %>%
  dplyr::select(-estado)

library(knitr)
library(kableExtra)

# Calcular el vector de medias y desviaciones estándar de los datos originales
vector_medias <- colMeans(DatosAD)
vector_desviaciones <- apply(DatosAD, 2, sd)  # Calcula la desviación estándar por columna

# Crear el dataframe con medias y desviaciones estándar
df_medias_desviaciones <- data.frame(
  Media = vector_medias,
  `Desviación Estándar` = vector_desviaciones
)

# Configurar la tabla con kable
tabla_medias_desviaciones <- df_medias_desviaciones %>%
  kbl(col.names = c( "Media", "Desviación Estándar"), booktabs = TRUE, escape = FALSE, align = "c", caption = "Vector de Medias y Desviaciones Estándar de las Variables Estandarizadas") %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "hover", "condensed", "responsive"), full_width = F) %>%
  row_spec(0, background = "#D3D3D3")  # Fila de encabezado en gris claro

# Imprimir la tabla
print(tabla_medias_desviaciones)
# Cambiar los nombres de las columnas por números
colnames(DatosAD) <- 1:ncol(DatosAD)


#Grafica de Pares de datos escalados
DatosAD <- scale(DatosAD, scale = TRUE)
head(DatosAD)
X11()

p <- ggpairs(DatosAD)
ggsave("grafica_pares.png", plot = p, width = 12, height = 12)



#####################################
#####Ajuste del Modelo##############
####################################

#Nacimientos
DatosN <- Datos %>%
  dplyr::select(-defunciones, -estado) %>%
  transmute( Alfabetismo = PobAlfabeta/poblacionT,
            Discapacidad = discapacidad/poblacionT, 
            Divorcios = divorcios/poblacionT, Inmigracion = inmigrantes/poblacionT,
            Emigracion = emigrantes/poblacionT, Licenciatura = licenciatura/poblacionT, 
            Matrimonios = matrimonios/poblacionT, Nacimientos = nacimientos/poblacionT, 
            VictimasAccidentes = victimasAcc/poblacionT, pobreza/poblacionT, Religion = religion/poblacionT,
            AfiliadosSalud = afiliadosSalud/poblacionT,
            Transtornos = transtornos/poblacionT)
str(DatosN)
head(DatosN)


#Definir las combinaciones de distribuciones y funciones de enlace
Distribuciones <- c("gaussian", "Gamma", "inverse.gaussian")
FunLigas <- c("identity", "log", "inverse", "1/mu^2")

# Inicializar listas para almacenar los modelos, AIC, BIC y fórmulas
ModelList <- list()
AICList <- numeric()
BICList <- numeric()
FormList <- list()
DistList <- character()
LinkList <- character()

# Iterar sobre las combinaciones y ajustar los modelos GLM
for (dist in Distribuciones) {
  for (link in FunLigas) {
    # Ajustar el modelo GLM
    if (link == "1/mu^2" && dist != "inverse.gaussian") next  # Omitir combinaciones inválidas
    form <- as.formula("Nacimientos ~ .")
    model <- glm(form, data = DatosN, family = get(dist)(link = link))
    
    # Guardar el modelo, AIC, BIC, fórmula, distribución y función de enlace
    ModelList[[length(ModelList) + 1]] <- model
    AICList[length(AICList) + 1] <- AIC(model)
    BICList[length(BICList) + 1] <- BIC(model)
    FormList[[length(FormList) + 1]] <- form
    DistList <- c(DistList, dist)
    LinkList <- c(LinkList, link)
  }
}


# Encontrar el modelo con el menor AIC
minAIC_index <- which.min(AICList)
minBIC_index <- which.min(BICList)

ModelList[minAIC_index]
DistList[minAIC_index]
LinkList[minAIC_index]

fit_model <- glm(form, data = DatosN, family = gaussian(link = "identity"))

summary(fit_model)
AIC(fit_model)



######################
####Ajuste Poisson#### 
######################
#No consideré algún modelo que pudiera servir de los continuos.
#Se intentará con un ajuste Poisson para conteos
View(Datos)
DatosNP <- Datos %>%
  dplyr::select(-defunciones, -estado) %>%
  transmute(Nacimientos = nacimientos,
            poblacionT,
            LogPoblacionTotal = log(poblacionT), 
            Alfabetismo = PobAlfabeta,
            Discapacidad = discapacidad, 
            Divorcios = divorcios, 
             Inmigracion = inmigrantes,
            Emigracion = emigrantes, 
            Inmigracion = inmigrantes,
            Licenciatura = licenciatura, 
            Matrimonios = matrimonios, 
            VictimasAccidentes = victimasAcc, 
            Pobreza = pobreza, 
            Religion = religion,
            AfiliadosSalud = afiliadosSalud, 
            Transtornos = transtornos)
head(DatosNP)

fitPoisson.glm <- glm(Nacimientos ~ offset(LogPoblacionTotal) + 
                        Alfabetismo +
                        Discapacidad + 
                        Divorcios + 
                        Emigracion +
                        Inmigracion +
                        Licenciatura + 
                        Matrimonios +
                        VictimasAccidentes+
                        Pobreza+
                        Religion + 
                        AfiliadosSalud+
                        Transtornos, 
                  family=poisson(link="log"), data=DatosNP)
summary(fitPoisson.glm)
AIC(fitPoisson.glm)




##################################
##Seleccion de variables Poisson##
###################################

Poisson.step <- stepAIC(fitPoisson.glm, trace = TRUE) 
summary(Poisson.step)
AIC(Poisson.step)

# Regla de dedo para ver si es un modelo adecuado en cuanto al supuesto de
# la media igual a la varianza que se hace en un modelo Poisson
# Residual deviance / degrees of freedom debe ser similar a 1

deviance(Poisson.step)/df.residual(Poisson.step)

# O usando el que sería el estimador del parámetro de dispersión

# Estimador de phi
sum(residuals(Poisson.step, "pearson")^2)/(dim(DatosNP)[1]-summary(Poisson.step)$df[3])


# En este caso es muy diferente de 1, lo que muestra problemas
# con el supuesto de media igual a varianza


#################################
#####Supuestos Poisson###########
#################################

#Veamos la gráfica de los supuestos


library(ggplot2)
library(ggResidpanel)
resid_panel(Poisson.step, plots=c("all"))

#Al parecer no tenemos problemas con la qqplot

library(DHARMa)  
set.seed(245)
fit1res <- simulateResiduals(fittedModel = Poisson.step)

plot(fit1res)

# Se observan muchos problemas con los supuestos.

### Alternativa recomendada, usar modelo Binomial Negativo

###################################
####Ajuste Binomial Negativo#######
###################################


library(MASS)
fitBN <- glm.nb(Nacimientos ~ offset(LogPoblacionTotal) + 
                  Alfabetismo + 
                  Discapacidad + 
                  Divorcios +  
                  Emigracion + 
                  Inmigracion + 
                  Licenciatura +  
                  Matrimonios + 
                  VictimasAccidentes + 
                  Pobreza + 
                  Religion +  
                  AfiliadosSalud + 
                  Transtornos, data=DatosNP, link="log") 
summary(fitBN)

# sólo se pueden verificar supuestos con DHARMa

set.seed(123)
fitresBN <- simulateResiduals(fittedModel = fitBN)

plot(fitresBN )


# Al parecer cumple la mayoría de 
#supuestos pero puede que haya variables que no aporten
#más al modelo de algunas de las que ya están. 

#################################################
#######Seleccion de Variables Nacimientos########
#################################################

#Ocuparemos Regresión Lasso para seleccionar las variables

library(smurf)
formu <- Nacimientos/poblacionT ~  
  p(Alfabetismo, pen = "lasso") + 
  p(Discapacidad, pen = "lasso") + 
  p(Divorcios, pen = "lasso")  +  
  p(Emigracion, pen = "lasso") + 
  p(Inmigracion, pen = "lasso") + 
  p(Licenciatura, pen = "lasso") +  
  p(Matrimonios, pen = "lasso") + 
  p(VictimasAccidentes, pen = "lasso") + 
  p(Pobreza, pen = "lasso") + 
  p(Religion, pen = "lasso") +  
  p(AfiliadosSalud, pen = "lasso") + 
  p(Transtornos, pen = "lasso")

ejemplo.fit <- glmsmurf(formula = formu, family=poisson(link="log"), data = DatosNP, 
                        pen.weights = "glm.stand", lambda = "is.bic", 
                        control=list(lambda.length=200, reest = TRUE, lambda.reest=TRUE))


#gr?fica mostrando los diferentes BIC y el valor
#de lambda donde se obtiene el menor
plot_lambda(ejemplo.fit)
ejemplo.fit$lambda
log(ejemplo.fit$lambda)

# La siguiente gr?fica resume por variable (divididas por l?neas verticales)
# los coeficientes que se consideran cero (en cuadrados grises)
# para concatenar niveles de variables categ?ricas se usan los mismos colores

plot(ejemplo.fit, cex=3)

# el orden de las variables es el que se usa en la f?rmula
# el primer espacio corresponde al intercepto
summary(ejemplo.fit)


#Alfabetismo coef diferente de 0
#Discapacidad coef diferente a 0
#Divorcios coef diferente a 0
#Emigración coef diferente a 0
#Inmigración coef diferente a 0
#Licenciatura coef diferente a 0
#Matrimonios coef diferente a 0
#VictimasAccidentes coef diferente a 0
#Pobreza coef diferente a 0
#Religion coef igual a 0
#AfiliadosSalud coef diferente a 0
#Transtornos coef igual a 0


#Ajustando el modelo con la nueva seleccion de variables
fit_BNlasso <- glm.nb(Nacimientos ~ offset(LogPoblacionTotal) + 
                        Alfabetismo + 
                        Discapacidad + 
                        Divorcios +  
                        Emigracion + 
                        Inmigracion + 
                        Licenciatura +  
                        Matrimonios + 
                        VictimasAccidentes + 
                        Pobreza + 
                        Religion +  
                        AfiliadosSalud, data=DatosNP, link="log") 

summary(fit_BNlasso)

#Checando supuestos 
set.seed(1034)
fitresBNL <- simulateResiduals(fittedModel = fit_BNlasso)
plot(fitresBNL)


# # Hacer prueba de hipotesis para ver si puedo quitar más variables
# K=matrix(c(0,0,0,1,0,0,0,0,0,0,0,
#            0,0,0,0,0,1,0,0,0,0,0,
#            0,0,0,0,0,0,1,0,0,0,0,
#            0,0,0,0,0,0,0,1,0,0,0,
#            0,0,0,0,0,0,0,0,0,0,1), ncol=11, nrow=5, byrow=TRUE)
# m=matrix(c(0,0,0,0,0), ncol=1, nrow=5)
# summary(glht(fit_BNlasso, linfct=K, rhs=m, alternative="two.sided"))


#Checaremos qué variables podemos ir retirando y ver qué tanto nos aportan al modelo
fitBN2 <- glm.nb(Nacimientos ~ offset(LogPoblacionTotal) + 
                   Alfabetismo + 
                   Discapacidad + 
                   Emigracion + 
                   Inmigracion + 
                   Matrimonios +
                   Pobreza +
                   Religion + 
                   AfiliadosSalud, data=DatosNP, link="log") 

summary(fitBN2)
set.seed(123)
fit3res <- simulateResiduals(fittedModel = fitBN2)
plot(fit3res)

fit_BNfinal <- glm.nb(Nacimientos ~ offset(LogPoblacionTotal) + 
                        Alfabetismo  + 
                        Emigracion   + 
                        Pobreza +
                        AfiliadosSalud, data=DatosNP, link="log") 
summary(fit_BNfinal)
set.seed(189)
fitresBNfinal <- simulateResiduals(fittedModel = fit_BNfinal)
plot(fitresBNfinal)

#Veamos que se cumplen los supuestos
#Por lo que finalmente, mi modelo sería

## ln(E(Y|X)) = log(Poblacion) +b0 +  b1*Alfabetismo + b2*Emigracion + b3*Pobreza 
#             + b4*AfiliadosSalud
#                  
coef(fit_BNfinal)

########################
####Ajuste Muertes######
########################

DatosDP <- Datos %>%
  dplyr::select(-nacimientos, -estado) %>%
  transmute(Defunciones = defunciones,
            LogPoblacionTotal = log(poblacionT), 
            poblacionT,
            Alfabetismo = PobAlfabeta, 
            Discapacidad = discapacidad, 
            Divorcios = divorcios,
            Inmigracion = inmigrantes,
            Emigracion = emigrantes, 
            Licenciatura = licenciatura, 
            Matrimonios = matrimonios, 
            VictimasAccidentes = victimasAcc, 
            Pobreza = pobreza, 
            Religion = religion,
            AfiliadosSalud = afiliadosSalud, 
            Transtornos = transtornos)

fitPoissonM.glm <- glm(Defunciones ~ offset(LogPoblacionTotal) + 
                        Alfabetismo +
                        Discapacidad + 
                        Divorcios  +
                        Emigracion +
                        Licenciatura + 
                        Matrimonios +
                        VictimasAccidentes +
                        Pobreza +
                        Religion + 
                        AfiliadosSalud +
                        Transtornos, 
                      family=poisson(link="log"), data=DatosDP)
summary(fitPoissonM.glm)
AIC(fitPoissonM.glm)

Poisson2.step <- stepAIC(fitPoissonM.glm, trace = TRUE) 
summary(Poisson2.step)
AIC(Poisson2.step)

##Checando supuestos de este nuevo Poisson sin grado_escolar

resid_panel(Poisson2.step, plots=c("all"))
library(DHARMa)  
set.seed(245)
fit1res <- simulateResiduals(fittedModel = Poisson2.step)

plot(fit1res)

# Se observan muchos problemas con los supuestos.

#Volveremos a ocupar un ajuste Binomial Negativo

#########################################
#####Ajuste Binomial Negativo Muertes####
#########################################

library(MASS)
fit_BN_muertes <- glm.nb(Defunciones ~ offset(LogPoblacionTotal) + 
                           Alfabetismo + 
                           Discapacidad + 
                           Divorcios +  
                           Emigracion + 
                           Inmigracion + 
                           Licenciatura +  
                           Matrimonios + 
                           VictimasAccidentes + 
                           Pobreza + 
                           Religion +  
                           AfiliadosSalud + 
                           Transtornos, data=DatosDP, link="log") 
summary(fit_BN_muertes)



# sólo se pueden verificar supuestos con DHARMa
set.seed(123)
fitresBN_m <- simulateResiduals(fittedModel = fit_BN_muertes)

plot(fitresBN_m )
#No se tienen problemas con los supuestos

#######################################
###Seleccion de variables muertes######
######################################

#Ocuparemos regresión lasso para la selección de variables en este 
#modelo


library(smurf)
formu <- Defunciones ~  
  p(Alfabetismo, pen = "lasso") + 
  p(Discapacidad, pen = "lasso") + 
  p(Divorcios, pen = "lasso") +  
  p(Emigracion, pen = "lasso") + 
  p(Inmigracion, pen = "lasso") + 
  p(Licenciatura, pen = "lasso") +  
  p(Matrimonios, pen = "lasso") + 
  p(VictimasAccidentes, pen = "lasso") + 
  p(Pobreza, pen = "lasso") + 
  p(Religion, pen = "lasso") +  
  p(AfiliadosSalud, pen = "lasso") + 
  p(Transtornos, pen = "lasso")

ejemplo.fit <- glmsmurf(formula = formu, family=poisson(link="log"), data = DatosDP, 
                        pen.weights = "glm.stand", lambda = "is.bic", 
                        control=list(lambda.length=200, reest = TRUE, lambda.reest=TRUE))


#gr?fica mostrando los diferentes BIC y el valor
#de lambda donde se obtiene el menor
plot_lambda(ejemplo.fit)
ejemplo.fit$lambda
log(ejemplo.fit$lambda)

# La siguiente gr?fica resume por variable (divididas por l?neas verticales)
# los coeficientes que se consideran cero (en cuadrados grises)
# para concatenar niveles de variables categ?ricas se usan los mismos colores

plot(ejemplo.fit, cex=3)

# el orden de las variables es el que se usa en la f?rmula
# el primer espacio corresponde al intercepto
summary(ejemplo.fit)

#Analfabetismo coef diferente de 0
#Alfabetismo coef diferente de 0
#Discapacidad coef diferente a 0
#Divorcios coef diferente a 0
#Emigración coef igual a 0
#Inmigración coef diferente a 0
#Licenciatura coef diferente a 0
#Matrimonios coef diferente a 0
#VictimasAccidentes coef diferente a 0
#Pobreza coef diferente a 0
#Religion coef diferente a 0
#AfiliadosSalud coef diferente a 0
#NoAfiliadosSalud coef diferente a 0
#Transtornos coef diferente a 0


#Ajustando el modelo con la nueva seleccion de variables
fit_BN_muertesL <- glm.nb(Defunciones ~ offset(LogPoblacionTotal) + 
                            Alfabetismo + 
                            Discapacidad + 
                            Divorcios + 
                            Inmigracion + 
                            Licenciatura +  
                            Matrimonios + 
                            VictimasAccidentes + 
                            Pobreza + 
                            Religion +  
                            AfiliadosSalud + 
                            Transtornos, data=DatosDP, link="log") 

summary(fit_BN_muertesL)

#Checando supuestos 
set.seed(1034)
fitresBNmL <- simulateResiduals(fittedModel = fit_BN_muertesL)
plot(fitresBNmL)


fit_BN_muertes_final <- glm.nb(Defunciones ~ offset(LogPoblacionTotal) + 
                                 Alfabetismo + 
                                 Discapacidad  + 
                                 Inmigracion + 
                                 Licenciatura + 
                                 #Pobreza + 
                                 Religion, data=DatosDP, link="log") 
summary(fit_BN_muertes_final)
#Checando supuestos 
set.seed(1034)
fitresBNm <- simulateResiduals(fittedModel = fit_BN_muertes_final)
plot(fitresBNm)
coef(fit_BN_muertes_final)

#Mi modelo de Defunciones al final quedaría

# ln(E(Y|X)) = log(PoblacionTotal) + bo + b1*Alfabetismo + b2*Discapacidad 
#      + b3*Inmigracion + b4*Licenciatura + b5*Pobreza + b6*Religion
