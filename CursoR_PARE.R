#reopen with encoding
# Curso de R y R Studio
# Interface RStudio
# Set Working directory a donde esten los datos

#setwd("~/Curso R para PARE")

# install.packages(prophet)

# Cargar los paquetes
library(tidyverse)
library(descr)
library(readxl)
library(prophet)


# Importar los datos
mort2007 <- read_csv("NDS2007.csv")
mort2008 <- read_csv("NDS2008.csv")

#Juntar(Merge 2007 y 2008)

mort_total <- rbind(mort2007, mort2008)

# Seleccionar las variables a utilizar
variables_interes <- c("placeresidence", "countryresidence", 
                       "monthdeath", "yeardeath",
                       "sex", "ageunit", "ageunitnumber", 
                       "education", "cod_icd10","typedeath")

datos <- mort_total %>% select(variables_interes)

# Remover los objetos que no se van a utilizar
rm(mort_total, mort2007, mort2008)

# Explorar los datos

str(datos)

names(datos)

summary(datos)

head(datos)

head(datos, 10)

tail(datos)

unique(datos$education)

# Convertir la variable de educación a numerica

datos$educacion <- as.numeric(datos$education)

# Crear la variable de edad

datos$edad <- ifelse(datos$ageunit %in% c(2:6), 0,
                     ifelse(datos$ageunit == 1, datos$ageunitnumber + 100,
                            ifelse(is.na(datos$ageunit), NA, datos$ageunitnumber)))


summary(datos$edad)

# Recodificar edad

datos <- datos %>% mutate(reedad = case_when(
  edad < 25 ~ "Meno de 25",
  edad >= 25 & edad <65 ~ "25 a 64 años",
  edad >= 65 ~ "65 años o más"
))


datos$reedad  <- recode(datos$reedad, "Meno de 25" = "Menor de 25 años")

# Recodificar educacion

datos <- datos %>% mutate(reeduca = case_when(
  educacion == 0 ~ "Sin educacion",
  educacion >= 1 & educacion < 12 ~ "No se graduo de escuela superior",
  educacion == 12 ~ "Graduado(a) de escuela superior",
  educacion > 12 ~ "Con alguna educacion universitaria"
))


# Crear la variable de municipio de residencia

datos$municipio <- ifelse(datos$countryresidence == 9, 999,
                          ifelse(datos$countryresidence %in% c(2:4), 888, datos$placeresidence))


sort(unique(datos$municipio))

# Convertir variables a factores

datos$sexo <- factor(datos$sex, levels = 1:2, labels = c("Masculino", "Femenino"))

datos$reedad <- factor(datos$reedad, levels = c("Menor de 25 años", "25 a 64 años", "65 años o más"))

datos$reeduca <- factor(datos$reeduca, levels = c("Sin educacion",
                                                "No se graduo de escuela superior",
                                                "Graduado(a) de escuela superior",
                                                "Con alguna educacion universitaria"))

freq(datos$reeduca)

datos$tipo <- factor(datos$typedeath, 
                     levels = 0:6, 
                     labels = c("Natural", 
                                "Accidente",
                                "Suicidio", 
                                "Homicidio", 
                                "Pendiente de investigación",
                                "No se pudo determinar",
                                "Intervención legal"))



# Manejo de datos con tidyverse

# Filter

viejos <- datos %>% 
  filter(reedad == "65 años o más")

jovenes2008 <- datos %>% 
  filter(reedad == "Menor de 25 años"  & yeardeath == 2008)


# Group by and summarise

datos %>% group_by(tipo) %>% 
  summarise(promedio_edad = mean(edad, na.rm = TRUE),
            mediana_edad = median(edad, na.rm = TRUE),
            varianza_edad = var(edad, na.rm = TRUE),
            sd_edad = sd(edad, na.rm = TRUE),
            iqr_edad = IQR(edad, na.rm = TRUE),
            minimo_edad = min(edad, na.rm = TRUE),
            maximo_edad = max(edad, na.rm = TRUE))


edad_tipo_educ <- datos %>% group_by(tipo, reeduca) %>% 
  summarise(promedio_edad = mean(edad, na.rm = TRUE),
            mediana_edad = median(edad, na.rm = TRUE),
            varianza_edad = var(edad, na.rm = TRUE),
            sd_edad = sd(edad, na.rm = TRUE),
            iqr_edad = IQR(edad, na.rm = TRUE),
            minimo_edad = min(edad, na.rm = TRUE),
            maximo_edad = max(edad, na.rm = TRUE))

edad_tipo_educ


# ordenar (arrange)

datos %>% filter(edad >= 25) %>%
  group_by(reeduca) %>% 
  summarise(promedio = mean(edad, na.rm = TRUE)) %>%
  arrange(desc(promedio))
  

# Mutate

datos <- datos %>% mutate(years_dif = 79 - edad)

datos %>% group_by(yeardeath) %>% 
  summarise(promedio_dif = mean(years_dif, na.rm = TRUE))



# Analisis
# Estadisticas descriptivas para variables numericas

# Tabla 1 way
freq(datos$reedad)

datos %>% count(reedad)


# Tabla 2 way

datos %>% count(reeduca, reedad)

crosstab(datos$reeduca, datos$reedad)

CrossTable(datos$reeduca, datos$reedad)

# Graficas

# Graficas de barra

# Minimo requerido

p1 <- ggplot(datos, aes(reedad)) + geom_bar()

p1

# Mas completa
p2 <- ggplot(datos, aes(reedad)) + 
  geom_bar(fill = "blue",
           color = "white")

p2

p3 <- p2 + theme_classic() +
  labs(title = "Muertes por grupos de edad", 
       x = "Grupo de edad",
       y = "Cantidad")

p3


p3 + coord_flip()


# Forma alterna
# Remover NA y calcular porcentaje y añadirlo a la grafica

datos %>% filter(!is.na(reedad)) %>% 
  count(reedad) %>%
  mutate(perc = scales::percent(n/sum(n), 
                                accuracy = .1, 
                                trim = FALSE)) %>%
  ggplot(aes(x = reedad, y = n)) +
  geom_col(fill = "gray70") +
  geom_text(aes(label = perc), vjust = -0.5) +
  theme_classic()


# Stacked

datos %>% 
  filter(reeduca %in% c("Sin educacion", "No se graduo de escuela superior")) %>%
  ggplot(aes(tipo, fill = reedad)) + 
  geom_bar() + theme_classic() +
  labs(title = "Tipo de muerte por edad para personas que no se graduaron de escuela superior",
       x = "Tipo de muerte",
       y = "Cantidad",
       fill = "Grupo de edad")

# Position dodge

datos %>% 
  filter(reeduca %in% c("Sin educacion", "No se graduo de escuela superior")) %>%
  ggplot(aes(tipo, fill = reedad)) + 
  geom_bar(position = "dodge") + theme_classic() +
  labs(title = "Tipo de muerte por edad para personas que no se graduaron de escuela superior",
       x = "Tipo de muerte",
       y = "Cantidad",
       fill = "Grupo de edad")


# Graficas de boxplot

ggplot(datos, aes(tipo, edad)) + geom_boxplot()

ggplot(datos, aes(tipo, edad, fill = sexo)) + 
  geom_boxplot() + theme_classic() +
  labs(title = "Edad por tipo de muerte y sexo",
       x = "Tipo de muerte",
       y = "Edad",
       fill = "Sexo")


# Graficas de linea


serie_tiempo <- read_csv("ejemplo_serie.csv")
serie_tiempo$mes <- as.Date(serie_tiempo$month, "%m/%d/%Y")

ggplot(serie_tiempo, aes(mes, todas_entidades)) + 
  geom_line() + theme_classic()


# left_join

df1 <- read_excel("Municipios Codigos Censo.xls")
df2 <- read_excel("cambio poblacion menos 18.xls")

df1 %>% left_join(df2)

# Lo mismo pero especificando manualmente las variables para unir
df1 %>% left_join(df2, by = c("COUNTY", "GEOID"))


# Ejemplo proyecciones con prophet

# Cambiar nombre variables para que funcione la funcion prophet
serie_tiempo$ds <- serie_tiempo$mes
serie_tiempo$y <- serie_tiempo$todas_entidades


m <- prophet(serie_tiempo)

future <- make_future_dataframe(m, periods = 365)

tail(future)

forecast <- predict(m, future)

tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast)

prophet_plot_components(m, forecast)


