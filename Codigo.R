


# =====================================================================
# EVALUACIÓN SUMATIVA 2 - TI3061 | Parte A: datos, partición y descriptivo
# El CSV debe estar en la misma carpeta que este script (abrir el proyecto en esa carpeta)
# Si faltan paquetes, instalar una vez: install.packages(c("dplyr", "ggplot2", "tidyr"))
# =====================================================================
library(dplyr)
library(ggplot2)
library(tidyr)

# CARGA DEL DATASET
salud <- read.csv("atenciones_salud_sucio.csv", stringsAsFactors = FALSE)
names(salud)[1] <- "id_paciente"   # el archivo trae BOM y deja la 1ª columna con nombre raro



# VERIFICACIÓN INICIAL 
dim(salud)                 
str(salud)                
summary(salud)             
colSums(is.na(salud))     
sum(duplicated(salud))     



#comprueba rangos plausibles

min(salud$edad, na.rm = TRUE); max(salud$edad, na.rm = TRUE)
min(salud$cantidad_consultas, na.rm = TRUE); max(salud$cantidad_consultas, na.rm = TRUE)
min(salud$dias_hospitalizacion, na.rm = TRUE); max(salud$dias_hospitalizacion, na.rm = TRUE)

table(salud$comuna, useNA = "ifany")
table(salud$diagnostico_principal, useNA = "ifany")

#Estandarización de la variable comuna

salud$comuna <- trimws(salud$comuna)
salud$diagnostico_principal <- trimws(salud$diagnostico_principal)
salud$comuna[salud$comuna == ""] <- NA
salud$diagnostico_principal[salud$diagnostico_principal == ""] <- NA

salud$comuna <- tolower(salud$comuna)
salud$comuna[salud$comuna == "maipu"] <- "maipú"
salud$comuna[salud$comuna == "pte alto"] <- "puente alto"
table(salud$comuna, useNA = "ifany")


#Estandarización de la variable diagnóstico_principal

d0 <- tolower(salud$diagnostico_principal)
salud$diagnostico_principal <- case_when(
  d0 %in% c("dm2", "diabetes mellitus 2", "diabetes mellitus tipo 2") ~ "Diabetes Mellitus Tipo 2",
  d0 %in% c("hta", "hipertensión arterial") ~ "Hipertensión Arterial",
  d0 %in% c("ira", "infección respiratoria aguda") ~ "Infección Respiratoria Aguda",
  d0 == "neumonía" ~ "Neumonía",
  d0 == "apendicitis aguda" ~ "Apendicitis Aguda",
  d0 == "gastritis crónica" ~ "Gastritis Crónica",
  d0 == "artrosis" ~ "Artrosis",
  TRUE ~ NA_character_
)
table(salud$diagnostico_principal, useNA = "ifany")

# Estandarización de la variable costo_atencion
x <- salud$costo_atencion
x <- ifelse(grepl("^\\$", x), gsub("[$.]", "", x), x)   # solo limpia $ y puntos si empieza con $
salud$costo_atencion <- as.numeric(x)
salud$costo_atencion[salud$costo_atencion <= 0] <- NA   # los negativos y ceros son imposibles

str(salud$costo_atencion)
summary(salud$costo_atencion)
sum(is.na(salud$costo_atencion))


# Estandarización de la variable fecha ultima atencion

f <- salud$fecha_ultima_atencion
f[f == ""] <- NA
salud$fecha_ultima_atencion <- dplyr::coalesce(as.Date(f, format = "%Y-%m-%d"),
                                               as.Date(f, format = "%d/%m/%Y"))
salud$fecha_ultima_atencion[salud$fecha_ultima_atencion > as.Date("2026-10-04")] <- NA

class(salud$fecha_ultima_atencion)
summary(salud$fecha_ultima_atencion)
sum(is.na(salud$fecha_ultima_atencion))


#eliminacion de duplicados
salud <- distinct(salud)


#NA datos fuera de rango


salud$edad[salud$edad < 0 | salud$edad > 122] <- NA
salud$cantidad_consultas[salud$cantidad_consultas < 1 | salud$cantidad_consultas > 15] <- NA
salud$dias_hospitalizacion[salud$dias_hospitalizacion < 0 | salud$dias_hospitalizacion > 14] <- NA


#tipos finales


salud$comuna <- as.factor(salud$comuna)
salud$diagnostico_principal <- as.factor(salud$diagnostico_principal)

# Celdas vacías de id_paciente pasan a NA
salud$id_paciente[salud$id_paciente == ""] <- NA
sum(is.na(salud$id_paciente)) 



#verificacion

dim(salud)
str(salud)
summary(salud)

# Tabla de faltantes con porcentaje
na_tabla <- data.frame(
  variable = names(salud),
  NA_n = colSums(is.na(salud)),
  NA_pct = round(100 * colSums(is.na(salud)) / nrow(salud), 1)
)
na_tabla

# VARIABLE BINARIA PARA LA REGRESIÓN LOGÍSTICA 
salud$hosp_prolongada <- factor(ifelse(salud$dias_hospitalizacion >= 5, "Si", "No"),
                                levels = c("No", "Si"))
table(salud$hosp_prolongada, useNA = "ifany")
round(100 * prop.table(table(salud$hosp_prolongada)), 1)


#PARTICIÓN ENTRENAMIENTO / PRUEBA
set.seed(123)                      # semilla fija para reproducibilidad
idx <- sample(seq_len(nrow(salud)), size = round(0.70 * nrow(salud)))
train <- salud[idx, ]              # 70% para ajustar modelos
test  <- salud[-idx, ]             # 30% solo para evaluar
dim(train); dim(test)              # 2100 y 900 filas


# ANÁLISIS DESCRIPTIVO 
library(dplyr)
library(ggplot2)

# 1. Tendencia central, dispersión y posición (4 variables numéricas)
options(scipen = 999)
medidas <- salud %>%
  summarise(across(c(edad, cantidad_consultas, dias_hospitalizacion, costo_atencion),
                   list(media   = ~mean(.x, na.rm = TRUE),
                        mediana = ~median(.x, na.rm = TRUE),
                        sd      = ~sd(.x, na.rm = TRUE),
                        min     = ~min(.x, na.rm = TRUE),
                        q1      = ~quantile(.x, 0.25, na.rm = TRUE),
                        q3      = ~quantile(.x, 0.75, na.rm = TRUE),
                        max     = ~max(.x, na.rm = TRUE)))) %>%
  tidyr::pivot_longer(everything(), names_to = c("variable", ".value"),
                      names_pattern = "^(.*)_(media|mediana|sd|min|q1|q3|max)$")
print(as.data.frame(medidas), digits = 7)

# 2. Resumen por diagnóstico
por_diag <- salud %>%
  filter(!is.na(diagnostico_principal)) %>%
  group_by(diagnostico_principal) %>%
  summarise(n = n(),
            costo_medio = mean(costo_atencion, na.rm = TRUE),
            costo_sd = sd(costo_atencion, na.rm = TRUE),
            consultas_medias = mean(cantidad_consultas, na.rm = TRUE),
            dias_hosp_medios = mean(dias_hospitalizacion, na.rm = TRUE),
            prop_prolongada = mean(hosp_prolongada == "Si", na.rm = TRUE))
print(as.data.frame(por_diag), digits = 4)

# 3. Resumen por comuna
por_comuna <- salud %>%
  filter(!is.na(comuna)) %>%
  group_by(comuna) %>%
  summarise(n = n(),
            costo_medio = mean(costo_atencion, na.rm = TRUE),
            dias_hosp_medios = mean(dias_hospitalizacion, na.rm = TRUE))
print(as.data.frame(por_comuna), digits = 4)





# Gráfico 1: distribución
ggplot(salud, aes(x = costo_atencion)) +
  geom_histogram(bins = 20, fill = "lightblue", col = "black", na.rm = TRUE) +
  labs(title = "Distribución del costo de atención", x = "Costo ($)", y = "Frecuencia")

# Gráfico 2: comparación entre grupos
ggplot(subset(salud, !is.na(diagnostico_principal)),
       aes(x = diagnostico_principal, y = costo_atencion)) +
  geom_boxplot(na.rm = TRUE) + coord_flip() +
  labs(title = "Costo de atención por diagnóstico", x = "Diagnóstico", y = "Costo ($)")

# Gráfico 3: relación entre variables
ggplot(salud, aes(x = edad, y = costo_atencion)) +
  geom_point(alpha = 0.3, na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
  labs(title = "Edad vs costo de atención", x = "Edad (años)", y = "Costo ($)")



# Correlaciones (pairwise porque hay NA en distintas columnas)
nums <- salud[, c("edad", "cantidad_consultas", "dias_hospitalizacion", "costo_atencion")]
round(cor(nums, use = "pairwise.complete.obs"), 3)

# Atípicos por regla del IQR
boxplot.stats(salud$costo_atencion)$out
length(boxplot.stats(salud$costo_atencion)$out)
length(boxplot.stats(salud$edad)$out)
length(boxplot.stats(salud$cantidad_consultas)$out)
length(boxplot.stats(salud$dias_hospitalizacion)$out)

boxplot(salud$costo_atencion, main = "Costo de atención")
boxplot(salud$dias_hospitalizacion, main = "Días de hospitalización")

nums <- salud[, c("edad", "cantidad_consultas", "dias_hospitalizacion", "costo_atencion")]
round(cor(nums, use = "pairwise.complete.obs"), 3)

