
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




