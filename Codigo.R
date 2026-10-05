# =====================================================================
# EVALUACIÓN SUMATIVA 2 - TI3061
# PARTE A: CARGA, LIMPIEZA, PARTICIÓN Y ANÁLISIS DESCRIPTIVO
# =====================================================================
#
# Requisitos:
# - El archivo "atenciones_salud_sucio.csv" debe estar en la misma
#   carpeta que este script.
# - Se recomienda abrir un proyecto de RStudio en esa carpeta.
#
# Si faltan paquetes, instalarlos una sola vez con:
# install.packages(c("dplyr", "ggplot2", "tidyr"))
# =====================================================================


# =====================================================================
# 1. CARGA DE PAQUETES
# =====================================================================

library(dplyr)
library(ggplot2)
library(tidyr)


# Evita notación científica en los resultados numéricos
options(scipen = 999)


# =====================================================================
# 2. CONFIGURACIÓN INICIAL
# =====================================================================

archivo_entrada <- "atenciones_salud_sucio.csv"
archivo_salida <- "atenciones_salud_limpio.csv"

# Fecha utilizada para determinar si una atención está en el futuro.
# Se mantiene fija para asegurar que el análisis sea reproducible.
fecha_revision <- as.Date("2026-10-04")


# Verificación de existencia del archivo
if (!file.exists(archivo_entrada)) {
  stop(
    paste(
      "No se encontró el archivo",
      archivo_entrada,
      "en la carpeta de trabajo."
    )
  )
}


# =====================================================================
# 3. CARGA DEL DATASET
# =====================================================================

salud <- read.csv(
  archivo_entrada,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# Elimina un posible carácter BOM del nombre de la primera columna
names(salud)[1] <- sub("^\ufeff", "", names(salud)[1])


# Si la primera columna tiene un nombre extraño debido al BOM,
# se asigna el nombre esperado
if (names(salud)[1] != "id_paciente") {
  names(salud)[1] <- "id_paciente"
}


# Se guardan las dimensiones y tipos iniciales para compararlos después
dim_inicial <- dim(salud)
tipos_iniciales <- sapply(salud, class)
duplicados_iniciales <- sum(duplicated(salud))
na_inicial <- colSums(is.na(salud))


# =====================================================================
# 4. DIAGNÓSTICO INICIAL
# =====================================================================

cat("\n=============================================\n")
cat("DIAGNÓSTICO INICIAL DEL DATASET\n")
cat("=============================================\n\n")


# Dimensiones
dim(salud)
nrow(salud)
ncol(salud)


# Primeros registros
head(salud)


# Estructura y tipos de variables
str(salud)
sapply(salud, class)


# Resumen estadístico inicial
summary(salud)


# Valores reconocidos inicialmente como NA
colSums(is.na(salud))


# Duplicados exactos
sum(duplicated(salud))


# Conteo de cadenas vacías en variables de texto
vacios_texto <- sapply(
  salud,
  function(x) {
    if (is.character(x)) {
      sum(trimws(x) == "", na.rm = TRUE)
    } else {
      0
    }
  }
)

vacios_texto


# =====================================================================
# 5. VERIFICACIÓN DE RANGOS INICIALES
# =====================================================================

rangos_iniciales <- salud %>%
  summarise(
    edad_min = min(edad, na.rm = TRUE),
    edad_max = max(edad, na.rm = TRUE),
    
    consultas_min = min(cantidad_consultas, na.rm = TRUE),
    consultas_max = max(cantidad_consultas, na.rm = TRUE),
    
    hospitalizacion_min = min(
      dias_hospitalizacion,
      na.rm = TRUE
    ),
    
    hospitalizacion_max = max(
      dias_hospitalizacion,
      na.rm = TRUE
    )
  )

rangos_iniciales


# Categorías originales de comuna
table(
  salud$comuna,
  useNA = "ifany"
)


# Categorías originales de diagnóstico
table(
  salud$diagnostico_principal,
  useNA = "ifany"
)


# Celdas vacías que R todavía no reconoce como NA
sum(trimws(salud$comuna) == "", na.rm = TRUE)

sum(
  trimws(salud$diagnostico_principal) == "",
  na.rm = TRUE
)


# =====================================================================
# 6. LIMPIEZA DE IDENTIFICADOR
# =====================================================================

# El identificador se mantiene como texto y no se utilizará
# como predictor de los modelos
salud$id_paciente <- trimws(
  as.character(salud$id_paciente)
)

salud$id_paciente[
  salud$id_paciente == ""
] <- NA


# =====================================================================
# 7. ESTANDARIZACIÓN DE COMUNA
# =====================================================================

salud <- salud %>%
  mutate(
    comuna = trimws(comuna),
    comuna = na_if(comuna, ""),
    comuna = tolower(comuna),
    
    comuna = case_when(
      comuna == "maipu" ~ "maipú",
      comuna == "pte alto" ~ "puente alto",
      TRUE ~ comuna
    )
  )


# Verificación de comunas después de estandarizar
table(
  salud$comuna,
  useNA = "ifany"
)


# Lista de comunas esperadas
comunas_validas <- c(
  "antofagasta",
  "concepción",
  "la florida",
  "las condes",
  "maipú",
  "providencia",
  "puente alto",
  "santiago",
  "valparaíso",
  "viña del mar"
)


# Identificación de categorías inesperadas
comunas_no_reconocidas <- setdiff(
  unique(na.omit(salud$comuna)),
  comunas_validas
)

comunas_no_reconocidas


# Las comunas desconocidas, si existieran, se convierten en NA
salud$comuna[
  !is.na(salud$comuna) &
    !(salud$comuna %in% comunas_validas)
] <- NA


# =====================================================================
# 8. ESTANDARIZACIÓN DE DIAGNÓSTICO PRINCIPAL
# =====================================================================

# Se crea una versión temporal en minúsculas para comparar categorías
diagnostico_original <- salud$diagnostico_principal

d0 <- tolower(
  trimws(salud$diagnostico_principal)
)

d0[d0 == ""] <- NA


# Categorías conocidas antes de recodificar
variantes_diagnostico_validas <- c(
  "dm2",
  "diabetes mellitus 2",
  "diabetes mellitus tipo 2",
  "hta",
  "hipertensión arterial",
  "ira",
  "infección respiratoria aguda",
  "neumonía",
  "apendicitis aguda",
  "gastritis crónica",
  "artrosis"
)


# Se comprueba si existen diagnósticos no contemplados
diagnosticos_no_reconocidos <- unique(
  d0[
    !is.na(d0) &
      !(d0 %in% variantes_diagnostico_validas)
  ]
)

diagnosticos_no_reconocidos


# Recodificación de diagnósticos
salud$diagnostico_principal <- case_when(
  is.na(d0) ~ NA_character_,
  
  d0 %in% c(
    "dm2",
    "diabetes mellitus 2",
    "diabetes mellitus tipo 2"
  ) ~ "Diabetes Mellitus Tipo 2",
  
  d0 %in% c(
    "hta",
    "hipertensión arterial"
  ) ~ "Hipertensión Arterial",
  
  d0 %in% c(
    "ira",
    "infección respiratoria aguda"
  ) ~ "Infección Respiratoria Aguda",
  
  d0 == "neumonía" ~ "Neumonía",
  
  d0 == "apendicitis aguda" ~
    "Apendicitis Aguda",
  
  d0 == "gastritis crónica" ~
    "Gastritis Crónica",
  
  d0 == "artrosis" ~ "Artrosis",
  
  # Toda categoría no reconocida se considera inválida
  TRUE ~ NA_character_
)


# Verificación de diagnósticos estandarizados
table(
  salud$diagnostico_principal,
  useNA = "ifany"
)


# =====================================================================
# 9. CONVERSIÓN Y DEPURACIÓN DE COSTO DE ATENCIÓN
# =====================================================================

# Se guarda la variable original para controlar la transformación
costo_original <- trimws(
  as.character(salud$costo_atencion)
)

costo_original[costo_original == ""] <- NA


# Cantidad de NA antes de la coerción
na_costo_antes_coercion <- sum(
  is.na(costo_original)
)


# Si el costo comienza con "$", se eliminan el signo y los puntos
# usados como separadores de miles
costo_limpio <- ifelse(
  grepl("^\\$", costo_original),
  gsub("[\\$.]", "", costo_original),
  costo_original
)


# Conversión a variable numérica
salud$costo_atencion <- suppressWarnings(
  as.numeric(costo_limpio)
)


# Cantidad de NA después de la coerción
na_costo_despues_coercion <- sum(
  is.na(salud$costo_atencion)
)


# NA adicionales generados por valores no convertibles
na_costo_generados <- (
  na_costo_despues_coercion -
    na_costo_antes_coercion
)

na_costo_generados


# Los costos iguales o menores que cero se consideran no plausibles
salud$costo_atencion[
  !is.na(salud$costo_atencion) &
    salud$costo_atencion <= 0
] <- NA


# Verificación de costo
str(salud$costo_atencion)
summary(salud$costo_atencion)
sum(is.na(salud$costo_atencion))


# =====================================================================
# 10. CONVERSIÓN Y DEPURACIÓN DE FECHA DE ÚLTIMA ATENCIÓN
# =====================================================================

fecha_original <- trimws(
  as.character(salud$fecha_ultima_atencion)
)


# Se convierten cadenas vacías y "Sin registro" en NA
fecha_original[
  fecha_original %in% c("", "Sin registro")
] <- NA


# Cantidad de NA antes de la conversión
na_fecha_antes_coercion <- sum(
  is.na(fecha_original)
)


# Se reconocen los dos formatos existentes:
# - año-mes-día
# - día/mes/año
salud$fecha_ultima_atencion <- suppressWarnings(
  coalesce(
    as.Date(
      fecha_original,
      format = "%Y-%m-%d"
    ),
    
    as.Date(
      fecha_original,
      format = "%d/%m/%Y"
    )
  )
)


# Cantidad de NA después de interpretar las fechas
na_fecha_despues_coercion <- sum(
  is.na(salud$fecha_ultima_atencion)
)


# NA generados por fechas inválidas o no interpretables
na_fecha_generados <- (
  na_fecha_despues_coercion -
    na_fecha_antes_coercion
)

na_fecha_generados


# Las fechas posteriores a la fecha de revisión se consideran inválidas
salud$fecha_ultima_atencion[
  !is.na(salud$fecha_ultima_atencion) &
    salud$fecha_ultima_atencion > fecha_revision
] <- NA


# Verificación de fechas
class(salud$fecha_ultima_atencion)
summary(salud$fecha_ultima_atencion)
sum(is.na(salud$fecha_ultima_atencion))


# =====================================================================
# 11. ELIMINACIÓN DE DUPLICADOS EXACTOS
# =====================================================================

n_antes_duplicados <- nrow(salud)

duplicados_exactos_detectados <- sum(
  duplicated(salud)
)

duplicados_exactos_detectados


# Se eliminan solamente filas completamente idénticas
salud <- salud %>%
  distinct()


n_despues_duplicados <- nrow(salud)

duplicados_eliminados <- (
  n_antes_duplicados -
    n_despues_duplicados
)

duplicados_eliminados


# Verificación posterior
sum(duplicated(salud))


# Los ID repetidos no se eliminan, porque un paciente puede
# contar con más de una atención
id_repetidos <- salud %>%
  filter(!is.na(id_paciente)) %>%
  count(id_paciente, sort = TRUE) %>%
  filter(n > 1)

head(id_repetidos)


# =====================================================================
# 12. TRATAMIENTO DE VALORES FUERA DE RANGO
# =====================================================================

salud <- salud %>%
  mutate(
    edad = ifelse(
      edad < 0 | edad > 122,
      NA,
      edad
    ),
    
    cantidad_consultas = ifelse(
      cantidad_consultas < 1 |
        cantidad_consultas > 15,
      NA,
      cantidad_consultas
    ),
    
    dias_hospitalizacion = ifelse(
      dias_hospitalizacion < 0 |
        dias_hospitalizacion > 14,
      NA,
      dias_hospitalizacion
    )
  )


# =====================================================================
# 13. ASIGNACIÓN DE TIPOS FINALES
# =====================================================================

salud <- salud %>%
  mutate(
    comuna = factor(comuna),
    diagnostico_principal = factor(
      diagnostico_principal
    )
  )


# =====================================================================
# 14. VARIABLE BINARIA PARA REGRESIÓN LOGÍSTICA
# =====================================================================

salud <- salud %>%
  mutate(
    hosp_prolongada = case_when(
      is.na(dias_hospitalizacion) ~ NA_character_,
      dias_hospitalizacion >= 5 ~ "Si",
      TRUE ~ "No"
    ),
    
    # "No" queda como categoría de referencia.
    # El modelo logístico predecirá la probabilidad de "Si".
    hosp_prolongada = factor(
      hosp_prolongada,
      levels = c("No", "Si")
    )
  )


# Frecuencia de hospitalización prolongada
table(
  salud$hosp_prolongada,
  useNA = "ifany"
)


# Porcentajes sin considerar los valores faltantes
round(
  100 * prop.table(
    table(salud$hosp_prolongada)
  ),
  1
)


# =====================================================================
# 15. NORMALIZACIÓN / ESTANDARIZACIÓN
# =====================================================================

# Se utiliza una estandarización Z-score para costo_atencion.
# La transformación se realiza después de corregir valores imposibles.
# Esta variable se incorpora para cumplir el requisito de normalización.
#
# No es obligatorio usar costo_std en los modelos predictivos.

salud <- salud %>%
  mutate(
    costo_std = as.numeric(
      scale(costo_atencion)
    )
  )


# Comprobación del tipo y comportamiento de la variable estandarizada
class(salud$costo_std)
summary(salud$costo_std)


# Media aproximada a 0
mean(
  salud$costo_std,
  na.rm = TRUE
)


# Desviación estándar aproximada a 1
sd(
  salud$costo_std,
  na.rm = TRUE
)


# =====================================================================
# 16. IDENTIFICADOR DE FILA PARA TRAZABILIDAD
# =====================================================================

# Este índice permitirá relacionar posteriormente cada predicción
# con su observación original dentro del dataset limpio.
salud <- salud %>%
  mutate(
    id_fila = row_number()
  )


# =====================================================================
# 17. VALIDACIÓN FINAL
# =====================================================================

cat("\n=============================================\n")
cat("VALIDACIÓN FINAL DEL DATASET\n")
cat("=============================================\n\n")


# Dimensiones finales
dim(salud)


# Estructura final
str(salud)


# Resumen final
summary(salud)


# Comprobación de duplicados
duplicados_finales <- sum(
  duplicated(
    salud %>%
      select(-id_fila)
  )
)

duplicados_finales


# Reglas de validez: todos los resultados deberían ser cero
validacion_rangos <- data.frame(
  regla = c(
    "Edad fuera de rango",
    "Consultas fuera de rango",
    "Hospitalización fuera de rango",
    "Costo no positivo",
    "Fecha futura",
    "Duplicados exactos"
  ),
  
  registros_invalidos = c(
    sum(
      salud$edad < 0 |
        salud$edad > 122,
      na.rm = TRUE
    ),
    
    sum(
      salud$cantidad_consultas < 1 |
        salud$cantidad_consultas > 15,
      na.rm = TRUE
    ),
    
    sum(
      salud$dias_hospitalizacion < 0 |
        salud$dias_hospitalizacion > 14,
      na.rm = TRUE
    ),
    
    sum(
      salud$costo_atencion <= 0,
      na.rm = TRUE
    ),
    
    sum(
      salud$fecha_ultima_atencion > fecha_revision,
      na.rm = TRUE
    ),
    
    duplicados_finales
  )
)

validacion_rangos


# =====================================================================
# 18. TABLA FINAL DE VALORES FALTANTES
# =====================================================================

na_tabla <- data.frame(
  variable = names(salud),
  
  NA_n = colSums(
    is.na(salud)
  ),
  
  NA_pct = round(
    100 * colSums(is.na(salud)) /
      nrow(salud),
    1
  )
) %>%
  arrange(desc(NA_pct))

na_tabla


# =====================================================================
# 19. COMPARACIÓN ANTES Y DESPUÉS
# =====================================================================

dim_final <- c(
  nrow(salud),
  ncol(salud)
)

tipos_finales <- sapply(
  salud,
  class
)


comparacion_general <- data.frame(
  indicador = c(
    "Filas",
    "Columnas originales",
    "Duplicados exactos"
  ),
  
  antes = c(
    dim_inicial[1],
    dim_inicial[2],
    duplicados_iniciales
  ),
  
  despues = c(
    dim_final[1],
    dim_inicial[2],
    duplicados_finales
  )
)

comparacion_general


# Comparación de tipos de las variables originales
comparacion_tipos <- data.frame(
  variable = names(tipos_iniciales),
  tipo_inicial = as.character(tipos_iniciales),
  tipo_final = as.character(
    tipos_finales[names(tipos_iniciales)]
  )
)

comparacion_tipos


# =====================================================================
# 20. PIPELINE DPLYR Y RECODIFICACIÓN DE GRUPO ETARIO
# =====================================================================

# Este bloque utiliza:
# select(), filter(), mutate(), group_by(), summarise() y arrange().

resumen_gestion <- salud %>%
  select(
    comuna,
    diagnostico_principal,
    edad,
    dias_hospitalizacion,
    costo_atencion
  ) %>%
  
  filter(
    !is.na(comuna),
    !is.na(edad)
  ) %>%
  
  mutate(
    grupo_etario = case_when(
      edad < 18 ~ "Menor de edad",
      edad < 65 ~ "Adulto",
      TRUE ~ "Adulto mayor"
    )
  ) %>%
  
  group_by(
    comuna,
    grupo_etario
  ) %>%
  
  summarise(
    total_registros = n(),
    
    promedio_hospitalizacion = mean(
      dias_hospitalizacion,
      na.rm = TRUE
    ),
    
    costo_promedio = mean(
      costo_atencion,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  
  arrange(
    comuna,
    desc(promedio_hospitalizacion)
  )

resumen_gestion


# Se agrega grupo_etario también al dataset principal
salud <- salud %>%
  mutate(
    grupo_etario = case_when(
      is.na(edad) ~ NA_character_,
      edad < 18 ~ "Menor de edad",
      edad < 65 ~ "Adulto",
      TRUE ~ "Adulto mayor"
    ),
    
    grupo_etario = factor(
      grupo_etario,
      levels = c(
        "Menor de edad",
        "Adulto",
        "Adulto mayor"
      )
    )
  )


# =====================================================================
# 21. PARTICIÓN ENTRENAMIENTO / PRUEBA
# =====================================================================

set.seed(123)


# Selección aleatoria del 70 % de las observaciones
idx <- sample(
  seq_len(nrow(salud)),
  size = round(
    0.70 * nrow(salud)
  )
)


# Conjunto de entrenamiento
train <- salud[idx, ]


# Conjunto de prueba
test <- salud[-idx, ]


# Comprobación de dimensiones
dim(train)
dim(test)


# Comprobación de que no existan filas compartidas
length(
  intersect(
    train$id_fila,
    test$id_fila
  )
)


# =====================================================================
# 22. COMPARACIÓN DE CLASES ENTRE TRAIN Y TEST
# =====================================================================

distribucion_total <- round(
  prop.table(
    table(salud$hosp_prolongada)
  ),
  4
)

distribucion_train <- round(
  prop.table(
    table(train$hosp_prolongada)
  ),
  4
)

distribucion_test <- round(
  prop.table(
    table(test$hosp_prolongada)
  ),
  4
)


distribucion_total
distribucion_train
distribucion_test


comparacion_clases <- data.frame(
  conjunto = c(
    "Total",
    "Entrenamiento",
    "Prueba"
  ),
  
  proporcion_no = c(
    distribucion_total["No"],
    distribucion_train["No"],
    distribucion_test["No"]
  ),
  
  proporcion_si = c(
    distribucion_total["Si"],
    distribucion_train["Si"],
    distribucion_test["Si"]
  )
)

comparacion_clases


# =====================================================================
# 23. ALINEACIÓN DE NIVELES ENTRE TRAIN Y TEST
# =====================================================================

# Los niveles de los factores de test deben coincidir con train
# para evitar errores posteriores en predict().

train$diagnostico_principal <- factor(
  train$diagnostico_principal
)

test$diagnostico_principal <- factor(
  test$diagnostico_principal,
  levels = levels(
    train$diagnostico_principal
  )
)


train$comuna <- factor(
  train$comuna
)

test$comuna <- factor(
  test$comuna,
  levels = levels(
    train$comuna
  )
)


train$hosp_prolongada <- factor(
  train$hosp_prolongada,
  levels = c("No", "Si")
)

test$hosp_prolongada <- factor(
  test$hosp_prolongada,
  levels = c("No", "Si")
)


# =====================================================================
# 24. ESTADÍSTICAS DESCRIPTIVAS GENERALES
# =====================================================================

medidas <- salud %>%
  summarise(
    across(
      c(
        edad,
        cantidad_consultas,
        dias_hospitalizacion,
        costo_atencion
      ),
      
      list(
        media = ~ mean(
          .x,
          na.rm = TRUE
        ),
        
        mediana = ~ median(
          .x,
          na.rm = TRUE
        ),
        
        sd = ~ sd(
          .x,
          na.rm = TRUE
        ),
        
        min = ~ min(
          .x,
          na.rm = TRUE
        ),
        
        q1 = ~ quantile(
          .x,
          0.25,
          na.rm = TRUE
        ),
        
        q3 = ~ quantile(
          .x,
          0.75,
          na.rm = TRUE
        ),
        
        max = ~ max(
          .x,
          na.rm = TRUE
        )
      )
    )
  ) %>%
  
  pivot_longer(
    everything(),
    
    names_to = c(
      "variable",
      ".value"
    ),
    
    names_pattern =
      "^(.*)_(media|mediana|sd|min|q1|q3|max)$"
  )


print(
  as.data.frame(medidas),
  digits = 7
)


# =====================================================================
# 25. RESUMEN POR DIAGNÓSTICO
# =====================================================================

por_diag <- salud %>%
  filter(
    !is.na(diagnostico_principal)
  ) %>%
  
  group_by(
    diagnostico_principal
  ) %>%
  
  summarise(
    n_total = n(),
    
    n_costo_valido = sum(
      !is.na(costo_atencion)
    ),
    
    n_hosp_valido = sum(
      !is.na(hosp_prolongada)
    ),
    
    costo_medio = mean(
      costo_atencion,
      na.rm = TRUE
    ),
    
    costo_sd = sd(
      costo_atencion,
      na.rm = TRUE
    ),
    
    consultas_medias = mean(
      cantidad_consultas,
      na.rm = TRUE
    ),
    
    dias_hosp_medios = mean(
      dias_hospitalizacion,
      na.rm = TRUE
    ),
    
    prop_prolongada = mean(
      hosp_prolongada == "Si",
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  
  arrange(
    desc(costo_medio)
  )


print(
  as.data.frame(por_diag),
  digits = 4
)


# =====================================================================
# 26. RESUMEN POR COMUNA
# =====================================================================

por_comuna <- salud %>%
  filter(
    !is.na(comuna)
  ) %>%
  
  group_by(
    comuna
  ) %>%
  
  summarise(
    n_total = n(),
    
    n_costo_valido = sum(
      !is.na(costo_atencion)
    ),
    
    costo_medio = mean(
      costo_atencion,
      na.rm = TRUE
    ),
    
    dias_hosp_medios = mean(
      dias_hospitalizacion,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  
  arrange(
    desc(costo_medio)
  )


print(
  as.data.frame(por_comuna),
  digits = 4
)


# =====================================================================
# 27. VISUALIZACIONES
# =====================================================================

# ---------------------------------------------------------------------
# Gráfico 1: distribución del costo de atención
# ---------------------------------------------------------------------

grafico_distribucion <- ggplot(
  salud,
  aes(x = costo_atencion)
) +
  geom_histogram(
    bins = 20,
    fill = "lightblue",
    colour = "black",
    na.rm = TRUE
  ) +
  labs(
    title = "Distribución del costo de atención",
    subtitle = "Datos válidos después del proceso de limpieza",
    x = "Costo de atención ($)",
    y = "Frecuencia",
    caption = "Fuente: elaboración propia en RStudio"
  ) +
  theme_minimal()

print(grafico_distribucion)


# ---------------------------------------------------------------------
# Gráfico 2: comparación del costo entre diagnósticos
# ---------------------------------------------------------------------

grafico_grupos <- salud %>%
  filter(
    !is.na(diagnostico_principal),
    !is.na(costo_atencion)
  ) %>%
  
  ggplot(
    aes(
      x = diagnostico_principal,
      y = costo_atencion
    )
  ) +
  
  geom_boxplot(
    fill = "lightgreen"
  ) +
  
  coord_flip() +
  
  labs(
    title = "Costo de atención por diagnóstico",
    x = "Diagnóstico principal",
    y = "Costo de atención ($)",
    caption = "Fuente: elaboración propia en RStudio"
  ) +
  
  theme_minimal()

print(grafico_grupos)


# ---------------------------------------------------------------------
# Gráfico 3: relación entre edad y costo de atención
# ---------------------------------------------------------------------

grafico_relacion <- ggplot(
  salud,
  aes(
    x = edad,
    y = costo_atencion
  )
) +
  geom_point(
    alpha = 0.3,
    na.rm = TRUE
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "red",
    na.rm = TRUE
  ) +
  labs(
    title = "Relación entre edad y costo de atención",
    x = "Edad en años",
    y = "Costo de atención ($)",
    caption = "Fuente: elaboración propia en RStudio"
  ) +
  theme_minimal()

print(grafico_relacion)


# =====================================================================
# 28. CORRELACIONES
# =====================================================================

nums <- salud %>%
  select(
    edad,
    cantidad_consultas,
    dias_hospitalizacion,
    costo_atencion
  )


# Matriz de correlaciones usando pares de observaciones completas
matriz_correlaciones <- round(
  cor(
    nums,
    use = "pairwise.complete.obs"
  ),
  3
)

matriz_correlaciones


# Cantidad de observaciones utilizadas en cada correlación
n_pares_correlacion <- outer(
  names(nums),
  names(nums),
  
  Vectorize(
    function(x, y) {
      sum(
        complete.cases(
          nums[, c(x, y)]
        )
      )
    }
  )
)


dimnames(n_pares_correlacion) <- list(
  names(nums),
  names(nums)
)

n_pares_correlacion


# =====================================================================
# 29. DETECCIÓN DE VALORES ATÍPICOS
# =====================================================================

contar_atipicos <- function(x) {
  
  # Se retiran los NA antes de aplicar boxplot.stats()
  x <- x[!is.na(x)]
  
  length(
    boxplot.stats(x)$out
  )
}


atipicos <- data.frame(
  variable = c(
    "costo_atencion",
    "edad",
    "cantidad_consultas",
    "dias_hospitalizacion"
  ),
  
  cantidad = c(
    contar_atipicos(
      salud$costo_atencion
    ),
    
    contar_atipicos(
      salud$edad
    ),
    
    contar_atipicos(
      salud$cantidad_consultas
    ),
    
    contar_atipicos(
      salud$dias_hospitalizacion
    )
  )
)

atipicos


# Valores atípicos específicos de hospitalización
atipicos_hospitalizacion <- boxplot.stats(
  na.omit(
    salud$dias_hospitalizacion
  )
)$out

table(atipicos_hospitalizacion)


# Diagrama de cajas del costo
boxplot(
  salud$costo_atencion,
  main = "Costo de atención",
  ylab = "Costo ($)",
  col = "lightblue"
)


# Diagrama de cajas de hospitalización
boxplot(
  salud$dias_hospitalizacion,
  main = "Días de hospitalización",
  ylab = "Número de días",
  col = "lightgreen"
)


# =====================================================================
# 30. EXPORTACIÓN DEL DATASET LIMPIO
# =====================================================================

# Se exporta el dataset final con las variables limpias,
# transformadas y preparadas para el modelado.

write.csv(
  salud,
  archivo_salida,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# Verificación de que el archivo fue creado
file.exists(archivo_salida)


cat("\n=============================================\n")
cat("PROCESO FINALIZADO CORRECTAMENTE\n")
cat("Archivo generado:", archivo_salida, "\n")
cat("Filas de entrenamiento:", nrow(train), "\n")
cat("Filas de prueba:", nrow(test), "\n")
cat("=============================================\n")

# =====================================================================
# PARTE B: MODELOS
# REGRESIÓN LINEAL Y REGRESIÓN LOGÍSTICA
# =====================================================================


# =====================================================================
# 31. PREPARACIÓN DE DATOS PARA REGRESIÓN LINEAL
# =====================================================================

# La regresión lineal estima el costo de atención.
# Se usan solamente filas completas para las variables del modelo.

train_lm <- train %>%
  select(
    id_fila,
    costo_atencion,
    edad,
    cantidad_consultas,
    dias_hospitalizacion,
    diagnostico_principal
  ) %>%
  filter(
    complete.cases(
      costo_atencion,
      edad,
      cantidad_consultas,
      dias_hospitalizacion,
      diagnostico_principal
    )
  )


test_lm <- test %>%
  select(
    id_fila,
    costo_atencion,
    edad,
    cantidad_consultas,
    dias_hospitalizacion,
    diagnostico_principal
  ) %>%
  filter(
    complete.cases(
      costo_atencion,
      edad,
      cantidad_consultas,
      dias_hospitalizacion,
      diagnostico_principal
    )
  )


# Los diagnósticos de test deben tener los mismos niveles de train
train_lm$diagnostico_principal <- factor(
  train_lm$diagnostico_principal
)

test_lm$diagnostico_principal <- factor(
  test_lm$diagnostico_principal,
  levels = levels(train_lm$diagnostico_principal)
)


cat("\n=============================================\n")
cat("DATOS USADOS EN REGRESIÓN LINEAL\n")
cat("=============================================\n")

cat("Filas disponibles en train:", nrow(train), "\n")
cat("Filas utilizadas por el modelo:", nrow(train_lm), "\n")
cat("Filas disponibles en test:", nrow(test), "\n")
cat("Filas utilizadas para evaluar:", nrow(test_lm), "\n")


# =====================================================================
# 32. MODELO DE REGRESIÓN LINEAL
# =====================================================================

modelo_lm <- lm(
  costo_atencion ~
    edad +
    cantidad_consultas +
    dias_hospitalizacion +
    diagnostico_principal,
  data = train_lm
)


# Resultado completo del modelo
resumen_lm <- summary(modelo_lm)

resumen_lm


# =====================================================================
# 33. TABLA DE COEFICIENTES DEL MODELO LINEAL
# =====================================================================

tabla_coef_lm <- as.data.frame(
  resumen_lm$coefficients
)

tabla_coef_lm$variable <- rownames(tabla_coef_lm)

rownames(tabla_coef_lm) <- NULL


# Nombres más sencillos
names(tabla_coef_lm) <- c(
  "coeficiente",
  "error_estandar",
  "valor_t",
  "p_valor",
  "variable"
)


# Orden de las columnas
tabla_coef_lm <- tabla_coef_lm %>%
  select(
    variable,
    coeficiente,
    error_estandar,
    valor_t,
    p_valor
  )


tabla_coef_lm


# Coeficientes que resultaron significativos con p menor que 0,05
coeficientes_significativos_lm <- tabla_coef_lm %>%
  filter(p_valor < 0.05)

coeficientes_significativos_lm


# =====================================================================
# 34. MEDIDAS GENERALES DEL MODELO LINEAL
# =====================================================================

r2_lm <- resumen_lm$r.squared

r2_ajustado_lm <- resumen_lm$adj.r.squared

error_estandar_residual_lm <- resumen_lm$sigma


medidas_modelo_lm <- data.frame(
  medida = c(
    "R cuadrado",
    "R cuadrado ajustado",
    "Error estándar residual"
  ),
  
  valor = c(
    r2_lm,
    r2_ajustado_lm,
    error_estandar_residual_lm
  )
)

medidas_modelo_lm


# =====================================================================
# 35. PREDICCIONES DEL MODELO LINEAL
# =====================================================================

pred_lm <- predict(
  modelo_lm,
  newdata = test_lm
)


# Se agregan las predicciones al conjunto de prueba del modelo
resultados_lm <- test_lm %>%
  mutate(
    costo_predicho = as.numeric(pred_lm),
    error = costo_atencion - costo_predicho,
    error_absoluto = abs(error),
    error_cuadrado = error^2
  )


head(resultados_lm)


# =====================================================================
# 36. MAE Y RMSE
# =====================================================================

# MAE: promedio del error absoluto
mae_lm <- mean(
  resultados_lm$error_absoluto
)


# RMSE: raíz del promedio de los errores al cuadrado
rmse_lm <- sqrt(
  mean(resultados_lm$error_cuadrado)
)


metricas_lm <- data.frame(
  metrica = c(
    "MAE",
    "RMSE"
  ),
  
  valor = c(
    mae_lm,
    rmse_lm
  )
)

metricas_lm


cat("\n=============================================\n")
cat("RESULTADOS DE REGRESIÓN LINEAL\n")
cat("=============================================\n")

cat("R cuadrado:", round(r2_lm, 4), "\n")
cat(
  "R cuadrado ajustado:",
  round(r2_ajustado_lm, 4),
  "\n"
)
cat(
  "Error estándar residual:",
  round(error_estandar_residual_lm, 2),
  "\n"
)
cat("MAE:", round(mae_lm, 2), "\n")
cat("RMSE:", round(rmse_lm, 2), "\n")


# =====================================================================
# 37. GRÁFICOS DEL MODELO LINEAL
# =====================================================================

# Datos utilizados en los gráficos
diagnostico_lm <- data.frame(
  ajustados = fitted(modelo_lm),
  residuos = residuals(modelo_lm),
  residuos_estandarizados = rstandard(modelo_lm)
)


# ---------------------------------------------------------------------
# Gráfico 1: residuos frente a valores ajustados
# ---------------------------------------------------------------------

grafico_residuos <- ggplot(
  diagnostico_lm,
  aes(
    x = ajustados,
    y = residuos
  )
) +
  geom_point(
    alpha = 0.4,
    colour = "steelblue"
  ) +
  geom_hline(
    yintercept = 0,
    colour = "red",
    linetype = "dashed"
  ) +
  labs(
    title = "Residuos frente a valores ajustados",
    x = "Costo ajustado por el modelo",
    y = "Residuo"
  ) +
  theme_minimal()

print(grafico_residuos)


# ---------------------------------------------------------------------
# Gráfico 2: normalidad de los residuos
# ---------------------------------------------------------------------

grafico_qq <- ggplot(
  diagnostico_lm,
  aes(
    sample = residuos_estandarizados
  )
) +
  stat_qq(
    alpha = 0.5,
    colour = "steelblue"
  ) +
  stat_qq_line(
    colour = "red"
  ) +
  labs(
    title = "Gráfico Q-Q de los residuos",
    x = "Valores teóricos",
    y = "Residuos estandarizados"
  ) +
  theme_minimal()

print(grafico_qq)


# ---------------------------------------------------------------------
# Gráfico 3: costo real frente a costo predicho
# ---------------------------------------------------------------------

grafico_real_predicho <- ggplot(
  resultados_lm,
  aes(
    x = costo_atencion,
    y = costo_predicho
  )
) +
  geom_point(
    alpha = 0.4,
    colour = "darkgreen"
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    colour = "red",
    linetype = "dashed"
  ) +
  labs(
    title = "Costo real frente a costo predicho",
    x = "Costo real",
    y = "Costo predicho"
  ) +
  theme_minimal()

print(grafico_real_predicho)


# =====================================================================
# 38. PREPARACIÓN DE DATOS PARA REGRESIÓN LOGÍSTICA
# =====================================================================

# La regresión logística estima la probabilidad de que una
# hospitalización sea prolongada.
#
# dias_hospitalizacion no se utiliza como predictor porque esa variable
# se usó para crear hosp_prolongada.

train_log <- train %>%
  select(
    id_fila,
    hosp_prolongada,
    edad,
    cantidad_consultas,
    diagnostico_principal,
    costo_atencion
  ) %>%
  filter(
    complete.cases(
      hosp_prolongada,
      edad,
      cantidad_consultas,
      diagnostico_principal,
      costo_atencion
    )
  )


test_log <- test %>%
  select(
    id_fila,
    hosp_prolongada,
    edad,
    cantidad_consultas,
    diagnostico_principal,
    costo_atencion
  ) %>%
  filter(
    complete.cases(
      hosp_prolongada,
      edad,
      cantidad_consultas,
      diagnostico_principal,
      costo_atencion
    )
  )


# Asegura que "No" sea la referencia y "Si" el resultado de interés
train_log$hosp_prolongada <- factor(
  train_log$hosp_prolongada,
  levels = c("No", "Si")
)

test_log$hosp_prolongada <- factor(
  test_log$hosp_prolongada,
  levels = c("No", "Si")
)


# Niveles iguales para diagnóstico en train y test
train_log$diagnostico_principal <- factor(
  train_log$diagnostico_principal
)

test_log$diagnostico_principal <- factor(
  test_log$diagnostico_principal,
  levels = levels(train_log$diagnostico_principal)
)


cat("\n=============================================\n")
cat("DATOS USADOS EN REGRESIÓN LOGÍSTICA\n")
cat("=============================================\n")

cat("Filas disponibles en train:", nrow(train), "\n")
cat("Filas utilizadas por el modelo:", nrow(train_log), "\n")
cat("Filas disponibles en test:", nrow(test), "\n")
cat("Filas utilizadas para evaluar:", nrow(test_log), "\n")


# Distribución de la respuesta en entrenamiento y prueba
prop.table(
  table(train_log$hosp_prolongada)
)

prop.table(
  table(test_log$hosp_prolongada)
)


# =====================================================================
# 39. MODELO DE REGRESIÓN LOGÍSTICA
# =====================================================================

modelo_log <- glm(
  hosp_prolongada ~
    edad +
    cantidad_consultas +
    diagnostico_principal +
    costo_atencion,
  data = train_log,
  family = binomial
)


# Resultado completo
resumen_log <- summary(modelo_log)

resumen_log


# =====================================================================
# 40. TABLA DE COEFICIENTES DEL MODELO LOGÍSTICO
# =====================================================================

tabla_coef_log <- as.data.frame(
  resumen_log$coefficients
)

tabla_coef_log$variable <- rownames(tabla_coef_log)

rownames(tabla_coef_log) <- NULL


names(tabla_coef_log) <- c(
  "coeficiente",
  "error_estandar",
  "valor_z",
  "p_valor",
  "variable"
)


tabla_coef_log <- tabla_coef_log %>%
  select(
    variable,
    coeficiente,
    error_estandar,
    valor_z,
    p_valor
  )


tabla_coef_log


# Variables con p menor que 0,05
coeficientes_significativos_log <- tabla_coef_log %>%
  filter(p_valor < 0.05)

coeficientes_significativos_log


# =====================================================================
# 41. ODDS RATIOS
# =====================================================================

# El odds ratio se obtiene aplicando exp() a cada coeficiente.
# También se calcula un intervalo aproximado del 95 %.

odds_ratios <- tabla_coef_log %>%
  mutate(
    odds_ratio = exp(coeficiente),
    
    limite_inferior = exp(
      coeficiente -
        1.96 * error_estandar
    ),
    
    limite_superior = exp(
      coeficiente +
        1.96 * error_estandar
    )
  ) %>%
  select(
    variable,
    coeficiente,
    p_valor,
    odds_ratio,
    limite_inferior,
    limite_superior
  )


odds_ratios


# Se muestran los odds ratios sin el intercepto
odds_ratios_predictores <- odds_ratios %>%
  filter(variable != "(Intercept)")

odds_ratios_predictores


# =====================================================================
# 42. PROBABILIDADES EN EL CONJUNTO DE PRUEBA
# =====================================================================

prob_hosp_prolongada <- predict(
  modelo_log,
  newdata = test_log,
  type = "response"
)


# Nombre compatible con el reparto original del grupo
prob_falla <- prob_hosp_prolongada


# Se guardan las probabilidades junto con el resultado real
resultados_log <- test_log %>%
  mutate(
    prob_hosp_prolongada = as.numeric(
      prob_hosp_prolongada
    )
  )


head(resultados_log)


# Verificación del rango de las probabilidades
range(
  resultados_log$prob_hosp_prolongada
)


# Resumen de las probabilidades
summary(
  resultados_log$prob_hosp_prolongada
)


# Gráfico de probabilidades
grafico_probabilidades <- ggplot(
  resultados_log,
  aes(
    x = prob_hosp_prolongada,
    fill = hosp_prolongada
  )
) +
  geom_histogram(
    bins = 20,
    alpha = 0.6,
    position = "identity"
  ) +
  labs(
    title = "Probabilidades de hospitalización prolongada",
    x = "Probabilidad estimada",
    y = "Frecuencia",
    fill = "Resultado real"
  ) +
  theme_minimal()

print(grafico_probabilidades)


# =====================================================================
# 43. ARCHIVO FINAL DE PREDICCIONES
# =====================================================================

# Se parte desde todo el conjunto test.
# Las predicciones se unen mediante id_fila para no mezclar registros.

predicciones_lm_exportar <- resultados_lm %>%
  select(
    id_fila,
    costo_real = costo_atencion,
    costo_predicho
  )


predicciones_log_exportar <- resultados_log %>%
  select(
    id_fila,
    hosp_prolongada_real = hosp_prolongada,
    prob_hosp_prolongada
  )


predicciones_test <- test %>%
  select(
    id_fila
  ) %>%
  
  left_join(
    predicciones_lm_exportar,
    by = "id_fila"
  ) %>%
  
  left_join(
    predicciones_log_exportar,
    by = "id_fila"
  ) %>%
  
  arrange(id_fila)


head(predicciones_test)


# Cantidad de predicciones disponibles
sum(!is.na(predicciones_test$costo_predicho))

sum(
  !is.na(
    predicciones_test$prob_hosp_prolongada
  )
)


# Exportación del archivo
write.csv(
  predicciones_test,
  "predicciones_test.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# Comprobación del archivo
file.exists("predicciones_test.csv")


# =====================================================================
# 44. RESUMEN FINAL DE PERSONA B
# =====================================================================

cat("\n=============================================\n")
cat("PARTE B FINALIZADA CORRECTAMENTE\n")
cat("=============================================\n")

cat(
  "Registros usados en modelo lineal:",
  nrow(train_lm),
  "\n"
)

cat(
  "Registros usados para probar modelo lineal:",
  nrow(test_lm),
  "\n"
)

cat(
  "R cuadrado:",
  round(r2_lm, 4),
  "\n"
)

cat(
  "R cuadrado ajustado:",
  round(r2_ajustado_lm, 4),
  "\n"
)

cat(
  "MAE:",
  round(mae_lm, 2),
  "\n"
)

cat(
  "RMSE:",
  round(rmse_lm, 2),
  "\n"
)

cat(
  "Registros usados en modelo logístico:",
  nrow(train_log),
  "\n"
)

cat(
  "Registros usados para probar modelo logístico:",
  nrow(test_log),
  "\n"
)

cat(
  "Archivo generado: predicciones_test.csv\n"
)

cat("=============================================\n")