
# Leer el archivo CSV con la ruta corregida
base_manga <- final_data

library(dplyr)

base_manga <- base_manga %>%
  mutate(
    status_num = as.numeric(status_survey),
    visita_num = as.numeric(visita_pull),
    
    # EFECTIVA: Status 1 (Completa)
    efectiva = if_else(status_num == 1, 1, 0, missing = 0),
    
    # NO EFECTIVA: Rechazos, desocupadas, sin red, o +4 visitas sin éxito
    no_efectiva = if_else(
      status_num %in% c(5, 6, 7, 9) | (status_num %in% c(2, 4, 8) & visita_num >= 4), 
      1, 0, missing = 0
    ),
    
    # AGENDADA: Citas o ausentes con menos de 4 visitas
    agendada = if_else(
      status_num %in% c(2, 4, 8) & visita_num < 4, 
      1, 0, missing = 0
    ),
    
    # Variable de etiqueta para agrupar
    tipo_encuesta_lbl = case_when(
      efectiva == 1 ~ "EFECTIVA",
      no_efectiva == 1 ~ "NO EFECTIVA",
      agendada == 1 ~ "AGENDADA",
      TRUE ~ "OTRO"
    )
  )

base_manga <- base_manga %>%
  group_by(padron_pull) %>%
  mutate(
    # Creamos un peso de prioridad: Efectiva (3), Agendada (2), No Efectiva (1), Otros (0)
    prioridad_status = case_when(
      tipo_encuesta_lbl == "EFECTIVA" ~ 3,
      tipo_encuesta_lbl == "AGENDADA" ~ 2,
      tipo_encuesta_lbl == "NO EFECTIVA" ~ 1,
      TRUE ~ 0
    )
  ) %>%
  # Ordenamos dentro de cada padrón: 
  # Primero por prioridad (desc), luego por número de visita (desc)
  arrange(padron_pull, desc(prioridad_status), desc(visita_num)) %>%
  mutate(
    # Marcamos como 1 la primera fila de cada grupo tras el ordenamiento
    encuesta_final = if_else(row_number() == 1, 1, 0)
  ) %>%
  ungroup()

# Verificación rápida
table(base_manga$encuesta_final, base_manga$tipo_encuesta_lbl)

# Esto debería darte exactamente 1 por cada padrón único
check_unicos <- base_manga %>% 
  filter(encuesta_final == 1) %>% 
  count(padron_pull) %>% 
  filter(n > 1)

if(nrow(check_unicos) == 0) {
  print("Logrado: Cada padrón tiene una única observación final.")
} else {
  print("Ojo: Hay padrones duplicados aún. Revisar datos de origen.")
}

#### Alertas ####
#---------------#

#### ALERTA DE TIEMPO ####

# 2. Alerta de Tiempo usando tu lógica pero agrupada
base_manga <- base_manga %>%
  mutate(duration_min = as.numeric(duration) / 60) %>%
  group_by(tipo_encuesta_lbl) %>% # <--- La clave está aquí
  mutate(
    media_grupo = mean(duration_min, na.rm = TRUE),
    sd_grupo = sd(duration_min, na.rm = TRUE),
    
    # Definir umbrales dinámicos por grupo
    umbral_corto = media_grupo - (3 * sd_grupo),
    umbral_excesivo = media_grupo + (3 * sd_grupo),
    
    # Crear alertas binarias
    alerta_corto = if_else(duration_min < umbral_corto, 1, 0, missing = 0),
    alerta_excesivo = if_else(duration_min > umbral_excesivo, 1, 0, missing = 0),
    
    # Etiqueta de texto (opcional, para limpieza)
    duration_alert = case_when(
      alerta_corto == 1 ~ "Tiempo corto",
      alerta_excesivo == 1 ~ "Tiempo excesivo",
      TRUE ~ "Normal"
    )
  ) %>%
  ungroup() # Siempre desagrupar al terminar

base_manga <- base_manga %>%
  mutate(
    alerta_excesivo = if_else(duration_alert == "Tiempo excesivo", 1, 0),
    alerta_corto = if_else(duration_alert == "Tiempo corto", 1, 0)
  )

#### ALERTA DE MISSINGS ####


ODK_filtrado <- odkmissing::import_odk_propagate_required("G:/Unidades compartidas/PROYECTOS 🌍📂/🥁 PROYECTOS/🏆 Proyectos S. Desarrollo/163. BID Uruguay - Manga/2. Implementación/Componente cuantitativo/Línea de Base/05_Auditoria/ODK/encuestadores_Campo_missings.xlsx", required_value = "yes")
ODK_filtrado <- ODK_filtrado |> 
  dplyr::filter(name %in% names(base_manga))

ODK_procesado <- odkmissing::build_spec_for_flags(datos = base_manga, ODK_filtrado = ODK_filtrado)


spec_for_flags <- ODK_procesado$spec_for_flags
datos_tokens   <- ODK_procesado$datos_tokens

print((spec_for_flags$manual_expr))

base_manga <- odkmissing::flags_missing_por_variable(
  data          = datos_tokens,
  spec          = spec_for_flags,
  prefix        = "m",
  numeric_conds = TRUE,
  coerce_target = FALSE
)

variables_missing <- names(base_manga)[grepl("^m_", names(base_manga))]

base_manga <- base_manga |>
  dplyr::mutate(
    total_missing = rowSums(dplyr::pick(dplyr::all_of(variables_missing)), na.rm = TRUE)
  )


# Levantar saltos

base_manga <- odkmissing::create_skip_vars(
  data          = base_manga,
  spec          = spec_for_flags,
  prefix        = "s",
  numeric_conds = TRUE
)


variables_saltos <- names(base_manga)[grepl("^s_", names(base_manga))]

base_manga <- base_manga |>
  dplyr::mutate(
    total_saltos = rowSums(dplyr::pick(dplyr::all_of(variables_missing)), na.rm = TRUE)
  )


#---------------------------------------#
# Valores numéricos extremos #
#---------------------------------------#

# 1. Definimos la lista de variables a las que queremos aplicar el Z-score
vars_to_check <- c("p2_02", "p3_02a", "p3_07", "p4_01", "p4_02", 
                   "p4_03", "p4_04", "p10_03", "p5_09", "p5_19", "p8_01")

# 2. Aplicamos la lógica de forma masiva
base_manga <- base_manga %>%
  mutate(across(
    all_of(vars_to_check),
    .fns = list(ex = ~ {
      # Convertimos a numérico
      val <- as.numeric(.x)
      # Limpiamos valores especiales como -1 para que no afecten el promedio
      clean_val <- ifelse(val == -1, NA, val)
      
      # Calculamos el Z-score usando la media y SD de los datos limpios
      mu <- mean(clean_val, na.rm = TRUE)
      sigma <- sd(clean_val, na.rm = TRUE)
      
      # Si el valor original es -1, no es un outlier (es una respuesta válida "NS/NR")
      # Si no, calculamos si está a más de 3 desviaciones estándar
      ifelse(!is.na(clean_val) & abs((clean_val - mu) / sigma) > 3, 1, 0)
    }),
    .names = "ex_{.col}" # Esto creará columnas tipo ex_p2_02, ex_p4_04, etc.
  ))

# 3. Revisar cuántas alertas se generaron por cada variable
resumen_alertas <- base_manga %>%
  summarise(across(starts_with("ex_"), ~ sum(.x, na.rm = TRUE)))

print(resumen_alertas)

#-----------------------#
#  Alerta: Duplicados   #
#-----------------------#
 caract_especi_mayus <- c("Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", "Ú" = "U", "Ñ" = "N")
 
base_manga <- base_manga %>%
  mutate(
    id_unico = case_when(
      status_survey == 1 ~ paste(p11_02_nombre, p11_02_apellido, p11_03a, sep = "_"),
      status_survey == 4 ~ paste(nombre_contacto, telefono, sep = "_"),
      TRUE ~ as.character(status_survey)
    )
  ) %>%
  mutate(
    id_unico = str_squish(str_replace_all(toupper(id_unico), caract_especi_mayus))
  )%>%
  group_by(id_unico) %>%
  mutate(
    total_dup = if_else(
      n() > 1 & id_unico != "", 1, 0
    )
  ) %>%
  ungroup()

# Selecciona las columnas 'nombre', 'apellido' y 'id' del dataframe original
colegios <- base_manga[, c("total_dup", "id_unico", "status_survey_resp","p11_02_nombre","nombre_contacto")]

# Crea un vector con los id_unico que quieres eliminar
# ids_a_borrar <- c("62165385", "62500313", "10000411", "62500338") 

# Filtra el dataframe para mantener solo las filas que NO cumplen la condición
# base_manga <- base_manga %>%
#  filter(!(id_unico %in% ids_a_borrar))

table(base_manga$total_dup)

#### ALERTA GEOREFERENCIACIÓN ####
 Una vez que aceptes los permisos en el navegador, corre esto:
url_maestra <- "https://docs.google.com/spreadsheets/d/1brA0QxuJqCq8UAE4Umc-Ms89l06OEhtR-9DmgFMv1Pg/edit#gid=0"
maestra_poligonos <- read_sheet(url_maestra)

# ... (Pasos 1 y 2 igual: cargar librerías y leer Sheet)

# 3. Procesar Maestra (Mantenla como tabla normal para el Join)
maestra_data <- maestra_poligonos %>%
  filter(!is.na(wkt_geom)) %>%
  mutate(padron = as.character(padron)) %>%
  as.data.frame() # <-- ESTO ELIMINA EL ERROR DEL JOIN
# No la convertimos a SF aquí todavía para evitar el error del Join

# 4. Procesar Base Manga (Puntos)
base_manga <- base_manga %>%
  mutate(
    latitud = as.numeric(georeferenciacion_latitude),
    longitud = as.numeric(georeferenciacion_longitude),
    padron_pull = as.character(padron_pull),
    padron_pull = "129431",
  ) %>%
  filter(!is.na(latitud) & !is.na(longitud)) %>%
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326) %>%
  st_transform(32721)

# 5. Cruce de datos (Ahora el left_join funcionará sin errores)
base_manga <- base_manga %>%
  left_join(maestra_data, by = c("padron_pull" = "padron")) %>%
  filter(!is.na(wkt_geom)) # Quitamos los que no tienen polígono en la maestra

# 6. Cálculo FILA POR FILA
# Convertimos la columna wkt_geom a geometría real dentro del cálculo
poligonos_geometria <- st_as_sfc(base_manga$wkt_geom, crs = 32721)

base_manga$distancia_m <- as.numeric(
  st_distance(base_manga$geometry, poligonos_geometria, by_element = TRUE)
)

# 7. Crear Alerta y Resumen
base_manga <- base_manga %>%
  mutate(alerta = ifelse(distancia_m > 10, "REVISAR: Fuera de rango", "OK"))

# Resultados finales
summary(base_manga$distancia_m)
table(base_manga$alerta)

#### ALERTA NSNR ####

# 1. Definimos la lista de variables que tienen opción NS/NR según tus choices
# Incluimos ingresos, gastos, años, y objetos del hogar
vars_nsnr <- c("p4_04", "p4_05", "p5_08", "p5_19", "p8_01", "p3_08a", 
               "p3_08b", "p3_08c", "p3_08d", "p3_08e", "p3_08f", 
               "p3_08g", "p3_08h", "p3_08i", "p3_08j", "p5_06", "p5_07",  "p5_11")

# 2. Contamos cuántos NS/NR hay por encuesta
# En tus labels, los valores NS/NR son -1 o 98
base_manga <- base_manga %>%
  mutate(
    total_nsnr = rowSums(across(all_of(vars_nsnr), ~ .x %in% c(-1, 98, 99)), na.rm = TRUE)
  )

# 3. Calculamos la alerta de "Exceso de NS/NR" por grupo (Efectivas únicamente)
# Es vital hacerlo solo en efectivas porque en las otras no se llega a esas preguntas
base_manga <- base_manga %>%
  group_by(tipo_encuesta_lbl) %>%
  mutate(
    mu_nsnr = mean(total_nsnr, na.rm = TRUE),
    sd_nsnr = sd(total_nsnr, na.rm = TRUE),
    
    # Umbral: más de 3 desviaciones estándar del promedio de NS/NR
    umbral_nsnr = mu_nsnr + (3 * sd_nsnr),
    
    # Variable binaria de alerta
    alerta_exceso_nsnr = if_else(efectiva == 1 & total_nsnr > umbral_nsnr, 1, 0, missing = 0)
  ) %>%
  ungroup()


#-------------------------------#
# 7.Alerta: Contenido basura    #
#-------------------------------#

# 1. Definir las variables de texto a auditar
vars_texto <- c("p3_03_otro",
                "p3_05_otro",
                "p3_06_otro",
                "p10_07",
                "p5_11_otro",
                "p5_17_otro",
                "p6_02_otro",
                "p7_02_otro",
                "p7_05",
                "p8_03",
                "p9_01_otro",
                "p9_02_otro")

detectar_basura_vectorizada <- function(columna) {
  # 1. Normalización inicial
  texto <- str_to_lower(str_trim(columna))
  texto <- stri_trans_general(texto, "Latin-ASCII") # Quita tildes para comparar mejor
  
  # 2. Patrones de teclado (Vectorizado)
  patrones_teclado <- "qwer|asdf|sdfg|dfgh|ghjk|zxcv|cvbn|vbnm|wert|tyui|uio|erwe"
  alerta_teclado <- str_detect(texto, patrones_teclado)
  
  # 3. Ratio de letras únicas (Vectorizado)
  # Dividimos cantidad de caracteres únicos por longitud total
  letras_unicas <- sapply(str_split(texto, ""), function(x) length(unique(x)))
  ratio_unicas <- letras_unicas / nchar(texto)
  
  # 4. Estructura lingüística (Consonantes seguidas y falta de vocales)
  consonantes_seguidas <- str_detect(texto, "[bcdfghjklmnpqrstvwxyz]{3,}")
  sin_vocales <- !str_detect(texto, "[aeiou]")
  
  # Lógica de decisión combinada (vectorizada)
  resultado <- case_when(
    alerta_teclado ~ 1,
    ratio_unicas < 0.45 & nchar(texto) > 3 ~ 1, # Ejemplo: "aaaaa" o "ababab"
    sin_vocales & nchar(texto) >= 3 ~ 1,
    consonantes_seguidas & nchar(texto) > 4 ~ 1,
    TRUE ~ 0
  )
  
  return(resultado)
}

# --- Aplicación Eficiente ---
base_manga <- base_manga %>%
  mutate(across(
    all_of(intersect(vars_texto, names(.))),
    ~ detectar_basura_vectorizada(.x),
    .names = "trash_{.col}"
  )) %>%
  # Totalización optimizada
  mutate(
    total_texto_basura = rowSums(pick(starts_with("trash_")), na.rm = TRUE),
    flag_texto_basura = if_else(total_texto_basura > 0, 1, 0)
  )

#==========================#
### Crear alertas LOOKER ###
#==========================#

# Creamos todos los flags binarios (0 o 1)
base_manga <- base_manga %>%
  mutate(
    # 1. Flag Rechazo (Usando tu variable status_num: 5 es Rechazo)
    flag_rechazo = if_else(status_num == 5, 1, 0, missing = 0),
    
    # 2. Flag Saltos
    flag_saltos = if_else(total_saltos > 0, 1, 0, missing = 0),
    
    # 3. Flag Missings (Usando un umbral dinámico o simple)
    # Si no tienes umbral_missing_sd, podemos usar total_missing > 0
    flag_missing = if_else(total_missing > 0, 1, 0, missing = 0),
    
    # 4. Flag Tiempos (Corto o Excesivo)
    flag_duration_outlier = if_else(alerta_corto == 1 | alerta_excesivo == 1, 1, 0, missing = 0),
    
    # 5. Flag Valores Extremos Numéricos (Z-score > 3)
    # Sumamos todas las columnas que empiezan con "ex_"
    total_extremos = rowSums(across(starts_with("ex_")), na.rm = TRUE),
    flag_extreme_values = if_else(total_extremos > 0, 1, 0, missing = 0),
    
    # 6. Flag Duplicados
    flag_duplicated = if_else(total_dup == 1, 1, 0, missing = 0),
    
    # 7. Flag Georeferenciación (Fuera de rango > 10m)
    flag_geofencing = if_else(!is.na(distancia_m) & distancia_m > 10, 1, 0, missing = 0),
    
    # 8. Flag Exceso de NS/NR
    flag_nsnr = if_else(alerta_exceso_nsnr == 1, 1, 0, missing = 0)
  )

base_manga <- base_manga %>%
  mutate(
    # ÉXITO: Encuesta efectiva que no dispara ninguna alerta crítica
    Exito_Auditoria = if_else(
        flag_duration_outlier == 0 & 
        flag_extreme_values == 0 & 
        flag_duplicated == 0 & 
        flag_geofencing == 0 &
        flag_nsnr == 0 &
        flag_texto_basura == 0,
      1, 0
    ),
    
    # ALERTA: Cualquier encuesta que deba ser revisada por el supervisor
    Alerta_Auditoria = if_else(
        flag_duration_outlier == 1 | 
        flag_extreme_values == 1 | 
        flag_duplicated == 1 | 
        flag_geofencing == 1 |
        flag_nsnr == 1 |
        flag_missing == 1 |
        flag_saltos == 1 |
        flag_texto_basura == 1,
      1, 0
    ),
    
    # Etiquetas de texto para los filtros de Looker (Sí/No)
    Rechazo_lbl          = if_else(flag_rechazo == 1, "Sí", "No"),
    Tiempos_Anomalos_lbl = if_else(flag_duration_outlier == 1, "Anómalo", "Normal"),
    ID_Repetido_lbl      = if_else(flag_duplicated == 1, "Duplicado", "Único"),
    Fuera_Rango_Geo_lbl  = if_else(flag_geofencing == 1, "Fuera de Padrones", "OK"),
    Exceso_NSNR_lbl      = if_else(flag_nsnr == 1, "Sí", "No"),
    Tiene_Missings_lbl   = if_else(flag_missing == 1, "Sí", "No"),
    Tiene_Saltos_lbl     = if_else(flag_saltos == 1, "Sí", "No"),
    Contenido_basura_lbl = if_else(flag_texto_basura == 1, "Sí", "No")
  )

#---------------------------------------------------#
#    Exportación a Google Sheets                    #
#---------------------------------------------------#

# 1. Convertir la geometría a columnas de texto/número y eliminar el objeto espacial
base_para_looker <- base_manga %>%
  # Extraemos las coordenadas de la columna geometry
  mutate(
    lat_final = st_coordinates(geometry)[,2],
    lon_final = st_coordinates(geometry)[,1]
  ) %>%
  # Eliminamos la columna de geometría para que no dé error
  st_drop_geometry() %>%
  # Opcional: convertir a data.frame puro para asegurar compatibilidad
  as.data.frame()

# Sustituye con el ID o URL de tu Google Sheet
id_sheet <- "https://docs.google.com/spreadsheets/d/1bY3Ua6IpZOWrX99Mr2SenWUgByhRIbBk1Bce3oRcmvw/edit?gid=527719951#gid=527719951"

# Escribir en la hoja (si la hoja no existe, la crea; si existe, la sobrescribe)
sheet_write(base_para_looker, ss = id_sheet, sheet = "base_manga_looker") 
