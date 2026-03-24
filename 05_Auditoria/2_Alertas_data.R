
# Leer el archivo CSV con la ruta corregida
base_manga <- final_data

base_manga <- base_manga %>%
  relocate(key, .before = padron_pull)

# --- SECCIÓN DE LIMPIEZA INICIAL ---
# Lista de IDs (instanceID o id_unico) que deben eliminarse definitivamente
ids_a_eliminar <- c(
  "uuid:57dbdf02-f95c-4e09-8614-6df9ae81f309",
  "uuid:5d8d35c8-c43f-4935-a8ff-ff6388f03bd1",
  "uuid:51b19331-23f1-420f-8ca2-70f17b0f343c",
  "uuid:af9818e4-e7ae-485b-844f-ee139dc0beaa",
  "uuid:bf556c11-a005-4c3d-9d74-26654f488f1f",
  "uuid:dae55bf4-69ad-4a49-9236-8d17985b4705"
)

base_manga <- base_manga %>%
  filter(!(key %in% ids_a_eliminar))

print(paste("Encuestas eliminadas por blacklist:", length(ids_a_eliminar)))

base_manga <- base_manga %>%
  filter(!(day == "Mar 23, 2026"))

# --- DICCIONARIO DE CORRECCIONES MANUALES ---
# Agrega aquí cualquier KEY que necesite corrección de ID
correcciones_manuales <- tribble(
  ~key,                                         ~id_correcto,
  "uuid:9029d292-00c6-4fc4-b59e-c8a5ffdc7c4d",  "164079",
)

# --- APLICACIÓN AUTOMÁTICA DE CORRECCIONES ---
base_manga <- base_manga %>%
  left_join(correcciones_manuales, by = "key") %>%
  mutate(
    # Si existe un id_correcto en nuestro diccionario, lo usamos; 
    # si no (NA), mantenemos el original.
    id = if_else(!is.na(id_correcto), id_correcto, as.character(id)),
    padron_pull = if_else(!is.na(id_correcto), id_correcto, as.character(padron_pull))
  ) %>%
  select(-id_correcto) # Eliminamos la columna auxiliar para mantener limpia la base

# Verificación en consola (opcional)
print(paste("Correcciones manuales aplicadas:", nrow(correcciones_manuales)))


# --- DICCIONARIO DE CORRECCIONES DE STATUS ---
# Para forzar un cambio de resultado en encuestas específicas
correcciones_status <- tribble(
  ~key,                                         ~status_nuevo,
  "uuid:a95eef92-c5a1-40af-9c84-41fbfb5332ec",  5, # Código 5 = RECHAZO
)

# --- APLICACIÓN DE CORRECCIONES DE STATUS ---
base_manga <- base_manga %>%
  left_join(correcciones_status, by = "key") %>%
  mutate(
    # Cambiamos el código numérico principal
    status_survey = if_else(!is.na(status_nuevo), as.character(status_nuevo), as.character(status_survey)),
    
    # CAMBIO SOLICITADO: Ajustamos las columnas de salida/publicación
    pub_status = if_else(!is.na(status_nuevo), "RECHAZO", as.character(pub_status)),
    
    # Ajustamos la respuesta de texto (Label)
    status_survey_resp = if_else(!is.na(status_nuevo), "RECHAZO", as.character(status_survey_resp))
  ) %>%
  select(-status_nuevo) # Limpiamos la columna auxiliar

# --- CORRECIÓN DE FECHA ---

base_manga <- base_manga %>%
  mutate(
    across(starts_with("fcs"), as.numeric),
    across(starts_with("rcsi"), as.numeric),
    across(starts_with("hhs"), as.numeric),
    submission_date = mdy_hms(submission_date, tz = "UTC"),
    # Ajuste para Uruguay (UTC-3)
    submission_date = submission_date - hours(3)
  ) %>%
  filter(submission_date >= ymd("2026-02-27"))

# --- FLUJO PRINCIPAL DE AUDITORÍA ---
# 2. CLASIFICACIÓN DE AUDITORÍA (CORREGIDO)
base_manga <- base_manga %>%
  mutate(
    status_num = as.numeric(status_survey),
    # Creamos las categorías maestras
    es_efectiva = if_else(status_num == 1, 1, 0),
    es_rechazo   = if_else(status_num == 5, 1, 0),
    es_no_elegible = if_else(status_num %in% c(6, 7,8, 9, 10), 1, 0),
    es_agendada  = if_else(status_num %in% c(2, 3, 4), 1, 0),
    
    # Esta etiqueta es la que usaremos para promedios y grupos
    categoria_auditoria = case_when(
      es_efectiva == 1    ~ "EFECTIVA",
      es_rechazo == 1     ~ "RECHAZO",
      es_no_elegible == 1 ~ "NO ELEGIBLE",
      es_agendada == 1    ~ "AGENDADA/PENDIENTE",
      TRUE                ~ "OTRO/ERROR"
    )
  )

# 3. SELECCIÓN DE LA MEJOR VISITA POR PADRÓN
base_manga <- base_manga %>%
  group_by(padron_pull) %>%
  mutate(
    # Prioridad: Efectiva > Agendada > Rechazo > No Elegible
    prioridad_status = case_when(
      categoria_auditoria == "EFECTIVA" ~ 4,
      categoria_auditoria == "AGENDADA/PENDIENTE" ~ 3,
      categoria_auditoria == "RECHAZO" ~ 2,
      categoria_auditoria == "NO ELEGIBLE" ~ 1,
      TRUE ~ 0
    )
  ) %>%
  # Ordenamos por prioridad y luego por la visita más alta
  arrange(padron_pull, desc(submission_date)) %>%
  mutate(encuesta_final = if_else(row_number() == 1, 1, 0)) %>%
  ungroup()

# Verificación rápida
table(base_manga$encuesta_final, base_manga$categoria_auditoria)

otro <- base_manga %>%
  filter(encuesta_final==0 & categoria_auditoria == "EFECTIVA" )


# Suponiendo que tu base se llama base_manga y la columna de texto es comentarios_pub
base_manga <- base_manga %>%
  mutate(
    categoria_comentario = case_when(
      # 1. INFRAESTRUCTURA TÉCNICA
      str_detect(str_to_lower(pub_comentario), "colector|frentista|camara|pozo|conexion|saneamiento") ~ "Infraestructura/Técnico",
      
      # 2. AUSENTISMO (Logística)
      str_detect(str_to_lower(pub_comentario), "nadie|no hay nadie|no atendio|no responde|ausente|vuelv|mañana|cita") ~ "Ausentismo/Revisita",
      
      # 3. RECHAZOS
      str_detect(str_to_lower(pub_comentario), "rechazo|no quiso|no quiere|disconformidad|no dispuesta") ~ "Rechazo",
      
      # 4. CONDICIÓN DEL PADRÓN (No elegibles)
      str_detect(str_to_lower(pub_comentario), "desocupado|baldio|construccion|vacia|comercio|deposito|inau|iglesia|usina") ~ "Padrón No Elegible",
      
      # 5. DESCRIPTIVOS / REFERENCIAS
      str_detect(str_to_lower(pub_comentario), "casa|puerta|reja|jardin|ladrillo|fachada|techo|cerco") ~ "Descripción Física",
      
      # 6. ÉXITO / TRÁMITE NORMAL
      str_detect(str_to_lower(pub_comentario), "completa|fluida|buena|correcta|terminada|finalizada") ~ "Encuesta Exitosa",
      
      # 7. CASOS VACÍOS O PUNTOS
      is.na(pub_comentario) | pub_comentario %in% c(".", "") ~ "Sin Comentario Real",
      
      # Categoría por defecto si no entra en ninguna
      TRUE ~ "Otros/Revisión Manual"
    )
  ) %>%
  # Mover la variable recién creada después de comentarios_pub
  relocate(categoria_comentario, .after = pub_comentario)

# Verificación de cuántos cayeron en cada bolsa
table(base_manga$categoria_comentario)

coment <- base_manga %>%
  select(pub_comentario, categoria_comentario)


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

base_manga <- base_manga %>%
  filter(encuesta_final==1)

#### Alertas ####
#---------------#

#### ALERTA DE TIEMPO ####

# 2. Alerta de Tiempo usando tu lógica pero agrupada
base_manga <- base_manga %>%
  mutate(duration_min = as.numeric(duration) / 60) %>%
  group_by(categoria_auditoria) %>% # <--- La clave está aquí
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

table(base_manga$alerta_corto, base_manga$categoria_auditoria)
table(base_manga$alerta_excesivo, base_manga$categoria_auditoria)

#### ALERTA DE MISSINGS ####


ODK_filtrado <- odkmissing::import_odk_propagate_required("05_Auditoria/ODK/encuestadores_Campo_missings.xlsx", required_value = "yes")
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
    total_saltos = rowSums(dplyr::pick(dplyr::all_of(variables_saltos)), na.rm = TRUE)
  )

library(dplyr)
library(tidyr)

# 1. Identificamos todas las columnas que empiezan con m_ o s_
variables_audit <- names(base_manga)[grepl("^[ms]_", names(base_manga))]

# 2. Creamos la tabla de "Hallazgos"
tabla_errores_por_padron <- base_manga |>
  # Seleccionamos el ID y las columnas de flags
  dplyr::select(padron_pull, dplyr::all_of(variables_audit)) |>
  # Pasamos a formato largo: una fila por cada variable con flag
  tidyr::pivot_longer(
    cols = -padron_pull,
    names_to = "variable_con_problema",
    values_to = "valor"
  ) |>
  # Filtramos para quedarnos SOLO con los casos donde hay un 1 (error/salto)
  dplyr::filter(valor == 1) |>
  # Opcional: Limpiar el nombre para que sea más legible
  dplyr::mutate(
    tipo_incidencia = ifelse(grepl("^m_", variable_con_problema), "Missing", "Salto"),
    variable_original = gsub("^[ms]_", "", variable_con_problema)
  ) |>
  # Ordenamos por padrón para que sea fácil de leer
  dplyr::arrange(padron_pull)

# 3. Ver el resultado
print(tabla_errores_por_padron)

#---------------------------------------#
# Valores numéricos extremos #
#---------------------------------------#
# 1. Lista expandida de todas las variables integer
vars_to_check <- c("p1_06", "p1_07", "p1_08a", "p2_02", "p3_06", 
                   "p4_01", "p4_02", "p4_03", "p4_04", "p10_03", 
                   "p5_09", "p5_19", "p8_01")

# 2. Aplicamos lógica masiva con protección de varianza
base_manga <- base_manga %>%
  group_by(categoria_auditoria) %>% # Agrupamos para que la media sea justa (Efectivas vs Resto)
  mutate(across(
    all_of(intersect(vars_to_check, names(.))),
    .fns = list(
      ex = ~ {
        val <- as.numeric(.x)
        clean_val <- ifelse(val %in% c(-1, 98, 99), NA, val)
        
        mu <- mean(clean_val, na.rm = TRUE)
        sigma <- sd(clean_val, na.rm = TRUE)
        
        # Lógica de Alerta:
        # - El valor no debe ser NA
        # - Sigma debe ser mayor a 0 (si todos pusieron lo mismo, no hay outlier)
        # - El valor debe estar a más de 3 desviaciones estándar
        # - El valor debe ser distinto a la media (evita errores de precisión)
        ifelse(!is.na(sigma) & sigma > 0 & abs(val - mu) > (3 * sigma) & val != mu, 1, 0)
      },
      # Guardamos la media para el reporte de gestión
      mu = ~ mean(as.numeric(ifelse(.x %in% c(-1, 98, 99), NA, .x)), na.rm = TRUE)
    ),
    .names = "{.fn}_{.col}" # Crea ex_p4_04 y mu_p4_04
  )) %>%
  ungroup()

# 3. Totalizar alertas numéricas
base_manga <- base_manga %>%
  mutate(
    total_extremos = rowSums(across(starts_with("ex_")), na.rm = TRUE),
    flag_extreme_values = if_else(total_extremos > 0, 1, 0)
  )

# --- VERIFICACIÓN EN CONSOLA ---
print("Resumen de Outliers por Variable:")
base_manga %>% summarise(across(starts_with("ex_"), ~ sum(.x, na.rm = TRUE)))


#-----------------------#
#  Alerta: Duplicados   #
#-----------------------#
 caract_especi_mayus <- c("Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", "Ú" = "U", "Ñ" = "N")
 
base_manga <- base_manga %>%
  mutate(
    id_unico = case_when(
      status_survey == 1 ~ paste(p11_02_nombre, p11_02_apellido, p11_03a, sep = "_"),
      status_survey == 4 ~ paste(id,nombre_contacto, telefono, sep = "_"),
      status_survey !=4 &  status_survey !=1 ~ paste(padron_pull, status_survey_resp, sep = "_"),
      TRUE ~ as.character(NA)
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
dup <- base_manga[, c("total_dup","id", "day" ,"id_unico", "status_survey_resp","p11_02_nombre","nombre_contacto", "key")]


table(base_manga$total_dup)

#### ALERTA GEOREFERENCIACIÓN ####
url_maestra <- "https://docs.google.com/spreadsheets/d/1brA0QxuJqCq8UAE4Umc-Ms89l06OEhtR-9DmgFMv1Pg/edit#gid=0"
maestra_poligonos <- read_sheet(url_maestra)

# 1. Preparar la Maestra de Polígonos
maestra_data <- maestra_poligonos %>%
  filter(!is.na(wkt_geom)) %>%
  mutate(padron = as.character(padron))

# Convertir la columna de texto WKT a objetos espaciales reales
# IMPORTANTE: Definimos que el origen es 32721 porque tus datos tienen números grandes (metros)
maestra_sf <- st_as_sf(maestra_data, wkt = "wkt_geom", crs = 32721)

# 2. Procesar Base Manga (Tus Puntos)
base_manga_sf <- base_manga %>%
  mutate(
    latitud = as.numeric(georeferenciacion_latitude),
    longitud = as.numeric(georeferenciacion_longitude),
    padron_pull = as.character(padron_pull) # ELIMINADA la línea que forzaba el "129431"
  ) %>%
  filter(!is.na(latitud) & !is.na(longitud)) %>%
  # Creamos el objeto espacial en WGS84 (4326)
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326) %>%
  # Lo transformamos a UTM (32721) para que coincida con los polígonos y medir en METROS
  st_transform(32721)

# 3. Join de datos
# Convertimos maestra_sf a un df normal para que el join no de error de clase 'sf'
# Pero mantenemos la columna de geometría (wkt_geom) intacta
maestra_join <- maestra_sf %>% 
  as.data.frame() %>% 
  select(padron, wkt_geom)

base_manga_sf <- base_manga_sf %>%
  left_join(maestra_join, by = c("padron_pull" = "padron")) %>%
  # Ahora tenemos 'geometry' (puntos) y 'wkt_geom' (polígonos)
  rename(geom_punto = geometry, geom_poligono = wkt_geom)

# 4. Cálculo de distancia fila por fila
# Es fundamental usar 'by_element = TRUE' para que compare 
# el punto 1 con el polígono 1, el punto 2 con el polígono 2, etc.
base_manga_sf$distancia_m <- as.numeric(
  st_distance(base_manga_sf$geom_punto, base_manga_sf$geom_poligono, by_element = TRUE)
)

# 5. Verificación de resultados
summary(base_manga_sf$distancia_m)

# 5. Crear Alerta
base_manga_sf <- base_manga_sf %>%
  mutate(alerta_geo = ifelse(distancia_m > 20, "REVISAR: Fuera de rango", "OK"))

# Volver a dataframe normal si es necesario para Looker
base_manga <- base_manga_sf %>% st_drop_geometry() %>% as.data.frame()

# Selecciona las columnas 'nombre', 'apellido' y 'id' del dataframe original
distancia <- base_manga[, c("alerta_geo", "distancia_m", "status_survey_resp","gps_precision","key")]



#### ALERTA NSNR ####

# 1. Definimos la lista expandida (Aseguramos que existan en la base)
vars_nsnr <- c( "p4_05", "p5_08", "p5_19", "p8_01", 
               "p3_07a", "p3_07b", "p3_07c", "p3_07d", "p3_07e", 
               "p3_07f", "p3_07g", "p3_07h", "p3_07i", "p3_07j", 
               "p5_06", "p5_07", "p5_11")

# --- 1. CONTEO DE NS/NR (Fuerza valores a texto para evitar fallos) ---
base_manga <- base_manga %>%
  mutate(
    # Cuenta los NS/NR tratándolos siempre como texto
    total_nsnr = rowSums(across(all_of(intersect(vars_nsnr, names(.))), 
                                ~ str_trim(as.character(.x)) %in% c("-1", "98", "99")), na.rm = TRUE),
    
    preguntas_vistas = rowSums(!is.na(across(all_of(intersect(vars_nsnr, names(.)))))),
    tasa_nsnr = if_else(preguntas_vistas > 0, (total_nsnr / preguntas_vistas) * 100, 0)
  )

# --- 2. ALERTAS (Bajamos el piso a 3 para que sea más sensible) ---
base_manga <- base_manga %>%
  group_by(categoria_auditoria) %>%
  mutate(
    mu_nsnr = mean(total_nsnr[es_efectiva == 1], na.rm = TRUE),
    sd_nsnr = sd(total_nsnr[es_efectiva == 1], na.rm = TRUE),
    
    # Bajamos el mínimo a 3 respuestas NS/NR para activar la alerta
    umbral_dinamico = pmax(mu_nsnr + (3 * sd_nsnr), 3, na.rm = TRUE),
    
    alerta_exceso_nsnr = if_else(
      es_efectiva == 1 & (total_nsnr >= umbral_dinamico | tasa_nsnr > 30), 
      1, 0, missing = 0
    )
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
  # 1. Limpieza inicial
  texto <- str_to_lower(str_trim(columna))
  texto <- stri_trans_general(texto, "Latin-ASCII")
  
  # 2. Conteo de palabras (Separadas por espacios)
  # str_count cuenta cuántos grupos de caracteres hay
  num_palabras <- str_count(texto, "\\S+")
  n_char <- nchar(texto)
  
  # 3. Patrones de "Teclazo" (Aumentamos a 5 caracteres para evitar falsos positivos)
  patrones_teclado <- "qwer|asdf|sdfg|dfgh|ghjk|zxcv|cvbn|vbnm|wert|tyui|uio"
  alerta_teclado <- str_detect(texto, patrones_teclado)
  
  # 4. Análisis de calidad
  letras_unicas <- sapply(str_split(texto, ""), function(x) length(unique(x)))
  ratio_unicas <- letras_unicas / n_char
  
  consonantes_seguidas <- str_detect(texto, "[bcdfghjklmnpqrstvwxyz]{4,}") # Subimos a 4
  sin_vocales <- !str_detect(texto, "[aeiou]")
  
  # --- LÓGICA DE DECISIÓN REFINADA ---
  resultado <- case_when(
    # SI TIENE 3 O MÁS PALABRAS, asumimos que es una observación válida (indulto)
    num_palabras >= 3 ~ 0,
    
    # Si está vacío o es NA
    is.na(texto) | texto == "" ~ 0,
    
    # Reglas de basura para textos cortos (1 o 2 palabras)
    alerta_teclado ~ 1,
    ratio_unicas < 0.35 & n_char > 5 ~ 1,  # Ej: "aaaaaaaaa"
    sin_vocales & n_char >= 4 ~ 1,         # Ej: "pndx" (muy corto sin vocales)
    consonantes_seguidas & n_char > 5 ~ 1, # Ej: "dsfghj"
    
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


base_manga_clear <- base_manga %>%
  select(!starts_with("m_"))
base_manga_clear <- base_manga_clear %>%
  select(!starts_with("s_"))
base_manga_clear <- base_manga_clear %>%
  select(!starts_with("ex_"))
base_manga_clear <- base_manga_clear %>%
  select(!starts_with("trash_"))

# Generamos el resumen agrupado por día con las variables correctas
resumen_diario_vertical <- base_manga %>%
  filter(encuesta_final == 1) %>% 
  group_by(day) %>%
  summarise(
    `Total Manzanas`          = n_distinct(manzana_pull),
    `Encuestas Efectivas`     = sum(es_efectiva, na.rm = TRUE),
    `Rechazos`                = sum(es_rechazo, na.rm = TRUE),
    `No Elegibles (Cierres)`  = sum(es_no_elegible, na.rm = TRUE),
    `Agendadas (Pendientes)`  = sum(es_agendada, na.rm = TRUE),
    `Sin Colector (Frentista)`= sum(p1_01 == 2, na.rm = TRUE),
    `Alertas de GPS (>10m)`   = sum(flag_geofencing, na.rm = TRUE),
    `Alertas Texto Basura`    = sum(flag_texto_basura, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = -day, 
    names_to = "Indicador", 
    values_to = "Valor"
  ) %>%
  arrange(desc(day))

print(resumen_diario_vertical)