
# Creamos una copia de final_data
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
  "uuid:dae55bf4-69ad-4a49-9236-8d17985b4705",
  "uuid:5c61615f-73cd-4835-b76a-66d72a73281d"
)

base_manga <- base_manga %>%
  filter(!(key %in% ids_a_eliminar))

print(paste("Encuestas eliminadas por blacklist:", length(ids_a_eliminar)))

# Otras observaciones de prueba
base_manga <- base_manga %>%
  filter(!(day == "Mar 23, 2026"))

# Base para ver por día
base_manga_prueba <- base_manga %>%
  filter((day == "Mar 20, 2026"))


# --- DICCIONARIO DE CORRECCIONES MANUALES ---
# Agrega aquí cualquier KEY que necesite corrección de ID
correcciones_manuales <- tribble(
  ~key,                                         ~id_correcto,
  "uuid:9029d292-00c6-4fc4-b59e-c8a5ffdc7c4d",  "164079",
  "uuid:6fcf248c-e508-4282-b79d-db65c3487d5d", "129432",
  "uuid:8dfc622e-a716-4c7c-af15-7d70c73ebadc", "156050",
  "uuid:12bddf70-2967-4362-92aa-e69457e2db6d", "156073",
  "uuid:ba2e1d32-de1e-4035-a2cf-26268dffcdd7", "171718",
  "uuid:f756a0b3-2dbc-4c6c-8a79-9fdbe42bf0b2", "91431",
  "uuid:37caab2a-df0b-4859-97db-867798143707", "91436" #<- 91434 verificar luego
  
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
  "uuid:d7496e5f-d6e7-4dbf-80c0-142d38344efb", 5,
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

base_manga <- base_manga %>%
  mutate(
    across(starts_with("fcs"), as.numeric),
    across(starts_with("rcsi"), as.numeric),
    across(starts_with("hhs"), as.numeric),
    starttime = mdy_hms(starttime, tz = "UTC"),
    # Ajuste para Uruguay (UTC-3)
    starttime = starttime - hours(3)
  ) %>%
  filter(starttime >= ymd("2026-02-27"))


base_manga <- base_manga %>%
  mutate(p1_06 = coalesce(p1_06, p1_08a),
         p1_08_resp = coalesce(p1_08_resp, p1_08_sel_resp))


# --- FLUJO PRINCIPAL DE AUDITORÍA ---
# 1. CLASIFICACIÓN DE AUDITORÍA (CORREGIDO)
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

# 2. SELECCIÓN DE LA MEJOR VISITA POR PADRÓN
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

# --- ANÁLISIS DE PADRONES CON MÚLTIPLES OBSERVACIONES Y ALERTAS COMPLETAS ---
analisis_duplicados <- base_manga %>%
  group_by(padron_pull) %>%
  # 1. Filtramos solo padrones que aparecen más de una vez
  filter(n() > 1) %>%
  # 2. Ordenamos cronológicamente
  arrange(padron_pull, submission_date) %>%
  mutate(
    # Lógica de detección de cierre previo
    cierre_previo = lag(cumany(categoria_auditoria %in% c("EFECTIVA", "NO ELEGIBLE"))),
    cierre_previo = coalesce(cierre_previo, FALSE),
    
    alerta_auditoria = case_when(
      cierre_previo & !(categoria_auditoria %in% c("EFECTIVA", "NO ELEGIBLE")) ~ "⚠️ REVISITAR TRAS CIERRE",
      cierre_previo & (categoria_auditoria %in% c("EFECTIVA", "NO ELEGIBLE")) ~ "⚠️ DOBLE CIERRE",
      TRUE ~ "Seguimiento normal"
    )
  ) %>%
  # 3. FILTRO CRÍTICO: Si el padrón tiene ALGUNA alerta, mostramos TODAS sus visitas
  filter(any(str_detect(alerta_auditoria, "⚠️"))) %>%
  ungroup() %>%
  
  # 4. Selección de columnas
  select(
    padron      = padron_pull,
    status      = categoria_auditoria,
    comentario  = pub_comentario,
    fecha_envio = submission_date,
    fecha_inicio = starttime,
    day,
    alerta      = alerta_auditoria,
    tecnico_pull,
    grupo_experimento,
    p11_02_nombre,
    p2_02,
    pub_direccion,
    key
  )


# --- LIMPIEZA DE GRUPO_EXPERIMENTO SEGÚN STATUS ---

base_manga <- base_manga %>%
  mutate(
    # Transformamos grupo_experimento basándonos en la condición de status
    grupo_experimento = case_when(
      status_num %in% c(1, 3, 5) ~ grupo_experimento,
      TRUE ~ NA_character_ # Para todo lo demás (2, 4, 6, etc.) asignamos NA
    )
  )

check_limpieza <- base_manga %>%
  group_by(status_num, grupo_experimento) %>%
  summarise(conteo = n(), .groups = "drop")

####################################################
# BASE DE DATOS: HISTORIAL DE PARCIALMENTE COMPLETAS
####################################################

base_parcialmente_completas <- base_manga %>%
  # 1. Agrupamos por padrón para analizar su historia completa
  group_by(padron_pull) %>%
  
  # 2. Filtramos: Solo nos quedamos con los padrones que tienen 
  # AL MENOS una observación con status_num == 3
  filter(any(status_num == 3, na.rm = TRUE)) %>%
  
  # 3. Ordenamos cronológicamente para leer los comentarios en orden
  arrange(padron_pull, starttime) %>%
  
  # 4. Seleccionamos las columnas de interés para tu revisión
  select(
    id_padron = padron_pull,
    fecha = starttime,
    tecnico = tecnico_pull,
    encuestador = ident_enc_resp,
    status = status_survey_resp,
    status_num,
    grupo_experimento,
    comentario = pub_comentario,
    key
  ) %>%
  ungroup()

# --- VERIFICACIÓN ---
# Cuántos padrones únicos tienen este problema:
n_padrones_parciales <- n_distinct(base_parcialmente_completas$id_padron)
print(paste("Se encontraron", n_padrones_parciales, "padrones con al menos una carga parcial."))


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

# BASE DE DATOS DE CASOS
base_manga_casos <- base_manga 

# --- CREACIÓN DE BASE_MANGA_CASOS ---
base_manga_casos_clean <- base_manga_casos %>%
  # 1. Seleccionamos lo esencial para el historial de visitas
  select(
    id_padron = padron_pull,
    manzana = manzana_pull,
    key, 
    fecha_visita = starttime,
    supervisor = supervisor_pull,
    tecnico = tecnico_pull,
    encuestador = ident_enc_resp ,
    status = status_survey_resp,
    resultado_visita = categoria_auditoria, # Ej: EFECTIVA, RECHAZO, NADIE EN CASA
  ) %>%
  # 2. Ordenamos cronológicamente por padrón para ver la historia de cada uno
  arrange(id_padron, fecha_visita) %>%
  # 3. Creamos un contador de intentos real por si el orden_visita del ODK falló
  group_by(id_padron) %>%
  mutate(intento = row_number()) %>%
  ungroup()

base_manga_casos_clean <- base_manga_casos_clean %>%
  group_by(id_padron) %>%
  # Ordenamos por prioridad y luego por la visita más alta
  arrange(id_padron, desc(fecha_visita)) %>%
  mutate(encuesta_final = if_else(row_number() == 1, 1, 0)) %>%
  ungroup()


# Ver los primeros casos
head(base_manga_casos_clean)

# BASE DE DATOS DE ENCUESTAS FINALES 
base_manga <- base_manga %>%
  filter(encuesta_final==1)

# --- CREACIÓN DE FLAG DE AUDIO ---
base_manga <- base_manga %>%
  mutate(
    # Si audio_encuesta ES NA, ponemos 0. Si TIENE contenido (no es NA), ponemos 1.
    tiene_audio = if_else(!is.na(audio_encuesta), 1, 0)
  )

# --- VERIFICACIÓN ---
# Para ver cuántas encuestas tienen audio vs cuántas no
table(base_manga$tiene_audio, useNA = "always")

base_manga_efect <- base_manga %>%
  filter(categoria_auditoria == "EFECTIVA")
# Muestra: Filas (Día) vs Columnas (Tiene Audio: 0=No, 1=Sí)
table(base_manga_efect$day, base_manga_efect$tiene_audio)

# Muestra: Filas (Día) vs Columnas (Tiene Audio: 0=No, 1=Sí)
table(base_manga_efect$day, base_manga_efect$p12_01)

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
    umbral_excesivo = media_grupo + (5 * sd_grupo),
    
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

# 1. Identificamos todas las columnas que empiezan con m_ o s_
variables_audit <- names(base_manga)[grepl("^[ms]_", names(base_manga))]

base_manga <- base_manga %>%
  mutate(s_status_survey = 0)

# 2. Creamos la tabla de "Hallazgos"
tabla_errores_por_padron <- base_manga |>
  # Seleccionamos el ID y las columnas de flags
  dplyr::select(padron_pull, day, formdef_version, dplyr::all_of(variables_audit)) |>
  # Pasamos a formato largo: una fila por cada variable con flag
  tidyr::pivot_longer(
    cols = -c(padron_pull, day, formdef_version),
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

# Selecciona las columnas 'nombre', 'apellido' y 'id' del dataframe original
missing <- base_manga |>
  filter(m_p5_11 == 1)
print(missing)


#---------------------------------------#
# Valores numéricos extremos #
#---------------------------------------#
# 1. Lista expandida de todas las variables integer
vars_to_check <- c("p1_06", "p1_07", "p2_02", "p3_06", 
                   "p4_01", "p10_03", 
                   "p5_09", "p5_19", "p8_01")

# 2. Aplicamos lógica masiva con protección de varianza
base_manga <- base_manga %>%
  group_by(categoria_auditoria) %>% # <--- La clave está aquí
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

####################################################
# RESUMEN ESTADÍSTICO Y AUDITORÍA POR PREGUNTA
####################################################

resumen_preguntas_criticas <- base_manga %>%
  # FILTRO CRÍTICO: Solo encuestas efectivas (ajusta el nombre de la columna si varía)
  filter(categoria_auditoria == "EFECTIVA") %>% 
  select(tecnico_pull, all_of(intersect(vars_to_check, names(.)))) %>%
  pivot_longer(
    cols = -tecnico_pull, 
    names_to = "Pregunta", 
    values_to = "Valor"
  ) %>%
  mutate(valor_limpio = ifelse(Valor %in% c(-1, 98, 99), NA, as.numeric(Valor))) %>%
  group_by(Pregunta) %>%
  summarise(
    Media = round(mean(valor_limpio, na.rm = TRUE), 2),
    Minimo = min(valor_limpio, na.rm = TRUE),
    Maximo = max(valor_limpio, na.rm = TRUE),
    
    # --- LOGICA CORREGIDA ---
    Tecnico_Mas_Alertas = {
      mu <- mean(valor_limpio, na.rm = TRUE)
      sigma <- sd(valor_limpio, na.rm = TRUE)
      
      # Creamos el conteo
      conteo <- tibble(tecnico_pull, valor_limpio) %>%
        mutate(es_outlier = if_else(!is.na(sigma) & sigma > 0 & abs(valor_limpio - mu) > (3 * sigma), 1, 0)) %>%
        group_by(tecnico_pull) %>%
        summarise(n_alertas = sum(es_outlier, na.rm = TRUE), .groups = "drop") %>%
        slice_max(n_alertas, n = 1, with_ties = FALSE)
      
      # SOLO devolvemos el nombre si n_alertas es mayor a 0
      if(nrow(conteo) > 0 && conteo$n_alertas > 0) {
        conteo$tecnico_pull
      } else {
        NA_character_  # O puedes poner "Sin Alertas"
      }
    },
    
    Alertas_Totales_Pregunta = {
      mu <- mean(valor_limpio, na.rm = TRUE)
      sigma <- sd(valor_limpio, na.rm = TRUE)
      sum(abs(valor_limpio - mu) > (3 * sigma), na.rm = TRUE)
    },
    
    .groups = "drop"
  ) %>%
  arrange(desc(Alertas_Totales_Pregunta))

# --- VERIFICACIÓN ---
print(resumen_preguntas_criticas)


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


base_manga <- base_manga %>%
  mutate(
    nombre_dup = case_when(
      status_survey == 1 ~ paste(p11_02_nombre, p11_02_apellido, sep = "_"),
      status_survey == 4 ~ paste(id,nombre_contacto, sep = "_"),
      status_survey !=4 &  status_survey !=1 ~ paste(padron_pull, status_survey_resp, sep = "_"),
      TRUE ~ as.character(NA)
    )
  ) %>%
  mutate(
    nombre_dup = str_squish(str_replace_all(toupper(nombre_dup), caract_especi_mayus))
  )%>%
  group_by(nombre_dup) %>%
  mutate(
    total_dup_name = if_else(
      n() > 1 & nombre_dup != "", 1, 0
    )
  ) %>%
  ungroup()

# Selecciona las columnas 'nombre', 'apellido' y 'id' del dataframe original
dup <- base_manga[, c("total_dup","padron_pull", "tecnico_pull", "day" ,"id_unico", "status_survey_resp","p11_02_nombre","pub_comentario",  "key")]


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
  mutate(alerta_geo = ifelse(distancia_m > 15, "REVISAR: Fuera de rango", "OK"))

# Volver a dataframe normal si es necesario para Looker
base_manga <- base_manga_sf %>% st_drop_geometry() %>% as.data.frame()

# Selecciona las columnas 'nombre', 'apellido' y 'id' del dataframe original
distancia <- base_manga[, c("alerta_geo", "distancia_m", "status_survey_resp","gps_precision","key")]



#### ALERTA NSNR ####

# 1. Definimos la lista expandida (Aseguramos que existan en la base)
vars_nsnr <- c( "p4_04","p4_05", "p5_08", "p5_19", "p8_01", 
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
    mu_nsnr = mean(total_nsnr, na.rm = TRUE),
    sd_nsnr = sd(total_nsnr, na.rm = TRUE),
    
    # Bajamos el mínimo a 3 respuestas NS/NR para activar la alerta
    umbral_dinamico = pmax(mu_nsnr + (5 * sd_nsnr), 5, na.rm = TRUE),
    
    alerta_exceso_nsnr = if_else( (total_nsnr >= 4), 
      1, 0, missing = 0
    )
  ) %>%
  ungroup()


# Selecciona las columnas 'nombre', 'apellido' y 'id' del dataframe original
alerta_exceso_nsnr <- base_manga[, c("total_nsnr", "umbral_dinamico", "alerta_exceso_nsnr","key", "categoria_auditoria")]

# 1. Creamos el resumen solo para las encuestas efectivas
resumen_nsnr_preguntas <- base_manga %>%
  # FILTRO CRÍTICO: Solo encuestas efectivas (ajusta el nombre de la columna si varía)
  filter(categoria_auditoria == "EFECTIVA") %>% 
  
  # Seleccionamos técnico y las variables que definiste en vars_nsnr
  select(tecnico_pull, all_of(intersect(vars_nsnr, names(.)))) %>%
  
  # Pasamos a formato largo para analizar pregunta por pregunta
  pivot_longer(
    cols = -tecnico_pull, 
    names_to = "Pregunta", 
    values_to = "Valor"
  ) %>%
  
  # Identificamos si el valor es un NS/NR (1 si lo es, 0 si es respuesta válida)
  mutate(es_nsnr = if_else(str_trim(as.character(Valor)) %in% c("-1", "98", "99"), 1, 0, missing = 0)) %>%
  
  # Agrupamos por Pregunta para las estadísticas
  group_by(Pregunta) %>%
  summarise(
    Encuestas_Efectivas = n(),
    Total_NSNR = sum(es_nsnr, na.rm = TRUE),
    
    # % de incidencia de NS/NR en esta pregunta
    Tasa_Incidencia = round((Total_NSNR / Encuestas_Efectivas) * 100, 2),
    Tecnico_Mas_NSNR = {
      conteo_tec <- tibble(tecnico_pull, es_nsnr) %>%
        group_by(tecnico_pull) %>%
        summarise(n = sum(es_nsnr), .groups = "drop") %>%
        slice_max(n, n = 1, with_ties = FALSE)
      
      if(nrow(conteo_tec) > 0 && conteo_tec$n > 0) {
        conteo_tec$tecnico_pull
      } else {
        NA_character_
      }
    },
    Max_por_Tecnico = {
      max_n <- tibble(tecnico_pull, es_nsnr) %>%
        group_by(tecnico_pull) %>%
        summarise(n = sum(es_nsnr), .groups = "drop") %>%
        pull(n) %>% max(na.rm = TRUE)
      
      if(is.infinite(max_n)) 0 else max_n
    },
    
    .groups = "drop"
  ) %>%
  arrange(desc(Total_NSNR))

# --- VERIFICACIÓN ---
print("Resumen de NS/NR en Encuestas Efectivas:")
print(resumen_nsnr_preguntas)

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
    flag_geofencing = if_else(!is.na(distancia_m) & distancia_m > 15, 1, 0, missing = 0),
    
    # 8. Flag Exceso de NS/NR
    flag_nsnr = if_else(alerta_exceso_nsnr == 1, 1, 0, missing = 0)
  )

base_manga <- base_manga %>%
  mutate(
    # ÉXITO: Encuesta efectiva que no dispara ninguna alerta crítica
    Exito_Auditoria = if_else(
      (categoria_auditoria == "RECHAZO" |
      categoria_auditoria == "NO ELEGIBLE" ) | (
        flag_duration_outlier == 0 & 
        flag_extreme_values == 0 & 
        flag_duplicated == 0 & 
        flag_geofencing == 0 &
        flag_missing == 0 &
        flag_nsnr == 0 &
        flag_texto_basura == 0),
      1, 0
    ),
    
    # ALERTA: Cualquier encuesta que deba ser revisada por el supervisor
    Alerta_Auditoria = if_else( 
        categoria_auditoria == "EFECTIVA" & (
        flag_duration_outlier == 1 | 
        flag_extreme_values == 1 | 
        flag_duplicated == 1 | 
        flag_geofencing == 1 |
        flag_nsnr == 1 |
        flag_missing == 1 |
        flag_texto_basura == 1
        ),
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

# --- CREACIÓN DE BASE DE CONTROL DE AUDITORÍA ---
base_verificacion_audit <- base_manga %>%
  dplyr::select(
    key,                        # Identificador para saber de qué encuesta hablamos
    padron_pull,                # Para ubicarla rápido
    tecnico_pull,               # Para saber quién la hizo
    categoria_auditoria,
    p4_01,
    p4_02,
    p4_03,# El estado de la encuesta (Efectiva, Rechazo, etc.)
    
    # Seleccionamos todas las que empiezan con flag_
    starts_with("flag_"), 
    
    # Seleccionamos las variables de resultado de auditoría
    Exito_Auditoria, 
    Alerta_Auditoria,
    
    # Opcional: añadimos total_missing y total_saltos si los calculamos antes
    any_of(c("total_missing", "total_saltos"))
  )

# --- VERIFICACIÓN RÁPIDA ---
# 1. ¿Hay algún caso que sea Éxito y Alerta al mismo tiempo? (Debería dar 0 filas)
contradicciones <- base_verificacion_audit %>% 
  filter(Exito_Auditoria == 1 & Alerta_Auditoria == 1)

print(paste("Casos contradictorios encontrados:", nrow(contradicciones)))

base_manga_clear <- base_manga %>%
  select(!starts_with("m_"))
base_manga_clear <- base_manga_clear %>%
  select(!starts_with("s_"))
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

####################################################
# CONSOLIDADO DE ALERTAS POR MANZANA
####################################################

consolidado_alertas_manzana <- base_manga %>%
  # Trabajamos sobre las encuestas finales para no duplicar alertas
  filter(encuesta_final == 1) %>% 
  group_by(Manzana = paste("Manzana", str_extract(manzana_pull, "\\d+"))) %>%
  summarise(
    # 1. Dimensión de la Manzana
    Total_Padrones_Visitados = n(),
    Total_Efectivas = sum(es_efectiva, na.rm = TRUE),
    
    # 2. Resumen de Alertas Críticas (Conteo de incidencias)
    Alertas_GPS = sum(flag_geofencing, na.rm = TRUE),
    Alertas_Texto_Basura = sum(flag_texto_basura, na.rm = TRUE),
    Alertas_Tiempos = sum(flag_duration_outlier, na.rm = TRUE),
    Alertas_Duplicados = sum(flag_duplicated, na.rm = TRUE),
    Alertas_NSNR = sum(flag_nsnr, na.rm = TRUE),
    Alertas_Valores_Extremos = sum(flag_extreme_values, na.rm = TRUE),
    
    # 3. Resumen de Integridad (Missings y Saltos)
    Promedio_Missings = round(mean(total_missing, na.rm = TRUE), 2),
    Promedio_Saltos = round(mean(total_saltos, na.rm = TRUE), 2),
    
    # 4. Cálculo de "Salud de la Manzana"
    Encuestas_Con_Alerta = sum(Alerta_Auditoria, na.rm = TRUE),
    Encuestas_Exitosas = sum(Exito_Auditoria, na.rm = TRUE),
    
    # Porcentaje de Calidad: (Exitosas / Total Visitadas)
    Indice_Calidad = round((Encuestas_Exitosas / Total_Padrones_Visitados) * 100, 1),
    
    # Identificar Supervisores y Equipos involucrados
    Supervisor = first(supervisor_pull),
    Equipos = paste(unique(tecnico_pull), collapse = " | "),
    .groups = "drop"
  ) %>%
  # 5. Clasificación de la Manzana según su calidad
  mutate(
    Estado_Auditoria = case_when(
      Indice_Calidad >= 90 ~ "✅ ALTA CALIDAD",
      Indice_Calidad >= 70 ~ "⚠️ REVISIÓN LEVE",
      Indice_Calidad < 70  ~ "🚨 CRÍTICA: REVISAR EQUIPO",
      TRUE ~ "Sin Datos"
    )
  ) %>%
  # Ordenar por las más críticas primero
  arrange(Indice_Calidad)

# --- VERIFICACIÓN ---
print(consolidado_alertas_manzana)


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

####################################################
# FLUJO DE LIMPIEZA: BASE ESPEJO ODK (LABELS LIMPIOS)
####################################################

base_manga_clear <- base_manga_clear %>%
  # 1. Creamos el Label para asistencia (ahora p10_05a)
  mutate(
    p10_05a_label = case_when(
      asistencia == 1 ~ "SI",
      asistencia == 2 ~ "NO",
      TRUE ~ as.character(asistencia)
    ))

base_manga_clear <- base_manga_clear %>%
  # 1. Transformamos los códigos numéricos a Labels para toda la batería 3.07
  mutate(across(
    c(p3_07a, p3_07b, p3_07c, p3_07d, p3_07e, p3_07f, p3_07g, p3_07h, p3_07i, p3_07j),
    ~ case_when(
      . == 1  ~ "SI",
      . == 2  ~ "NO",
      . == 98 ~ "NO RESPONDE",
      TRUE    ~ as.character(.) # Por si hay algún NA o valor inesperado
    )
  ))

base_odk_limpia <- base_manga_clear %>%
  filter(Exito_Auditoria == 1) %>%
  arrange(starttime) %>%
  # 1. Conservamos Identificadores Clave (Metadata y Pulls)
  # 2. Seleccionamos solo las respuestas finales (Labels)
  # 3. Mantenemos variables de georreferencia y tiempos
  select(
    # --- Identificación ---
    day,
    key,
    padron = padron_pull,
    manzana = manzana_pull,
    tecnico = tecnico_pull,
    supervisor = supervisor_pull,
    encuestador = ident_enc_resp,
    submission_date,
    
    # --- georreferenciacion ---
    latitude = georeferenciacion_latitude,
    longitude = georeferenciacion_longitude,
    altitude = georeferenciacion_altitude,
    precision = georeferenciacion_accuracy,
    
    # --- Sección 1: Portada y Filtros ---
    p1_01 = p1_01_resp,
    p1_02 = p1_02_resp,
    p1_03 = p1_03_resp,
    p1_04 = p1_04_resp,
    p1_05 = p1_05_resp,
    p1_06, # Es integer, no tiene resp
    p1_07,
    p1_08 = p1_08_resp, # Variable calculada de contacto
    direccion,
    n_predio,
    piso,
    apartamento,
    referencia_padron,
    referencia_vivienda,
    p1_10 = p1_10_resp,
    p1_11 = p1_11_resp,
    p1_12 = p1_12_resp,
    
    # --- Sección 2 y 3: Perfil y Vivienda ---
    p2_01 = p2_01_resp,
    p2_02, 
    p2_03 = p2_03_resp,
    p2_04 = p2_04_resp,
    p2_05 = p2_05_resp,
    p3_01 = p3_01_resp,
    p3_02 = p3_02_resp,
    p3_02_otro,
    p3_03 = p3_03_resp,
    p3_04 = p3_04_resp,
    p3_05 = p3_05_resp,
    p3_06,
    p3_07a, p3_07b, p3_07c, p3_07d, p3_07e, 
    p3_07f, p3_07g, p3_07h, p3_07i, p3_07j,
    
    # --- Sección 4 y 5: Hogar y Saneamiento ---
    p4_01, p4_02, p4_03, p4_04,
    p4_05 = p4_05_resp,
    p5_01 = p5_01_resp,
    p5_02 = p5_02_resp,
    p5_02a = p5_02a_resp,
    p5_03 = p5_03_resp,
    p5_04 = p5_04_resp,
    
    # --- Sección 10: Relevamiento técnico ---
    p10_00 = p10_00_resp,
    p10_01 = p10_01_resp,
    p10_02 = p10_02_resp,
    p10_03,
    p10_04 = p10_04_resp,
    p10_05 = p10_05_resp,
    p10_05a = p10_05a_label,
    p10_06 = p10_06_resp,
    p10_06a,
    p10_08 = p10_08_resp,
    fuente = fuente_resp,
    p10_09 = p10_09_resp,
    p10_09a = p10_09a_resp,
    grupo_experimento,
    
    p5_06 = p5_06_resp,
    p5_07 = p5_07_res, # Nota: En tu ODK pusiste _res
    p5_08 = p5_08_resp,
    p5_09,
    p5_11 = p5_11_resp,
    p5_11_otro,
    p5_12 = p5_12_resp,
    p5_14 = p5_14_resp,
    p5_15 = p5_15_resp,
    p5_16 = p5_16_resp,
    p5_17a = p5_17_a,
    p5_17b = p5_17_b,
    p5_17c = p5_17_c,
    p5_17d = p5_17_d,
    p5_17z = p5_17_z,
    p5_17_otro,
    p5_18 = p5_18_resp,
    p5_19,
    p5_20 = p5_20_resp,
    
    # --- Sección 6 y 7: Percepciones ---
    p6_01 = p6_01_resp,
    p6_02 = p6_02_resp,
    p6_02_otro,
    p7_01 = p7_01_resp,
    p7_02a = p7_02_a,
    p7_02b = p7_02_b,
    p7_02c = p7_02_c,
    p7_02d = p7_02_d,
    p7_02e = p7_02_e,
    p7_02f = p7_02_f,
    p7_02g = p7_02_g,
    p7_02h = p7_02_h,
    p7_02z = p7_02_z,
    p7_02_otro,
    p7_03 = p7_03_resp,
    p7_04 = p7_04_resp,
    p7_05,
    
    # --- Sección 8 y 9: Conexión ---
    p8_01,
    p8_02 = p8_02_resp,
    p8_03,
    p9_01a = p9_01_a,
    p9_01b = p9_01_b,
    p9_01c = p9_01_c,
    p9_01d = p9_01_d,
    p9_01e = p9_01_e,
    p9_01f = p9_01_f,
    p9_01g = p9_01_g,
    p9_01h = p9_01_h,
    p9_01i = p9_01_i,
    p9_01j = p9_01_j,
    p9_01k = p9_01_k,
    p9_01_otro,
    p9_02a = p9_02_a,
    p9_02b = p9_02_b,
    p9_02c = p9_02_c,
    p9_02d = p9_02_d,
    p9_02e = p9_02_e,
    p9_02f = p9_02_f,
    p9_02z = p9_02_z,
    p9_02_otro,
    p9_03 = p9_03_resp,
    
    # --- Sección 10 y 11: Relevamiento Técnico --
    p10_10 = p10_10_resp,
    p10_11 = p10_11_resp,
    p10_12 = p10_12_resp,
    p10_13 = p10_13_resp,
    
    # --- Recontacto y Cierre ---
    p11_01 = p11_01_resp,
    p11_02_nombre,
    p11_02_apellido,
    p11_03a, p11_03b, # Teléfonos
    p11_04 = p11_04_resp,
    p11_05 = p11_05_resp,
    p12_01 = p12_01_resp,
    status_survey = status_survey_resp,
    obs_finales,
    
    # --- Metadata de Calidad (Variables Mejoradas) ---
    categoria_auditoria
  )

# --- LIMPIEZA DE TEXTO ADICIONAL ---


print("Base limpia generada con éxito siguiendo el orden del ODK.")



####################################################
# REPORTE OPERATIVO HORIZONTAL (DÍAS POR COLUMNA)
####################################################

# 1. Calculamos la Sección de DETALLE (Solo encuestas efectivas/finales)
detalle_data <- base_manga %>%
  filter(encuesta_final == 1) %>% 
  group_by(day) %>%
  summarise(
    `1.1 Número de duplas (Tec+Enc)` = n_distinct(paste(tecnico_pull, ident_enc_resp)),
    `1.2 Padrones conectados (Red)`  = sum(as.numeric(p10_09) == 1 & status_num == 1, na.rm = TRUE),
    `1.3 Padrones no conectados`     = sum(as.numeric(p10_09) %in% 2:9 & status_num == 1, na.rm = TRUE),
    `1.4 Esquema Funcional Simple`   = sum(grupo_experimento == "ESQUEMA FUNCIONAL SIMPLE" & status_num == 1, na.rm = TRUE),
    `1.5 Esquema Funcional Completo` = sum(grupo_experimento == "ESQUEMA FUNCIONAL DETALLADO" & status_num == 1, na.rm = TRUE),
    `1.6 Grupo Control`              = sum(grupo_experimento == "CONTROL" & status_num == 1, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Calculamos la Sección de STATUS (Usando la base de TODOS los intentos/casos)
status_data <- base_manga_casos %>%
  group_by(day) %>%
  summarise(
    `2.01 COMPLETA`               = sum(status_num == 1, na.rm = TRUE),
    `2.02 OCUPANTES AUSENTES`     = sum(status_num == 2, na.rm = TRUE),
    `2.03 PARCIALMENTE COMPLETA`  = sum(status_num == 3, na.rm = TRUE),
    `2.04 CITA`                   = sum(status_num == 4, na.rm = TRUE),
    `2.05 RECHAZO`                = sum(status_num == 5, na.rm = TRUE),
    `2.06 VIVIENDA DESOCUPADA`    = sum(status_num == 6, na.rm = TRUE),
    `2.07 NO ES VIVIENDA / TERRENO / CONSTR.` = sum(status_num == 7, na.rm = TRUE),
    `2.08 NO TIENE BAÑO`          = sum(status_num == 8, na.rm = TRUE),
    `2.09 NO EXISTE RED SANEAMIENTO` = sum(status_num == 9, na.rm = TRUE),
    `2.10 PADRÓN > 10 VIVIENDAS`   = sum(status_num == 10, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Unimos ambas secciones y pivotamos
resumen_horizontal <- detalle_data %>%
  full_join(status_data, by = "day") %>%
  pivot_longer(cols = -day, names_to = "Indicador", values_to = "Valor") %>%
  # Sustituimos NAs por 0 para que la tabla sea legible
  mutate(Valor = coalesce(Valor, 0)) %>% 
  pivot_wider(names_from = day, values_from = Valor)

# --- VERIFICACIÓN ---
print(resumen_horizontal)
