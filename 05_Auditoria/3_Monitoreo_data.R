

# 1. LEER CASE MANAGEMENT Y ENUMERATORS
url_case_management <- "https://docs.google.com/spreadsheets/d/1sTN_sAiAoEGg30Y5Y2pC382ZVs5lD3WlFzWLPReuMsU/edit?gid=179862683#gid=179862683"
case_management <- read_sheet(url_case_management)

url_enumerators <- "https://docs.google.com/spreadsheets/d/1YcafSQeOn0183FXhMjHj8n6Nrebpc3BbIbgadFstPDU/edit?gid=0#gid=0"
enumerators_tab <- read_sheet(url_enumerators)


# 1. PREPARAR META ÚNICA POR TÉCNICO
####################################################
# Colapsamos las manzanas y sumamos padrones para evitar duplicados en el join
meta_tecnico_consolidada <- meta_resumen %>%
  group_by(Tecnico) %>%
  summarise(
    # Concatenamos los nombres de las manzanas en una sola celda
    Manzanas_Listado = paste(unique(Manzana), collapse = ", "),
    # Sumamos el total de padrones asignados al técnico en todas sus manzanas
    Total_Asignados = sum(Padrones_Asignados, na.rm = TRUE),
    # Nos quedamos con el primer supervisor que aparezca
    Supervisor_Asignado = first(Supervisor),
    .groups = "drop"
  )

####################################################
# 2. CONSOLIDADO MULTICATEGORÍA (AUDITORÍA)
####################################################
consolidado_final_manga <- base_manga %>%
  group_by(Tecnico = tecnico_pull, Encuestador = ident_enc_resp) %>% 
  summarise(
    Total_Visitas = n(),
    # Conteo de categorías finales (solo encuesta_final == 1)
    EFECTIVAS     = sum(encuesta_final == 1 & categoria_auditoria == "EFECTIVA", na.rm = TRUE),
    RECHAZOS      = sum(encuesta_final == 1 & categoria_auditoria == "RECHAZO", na.rm = TRUE),
    NO_ELEGIBLES  = sum(encuesta_final == 1 & categoria_auditoria == "NO ELEGIBLE", na.rm = TRUE),
    AGENDADAS     = sum(encuesta_final == 1 & categoria_auditoria == "AGENDADA/PENDIENTE", na.rm = TRUE),
    OTROS_ERRORES = sum(encuesta_final == 1 & categoria_auditoria == "OTRO/ERROR", na.rm = TRUE),
    
    # ALERTAS ESPECÍFICAS: Solo Efectivas (es_efectiva) que tienen Alerta_Auditoria
    EFECTIVAS_CON_ALERTA = sum(es_efectiva == 1 & Alerta_Auditoria == 1, na.rm = TRUE),
    
    TOTAL_ALERTAS_GENERAL = sum(Alerta_Auditoria == 1, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Ahora el join es seguro: 1 fila en meta por cada técnico
  left_join(meta_tecnico_consolidada, by = "Tecnico") %>%
  mutate(
    # Cobertura = (Efectivas + Rechazos + No Elegibles) / Meta Total
    `% Avance Cobertura` = round(((EFECTIVAS + RECHAZOS + NO_ELEGIBLES) / Total_Asignados) * 100, 1)
  ) %>%
  # Reemplazamos NAs por 0 en la meta por si algún técnico no está en el Case Management
  mutate(Total_Asignados = coalesce(Total_Asignados, 0)) %>%
  select(
    `Técnico` = Tecnico,
    `Encuestador` = Encuestador,
    `Manzanas` = Manzanas_Listado, # Ahora sí existe porque la creamos en meta_tecnico_consolidada
    `Metas (Asignados)` = Total_Asignados,
    `EFECTIVAS CON ALERTA` = EFECTIVAS,
    `EFECTIVAS CON ALERTA` = EFECTIVAS_CON_ALERTA,
    RECHAZOS,
    NO_ELEGIBLES,
    AGENDADAS,
    OTROS_ERRORES,
    `Total Alertas (Histórico)` = TOTAL_ALERTAS_GENERAL,
    `% Avance Cobertura`,
    Supervisor = Supervisor_Asignado
  )

print(consolidado_final_manga)

####################################################
# 4. EXPORTACIÓN
####################################################
id_sheet <- "https://docs.google.com/spreadsheets/d/1my_dgomVoXUI8ADPGIV8WyYek1qQtFuVsO_lvKxDhLY/edit"

# Escribir la nueva tabla multinivel
sheet_write(consolidado_final_manga, ss = id_sheet, sheet = "Consolidado_Tecnico_Encuestador")



# --- PASO A: Detalle de Texto Basura ---
# --- 1. DICCIONARIO DE ETIQUETAS (Para que el reporte sea legible) ---
# Usamos tu ODK_filtrado para mapear nombres técnicos a preguntas reales
diccionario_nombres <- ODK_filtrado %>% 
  select(name, label) %>% 
  mutate(
    # 1. Limpiamos etiquetas HTML
    label = str_replace_all(label, "<[^>]*>", ""),
    # 2. Corrección específica para variables con lógica dinámica (${...})
    label = case_when(
      name == "p5_19" ~ "5,19 ¿Cuánto gasta en saneamiento?",
      name == "p4_04" ~ "4,04 Ingresos mensuales del hogar",
      TRUE ~ label
    ),
    # 3. Cortamos el texto para que no desborde en Looker
    label = str_trunc(str_trim(label), 45) 
  ) %>% 
  # Evitamos duplicados si la variable aparece varias veces en el ODK
  group_by(name) %>% 
  summarise(label = first(label), .groups = "drop")

# --- 2. DETALLE DE OUTLIERS CON LABELS ---
detalle_extremos_manga <- base_manga %>%
  filter(flag_extreme_values == 1) %>%
  select(key, all_of(intersect(vars_to_check, names(.))), starts_with("ex_")) %>%
  tidyr::pivot_longer(cols = all_of(intersect(vars_to_check, names(.))), names_to = "name", values_to = "valor") %>%
  tidyr::pivot_longer(cols = starts_with("ex_"), names_to = "name_ex", values_to = "es_extremo") %>%
  filter(es_extremo == 1, paste0("ex_", name) == name_ex) %>%
  left_join(diccionario_nombres, by = "name") %>% # Traemos la pregunta real
  mutate(pregunta_limpia = if_else(!is.na(label), label, name)) %>%
  group_by(key) %>%
  summarise(detalle_outliers = paste0(pregunta_limpia, ": [", valor, "]", collapse = " | "), .groups = "drop")

# --- 3. CONSTRUCCIÓN DE LA TABLA DE GESTIÓN ---
tabla_gestion_manga <- base_manga %>%
  sf::st_drop_geometry() %>% 
  filter(Alerta_Auditoria == 1) %>%
  left_join(detalle_basura_manga, by = "key") %>%
  left_join(detalle_extremos_manga, by = "key") %>%
  left_join(detalle_nsnr_manga, by = "key") %>% # Usamos el que tiene los labels de NSNR
  mutate(
    # Componentes individuales con iconos
    geo   = if_else(flag_geofencing == 1, paste0("📍 GPS: ", round(distancia_m, 0), "m fuera"), NA_character_),
    time  = if_else(flag_duration_outlier == 1, paste0("⏱️ Duración: ", round(duration_min, 1), "m"), NA_character_),
    trash = if_else(flag_texto_basura == 1, paste0("🗑️ Basura: ", texto_detalle_trash), NA_character_),
    nsnr  = if_else(flag_nsnr == 1, paste0("❓ NS/NR: ", lista_nsnr), NA_character_),
    out   = if_else(flag_extreme_values == 1, paste0("📈 Valor: ", detalle_outliers), NA_character_),
    dup   = if_else(flag_duplicated == 1, "👥 DUPLICADO", NA_character_)
  ) %>%
  # Unión limpia de alertas
  unite("Alertas_Final", geo, time, trash, nsnr, out, dup, sep = " | ", na.rm = TRUE) %>%
  mutate(
    # Semáforo de criticidad
    Status = case_when(
      flag_duplicated == 1 | flag_texto_basura == 1 ~ "🔴 CRÍTICO",
      flag_geofencing == 1 | flag_extreme_values == 1 ~ "🟡 ADVERTENCIA",
      TRUE ~ "🔵 REVISIÓN"
    ),
    # Acción simplificada
    Accion = case_when(
      flag_duplicated == 1 ~ "Eliminar registro",
      flag_texto_basura == 1 ~ "Invalidar y re-encuestar",
      flag_geofencing == 1 ~ "Verificar ubicación",
      flag_extreme_values == 1 ~ "Confirmar dato numérico",
      TRUE ~ "Revisión general"
    )
  ) %>%
  select(
    Status,
    `ID Encuesta` = key,
    Fecha = starttime,
    Supervisor = supervisor_pull,
    Tecnico = tecnico_pull,
    Encuestador = ident_enc_resp,
    Padron = padron_pull,
    `Tipo de encuesta` = categoria_auditoria,
    `Grupo` = grupo_experimento,
    `Alertas Detalladas` = Alertas_Final,
    Accion,
  ) %>%
  arrange(Accion, desc(`Tipo de encuesta`))

# --- EXPORTACIÓN ---
sheet_write(tabla_gestion_manga, ss = id_sheet, sheet = "Tabla_Gestion_Alertas")



# 1. Crear la base específica para la Arquitecta
base_revision_arqui <- base_manga %>%
  # Filtrar solo grupos 3 (Esquema Simple) o 4 (Esquema Detallado)
  filter(grupo_3 == 1 | grupo_4 == 1) %>%
  arrange(day)%>%
  # Seleccionar y renombrar columnas para que sean legibles en Excel
  select(
    `PADRÓN` = padron_pull,
    `FECHA` = day,
    `SUPERVISOR` = supervisor_pull,
    `TÉCNICO` = tecnico_pull,
    `ENCUESTADOR` = ident_enc_resp,
    `FUENTE` = fuente_resp,
    `TIPO DE CONEXIÓN` = p10_09_resp,
    `TIPO DE ESQUEMA` = grupo_experimento,
    `FOTO FRENTE ESQUEMA` = foto_ef_anverso,
    `FOTO REVERSO ESQUEMA` = foto_ef_reverso
  ) %>%
  # Añadir las columnas vacías para que la arquitecta escriba
  mutate(
    `STATUS DE REVISIÓN` = "",
    `COMENTARIOS DE LA ARQUI` = ""
  )


sheet_write(base_revision_arqui, ss = id_sheet, sheet = "Supervisión esquemas funcionales") 



####################################################
# REPORTE DE AVANCE ESTRATÉGICO POR MANZANA (FIX)
####################################################

# 1. Preparar la Meta a nivel de Manzana (Limpieza de texto)
meta_manzana <- case_management %>%
  mutate(users_id = str_trim(as.character(enumerators))) %>%
  inner_join(enumerators_tab, by = c("users_id" = "id")) %>%
  mutate(Manzana = str_trim(as.character(name))) %>% # Limpiamos espacios
  group_by(Manzana) %>%
  summarise(
    Padrones_Totales = n_distinct(padron, na.rm = TRUE),
    enumerators = first(enumerators),
    .groups = "drop"
  )

# 2. Resumen de trabajo real (Formateamos el pull para que diga "Manzana X")
trabajo_manzana <- base_manga %>%
  mutate(
    # Si manzana_pull trae solo el número "1", le pegamos "Manzana "
    # Si ya trae la palabra, str_replace asegura que no se duplique
    num_manzana = str_extract(manzana_pull, "\\d+"),
    Manzana = paste("Manzana", num_manzana)
  ) %>%
  group_by(Manzana) %>%
  summarise(
    EFECTIVAS    = sum(categoria_auditoria == "EFECTIVA", na.rm = TRUE),
    RECHAZOS     = sum(categoria_auditoria == "RECHAZO", na.rm = TRUE),
    NO_ELEGIBLES = sum(categoria_auditoria == "NO ELEGIBLE", na.rm = TRUE),
    AGENDADAS    = sum(categoria_auditoria == "AGENDADA/PENDIENTE", na.rm = TRUE),
    Equipos_Intervencion = paste(unique(paste0(tecnico_pull, " + ", ident_enc_resp)), collapse = " | "),
    .groups = "drop"
  )

####################################################
# REPORTE DE INTELIGENCIA TERRITORIAL POR MANZANA (FIXED)
####################################################

avance_manzana_pro <- meta_manzana %>%
  left_join(trabajo_manzana, by = "Manzana") %>%
  # 1. Limpieza de datos (NAs a 0)
  mutate(across(c(EFECTIVAS, RECHAZOS, NO_ELEGIBLES, AGENDADAS), ~ coalesce(.x, 0))) %>%
  
  # 2. Conteo de Status Específicos para Alertas
  left_join(
    base_manga %>%
      mutate(Manzana_join = paste("Manzana", str_extract(manzana_pull, "\\d+"))) %>%
      group_by(Manzana = Manzana_join) %>%
      summarise(
        Status_9 = sum(status_num == 9, na.rm = TRUE),  # Sin red
        Status_10 = sum(status_num == 10, na.rm = TRUE), # >10 viviendas
        Status_2 = sum(status_num == 2, na.rm = TRUE),   # Ausentes
        Ultima_Visita = max(as.Date(submission_date), na.rm = TRUE), # Levantamos fecha
        .groups = "drop"
      ), by = "Manzana"
  ) %>%
  mutate(across(starts_with("Status_"), ~ coalesce(.x, 0))) %>%
  
  # 3. Cálculos de Base y Porcentajes
  mutate(
    VISITADOS = EFECTIVAS + RECHAZOS + NO_ELEGIBLES + AGENDADAS,
    NO_VISITADOS = Padrones_Totales - VISITADOS,
    
    # Calculamos la variable de avance primero para que el case_when la encuentre
    AVANCE_TOTAL_PERC = round((VISITADOS / Padrones_Totales) * 100, 1),
    
    # Porcentajes detallados
    PERC_EFECTIVAS    = round((EFECTIVAS / Padrones_Totales) * 100, 1),
    PERC_NO_VISITADO  = round((NO_VISITADOS / Padrones_Totales) * 100, 1),
    PERC_RECHAZOS     = round((RECHAZOS / Padrones_Totales) * 100, 1),
    
    # 4. Lógica de Alertas de Negocio
    Alerta_Sin_Red = if_else(Status_9 / Padrones_Totales > 0.50, "🚨 CRÍTICO: Sin Red (>50%)", NA_character_),
    Alerta_Densidad = if_else(Status_10 / Padrones_Totales > 0.40, "🏢 ALTA DENSIDAD (>40%)", NA_character_),
    Alerta_Ausentismo = if_else(Status_2 / pmax(VISITADOS, 1) > 0.30, "🏃 ALTO AUSENTISMO", NA_character_),
    
    # Calculamos días de inactividad (Sugerencia extra)
    Dias_Inactiva = as.numeric(Sys.Date() - Ultima_Visita),
    
    # 5. SEMÁFORO ESTRATÉGICO (Corregido)
    Semaforo = case_when(
      Status_9 / Padrones_Totales > 0.50 ~ "⚫ SIN RED (Descartar)",
      (RECHAZOS + Status_2) / pmax(VISITADOS, 1) > 0.40 ~ "🔴 COMPLICADA (Rechazo/Ausentes)",
      Status_10 / Padrones_Totales > 0.40 ~ "🟣 EDIFICIOS (Carga especial)",
      Dias_Inactiva > 4 & AVANCE_TOTAL_PERC < 100 ~ "🔥 CONGELADA (Sin visitas)",
      AVANCE_TOTAL_PERC > 80 ~ "🟢 ENCAMINADA (Finalizando)",
      AVANCE_TOTAL_PERC > 40 ~ "🟡 EN PROCESO",
      TRUE ~ "⚪ INICIO / BAJO AVANCE"
    )
  ) %>%
  unite("Alertas_Manzana", Alerta_Sin_Red, Alerta_Densidad, Alerta_Ausentismo, sep = " | ", na.rm = TRUE) %>%
  
  # 6. Selección Final con Nombres Claros para Looker
  select(
    Semaforo,
    Manzana,
    `Padrones Totales` = Padrones_Totales,
    `No Visitados` = NO_VISITADOS,
    `% No Visitado` = PERC_NO_VISITADO,
    EFECTIVAS,
    `% Efectividad` = PERC_EFECTIVAS,
    RECHAZOS,
    `% Rechazo` = PERC_RECHAZOS,
    NO_ELEGIBLES,
    AGENDADAS,
    Alertas_Manzana,
    `Días Inactiva` = Dias_Inactiva,
    `Equipos` = Equipos_Intervencion,
    `% Avance Total` = AVANCE_TOTAL_PERC
  ) %>%
  arrange(Semaforo, desc(`% Avance Total`))

# --- EXPORTACIÓN ---
sheet_write(avance_manzana_pro, ss = id_sheet, sheet = "Avance_Inteligente_Manzana")

# --- EXPORTACIÓN ---
sheet_write(avance_manzana_final, ss = id_sheet, sheet = "Avance_por_Manzana")

print("Reporte de avance por manzana generado con éxito.")
