#==============================================================================#
#                PROYECTO: MONITOREO DE CAMPO - MANGA                       ----
#==============================================================================#

# 1. CARGA DE METADATOS (CASE MANAGEMENT & ENUMERATORS) ----
# 1. LEER CASE MANAGEMENT Y ENUMERATORS

url_case_management <- "https://docs.google.com/spreadsheets/d/1qAtDSHtNwFmmxp3HWp4nNe64ic3YTY2pP9cl3E50CTc/edit?gid=1160660477#gid=1160660477"
case_management <- read_sheet(url_case_management)

url_enumerators <- "https://docs.google.com/spreadsheets/d/1YcafSQeOn0183FXhMjHj8n6Nrebpc3BbIbgadFstPDU/edit?gid=0#gid=0"
enumerators_tab <- read_sheet(url_enumerators)

id_sheet <- "https://docs.google.com/spreadsheets/d/1my_dgomVoXUI8ADPGIV8WyYek1qQtFuVsO_lvKxDhLY/edit"


# 2. PREPARACIÓN DE METAS Y ASIGNACIÓN POR TÉCNICO ----

###    2.1. Procesamiento de Metas por Manzana ----

# Ahora agrupamos por Técnico y Manzana para tener la meta clara
meta_resumen <- case_management %>%
  # Limpieza de nombres para el join
  mutate(users.x = str_trim(as.character(enumerators))) %>%
  inner_join(enumerators_tab, by = c("users.x" = "id")) %>%
  filter(users.y != "not_yet") %>%
  group_by(
    Manzana = name,
    Tecnico = enumerator_name, # Este es el responsable del equipo/manzana
    Supervisor = supervisor
  ) %>%
  summarise(
    Padrones_Asignados = n_distinct(padron, na.rm = TRUE),
    .groups = "drop"
  )

###    2.2. Consolidación de Metas por Técnico ----
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

# 3. CONSOLIDADO MULTICATEGORÍA DE AUDITORÍA ----

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
    `EFECTIVAS` = EFECTIVAS_CON_ALERTA,
    RECHAZOS,
    NO_ELEGIBLES,
    AGENDADAS,
    OTROS_ERRORES,
    `Total Alertas (Histórico)` = TOTAL_ALERTAS_GENERAL,
    `% Avance Cobertura`,
    Supervisor = Supervisor_Asignado
  )

print(consolidado_final_manga)

# 4. PROCESAMIENTO DE DETALLES PARA GESTIÓN ----

###    4.1. Diccionario de Etiquetas (Labels ODK) ----

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
      name == "p1_06" ~ "1,06 ¿Cuantas viviendas hay en este padrón?",
      name == "p5_11" ~ "5,11 ¿a quién solicitó el vaciado ",
      TRUE ~ label
    ),
    # 3. Cortamos el texto para que no desborde en Looker
    label = str_trunc(str_trim(label), 45) 
  ) %>% 
  # Evitamos duplicados si la variable aparece varias veces en el ODK
  group_by(name) %>% 
  summarise(label = first(label), .groups = "drop")

###    4.2. Detalle de Texto Basura (Trash )----
detalle_basura_manga <- base_manga %>%
  filter(flag_texto_basura == 1) %>%
  select(key, all_of(intersect(vars_texto, names(.))), starts_with("trash_")) %>%
  tidyr::pivot_longer(cols = all_of(intersect(vars_texto, names(.))), names_to = "pregunta", values_to = "texto_escrito") %>%
  tidyr::pivot_longer(cols = starts_with("trash_"), names_to = "pregunta_trash", values_to = "es_basura") %>%
  filter(es_basura == 1, paste0("trash_", pregunta) == pregunta_trash) %>%
  group_by(key) %>%
  summarise(texto_detalle_trash = paste0(pregunta, ": '", texto_escrito, "'", collapse = " | "), .groups = "drop")

###    4.3. Detalle de Incidencias NS/NR ----
# Esto sirve para que el supervisor vea qué preguntas fueron marcadas como NS/NR
detalle_nsnr_manga <- base_manga %>%
  filter(flag_nsnr == 1) %>%
  select(key, all_of(intersect(vars_nsnr, names(.)))) %>%
  tidyr::pivot_longer(cols = -key, names_to = "pregunta", values_to = "valor") %>%
  filter(valor %in% c(-1, 98, 99, "-1", "98", "99")) %>%
  group_by(key) %>%
  summarise(
    lista_nsnr = paste(pregunta, collapse = ", "),
    .groups = "drop"
  )

###   4.4. Detalle de Valores Extremos (Outliers) ----
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

###    4.5. Detalle de Datos Faltantes (Missings) ----
detalle_missings_manga <- base_manga %>%
  # Filtramos encuestas que tengan al menos un missing detectado
  filter(total_missing > 0) %>% 
  # Seleccionamos el KEY y todas las columnas que empiezan con "m_"
  select(key, all_of(variables_missing)) %>%
  # Pasamos a formato largo para identificar CUÁL es el missing
  tidyr::pivot_longer(
    cols = all_of(variables_missing), 
    names_to = "name_m", 
    values_to = "es_missing"
  ) %>%
  # Nos quedamos solo con las filas donde efectivamente hay un missing (valor 1)
  filter(es_missing == 1) %>%
  # Limpiamos el nombre (m_p5_11 -> p5_11) para cruzar con el diccionario
  mutate(name = gsub("^m_", "", name_m)) %>%
  left_join(diccionario_nombres, by = "name") %>% 
  # Si no hay label en el diccionario, usamos el nombre técnico
  mutate(pregunta_limpia = if_else(!is.na(label), label, name)) %>%
  group_by(key) %>%
  summarise(
    detalle_missings = paste0("🚫 Faltante: ", pregunta_limpia, collapse = " | "), 
    .groups = "drop"
  )

# 5. CONSTRUCCIÓN DE LA TABLA DE GESTIÓN DE ALERTAS ----
tabla_gestion_manga <- base_manga %>%
  sf::st_drop_geometry() %>% 
  filter(Alerta_Auditoria == 1 & categoria_auditoria == "EFECTIVA" ) %>%
  left_join(detalle_basura_manga, by = "key") %>%
  left_join(detalle_extremos_manga, by = "key") %>%
  left_join(detalle_nsnr_manga, by = "key") %>% 
  left_join(detalle_missings_manga, by = "key") %>% # <--- El que acabamos de crear
  mutate(
    # Componentes individuales con iconos
    geo   = if_else(flag_geofencing == 1, paste0("📍 GPS: ", round(distancia_m, 0), "m fuera"), NA_character_),
    time  = if_else(flag_duration_outlier == 1, paste0("⏱️ Duración: ", round(duration_min, 1), "m"), NA_character_),
    trash = if_else(flag_texto_basura == 1, paste0("🗑️ Basura: ", texto_detalle_trash), NA_character_),
    nsnr  = if_else(flag_nsnr == 1, paste0("❓ NS/NR: ", lista_nsnr), NA_character_),
    out   = if_else(flag_extreme_values == 1, paste0("📈 Valor: ", detalle_outliers), NA_character_),
    dup   = if_else(flag_duplicated == 1, paste0("👥 DUPLICADO: ", id_unico), NA_character_),
    # NUEVO: Detalle de Missings
    miss  = if_else(flag_missing == 1, detalle_missings, NA_character_)
    ) %>%
  # Unión limpia de alertas
  unite("Alertas_Final", geo, time, trash, nsnr, out, dup, miss, sep = " | ", na.rm = TRUE) %>%
  mutate(
    # Semáforo de criticidad
    Status = case_when(
      flag_duplicated == 1 | flag_texto_basura == 1 ~ "🔴 CRÍTICO",
      flag_geofencing == 1 | flag_extreme_values == 1 ~ "🟡 ADVERTENCIA",
      TRUE ~ "🔵 REVISIÓN"
    ),
    # Acción simplificada
    Accion = case_when(
      flag_duplicated == 1 ~ "Revisar registro",
      flag_texto_basura == 1 ~ "Revisar texto",
      flag_geofencing == 1 ~ "Verificar ubicación",
      flag_extreme_values == 1 ~ "Confirmar dato numérico",
      flag_missing == 1 ~ "Revisar datos faltantes",
      flag_nsnr == 1 ~ "Revisar cantidad atipica de NSNR",
      flag_duration_outlier == 1 ~ "Duración atipica",
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
    Manzana = manzana_pull,
    Padron = padron_pull,
    `Tipo de encuesta` = categoria_auditoria,
    Accion,
    `Alertas Detalladas` = Alertas_Final,
  ) %>%
  arrange(`Alertas Detalladas`, desc(`Tipo de encuesta`))

###    5.1. Exportación  ---- 
sheet_write(tabla_gestion_manga, ss = id_sheet, sheet = "Tabla_Gestion_Alertas")

base_manga_clear <- base_manga_clear %>%
  left_join(detalle_basura_manga, by = "key") %>%
  left_join(detalle_extremos_manga, by = "key") %>%
  left_join(detalle_nsnr_manga, by = "key") %>% 
  left_join(detalle_missings_manga, by = "key")


# 6. MÓDULO DE SUPERVISIÓN TÉCNICA (ARQUITECTA) ----

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

###    6.1. Exportación  ---- 
sheet_write(base_revision_arqui, ss = id_sheet, sheet = "Supervisión esquemas funcionales") 


# 7. REPORTE DE AVANCE INTELIGENTE POR MANZANA ----


###    7.1. Lógica de Cierre y Estatus de Recorrido ----

# 1. DICCIONARIO DE NOMBRES REALES
nombres_tecnicos <- tribble(
  ~users,                            ~enumerator_name,
  "anaclaranunez2628@gmail.com",     "Anaclara Núñez de los Santos",
  "joadog12@gmail.com",              "Franco Joaquín Dogliotti Espinosa",
  "el978er@gmail.com",               "Oscar Rodríguez",
  "valentinamunist@gmail.com",       "Valentina Munist",
  "juanlozano.a92@gmail.com",        "Juan Luis Lozano Alvarenga",
  "cuervo0305@gmail.com",            "Joaquín Cuervo D'ottone",
  "bentancornahuel349@gmail.com",    "Nahuel Bentancor Burgues",
  "villalbaptadeo@gmail.com",        "Tadeo Villalba Pereira",
  "ivobritos09@gmail.com",           "Ivo Britos",
  "pantonella895@gmail.com",         "Antonella Pirri Piñeyro",
  "manurodriguez201@gmail.com",      "Manuel Rodriguez",
  "valentinabarracoromero@gmail.com", "Valentina Barraco",
  "altezmayra@gmail.com",            "Mayra Altez",
  "federicoderossiesteves@gmail.com", "Federico Derossi"
)

# 2. PROCESAR ASIGNACIONES (ENUMERATORS)
# Estandarizamos a "Manzana X" extrayendo solo los números
enumerators_limpio <- enumerators_tab %>%
  left_join(nombres_tecnicos, by = "users") %>%
  mutate(
    num = str_extract(name, "\\d+"),
    Manzana = paste("Manzana", num),
    Tecnico_Asignado = if_else(users == "not_yet", "🚫 No Asignada", coalesce(enumerator_name, users))
  ) %>%
  select(Manzana, Tecnico_Asignado)

# 3. PREPARAR META (CASE MANAGEMENT)
meta_manzana <- case_management %>%
  filter(id != 1) %>%
  mutate(
    num = str_extract(as.character(manzana), "\\d+"),
    Manzana = paste("Manzana", num)
  ) %>%
  group_by(Manzana) %>%
  summarise(
    Padrones_Totales = n_distinct(padron, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Unimos con la asignación de enumerators
  left_join(enumerators_limpio, by = "Manzana")

# 1. Resumen de trabajo real (Separando Éxito de Alertas)
trabajo_manzana <- base_manga %>%
  mutate(
    num_manzana = str_extract(manzana_pull, "\\d+"),
    Manzana = paste("Manzana", num_manzana)
  ) %>%
  group_by(Manzana) %>%
  summarise(
    # --- MÉTRICAS DE CIERRE (Solo con ÉXITO) ---
    EFECTIVAS    = sum(categoria_auditoria == "EFECTIVA" & Exito_Auditoria == 1, na.rm = TRUE),
    RECHAZOS     = sum(categoria_auditoria == "RECHAZO" & Exito_Auditoria == 1, na.rm = TRUE),
    NO_ELEGIBLES = sum(categoria_auditoria == "NO ELEGIBLE" & Exito_Auditoria == 1, na.rm = TRUE),
    AGENDADAS    = sum(categoria_auditoria == "AGENDADA/PENDIENTE" & Exito_Auditoria == 1, na.rm = TRUE),
    
    # --- MÉTRICA DE ESFUERZO FÍSICO (Independiente de alertas) ---
    TOTAL_VISITADOS_FISICOS = n_distinct(padron_pull, na.rm = TRUE),
    
    # --- MÉTRICA DE ALERTAS ---
    ENCUESTAS_EN_REVISION = sum(Alerta_Auditoria == 1 & categoria_auditoria != "AGENDADA/PENDIENTE" , na.rm = TRUE),
    
    # --- EQUIPOS (Separados como solicitaste) ---
    Tecnico = paste(unique(tecnico_pull), collapse = " | "),
    Encuestadores = paste(unique(ident_enc_resp), collapse = " | "),
    SUPERVISOR_Intervencion = first(supervisor_pull),
    
    # --- FECHAS Y ACTIVIDAD ---
    Fecha_Primera_Visita = min(as.Date(submission_date), na.rm = TRUE),
    Fecha_Ultima_Visita  = max(as.Date(submission_date), na.rm = TRUE),
    # Días activos: días transcurridos entre la primera y última visita
    Dias_Activos = as.numeric(Fecha_Ultima_Visita - Fecha_Primera_Visita) + 1,
    .groups = "drop"
  )

###    7.2. Semáforo Estratégico de Manzanas ----
avance_manzana_pro <- meta_manzana %>%
  left_join(trabajo_manzana, by = "Manzana") %>%
  mutate(across(c(EFECTIVAS, RECHAZOS, NO_ELEGIBLES, AGENDADAS, ENCUESTAS_EN_REVISION, TOTAL_VISITADOS_FISICOS), ~ coalesce(.x, 0))) %>%
  
  # Cruce con datos temporales y específicos de status
  left_join(
    base_manga %>%
      mutate(Manzana_join = paste("Manzana", str_extract(manzana_pull, "\\d+"))) %>%
      group_by(Manzana = Manzana_join) %>%
      summarise(
        Status_9 = sum(status_num == 9, na.rm = TRUE),
        Status_10 = sum(status_num == 10, na.rm = TRUE),
        Ultima_Visita = max(as.Date(submission_date), na.rm = TRUE),
        .groups = "drop"
      ), by = "Manzana"
  ) %>%
  mutate(across(starts_with("Status_"), ~ coalesce(.x, 0))) %>%
  
  # 3. Cálculos de Porcentajes y Status
  mutate(
    # No visitado real: Padrón que nunca fue tocado
    NO_VISITADOS = pmax(0, Padrones_Totales - TOTAL_VISITADOS_FISICOS),
    PERC_RECORRIDO = round((TOTAL_VISITADOS_FISICOS / Padrones_Totales) * 100, 1),
    
    # Avance Real: Solo casos cerrados y limpios
    CASOS_CERRADOS_LIMPIOS = EFECTIVAS + RECHAZOS + NO_ELEGIBLES,
    PERC_AVANCE_REAL = round((CASOS_CERRADOS_LIMPIOS / Padrones_Totales) * 100, 1),
    
    # Días Inactivos desde la última visita hasta hoy
    Dias_Inactivos = as.numeric(Sys.Date() - Fecha_Ultima_Visita),
    
    # 4. SEMÁFORO ESTRATÉGICO (Reglas actualizadas)
    Semaforo = case_when(
      Tecnico_Asignado == "🚫 No Asignada" ~ "📁 NO ASIGNADA",
      # REGLA ORO: Si ya no hay No Visitados pero hay alertas
      PERC_AVANCE_REAL == 100 & ENCUESTAS_EN_REVISION > 0 ~ "🔍 PENDIENTE AUDITORÍA",
      
      # Si ya no hay nada pendiente de ningún tipo
      PERC_AVANCE_REAL >= 100 & ENCUESTAS_EN_REVISION == 0 ~ "🟢 COMPLETA",
      
      # Si no se ha movido nada
      TOTAL_VISITADOS_FISICOS == 0 ~ "⚪ SIN EMPEZAR",
      
      # Si pasaron más de 4 días
      Dias_Inactivos > 4 & NO_VISITADOS > 0 ~ "🔥 MANZANA INACTIVA",
      
      # Recorrida al 100 pero con agendadas (limpias)
      PERC_RECORRIDO >= 100 & AGENDADAS > 0 ~ "🟡 REVISITAR (Pendientes)",
      
      # En proceso normal
      TRUE ~ "🔵 EN PROCESO"
    ),
    
    Alertas_Manzana = paste0(
      if_else(Status_9 / Padrones_Totales > 0.50, "🚨 Mayoria Sin Red", ""),
      if_else(ENCUESTAS_EN_REVISION > 0, paste0(" | 🔍 ", ENCUESTAS_EN_REVISION, " alertas"), ""),
      sep = ""
    )
  ) %>%
  
  # 5. Selección y Orden
  select(
    Manzana,
    Supervisor = SUPERVISOR_Intervencion,
    Tecnico = Tecnico_Asignado,
    Encuestadores,
    `Padrones Totales` = Padrones_Totales,
    `No Visitados` = NO_VISITADOS,
    EFECTIVAS,
    RECHAZOS,
    NO_ELEGIBLES,
    AGENDADAS,
    `Alertas de Auditoría` = ENCUESTAS_EN_REVISION,
    `% Recorrido` = PERC_RECORRIDO,
    `% Avance real` = PERC_AVANCE_REAL,
    `Primera Visita` = Fecha_Primera_Visita,
    `Última Visita` = Fecha_Ultima_Visita,
    `Días Activos (Duración)` = Dias_Activos,
    `Días Inactiva (Desde última)` = Dias_Inactivos,
    Alertas_Manzana,
    Semaforo,
  ) %>%
  arrange(desc(`% Recorrido`), Semaforo)

### 7.3 Exportación  ----
sheet_write(avance_manzana_pro, ss = id_sheet, sheet = "Avance_Inteligente_Manzana")

print("Reporte de avance por manzana generado con éxito.")

# 8. CONSOLIDADO INTEGRAL DE PRODUCTIVIDAD Y CALIDAD ----

consolidado_detallado_final <- base_manga %>%
  group_by(
    Tecnico = tecnico_pull, 
    Encuestador = ident_enc_resp
  ) %>% 
  summarise(
    # --- 1. ESTADO DE AVANCE (Encuestas Finales) ---
    Total_Padrones = n(),
    EFECTIVAS     = sum(categoria_auditoria == "EFECTIVA", na.rm = TRUE),
    RECHAZOS      = sum(categoria_auditoria == "RECHAZO", na.rm = TRUE),
    NO_ELEGIBLES  = sum(categoria_auditoria == "NO ELEGIBLE", na.rm = TRUE),
    AGENDADAS     = sum(categoria_auditoria == "AGENDADA/PENDIENTE", na.rm = TRUE),
    
    # --- 2. MÉTRICAS DE TIEMPO (Solo Efectivas para no sesgar) ---
    Minutos_Promedio = round(mean(duration_min[es_efectiva == 1], na.rm = TRUE), 1),
    
    # --- 3. PRODUCTOS TÉCNICOS (Esquemas) ---
    Esquemas_Simples_Ok   = sum(es_efectiva == 1 & grupo_3 == 1 & p10_13 == 1, na.rm = TRUE),
    Esquemas_Detallados_Ok = sum(es_efectiva == 1 & grupo_4 == 1 & p10_13 == 1, na.rm = TRUE),
  
    
    # --- 4. DESGLOSE DE ALERTAS (SOLO PARA EFECTIVAS) ---
    # Aquí mapeamos cada flag que creamos en el flujo de auditoría
    Alertas_GPS    = sum(es_efectiva == 1 & flag_geofencing == 1, na.rm = TRUE),
    Alertas_Tiempo = sum(es_efectiva == 1 & flag_duration_outlier == 1, na.rm = TRUE),
    Alertas_Basura = sum(es_efectiva == 1 & flag_texto_basura == 1, na.rm = TRUE),
    Alertas_NSNR   = sum(es_efectiva == 1 & flag_nsnr == 1, na.rm = TRUE),
    Alertas_Outlier_Num = sum(es_efectiva == 1 & flag_extreme_values == 1, na.rm = TRUE),
    Alertas_Duplicados  = sum(es_efectiva == 1 & flag_duplicated == 1, na.rm = TRUE),
    Alertas_missings  = sum(es_efectiva == 1 & flag_missing == 1, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>%
  mutate(
    # Indicador de calidad: % de encuestas efectivas que tienen al menos una alerta
    Tasa_Error_Efectivas = round(((Alertas_GPS + Alertas_Tiempo + Alertas_Basura + 
                                     Alertas_NSNR + Alertas_Outlier_Num) / EFECTIVAS) * 100, 1)
  ) %>%
  # Limpiar posibles NaNs si no hay efectivas todavía
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), 0, .))) %>%
  arrange(desc(EFECTIVAS))

### 8.1. Exportación  ----
sheet_write(resumen_esquemas_tecnico, ss = id_sheet, sheet = "Resumen esquemas funcionales")

print("Consolidado detallado generado y exportado a Google Sheets.")