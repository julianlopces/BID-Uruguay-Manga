
# 1. Leer el Case Management
url_case_management <- "https://docs.google.com/spreadsheets/d/1RvN_wdzkPQWiznNioztrt3AaB1kjVevf5q2koRFAc1Y/edit#gid=1876947279"
case_management <- read_sheet(url_case_management)

# 2. Calcular padrones únicos asignados por encuestador
asignacion_meta <- case_management %>%
  # Aseguramos que los nombres coincidan con tu base (limpieza)
  mutate(enumerator = str_trim(enumerators)) %>%
  group_by(encuestador = enumerator) %>%
  summarise(
    Encuestador  = first(enumerator_name),
    Tecnico  = first(tecnico),
    Padrones_Asignados = n_distinct(padron, na.rm = TRUE),
    .groups = "drop"
  )

####################################################
# 1. CONSOLIDADO POR ENCUESTADOR
####################################################

consolidado_encuestadores_manga <- base_manga %>%
  # Usamos la base COMPLETA para métricas de trabajo, 
  # pero filtraremos para métricas de avance
  group_by(encuestador = encuestador_pull) %>% 
  summarise(
    # Metricas de Trabajo (Todas las visitas realizadas)
    Visitas_Totales = n(),
    Dias_Trabajados = n_distinct(as.Date(ymd_hms(starttime))),
    
    # Metricas de Avance (Solo la encuesta final por padrón)
    Padrones_Contactados = sum(encuesta_final == 1, na.rm = TRUE),
    Efectivas_Finales    = sum(encuesta_final == 1 & tipo_encuesta_lbl == "EFECTIVA", na.rm = TRUE),
    No_Efectivas_Finales = sum(encuesta_final == 1 & tipo_encuesta_lbl == "NO EFECTIVA", na.rm = TRUE),
    Agendadas_Finales    = sum(encuesta_final == 1 & tipo_encuesta_lbl == "AGENDADA", na.rm = TRUE),
    
    # Alertas (Sobre todas las visitas)
    Total_Alertas = sum(Alerta_Auditoria == 1, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # 3. Cruzar con la Meta del Case Management
  left_join(asignacion_meta, by = "encuestador") %>%
  mutate(
    # Cálculos de Avance
    Porcentaje_Cobertura = round((Padrones_Contactados / Padrones_Asignados) * 100, 1),
    Porcentaje_Efectividad = round((Efectivas_Finales / Padrones_Asignados) * 100, 1),
    
    # Rendimiento
    Rendimiento_Diario = round(Visitas_Totales / Dias_Trabajados, 2)
  ) %>%
  arrange(desc(Porcentaje_Cobertura)) %>%
  select(
    `Nombre encuestador` = encuestador,
    `Padrones Asignados` = Padrones_Asignados,
    `Padrones Contactados` = Padrones_Contactados,
    `% Avance (Cobertura)` = Porcentaje_Cobertura,
    `Efectivas Reales` = Efectivas_Finales,
    `% Avance (Efectividad)` = Porcentaje_Efectividad,
    `Días trabajados` = Dias_Trabajados,
    `Total Visitas` = Visitas_Totales,
    `Alertas Totales` = Total_Alertas
  )

print(consolidado_encuestadores_manga)

# Sustituye con el ID o URL de tu Google Sheet
id_sheet <- "https://docs.google.com/spreadsheets/d/1my_dgomVoXUI8ADPGIV8WyYek1qQtFuVsO_lvKxDhLY/edit?gid=0#gid=0"

# Escribir en la hoja (si la hoja no existe, la crea; si existe, la sobrescribe)

consolidado_encuestadores_manga <- consolidado_encuestadores_manga %>%
  # Extraemos las coordenadas de la columna geometry
  mutate(
    lat_final = st_coordinates(geometry)[,2],
    lon_final = st_coordinates(geometry)[,1]
  ) %>%
  # Eliminamos la columna de geometría para que no dé error
  st_drop_geometry() %>%
  # Opcional: convertir a data.frame puro para asegurar compatibilidad
  as.data.frame()

sheet_write(consolidado_encuestadores_manga, ss = id_sheet, sheet = "consolidado_encuestadores_manga") 

# --- PASO A: Detalle de Texto Basura ---
# Identificamos qué campos específicos dispararon la alerta de basura
detalle_basura_manga <- base_manga %>%
  filter(flag_texto_basura == 1) %>%
  select(key, all_of(intersect(vars_texto, names(.))), starts_with("trash_")) %>%
  tidyr::pivot_longer(
    cols = all_of(intersect(vars_texto, names(.))),
    names_to = "pregunta",
    values_to = "texto_escrito"
  ) %>%
  tidyr::pivot_longer(
    cols = starts_with("trash_"),
    names_to = "pregunta_trash",
    values_to = "es_basura"
  ) %>%
  filter(es_basura == 1, paste0("trash_", pregunta) == pregunta_trash) %>%
  group_by(key) %>%
  summarise(
    texto_detalle_trash = paste0(pregunta, ": '", texto_escrito, "'", collapse = " | "),
    .groups = "drop"
  )

# --- PASO B: Tabla de Gestión Looker (CORREGIDO) ---
tabla_gestion_manga <- base_manga %>%
  # IMPORTANTE: Quitamos la geometría para poder hacer joins normales y exportar a Excel/Looker
  sf::st_drop_geometry() %>% 
  filter(Alerta_Auditoria == 1) %>%
  left_join(detalle_basura_manga, by = "key") %>%
  mutate(
    # 1. Concatenar TODAS las alertas detectadas
    todas_las_alertas = str_remove(paste0(
      if_else(flag_duration_outlier == 1, "Tiempos fuera de rango, ", ""),
      if_else(flag_duplicated == 1, "ID Duplicado, ", ""),
      if_else(flag_geofencing == 1, "Fuera de rango Geográfico (>10m), ", ""),
      if_else(flag_texto_basura == 1, paste0("Texto basura (", texto_detalle_trash, "), "), ""),
      if_else(flag_missing == 1, "Missings detectados, ", ""),
      if_else(flag_nsnr == 1, "Exceso de NS/NR, ", ""),
      if_else(flag_extreme_values == 1, "Valores numéricos extremos, ", "")
    ), ", $"),
    
    # 2. Definir Prioridad
    Prioridad = case_when(
      flag_duplicated == 1 ~ "1-CRÍTICO (Duplicado)",
      flag_geofencing == 1 ~ "2-ALTA (Geo)",
      flag_texto_basura == 1 ~ "2-ALTA (Basura)",
      flag_duration_outlier == 1 ~ "3-MEDIA (Tiempo)",
      flag_nsnr == 1 ~ "3-MEDIA (NSNR)",
      TRUE ~ "4-BAJA"
    ),
    
    # 3. Acción Sugerida
    accion_a_realizar = case_when(
      flag_duplicated == 1 ~ "RECHAZAR: Registro duplicado en base.",
      flag_geofencing == 1 ~ "VERIFICAR: Ubicación no coincide con padrón.",
      flag_texto_basura == 1 ~ "VERIFICAR: Textos sin sentido en campos abiertos.",
      flag_duration_outlier == 1 ~ "REVISAR: Duración atípica.",
      flag_missing == 1 | flag_nsnr == 1 ~ "COMPLETAR/REVISAR: Vacíos o NSNR excesivos.",
      TRUE ~ "REVISAR: Auditoría general."
    )
  ) %>%
  select(
    Fecha = starttime,
    ID_Encuesta = key,
    Encuestador = encuestador_pull,
    Tipo_Encuesta = tipo_encuesta_lbl,
    Padron = padron_pull,
    Distancia_m = distancia_m,
    `Alertas Detectadas` = todas_las_alertas,
    Prioridad,
    `Acción Sugerida` = accion_a_realizar
  ) %>%
  arrange(Prioridad)


# Escribir en la hoja (si la hoja no existe, la crea; si existe, la sobrescribe)


sheet_write(tabla_gestion_manga, ss = id_sheet, sheet = "tabla_gestion_manga") 