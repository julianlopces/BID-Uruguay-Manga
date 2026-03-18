
# 1. Leer el Case Management
url_case_management <- "https://docs.google.com/spreadsheets/d/1RvN_wdzkPQWiznNioztrt3AaB1kjVevf5q2koRFAc1Y/edit#gid=1876947279"
case_management <- read_sheet(url_case_management)

# 2. Calcular padrones únicos asignados por encuestador
asignacion_meta <- case_management %>%
  # Aseguramos que los nombres coincidan con tu base (limpieza)
  mutate(enumerator_name = str_trim(enumerators)) %>%
  group_by(encuestador = enumerator_name) %>%
  summarise(
    Padrones_Asignados = n_distinct(padron, na.rm = TRUE),
    .groups = "drop"
  )

####################################################
# 1. CONSOLIDADO POR ENCUESTADOR
####################################################

consolidado_encuestadores_manga <- base_manga %>%
  # Paso 1: Agrupamos por los campos de identificación del encuestador
  # Ajusta 'deviceid' o 'user_id' según el nombre real de la columna del encuestador
  group_by(encuestador = encuestador_pull, id_dispositivo = deviceid) %>% 
  summarise(
    # Fechas de actividad
    Fecha_Inicio = as.Date(min(ymd_hms(starttime), na.rm = TRUE)),
    Fecha_Ultima = as.Date(max(ymd_hms(starttime), na.rm = TRUE)),
    
    # Métricas de conteo
    Total_Contactos = n(),
    Total_Efectivas = sum(efectiva == 1, na.rm = TRUE),
    Sin_Alertas_Exitosas = sum(Exito_Auditoria == 1, na.rm = TRUE),
    
    # Rendimiento
    Dias_Trabajados = n_distinct(as.Date(ymd_hms(starttime))),
    Rendimiento_Diario = round(Total_Contactos / Dias_Trabajados, 2),
    
    # --- DESGLOSE DE ALERTAS ---
    M_Saltos_Missings = sum(flag_missing == 1 | flag_saltos == 1, na.rm = TRUE),
    M_Tiempos = sum(flag_duration_outlier == 1, na.rm = TRUE),
    M_Basura = sum(flag_texto_basura == 1, na.rm = TRUE),
    M_Georef = sum(flag_geofencing == 1, na.rm = TRUE),
    M_Duplicados = sum(flag_duplicated == 1, na.rm = TRUE),
    M_NSNR = sum(flag_nsnr == 1, na.rm = TRUE),
    M_Extremos = sum(flag_extreme_values == 1, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>%
  arrange(desc(Total_Efectivas)) %>%
  select(
    `Nombre encuestador` = encuestador,
    `ID Dispositivo` = id_dispositivo,
    `Fecha Inicio` = Fecha_Inicio,
    `Fecha Última` = Fecha_Ultima,
    `Días trabajados` = Dias_Trabajados,
    `Rendimiento Diario` = Rendimiento_Diario,
    `TOTAL CONTACTOS` = Total_Contactos,
    `EFECTIVAS` = Total_Efectivas,
    `EFECTIVAS LIMPIAS` = Sin_Alertas_Exitosas,
    `Alertas: Saltos/Missings` = M_Saltos_Missings,
    `Alertas: Tiempos` = M_Tiempos,
    `Alertas: Basura` = M_Basura,
    `Alertas: Geo` = M_Georef,
    `Alertas: Duplicados` = M_Duplicados,
    `Alertas: NSNR` = M_NSNR,
    `Alertas: Extremos` = M_Extremos
  )

print(consolidado_encuestadores_manga)