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
    categoria_auditoria,
    encuesta_final,
    Rechazo_lbl,
    Tiempos_Anomalos_lbl,
    ID_Repetido_lbl,
    Fuera_Rango_Geo_lbl,
    Exceso_NSNR_lbl,
    Contenido_basura_lbl
  )

# --- LIMPIEZA DE TEXTO ADICIONAL ---

nombre_archivo <- paste0("03_Data/3_Limpias/Base_Limpia_Manga_", Sys.Date(), ".csv")
write_csv(base_odk_limpia, nombre_archivo, na = "")
print(paste("Archivo exportado exitosamente como:", nombre_archivo))

print("Base limpia generada con éxito siguiendo el orden del ODK.")