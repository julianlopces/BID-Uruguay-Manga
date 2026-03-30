# Exportar datos

# Verificar si las credenciales están disponibles
if (!exists("creds") || !file.exists(creds)) {
  stop("No se encontraron las credenciales de Google Sheets. Asegúrate de cargarlas desde el script maestro.")
}

# Helper genérico para exportar a Google Sheets con manejo de errores y pausa opcional
export_sheet <- function(df, ss, sheet_name, label = sheet_name, pause = 0) {
  message(sprintf("Exportando %s...", label))
  tryCatch({
    sheet_write(df, ss = ss, sheet = sheet_name)
    message(sprintf("Datos de %s exportados correctamente.", label))
  }, error = function(e) {
    stop(sprintf("Error al exportar %s: %s", label, conditionMessage(e)))
  })
  if (pause > 0) Sys.sleep(pause)
}

# Exportar datos crudos --------------------------------------------------------

sheet_crudas <- tryCatch({
  gs4_get("1bY3Ua6IpZOWrX99Mr2SenWUgByhRIbBk1Bce3oRcmvw")
}, error = function(e) {
  stop("Error al conectar con el Google Sheet de alertas: ", e)
})

# Exportar usando el wrapper
export_sheet(data_cruda,sheet_crudas, "BID_URUGUAY_Crudas",label = "Crudas",pause = 5)


# Exportar datos auditados -----------------------------------------------------

sheet_auditadas <- tryCatch({
  gs4_get("1bY3Ua6IpZOWrX99Mr2SenWUgByhRIbBk1Bce3oRcmvw")
}, error = function(e) {
  stop("Error al conectar con el Google Sheet de alertas: ", e)
})

# 1. Convertir la geometría a columnas de texto/número y eliminar el objeto espacial
base_para_looker <- base_manga_clear 

# 1. Convertimos la base de Looker a un DataFrame estándar (sin geometrías)
base_para_looker_clean <- base_para_looker %>%
  # Si aún tiene la columna geom_poligono o geometry, esto las elimina
  st_drop_geometry() %>% 
  # Por seguridad, si geom_poligono era una columna de texto con formato S3, la quitamos
  select(-where(~ inherits(.x, "sfc"))) %>%
  as.data.frame()

# 2. Ahora exportamos la versión limpia
export_sheet(base_para_looker_clean, sheet_auditadas, "base_manga_looker", 
             label = "Auditadas", pause = 5)


# Exportar datos limpias hogares -----------------------------------------------------

sheet_limpias <- tryCatch({
  gs4_get("1cH3NXLB6i0zSZbor1U4vqtsCL3aWa9AoJ-GVB5GcGVg")
}, error = function(e) {
  stop("Error al conectar con el Google Sheet de alertas: ", e)
})


# 2. Ahora exportamos la versión limpia
export_sheet(base_odk_limpia, sheet_limpias, "Base_datos_hogares_final", 
             label = "Limpias_final_case", pause = 5)


# Exportar datos limpias casos -----------------------------------------------------

sheet_limpias <- tryCatch({
  gs4_get("1cH3NXLB6i0zSZbor1U4vqtsCL3aWa9AoJ-GVB5GcGVg")
}, error = function(e) {
  stop("Error al conectar con el Google Sheet de alertas: ", e)
})


# 2. Ahora exportamos la versión limpia
export_sheet(base_manga_casos_clean, sheet_limpias, "Base_datos_casos", 
             label = "Limpias_casos", pause = 5)


# Exportar datos limpias casos -----------------------------------------------------

sheet_auditadas <- tryCatch({
  gs4_get("1bY3Ua6IpZOWrX99Mr2SenWUgByhRIbBk1Bce3oRcmvw")
}, error = function(e) {
  stop("Error al conectar con el Google Sheet de alertas: ", e)
})


# 2. Ahora exportamos la versión limpia
export_sheet(base_manga_casos_clean, sheet_auditadas, "Base_datos_casos", 
             label = "Limpias_casos", pause = 5)


# Exportar datos avance campo -----------------------------------------------------

sheet_avance <- tryCatch({
  gs4_get("1Kp8tFRvJKMkLRFKVsAgysQzcz5BDtn9HCegZFgs9siw")
}, error = function(e) {
  stop("Error al conectar con el Google Sheet de alertas: ", e)
})


# 2. Ahora exportamos la versión limpia
export_sheet(resumen_horizontal, sheet_avance, "Avance diario reporte", 
             label = "Avance reporte", pause = 5)

# Exportar datos avance campo -----------------------------------------------------

sheet_avance <- tryCatch({
  gs4_get("1Kp8tFRvJKMkLRFKVsAgysQzcz5BDtn9HCegZFgs9siw")
}, error = function(e) {
  stop("Error al conectar con el Google Sheet de alertas: ", e)
})

# 2. Ahora exportamos la versión limpia
export_sheet(avance_manzana_pro, sheet_avance, "Avance por Manzana (Metas)", 
             label = "Manzana Metas", pause = 5)
