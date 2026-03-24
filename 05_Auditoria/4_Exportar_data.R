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
export_sheet(final_data,sheet_crudas, "BID_URUGUAY_Crudas",label = "Crudas",pause = 5)


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




