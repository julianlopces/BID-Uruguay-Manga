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
  gs4_get("1iOT0sJWPHO4L62nJk0YeEACFrTupodGeYyWn7DwIhBc")
}, error = function(e) {
  stop("Error al conectar con el Google Sheet de alertas: ", e)
})

# Exportar usando el wrapper
export_sheet(final_data,sheet_crudas, "BID_Celulares_Crudas",label = "Crudas",pause = 5)


# Exportar datos auditados -----------------------------------------------------

sheet_auditadas <- tryCatch({
  gs4_get("1bY3Ua6IpZOWrX99Mr2SenWUgByhRIbBk1Bce3oRcmvw")
}, error = function(e) {
  stop("Error al conectar con el Google Sheet de alertas: ", e)
})

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

# Exportar usando el wrapper
export_sheet(base_para_looker,sheet_auditadas, "BID_Celulares_Auditadas",label = "Auditadas",pause = 5)






