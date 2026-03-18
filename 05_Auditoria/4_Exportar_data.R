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
  gs4_get("16JVkLiNzUsULos7e9z7aHnvUmwDFFNG2zXlhKQq8FSk")
}, error = function(e) {
  stop("Error al conectar con el Google Sheet de alertas: ", e)
})

base_manga_export <- base_manga %>%
  mutate(across(where(~inherits(.x, "sfc")), st_as_text)) %>%
  as_tibble() # Transformar columnas geoespaciales a texto para exportar

# Exportar usando el wrapper
export_sheet(base_manga_export,sheet_auditadas, "BID_Celulares_Auditadas",label = "Auditadas",pause = 5)

