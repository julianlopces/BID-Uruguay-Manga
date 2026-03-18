# Importar datos

proyectos_ids <- c("prueba_censo_manga_encuestadores") 

# --- FUNCIÓN MEJORADA --- 
procesar_proyecto_missings <- function(id_proyecto) {
  
  # 1. Descarga de datos
  url <- paste0("https://", server_name, ".surveycto.com/api/v1/forms/data/wide/json/", id_proyecto)
  response <- GET(url, authenticate(api_user, api_pw, type = "basic"))
  
  if (status_code(response) != 200) {
    warning(paste("Error API en:", id_proyecto))
    return(NULL)
  }
  
  # 2. Parsear JSON
  data_cruda <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
  
  # 3. Convertir a tibble y transformar "" en NA
  # Usamos as.character en el across para asegurar que el match de "" funcione
  data_limpia <- data_cruda %>%
    as_tibble() %>%
    mutate(across(everything(), ~na_if(as.character(.), "")))
  
  return(data_limpia)
}

# --- EJECUCIÓN ---
# lapply aplicará la limpieza a cada formulario individualmente
lista_resultados <- lapply(proyectos_ids, procesar_proyecto_missings)

# Unir todos los proyectos
# bind_rows es genial aquí porque si un formulario tiene columnas que el otro no, 
# las llenará con NA automáticamente.
final_data <- bind_rows(lista_resultados)

# Opcional: Limpiar nombres de columnas (quitar puntos, espacios, etc.)
final_data <- final_data %>% clean_names()

