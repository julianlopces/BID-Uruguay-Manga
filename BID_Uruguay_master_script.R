#### Script BID Uruguay --------------------------------------------------------
# Proyecto: BID Uruguay Manga
# Este script centraliza el pipeline de auditoría y exportación

# 0. Limpieza del entorno
rm(list = ls())

# 1. Gestión de paquetes (Solo los estrictamente necesarios para el flujo)
if (!require("pacman")) install.packages("pacman")
library(pacman)

# 2. Carga de librerías optimizada para GitHub Actions
p_load(
  dplyr,          # Manipulación de datos (mutate, group_by, etc.)
  tidyr,          # Limpieza de datos (pivot, drop_na)
  stringr,        # Manipulación de texto básica
  lubridate,      # Gestión de fechas y starttime
  httr,           # API SurveyCTO
  jsonlite,       # Parsear JSON
  googledrive,    # Google Drive
  googlesheets4,  # Google Sheets
  janitor,        # clean_names()
  dotenv,         # Variables de entorno
  sf,             # Cálculos espaciales
  stringi         # stri_trans_general (para limpieza de tildes/basura)
)

# 3. Instalación de odkmissing (Repositorio externo)
if (!requireNamespace("odkmissing", quietly = TRUE)) {
  if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
  pak::pkg_install("julianlopces/odkmissing")
}
library(odkmissing)

project_path <- getwd()
message("Directorio base: ", project_path)

# Helper para validar variables de entorno
is_blank <- function(x) is.na(x) || !nzchar(x)

# 4. Manejo de Credenciales (GitHub Actions vs Local)
if (Sys.getenv("GITHUB_ACTIONS") == "true") {
  message("Modo: GitHub Actions detectado.")
  
  server_name <- Sys.getenv("SCTO_SERVER")
  api_user    <- Sys.getenv("SCTO_USER")
  api_pw      <- Sys.getenv("SCTO_PW")
  creds_json  <- Sys.getenv("GOOGLE_SHEETS_CREDENTIALS")
  
  if (any(vapply(c(server_name, api_user, api_pw, creds_json), is_blank, logical(1)))) {
    stop("Error: Faltan secretos en la configuración de GitHub.")
  }
  
  # Crear archivo temporal para el JSON de Google
  creds <- tempfile(fileext = ".json")
  writeLines(creds_json, creds)
  
  # Autenticación no interactiva
  drive_auth(path = creds)
  gs4_auth(path = creds)
  
} else {
  message("Modo: Ejecución local detectada.")
  if (file.exists(".env")) {
    dotenv::load_dot_env(".env")
    server_name <- Sys.getenv("SCTO_SERVER")
    api_user    <- Sys.getenv("SCTO_USER")
    api_pw      <- Sys.getenv("SCTO_PW")
    creds       <- Sys.getenv("GOOGLE_SHEETS_CREDENTIALS") # Ruta al archivo local
    
    drive_auth(path = creds, cache = ".secrets")
    gs4_auth(path = creds)
  } else {
    stop("No se encontró el archivo .env para ejecución local.")
  }
}

message("Conexión establecida con éxito.")

# 5. Función para ejecutar scripts secundarios
load_script <- function(script_name) {
  script_path <- file.path(project_path, "05_Auditoria", script_name)
  if (file.exists(script_path)) {
    message("--- Iniciando: ", script_name, " ---")
    source(script_path)
  } else {
    stop("Error crítico: No se encontró el script en ", script_path)
  }
}

# 6. EJECUCIÓN DEL PIPELINE
load_script("1_Importar_data.R")
load_script("2_Alertas_data.R")
load_script("3_Monitoreo_data.R")
load_script("4_Exportar_data.R")

message("--- Pipeline BID Uruguay finalizado con éxito ---")
