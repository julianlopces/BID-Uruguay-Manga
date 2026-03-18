#### Script BID Uruguay --------------------------------------------------------
# Proyecto: BID Uruguay Manga
# Este script crea las alertas de auditoría y exporta los resultados a Google Sheets

# 0. Limpieza del entorno
rm(list = ls())

# 1. Gestión de paquetes con pacman
if (!require("pacman")) install.packages("pacman")
library(pacman)

# 2. Carga de librerías solicitadas
p_load(
  tidyverse,      # Incluye dplyr, stringr, lubridate, etc.
  httr,           # Importar JSON/APIs
  jsonlite,       # JSON a data frame
  googledrive,    # Gestionar Google Drive
  googlesheets4,  # Gestionar Google Sheets
  writexl,        # Escribir Excel
  readxl,         # Leer Excel
  haven,          # SPSS y STATA
  labelled,       # Etiquetas (labels)
  gtsummary,      # Tablas resumen
  stringi,        # Procesamiento de texto
  stringdist,     # Distancia de caracteres
  fuzzyjoin,      # Cruces difusos
  janitor,        # Limpieza de datos
  dotenv,         # Variables de entorno
  sf              # Datos espaciales
)

# 3. Instalación y carga de odkmissing
if (!requireNamespace("odkmissing", quietly = TRUE)) {
  if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
  pak::pkg_install("julianlopces/odkmissing")
}
library(odkmissing)
project_path <- getwd()
message("Directorio base: ", project_path)

# Helper para validar env vars
is_blank <- function(x) is.na(x) || !nzchar(x)

if (Sys.getenv("GITHUB_ACTIONS") == "true") {
  message("Cargando credenciales desde secretos en GitHub Actions...")
  server_name    <- Sys.getenv("SCTO_SERVER")
  api_user  <- Sys.getenv("SCTO_USER")
  api_pw     <- Sys.getenv("SCTO_PW")
  creds     <- Sys.getenv("GOOGLE_SHEETS_CREDENTIALS")

  # Validación temprana
  if (any(vapply(c(server_name, api_user, api_pw, creds), is_blank, logical(1)))) {
    stop("Faltan credenciales requeridas en variables de entorno de Actions.")
  }
  
  # Escribir JSON a archivo temporal
  temp_creds_file <- tempfile(fileext = ".json")
  writeLines(creds, temp_creds_file)
  
  # Autenticación con cuenta de servicio (sin prompts)
  googledrive::drive_auth(path = temp_creds_file, cache = ".secrets")
  googlesheets4::gs4_auth(path = temp_creds_file)
  
} else {
  # Local
  if (file.exists(".env")) {
    message("Archivo .env encontrado en: ", project_path)
    dotenv::load_dot_env(".env")
    
    server_name    <- Sys.getenv("SCTO_SERVER")
    api_user  <- Sys.getenv("SCTO_USER")
    api_pw     <- Sys.getenv("SCTO_PW")
    creds     <- Sys.getenv("GOOGLE_SHEETS_CREDENTIALS")
    
    if (any(vapply(c(server_name, api_user, api_pw, creds), is_blank, logical(1)))) {
      stop("Faltan credenciales requeridas en .env (o la ruta del JSON).")
    }
    
    googledrive::drive_auth(path = creds, cache = ".secrets")
    googlesheets4::gs4_auth(path = creds)
    
  } else {
    stop("No se encontró .env. Configúralo o exporta variables de entorno.")
  }
}

message("Credenciales cargadas correctamente.")
message("- Email Google Sheets: ", api_user)

# Función para cargar scripts secundarios
load_script <- function(script_name) {
  script_path <- file.path(project_path, "05_Auditoria", script_name)
  if (file.exists(script_path)) {
    message("Ejecutando script: ", script_name)
    source(script_path)
  } else {
    stop(paste("No se encontró el script:", script_path))
  }
}

# Ejecutar scripts secundarios en orden
load_script("1_Importar_data.R")
load_script("2_Alertas_data.R")
load_script("3_Monitoreo_data.R")
load_script("4_Exportar_data.R")

message("Pipeline completado exitosamente.")
