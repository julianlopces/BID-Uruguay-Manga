# Cargar librerías necesarias
library(dplyr)
library(readxl) # Para leer archivos Excel
library(stringr)

# 1. Cargar tus archivos
# Cambia los nombres de archivo por tus rutas reales
df_original <- read.csv("G:/Unidades compartidas/PROYECTOS 🌍📂/🥁 PROYECTOS/🏆 Proyectos S. Desarrollo/163. BID Uruguay - Manga/2. Implementación/Componente cuantitativo/Línea de Base/05_Auditoria/cases_manga (2).csv", stringsAsFactors = FALSE)
df_encuesta <- base_manga

# 2. Asegurarse de que el ID (Padrón) sea del mismo tipo en ambos (ej: carácter)
df_original$id <- as.character(df_original$id)
df_encuesta$id <- as.character(df_encuesta$id)
# 1. Función de limpieza para concatenar sin errores de NA
clean_txt <- function(x) {
  ifelse(is.na(x) | x == "" | x == "NA", "", as.character(x))
}

# 2. Preparar la encuesta: Última observación y Cálculo de Dirección Completa
df_encuesta_prep <- base_manga %>%
  arrange(id, desc(instance_time)) %>% 
  distinct(id, .keep_all = TRUE) %>%
  mutate(
    # Replicamos el cálculo complejo de dirección
    pub_direccion_full = str_squish(paste0(
      clean_txt(direccion), " ", 
      clean_txt(n_predio), 
      " Piso ", clean_txt(piso), 
      " Apartamento ", clean_txt(apartamento), 
      " Ref padrón ", clean_txt(referencia_padron), 
      " Ref vivienda ", clean_txt(referencia_vivienda)
    ))
  ) %>%
  # Nos quedamos con el ID y todas las variables que empiezan con "pub_"
  select(id, starts_with("pub_"), fecha) 

# 3. Merge con el archivo original
df_merge <- df_original %>%
  mutate(id = as.character(id)) %>%
  left_join(df_encuesta_prep %>% mutate(id = as.character(id)), by = "id")

# 4. REEMPLAZO MANUAL (Lógica final)
df_final <- df_merge %>%
  mutate(
    # Priorizamos la DIRECCIÓN CALCULADA
    direccion = coalesce(pub_direccion_full, as.character(direccion)),
    
    # Estatus y Seguimiento
    status    = coalesce(as.character(pub_status), as.character(status)),
    visita    = coalesce(as.numeric(pub_visita), as.numeric(visita)),
    intento   = coalesce(as.numeric(pub_intento), as.numeric(intento)),
    
    # Citas y Reprogramación
    cita      = coalesce(as.character(pub_reprogramacion), as.character(cita)),
    fecha     = coalesce(as.character(fecha.y), as.character(fecha.x)), # Por si el merge creó .x .y
    
    # Datos de contacto (Forzando texto para evitar errores)
    nombre_participante = coalesce(as.character(pub_nombre), as.character(nombre_participante)),
    telefono_1 = coalesce(as.character(pub_telf_cita), as.character(telefono_1)),
    telefono_2 = coalesce(as.character(pub_telf_1), as.character(telefono_2)),
    telefono_3 = coalesce(as.character(pub_telf_2), as.character(telefono_3)),
    telefono_4 = coalesce(as.character(pub_telf_3), as.character(telefono_4)),
    
    # Gestión
    formids = coalesce(as.character(pub_form_id), as.character(formids)),
    users   = coalesce(as.character(pub_users), as.character(users)),
    comentarios_enc = coalesce(as.character(pub_comentario), as.character(comentarios_enc))
  ) %>%
  # 5. PASO CRÍTICO: Solo columnas originales
  select(any_of(colnames(df_original)))

# Sustituye con el ID o URL de tu Google Sheet
id_sheet <- "https://docs.google.com/spreadsheets/d/1ZckM2kX8c8DliCOu6iBkN53gsCIct2sQWInuA73kNEk/edit?gid=1325784437#gid=1325784437"


sheet_write(df_final, ss = id_sheet, sheet = "cases_manga_0320") 