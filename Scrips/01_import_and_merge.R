# ==============================================================================
# PROYECTO GEIH: PENALIZACIÓN POR INFORMALIDAD Y BRECHA DE GÉNERO
# Script 01: Importación, unificación de meses y filtro regional (Caribe)
# ==============================================================================

# 1. Cargamos las librerías necesarias
install.packages(c("tidyverse", "haven", "here"))
library(tidyverse)
library(haven)
library(here)


# 2. Definir la ruta de la carpeta "Datos" de forma automática
ruta_datos <- here("Datos")

# Códigos de departamento para la Región Caribe 
departamentos_caribe <- c("08", "13", "20", "23", "44", "47", "70")

# ==============================================================================
# FUNCIÓN PARA IMPORTAR, FILTRAR Y UNIR LOS ARCHIVOS .DTA
# ==============================================================================
importar_modulo_caribe <- function(patron_archivo) {
  
  # Buscar todos los archivos .dta que coincidan con el nombre del módulo dentro de "Datos"
  archivos <- list.files(
    path = ruta_datos, 
    pattern = paste0(".*", patron_archivo, ".*\\.dta$"), 
    recursive = TRUE, 
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if(length(archivos) == 0) {
    stop(paste("No se encontraron archivos .dta en la carpeta Datos con el patrón:", patron_archivo))
  }
  
  message(paste("Se encontraron", length(archivos), "archivos para procesar."))
  
  lista_df <- list()
  
  for (i in seq_along(archivos)) {
    message(paste("Procesando (", i, "/", length(archivos), "):", basename(archivos[i])))
    
    # Leer el archivo .dta de Stata
    temp_df <- read_dta(archivos[i])
    
    # Pasar todos los nombres de columnas a minúsculas para evitar discordancias entre meses
    colnames(temp_df) <- tolower(colnames(temp_df))
    
    # Limpieza y estandarización de la variable de departamento (dpto)
    temp_df <- temp_df %>% 
      mutate(
        # Convertir a texto limpio si viene como factor etiquetado de Stata
        dpto_clean = as.character(as_factor(dpto)),
        # Extraer solo los números si el texto viene acompañado del nombre (ej: "08 - Atlántico" -> "08")
        dpto_clean = str_extract(dpto_clean, "^\\d+"),
        # Rellenar con ceros a la izquierda si viene como número puro (ej: "8" -> "08")
        dpto_clean = str_pad(dpto_clean, width = 2, pad = "0")
      )
    
    # Filtrar de inmediato para conservar solo la Región Caribe (así la RAM no colapsa)
    temp_caribe <- temp_df %>% 
      filter(dpto_clean %in% departamentos_caribe) %>% 
      mutate(dpto = dpto_clean) %>% 
      select(-dpto_clean) # Borramos la variable temporal
    
    lista_df[[i]] <- temp_caribe
  }
  
  # Unir todos los meses en un único data frame
  modulo_unificado <- bind_rows(lista_df)
  return(modulo_unificado)
}

# ==============================================================================
# EJECUCIÓN DEL PROCESAMIENTO
# ==============================================================================

# Creamos automáticamente una carpeta para guardar los resultados
ruta_salida <- here("output_temp")
if(!dir.exists(ruta_salida)) dir.create(ruta_salida)

# 1. Importar módulo de Características Generales (Personas)
print("--- PROCESANDO CARACTERÍSTICAS GENERALES ---")
caracteristicas_caribe <- importar_modulo_caribe("Características generales")
saveRDS(caracteristicas_caribe, here("output_temp", "caracteristicas_caribe.rds"))

# 2. Importar módulo de Ocupados
print("--- PROCESANDO OCUPADOS ---")
ocupados_caribe <- importar_modulo_caribe("Ocupados")
saveRDS(ocupados_caribe, here("output_temp", "ocupados_caribe.rds"))

print("¡PROCESO FINALIZADO CON ÉXITO! Los archivos filtrados están guardados en 'output_temp'.")
