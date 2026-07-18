# ==============================================================================
# PROYECTO GEIH: PENALIZACIÓN POR INFORMALIDAD Y BRECHA DE GÉNERO
# Script 02: Unión de módulos (Merge) y preparación de variables
# ==============================================================================

library(tidyverse)
library(here)

# 1. Cargar las bases filtradas del Caribe que guardamos en el Paso 1
print("--- Cargando datos temporales ---")
caracteristicas <- readRDS(here("output_temp", "caracteristicas_caribe.rds"))
ocupados <- readRDS(here("output_temp", "ocupados_caribe.rds"))

# 2. Hacer el MERGE (Unión) de Ocupados con Características Generales
print("--- Uniendo módulos (Merge) ---")
datos_unidos <- ocupados %>%
  left_join(caracteristicas, by = c("directorio", "secuencia_p", "orden"))

# Limpiar memoria RAM inmediatamente
rm(caracteristicas, ocupados)
gc()

# ==============================================================================
# 3. CREACIÓN Y LIMPIEZA DE VARIABLES ECONÓMICAS
# ==============================================================================
print("--- Limpiando y creando variables ---")

datos_limpios <- datos_unidos %>%
  # Limpieza inicial de datos geográficos y numéricos básicos
  mutate(
    dpto_clean = str_trim(as.character(dpto.x)),
    clase_clean = as.numeric(clase.x),
    
    # A. IDENTIFICAR CARTAGENA (Bolívar urbana: dpto == "13" y clase == 1)
    es_cartagena = ifelse(dpto_clean == "13" & clase_clean == 1, 1, 0),
    nombre_zona = ifelse(es_cartagena == 1, "Cartagena", "Resto del Caribe"),
    
    # B. GÉNERO (p3271: 1 = Hombre, 2 = Mujer)
    mujer = ifelse(p3271 == 2, 1, 0),
    
    # C. EDAD, NIVEL EDUCATIVO Y GRADO (Mapeo exacto de variables GEIH)
    edad = as.numeric(p6040),       # p6040 es la edad real en años cumplidos
    nivel = as.numeric(p3042),      # p3042 es el nivel educativo alcanzado (1-13)
    grado = as.numeric(p3042s1)     # p3042s1 es el último grado o semestre aprobado
  ) %>% 
  
  # D. CONSTRUCCIÓN OFICIAL DE AÑOS DE EDUCACIÓN (Metodología DANE corregida para los 13 niveles)
  mutate(
    grado = ifelse(is.na(grado), 0, grado), # Reemplazar nulos en grado con 0
    educacion = case_when(
      nivel == 1 | nivel == 2 ~ 0,                          # Ninguno o preescolar
      nivel == 3 ~ grado,                                   # Primaria (1 a 5)
      nivel == 4 ~ grado,                                   # Secundaria (6 a 9 ya viene codificado como 6,7,8,9)
      nivel == 5 | nivel == 6 ~ grado,                      # Media académica o técnica (10 o 11)
      nivel == 7 ~ 11 + grado,                              # Normalista
      nivel == 8 | nivel == 9 ~ 11 + (grado / 2),           # Técnica profesional / Tecnológica (semestres / 2)
      nivel == 10 ~ 11 + (grado / 2),                       # Universitaria (semestres / 2)
      nivel >= 11 & nivel <= 13 ~ 16 + (grado / 2),         # Postgrados (Especialización, Maestría, Doctorado semestres)
      TRUE ~ 0                                              # NS/NR o vacíos
    ),
    # Acotar la educación a un rango lógico
    educacion = ifelse(educacion > 25, 25, educacion)
  ) %>%
  
  # E. INFORMALIDAD (Criterio de Seguridad Social: No cotiza a pensión)
  # p6920: 1 = Sí, 2 = No, 3 = Pensionado. Informal = No cotiza (2)
  mutate(
    informal = ifelse(p6920 == 2, 1, 0)
  ) %>% 
  
  # F. SALARIO MENSUAL Y POR HORA (Trabajo Principal)
  # inglabo es la variable armonizada de ingresos; p6850 son las horas semanales
  mutate(
    salario_mensual = as.numeric(inglabo),
    horas_semanales = as.numeric(p6850),
    salario_hora = salario_mensual / (horas_semanales * 4.333)
  ) %>% 
  
  # G. EXPERIENCIA POTENCIAL (Fórmula clásica de Mincer: Edad - Educación - 6)
  mutate(
    experiencia = edad - educacion - 6,
    experiencia_sq = experiencia^2 
  ) %>% 
  
  # H. FILTRO DE SEGURIDAD (Población activa ocupada con datos consistentes)
  filter(
    edad >= 18 & edad <= 65,            
    salario_hora > 0 & !is.na(salario_hora), 
    !is.na(educacion),                  
    experiencia >= 0                    
  ) %>% 
  
  # Seleccionamos las variables finales limpias para que la base pese muy poco
  # H. FILTRO DE SEGURIDAD (Población activa ocupada con datos consistentes)
  filter(
    edad >= 18 & edad <= 65,            
    horas_semanales > 0 & !is.na(horas_semanales), # <--- AGREGAR ESTA LÍNEA DE SEGURIDAD
    salario_mensual > 0 & !is.na(salario_mensual), # <--- Asegurar que el ingreso sea positivo
    salario_hora > 0 & !is.na(salario_hora), 
    !is.na(educacion),                  
    experiencia >= 0                    
  )

# ==============================================================================
# 4. GUARDAR BASE DE DATOS FINAL Y LIMPIA
# ==============================================================================
saveRDS(datos_limpios, here("output_temp", "datos_caribe_cartagena_limpios.rds"))

print("--- ¡Paso 2 completado con éxito! ---")
print(paste("La base limpia tiene", nrow(datos_limpios), "observaciones listas para el análisis."))
