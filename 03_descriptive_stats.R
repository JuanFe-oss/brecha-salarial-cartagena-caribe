# ==============================================================================
# PROYECTO GEIH: PENALIZACIÓN POR INFORMALIDAD Y BRECHA DE GÉNERO
# Script 03: Estadísticas Descriptivas y Visualización
# ==============================================================================

library(tidyverse)
library(here)
library(scales) # Para dar formato de moneda a los ejes de los gráficos

# 1. Cargar la base de datos limpia de 82k observaciones
print("--- Cargando datos limpios ---")
datos <- readRDS(here("output_temp", "datos_caribe_cartagena_limpios.rds"))

# Crear la carpeta 'output_plots' para guardar el gráfico
if(!dir.exists(here("output_plots"))) {
  dir.create(here("output_plots"))
}

# ==============================================================================
# 2. TABLA 1: CARACTERIZACIÓN GENERAL DE LA MUESTRA
# ==============================================================================
print("--- Generando Tabla Descriptiva General ---")

tabla_descriptiva <- datos %>%
  group_by(nombre_zona) %>%
  summarise(
    Muestra_N = n(),
    Edad_Promedio = mean(edad, na.rm = TRUE),
    Educacion_Promedio = mean(educacion, na.rm = TRUE),
    Tasa_Informalidad = mean(informal, na.rm = TRUE) * 100,
    Salario_Hora_Promedio = mean(salario_hora, na.rm = TRUE),
    Salario_Mensual_Promedio = mean(salario_mensual, na.rm = TRUE),
    Porcentaje_Mujeres = mean(mujer, na.rm = TRUE) * 100
  )

print("TABLA 1: Características de la fuerza laboral (Cartagena vs. Resto del Caribe)")
print(t(tabla_descriptiva)) # Se transpone para que sea más fácil de leer en la consola


# ==============================================================================
# 3. TABLA 2: BRECHA SALARIAL Y DE INFORMALIDAD POR GÉNERO
# ==============================================================================
print("--- Generando Tabla de Brechas de Género ---")

tabla_genero <- datos %>%
  group_by(nombre_zona, mujer) %>%
  summarise(
    Muestra_N = n(),
    Educacion = mean(educacion, na.rm = TRUE),
    Tasa_Informalidad = mean(informal, na.rm = TRUE) * 100,
    Salario_Hora = mean(salario_hora, na.rm = TRUE),
    Horas_Semanales = mean(horas_semanales, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(Sexo = ifelse(mujer == 1, "Mujeres", "Hombres")) %>%
  select(nombre_zona, Sexo, Muestra_N, Educacion, Tasa_Informalidad, Salario_Hora, Horas_Semanales)

print("TABLA 2: Estadísticas por Género y Zona (0 = Hombre, 1 = Mujer)")
print(tabla_genero)



# ==============================================================================
# 4. GRÁFICO 1: DISTRIBUCIÓN DEL SALARIO POR HORA (DENSIDAD)
# ==============================================================================
print("--- Generando Gráfico 1: Densidad de Salarios ---")

grafico_densidad <- ggplot(datos, aes(x = log(salario_hora), fill = factor(informal))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~nombre_zona) +
  scale_fill_manual(
    values = c("#2b8cbe", "#de2d26"), 
    labels = c("Formal", "Informal"),
    name = "Estatus Laboral"
  ) +
  labs(
    title = "Distribución del Ingreso Laboral por Hora",
    subtitle = "Comparación entre el sector formal e informal (Escala Logarítmica)",
    x = "Logaritmo del Salario por Hora",
    y = "Densidad",
    caption = "Fuente: Microdatos GEIH (DANE). Cálculos propios."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray30", size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

ggsave(here("output_plots", "01_distribucion_salarios.png"), grafico_densidad, width = 9, height = 5.5, dpi = 300)


# ==============================================================================
# 5. GRÁFICO 2: COMPARACIÓN DE SALARIOS PROMEDIO (BRECHA DE GÉNERO Y ENTRADA)
# ==============================================================================
print("--- Generando Gráfico 2: Barras de Brecha Salarial ---")

# Preparar datos agregados para el gráfico de barras
datos_barras <- datos %>%
  group_by(nombre_zona, Sexo = ifelse(mujer == 1, "Mujeres", "Hombres"), Informalidad = ifelse(informal == 1, "Informal", "Formal")) %>%
  summarise(Salario_Promedio = mean(salario_hora, na.rm = TRUE), .groups = 'drop')

grafico_brecha <- ggplot(datos_barras, aes(x = Sexo, y = Salario_Promedio, fill = Sexo)) +
  geom_col(position = "dodge", alpha = 0.85, width = 0.7) +
  facet_grid(nombre_zona ~ Informalidad) +
  # Especificamos explícitamente tanto el separador de miles (.) como el decimal (,)
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ".", decimal.mark = ",")) +
  scale_fill_manual(values = c("#1f77b4", "#e377c2")) + # Azul para hombres, rosa/magenta para mujeres
  labs(
    title = "Brecha Salarial de Género por Estatus Laboral",
    subtitle = "Salario promedio por hora (en COP) según formalidad y zona geográfica",
    x = NULL,
    y = "Salario Promedio por Hora (COP)",
    caption = "Fuente: Microdatos GEIH (DANE). Cálculos propios."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray30", size = 11),
    legend.position = "none", # No hace falta leyenda porque el eje X ya dice el sexo
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

ggsave(here("output_plots", "02_brecha_salarial_genero.png"), grafico_brecha, width = 9, height = 6, dpi = 300)

# ==============================================================================
# 6. GRÁFICO 3: DISTRIBUCIÓN DE LA EDUCACIÓN POR ESTATUS LABORAL (BOXPLOT)
# ==============================================================================
print("--- Generando Gráfico 3: Boxplot de Educación ---")

grafico_educacion <- ggplot(datos, aes(x = factor(informal, labels = c("Formal", "Informal")), y = educacion, fill = factor(informal))) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.1, outlier.color = "gray") +
  facet_wrap(~nombre_zona) +
  scale_fill_manual(values = c("#2b8cbe", "#de2d26")) +
  labs(
    title = "Años de Educación según Estatus Laboral",
    subtitle = "Distribución y mediana de los años de escolaridad formal",
    x = "Estatus Laboral",
    y = "Años de Educación Aprobados",
    caption = "Fuente: Microdatos GEIH (DANE). Cálculos propios."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray30", size = 11),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

ggsave(here("output_plots", "03_boxplot_educacion.png"), grafico_educacion, width = 9, height = 5.5, dpi = 300)

print("--- ¡Los 3 gráficos han sido exportados con éxito a la carpeta 'output_plots/'! ---")
print("--- ¡Paso 3 completado! ---")

# ==============================================================================
# 7. MATRIZ DE CORRELACIONES Y MAPA DE CALOR
# ==============================================================================
print("--- Generando Gráfico 05 y 06: Correlaciones ---")

# Forzar la instalación y carga de las librerías necesarias
if(!require(corrplot)) install.packages("corrplot")
if(!require(ggcorrplot)) install.packages("ggcorrplot", dependencies = TRUE)
library(corrplot)
library(ggcorrplot)

# 1. Crear un dataframe con el logaritmo del salario y las variables numéricas clave
# Usamos exactamente los nombres de variables de tu script ('salario_hora', 'educacion', 'edad', 'informal', 'mujer')
datos_correlacion <- datos %>%
  mutate(log_salario_hora = log(salario_hora)) %>%
  select(
    `Log(Salario Hora)` = log_salario_hora,
    `Educación` = educacion,
    `Edad` = edad,
    `Informalidad` = informal,
    `Mujer` = mujer
  )

# 2. Calcular la matriz de correlación omitiendo valores nulos
matriz_corr <- cor(datos_correlacion, use = "complete.obs")


# 3. EXPORTAR ARCHIVO 05: Matriz de Correlación Numérica Clásica (corrplot)
png(filename = here("output_plots", "05_matriz_correlaciones.png"), 
    width = 800, height = 800, res = 120)

corrplot(matriz_corr, 
         method = "number", 
         type = "upper", 
         tl.col = "black", 
         cl.pos = "n", 
         number.digits = 2)

dev.off()


# 4. EXPORTAR ARCHIVO 06: Mapa de Calor Estético (ggcorrplot con contraste corregido)
grafico_calor <- ggcorrplot(matriz_corr, 
                            hc.order = FALSE, 
                            type = "lower",   
                            lab = TRUE,       
                            lab_size = 3.5,     # Tamaño ideal para los números
                            lab_col = "black",  # Fuerza el texto negro para que los números bajitos se lean bien
                            colors = c("#de2d26", "white", "#2b8cbe"), # Paleta consistente con tus gráficos (Rojo/Azul)
                            title = "Mapa de Calor: Correlaciones del Mercado Laboral", 
                            ggtheme = theme_minimal(base_size = 12)) +
  # Subtítulo y pie de página añadidos de forma compatible mediante ggplot
  labs(
    subtitle = "Variables clave de la fuerza laboral (Caribe 2025)",
    caption = "Fuente: Microdatos GEIH (DANE). Cálculos propios."
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray30", size = 11),
    panel.grid.minor = element_blank()
  )

ggsave(here("output_plots", "06_mapa_de_calor.png"), grafico_calor, width = 7, height = 6, dpi = 300)

print("--- ¡Gráficos 05 y 06 exportados con éxito/'! ---")