# ==============================================================================
# PROYECTO GEIH: PENALIZACIÓN POR INFORMALIDAD Y BRECHA DE GÉNERO
# Script 04: Modelos Econométricos (Ecuación de Mincer)
# ==============================================================================

# 1. Preparación del entorno e instalación de paquetes requeridos
if (!require("stargazer", quietly = TRUE)) {
  install.packages("stargazer", repos = "https://cloud.r-project.org")
}

library(tidyverse)
library(here)
library(stargazer)
library(sandwich)

# 2. Cargar la base de datos limpia
print("--- Cargando datos limpios ---")
datos <- readRDS(here("output_temp", "datos_caribe_cartagena_limpios.rds"))

# Filtrar submuestras para las estimaciones
datos_cartagena <- datos %>% filter(es_cartagena == 1)
datos_caribe <- datos    # Toda la región Caribe

# ==============================================================================
# 3. ESTIMACIÓN DE MODELOS PARA CARTAGENA
# ==============================================================================
print("--- Estimando modelos para Cartagena ---")

# Modelo 1: Mincer Tradicional
m1_cartagena <- lm(log(salario_hora) ~ educacion + experiencia + experiencia_sq, 
                   data = datos_cartagena)

# Modelo 2: Incorporando Brecha de Género e Informalidad
m2_cartagena <- lm(log(salario_hora) ~ educacion + experiencia + experiencia_sq + mujer + informal, 
                   data = datos_cartagena)

# Modelo 3: Interacción entre Género e Informalidad (¿Doble penalización?)
m3_cartagena <- lm(log(salario_hora) ~ educacion + experiencia + experiencia_sq + mujer * informal, 
                   data = datos_cartagena)


# ==============================================================================
# 4. ESTIMACIÓN DE MODELOS PARA TODA LA REGIÓN CARIBE (Para comparar)
# ==============================================================================
print("--- Estimando modelos para el Caribe ---")

m1_caribe <- lm(log(salario_hora) ~ educacion + experiencia + experiencia_sq, 
                data = datos_caribe)

m2_caribe <- lm(log(salario_hora) ~ educacion + experiencia + experiencia_sq + mujer + informal, 
                data = datos_caribe)

m3_caribe <- lm(log(salario_hora) ~ educacion + experiencia + experiencia_sq + mujer * informal, 
                data = datos_caribe)

# ==============================================================================
# 5. REPORTAR RESULTADOS CON ERRORES ESTÁNDAR ROBUSTOS (HC1)
# ==============================================================================
print("--- Calculando Errores Estándar Robustos (Huber-White) ---")

# 5.1. Extraer errores estándar robustos para Cartagena (tipo HC1, estándar de Stata)
se_m1_cartagena <- sqrt(diag(vcovHC(m1_cartagena, type = "HC1")))
se_m2_cartagena <- sqrt(diag(vcovHC(m2_cartagena, type = "HC1")))
se_m3_cartagena <- sqrt(diag(vcovHC(m3_cartagena, type = "HC1")))

# 5.2. Extraer errores estándar robustos para la Región Caribe
se_m1_caribe <- sqrt(diag(vcovHC(m1_caribe, type = "HC1")))
se_m2_caribe <- sqrt(diag(vcovHC(m2_caribe, type = "HC1")))
se_m3_caribe <- sqrt(diag(vcovHC(m3_caribe, type = "HC1")))


# --- TABLA 1: CARTAGENA ---
print("TABLA DE REGRESIÓN: MODELOS PARA CARTAGENA (CON ERRORES ROBUSTOS)")
stargazer(m1_cartagena, m2_cartagena, m3_cartagena,
          type = "text",
          se = list(se_m1_cartagena, se_m2_cartagena, se_m3_cartagena), # <-- Aquí le pasamos los robustos
          title = "Ecuación de Mincer para Cartagena (Errores Estándar Robustos HC1)",
          covariate.labels = c("Educación", "Experiencia", "Experiencia²", "Mujer (Dummy)", "Informal (Dummy)", "Mujer x Informal", "Constante")
)

# --- TABLA 2: REGRESIÓN CARIBE ---
print("TABLA DE REGRESIÓN: MODELOS PARA LA REGION CARIBE (CON ERRORES ROBUSTOS)")
stargazer(m1_caribe, m2_caribe, m3_caribe,
          type = "text",
          se = list(se_m1_caribe, se_m2_caribe, se_m3_caribe), # <-- Aquí le pasamos los robustos
          title = "Ecuación de Mincer para la Región Caribe (Errores Estándar Robustos HC1)",
          covariate.labels = c("Educación", "Experiencia", "Experiencia²", "Mujer (Dummy)", "Informal (Dummy)", "Mujer x Informal", "Constante")
)
# ==============================================================================
# 6. GRÁFICO DE MARGENES PREDICTIVOS 
# ==============================================================================
print("--- Generando Gráfico de Efectos Predictivos para Cartagena ---")

# Creamos un escenario sintético/promedio para predecir los salarios en Cartagena
# Fijamos la experiencia en la mediana de la muestra (ej. 20 años)
experiencia_mediana <- median(datos_cartagena$experiencia, na.rm = TRUE)

escenario_prediccion <- expand.grid(
  educacion = seq(0, 20, by = 1),
  experiencia = experiencia_mediana,
  experiencia_sq = experiencia_mediana^2,
  mujer = c(0, 1),
  informal = c(0, 1)
)

# Predecimos el logaritmo del salario y calculamos el error estándar para las bandas de confianza
predicciones <- predict(m3_cartagena, newdata = escenario_prediccion, se.fit = TRUE)

escenario_prediccion <- escenario_prediccion %>%
  mutate(
    fit_log = predicciones$fit,
    se_log = predicciones$se.fit,
    # Convertimos de escala logarítmica a pesos reales (COP) usando la corrección exponencial
    salario_predicho = exp(fit_log),
    salario_bajo = exp(fit_log - 1.96 * se_log),
    salario_alto = exp(fit_log + 1.96 * se_log),
    # Etiquetas limpias para el gráfico
    Sexo = ifelse(mujer == 1, "Mujeres", "Hombres"),
    Estatus = ifelse(informal == 1, "Informal", "Formal")
  )

# Generamos el gráfico de líneas con bandas de confianza (ribbons)
grafico_predicciones <- ggplot(escenario_prediccion, aes(x = educacion, y = salario_predicho, color = Sexo, fill = Sexo)) +
  geom_ribbon(aes(ymin = salario_bajo, ymax = salario_alto), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~Estatus) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ".", decimal.mark = ",")) +
  scale_color_manual(values = c("#1f77b4", "#e377c2")) +
  scale_fill_manual(values = c("#1f77b4", "#e377c2")) +
  labs(
    title = "Efecto Marginal de la Educación sobre el Salario Esperado en Cartagena",
    subtitle = paste0("Valores predichos con intervalos de confianza al 95% (Experiencia fija en ", experiencia_mediana, " años)"),
    x = "Años de Educación Aprobados",
    y = "Salario por Hora Predicho (COP)",
    caption = "Fuente: Estimaciones propias basadas en microdatos de la GEIH (DANE)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "gray30", size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

# Guardar el gráfico en la carpeta de plots
ggsave(here("output_plots", "04_margenes_predictivos_cartagena.png"), grafico_predicciones, width = 9.5, height = 6, dpi = 300)

print("--- ¡Gráfico de márgenes predictivos guardado con éxito! ---")

