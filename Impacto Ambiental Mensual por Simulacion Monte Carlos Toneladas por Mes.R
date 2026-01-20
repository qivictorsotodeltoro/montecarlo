# ============================================================
# Simulación Monte Carlo – Visualización Interactiva Mensual
# Histogramas + ECDF por mes
# Guardado como objetos RDS
# ============================================================

# ---- Librerías --------------------------------------------
library(dplyr)
library(ggplot2)
library(plotly)

# ---- Carga de datos ---------------------------------------
ruta <- file.choose()
masa <- readRDS(ruta)

# ---- Conversión a data frame largo ------------------------
df <- data.frame(
  Mes = rep(names(masa), lengths(masa)),
  x   = unlist(masa, use.names = FALSE)
)

df$x <- round(df$x / 1000, 2)

# ---- Estadísticos por mes ---------------------------------
stats <- df %>%
  group_by(Mes) %>%
  summarise(
    media   = mean(x),
    ic_low  = quantile(x, 0.025),
    ic_high = quantile(x, 0.975),
    .groups = "drop"
  )

# ============================================================
# Funciones de visualización
# ============================================================

# Colores institucionales
verde <- "#265129"
oro   <- "#E3A600"

# ============================================================
# Función para Histogramas y Densidades (Plotly) con colores institucionales
# ============================================================

plot_hist_mes_ly <- function(df, stats, mes) {
  
  df_m <- df %>% filter(Mes == mes)
  st_m <- stats %>% filter(Mes == mes)
  
  p <- ggplot(df_m, aes(x)) +
    # Histograma con relleno amarillo
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 50,
      fill = oro
      #color = "grey40"
    ) +
    # Curva de densidad verde
    geom_density(
      bw = 0.010,
      linewidth = 0.8,
      color = verde
    ) +
    # Barras verticales (media e IC) en verde
    geom_vline(aes(xintercept = st_m$media), color = verde, linewidth = 1) +
    geom_vline(aes(xintercept = st_m$ic_low),  color = verde, linetype = "dashed") +
    geom_vline(aes(xintercept = st_m$ic_high), color = verde, linetype = "dashed") +
    # Anotación del mes
    annotate(
      "text",
      x = 2.68, y = 15.5,
      label = mes,
      hjust = 1, vjust = 1,
      size = 4, fontface = "bold",
      color = "grey20"
    ) +
    coord_cartesian(xlim = c(2, 2.7), ylim = c(0, 16)) +
    scale_x_continuous(breaks = seq(2, 2.7, 0.1)) +
    labs(x = "Ton / mes", y = "Densidad") +
    theme_minimal(base_size = 11)
  
  # Convertir a plotly sin título adicional
  ggplotly(p)
}


# ============================================================
# Función ECDF (Plotly) con colores institucionales
# ============================================================

plot_ecdf_mes <- function(df, stats, mes) {
  
  df_m <- df %>% filter(Mes == mes)
  st_m <- stats %>% filter(Mes == mes)
  
  p <- ggplot(df_m, aes(x)) +
    # ECDF verde
    stat_ecdf(linewidth = 1, color = verde) +
    # Barras verticales (media e IC) en verde
    geom_vline(aes(xintercept = st_m$media), color = verde, linewidth = 1) +
    geom_vline(aes(xintercept = st_m$ic_low),  color = verde, linetype = "dashed") +
    geom_vline(aes(xintercept = st_m$ic_high), color = verde, linetype = "dashed") +
    coord_cartesian(xlim = c(2, 2.7), ylim = c(0, 1)) +
    scale_x_continuous(breaks = seq(2, 2.7, 0.1)) +
    scale_y_continuous(breaks = seq(0, 1, 0.25)) +
    labs(x = "Ton / mes", y = "F(x)") +
    theme_minimal(base_size = 11)
  
  ggplotly(p)
}


# ============================================================
# Conversión a objetos plotly individuales
# ============================================================

h1 <- ggplotly(plot_hist_mes_ly(df, stats, "Mes1"))
h2 <- ggplotly(plot_hist_mes_ly(df, stats, "Mes2"))
h3 <- ggplotly(plot_hist_mes_ly(df, stats, "Mes3"))
h4 <- ggplotly(plot_hist_mes_ly(df, stats, "Mes4"))
h5 <- ggplotly(plot_hist_mes_ly(df, stats, "Mes5"))
h6 <- ggplotly(plot_hist_mes_ly(df, stats, "Mes6"))

e1 <- ggplotly(plot_ecdf_mes(df, stats, "Mes1"))
e2 <- ggplotly(plot_ecdf_mes(df, stats, "Mes2"))
e3 <- ggplotly(plot_ecdf_mes(df, stats, "Mes3"))
e4 <- ggplotly(plot_ecdf_mes(df, stats, "Mes4"))
e5 <- ggplotly(plot_ecdf_mes(df, stats, "Mes5"))
e6 <- ggplotly(plot_ecdf_mes(df, stats, "Mes6"))

# ===================== Crear visualizaciones =====================
viz_12 <- subplot(h1, h2, e1, e2, nrows = 2, shareX = TRUE, titleX = TRUE, titleY = TRUE)
viz_34 <- subplot(h3, h4, e3, e4, nrows = 2, shareX = TRUE, titleX = TRUE, titleY = TRUE)
viz_56 <- subplot(h5, h6, e5, e6, nrows = 2, shareX = TRUE, titleX = TRUE, titleY = TRUE)


# ============================================================
# Agregar encabezado discreto a cada visualización
# ============================================================

# Función auxiliar para agregar título discreto a un plotly en bold
add_discrete_title <- function(plotly_obj, title_text = "Impacto Ambiental en Toneladas/Mes (Monte Carlo)") {
  plotly_obj %>%
    layout(
      annotations = list(
        list(
          text = paste0("<b>", title_text, "</b>"),  # Texto en bold
          x = 0.01,         # esquina superior izquierda
          y = 1.050,        # un poco por encima de la figura
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          align = "left",
          font = list(size = 13.5, color = "black")
        )
      )
    )
}


# ===================== Agregar encabezado discreto =====================
viz_12 <- add_discrete_title(viz_12)
viz_34 <- add_discrete_title(viz_34)
viz_56 <- add_discrete_title(viz_56)

# ===================== Guardado como RDS =====================
saveRDS(viz_12, file = "viz_meses_1_2.rds")
saveRDS(viz_34, file = "viz_meses_3_4.rds")
saveRDS(viz_56, file = "viz_meses_5_6.rds")




# ============================================================
# Uso posterior (cuando alguien más lo abra)
# ============================================================

library(plotly)

viz_12 <- readRDS("viz_meses_1_2.rds")
viz_12

viz_34 <- readRDS("viz_meses_3_4.rds")
viz_34

viz_56 <- readRDS("viz_meses_5_6.rds")
viz_56
