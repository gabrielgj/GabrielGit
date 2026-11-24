library(shiny)
library(bslib)
library(tidyverse)
library(scales)
library(sf)
library(geobr)

# ------------------------------------------------------------------
# DADOS (carregados uma única vez, quando o app inicia)
# ------------------------------------------------------------------
# Gerado no script de tratamento:
#   save(emendas_municipio, file = "emendas_municipio.RData")
load("emendas_municipio.RData")

# Malha dos municípios: baixa uma vez e guarda em cache local
if (file.exists("malha_municipios.rds")) {
  malha <- readRDS("malha_municipios.rds")
} else {
  malha <- read_municipality(code_muni = "all", year = 2020) |>
    select(codigo_ibge = code_muni, geom)
  saveRDS(malha, "malha_municipios.rds")
}

# Centroides dos municípios (usados no mapa de bolhas)
malha_pts <- suppressMessages(suppressWarnings(st_centroid(malha)))

anos <- emendas_municipio |>
  filter(!is.na(emenda_pc_real)) |>
  pull(ano) |> unique() |> sort(decreasing = TRUE)

ufs <- emendas_municipio |> filter(!is.na(uf)) |> pull(uf) |> unique() |> sort()

anos_serie <- sort(anos)

# Paletas e opções
cores_regiao <- c(NE = "#D9B44A", NO = "#6E8B3D", CO = "#D98989",
                  SE = "#7BAFD4", SU = "#B29AD8")
cores_linhas <- c("#4E79A7", "#F28E2B", "#59A14F", "#E15759", "#B07AA1", "#76B7B2")

vars_x <- c(
  "População"                   = "populacao",
  "Receita total per capita"    = "rec_total_pc_real",
  "Receita própria per capita"  = "rec_propria_pc_real",
  "Transferências per capita"   = "rec_transf_pc_real",
  "PIB per capita"              = "pib_pc_real"
)

fmt <- label_number(big.mark = ".", decimal.mark = ",")

# arredonda para baixo mantendo 2 algarismos significativos (quebras da legenda)
arredonda_baixo <- function(x) {
  esc <- 10^(floor(log10(x)) - 1)
  floor(x / esc) * esc
}

# ------------------------------------------------------------------
# UI
# ------------------------------------------------------------------
ui <- page_navbar(
  title = "Emendas parlamentares nos municípios",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  # 1. Medidas de posição ------------------------------------------
  nav_panel(
    "Medidas de posição",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("pos_ano", "Ano", choices = anos, selected = anos[1]),
        selectInput("pos_uf", "Recorte", choices = c("Brasil", ufs), selected = "Brasil"),
        checkboxInput("pos_facet", "Separar por região", FALSE)
      ),
      layout_columns(
        col_widths = c(12, 12),
        plotOutput("g_pos_barra", height = "350px"),
        plotOutput("g_pos_serie", height = "350px")
      )
    )
  ),

  # 2. Dispersão -----------------------------------------------------
  nav_panel(
    "Dispersão",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("sc_ano", "Ano", choices = anos, selected = anos[1]),
        selectInput("sc_x", "Eixo X", choices = vars_x, selected = "pib_pc_real"),
        selectInput("sc_reg", "Região", selected = "Todas",
                    choices = c("Todas", names(cores_regiao))),
        radioButtons("sc_metodo", "Linha de tendência",
                     choices = c("Linear" = "lm", "Loess" = "loess", "Nenhuma" = "nenhuma"),
                     selected = "lm"),
        checkboxInput("sc_corte", "Cortar outliers (P99 nos dois eixos)", TRUE),
        checkboxInput("sc_tam", "Tamanho do ponto = emenda total", FALSE),
        checkboxInput("sc_facet", "Separar por região", FALSE)
      ),
      plotOutput("g_scatter", height = "600px")
    )
  ),

  # 3. Boxplot por UF --------------------------------------------------
  nav_panel(
    "Boxplot por UF",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("uf_ano", "Ano", choices = anos, selected = anos[1]),
        numericInput("uf_ylim", "Limite do eixo Y (R$)", value = 3000, min = 100, step = 500),
        checkboxInput("uf_pontos", "Mostrar pontos", FALSE)
      ),
      plotOutput("g_box_uf", height = "600px")
    )
  ),

  # 4. Boxplot por faixa do FPM ---------------------------------------
  nav_panel(
    "Boxplot por faixa do FPM",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("fpm_ano", "Ano", choices = anos, selected = anos[1]),
        selectInput("fpm_uf", "Recorte", choices = c("Brasil", ufs), selected = "Brasil"),
        checkboxInput("fpm_facet", "Separar por região", TRUE),
        numericInput("fpm_ylim", "Limite do eixo Y (R$)", value = 2000, min = 100, step = 500),
        checkboxInput("fpm_pontos", "Mostrar pontos", FALSE)
      ),
      plotOutput("g_box_fpm", height = "600px")
    )
  ),

  # 5. Mapa ----------------------------------------------------------
  nav_panel(
    "Mapa",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("mp_ano", "Ano", choices = anos, selected = anos[1]),
        selectInput("mp_uf", "Recorte", choices = c("Brasil", ufs), selected = "Brasil"),
        radioButtons("mp_estilo", "Estilo do mapa",
                     choices = c("Território pintado" = "area",
                                 "Bolhas (tamanho = emenda total)" = "bolhas")),
        conditionalPanel(
          condition = "input.mp_estilo == 'bolhas'",
          sliderInput("mp_tam", "Tamanho máximo das bolhas",
                      min = 4, max = 40, value = 12, step = 1),
          sliderInput("mp_contraste", "Contraste entre bolhas pequenas e grandes",
                      min = 1, max = 3, value = 1.5, step = 0.25),
          helpText("Contraste 1 = área proporcional ao valor;",
                   "valores maiores aumentam a diferença entre bolhas pequenas e grandes.")
        ),
        radioButtons("mp_tipo", "Escala de cor",
                     choices = c("Percentil" = "pct",
                                 "R$ per capita (topo no P95)" = "valor")),
        helpText("Percentis e P95 são calculados sobre todos os municípios do país,",
                 "mesmo quando o recorte é uma UF.")
      ),
      plotOutput("g_mapa", height = "700px")
    )
  )
)

# ------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------
server <- function(input, output, session) {

  dados <- function(ano) emendas_municipio |> filter(ano == !!as.integer(ano))

  # 1. Medidas de posição --------------------------------------------
  # medidas de posição conforme o recorte (estados) e a separação por região
  percentis <- reactive({
    d <- emendas_municipio |> filter(!is.na(emenda_pc_real))
    if (input$pos_uf != "Brasil") d <- d |> filter(uf == input$pos_uf)
    if (input$pos_facet) d <- d |> filter(!is.na(regiao))
    validate(need(nrow(d) > 0, "Sem dados para essa seleção."))

    grupos <- if (input$pos_facet) c("ano", "regiao") else "ano"

    d |>
      group_by(across(all_of(grupos))) |>
      summarise(
        Media   = mean(emenda_pc_real, na.rm = TRUE),
        Mediana = median(emenda_pc_real, na.rm = TRUE),
        P99 = quantile(emenda_pc_real, 0.99, na.rm = TRUE),
        P95 = quantile(emenda_pc_real, 0.95, na.rm = TRUE),
        P90 = quantile(emenda_pc_real, 0.90, na.rm = TRUE),
        P10 = quantile(emenda_pc_real, 0.10, na.rm = TRUE),
        .groups = "drop"
      ) |>
      pivot_longer(c(Media, Mediana, P99, P95, P90, P10),
                   names_to = "Estatistica", values_to = "Valor") |>
      mutate(Estatistica = factor(Estatistica,
                                  levels = c("P99", "P95", "P90", "Media", "Mediana", "P10")))
  })

  escopo_pos <- reactive(input$pos_uf)

  output$g_pos_barra <- renderPlot({
    p <- percentis() |>
      filter(ano == as.integer(input$pos_ano)) |>
      ggplot(aes(Estatistica, Valor)) +
      geom_col(fill = "royalblue4") +
      geom_text(aes(label = comma(round(Valor, 0), big.mark = ".", decimal.mark = ",")),
                vjust = -0.3, size = if (input$pos_facet) 3.5 else 5) +
      scale_y_continuous(labels = fmt, expand = expansion(mult = c(0, 0.12))) +
      labs(title = paste0("Média, mediana e percentis, ", input$pos_ano,
                          " - R$ per capita de 2025 (IPCA)"),
           subtitle = escopo_pos(),
           x = NULL, y = "Valor") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.x = element_text(face = "bold"))

    if (input$pos_facet) p <- p + facet_wrap(~regiao)
    p
  })

  output$g_pos_serie <- renderPlot({
    p <- percentis() |>
      ggplot(aes(ano, Valor, color = Estatistica, group = Estatistica)) +
      geom_line(linewidth = 1.6) +
      geom_vline(xintercept = as.integer(input$pos_ano),
                 linetype = "dashed", color = "grey40") +
      scale_color_manual(values = cores_linhas) +
      scale_x_continuous(breaks = anos_serie) +
      scale_y_continuous(labels = fmt) +
      labs(title = "Série histórica das medidas de posição",
           subtitle = escopo_pos(),
           x = "Ano", y = "R$ per capita de 2025 (IPCA)", color = "Medida") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"),
            panel.grid.minor = element_blank())

    if (input$pos_facet) {
      p <- p + facet_wrap(~regiao) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    p
  })

  # 2. Dispersão -------------------------------------------------------
  output$g_scatter <- renderPlot({
    xvar <- input$sc_x
    xlab <- names(vars_x)[vars_x == xvar]

    d <- dados(input$sc_ano) |>
      filter(!is.na(.data[[xvar]]), !is.na(emenda_pc_real))
    if (input$sc_reg != "Todas") d <- d |> filter(regiao == input$sc_reg)
    validate(need(nrow(d) > 0, "Sem dados para essa seleção."))

    med_y <- median(d$emenda_pc_real, na.rm = TRUE)
    med_x <- median(d[[xvar]], na.rm = TRUE)

    p <- ggplot(d, aes(x = .data[[xvar]], y = emenda_pc_real, color = regiao))

    p <- p + if (input$sc_tam) {
      geom_point(aes(size = emenda_total), alpha = 0.25)
    } else {
      geom_point(alpha = 0.35, size = 1.5)
    }

    p <- p +
      geom_hline(yintercept = med_y, color = "brown4", linewidth = 0.8) +
      geom_vline(xintercept = med_x, color = "brown4", linewidth = 0.8) +
      annotate("text", x = Inf, y = med_y, hjust = 1.1, vjust = -0.3,
               color = "brown4", size = 4,
               label = paste0("Mediana = ", round(med_y, 1)))

    if (input$sc_metodo != "nenhuma") {
      p <- p + geom_smooth(aes(group = 1), method = input$sc_metodo,
                           se = (input$sc_metodo == "lm"),
                           color = "red", linewidth = 1, formula = y ~ x)
    }

    if (input$sc_corte) {
      p <- p + coord_cartesian(
        xlim = c(0, quantile(d[[xvar]], 0.99, na.rm = TRUE)),
        ylim = c(0, quantile(d$emenda_pc_real, 0.99, na.rm = TRUE)))
    }
    if (input$sc_facet) p <- p + facet_wrap(~regiao)

    p +
      scale_color_manual(values = cores_regiao, na.value = "grey60") +
      scale_x_continuous(labels = fmt) +
      scale_y_continuous(labels = fmt) +
      labs(title = paste0("Emendas per capita por município, ", input$sc_ano,
                          " - R$ de 2025 (IPCA)"),
           x = xlab, y = "Emenda per capita (R$)",
           color = "Região", size = "Emendas totais") +
      theme_minimal(base_size = 14)
  })

  # 3. Boxplot por UF ----------------------------------------------------
  output$g_box_uf <- renderPlot({
    d <- dados(input$uf_ano) |> filter(!is.na(emenda_pc_real), !is.na(uf))
    validate(need(nrow(d) > 0, "Sem dados para essa seleção."))

    p <- ggplot(d, aes(fct_reorder(uf, emenda_pc_real, median, na.rm = TRUE),
                       emenda_pc_real, fill = regiao)) +
      geom_boxplot(alpha = 0.6, outlier.shape = NA)

    if (input$uf_pontos) {
      p <- p + geom_jitter(aes(color = regiao), width = 0.2, size = 0.3, alpha = 0.5) +
        scale_color_manual(values = cores_regiao, guide = "none")
    }

    p +
      coord_cartesian(ylim = c(0, input$uf_ylim)) +
      scale_fill_manual(values = cores_regiao, name = "Região") +
      scale_y_continuous(labels = fmt) +
      labs(title = paste0("Emendas per capita por UF, ", input$uf_ano,
                          " - R$ de 2025 (IPCA)"),
           x = "UF", y = "Emenda per capita (R$)") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"))
  })

  # 4. Boxplot por faixa do FPM ----------------------------------------------
  output$g_box_fpm <- renderPlot({
    d <- dados(input$fpm_ano) |> filter(!is.na(emenda_pc_real), !is.na(faixa_fpm))
    if (input$fpm_uf != "Brasil") d <- d |> filter(uf == input$fpm_uf)
    por_regiao <- input$fpm_facet
    if (por_regiao) d <- d |> filter(!is.na(regiao))
    validate(need(nrow(d) > 0, "Sem dados para essa seleção."))

    if (por_regiao) {
      p <- ggplot(d, aes(faixa_fpm, emenda_pc_real, fill = regiao)) +
        geom_boxplot(alpha = 0.6, outlier.shape = NA) +
        scale_fill_manual(values = cores_regiao, guide = "none") +
        facet_wrap(~regiao)

      if (input$fpm_pontos) {
        p <- p + geom_jitter(aes(color = regiao), width = 0.2, size = 0.3, alpha = 0.5) +
          scale_color_manual(values = cores_regiao, guide = "none")
      }
    } else {
      p <- ggplot(d, aes(faixa_fpm, emenda_pc_real)) +
        geom_boxplot(fill = "royalblue4", alpha = 0.4, outlier.shape = NA)

      if (input$fpm_pontos) {
        p <- p + geom_jitter(color = "royalblue4", width = 0.2, size = 0.3, alpha = 0.4)
      }
    }

    p +
      coord_cartesian(ylim = c(0, input$fpm_ylim)) +
      scale_y_continuous(labels = fmt) +
      labs(title = paste0("Emendas per capita por faixa do FPM, ", input$fpm_ano,
                          " - R$ de 2025 (IPCA)"),
           subtitle = input$fpm_uf,
           x = "Faixa do FPM (mil habitantes)", y = "Emenda per capita (R$)") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # 5. Mapa -------------------------------------------------------------
  output$g_mapa <- renderPlot({
    d <- dados(input$mp_ano) |>
      select(codigo_ibge, uf, emenda_pc_real, emenda_total) |>
      mutate(pct = percent_rank(emenda_pc_real))

    uf_sel  <- input$mp_uf
    cores   <- c("#D73027", "#FEE08B", "#1A9850")
    p95     <- quantile(d$emenda_pc_real, 0.95, na.rm = TRUE)
    cor_var <- if (input$mp_tipo == "pct") "pct" else "emenda_pc_real"

    # escala de cor (a mesma para território pintado e para bolhas)
    escala_cor <- function(aes_nome) {
      if (input$mp_tipo == "pct") {
        scale_colour_gradientn(colours = cores, name = "Percentil",
                               limits = c(0, 1), na.value = "grey90",
                               labels = percent, aesthetics = aes_nome)
      } else {
        scale_colour_gradientn(colours = cores, limits = c(0, p95),
                               oob = scales::squish, na.value = "grey90",
                               name = "R$ per capita", labels = fmt,
                               aesthetics = aes_nome)
      }
    }

    mun <- malha |> left_join(d, by = "codigo_ibge")
    if (uf_sel != "Brasil") mun <- mun |> filter(uf == uf_sel)

    if (input$mp_estilo == "area") {
      p <- ggplot(mun) +
        geom_sf(aes(fill = .data[[cor_var]]), color = NA) +
        escala_cor("fill")
    } else {
      pts <- malha_pts |>
        left_join(d, by = "codigo_ibge") |>
        filter(!is.na(emenda_total), emenda_total > 0, !is.na(.data[[cor_var]]))
      if (uf_sel != "Brasil") pts <- pts |> filter(uf == uf_sel)
      validate(need(nrow(pts) > 0, "Sem dados para essa seleção."))
      # contraste: tamanho da bolha = emenda_total elevada a um expoente
      # (1 = área proporcional ao valor; >1 amplia a diferença entre pequenas e grandes)
      expo <- input$mp_contraste
      pts <- pts |>
        mutate(tam = emenda_total^expo) |>
        arrange(desc(emenda_total))  # bolhas grandes por baixo

      # legenda em valores originais (R$), mesmo com o expoente aplicado
      br_orig <- unique(arredonda_baixo(
        quantile(pts$emenda_total, c(0.25, 0.75, 0.95, 1), na.rm = TRUE)))

      p <- ggplot() +
        geom_sf(data = mun, fill = "grey93", color = "white", linewidth = 0.1) +
        geom_sf(data = pts,
                aes(color = .data[[cor_var]], size = tam),
                alpha = 0.75) +
        escala_cor("colour") +
        scale_size_area(max_size = input$mp_tam * (if (uf_sel == "Brasil") 1 else 1.75),
                        breaks = br_orig^expo, labels = fmt(br_orig),
                        name = "Emenda total\n(R$ correntes)")
    }

    p +
      labs(title = paste0("Emendas per capita por município, ", input$mp_ano,
                          if (uf_sel != "Brasil") paste0(" - ", uf_sel) else "")) +
      theme_void(base_size = 14) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
  })
}

shinyApp(ui, server)
