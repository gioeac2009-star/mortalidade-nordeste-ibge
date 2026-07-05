library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(stringr)
library(viridis)
library(readxl)

# ==========================================
# 1. CARREGAMENTO E TRATAMENTO DOS DADOS (EXCEL)
# ==========================================
excel_file <- "Tabua_teste.xlsx"

# Função auxiliar para limpar os dados numéricos (remove espaços e padroniza decimais)
limpar_numero <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\\s+", "") # Remove espaços ocultos
  x <- str_replace_all(x, ",", ".")   # Substitui vírgula por ponto
  as.numeric(x)
}

# Leitura da aba "Tábuas Abrev de Mortalidade"
df1 <- read_excel(excel_file, sheet = "Tábuas Abrev de Mortalidade") %>%
  select(
    Ano = Censo, 
    Estado = `Estado/Região`, 
    Idade = `Grupos Quinquenais de idade`, 
    Esperanca_Vida = ex
  ) %>%
  mutate(
    Ano = as.numeric(Ano),
    Esperanca_Vida = limpar_numero(Esperanca_Vida)
  )

# Leitura da aba "Tábua Abreviada de Mortalidade"
df2 <- read_excel(excel_file, sheet = "Tábua Abreviada de Mortalidade") %>%
  select(
    Ano = Ano, 
    Estado = Local, 
    Idade = `Grupos quinquenais de idade`, 
    Esperanca_Vida = `E(x)`
  ) %>%
  mutate(
    Ano = as.numeric(Ano),
    Esperanca_Vida = limpar_numero(Esperanca_Vida)
  )

# Unificação das bases e filtragem para a expectativa de vida ao nascer e(0)
# Filtramos "Nordeste" para isolar a análise estritamente nos estados da série
df_final <- bind_rows(df1, df2) %>%
  filter(Idade == "Menos de 1 ano") %>%
  filter(!Estado %in% c("Nordeste", "Brasil")) %>%
  filter(!is.na(Esperanca_Vida)) %>%
  arrange(Ano, Estado)

# ==========================================
# 2. INTERFACE DO USUÁRIO (UI)
# ==========================================
ui <- page_navbar(
  title = "Análise Demográfica - Mortalidade IBGE",
  theme = bs_theme(
    version = 5, 
    preset = "yeti" # Tema minimalista e altamente legível
  ),
  
  nav_panel(
    title = "Evolução do e(0)",
    
    # Texto explicativo sobre o trabalho
    markdown("
    ### Estudo da Evolução da Expectativa de Vida ao Nascer no Nordeste
    
    Este projeto consiste em um ambiente interativo para a exploração de tábuas de mortalidade construídas pelo IBGE, concentrando-se nas séries temporais históricas e projeções dos estados da Região Nordeste. 
    
    A análise foca no indicador **e(0)** (esperança de vida ao nascer), extraído do grupo de idade de *Menos de 1 ano*. O acompanhamento dessa métrica desempenha um papel fundamental no entendimento do desenvolvimento socioeconômico regional, servindo de insumo crítico para o cálculo de riscos biométricos, modelagem atuarial de tábuas de sobrevivência e estruturação de fundos previdenciários e de seguros de pessoas.
    "),
    
    hr(),
    
    layout_columns(
      col_widths = c(12, 12),
      
      # Bloco do Gráfico de Linhas
      card(
        full_screen = TRUE,
        card_header("Série Temporal da Expectativa de Vida por Estado"),
        plotOutput("grafico_linhas", height = "420px")
      ),
      
      # Bloco do Mapa de Calor
      card(
        full_screen = TRUE,
        card_header("Mapa de Calor: Intensidade e Ganhos de Longevidade"),
        plotOutput("grafico_calor", height = "420px")
      )
    )
  )
)

# ==========================================
# 3. LOGICA DO SERVIDOR (SERVER)
# ==========================================
server <- function(input, output, session) {
  
  # Gráfico 1: Séries Temporais (Linhas)
  output$grafico_linhas <- renderPlot({
    ggplot(df_final, aes(x = Ano, y = Esperanca_Vida, color = Estado, group = Estado)) +
      geom_line(linewidth = 1.2, alpha = 0.85) +
      geom_point(size = 2.5) +
      scale_color_viridis_d(option = "turbo") +
      scale_x_continuous(breaks = unique(df_final$Ano)) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "right",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      labs(
        x = "Ano",
        y = "Expectativa de Vida ao Nascer (Anos)",
        color = "Estado"
      )
  })
  
  # Gráfico 2: Mapa de Calor (Heatmap)
  output$grafico_calor <- renderPlot({
    ggplot(df_final, aes(x = factor(Ano), y = reorder(Estado, Esperanca_Vida), fill = Esperanca_Vida)) +
      geom_tile(color = "white", linewidth = 0.6) +
      scale_fill_viridis_c(option = "mako", direction = -1) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      labs(
        x = "Ano de Referência",
        y = "Estado",
        fill = "e(0) em Anos"
      )
  })
}

# Execução do aplicativo
shinyApp(ui, server, options = list(launch.browser = TRUE))