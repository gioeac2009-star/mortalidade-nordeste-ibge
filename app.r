library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(stringr)
library(viridis)
library(readxl)
library(plotly)
library(DT)

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
    População = População,
    Óbitos = Óbitos,
    nMx = nMx,
    nQx = nQx,
    `l(x)` = `l(x)`,
    nDx = nDx,
    nLx = nLx,
    Tx = Tx,
    Esperanca_Vida = ex
  ) %>%
  mutate(
    Ano = as.numeric(Ano),
    across(c(População, Óbitos, nMx, nQx, `l(x)`, nDx, nLx, Tx, Esperanca_Vida), as.character)
  )

# Leitura da aba "Tábua Abreviada de Mortalidade"
df2 <- read_excel(excel_file, sheet = "Tábua Abreviada de Mortalidade") %>%
  select(
    Ano = Ano, 
    Estado = Local, 
    Idade = `Grupos quinquenais de idade`, 
    População = População,
    Óbitos = Óbitos,
    nMx = `M(x,n)`,
    nQx = `Q(x,n)`,
    `l(x)` = `l(x)`,
    nDx = `D(x,n)`,
    nLx = `L(x,n)`,
    Tx = `T(x)`,
    Esperanca_Vida = `E(x)`
  ) %>%
  mutate(
    Ano = as.numeric(Ano),
    across(c(População, Óbitos, nMx, nQx, `l(x)`, nDx, nLx, Tx, Esperanca_Vida), as.character)
  )

# Unificação das bases
df_final <- bind_rows(df1, df2) %>%
  filter(!Estado %in% c("Nordeste", "Brasil")) %>%
  mutate(across(c(População, Óbitos, nMx, nQx, `l(x)`, nDx, nLx, Tx, Esperanca_Vida), limpar_numero)) %>%
  arrange(Ano, Estado, Idade)

# Corrigir Idade column factor levels para ordem correta
ordem_idades <- c("Menos de 1 ano", "1 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos", 
                  "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", 
                  "45 a 49 anos", "50 a 54 anos", "55 a 59 anos", "60 a 64 anos", "65 a 69 anos", 
                  "70 a 74 anos", "75 a 79 anos", "80 anos e mais")

df_final <- df_final %>%
  mutate(Idade = factor(Idade, levels = ordem_idades))

# ==========================================
# 2. INTERFACE DO USUÁRIO (UI)
# ==========================================
ui <- page_sidebar(
  title = "Análise Demográfica - Mortalidade IBGE",
  theme = bs_theme(
    version = 5, 
    preset = "yeti" 
  ),
  
  sidebar = sidebar(
    title = "Filtros Principais",
    selectInput("idade_input", "Faixa Etária (para Dashboards 1 e 2):", 
                choices = levels(df_final$Idade),
                selected = "Menos de 1 ano"),
    selectInput("ano_input", "Ano Base (para Dashboard 3 e Tabelas):", 
                choices = sort(unique(df_final$Ano), decreasing = TRUE),
                selected = max(df_final$Ano, na.rm = TRUE)),
    hr(),
    markdown("
    **Informações:**
    Estudo interativo das tábuas de mortalidade da Região Nordeste.
    
    A variável **e(x)** indica o número médio de anos que um indivíduo daquela faixa etária ainda tem a viver.
    ")
  ),
  
  navset_card_underline(
    title = "Painéis Atuariais",
    
    # Aba 1: Dashboard Histórico
    nav_panel("1. Dashboard Histórico e(x)", 
              layout_columns(
                value_box(
                  title = "Máxima Esperança de Vida no Ano Selecionado",
                  value = textOutput("vb_max_ex"),
                  showcase = bsicons::bs_icon("arrow-up-circle"),
                  theme = "success"
                ),
                value_box(
                  title = "Mínima Esperança de Vida no Ano Selecionado",
                  value = textOutput("vb_min_ex"),
                  showcase = bsicons::bs_icon("arrow-down-circle"),
                  theme = "danger"
                )
              ),
              hr(),
              plotlyOutput("grafico_linhas", height = "400px"),
              br(),
              plotlyOutput("grafico_calor", height = "400px")
    ),
    
    # Aba 2: Curva de Sobrevivência/Longevidade
    nav_panel("2. Curva de Longevidade por Idade",
              p("A curva abaixo mostra o declínio da esperança de vida ao longo das faixas etárias para o ano selecionado. Comparativo entre todos os estados."),
              plotlyOutput("grafico_curva_idade", height = "500px")
    ),
    
    # Aba 3: Tabela Atuarial
    nav_panel("3. Tabela Atuarial",
              p("Explore os dados completos da tábua de mortalidade para o ano selecionado. (Utilize a barra lateral para alterar o ano)"),
              DTOutput("tabela_atuarial")
    )
  )
)

# ==========================================
# 3. LÓGICA DO SERVIDOR (SERVER)
# ==========================================
server <- function(input, output, session) {
  
  # Dados filtrados reativos
  dados_filtrados_idade <- reactive({
    req(input$idade_input)
    df_final %>% filter(Idade == input$idade_input, !is.na(Esperanca_Vida))
  })
  
  dados_filtrados_ano <- reactive({
    req(input$ano_input)
    df_final %>% filter(Ano == as.numeric(input$ano_input), !is.na(Esperanca_Vida))
  })
  
  # Value Boxes
  output$vb_max_ex <- renderText({
    df_ano_idade <- dados_filtrados_idade() %>% filter(Ano == as.numeric(input$ano_input))
    if (nrow(df_ano_idade) == 0) return("Sem dados")
    max_estado <- df_ano_idade %>% arrange(desc(Esperanca_Vida)) %>% slice(1)
    paste0(round(max_estado$Esperanca_Vida, 1), " anos (", max_estado$Estado, ")")
  })
  
  output$vb_min_ex <- renderText({
    df_ano_idade <- dados_filtrados_idade() %>% filter(Ano == as.numeric(input$ano_input))
    if (nrow(df_ano_idade) == 0) return("Sem dados")
    min_estado <- df_ano_idade %>% arrange(Esperanca_Vida) %>% slice(1)
    paste0(round(min_estado$Esperanca_Vida, 1), " anos (", min_estado$Estado, ")")
  })
  
  # Gráfico 1: Séries Temporais (Linhas)
  output$grafico_linhas <- renderPlotly({
    p <- ggplot(dados_filtrados_idade(), aes(x = Ano, y = Esperanca_Vida, color = Estado, group = Estado, 
                                             text = paste("Estado:", Estado, "<br>Ano:", Ano, "<br>e(x):", Esperanca_Vida))) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_color_viridis_d(option = "turbo") +
      scale_x_continuous(breaks = unique(df_final$Ano)) +
      theme_minimal() +
      labs(
        title = paste("Evolução Histórica da Expectativa de Vida - Faixa:", input$idade_input),
        x = "Ano", y = "Expectativa de Vida (e(x))"
      )
    
    ggplotly(p, tooltip = "text") %>% layout(margin = list(b = 50))
  })
  
  # Gráfico 2: Mapa de Calor (Heatmap)
  output$grafico_calor <- renderPlotly({
    p <- ggplot(dados_filtrados_idade(), aes(x = factor(Ano), y = reorder(Estado, Esperanca_Vida), fill = Esperanca_Vida, 
                                             text = paste("Estado:", Estado, "<br>Ano:", Ano, "<br>e(x):", Esperanca_Vida))) +
      geom_tile(color = "white") +
      scale_fill_viridis_c(option = "mako", direction = -1) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = paste("Ganhos de Longevidade - Faixa:", input$idade_input),
        x = "Ano de Referência", y = "Estado", fill = "e(x)"
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # Gráfico 3: Curva de Idade
  output$grafico_curva_idade <- renderPlotly({
    p <- ggplot(dados_filtrados_ano(), aes(x = Idade, y = Esperanca_Vida, color = Estado, group = Estado, 
                                           text = paste("Estado:", Estado, "<br>Faixa Etária:", Idade, "<br>e(x):", Esperanca_Vida))) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_color_viridis_d(option = "turbo") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = paste("Curva de Longevidade por Idade no Ano de", input$ano_input),
        x = "Faixa Etária", y = "Expectativa de Vida (e(x))"
      )
    
    ggplotly(p, tooltip = "text") %>% layout(margin = list(b = 80))
  })
  
  # Tabela Atuarial
  output$tabela_atuarial <- renderDT({
    datatable(dados_filtrados_ano(), 
              options = list(pageLength = 18, scrollX = TRUE),
              rownames = FALSE,
              colnames = c("Ano", "Estado", "Faixa Etária", "População", "Óbitos", "nMx", "nQx", "l(x)", "nDx", "nLx", "Tx", "e(x)")) %>%
      formatRound(columns = c("População", "Óbitos", "l(x)", "nDx", "nLx", "Tx"), digits = 0) %>%
      formatRound(columns = c("nMx", "nQx", "Esperanca_Vida"), digits = 4)
  })
}

# Execução do aplicativo
shinyApp(ui, server, options = list(launch.browser = TRUE))