library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggtech)
library(tidyverse)
library(lubridate)
library(rjson)
library(jsonlite)
library(leaflet)
library(RCurl)


# Carregando as base de dados -------------------------------------------------------

Meta1  <- readr::read_rds("../data/Meta1.rds") |>
  tidyr::drop_na() |>
  mutate(
    indice1b = round(indice1b,3),
    indice1a = round(indice1a, 3),
    meta1    = round(meta1,3)
  )


# Construindo a ui ------------------------------------------------------------------
ui <- dashboardPage(

  dashboardHeader(title = HTML("PNE-Municípios"),
                  disable = FALSE,
                  titleWidth  = 550),

# Construindo o menu---------
  dashboardSidebar(
    sidebarMenu(
      menuItem("Meta 1",
               tabName = "meta1",
               icon = icon("fas fa-chevron-right")))),

# Página 1 --------------------------------------------------------------------------
dashboardBody(
  tabItems(
    tabItem(
      tabName = "meta1",
      fluidRow(
        box(
          width = 12,
          h1("Monitoramento do Plano Nacional de Educação - BI Norte")),
hr(style = "border-top: 1px solid black;"), # tag horizontal row style para mudar o css
        ),
    )
),

# Apresentação --------------------------------------------------------------------------
     tabItem(
        tabName = "meta1",
        fluidRow(
          box(
            width = 12,
h1("Meta 1: Universalizar, até 2016, a educação infantil na pré-escola para as crianças
de 4 (quatro) a 5 (cinco) anos de idade e ampliar a oferta de educação infantil em
creches, de forma a atender, no mínimo, 50% (cinquenta por cento) das crianças de
até 3 (três) anos até o final da vigência deste PNE",
style = "font-size: 25px; color:Black")),
hr(style = "border-top: 1px solid black;"), # tag horizontal row style para mudar o css
),

# Caixa de seleção ----------------------------------------------------------------
fluidRow(
  box(
    width = 8,
    fluidRow(
      column(
        width = 6,
        selectInput(
          inputId = "uf",
          label = "Selecione um estado",
          choices = Meta1 |>
            filter(nome_regiao == "Norte") |>
            pull(nome_uf) |> unique() |>
            sort(),
          selected = "Amazonas"
        )
      ),
      column(
        width = 6,
        selectInput(
          inputId ="municipio",
          label = "Selecione um município",
          choices = "Carregando"
        )
      ),
    )
  )),


# Info Box ------------------------------------------------------------------
fluidRow(

  infoBoxOutput("meadiaReg1b"),
  infoBoxOutput("meadiaReg1a"),
  infoBoxOutput("meadiaMeta1"),



),

# Gráficos --------------------------------------------------------------------------
fluidRow(
  box(
    width = 12,
    fluidRow(

      # Gráfico 1: Indicador 1B

       box(
        width = 4,
        title = "Indicador 1B: Matrícula em Creche (0 a 3 anos)",
        solidHeader = TRUE, #cabeçalho colorido
        status = "primary",  # para mudar a cor igual do anterior
        shinycssloaders::withSpinner(
          plotOutput("graf_indicador1b"))),

      # Gráfico 2: Indicador 1A

      box(
        width = 4,
        title = "Indicador 1A: Matrícula em Pré-Escola (4 e 5 anos)",
        solidHeader = TRUE, #cabeçalho colorido
        status = "primary",  # para mudar a cor igual do anterior
        shinycssloaders::withSpinner(
          plotOutput("graf_indicador1a"))),

      # Gráfico 3: Meta 1

      box(
        width = 4,
        title = "Meta: Matrícula de 0 e 5 anos",
        solidHeader = TRUE, #cabeçalho colorido
        status = "primary",  # para mudar a cor igual do anterior
        shinycssloaders::withSpinner(
          plotOutput("graf_meta1")))
      ))),
)))


# Constuindo o server ---------------------------------------------------------------
server <- function(input, output, session) {

#  Meta1  <- readr::read_rds("data/Meta1.rds")

  # Necessário

  observe({
    muni <- Meta1 |>
      filter(nome_uf == input$uf) |>
      pull("nome_municipio") |>
      unique() |> sort()

    updateSelectInput(
      session,
      "municipio",
      choices = muni
    )
  })



  # Graficos ----------------------------------------------------------------

  # Gráfico 1B

  output$graf_indicador1b <- renderPlot({


# info Box --------------------------------------------------------------------------

  output$meadiaReg1b <- renderInfoBox({
    infoBox(
      title = "Média Estadual do Indicador 1B",
      value = Meta1 |> filter(nome_uf == isolate(input$uf)) |>
        summarise(
          media = round(mean(indice1b, na.rm = T),3),
        ),
      icon = icon("baby"),
      color = "teal",
      width = 4) # não pode ser qq cor(ver no Help)

  })

  output$meadiaReg1a <- renderInfoBox({
    infoBox(
      title = "Média Estadual do Indicador 1A",
      value = Meta1 |> filter(nome_uf == isolate(input$uf)) |>
        summarise(
          media = round(mean(indice1a, na.rm = T),3),
        ),
      icon = icon("child"),
      color = "light-blue",
      width = 4)

  })

  output$meadiaMeta1 <- renderInfoBox({
    infoBox(
      title = "Média Estadual da Meta 1",
      value = Meta1 |> filter(nome_uf == isolate(input$uf)) |>
        summarise(
          media = round(mean(meta1, na.rm = T),3),
        ),
      icon = icon("users"),
      color = "navy",
      width = 4)

  })



# Gráfico ---------------------------------------------------------------------------
  Meta1 |>
      filter(nome_uf == input$uf,
             nome_municipio == input$municipio) |>
      ggplot() +
      geom_col(aes(x = ano, y = indice1b, fill = indice1b)) +
      coord_cartesian(ylim = c(0, 1)) +
      geom_abline(intercept = 0.5, slope = 0, color = "red") +
      geom_label(aes(x = ano, y = indice1b, label = indice1b),
                 label.size = 0.25) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )



  }  )

  # Gráfico 1A

  output$graf_indicador1a <- renderPlot({

    Meta1 |> filter(nome_uf == isolate(input$uf),
                    nome_municipio == input$municipio) |>
      ggplot() +
      geom_col(aes(x = ano, y = indice1a, fill = indice1a)) +
      coord_cartesian(ylim = c(0, 1)) +
      geom_label(aes(x = ano, y = indice1a, label = indice1a),
                 label.size = 0.25)+
      geom_abline(intercept = 1, slope = 0, color = "red") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )
  }  )

  # Meta 1

  output$graf_meta1 <- renderPlot({

    Meta1 |> filter(nome_uf == isolate(input$uf),
                    nome_municipio == input$municipio) |>
      ggplot() +
      geom_col(aes(x = ano, y = meta1, fill = meta1)) +
      geom_label(aes(x = ano, y = meta1, label = meta1),
                 label.size = 0.25)+
      coord_cartesian(ylim = c(0, 1)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )
  }  )
}

shinyApp(ui, server)
