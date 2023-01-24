meta1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tagList(
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

  )
}
