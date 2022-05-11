## Empilhando as bases do Censo Escolar -----------------------------------------------------

## Neste scrip, importa-se as bases do Censo Escolar que serão úteis em
## pesquisas futuras

## As varáveis dos anos 2013 a 2014 possuem nomes diferentes, por isso a decisão
## importar separadamente.

## Nesta etapa, eu seleciono as variáveis equivantes nas duas base de dados
variaveis_selecionadas_1314 <- c("ANO_CENSO",
                                 "NUM_IDADE_REFERENCIA",
                                 "ID_DEPENDENCIA_ADM_ESC",
                                 "FK_COD_ETAPA_ENSINO",
                                 "FK_COD_ESTADO_ESCOLA",
                                 "COD_MUNICIPIO_ESCOLA")


variaveis_selecionadas_1520 <- c("NU_ANO_CENSO",
                                 "NU_IDADE_REFERENCIA",
                                 "TP_DEPENDENCIA",
                                 "TP_ETAPA_ENSINO",
                                 "CO_UF",
                                 "CO_MUNICIPIO")

## Nesta etapa, eu aponto os caminhos das bases de dados
arq_matriculas_no_1314 <-list.files("data-raw/2013_2014/",
                                    full.names = T)

arq_matriculas_no_1520 <-list.files("data-raw/2015_2020/",
                                    full.names = T)

## Aqui, eu importo as Matrículas dos alunos do Norte do Brasil
## Eu opto pelo pacote função "fread" do pacote data.table, pela possibilidade
## de importar apenas as variáveies de interesse.

matriculas_no_1314 <- purrr::map_dfr(arq_matriculas_no_1314,
                                     data.table::fread,
                                     select = (variaveis_selecionadas_1314))


matriculas_no_1520 <- purrr::map_dfr(arq_matriculas_no_1520,
                                     data.table::fread,
                                     select = (variaveis_selecionadas_1520))

## Como as bases tem nomes diferentes nas tabelas, eu preferi usar a função colnames
## ao invés do mutate (que ainda não tenho muita experiência)
colnames(matriculas_no_1314) <- names(matriculas_no_1520)

matriculas_no_1320 <- dplyr::bind_rows(matriculas_no_1314,
                                       matriculas_no_1520)

readr::write_rds(matriculas_no_1320, file = "data/matricula1320.rds")

## Para não ter que repassar toda a base de dados, aqui gero uma amostra que será
## utilizada em minha análise
#amostra_dados <- slice_sample(matriculas_no_1320, n=10000)

# write_rds(amostra_dados, file = "data/amostra_dados.rds")

# Calculando as Metas -----------------------------------------------------

# Importando a amostra da matricula

matriculas_no_1320 <- readr::read_rds("data/amostra_dados.rds")

#  Populacao de 0 a 3 anos

qtde0a3 <- estimativas_por_idade |>
  dplyr::filter(Faixa_Etaria=="0 a 3 anos") |>
  #    dplyr::group_by(Faixa_Etaria) |>
    dplyr::summarise(
    Populacao = sum(`2015`)) |>
    as.numeric()

# Matricula de 0 a 3 anos

matricula0a3 <- matriculas_no_1320 |>
  dplyr::filter(NU_ANO_CENSO == ano2) |>
  dplyr::filter(TP_ETAPA_ENSINO == 1) |>
  dplyr::count(TP_ETAPA_ENSINO)

matricula0a3 <- matricula0a3$n |> sum()

# Calculando o indicador 1B

indicador_1B = round(matricula0a3/qtde0a3,4)*100


mensagem1 <- paste0("O indicador 1B, da Meta 1 do Plano Nacional de Educação do",
                    " município ", cidade2, ", que mede o percetual",
                    " de crianças de 0 a 3 anos matriculados",
                    " em creches é de ", indicador_1B, "%.")

print(mensagem1)

# Meta 1: Indicador 1A

#  Populacao de 4 a 5 anos

qtde4a5 <- estimativas_por_idade |>
  dplyr::filter(Faixa_Etaria=="4 a 5 anos") |>
  #    dplyr::group_by(Faixa_Etaria) |>
  dplyr::summarise(
    Populacao = sum(`2015`)
  ) |> as.numeric()

# Matricula de 4 a 5 anos

matricula4a5 <- matriculas_no_1320 |>
  dplyr::filter(NU_ANO_CENSO == ano2) |>
  dplyr::filter(TP_ETAPA_ENSINO == 2) |>
  dplyr::count(TP_ETAPA_ENSINO)

matricula4a5 <- matricula4a5$n |> sum()


indicador_1A <- matricula4a5/qtde4a5*100

mensagem2 <- paste0("O indicador 1A, da Meta 1 do Plano Nacional de Educação do",
                    " município ", cidade2, " que mede o percetual",
                    " de crianças de 4 a 5 anos matriculados",
                    " em creches é de ", round(indicador_1A, 2), "%.")

print(mensagem2)


Meta_1 <- (matricula0a3 + matricula4a5)/(qtde0a3  + qtde4a5)

mensagem3 <- paste0("O município de ", cidade2, " alcançou ",
                    round(Meta_1, 2), "% referente à Meta 1 do Plano Nacional de Educação",
                    " no ano de ", ano2)

print(mensagem3)
