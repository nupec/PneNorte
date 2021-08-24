library(readxl)

# Importando a base de dados ----------------------------------------------

## Nesta etapa, importa-se a Tabela 1552, gerada pelo Sistema SIDRA/IBGE e que
## contém a contagem da popuçação desagregada por idade (CENSO - 2010). Link:

url("https://sidra.ibge.gov.br/tabela/1552?fbclid=IwAR3PonnEhgVF1jdvbkyUnGCFvg2dnogSyRLtnGqY6M65jJo7T0eou3DVsQ0#/n1/all/n3/all/n6/all/v/allxp/p/all/c1/0/c2/0/c286/0/c287/all/l/v,p+c1+c2,t+c286+c287/resultado")

## A seguir, importa-se a tabela 1552 e corrige-se o tipo de variável
## "codigo_municipio" para o formato numérico.
df <- readxl::read_excel("data-raw/tabela1552_tidy.xlsx", sheet = 2) |>
  dplyr::mutate(
    codigo_municipio = as.numeric(codigo_municipio)
  )

## O código a seguir faz um cópia da coluna 1 do data frame que contém o código
## do muncípio, retira os NA´s, transforma o reesultado em um vetor e
## e depois replica o código de cada cidade 101 vezes. Ao final transforma os
## dados no formato de tibble,
col1 <- df[1] |> tidyr::drop_na() |> dplyr::pull() |>
  rep(101) |>
  dplyr::as_tibble()

## O mesmo processo é feito com as colunas 2 e 3
col2 <- df[2] |> tidyr::drop_na() |> dplyr::pull() |>
  rep(101) |>
  dplyr::as_tibble()

col3 <- df[3] |> tidyr::drop_na() |> dplyr::pull() |>
  rep(101) |>
  dplyr::as_tibble()

## Nesta etapa, as colunas 1, 2 e 3 são juntadas em um novo data frame (df2)
df2 <- dplyr::bind_cols(col1, col2, col3) |>
  dplyr::rename(codigo_municipio = value...1,
                nome_municipio = value...2,
                nome_uf = value...3)

## Ordeno os dados pela variável "codigo_municipio" e atualizo o df2
df2 <- df2 |> dplyr::arrange(df2, "codigo_municipio")

## Nesta etapa, junto os dados do df2 com df
df_final <- dplyr::bind_cols(df2, df[4:5])

## Exporto o data frame final
readr::write_rds(df_final, "data/indicesCidades.rds")


# Atualizado em 01/08/2021
# Nova atuzalização em 24/08/2021
