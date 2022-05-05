
# Importando a base de dados

Meta1 <- readr::read_rds("data/Meta1.rds") |>
  tidyr::drop_na(meta1) |>
  dplyr::mutate(
    ano = as.double(ano)
  )

dplyr::glimpse(Meta1)

# Principais variáveis:

## Região, Estado, Mesorregião, Microrregião, Ano, Nome do Município,
## Quantidade de Matrícula, População por Faixa Etária, Índice 1B, ìndice 1A

# Análise Descritiva

Meta1 |> summary()

Meta1 |> dplyr::group_by(ano, nome_municipio) |>
  dplyr::mutate(
    media = mean(indice1b)
  ) |>
  ggplot2::ggplot() +
  geom_col(aes(x = ano, y = meta1, fill = meta1))

Itacoataira <- Meta1 |> dplyr::filter(nome_microrregiao  == "Itacoatiara")

Amazonas  <-  Meta1 |> dplyr::filter(nome_uf  == "Amazonas")


