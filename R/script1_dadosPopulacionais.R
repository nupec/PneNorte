## code to prepare `dadosPopulacionais` dataset goes here

## usethis::use_data(dadosPopulacionais, overwrite = TRUE)

# 1) Importação dos índices da população brasileira por idade-------------------
## Base: Censo 2010
indices_tidy <- readr::read_rds("data/indicesCidades.rds")

# 2) Importando a estimativa da população das cidades brasileiras --------
## 2.1) Importando as estimativas da população
populacao_municipios <- readxl::read_excel("data-raw/popESTIMADA.xlsx")

## 2.2) Transformando a base importada no formato tidy
populacao_municipios_tidy <- populacao_municipios |>
  tidyr::pivot_longer(
    cols = starts_with("20"),
    names_to = "ano",
    values_to = "populacao") |>
  dplyr::mutate(
    codigo_municipio = as.numeric(codigo_municipio)
  )

## 2.4) EStimando a população desagregadas por idade
pop_est_idade <- dplyr::left_join(populacao_municipios_tidy,
                                  indices_tidy,
                                  by="codigo_municipio")|>
  dplyr::arrange(ano, idade) |>
  dplyr::rename(nome_municipio = nome_municipio.x) |>
  dplyr::mutate(populacao_estimada = ceiling(populacao*prop)) |>
  dplyr::select(codigo_municipio, nome_municipio, ano, nome_uf,
                idade, populacao_estimada)

## 3) Salvando em arquivo rds
readr::write_rds(pop_est_idade,"data/populacaoEstimadaPorIdade.rds")
