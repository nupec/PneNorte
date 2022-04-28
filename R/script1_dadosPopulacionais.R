## code to prepare `dadosPopulacionais` dataset goes here

## usethis::use_data(dadosPopulacionais, overwrite = TRUE)

# 1) Importação dos índices da população brasileira por idade-------------------
## Base: Censo 2010
indices_tidy <- readr::read_rds("data/indicesCidades.rds")

# 2) Importando a estimativa da população das cidades brasileiras --------
## 2.1) Importando as estimativas da população (2001 a 2020)
populacao_municipios <- readRDS("data/popMunBr1991a2021.rds") |>
  dplyr::rename(
    codigo_municipio = id_municipio
  ) |>
  dplyr::mutate(
    codigo_municipio = as.numeric(codigo_municipio)
  )

## 2.4) EStimando a população desagregadas por idade
pop_est_idade <- dplyr::left_join(populacao_municipios,
                                  indices_tidy,
                                  by="codigo_municipio")|>
  dplyr::arrange(ano, idade) |>
  dplyr::mutate(populacao_estimada = ceiling(populacao*prop)) |>
  dplyr::select(codigo_municipio, nome_municipio, nome_uf,
                ano, idade, populacao_estimada)

## 3) Salvando em arquivo rds
readr::write_rds(pop_est_idade,"data/populacaoEstimadaPorIdade.rds")
