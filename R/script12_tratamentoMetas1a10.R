
# Se faz necessário uma correção nas bases para eliminar erros pontuais

# Carregando pacotes necessários para a limpeza
library(dplyr)
library(tidyr)

## TRATAMENTO BASE MUNICIPIOS

baseMunicipios = write.csv(codMunicipios, file = 'data/Metas_Tratadas/baseMunicipios.csv', row.names = FALSE)

## META 03 --------------------------------------------------------------------

# Eliminando colunas desnecessárias
Meta3_trat = Meta3 |>
  dplyr::select(-total_mat_EM_15a17,-total_mat_escola_15a17,-popFaixa15a17)

# Eliminando linhas vazias
Meta3_trat = drop_na(Meta3_trat)

# Ajustando o intervalo dos indicadores (0,1)
Meta3_trat = Meta3_trat |> dplyr::mutate(indicador3A = ifelse(indicador3A > 1, 1, indicador3A)) |>
  dplyr::mutate(indicador3B = ifelse(indicador3B > 1, 1, indicador3B))

# Salvando a base da Meta 3 em .csv
write.csv(Meta3_trat, file = 'data/Metas_Tratadas/Meta3_trat.csv', row.names = FALSE)

## META 04 --------------------------------------------------------------------

# Fiz o ajuste no indicador 4A no próprio script

## META 07 --------------------------------------------------------------------

### INDICADOR 7A

# Eliminando colunas desnecessárias
Meta7_Indicador7A_trat = Meta7_Indicador7A |>
  dplyr::select(-projecao_indicador7A, -nota_saeb_media_padronizada,
                -indicador_rendimento,-taxa_aprovacao,-nota_saeb_matematica,
                -nota_saeb_lingua_portuguesa)

# Trocando os NA's por 0, ou seja, o inep não obteve informações sobre o ideb
# daquele município, naquele ano.
Meta7_Indicador7A_trat = replace(Meta7_Indicador7A_trat, is.na(Meta7_Indicador7A_trat), 0)

# Salvando a base do Indicador 7A em .csv
write.csv(Meta7_Indicador7A_trat, file = 'data/Metas_Tratadas/Meta7_Indicador7A_trat.csv', row.names = FALSE)

### INDICADOR 7B

# Eliminando colunas desnecessárias
Meta7_Indicador7B_trat = Meta7_Indicador7B |>
  dplyr::select(-projecao_indicador7B, -nota_saeb_media_padronizada,
                -indicador_rendimento,-taxa_aprovacao,-nota_saeb_matematica,
                -nota_saeb_lingua_portuguesa)

# Trocando os NA's por 0, ou seja, o inep não obteve informações sobre o ideb
# daquele município, naquele ano.

Meta7_Indicador7B_trat = replace(Meta7_Indicador7B_trat, is.na(Meta7_Indicador7B_trat), 0)

# Salvando a base do Indicador 7B em .csv
write.csv(Meta7_Indicador7B_trat, file = 'data/Metas_Tratadas/Meta7_Indicador7B_trat.csv', row.names = FALSE)

### INDICADOR 7C

# Eliminando colunas desnecessárias
Meta7_Indicador7C_trat = Meta7_Indicador7C |>
  dplyr::select(-projecao_indicador7C, -nota_saeb_media_padronizada,
                -indicador_rendimento,-taxa_aprovacao,-nota_saeb_matematica,
                -nota_saeb_lingua_portuguesa)

# Trocando os NA's por 0, ou seja, o inep não obteve informações sobre o ideb
# daquele município, naquele ano.

Meta7_Indicador7C_trat = replace(Meta7_Indicador7C_trat, is.na(Meta7_Indicador7C_trat), 0)

# Salvando a base do Indicador 7C em .csv
write.csv(Meta7_Indicador7C_trat, file = 'data/Metas_Tratadas/Meta7_Indicador7C_trat.csv', row.names = FALSE)
