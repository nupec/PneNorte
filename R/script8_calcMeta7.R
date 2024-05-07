# Meta 7: Fomentar a qualidade da educação básica em todas as etapas e
# modalidades, com melhoria do fluxo escolar e da aprendizagem, de modo a
# atingir as seguintes médias nacionais para o Ideb:
#
# Nível de Ensino                      | 2015 | 2017 | 2019 | 2021
# Anos iniciais do ensino fundamental  | 5.2  | 5.5  | 5.7  | 6.0
# Anos finais do ensino fundamental    | 4.7  | 5.0  | 5.2  | 5.5
# Ensino Médio                         | 4.3  | 4.7  | 5.0  | 5.2

# Indicador 7A: Ideb dos anos iniciais do ensino fundamental

# Indicador 7B: Ideb dos anos finais do ensino fundamental

# Indicador 7C: Ideb do ensino médio

## Carregando pacotes necessários para o cálculo dos indicadores
library(basedosdados)
library(tidyverse)

## Carregando as base de dados necessárias para o cálculo dos indicadores

# Carregando a base de dados dos municipios
codMunicipios <- readxl::read_excel("data-raw/CODIGO_MUNICIPIO.xls")

# Tratando os nomes das variáveis da base: Código dos Municípios
codMunicipios <- codMunicipios |> janitor::clean_names() |>
  dplyr::rename(co_uf = uf,
                codigo_municipio = codigo_municipio_completo) |>
  dplyr::select(nome_regiao,nome_uf,codigo_municipio,nome_municipio) |>
  dplyr::mutate(
    codigo_municipio = as.numeric(codigo_municipio))

## Carregando e salvando como .rds a base do IDEB ------------------------------
## Vamos utilizar a base de dados tradada pela organização Base dos Dados, pois
## a metodologia de tratamento é compatível com a análise feita desde o princípio

# install.packages("basedosdados")
# library("basedosdados")

# Defina o seu projeto no Google Cloud
# set_billing_id("indicadores-educacionais-ods4")

# Para carregar o dado direto no R
# query <- bdplyr("br_inep_ideb.municipio")
# df <- bd_collect(query)

# Salvando a base do IDEB com níveis municipais
# readr::write_rds(df, "data-raw/BaseIDEB.rds")

# Esse procedimento foi usado apenas uma vez, faça-o caso não tenha salvo
# a base de dados do IDEB em seu computador

# Use aqui o seu diretório dentro da função readRDS("") para a pontar o caminho
# da base de dados do IDEB que foi carregada e salva acima
BaseIDEB <- readRDS("~/GitHub/PneNorte/data-raw/BaseIDEB.rds")

## Tratamento na base geral do IDEB para filtrar apenas a região Norte e
## nomear os municípios e estados correspondentes
BaseIDEBNorte = BaseIDEB |>
  dplyr::rename(codigo_municipio = id_municipio) |>
  dplyr::mutate(codigo_municipio = as.numeric(codigo_municipio)) |>
  dplyr::full_join(codMunicipios,BaseIDEBNorte,by="codigo_municipio") |>
  dplyr::filter(sigla_uf == c("AM","AC","TO","PA","AP","RO","RR")) |>
  dplyr::select(-nome_regiao,-sigla_uf) |>
  dplyr::relocate(nome_uf,.after = "ano") |>
  dplyr::relocate(nome_municipio,.after = "nome_uf")

## OBSERVAÇÃO IMPORTANTE: Além de não haver projeção definida, há muitos
## valores faltantes referentes às características no ano de 2005. Por esse
## motivo decidimos retirar-lo da análise, ficando disponível os dados
## entre os anos de 2007 e 2021

## Indicador 7A -----------------------------------------------------

# Vamos filtrar apenas o IDEB dos anos iniciais do ensino fundamental
# Em seguida indicar corretamente o indicador 7A na base para salvar

baseIndicador7a = BaseIDEBNorte |>
  dplyr::filter(ano %in% "2007":"2021") |>
  dplyr::filter(anos_escolares == "iniciais (1-5)") |>
  dplyr::mutate(nivel_ensino = paste(ensino, anos_escolares,sep = " anos ")) |>
  dplyr::relocate(nivel_ensino,.after = "rede") |>
  dplyr::select(-ensino,-anos_escolares) |>
  dplyr::rename(indicador7A = ideb,projecao_indicador7A = projecao)

## Indicador 7B -----------------------------------------------------

# Vamos filtrar apenas o IDEB dos anos finais do ensino fundamental
# Em seguida indicar corretamente o indicador 7B na base para salvar

baseIndicador7b = BaseIDEBNorte |>
  dplyr::filter(ano %in% "2007":"2021") |>
  dplyr::filter(anos_escolares == "finais (6-9)") |>
  dplyr::mutate(nivel_ensino = paste(ensino, anos_escolares,sep = " anos ")) |>
  dplyr::relocate(nivel_ensino,.after = "rede") |>
  dplyr::select(-ensino,-anos_escolares) |>
  dplyr::rename(indicador7B = ideb,projecao_indicador7B = projecao)


## Indicador 7C -----------------------------------------------------

# Vamos filtrar apenas o IDEB dos anos finais do ensino fundamental
# Em seguida indicar corretamente o indicador 7C na base para salvar

baseIndicador7c = BaseIDEBNorte |>
  dplyr::filter(ano %in% "2007":"2021") |>
  dplyr::filter(anos_escolares == "todos (1-4)") |>
  dplyr::mutate(ensino = case_when(
    ensino == "medio" ~ "ensino medio",TRUE ~ ensino)) |>
  dplyr::mutate(nivel_ensino = paste(ensino, anos_escolares,sep = " ")) |>
  dplyr::relocate(nivel_ensino,.after = "rede") |>
  dplyr::select(-ensino,-anos_escolares) |>
  dplyr::rename(indicador7C = ideb,projecao_indicador7C = projecao)

# Note que o Indicador 7A só possui valores referentes aos anos de 2017 a 2021,
# pois, o IDEB do ensino médio só começou a ser registrado a partir de 2017 pelo
# INEP

## Salvando as bases dos indicadores da meta 7 -----------------------

## OBSERVAÇÃO IMPORTANTE: Decidimos salvar as bases dos indicadores separadamente,
## pois, há uma diferença significativa entre os tamanhos e características de
## cada indicador. Portanto, para a visualização gráfica será necessário
## carregar as bases separadamente.

# Salvando a base do Indicador 7A em .rds
readr::write_rds(baseIndicador7a, "data/Meta7_Indicador7A.rds")

# Salvando a base do Indicador 7B em .rds
readr::write_rds(baseIndicador7b, "data/Meta7_Indicador7B.rds")

# Salvando a base do Indicador 7C em .rds
readr::write_rds(baseIndicador7c, "data/Meta7_Indicador7C.rds")

# Salvando a base do Indicador 7A em .csv
write.csv(baseIndicador7a, file = 'data/Meta7_Indicador7A.csv', row.names = FALSE)

# Salvando a base do Indicador 7B em .csv
write.csv(baseIndicador7b, file = 'data/Meta7_Indicador7B.csv', row.names = FALSE)

# Salvando a base do Indicador 7C em .csv
write.csv(baseIndicador7c, file = 'data/Meta7_Indicador7C.csv', row.names = FALSE)

