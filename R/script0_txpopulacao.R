
# Importando a base de dados ----------------------------------------------

## Nesta etapa, importa-se a Tabela 1552, gerada pelo Sistema SIDRA/IBGE e que
## contém a contagem da popuçação desagregada por idade (CENSO - 2010). Link:

url("https://sidra.ibge.gov.br/tabela/1552?fbclid=IwAR3PonnEhgVF1jdvbkyUnGCFvg2dnogSyRLtnGqY6M65jJo7T0eou3DVsQ0#/n1/all/n3/all/n6/all/v/allxp/p/all/c1/0/c2/0/c286/0/c287/all/l/v,p+c1+c2,t+c286+c287/resultado")

## A seguir, importa-se a tabela 1552 e corrige-se o tipo de variável
## "codigo_municipio" para o formato numérico.

# 1) Coeficientes populacionais das idade simples por município brasileiro, ano de referência: 2010

df <- readxl::read_excel("data-raw/tabela1552_tidy.xlsx", sheet = 2) |>
  dplyr::mutate(
    codigo_municipio = as.numeric(codigo_municipio)
  ) |>
  tidyr::fill(c("codigo_municipio", "nome_municipio", "nome_uf"))

# 2) População dos municípios brasileiros entre 1991 e 2021

basedosdados::set_billing_id("indicadores-educacionais-ods4")

# Para carregar o dado direto no R
query <- basedosdados::bdplyr("br_ibge_populacao.municipio")
popMunicipiosBr <-basedosdados::bd_collect(query)

readr::write_rds(popMunicipiosBr, "data/popMunBr1991a2021.rds")

# 3) Base de mapas -----------------------------------------------------------

##  Delimitação do Brasil
brasil <- geobr::read_country(year = 2020)
readr::write_rds(brasil, "data-raw/Brasil_shp.rds")

##  Delimitação do Estado
estados <- geobr::read_state(year = 2020)
readr::write_rds(estados, "data-raw/Estados_shp.rds")

##  Delimitação do Municipios
municipios <- geobr::read_municipality(year = 2020)
readr::write_rds(municipios, "data-raw/Municipios_shp.rds")


# Atualizações:
# 01/08/2021
# 24/08/2021
# 31/08/2021
# 28/04/2021
