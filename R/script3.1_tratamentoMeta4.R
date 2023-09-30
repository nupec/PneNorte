
# Carregando e tratando os dados populacionais de pessoas com deficiência


# Carregando as base de dados que possivelmente poderão ser usadas ---------
matriculaNorte <- readr::read_rds("data/matricula1320.rds")
populacaoEst   <- readr::read_rds("data/populacaoEstimadaPorIdade.rds")
codMunicipios <- readxl::read_excel("data-raw/CODIGO_MUNICIPIO.xls")

# Tratamento da base dados -------------------------------------------------

# Dicionário das bases

## matriculaNorte: contém a matricula de todos os alunos, por estado,
## da região norte entre os anos de 2013 e 2020.

## populacaoEst: contém a população estimada entre os anos de 2014 e 2020 das
## cidade da região norte, bem como a estimativa de 0 a 90+ anos.

## cod_Municipios: contém todos os códigos das divisões territoriais brasileira,

# Tratando os nomes das variáveis da base: MatriculaNorte
matriculaNorte <- matriculaNorte |> janitor::clean_names() |>
  dplyr::rename(ano = nu_ano_censo,
                idade = nu_idade_referencia,
                codigo_municipio = co_municipio)

# Tratando os nomes das variáveis da base: Código dos Municípios
codMunicipios <- codMunicipios |> janitor::clean_names() |>
  dplyr::rename(co_uf = uf,
                codigo_municipio = codigo_municipio_completo) |>
  dplyr::select(nome_regiao,nome_uf,codigo_municipio,nome_municipio) |>
  dplyr::mutate(
    codigo_municipio = as.numeric(codigo_municipio)
  )

# Importando e tratando a base de dados populacional -----------------------------------

## Nesta etapa, importa-se a Tabela 3425 gerada pelo Sistema SIDRA/IBGE e que
## contém a contagem da popuçação de pessoas com deficiência (CENSO - 2010). Link:

# url("https://sidra.ibge.gov.br/tabela/3434#/n1/all/n3/all/n6/all/v/allxp/p/all/c134/0,7815,12161/c12081/0/c2/all/c58/0,1140,1141,1142,1143,2792,2798,110993/d/v93%200/l/v,p+c134+c12081,t+c2+c58/resultado")

populacaoComDeficiencia <- readxl::read_excel("data-raw/tabela3425_tidy.xlsx") |>
  dplyr::mutate(codigo_municipio = as.numeric(codigo_municipio),ano2010 = as.numeric(ano2010)) |>
  dplyr::full_join(codMunicipios,populacaoComDeficiencia,by="codigo_municipio") |>
  dplyr::select(-nome_municipio.x) |>
  dplyr::rename(nome_municipio = nome_municipio.y)

# Salvando a base de dados ------------------------------------------------

readr::write_rds(populacaoComDeficiencia, "data/populacaoComDeficiencia.rds")
