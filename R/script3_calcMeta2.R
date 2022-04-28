
# Meta 2: Universalizar o ensino fundamental de 9 (nove) anos para toda a
# população de 6 (seis) a 14 (quatorze) anos e garantir que pelo menos 95%
# (noventa e cinco por cento) dos alunos concluam essa etapa na idade recomendada,
# até o último ano de vigência deste PNE.

# Indicador 2A: Percentual de pessoas de 6 a 14 anos que frequentam ou que já concluíram o
# ensino fundamental (taxa de escolarização líquida ajustada).

# Indicador 2B: Percentual de pessoas de 16 anos com pelo menos o ensino fundamental
# concluído.

# Carregando a base de dados ------------------------------------
brasil         <- readr::read_rds("data-raw/Brasil_shp.rds")
estados        <- readr::read_rds("data-raw/Estados_shp.rds")
municipios     <- readr::read_rds("data-raw/Municipios_shp.rds")
dados_idh_muni <- readr::read_rds("data-raw/dados_idh_muni.rds")
matriculaNorte <- readr::read_rds("data-raw/matricula1320.rds")
populacaoEst   <- readr::read_rds("data/populacaoEstimadaPorIdade.rds")
codMunicipios  <- readxl::read_excel("data-raw/CODIGO_MUNICIPIO.xls")

# Tratamento da base dados ------------------------------------------------

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
  dplyr::select(-municipio) |>
  dplyr::mutate(
    codigo_municipio = as.numeric(codigo_municipio)
  )

# Seguindo a metodologia do INEP para estimativa do indicador

## São criadas as variáveis para o Ensino Regular (EF_regular),
## EJA (EF_EJA) e, caso tenha concluído o Ensino Fundamental (EF_conc)

# Matrículas da Ensino Fundamental por ano e municicipio entre os anos de 2014
# a 2020

EF_regular <- matriculaNorte |>
  dplyr::filter(ano %in% "2014":"2020") |>
  dplyr::filter(tp_etapa_ensino %in% c("14":"21","41")) |>
  dplyr::group_by(ano, codigo_municipio, tp_etapa_ensino) |>
  dplyr::count(codigo_municipio) |>
  dplyr::mutate(
    joinTab = stringr::str_c(ano, codigo_municipio,
                             sep = "_")
  ) |> dplyr::rename(qtdeMat = n)

EF_EJA <- matriculaNorte |>
  dplyr::filter(ano %in% "2014":"2020") |>
  dplyr::filter(tp_etapa_ensino %in% c("14":"21","41")) |>
  dplyr::group_by(ano, codigo_municipio, tp_etapa_ensino) |>
  dplyr::count(codigo_municipio) |>
  dplyr::mutate(
    joinTab = stringr::str_c(ano, codigo_municipio,
                             sep = "_")
  ) |> dplyr::rename(qtdeMat = n)

EF_conc <- matriculaNorte |>
  dplyr::filter(ano %in% "2014":"2020") |>
  dplyr::filter(tp_etapa_ensino %in% c("14":"21","41")) |>
  dplyr::group_by(ano, codigo_municipio, tp_etapa_ensino) |>
  dplyr::count(codigo_municipio) |>
  dplyr::mutate(
    joinTab = stringr::str_c(ano, codigo_municipio,
                             sep = "_")
  ) |> dplyr::rename(qtdeMat = n)
# Agrupando as populações --------------------------------------------------

## Agrupando a população de 6 a 14 anos
popEnsFund <- populacaoEst |>
  dplyr::filter(idade %in% c(6:14)) |>
  dplyr::group_by(ano, codigo_municipio, nome_municipio) |>
  dplyr::summarise(
    popFaixa6a14 = sum(populacao_estimada)
  ) |>
  dplyr::group_by(codigo_municipio) |>
  dplyr::mutate(
    joinTab = stringr::str_c(ano, codigo_municipio,
                             sep = "_")) |>
  dplyr::relocate("joinTab",.after = "nome_municipio")

baseMeta2 <- dplyr::left_join(matriculaEnsFund,
                              popEnsFund,
                              by = "joinTab") |>
  dplyr::select(ano, codigo_municipio, nome_municipio.x,
                qtdeMatCreche, popFaixa0a3, qtdeMatPre, popFaixa4e5) |>
  dplyr::rename(nome_municipio = nome_municipio.x) |>
  dplyr::mutate(
    indice1b = qtdeMatCreche/popFaixa0a3,
    indice1a = qtdeMatPre/popFaixa4e5,
    meta1 = (qtdeMatCreche + qtdeMatPre)/(popFaixa0a3+popFaixa4e5)
  )

baseGeral <- dplyr::left_join(codMunicipios, baseMeta1, by = "codigo_municipio") |>
  dplyr::select(-nome_municipio.x) |>
  dplyr::rename(nome_municipio = nome_municipio.y)


readr::write_rds(baseGeral, "data/Meta1.rds")
