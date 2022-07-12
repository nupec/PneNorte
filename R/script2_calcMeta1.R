
# Meta 1: Universalizar, até 2016, a educação infantil na pré-escola para as
# crianças de 4 (quatro) a 5 (cinco) anos de idade e ampliar a oferta de educação
# infantil em creches de forma a atender, no mínimo, 50% (cinquenta por cento)
# das crianças de até 3 (três) anos até o final da vigência deste PNE.

# Indicador 1A: Percentual da população de 4 a 5 anos que frequenta a
# escola/creche.

# Indicador 1B: Percentual da população de 0 a 3 anos que frequenta a
# escola/creche.

# Carregando a base de dados ------------------------------------
brasil         <- readr::read_rds("data-raw/Brasil_shp.rds")
estados        <- readr::read_rds("data-raw/Estados_shp.rds")
municipios     <- readr::read_rds("data-raw/Municipios_shp.rds")
dados_idh_muni <- readr::read_rds("data-raw/dados_idh_muni.rds")
matriculaNorte <- readr::read_rds("data/matricula1320.rds")
populacaoEst   <- readr::read_rds("data/populacaoEstimadaPorIdade.rds")
codMunicipios <- readxl::read_excel("data-raw/CODIGO_MUNICIPIO.xls")

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

# Matrículas da Educação Infantil por ano e municipio entre os anos de 2020
matriculaEduInf <- matriculaNorte |>
  dplyr::filter(ano %in% "2014":"2020") |>
  dplyr::filter(tp_etapa_ensino %in% "1":"2") |>
  dplyr::group_by(ano, codigo_municipio, tp_etapa_ensino) |>
  dplyr::count(codigo_municipio) |>
  dplyr::mutate(
    joinTab = stringr::str_c(ano, codigo_municipio,
                             sep = "_")
  ) |> dplyr::rename(qtdeMat = n)

# Agrupando as matrículas por etapa de ensino:

## Creche
matriculaCrecheNorte <- matriculaEduInf |>
  # Filtrando etapa 1 referente às matriculas em creche
  dplyr::filter(tp_etapa_ensino == "1") |>
  dplyr::rename(qtdeMatCreche = qtdeMat)

## Pré-Escola
matriculaPreNorte <- matriculaEduInf |>
  # Filtrando etapa 1 referente às matriculas em creche
  dplyr::filter(tp_etapa_ensino == "2")|>
  dplyr::rename(qtdeMatPre = qtdeMat)

# Agrupando as populações --------------------------------------------------

## Agrupando a população de 0 a 3 anos
popCrecheEst <- populacaoEst |>
  dplyr::filter(idade %in% c(0:3)) |>
  dplyr::group_by(ano, codigo_municipio, nome_municipio) |>
  dplyr::summarise(
    popFaixa0a3 = sum(populacao_estimada)
  ) |>
  dplyr::group_by(codigo_municipio) |>
  dplyr::mutate(
    joinTab = stringr::str_c(ano, codigo_municipio,
                             sep = "_")) |>
  dplyr::relocate("joinTab",.after = "nome_municipio")

## Agrupando a população de 4 e 5 anos
popPreEst <- populacaoEst |>
  dplyr::filter(idade %in% c(4:5)) |>
  dplyr::group_by(ano, codigo_municipio, nome_municipio) |>
  dplyr::summarise(
    popFaixa4e5 = sum(populacao_estimada)
  ) |>
  dplyr::group_by(codigo_municipio) |>
  dplyr::mutate(
    joinTab = stringr::str_c(ano, codigo_municipio,
                             sep = "_")) |>
  dplyr::relocate("joinTab",.after = "nome_municipio")

## Reunindo as bases das matrículas e da população de 0 a 5 anos

pop0a5 <- dplyr::left_join(popCrecheEst, popPreEst, by = "joinTab" ) |>
  dplyr::relocate(popFaixa0a3, .before = popFaixa4e5) |>
  dplyr::select(-ano.y, -codigo_municipio.y, - nome_municipio.y) |>
  dplyr::rename(ano = ano.x, codigo_municipio = codigo_municipio.x)

baseMatricula <- dplyr::left_join(matriculaCrecheNorte,
                                  matriculaPreNorte,
                                  by = "joinTab")

## Agrupando as variáveis pela chave primária e calculando os indicadores
baseMeta1 <- dplyr::left_join(baseMatricula,
                              pop0a5,
                              by = "joinTab") |>
  dplyr::select(ano, codigo_municipio, nome_municipio.x,
                qtdeMatCreche, popFaixa0a3, qtdeMatPre, popFaixa4e5) |>
  dplyr::rename(nome_municipio = nome_municipio.x) |>
  dplyr::mutate(
    indicador1b = qtdeMatCreche/popFaixa0a3,
    indicador1a = qtdeMatPre/popFaixa4e5 #,
    # meta1 = (qtdeMatCreche + qtdeMatPre)/(popFaixa0a3+popFaixa4e5)
  )

## Salvando a base da Meta 1
baseGeral <- dplyr::left_join(codMunicipios, baseMeta1, by = "codigo_municipio") |>
  dplyr::select(-nome_municipio.x) |>
  dplyr::rename(nome_municipio = nome_municipio.y)


readr::write_rds(baseGeral, "data/Meta1.rds")
