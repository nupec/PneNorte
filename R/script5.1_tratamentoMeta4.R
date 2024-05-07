
# Acessando a base de matrículas através do "basedosdados"

# install.packages("basedosdados")
# library("basedosdados")

# Definindo o id do projeto no Google Cloud
# set_billing_id("indicadores-educacionais-ods4")

# Carregando a base de dados
# query <- bdplyr("br_inep_censo_escolar.matricula")
# df <- bd_collect(query)

# Salvando a base e renomeando
# matriculaNorte_BD <- readr::write_rds(df, "data/matriculaNorte_BD.rds")

# Carregando e tratando os dados populacionais de pessoas com deficiência

# Carregando as base de dados que possivelmente poderão ser usadas ---------
matriculaNorte <- readr::read_rds("data/matricula1320.rds")
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
    codigo_municipio = as.numeric(codigo_municipio))

# Importando e tratando a base de dados da população com deficiencia -----------------------------------

## Nesta etapa, importa-se a Tabela 3425 gerada pelo Sistema SIDRA/IBGE e que
## contém a contagem da popuçação de pessoas com deficiência (CENSO - 2010). Link:

# url("https://sidra.ibge.gov.br/tabela/3425#/n6/all/v/allxp/p/all/c134/7815/c1/0/c2/0/c58/1141%201142%202792/d/v93%200/l/v,p+c134+c1,t+c2+c58/resultado")

populacaoComDeficiencia <- readxl::read_excel("data-raw/tabela3425_tidy.xlsx") |>
  dplyr::mutate(codigo_municipio = as.numeric(codigo_municipio),ano2010 = as.numeric(ano2010)) |>
  dplyr::full_join(codMunicipios,populacaoComDeficiencia,by="codigo_municipio") |>
  dplyr::select(-nome_municipio.x) |>
  dplyr::rename(nome_municipio = nome_municipio.y)

# Salvando a base de dados ------------------------------------------------

readr::write_rds(populacaoComDeficiencia, "data/populacaoComDeficiencia.rds")

# Importando e tratando a base de dados da população com deficiencia que frequentavam a escola ou creche -----------------------------------

## Nesta etapa, importa-se a Tabela 3434 gerada pelo Sistema SIDRA/IBGE e que
## contém a contagem da popuçação de pessoas com deficiência que frequentavam
## a escola ou creche (CENSO - 2010). Link:

# url("https://sidra.ibge.gov.br/tabela/3434#/n1/all/n3/all/n6/all/v/allxp/p/all/c134/0,7815,12161/c12081/0/c2/all/c58/0,1140,1141,1142,1143,2792,2798,110993/d/v93%200/l/v,p+c134+c12081,t+c2+c58/resultado")

popComDeficienciaFrequentaEscola <- readxl::read_excel("data-raw/tabela3434_tidy.xlsx") |>
  tidyr::fill(nome_municipio,codigo_municipio) |>
  dplyr::mutate(codigo_municipio = as.numeric(codigo_municipio)) |>
  dplyr::full_join(codMunicipios,popComDeficienciaFrequentaEscola,by="codigo_municipio") |>
  dplyr::select(-nome_municipio.x) |>
  dplyr::rename(nome_municipio = nome_municipio.y)

# Salvando a base de dados ------------------------------------------------

readr::write_rds(popComDeficienciaFrequentaEscola,"data/popComDeficienciaFrequentaEscola.rds")

## Carregando e tratando as bases de dados do censo escolar atualizado de 2012 a 2022

# Nesta etapa, eu seleciono as variáveis necessárias para a análise e cálculo dos indicadores
# nesta meta e para as próximas metas.

variaveis_selecionadas_1222 <- c("NU_ANO_CENSO","NO_REGIAO","SG_UF",
                                 "NO_MUNICIPIO","CO_MUNICIPIO","CO_ENTIDADE",
                                 "NO_ENTIDADE","TP_DEPENDENCIA","TP_LOCALIZACAO",
                                 "TP_SITUACAO_FUNCIONAMENTO","IN_DEPENDENCIA_PNE",
                                 "TP_AEE","IN_REGULAR","IN_DIURNO","IN_NOTURNO",
                                 "IN_EAD","IN_BAS","IN_INF","IN_INF_CRE","IN_INF_PRE",
                                 "IN_FUND","IN_FUND_AI","IN_FUND_AF","IN_MED","IN_PROF",
                                 "IN_PROF_TEC","IN_EJA","IN_EJA_FUND","IN_EJA_MED",
                                 "IN_ESP","IN_ESP_CC","IN_ESP_CE","QT_MAT_BAS",
                                 "QT_MAT_INF","QT_MAT_INF_CRE","QT_MAT_INF_PRE",
                                 "QT_MAT_FUND","QT_MAT_FUND_AI","QT_MAT_FUND_AF",
                                 "QT_MAT_MED","QT_MAT_PROF","QT_MAT_PROF_TEC",
                                 "QT_MAT_EJA","QT_MAT_EJA_FUND","QT_MAT_EJA_MED",
                                 "QT_MAT_ESP","QT_MAT_ESP_CC","QT_MAT_ESP_CE",
                                 "QT_MAT_BAS_FEM","QT_MAT_BAS_MASC","QT_MAT_BAS_ND",
                                 "QT_MAT_BAS_BRANCA","QT_MAT_BAS_PRETA",
                                 "QT_MAT_BAS_PARDA","QT_MAT_BAS_AMARELA","QT_MAT_BAS_INDIGENA",
                                 "QT_MAT_BAS_0_3","QT_MAT_BAS_4_5","QT_MAT_BAS_6_10",
                                 "QT_MAT_BAS_11_14","QT_MAT_BAS_15_17",
                                 "QT_MAT_BAS_18_MAIS","QT_MAT_INF_INT","QT_MAT_INF_CRE_INT",
                                 "QT_MAT_INF_PRE_INT","QT_MAT_FUND_INT","QT_MAT_FUND_AI_INT",
                                 "QT_MAT_FUND_AF_INT","QT_MAT_MED_INT")

## Nesta etapa, eu aponto os caminhos das bases de dados

arq_matriculas_no_1222 <-list.files("data-raw/censo_escolar_2012_2022_atualizado/microdados_ed_basica_2012_2022_tidy",full.names = T)

## Aqui, eu importo as Matrículas dos alunos do Norte do Brasil.
## Apenas com as variáveis de interesse para o cálculo das metas.

matriculas_no_1222 <- purrr::map_dfr(arq_matriculas_no_1222,data.table::fread,
                                     select = (variaveis_selecionadas_1222)) |>
  dplyr::filter(NO_REGIAO == "Norte")

# Salvando
readr::write_rds(matriculas_no_1222, file = "data/matricula_att_2012_2022.rds")
