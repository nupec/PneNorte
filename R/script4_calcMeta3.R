
# Meta 3: Universalizar, até 2016, o atendimento escolar para toda a população de 15
# (quinze) a 17 (dezessete) anos e elevar, até o final do período de vigência deste
# PNE, a taxa líquida de matrículas no ensino médio para 85% (oitenta e cinco por cento).

# Indicador 3A: Percentual da população de 15 a 17 anos que frequenta a escola ou já concluiu
# a educação básica.

# Indicador 3B: Percentual da população de 15 a 17 anos que frequenta o ensino médio ou
# possui educação básica completa.

# Carregando a base de dados ----------------------------------------------
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

# Agrupando as matriculas --------------------------------------------------

## Indicador 3A

# Matrículas do Ensino Fundamental,Médio e EJA por ano e municicipio entre os anos de
# 2014 a 2020

EF_EM_EJA_EP_15a17 <- matriculaNorte |>
  dplyr::filter(idade %in% c(15:17)) |>
  dplyr::filter(ano %in% "2014":"2020") |>
  dplyr::filter(tp_etapa_ensino %in% c("14":"21","25":"41","67":"71","73":"74")) |>
  dplyr::group_by(ano, codigo_municipio, tp_etapa_ensino) |>
  dplyr::count(codigo_municipio) |>
  dplyr::mutate(
    joinTab = stringr::str_c(ano, codigo_municipio,sep = "_")
  ) |> dplyr::rename(qtdeMat_escola15a17 = n)

### OBS 1: Aqui está sendo somado somente as pessoas de 15 a 17 anos que estão matriculadas no
### fundamental, médio, EJA e cursos tecnicos.

### OBS 2: Ainda não estão nesta conta as pessoas de 15 a 17 anos que concluíram a educação básica
### e estão no ensino superior e as pessoas que concluíram a EB e estão fora do sistema escolar.

## Indicador 3B

# Matrículas do Ensino Médio por ano e municicipio entre os anos de
# 2014 a 2020

EM_15a17 <- matriculaNorte |>
  dplyr::filter(idade %in% c(15:17)) |>
  dplyr::filter(ano %in% "2014":"2020") |>
  dplyr::filter(tp_etapa_ensino %in% c("25":"40","67","71","74")) |>
  dplyr::group_by(ano, codigo_municipio, tp_etapa_ensino) |>
  dplyr::count(codigo_municipio) |>
  dplyr::mutate(
    joinTab = stringr::str_c(ano, codigo_municipio,
                             sep = "_")
  ) |> dplyr::rename(qtdeMat_EM15a17 = n)

# Agrupando as populações --------------------------------------------------

## Agrupando a população de 15 a 17 anos
pop15a17 <- populacaoEst |>
  dplyr::filter(idade %in% c(15:17)) |>
  dplyr::group_by(ano, codigo_municipio, nome_municipio) |>
  dplyr::summarise(
    popFaixa15a17 = sum(populacao_estimada)
  ) |>
  dplyr::group_by(codigo_municipio) |>
  dplyr::mutate(
    joinTab = stringr::str_c(ano, codigo_municipio,
                             sep = "_")) |>
  dplyr::relocate("joinTab",.after = "nome_municipio")

# Agrupando as variáveis e calculando os indicadores ----------------------

## Indicador 3B

baseindicador3B <- dplyr::left_join(EM_15a17,
                                    pop15a17,
                                    by = "joinTab") |>
  dplyr::select(ano.x, codigo_municipio.x, nome_municipio,
                qtdeMat_EM15a17, popFaixa15a17) |>
  dplyr::rename(codigo_municipio = codigo_municipio.x,ano = ano.x) |>
  dplyr::group_by(ano,codigo_municipio,nome_municipio,popFaixa15a17) |>
  dplyr::summarise(total_mat_EM_15a17 = sum(qtdeMat_EM15a17, na.rm = T)) |>
  dplyr::mutate(
    indicador3B = total_mat_EM_15a17/popFaixa15a17
  ) |> dplyr::relocate("popFaixa15a17",.after = "total_mat_EM_15a17")|>
  dplyr::mutate(joinTab = stringr::str_c(ano, codigo_municipio,sep = "_"))

## Indicador 3A

baseindicador3A <- dplyr::left_join(EF_EM_EJA_EP_15a17,
                                    pop15a17,
                                    by = "joinTab") |>
  dplyr::select(ano.x, codigo_municipio.x, nome_municipio,
                qtdeMat_escola15a17, popFaixa15a17) |>
  dplyr::rename(codigo_municipio = codigo_municipio.x,ano = ano.x) |>
  dplyr::group_by(ano,codigo_municipio,nome_municipio,popFaixa15a17) |>
  dplyr::summarise(total_mat_escola_15a17 = sum(qtdeMat_escola15a17, na.rm = T)) |>
  dplyr::mutate(
    indicador3A = total_mat_escola_15a17/popFaixa15a17
  ) |> dplyr::relocate("popFaixa15a17",.after = "total_mat_escola_15a17") |>
  dplyr::mutate(joinTab = stringr::str_c(ano, codigo_municipio,sep = "_"))

## Juntando as bases dos indicadores e salvando a base geral da meta 3

baseMeta3 <- dplyr::left_join(baseindicador3B,
                              baseindicador3A,by = "joinTab")|>
  dplyr::select(-ano.y, -codigo_municipio.y, - nome_municipio.y,
                -joinTab,-popFaixa15a17.y)|>
  dplyr::rename(ano = ano.x, codigo_municipio = codigo_municipio.x,
                nome_municipio = nome_municipio.x,
                popFaixa15a17 = popFaixa15a17.x)

readr::write_rds(baseMeta3, "data/Meta3.rds")
