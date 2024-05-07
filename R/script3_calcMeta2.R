
# Meta 2: Universalizar o ensino fundamental de 9 (nove) anos para toda a
# população de 6 (seis) a 14 (quatorze) anos e garantir que pelo menos 95%
# (noventa e cinco por cento) dos alunos concluam essa etapa na idade recomendada,
# até o último ano de vigência deste PNE.

# Indicador 2A: Percentual de pessoas de 6 a 14 anos que frequentam ou que já concluíram
# o ensino fundamental (taxa de escolarização líquida ajustada).

# Indicador 2B: Percentual de pessoas de 16 anos com pelo menos o ensino fundamental
# concluído.

# Carregando a base de dados ------------------------------------
matriculaNorte <- readr::read_rds("data/matricula1320.rds")
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
    codigo_municipio = as.numeric(codigo_municipio))
# Agrupando as matriculas --------------------------------------------------

## Indicador 2A

# Matrículas da Ensino Fundamental por ano e municicipio entre os anos de 2014
# a 2020

EF_6a14 <- matriculaNorte |>
  dplyr::filter(idade %in% c(6:14)) |>
  dplyr::filter(ano %in% "2014":"2020") |>
  dplyr::filter(tp_etapa_ensino %in% c("14":"21","41","25":"40","68")) |>
  dplyr::group_by(ano, codigo_municipio, tp_etapa_ensino) |>
  dplyr::count(codigo_municipio) |>
  dplyr::mutate(
    joinTab = stringr::str_c(ano, codigo_municipio,sep = "_")
  ) |> dplyr::rename(qtdeMat_6a14 = n)

### OBS 1: Aqui está sendo somado as pessoas de 6 a 14 anos que estão matriculadas no
### fundamental ou que concluíram o EF e estão no ensino médio.

### OBS 2: Ainda não estão nesta conta as pessoas de 6 a 14 anos que concluíram o EF e
### estão no ensino superior e as pessoas que concluíram o EF e estão fora do sistema escolar.

### OBS 3: Não inclui alunos em etapas do EJA.

## Indicador 2B

EF_conc_16 <- matriculaNorte |>
  dplyr::filter(idade == "16") |>
  dplyr::filter(ano %in% "2014":"2020") |>
  dplyr::filter(tp_etapa_ensino %in% c("25":"40","67","71","74")) |>
  dplyr::group_by(ano, codigo_municipio, tp_etapa_ensino) |>
  dplyr::count(codigo_municipio) |>
  dplyr::mutate(
    joinTab = stringr::str_c(ano, codigo_municipio,sep = "_")) |>
  dplyr::rename(qtdeMat_16 = n)

### OBS 1: Verificar no censo escolar do ensino superior todos os alunos
### com 16 anos.

### OBS 2: Este indicador não contempla as pessoas de 16 anos que estão
### fora do sistema escolar.

### OBS 3: Inclui etapas do EJA.

# Agrupando as populações --------------------------------------------------

## Agrupando a população de 6 a 14 anos
popEnsFund6a14 <- populacaoEst |>
  dplyr::filter(idade %in% c(6:14)) |>
  dplyr::group_by(ano, codigo_municipio, nome_municipio) |>
  dplyr::summarise(
    popFaixa6a14 = sum(populacao_estimada)) |>
  dplyr::group_by(codigo_municipio) |>
  dplyr::mutate(
    joinTab = stringr::str_c(ano, codigo_municipio,
                             sep = "_")) |>
  dplyr::relocate("joinTab",.after = "nome_municipio")

## Agrupando a população de 16 anos
popEnsFund16 <- populacaoEst |>
  dplyr::filter(idade == "16") |>
  dplyr::group_by(ano, codigo_municipio, nome_municipio) |>
  dplyr::summarise(
    popFaixa16 = sum(populacao_estimada)
  ) |>
  dplyr::group_by(codigo_municipio) |>
  dplyr::mutate(
    joinTab = stringr::str_c(ano, codigo_municipio,
                             sep = "_")) |>
  dplyr::relocate("joinTab",.after = "nome_municipio")

# Agrupando as variáveis e calculando os indicadores -----------------------

## Indicador 2A

baseindicador2A <- dplyr::left_join(EF_6a14,popEnsFund6a14,
                              by = "joinTab") |>
  dplyr::select(ano.x, codigo_municipio.x, nome_municipio,
                qtdeMat_6a14, popFaixa6a14) |>
  dplyr::rename(codigo_municipio = codigo_municipio.x,ano = ano.x) |>
  dplyr::group_by(ano,codigo_municipio,nome_municipio,popFaixa6a14) |>
  dplyr::summarise(total_mat_6a14 = sum(qtdeMat_6a14, na.rm = T)) |>
  dplyr::mutate(
    indicador2A = total_mat_6a14/popFaixa6a14
  ) |> dplyr::relocate("popFaixa6a14",.after = "total_mat_6a14")|>
  dplyr::mutate(joinTab = stringr::str_c(ano, codigo_municipio,
                                         sep = "_"))

## Indicador 2B

baseindicador2B <- dplyr::left_join(EF_conc_16,
                                    popEnsFund16,
                                    by = "joinTab") |>
  dplyr::select(ano.x, codigo_municipio.x, nome_municipio,
                qtdeMat_16, popFaixa16) |>
  dplyr::rename(codigo_municipio = codigo_municipio.x,ano = ano.x) |>
  dplyr::group_by(ano,codigo_municipio,nome_municipio,popFaixa16) |>
  dplyr::summarise(total_mat_16 = sum(qtdeMat_16, na.rm = T)) |>
  dplyr::mutate(
    indicador2B = total_mat_16/popFaixa16
  ) |> dplyr::relocate("popFaixa16",.after = "total_mat_16") |>
  dplyr::mutate(joinTab = stringr::str_c(ano, codigo_municipio,
                                         sep = "_"))

## Juntando as bases dos indicadores e salvando a base geral da meta 2

baseMeta2 <- dplyr::left_join(baseindicador2B,
                              baseindicador2A,by = "joinTab")|>
  dplyr::select(-ano.y, -codigo_municipio.y, - nome_municipio.y,
                -joinTab)|>
  dplyr::rename(ano = ano.x, codigo_municipio = codigo_municipio.x,
                nome_municipio = nome_municipio.x)

readr::write_rds(baseMeta2, "data/Meta2.rds")

write.csv(baseMeta2, file = 'data/Meta2.csv', row.names = FALSE)

### Criando um token para atualizar os scripts no github----------------------

# library(usethis)
# use_git_config(user.name = "Nupec", user.email = "nupec@ufam.edu.br")
# usethis::create_github_token()
# gitcreds::gitcreds_set()
# ----------------------------------------------------------------------------
