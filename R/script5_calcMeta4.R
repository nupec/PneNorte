
# Meta 4: Universalizar, para a população de 4 (quatro) a 17 (dezessete) anos com deficiência,
# transtornos globais do desenvolvimento e altas habilidades ou superdotação, o acesso à educação
# básica e ao atendimento educacional especializado, preferencialmente na rede regular de ensino,
# com a garantia de sistema educacional inclusivo, de salas de recursos multifuncionais, classes,
# escolas ou serviços especializados, públicos ou conveniados.

# Indicador 4A: Percentual da população de 4 a 17 anos de idade com deficiência que
# frenquenta a escola.

# Indicador 4B: Percentual de matrículas em classes comuns da educação básica de alunos de 4 a 17
# anos de idade com deficiência, TGD e altas habilidades ou superdotação.

# Indicador 4C: Percentual de matrículas na Educação Básica de alunos de 4 a 17 anos de idade com
# deficiência, transtornos globais do desenvolvimento (TGD), altas habilidades ou superdotação que
# recebem Atendimento Educacional Especializado.

# Dicionário das bases

## matriculaNorte: contém a matricula de todos os alunos, por estado,
## da região norte entre os anos de 2013 e 2020.

## populacaoEst: contém a população estimada entre os anos de 2014 e 2020 das
## cidade da região norte, bem como a estimativa de 0 a 90+ anos.

## cod_Municipios: contém todos os códigos das divisões territoriais brasileira,

# Carregando a base de dados da população com deficiencia --------------------------------------------
populacaoComDeficiencia <- readRDS("~/GitHub/PneNorte/data/populacaoComDeficiencia.rds")

# Carregando a base de dados da população com deficiência e frequentavam a escola ou creche ----------
popComDeficienciaFrequentaEscola <- readRDS("~/GitHub/PneNorte/data/popComDeficienciaFrequentaEscola.rds")

# Carregando a base de dados das matrículas atualizadas -------------------------------
matriculaNorteAtualizada <- readr::read_rds("data/matricula_att_2012_2022.rds")

# Carregando a base de dados das matrículas --------------------------------------------
matriculaNorte <- readr::read_rds("data/matricula1320.rds")

# Carregando a base de dados dos municipios
codMunicipios <- readxl::read_excel("data-raw/CODIGO_MUNICIPIO.xls")

# Tratando os nomes das variáveis da base: Código dos Municípios
codMunicipios <- codMunicipios |> janitor::clean_names() |>
  dplyr::rename(co_uf = uf,
                codigo_municipio = codigo_municipio_completo) |>
  dplyr::select(nome_regiao,nome_uf,codigo_municipio,nome_municipio) |>
  dplyr::mutate(
    codigo_municipio = as.numeric(codigo_municipio))

# Tratando os nomes das variáveis da base: MatriculaNorte
matriculaNorte <- matriculaNorte |> janitor::clean_names() |>
  dplyr::rename(ano = nu_ano_censo,
                idade = nu_idade_referencia,
                codigo_municipio = co_municipio)

# Agrupando e filtrando as bases da população e calculando os indicadores

## Indicador 4A -----------------------------------------------------

baseIndicador4a <- popComDeficienciaFrequentaEscola |>
  dplyr::filter(sexo == "Total", nome_regiao == "Norte") |>
  dplyr::mutate(frequentavam_escola_ou_creche = as.numeric(frequentavam_escola_ou_creche),
                não_frequentavam_escola_ou_creche = as.numeric(não_frequentavam_escola_ou_creche),
                Total = frequentavam_escola_ou_creche + não_frequentavam_escola_ou_creche) |>
  dplyr::relocate("nome_municipio",.after = "codigo_municipio") |>
  dplyr::select(-sexo,-nome_regiao) |> dplyr::relocate("frequentavam_escola_ou_creche",.after = "nome_uf") |>
  dplyr::relocate("não_frequentavam_escola_ou_creche",.after = "frequentavam_escola_ou_creche") |>
  dplyr::mutate(indicador4A = frequentavam_escola_ou_creche/Total)

## Indicador 4B ----------------------------------------------

# Filtrando da base geral as características de interesse para o cálculo do
# Indicador 4B.

# Educação infantil (1, 2)
# Anos iniciais do ensino fundamental (4, 5, 6, 7, 14,15, 16, 17, 18)
# Anos finais do ensino fundamental (8, 9, 10, 11,19, 20, 21, 41)
# Ensino médio¹ (25, 26, 27,28, 29, 30, 31, 32, 33, 34, 36, 37, 38, 39, 40, 68)
# Educação de jovens e adultos (65, 67, 69, 70, 71, 73, 74)

numeradorIndicador4b <- matriculaNorte |> dplyr::filter(idade %in% 4:17) |>
  dplyr::filter(ano %in% 2015:2020) |>
  dplyr::filter(in_necessidade_especial == 1) |>
  dplyr::filter(in_especial_exclusiva == 0) |>
  dplyr::filter(tp_etapa_ensino %in% c("1","2","4":"11","14":"21","25":"41","65","67":"71","73","74")) |>
  dplyr::group_by(ano, codigo_municipio) |>
  dplyr::count(codigo_municipio) |> dplyr::rename(qtdeMat_4_17_cc = n) |>
  dplyr::mutate(joinTab = stringr::str_c(ano, codigo_municipio,sep = "_"))

denominadorIndicador4b <- matriculaNorte |> dplyr::filter(idade %in% 4:17) |>
  dplyr::filter(ano %in% 2015:2020) |>
  dplyr::filter(in_necessidade_especial == 1) |>
  dplyr::filter(in_especial_exclusiva %in% c("0","1")) |>
  dplyr::filter(tp_etapa_ensino %in% c("1","2","4":"11","14":"21","25":"41","65","67":"71","73","74")) |>
  dplyr::group_by(ano, codigo_municipio) |>
  dplyr::count(codigo_municipio) |> dplyr::rename(qtdeMat_4_17_geral = n) |>
  dplyr::mutate(joinTab = stringr::str_c(ano, codigo_municipio,sep = "_"))

# Juntando as bases acima, organizando e calculando o indicador
baseIndicador4b <- dplyr::full_join(numeradorIndicador4b,denominadorIndicador4b,
                                    by = "joinTab") |>
  dplyr::rename(ano = ano.x,codigo_municipio = codigo_municipio.x) |>
  dplyr::select(-ano.y,-codigo_municipio.y,-joinTab) |>
  dplyr::full_join(codMunicipios,baseIndicador4b,by="codigo_municipio") |>
  dplyr::filter(nome_regiao == "Norte") |> dplyr::select(-nome_regiao) |>
  dplyr::relocate("nome_municipio",.after = "codigo_municipio") |>
  dplyr::relocate("nome_uf",.after = "nome_municipio") |>
  dplyr::mutate(indicador4B = qtdeMat_4_17_cc/qtdeMat_4_17_geral)|>
  dplyr::mutate(joinTab = stringr::str_c(ano, codigo_municipio,sep = "_"))

# Teste de agrupamento das matrículas atualizadas = Falha

# numeradorIndicador4b <- matriculaNorteAtualizada |>
#  dplyr::filter(IN_ESP_CC == 1) |>
#  dplyr::filter(IN_ESP_CE == 0) |>
#  dplyr::mutate(QT_MAT_BAS_4_17 = QT_MAT_BAS_4_5 + QT_MAT_BAS_6_10 +
#                  QT_MAT_BAS_11_14 + QT_MAT_BAS_15_17) |>
#  dplyr::rename(codigo_municipio = CO_MUNICIPIO,ano = NU_ANO_CENSO,) |>
#  dplyr::left_join(numeradorIndicador4b,codMunicipios,by = "codigo_municipio")

## Indicador 4C --------------------------------------------------------

# Filtrando da base geral as características de interesse para o cálculo do
# Indicador 4C.

# Educação infantil (1, 2)
# Anos iniciais do ensino fundamental (4, 5, 6, 7, 14,15, 16, 17, 18)
# Anos finais do ensino fundamental (8, 9, 10, 11,19, 20, 21, 41)
# Ensino médio¹ (25, 26, 27,28, 29, 30, 31, 32, 33, 34, 36, 37, 38, 39, 40, 68)
# Educação de jovens e adultos (65, 67, 69, 70, 71, 73, 74)

numeradorIndicador4c <- matriculaNorte |> dplyr::filter(idade %in% 4:17) |>
  dplyr::filter(ano %in% 2015:2020) |>
  dplyr::filter(in_necessidade_especial == 1) |>
  dplyr::filter(in_especial_exclusiva == 1 | tp_tipo_atendimento_turma == 4) |>
  dplyr::filter(tp_etapa_ensino %in% c("1","2","4":"11","14":"21","25":"41","65","67":"71","73","74")) |>
  dplyr::group_by(ano, codigo_municipio) |>
  dplyr::count(codigo_municipio) |> dplyr::rename(qtdeMat_4_17_ce = n) |>
  dplyr::mutate(joinTab = stringr::str_c(ano, codigo_municipio,sep = "_"))

denominadorIndicador4c <- matriculaNorte |> dplyr::filter(idade %in% 4:17) |>
  dplyr::filter(ano %in% 2015:2020) |>
  dplyr::filter(in_necessidade_especial == 1) |>
  dplyr::filter(tp_etapa_ensino %in% c("1","2","4":"11","14":"21","25":"41","65","67":"71","73","74")) |>
  dplyr::group_by(ano, codigo_municipio) |>
  dplyr::count(codigo_municipio) |> dplyr::rename(qtdeMat_4_17_geral = n) |>
  dplyr::mutate(joinTab = stringr::str_c(ano, codigo_municipio,sep = "_"))

# Juntando as bases acima, organizando e calculando o indicador 4C
baseIndicador4c <- dplyr::full_join(numeradorIndicador4c,denominadorIndicador4c,
                                    by = "joinTab") |>
  dplyr::rename(ano = ano.x,codigo_municipio = codigo_municipio.x) |>
  dplyr::select(-ano.y,-codigo_municipio.y,-joinTab) |>
  dplyr::full_join(codMunicipios,baseIndicador4c,by="codigo_municipio") |>
  dplyr::filter(nome_regiao == "Norte") |> dplyr::select(-nome_regiao) |>
  dplyr::relocate("nome_municipio",.after = "codigo_municipio") |>
  dplyr::relocate("nome_uf",.after = "nome_municipio") |>
  dplyr::mutate(indicador4C = qtdeMat_4_17_ce/qtdeMat_4_17_geral) |>
  dplyr::mutate(joinTab = stringr::str_c(ano, codigo_municipio,sep = "_"))

## Juntando as bases dos indicadores e salvando a base geral da meta 2

### OBSERVAÇÃO!!! ---------------------------------------------------
# Devido as bases utilizadas no cálculo dos indicadores 4B e 4C serem
# diferentes da base utilizada no cálculo do indicador 4A, se faz
# necessário o salvamento desses indicadores em bases separadas

# Salvando a base do indicador 4A
readr::write_rds(baseIndicador4a, "data/Meta4_Indicador4A.rds")

# Agrupando as bases dos indicadores 4B e 4C e salvando a base
baseMeta4 <- dplyr::left_join(baseIndicador4b,baseIndicador4c,
                              by = "joinTab") |>
  dplyr::select(-ano.y, -codigo_municipio.y, - nome_municipio.y,
                -nome_uf.y,-qtdeMat_4_17_geral.x,-joinTab)|>
  dplyr::rename(ano = ano.x, codigo_municipio = codigo_municipio.x,
                nome_municipio = nome_municipio.x,nome_uf = nome_uf.x,
                qtdeMat_4_17_geral = qtdeMat_4_17_geral.y) |>
  dplyr::relocate(indicador4B,.after = qtdeMat_4_17_geral)

# Substituindo NA's por 0
baseMeta4[is.na(baseMeta4)] <- 0

# Salvando
readr::write_rds(baseMeta4, "data/Meta4_Indicador4B_4C.rds")

### OBSERVAÇÃO!!! ---------------------------------------------------
# Para o cálculo desta meta só foi possível utilizar o filtro na variável
# "ano" para os anos de 2015 a 2020, pois não constam matrículas em classes
# comuns ou especiais nos numeradores dos indicadores 4B e 4C para os anos
# anteriores a 2015.
