
# Meta 5: Alfabetizar todas as Crianças, no máximo, até o final do 3º (terceiro) ano do
# Ensino Fundamental.
# A Avaliação Nacional da Alfabetização (ANA) foi desenvolvida pelo Instituto Nacional
# de Estudos e Pesquisas Educacionais Anísio Teixeira (Inep) com o objetivo de aferir os níveis
# de alfabetização e letramento em Língua Portuguesa e alfabetização em Matemática das crianças
# regularmente matriculadas no 3º ano do ensino fundamental, fase final do ciclo de alfabetização,
# bem como as condições das instituições de ensino às quais estão vinculadas (Brasil. Inep, 2013,
# 2015a).

# Indicador 5A: Percentual de estudantes alfabetizados até o final do 2º ano do ensino
# fundamental em Língua Portuguesa no Saeb.

# Indicador 5B: Percentual de estudantes alfabetizados até o final do 2º ano do ensino
# fundamental em Matemática no Saeb.

# Carregando a base de dados ----------------------------------------------

# Utilizando dados do saeb 2019
library("basedosdados")

# Definindo o projeto que desejamos acessar no Google Cloud
set_billing_id("indicadores-educacionais-ods4")

# Para carregar o dado direto no R o banco de dados
query <- bdplyr("br_inep_saeb.aluno_ef_2ano")
dadosSaeb <- bd_collect(query)

# Tratamento da base dados ------------------------------------------------

# Dicionário das bases
set_billing_id("indicadores-educacionais-ods4")

# Para carregar o dado direto no R o banco de dados
query <- bdplyr("br_inep_saeb.dicionario")
dicionarioSaeb <- bd_collect(query)


# FILTRANDO OS INDICADORES --------------------------------------------------

## Indicador 5A

PROFICIENCIA_LP <- dadosSaeb |>
  dplyr::filter(id_regiao %in% 1) |>
  dplyr::filter(disciplina %in% "LP") |>
  dplyr::filter(presenca %in% 1)

#________________________________________________________________
PROFICIENCIA_LP <- dadosSaeb |>
  dplyr::filter(idade %in% c(15:17)) |>
  dplyr::filter(ano %in% "2014":"2020") |>
  dplyr::group_by(ano, codigo_municipio, tp_etapa_ensino) |>
  dplyr::count(codigo_municipio) |>
  dplyr::mutate(
    joinTab = stringr::str_c(ano, codigo_municipio,
                             sep = "_")
  ) |> dplyr::rename(qtdeMat_escola15a17 = n)

## Indicador 5B



