
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

# Utilizando dados do saeb 2019 e 2021 | Microdados recolhidos em anos ímpares
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

# Id_regiao   => 1  => Norte
# Disciplina  => LP => Lingua Portuguesa | MT => Matemática
# Localização => 1  => Urbana  | Localização => 2 => Rural
# Área        => 1  => Capital | Área        => 2 => Interior

# Escala de Proficiência de Língua Portuguesa e Matemática - Classificação

# Nível abaixo de 1 = Proficiência menor que 650
# Nível 1 = Proficiência maior ou igual a 650 e menor que 675
# Nível 2 = Proficiência maior ou igual a 675 e menor que 700
# Nível 3 = Proficiência maior ou igual a 700 e menor que 725
# Nível 4 = Proficiência maior ou igual a 725 e menor que 750
# Nível 5 = Proficiência maior ou igual a 750 e menor que 775
# Nível 6 = Proficiência maior ou igual a 775 e menor que 800
# Nível 7 = Proficiência maior ou igual a 800 e menor que 825
# Nível 8 = Proficiência maior ou igual a 825

# FILTRANDO OS INDICADORES --------------------------------------------------

# Classificando a pontuações obtidas de acordo com a escala de proficiencia

# Definindo os limites de proficiência para cada nível
limites_proficiencia <- c(650, 675, 700, 725, 750, 775, 800, 825)

# Definindo os rótulos para cada nível
rotulos_niveis <- c("Abaixo do Nível 1", "Nível 1", "Nível 2", "Nível 3",
                    "Nível 4", "Nível 5", "Nível 6", "Nível 7", "Nível 8")

## Indicador 5A

proficiencia_LP <- dadosSaeb |>
  dplyr::filter(id_regiao %in% 1) |>
  dplyr::filter(disciplina %in% "LP") |>
  dplyr::filter(presenca %in% 1) |>
  dplyr::group_by(ano, id_municipio, sigla_uf, localizacao,area,
                  disciplina, proficiencia_saeb) |>
  dplyr::count(id_municipio) |>
  dplyr::select(ano, id_municipio, sigla_uf, localizacao,area,
                disciplina, proficiencia_saeb) |>
  dplyr::mutate(classificacao_proficiencia = cut(proficiencia_saeb,
                                                 breaks = c(-Inf, limites_proficiencia, Inf),
                                                 labels = rotulos_niveis,
                                                 include.lowest = TRUE)) |>
  dplyr::mutate(joinTab = stringr::str_c(ano, id_municipio, sep = "_"))

# Definimos os limites de proficiência para cada nível e os rótulos correspondentes
# em dois vetores separados (`limites_proficiencia` e `rotulos_niveis`).
# Usamos a função `mutate` do `dplyr` para criar uma nova coluna chamada
# `classificacao_proficiencia`. A função `cut` é usada para categorizar os
# valores de `proficiencia_saeb` com base nos limites definidos e atribuir
# os rótulos correspondentes aos níveis.
# `breaks` define os limites de corte, `labels` define os rótulos para cada
# categoria e `include.lowest = TRUE` garante que o limite inferior seja incluído
# na categoria correspondente.
# Agora, a base de dados terá uma coluna adicional chamada
# `classificacao_proficiencia` que contém as classificações correspondentes
# aos valores de proficiência.

## Indicador 5B

proficiencia_MT <- dadosSaeb |>
  dplyr::filter(id_regiao %in% 1) |>
  dplyr::filter(disciplina %in% "MT") |>
  dplyr::filter(presenca %in% 1) |>
  dplyr::group_by(ano, id_municipio, sigla_uf, localizacao,area,
                  disciplina, proficiencia_saeb) |>
  dplyr::count(id_municipio) |>
  dplyr::select(ano, id_municipio, sigla_uf, localizacao,area,
                disciplina, proficiencia_saeb) |>
  dplyr::mutate(classificacao_proficiencia = cut(proficiencia_saeb,
                                                 breaks = c(-Inf, limites_proficiencia, Inf),
                                                 labels = rotulos_niveis,
                                                 include.lowest = TRUE)) |>
  dplyr::mutate(joinTab = stringr::str_c(ano, id_municipio, sep = "_"))

# Aqui utilizamos a mesma metodologia, porém com o filtro aplicado para as
# notas de Matemática.

## Salvando a base da Meta 5

meta5 <- dplyr::left_join(proficiencia_LP,proficiencia_MT,by = "joinTab") |>
  dplyr::select(-ano.y, -id_municipio.y, -sigla_uf.y, -localizacao.y,
                -area.y,-joinTab,-disciplina.x,-disciplina.y)|>
  dplyr::rename(ano = ano.x, id_municipio = id_municipio.x, uf = sigla_uf.x,
                localizacao = localizacao.x, area = area.x,
                proficiencia_saeb_LP = proficiencia_saeb.x,
                proficiencia_saeb_MT = proficiencia_saeb.y,
                classificacao_proficiencia_LP = classificacao_proficiencia.x,
                classificacao_proficiencia_MT = classificacao_proficiencia.y) |>
  dplyr::mutate(id_municipio = as.numeric(id_municipio))

readr::write_rds(meta5, "data/Meta5.rds")

## OBSERVAÇÃO IMPORTANTE!!!

# Os códigos dos municípios presentes na base de dados do Saeb possuem valores
# diferentes dos códigos municípios presentes na base do IBGE, portanto não é
# possível juntar a coluna que contém o nome dos municípios na base da meta 5.

# Precisa-se de uma solução para este problema!!!!!

# Ideia para junção dos nomes dos municípios juntamente aos seus códigos -------

# Tratando os nomes das variáveis da base: Código dos Municípios
# codMunicipios <- codMunicipios |> dplyr::filter(Nome_Regiao %in% "Norte") |>
#  janitor::clean_names() |>
#  dplyr::rename(id_municipio = codigo_municipio_completo) |>
#  dplyr::select(nome_municipio,id_municipio) |>
#  dplyr::mutate(id_municipio = as.numeric(id_municipio))

# Código para juntar o código dos municípios com o nome dos mesmos
# meta5b <= merge(meta5a, codMunicipios, by = "id_municipio", all.x = TRUE)
