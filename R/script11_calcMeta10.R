# Meta 10: Oferecer, no mínimo, 25% das Matrículas de Educação de Jovens e
# Adultos, nos Ensinos Fundamental e Médio, na Forma Integrada à Educação
# Profissional

# Indicador 10A: Percentual de matrículas da educação de jovens e adultos
# na forma integrada à educação profissional

# Carregando a base de dados das matrículas tratadas no script 11.1
matriculaNorte <- readr::read_rds("data/matricula1320_Att.rds")

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

# Substituindo NA's por 0 para que não seja gerado um erro genérico ao
# tentar gerar gráficos para o dashboard
matriculaNorte[is.na(matriculaNorte)] <- 0

## Indicador 10A -----------------------------------------------------

# A etapa de ensino diz respeito aos códigos abaixo:
# Educação infantil (1, 2)
# Anos iniciais do ensino fundamental (4, 5, 6, 7, 14,15, 16, 17, 18)
# Anos finais do ensino fundamental (8, 9, 10, 11, 19, 20, 21, 41)
# Ensino médio (25, 26, 27,28, 29, 30, 31, 32, 33, 34, 36, 37, 38, 39, 40, 68)
# Educação de jovens e adultos (65, 67, 69, 70, 71, 73, 74)

# Calculando o numerador
numeradorIndicador10a <- matriculaNorte |>
  dplyr::filter(co_uf %in% "11":"17") |> # Alunos da região norte
  dplyr::filter(tp_etapa_ensino %in% c("60":"63","65","67","73","74")) |>
  # Aqui filtramos os alunos que pertencem à educação de jovens e adultos de
  # nível fundamental e médio integrada à educação profissional
  dplyr::group_by(ano, codigo_municipio) |>
  # Aqui agrupamos as matrículas por ano e município
  dplyr::count(codigo_municipio) |> dplyr::rename(qtdeMatEjaProf = n) |>
  # Aqui contamos as matrículas
  dplyr::mutate(joinTab = stringr::str_c(ano, codigo_municipio,sep = "_"))
# Aqui criamos uma variável chave para que seja possível juntar essa base
# à outras

# Calculando o denominador
denominadorIndicador10a <- matriculaNorte |>
  dplyr::filter(co_uf %in% "11":"17") |> # Alunos da região norte
  dplyr::filter(tp_etapa_ensino %in% c("60":"63","65","67","73","74","43":"48","69":"71")) |>
  # Aqui filtramos os alunos que pertencem à educação de jovens e adultos de
  # nível fundamental e médio
  dplyr::group_by(ano, codigo_municipio) |>
  # Aqui agrupamos as matrículas por ano e município
  dplyr::count(codigo_municipio) |> dplyr::rename(qtdeMatEja = n) |>
  # Aqui contamos as matrículas
  dplyr::mutate(joinTab = stringr::str_c(ano, codigo_municipio,sep = "_"))
# Aqui criamos uma variável chave para que seja possível juntar essa base
# à outras

# Juntando as bases acima, organizando e calculando o indicador 6A
baseIndicador10a <- dplyr::full_join(numeradorIndicador10a,denominadorIndicador10a,
                                    by = "joinTab") |>
  dplyr::mutate(
    qtdeMatEjaProf = ifelse(is.na(qtdeMatEjaProf), 0,qtdeMatEjaProf),
    ano.x = ifelse(row_number() >= 464, ano.y, ano.x),
    codigo_municipio.x =
      ifelse(row_number() >= 464, codigo_municipio.y, codigo_municipio.x)) |>
  dplyr::rename(ano = ano.x,codigo_municipio = codigo_municipio.x) |>
  dplyr::select(-ano.y,-codigo_municipio.y,-joinTab) |>
  dplyr::full_join(codMunicipios,baseIndicador10a,by="codigo_municipio") |>
  dplyr::filter(nome_regiao == "Norte") |>
  dplyr::relocate("nome_municipio",.after = "codigo_municipio") |>
  dplyr::relocate("nome_uf",.after = "nome_municipio") |>
  dplyr::mutate(indicador10A = qtdeMatEjaProf/qtdeMatEja) |>
  dplyr::select(-nome_regiao)

# Salvando a base geral da meta 10
readr::write_rds(baseIndicador10a, "data/Meta10.rds")
