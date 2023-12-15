# Meta 6: OFERECER EDUCAÇÃO EM TEMPO INTEGRAL EM, NO MÍNIMO,
# 50% (CINQUENTA POR CENTO) DAS ESCOLAS PÚBLICAS, DE FORMA A
# ATENDER, PELO MENOS, 25%(VINTE E CINCO POR CENTO) DOS (AS)
# ALUNOS (AS) DA EDUCAÇÃO BÁSICA.

# Indicador 6A: PERCENTUAL DE ALUNOS DA EDUCAÇÃO BÁSICA
# PÚBLICA QUE PERTENCEM AO PÚBLICO-ALVO DA EDUCAÇÃO EM TEMPO
# INTEGRAL (ETI) E QUE ESTÃO EM JORNADA DE TEMPO INTEGRAL

# Indicador 6B: PERCENTUAL DE ESCOLAS PÚBLICAS DA
# EDUCAÇÃO BÁSICA QUE POSSUEM, PELO MENOS, 25% DOS ALUNOS
# DO PÚBLICO-ALVO DA ETI EM JORNADA DE TEMPO INTEGRAL.

# Carregando a base de dados das matrículas tratadas no script 6.1
matriculaNorte <- readr::read_rds("data/matricula1420_Att.rds")

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

# Agrupando e filtrando as bases da população e calculando os indicadores

## Indicador 6A -----------------------------------------------------

# A etapa de ensino diz respeito aos códigos abaixo:
# Educação infantil (1, 2)
# Anos iniciais do ensino fundamental (4, 5, 6, 7, 14,15, 16, 17, 18)
# Anos finais do ensino fundamental (8, 9, 10, 11,19, 20, 21, 41)
# Ensino médio¹ (25, 26, 27,28, 29, 30, 31, 32, 33, 34, 36, 37, 38, 39, 40, 68)
# Educação de jovens e adultos (65, 67, 69, 70, 71, 73, 74)

# Calculando o numerador
numeradorIndicador6a <- matriculaNorte |>
  dplyr::filter(co_uf %in% "11":"17", # Alunos da região norte
                tp_dependencia %in% "1":"3", # Alunos de escola pública
                tp_mediacao_didatico_pedago == "1", # Alunos no ensino presencial
                tp_etapa_ensino %in% c("1":"2","4":"11","14":"21","25":"38","41"),
                # Aqui filtramos os alunos que não pertencem à educação de
                # jovens e adultos nem à educação profissional técnica de nível
                # médio oferecida na forma subsequente ou concomitante
                nu_duracao_turma > 0
                # Aqui filtramos as matrículas de escolarização com a informação
                # do tempo de duração da turma
                ) |>
  dplyr::mutate(carga_horaria = nu_duracao_turma + nu_dur_ativ_comp_mesma_rede +
                  nu_dur_ativ_comp_outras_redes + nu_dur_aee_mesma_rede +
                  nu_dur_aee_outras_redes) |>
  # Aqui criamos uma variável para somar as horas dos alunos
  dplyr::filter(carga_horaria > 419) |>
  # Aqui filtramos apenas os alunos em tempo integral
  dplyr::group_by(ano, codigo_municipio) |>
  # Aqui agrupamos as matrículas por ano e município
  dplyr::count(codigo_municipio) |> dplyr::rename(qtdeMatETI = n) |>
  # Aqui contamos as matrículas
  dplyr::mutate(joinTab = stringr::str_c(ano, codigo_municipio,sep = "_"))
  # Aqui criamos uma variável chave para que seja possível juntar essa base
  # à outras

# Calculando o denominador
denominadorIndicador6a <- matriculaNorte |>
  dplyr::filter(co_uf %in% "11":"17", # Alunos da região norte
                tp_dependencia %in% "1":"3", # Alunos de escola pública
                tp_mediacao_didatico_pedago == "1", # Alunos no ensino presencial
                tp_etapa_ensino %in% c("1":"2","4":"11","14":"21","25":"38","41"),
                # Aqui filtramos os alunos que não pertencem à educação de
                # jovens e adultos nem à educação profissional técnica de nível
                # médio oferecida na forma subsequente ou concomitante
                nu_duracao_turma > 0
                # Aqui filtramos as matrículas de escolarização com a informação
                # do tempo de duração da turma
  ) |>
  dplyr::mutate(carga_horaria = nu_duracao_turma + nu_dur_ativ_comp_mesma_rede +
                  nu_dur_ativ_comp_outras_redes + nu_dur_aee_mesma_rede +
                  nu_dur_aee_outras_redes) |>
  dplyr::group_by(ano, codigo_municipio) |>
  dplyr::count(codigo_municipio) |> dplyr::rename(qtdeMatETI = n) |>
  dplyr::mutate(joinTab = stringr::str_c(ano, codigo_municipio,sep = "_"))

# Juntando as bases acima, organizando e calculando o indicador 6A
baseIndicador6a <- dplyr::full_join(numeradorIndicador6a,denominadorIndicador6a,
                                    by = "joinTab") |>
  dplyr::rename(ano = ano.x,codigo_municipio = codigo_municipio.x,
                n_alunos_eti = qtdeMatETI.x, n_alunos_alvo_eti_total = qtdeMatETI.y) |>
  dplyr::select(-ano.y,-codigo_municipio.y,-joinTab) |>
  dplyr::full_join(codMunicipios,baseIndicador6a,by="codigo_municipio") |>
  dplyr::filter(nome_regiao == "Norte") |>
  dplyr::relocate("nome_municipio",.after = "codigo_municipio") |>
  dplyr::relocate("nome_uf",.after = "nome_municipio") |>
  dplyr::mutate(indicador6A = n_alunos_eti/n_alunos_alvo_eti_total) |>
  dplyr::mutate(joinTab = stringr::str_c(ano, codigo_municipio,sep = "_")) |>
  dplyr::select(-nome_regiao)

## Indicador 6B ----------------------------------------------

# Filtrando da base geral as características de interesse para o cálculo do
# Indicador 6B.

# Contando a quantidade de alunos do público-alvo da ETI em jornada de tempo
# integral no ano t
qtdeAlunosTempoIntETI <- matriculaNorte |>
  dplyr::filter(co_uf %in% "11":"17", # Alunos da região norte
                tp_dependencia %in% "1":"3", # Alunos de escola pública
                tp_mediacao_didatico_pedago == "1", # Alunos no ensino presencial
                tp_etapa_ensino %in% c("1":"2","4":"11","14":"21","25":"38","41"),
                # Aqui filtramos os alunos que não pertencem à educação de
                # jovens e adultos nem à educação profissional técnica de nível
                # médio oferecida na forma subsequente ou concomitante
                nu_duracao_turma > 0
                # Aqui filtramos as matrículas de escolarização com a informação
                # do tempo de duração da turma
  ) |>
  dplyr::mutate(carga_horaria = nu_duracao_turma + nu_dur_ativ_comp_mesma_rede +
                  nu_dur_ativ_comp_outras_redes + nu_dur_aee_mesma_rede +
                  nu_dur_aee_outras_redes) |>
  # Aqui criamos uma variável para somar as horas dos alunos
  dplyr::filter(carga_horaria > 419) |>
  # Aqui filtramos apenas os alunos em tempo integral
  dplyr::group_by(ano, co_entidade, codigo_municipio) |>
  # Aqui agrupamos as matrículas por ano, escola e município
  dplyr::count(co_entidade) |> dplyr::rename(qtdeMatTempIntETI = n) |>
  # Aqui contamos as matrículas dos alunos do público-alvo da ETI em jornada
  # de tempo integral em cada escola
  dplyr::mutate(joinTab = stringr::str_c(ano, co_entidade,sep = "_"))
  # Aqui criamos uma variável chave para que seja possível juntar essa base
  # à outras

# Contando a quantidade de alunos do público-alvo da ETI
qtdeAlunosETI <- matriculaNorte |>
  dplyr::filter(co_uf %in% "11":"17", # Alunos da região norte
                tp_dependencia %in% "1":"3", # Alunos de escola pública
                tp_mediacao_didatico_pedago == "1", # Alunos no ensino presencial
                tp_etapa_ensino %in% c("1":"2","4":"11","14":"21","25":"38","41"),
                # Aqui filtramos os alunos que não pertencem à educação de
                # jovens e adultos nem à educação profissional técnica de nível
                # médio oferecida na forma subsequente ou concomitante
                nu_duracao_turma > 0
                # Aqui filtramos as matrículas de escolarização com a informação
                # do tempo de duração da turma
  ) |>
  dplyr::group_by(ano, co_entidade, codigo_municipio) |>
  # Aqui agrupamos as matrículas por ano, escola e município
  dplyr::count(co_entidade) |> dplyr::rename(qtdeMatETI = n) |>
  # Aqui contamos as matrículas dos alunos do público-alvo da ETI por escola
  dplyr::mutate(joinTab = stringr::str_c(ano, co_entidade,sep = "_"))
# Aqui criamos uma variável chave para que seja possível juntar essa base
# à outras

# Vamos agrupar as bases acima e calcular o numerador do indicador 6b
numeradorIndicador6b = dplyr::full_join(qtdeAlunosTempoIntETI,qtdeAlunosETI,
                                        by = "joinTab")|>
  dplyr::rename(ano = ano.x,codigo_municipio = codigo_municipio.x,
                codigo_escola = co_entidade.x) |>
  dplyr::select(-ano.y,-codigo_municipio.y,-joinTab,-co_entidade.y) |>
  # Substituindo NA por zero na coluna "qtdeMatTempoIntETI"
  dplyr::mutate(
    qtdeMatTempIntETI = ifelse(is.na(qtdeMatTempIntETI), 0, qtdeMatTempIntETI)) |>
  dplyr::mutate(percentual_matTI_por_matETI = qtdeMatTempIntETI/qtdeMatETI) |>
  dplyr::filter(percentual_matTI_por_matETI >= 0.25) |>
  dplyr::group_by(ano, codigo_municipio) |>
  dplyr::count(codigo_municipio) |> dplyr::rename(qtdeEsc25pcTempIntETI = n) |>
  dplyr::mutate(joinTab = stringr::str_c(ano, codigo_municipio,sep = "_"))

# Agora vamos calcular o denominador
denominadorIndicador6b = qtdeAlunosETI |> dplyr::group_by(ano, codigo_municipio) |>
  dplyr::count(codigo_municipio) |> dplyr::rename(qtdeEscPublicoAlvoETI = n) |>
  dplyr::mutate(joinTab = stringr::str_c(ano, codigo_municipio,sep = "_"))

# Juntando as bases acima, organizando e calculando o indicador 6B
baseIndicador6b <- dplyr::full_join(numeradorIndicador6b,denominadorIndicador6b,
                                    by = "joinTab") |>
  dplyr::mutate(
    qtdeEsc25pcTempIntETI = ifelse(is.na(qtdeEsc25pcTempIntETI), 0,
                                   qtdeEsc25pcTempIntETI),
    ano.x = ifelse(row_number() >= 2202, ano.y, ano.x),
    codigo_municipio.x =
      ifelse(row_number() >= 2202, codigo_municipio.y, codigo_municipio.x)) |>
  dplyr::select(-ano.y, -codigo_municipio.y) |>
  dplyr::rename(ano = ano.x,codigo_municipio = codigo_municipio.x,) |>
  dplyr::full_join(codMunicipios,baseIndicador6b,by="codigo_municipio") |>
  dplyr::filter(nome_regiao == "Norte") |>
  dplyr::relocate("nome_municipio",.after = "codigo_municipio") |>
  dplyr::relocate("nome_uf",.after = "nome_municipio") |>
  dplyr::mutate(indicador6B = qtdeEsc25pcTempIntETI/qtdeEscPublicoAlvoETI) |>
  dplyr::relocate("joinTab",.after = "indicador6B") |> dplyr::select(-nome_regiao)

## Juntando as bases dos indicadores e salvando a base geral da meta 6
baseMeta6 <- dplyr::left_join(baseIndicador6a,baseIndicador6b,
                              by = "joinTab") |>
  dplyr::select(-ano.y, -codigo_municipio.y, - nome_municipio.y,
                -nome_uf.y,-joinTab)|>
  dplyr::rename(ano = ano.x, codigo_municipio = codigo_municipio.x,
                nome_municipio = nome_municipio.x,nome_uf = nome_uf.x)

# Salvando a base geral da meta 06
readr::write_rds(baseMeta6, "data/Meta6.rds")
