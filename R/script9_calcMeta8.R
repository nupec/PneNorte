# Meta 8: OFERECER EDUCAÇÃO EM TEMPO INTEGRAL EM, NO MÍNIMO,
# 50% (CINQUENTA POR CENTO) DAS ESCOLAS PÚBLICAS, DE FORMA A
# ATENDER, PELO MENOS, 25%(VINTE E CINCO POR CENTO) DOS (AS)
# ALUNOS (AS) DA EDUCAÇÃO BÁSICA.

# Indicador 8A: PERCENTUAL DE ALUNOS DA EDUCAÇÃO BÁSICA
# PÚBLICA QUE PERTENCEM AO PÚBLICO-ALVO DA EDUCAÇÃO EM TEMPO
# INTEGRAL (ETI) E QUE ESTÃO EM JORNADA DE TEMPO INTEGRAL

# Indicador 8B: PERCENTUAL DE ESCOLAS PÚBLICAS DA
# EDUCAÇÃO BÁSICA QUE POSSUEM, PELO MENOS, 25% DOS ALUNOS
# DO PÚBLICO-ALVO DA ETI EM JORNADA DE TEMPO INTEGRAL.

## Empilhando as bases do Censo Escolar ----------------------------------------------------

## Neste scrip, importa-se as bases do Censo Escolar que serão úteis nas
## pesquisas futuras

## As varáveis dos anos 2013 a 2014 possuem nomes diferentes, por isso a decisão
## importar separadamente.

## Nesta etapa, eu seleciono as variáveis equivantes nas duas base de dados
variaveis_selecionadas_1314 <- c("ANO_CENSO",
                                 "NUM_IDADE_REFERENCIA",
                                 "ID_DEPENDENCIA_ADM_ESC",
                                 "FK_COD_ETAPA_ENSINO",
                                 "FK_COD_ESTADO_ESCOLA",
                                 "COD_MUNICIPIO_ESCOLA",
                                 "FK_COD_MOD_ENSINO",
                                 "ID_POSSUI_NEC_ESPECIAL")

variaveis_selecionadas_1520 <- c("NU_ANO_CENSO",
                                 "NU_IDADE_REFERENCIA",
                                 "TP_DEPENDENCIA",
                                 "TP_ETAPA_ENSINO",
                                 "CO_UF",
                                 "CO_MUNICIPIO",
                                 "IN_ESPECIAL_EXCLUSIVA",
                                 "IN_NECESSIDADE_ESPECIAL")

## Nesta etapa, eu aponto os caminhos das bases de dados
arq_matriculas_no_1314 <-list.files("data-raw/2013_2014/",
                                    full.names = T)

arq_matriculas_no_1520 <-list.files("data-raw/2015_2020/",
                                    full.names = T)

## Aqui, eu importo as Matrículas dos alunos do Norte do Brasil
## Eu opto pelo pacote função "fread" do pacote data.table, pela possibilidade
## de importar apenas as variáveis de interesse.

matriculas_no_1314 <- purrr::map_dfr(arq_matriculas_no_1314,
                                     data.table::fread,
                                     select = (variaveis_selecionadas_1314))


matriculas_no_1520 <- purrr::map_dfr(arq_matriculas_no_1520,
                                     data.table::fread,
                                     select = (variaveis_selecionadas_1520))

## Como as bases tem nomes diferentes nas tabelas, eu preferi usar a função colnames
## ao invés do mutate (que ainda não tenho muita experiência)

colnames(matriculas_no_1314) <- names(matriculas_no_1520)

matriculas_no_1320 <- dplyr::bind_rows(matriculas_no_1314,
                                       matriculas_no_1520)

readr::write_rds(matriculas_no_1320, file = "data/matricula1320.rds")
