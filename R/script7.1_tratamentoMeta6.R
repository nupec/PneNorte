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

## CARREGANDO AS BASES DE DADOS NECESSÁRIAS PARA O CÁLCULO DA META 06

## Neste scrip, importa-se as bases do Censo Escolar que serão utilizadas.

## Esse processo foi feito no script 0.2, porém algumas colunas importantes
## para o cálculo da Meta 06 ficaram de fora da base.

## As varáveis de 2014 possuem nomes diferentes, por isso a decisão
## importar separadamente.

## Nesta etapa, eu seleciono as variáveis equivantes nas duas base de dados
variaveis_selecionadas_14 <- c("ANO_CENSO",
                                 "PK_COD_MATRICULA",
                                 "NUM_IDADE_REFERENCIA",
                                 "ID_DEPENDENCIA_ADM_ESC",
                                 "FK_COD_ETAPA_ENSINO",
                                 "FK_COD_ESTADO_ESCOLA",
                                 "COD_MUNICIPIO_ESCOLA",
                                 "FK_COD_MOD_ENSINO",
                                 "ID_POSSUI_NEC_ESPECIAL",
                                 "NU_DUR_ESCOLARIZACAO",
                                 "NU_DUR_ATIV_COMP_MESMA_REDE",
                                 "NU_DUR_ATIV_COMP_OUTRAS_REDES",
                                 "NUM_DUR_AEE_MESMA_REDE",
                                 "NUM_DUR_AEE_OUTRAS_REDES",
                                 "PK_COD_ENTIDADE"
                                 )

variaveis_selecionadas_1520 <- c("NU_ANO_CENSO",
                                 "ID_MATRICULA",
                                 "NU_IDADE_REFERENCIA",
                                 "TP_DEPENDENCIA",
                                 "TP_ETAPA_ENSINO",
                                 "CO_UF",
                                 "CO_MUNICIPIO",
                                 "IN_ESPECIAL_EXCLUSIVA",
                                 "IN_NECESSIDADE_ESPECIAL",
                                 "NU_DURACAO_TURMA",
                                 "NU_DUR_ATIV_COMP_MESMA_REDE",
                                 "NU_DUR_ATIV_COMP_OUTRAS_REDES",
                                 "NU_DUR_AEE_MESMA_REDE",
                                 "NU_DUR_AEE_OUTRAS_REDES",
                                 "CO_ENTIDADE",
                                 "TP_MEDIACAO_DIDATICO_PEDAGO"
                                 )

## Nesta etapa, eu aponto os caminhos das bases de dados
arq_matriculas_no_14 <-list.files("data-raw/2014/",
                                    full.names = T)

arq_matriculas_no_1520 <-list.files("data-raw/2015_2020/",
                                    full.names = T)

## Aqui, eu importo as Matrículas dos alunos do Norte do Brasil
## Eu opto pelo pacote função "fread" do pacote data.table, pela possibilidade
## de importar apenas as variáveis de interesse.

matriculas_no_14 <- purrr::map_dfr(arq_matriculas_no_14,
                                     data.table::fread,
                                     select = (variaveis_selecionadas_14))


matriculas_no_1520 <- purrr::map_dfr(arq_matriculas_no_1520,
                                     data.table::fread,
                                     select = (variaveis_selecionadas_1520))

## Em 2014, cabe ressalvar que apenas as matrículas de educação presencial
## eram coletadas. Então, vamos adicionar uma coluna de 1's para identificar
## a maneira de ensino de maneira presencial.

# Carregando o dplyr
library(dplyr)

# Adicionando a coluna de 1's
matriculas_no_14 <- matriculas_no_14 %>%
  mutate(TP_MEDIACAO_DIDATICO_PEDAGO = 1)

## Como as bases tem nomes diferentes nas tabelas, eu preferi usar a função colnames
## ao invés do mutate (que ainda não tenho muita experiência)

colnames(matriculas_no_14) <- names(matriculas_no_1520)

# Juntando as duas bases
matriculas_no_1420 <- dplyr::bind_rows(matriculas_no_14,
                                       matriculas_no_1520)

# Salvando a base geral
readr::write_rds(matriculas_no_1420, file = "data/matricula1420_Att.rds")
