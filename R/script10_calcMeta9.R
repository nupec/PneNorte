# Meta 9: Elevar a taxa de alfabetização da população com 15 anos ou mais para
# 93,5% até 2015 e, até o final da vigência deste PNE, erradicar o analfabetismo
# absoluto e reduzir em 50% a taxa de analfabetismo funcional.

# Indicador 9A: Taxa de alfabetização da população de 15 anos ou mais de idade

# Indicador 9B: Taxa de analfabetismo funcional da população de 15 anos ou mais
# de idade

## Carregando pacotes necessários para o cálculo dos indicadores
library(basedosdados)
library(tidyverse)

## Carregando as base de dados necessárias para o cálculo dos indicadores

# Nesta primeira etapa vamos carregar os dados da pnad-c através do pacote
# "basedosdados", além de carregar os pacotes necessários para o tratamento
# e cálculo dos indicadores da meta 9

# Defina o seu projeto no Google Cloud
set_billing_id("indicadores-educacionais-ods4")

# Para carregar o dado direto no R
query <- bdplyr("br_ibge_pnadc.microdados")
df <- bd_collect(query)

# OBS: Essa base é muito grande para ser carregada através deste pacote,
# Vamos tentar fazer o download manualmente pelo seguinte endereço:
# https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/LEIA-ME.pdf

# Os arquivos estão em formato .txt, vamos ler com os seguintes códigos
dadosPnadc2012 = read.table(file = "data-raw/dadosPnadc/PNADC_2012_visita1.txt",header = F,sep = ",")

