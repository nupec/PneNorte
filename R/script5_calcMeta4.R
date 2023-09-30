
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

# Carregando a base de dados populacionais ---------------------------------
populacaoComDeficiencia <- readRDS("~/GitHub/PneNorte/data/populacaoComDeficiencia.rds")

# Agrupando as matriculas --------------------------------------------------

## Indicador 4A

# Filtrando o numerador



## Indicador 4B

# Filtrando o numerador



## Indicador 4C

# Filtrando o numerador

# Agrupando as bases de matrículas e da população e calculando os indicadores

## Indicador 4A



## Indicador 4B



## Indicador 4C



