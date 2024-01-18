####  Projeto  ####

# Configurando o diretório de trabalho
setwd("~/Desktop/DataScience/CienciaDeDados/1.Big-Data-Analytics-com-R-e-Microsoft-Azure-Machine-Learning/17.Mini-Projeto-1_-_Analise_Exploratoria_de_Dados_Socioeconomicos")
getwd()


## Carregando pacotes
library(tidyverse) # manipulação de dados
library(dplyr)     # manipulação de dados
library(corrplot)  # criar gráfico de mapa de correlação
library(ggplot2)   # criar outros gráficos (especificamente de dispersão)
library(caret)     # usado em tarefas de classificação e regressão para simplificar o processo de treinamento de modelos
library(openxlsx)


## Perguntas de Negócio

# Pergunta 1

# - O Aumento do PIB per capita de um país afeta positivamente a expectativa de vida dos cidadãos ao nascer?
#   Qual a correlação entre as duas variáveis ?

# Pergunta 2

# - Existe uma correlação entre a escala de vida e a conscientização do público em geral sobre a corrupção
#   nos negócios e no governo ? Qual a correlação entre essas duas variáveis ?

# Pergunta 3

# - O aumento na escala de vida tem algum efeito na média de felicidade entre o público em geral ?
#   Qual a relação entre essas duas variáveis ?

# Pergunta 4

# - O país com o menor índice de suporte social tem maior percepção de corrupção em relação às empresas e
#   ao governo no páis ?

# Pergunta 5

# - Pessoas generosas são mais felizes ?



## Carregando dados

dados <- read.xlsx("online-data-chapter-2-whr-2017.xlsx")
head(dados)
# View(dados)
  

## Análise Exploratória Geral dos Dados

# Renomeando Colunas
dados <- dados %>%
  rename(
    Codigo = WP5.Country,
    Pais = country,
    Ano = year,
    Indice_de_Felicidade = Life.Ladder,
    PIB_Log = Log.GDP.per.capita,
    Suporte_Social = Social.support,
    Expectativa_Saudavel_de_Vida_ao_Nascer = Healthy.life.expectancy.at.birth,
    Liberdade_para_Escolhas = Freedom.to.make.life.choices,
    Generosidade = Generosity,
    Percepcao_de_Corrupcao = Perceptions.of.corruption,
    Afeto_Positivo = Positive.affect,
    Afeto_Negativo = Negative.affect,
    Confianca_no_Governo = Confidence.in.national.government,
    Qualidade_Democratica = Democratic.Quality,
    Qualidade_Servicos_Publicos = Delivery.Quality,
    Desvio_Padrao_Indice_Felicidade_por_Pais_e_Ano = `Standard.deviation.of.ladder.by.country-year`,
    Razao_entre_Desvio_Padrao_e_Media_Do_Indice_de_Felicidade_por_Pais_e_Ano = `Standard.deviation/Mean.of.ladder.by.country-year`,
    Indice_GINI_World_Bank = `GINI.index.(World.Bank.estimate)`,
    Media_do_Indice_GINI_2000_a_2013 = `GINI.index.(World.Bank.estimate),.average.2000-13`,
    Indice_GINI_de_Renda_Domiciliar_por_Ano = `gini.of.household.income.reported.in.Gallup,.by.wp5-year`
  )
head(dados)

# verificando valores ausentes nas colunas
colSums(is.na(dados))

# Remover todas as linhas com dados NA na coluna PIB_Log
colSums(is.na(dados))
dados <- dados[complete.cases(dados$PIB_Log), ]

# Dimensões (nº de linhas e colunas)
dim(dados)

# Verificando tipos de dados
str(dados)

# Sumário estatístico
summary(dados)

# Selecione e converte apenas as variáveis do tipo caractere para tipo factor
colunas_chr <- sapply(dados, is.character)
dados <- mutate_if(dados, colunas_chr, as.factor)




## Pergunta 1

# - O Aumento do PIB per capita de um país afeta positivamente a expectativa de vida dos cidadãos ao nascer?
#   Qual a correlação entre as duas variáveis ?

# Análise Exploratória Específica
dados_1 <- dados %>% 
  select(Codigo,Pais,Ano,Indice_de_Felicidade,PIB_Log,Expectativa_Saudavel_de_Vida_ao_Nascer)

colSums(is.na(dados_1))
dados_1 <- na.omit(dados_1)
dados_1$Ano <- as.factor(dados_1$Ano)
summary(dados_1)


# Verificando a Correlação de Pearson entre duas variáveis
cor(dados_1$PIB_Log, dados_1$Expectativa_Saudavel_de_Vida_ao_Nascer, method = "pearson")  # 0.8331619
cor.test(dados_1$PIB_Log, dados_1$Expectativa_Saudavel_de_Vida_ao_Nascer, method = "pearson")

# - O valor de 0.83 está próximo a 1 o que indica uma forte correlação entre as duas variáveis.


# Criar gráfico de dispersão
ggplot(dados_1, aes(x = PIB_Log, y = Expectativa_Saudavel_de_Vida_ao_Nascer)) +
  geom_point() +
  labs(title = "Correlação entre PIB per capita e Expectativa de Vida",
       x = "PIB per capita (log)",
       y = "Expectativa de Vida ao Nascer") +
  theme_minimal()

# Criar gráfico de dispersão aprimorado
ggplot(dados_1, aes(x = PIB_Log, y = Expectativa_Saudavel_de_Vida_ao_Nascer)) +
  geom_point(color = "#4C72B0", size = 3, alpha = 0.7) +  # Define cor, tamanho e transparência dos pontos
  labs(title = "Correlação entre PIB per capita e Expectativa de Vida",
       x = "PIB per capita (log)",
       y = "Expectativa de Vida ao Nascer") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centraliza o título e ajusta a fonte
        axis.text = element_text(size = 12),  # Ajusta o tamanho do texto nos eixos
        axis.title = element_text(size = 14, face = "bold"))  # Ajusta o tamanho e a fonte dos títulos dos eixos

# Criar gráfico de dispersão com linha de correlação
ggplot(dados_1, aes(x = PIB_Log, y = Expectativa_Saudavel_de_Vida_ao_Nascer)) +
  geom_point(color = "#4C72B0", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#DD8452", linewidth = 1.5) +  # Ajustado para "linewidth"
  labs(title = "Correlação entre PIB per capita e Expectativa de Vida",
       x = "PIB per capita (log)",
       y = "Expectativa de Vida ao Nascer") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))




# Pergunta 2

# - Existe uma correlação entre a escala de vida e a conscientização do público em geral sobre a corrupção
#   nos negócios e no governo ? Qual a correlação entre essas duas variáveis ?

# Análise Exploratória Específica
dados_2 <- dados %>% 
  select(Codigo,Pais,Ano,Indice_de_Felicidade,Percepcao_de_Corrupcao)

colSums(is.na(dados_2))
dados_2 <- na.omit(dados_2)
dados_2$Ano <- as.factor(dados_2$Ano)

# Sumário estatístico
summary(dados_2)


# Verificando a Correlação de Pearson entre duas variáveis (Indice_de_Felicidade e Percepcao_de_Corrupcao)
cor(dados_2$Indice_de_Felicidade, dados_2$Percepcao_de_Corrupcao, method = "pearson")  # -0.4378447

# - O valor de -0.43 indica uma correlação negativa moderada entre as duas variáveis.
#   Isso significa que, em geral, à medida que o índice de felicidade aumenta, a percepção de corrupção tende a diminuir, e vice-versa.


# Criar gráfico de dispersão
ggplot(dados_2, aes(x = Indice_de_Felicidade, y = Percepcao_de_Corrupcao)) +
  geom_point() +
  labs(title = "Correlação entre Índice de Felicidade e Percepção de Corrupção",
       x = "Índice de Felicidade",
       y = "Percepção de Corrupção") +
  theme_minimal()

# Criar gráfico de dispersão aprimorado
ggplot(dados_2, aes(x = Indice_de_Felicidade, y = Percepcao_de_Corrupcao)) +
  geom_point(color = "#4C72B0", size = 3, alpha = 0.7) +
  labs(title = "Correlação entre Índice de Felicidade e Percepção de Corrupção",
       x = "Índice de Felicidade",
       y = "Percepção de Corrupção") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

# Criar gráfico de dispersão com linha de correlação
ggplot(dados_2, aes(x = Indice_de_Felicidade, y = Percepcao_de_Corrupcao)) +
  geom_point(color = "#4C72B0", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#DD8452", linewidth = 1.5) +
  labs(title = "Correlação entre Índice de Felicidade e Percepção de Corrupção",
       x = "Índice de Felicidade",
       y = "Percepção de Corrupção") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))




# Pergunta 3

# - O aumento na escala de vida tem algum efeito na média de felicidade entre o público em geral ?
#   Qual a relação entre essas duas variáveis ?

# Análise Exploratória Específica
dados_3 <- dados %>% 
  select(Codigo,Pais,Ano,Indice_de_Felicidade,Expectativa_Saudavel_de_Vida_ao_Nascer)

colSums(is.na(dados_3))
dados_3 <- na.omit(dados_3)
dados_3$Ano <- as.factor(dados_3$Ano)

# Sumário estatístico
summary(dados_3)

# Verificando a Correlação de Pearson entre duas variáveis (Indice_de_Felicidade e Percepcao_de_Corrupcao)
cor(dados_3$Indice_de_Felicidade, dados_3$Expectativa_Saudavel_de_Vida_ao_Nascer, method = "pearson")  # 0.724329

# - O valor de correlação de aproximadamente 0.724 indica uma correlação positiva forte entre essas duas variáveis.


# Criar gráfico de dispersão
ggplot(dados_3, aes(x = Expectativa_Saudavel_de_Vida_ao_Nascer, y = Indice_de_Felicidade)) +
  geom_point() +
  labs(title = "Correlação entre Expectativa de Vida e Índice de Felicidade",
       x = "Expectativa de Vida ao Nascer",
       y = "Índice de Felicidade") +
  theme_minimal()

# Criar gráfico de dispersão aprimorado
ggplot(dados_3, aes(x = Expectativa_Saudavel_de_Vida_ao_Nascer, y = Indice_de_Felicidade)) +
  geom_point(color = "#4C72B0", size = 3, alpha = 0.7) +
  labs(title = "Correlação entre Expectativa de Vida e Índice de Felicidade",
       x = "Expectativa de Vida ao Nascer",
       y = "Índice de Felicidade") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

# Criar gráfico de dispersão com linha de correlação
ggplot(dados_3, aes(x = Expectativa_Saudavel_de_Vida_ao_Nascer, y = Indice_de_Felicidade)) +
  geom_point(color = "#4C72B0", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#DD8452", linewidth = 1.5) +
  labs(title = "Correlação entre Expectativa de Vida e Índice de Felicidade",
       x = "Expectativa de Vida ao Nascer",
       y = "Índice de Felicidade") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))



# Pergunta 4

# - O país com o menor índice de suporte social tem maior percepção de corrupção em relação às empresas e
#   ao governo no páis ?

# Encontrar o índice do país com o menor índice de suporte social
indice_menor_suporte_social <- which.min(dados$Suporte_Social)

# Extrair informações sobre o país com o menor índice de suporte social
pais_menor_suporte_social <- dados[indice_menor_suporte_social, ]
pais_menor_suporte_social

# Extrair a percepção de corrupção desse país
percepcao_corrupcao_menor_suporte <- pais_menor_suporte_social$Percepcao_de_Corrupcao
percepcao_corrupcao_menor_suporte  # 0.859073

# Obter a média da percepção de corrupção de todos os países
media_perc_corrupcao <- mean(dados$Percepcao_de_Corrupcao, na.rm = TRUE)
media_perc_corrupcao  # 0.7556783

# Exibir a comparação
if (percepcao_corrupcao_menor_suporte > media_perc_corrupcao) {
  cat("O país com o menor índice de suporte social tem uma percepção de corrupção maior do que a média.")
} else {
  cat("O país com o menor índice de suporte social não tem uma percepção de corrupção maior do que a média.")
}


# Pergunta 5

# - Pessoas generosas são mais felizes ?

# Análise Exploratória Específica
dados_5 <- dados %>% 
  select(Pais, Indice_de_Felicidade, Generosidade)
head(dados_5)

colSums(is.na(dados_5))
dados_5 <- na.omit(dados_5)

# Verificando a Correlação de Pearson entre duas variáveis (Indice_de_Felicidade e Percepcao_de_Corrupcao)
cor(dados_5$Indice_de_Felicidade, dados_5$Generosidade, method = "pearson")  # 0.2247181

# - O valor de correlação de aproximadamente 0.23 indica uma correlação fraca entre essas duas variáveis.


# Criar gráfico de dispersão com linha de correlação
ggplot(dados_5, aes(x = Generosidade, y = Indice_de_Felicidade)) +
  geom_point(color = "#4C72B0", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#DD8452", linewidth = 1.5) +
  labs(title = "Correlação entre Generosidade e Índice de Felicidade",
       x = "Generosidade",
       y = "Índice de Felicidade") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))








#########################################           GABARITO           #########################################

#  -> Foi utilizado o dataset disponibilizado no curso!!!

## Carregando e Visualizando os dados
dados <- read.csv("dataset.csv")

dim(dados)
str(dados)
summary(dados)



##### Análise Exploratória (geral)

## Verificando complete_cases

# Quantas linhas tem todas os valores sem nenhum NA  
complete_cases <- sum(complete.cases(dados))
complete_cases                                              # (1708 linhas

# Quantas linhas NÃO tem todos os valores sem nenhum NA 
not_complete_cases <- sum(!complete.cases(dados))
not_complete_cases                                          # (241 linhas)

# Percentual de dados incompletos
percentual <- (not_complete_cases / complete_cases) * 100
percentual
rm(complete_cases)                                          # remove objeto
rm(not_complete_cases)                                      # remove objeto


## Renomeando colunas

# Verificando nome das colunas
colnames(dados)

# Grava os nomes das colunas em um vetor
myColumns <- colnames(dados)
myColumns                            

# Renomeando as colunas
myColumns[1] <- "NomePais"
myColumns[2] <- "Ano"
myColumns[3] <- "IndicadorNivelVida"
myColumns[4] <- "PIB_Per_Capita"
myColumns[5] <- "SuporteSocial"
myColumns[6] <- "ExpectativaVida"
myColumns[7] <- "IndicadorLiberdade"
myColumns[8] <- "IndicadorGenerosidade"
myColumns[9] <- "IndicadorCorrupcao"
myColumns[10] <- "IndicadorEmocoesPositivas"
myColumns[11] <- "IndicadorEmocoesNegativas"

# Atribui os novos nomes de colunas ao dataframe
colnames(dados) <- myColumns
rm(myColumns)                   

head(dados)


## Tratando valores NA

# Verificando quantos países foram incluídos na coleta de dados
length(unique(dados$NomePais))

# Lista os países únicos e grava o resultado (antes de remover registros com valores NA)
list_countries_with_na <- unique(dados$NomePais)
list_countries_with_na
head(dados)

# Vamos eliminar linhas com valores NA
dados <- na.omit(dados)

# Dimensões
dim(dados)

# Lista de países após remover valores NA
list_of_countries_without_na <- unique(dados$NomePais)
list_of_countries_without_na

# Verificando se perdemos países ao remover valores NA
length(list_countries_with_na)
length(list_of_countries_without_na)

# Verificando a diferença antes e depois de remover valores NA (lista os países onde tinha uma linha com dados NA e foi removido)
setdiff(list_countries_with_na, list_of_countries_without_na)


## Tratando Variável Ano

# Verificando quais anos estão presentes nos dados
anos <- unique(dados$Ano)
range(anos)                 # menor e maior valor
length(unique(dados$Ano))
rm(anos)

# Número de registros por ano (total de valores em cada categoria)
table(dados$Ano)

# Vamos remover os anos com menor contribuição (menor volume de dados / todos com menos de 100 registros foram removidos)
dados_por_anos <- dados[dados$Ano!=2005 & dados$Ano!=2006 & dados$Ano!=2007 & dados$Ano!=2020,]

# Número de registros por ano
table(dados_por_anos$Ano)


## Matriz de Correlação

# Extraindo as variáveis numéricas
numeric_variable_list <- sapply(dados, is.numeric)
numerical_data <- dados[numeric_variable_list]

# Matriz de Correlação
cor(numerical_data)

# Correlation Plot 
pairs(numerical_data)                                                 # todas as variáveis de uma vez
pairs(numerical_data[1:5],labels = colnames(numerical_data)[1:5])     # exibe as 5 primeiras variaveis
pairs(numerical_data[6:10],labels = colnames(numerical_data)[6:10])   # exibe as variáveis restantes




##### Análise Exploratória dos Dados - Resposta às Perguntas de Negócio

## Parte 1 - Organização dos Dados

# Vamos realizar a análise considerando a média de indicadores por país.
# Calculamos as médias fazendo agrupamento por indicador e concatenamos os dataframes resultantes.

# - Para está análise não precisaremos usar a variável ano, por conta disse calcularemos a média dos fatores econômicos


# Visualiza os dados
head(dados)

# Nomes das colunas
colnames(dados)


## Agrupando os dados e calculando média por país (PIB_Per_Capita e SuporteSocial)
pib_per_capita_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(PIB_Per_Capita = mean(PIB_Per_Capita))

suporte_social_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(SuporteSocial = mean(SuporteSocial))

# Merge (Unindo os dataframes acima)
df_medias <- merge(pib_per_capita_pais_media, suporte_social_pais_media)
head(df_medias)

# Remova o que não estiver mais usando
rm(pib_per_capita_pais_media)
rm(suporte_social_pais_media)


# Agrupando os dados e calculando média por país (IndicadorNivelVida)
ind_nivel_vida_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorNivelVida = mean(IndicadorNivelVida))

# Merge (Unindo ao dataframe df_medias)
df_medias <- merge(df_medias, ind_nivel_vida_pais_media)
head(df_medias)
rm(ind_nivel_vida_pais_media)


# Agrupando os dados e calculando média por país (ExpectativaVida)
expectativa_vida_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(ExpectativaVida = mean(ExpectativaVida))

# Merge (Unindo ao dataframe df_medias)
df_medias <- merge(df_medias, expectativa_vida_pais_media)
head(df_medias)
rm(expectativa_vida_pais_media)


# Agrupando os dados e calculando média por país (IndicadorLiberdade)
ind_liberdade_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorLiberdade = mean(IndicadorLiberdade))

df_medias <- merge(df_medias, ind_liberdade_pais_media)
head(df_medias)
rm(ind_liberdade_pais_media)


# Agrupando os dados e calculando média por país (IndicadorGenerosidade)
ind_generosidade_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorGenerosidade = mean(IndicadorGenerosidade))

df_medias <- merge(df_medias, ind_generosidade_pais_media)
head(df_medias)
rm(ind_generosidade_pais_media)


# Agrupando os dados e calculando média por país (IndicadorCorrupcao)
ind_corrupcao_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorCorrupcao = mean(IndicadorCorrupcao))

df_medias <- merge(df_medias, ind_corrupcao_pais_media)
head(df_medias)
rm(ind_corrupcao_pais_media)


# Agrupando os dados e calculando média por país (IndicadorEmocoesPositivas)
ind_pos_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorEmocoesPositivas = mean(IndicadorEmocoesPositivas))

df_medias <- merge(df_medias, ind_pos_pais_media)
head(df_medias)
rm(ind_pos_pais_media)


# Agrupando os dados e calculando média por país (IndicadorEmocoesNegativas)
ind_neg_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorEmocoesNegativas = mean(IndicadorEmocoesNegativas))

df_medias <- merge(df_medias, ind_neg_pais_media)
head(df_medias)
rm(ind_neg_pais_media)


dim(df_medias)
str(df_medias)
summary(df_medias)




##### Parte 2 - Plots e Estatísticas 

# Pergunta 1
# O aumento do PIB per capita de um país afeta positivamente a expectativa de vida dos cidadãos ao nascer?
# Qual a correlação entre essas duas variáveis?

plot(df_medias$PIB_Per_Capita, df_medias$ExpectativaVida)
cor.test(df_medias$PIB_Per_Capita, df_medias$ExpectativaVida, method = "pearson")  # 0.8537768

# Criar gráfico de dispersão com linha de correlação
ggplot(df_medias, aes(x = PIB_Per_Capita, y = ExpectativaVida)) +
  geom_point(color = "#4C72B0", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#DD8452", linewidth = 1.5) +  # Ajustado para "linewidth"
  labs(title = "Correlação entre PIB per capita e Expectativa de Vida",
       x = "PIB per capita (log)",
       y = "Expectativa de Vida ao Nascer") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))


# -> O valor de aproximadamente 0.85 está próximo a 1 o que indica uma forte correlação entre as duas variáveis.



# Pergunta 2
# Existe uma correlação entre a escala de vida e a conscientização do público em geral sobre a corrupção 
# nos negócios e no governo? 
# Qual a correlação entre essas duas variáveis?

plot(df_medias$IndicadorNivelVida, df_medias$IndicadorCorrupcao)
cor.test(df_medias$IndicadorNivelVida, df_medias$IndicadorCorrupcao, method = "pearson")  # -0.4642275 

# Criar gráfico de dispersão com linha de correlação
ggplot(df_medias, aes(x = IndicadorNivelVida, y = IndicadorCorrupcao)) +
  geom_point(color = "#4C72B0", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#DD8452", linewidth = 1.5) +  # Ajustado para "linewidth"
  labs(title = "Correlação entre IndicadorNivelVida e IndicadorCorrupcao",
       x = "IndicadorNivelVida",
       y = "IndicadorCorrupcao") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

# -> 



# Pergunta 3
# O aumento na escala de vida tem algum efeito na média de felicidade entre o público em geral?
# Qual a correlação entre essas duas variáveis?

plot(df_medias$IndicadorNivelVida, df_medias$IndicadorEmocoesPositivas)
cor.test(df_medias$IndicadorNivelVida, df_medias$IndicadorEmocoesPositivas, method = "pearson")  # 0.5778006 

# Criar gráfico de dispersão com linha de correlação
ggplot(df_medias, aes(x = IndicadorNivelVida, y = IndicadorEmocoesPositivas)) +
  geom_point(color = "#4C72B0", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#DD8452", linewidth = 1.5) +  # Ajustado para "linewidth"
  labs(title = "Correlação entre IndicadorNivelVida e IndicadorEmocoesPositivas",
       x = "IndicadorNivelVida",
       y = "IndicadorEmocoesPositivas") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))





