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

# Dimensões (nº de linhas e colunas)
dim(dados)

# Verificando tipos de dados
str(dados)

# Sumário estatístico
summary(dados)

# Selecione e converte apenas as variáveis do tipo caractere para tipo factor
colunas_chr <- sapply(dados, is.character)
dados <- mutate_if(dados, colunas_chr, as.factor)



# Pergunta 1

# - O Aumento do PIB per capita de um país afeta positivamente a expectativa de vida dos cidadãos ao nascer?
#   Qual a correlação entre as duas variáveis ?

# Análise Exploratória Específica
dados_1 <- dados %>% 
  select(Codigo,Pais,Ano,Indice_de_Felicidade,PIB_Log,Expectativa_Saudavel_de_Vida_ao_Nascer)

colSums(is.na(dados_1))
dados_1 <- na.omit(dados_1)
dados_1$Ano <- as.factor(dados_1$Ano)

# Sumário estatístico
summary(dados_1)


# Verificando a Correlação de Pearson entre duas variáveis
cor(dados_1$PIB_Log, dados_1$Expectativa_Saudavel_de_Vida_ao_Nascer, method = "pearson")  # 0.8331619

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


