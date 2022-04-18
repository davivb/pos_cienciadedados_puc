## Análise exploratória

library(tidyverse)
library(here)
library(ggplot2)
library(corrplot)
## Lendo o BD final

bd <- import(here("3.Outputs/3.base_de_dados_final.csv"))


# Analisando a estrutura do banco
str(bd)

## Estatísticas descritivas de todas as variaveis
summary(bd)

## Histograma 
hist(bd$empresa)


# Salario deflacionado
hist(bd$salario_def)

# Salario deflacionado normalizado
hist(bd$salario_def_norm)

# receitas deflacionado 
hist(bd$rlv_def)

# receitas deflacionado normalizado
hist(bd$rlv_def_norm)


## Plotando gráfico de dispersão
bd %>% ggplot( aes(x=emprego, y=salario_def_norm)) + 
  geom_point()

## Matriz de correlação
corrplot(cor(bd[5:26]), method = "circle")

## Selecionando apenas as variaveis de interesse
bd_cor <-  bd %>% select(rlv_def, empresa, emprego, salario_def, tam_empr, ql, pr)
corrplot(cor(bd_cor), method = "circle")


