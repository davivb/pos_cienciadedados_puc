### Manipulando a base de dados

library(tidyverse)
library(here)
library(rio)
library(deflateBR)

source("2.Script/1.ajuste_bases.R")
bd <- bd %>% filter(uf != "Total")
# Transformando , em .
bd$`Massa Salarial` <- as.numeric(sub(",", ".", sub(".", "", bd$`Massa Salarial`, fixed=TRUE), fixed=TRUE))
bd$salario <- as.numeric(sub(",", ".", sub(".", "", bd$salario, fixed=TRUE), fixed=TRUE))
bd$ano <- as.numeric(bd$ano)
# Transformando x em 0
bd$rlv <- ifelse(bd$rlv == "X", 0, bd$rlv)
bd$rlv <- as.numeric(bd$rlv)


## Deflacionando a série
deflat <- import(here("1.dados/add.xlsx")) %>% rename(ano = Ano)
bd <- bd %>% left_join(deflat)
bd$IPADI_2019 <- 789.5617
bd$IPCA_2019 <- 5213.613

rm(deflat)

bd$rlv_def <- ((bd$rlv * bd$IPADI_2019) / bd$`IPA DI`)
bd$mass_sal_def <- ((bd$`Massa Salarial` * bd$IPCA_2019) / bd$IPCA)
bd$salario_def <- ((bd$salario * bd$IPCA_2019) / bd$IPCA)

## Criando os indicadores
# 2.1 Média de empregos por empresa (tam_empr): 
bd$tam_empr <- bd$empresa / bd$empresa

# Participação relativa
# Dividir o valor do emprego da UF daquele setor, naquele ano, pela soma dos empregos de todas as UFs naquele setor, naquele ano. 
er <- bd %>% group_by(uf, ano, cod_cnaediv) %>%
  summarise(er = emprego / sum(empresa))

bd <- bd %>% left_join(er, by = c("uf", "ano", "cod_cnaediv"))
rm(er)


## Quociente locacional 
# Emprego da UF daquele setor, naquele ano dividido pelo emprego total da UF naquele ano (tabela add, planilha complemento). 
add <- import(here("1.dados/add.xlsx"), sheet = 2)
bd <- bd %>% left_join(add)
bd <- bd %>% filter(uf != "Total")

rm(add)

# Forma calculo 1
ql <- bd %>% group_by(uf, ano, cod_cnaediv) %>%
  summarise(ql = (emprego / emprego_total) / sum(emprego) / emprego_total)

bd <- bd %>% left_join(ql, by = c("uf", "ano", "cod_cnaediv"))
rm(ql)

# Forma calculo 2
bd$ql2 <- ((bd$emprego / bd$empresa) / (sum(bd$emprego) / bd$emprego_total))

#2.4 Indice Hirschman Herfindahl modificado (hhm) 

hhm <- bd %>% group_by(uf, ano, cod_cnaediv) %>%
  summarise(hhm = (emprego / sum(emprego)) - (emprego_total / sum(emprego_total)))

bd <- bd %>% left_join(hhm, by = c("uf", "ano", "cod_cnaediv"))
rm(hhm)

## Padronizar
bd$empresa_norm <- scale(bd$empresa, center = T)
bd$emprego_norm <- scale(bd$emprego, center = T)
bd$rlv_def_norm <- scale(bd$rlv_def, center = T)
bd$mass_sal_def_norm <- scale(bd$mass_sal_def, center = T)
bd$salario_def_norm <- scale(bd$salario_def, center = T)
bd$tam_empr_norm <- scale(bd$tam_empr, center = T)
bd$er_norm <- scale(bd$er, center = T)
bd$emprego_total_norm <- scale(bd$emprego_total, center = T)
bd$ql_norm <- scale(bd$ql, center = T)
bd$ql2_norm <- scale(bd$ql2, center = T)
bd$hhm_norm <- scale(bd$hhm, center = T)

export(bd, here("3.Outputs/2.base_dados_final.xlsx"), overwrite = T)

## Resumindo a base
summary(bd$emprego)

