### Ajuste bases de dados ### 
library(tidyverse)
library(here)
library(rio)
library(data.table)
library(janitor)
## Lendo as bases de dados


## Bd empresas
bd_emp <- import(here("1.dados/consulta23857154.csv"), encoding = "UTF-8")
bd_emp <- bd_emp %>% clean_names()
bd_emp$uf2 <- rep(1:28,each = 14)

bd_emp$uf2 <- factor(bd_emp$uf2, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
                                            17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28),
                     labels = c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", 
                                "Tocantins", "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", 
                                "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia", "Minas Gerais", 
                                "Espírito Santo", "Rio de Janeiro", "São Paulo", "Paraná", 
                                "Santa Catarina", "Rio Grande do Sul", "Mato Grosso do Sul", 
                                "Mato Grosso", "Goiás", "Distrito Federal", "Total"))





bd_emp$uf <- bd_emp$uf2 
bd_emp <- bd_emp %>% select(-uf2) %>% filter(ano != "Total")
bd_emp <- bd_emp %>% pivot_longer(!c(uf, ano), names_to = "nom_cnaediv", values_to = "empresa")
bd_emp <- bd_emp %>% filter(nom_cnaediv != "total")
bd_emp$cod_cnaediv <- rep(10:33, times =  (nrow(bd_emp) / 24))
bd_emp <- bd_emp %>% select(uf, ano, nom_cnaediv, cod_cnaediv, empresa)


## Bd emprego
bd_emprego <- import(here("1.dados/consulta27826759.csv"), encoding = "UTF-8")
bd_emprego <- bd_emprego %>% clean_names()
bd_emprego$uf2 <- rep(1:28,each = 14)

bd_emprego$uf2 <- factor(bd_emprego$uf2, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
                                            17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28),
                     labels = c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", 
                                "Tocantins", "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", 
                                "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia", "Minas Gerais", 
                                "Espírito Santo", "Rio de Janeiro", "São Paulo", "Paraná", 
                                "Santa Catarina", "Rio Grande do Sul", "Mato Grosso do Sul", 
                                "Mato Grosso", "Goiás", "Distrito Federal", "Total"))


bd_emprego$uf <- bd_emprego$uf2 
bd_emprego <- bd_emprego %>% select(-uf2) %>% filter(ano != "Total")
bd_emprego <- bd_emprego %>% pivot_longer(!c(uf, ano), names_to = "nom_cnaediv", values_to = "emprego")
bd_emprego <- bd_emprego %>% filter(nom_cnaediv != "total")
bd_emprego$cod_cnaediv <- rep(10:33, times =  (nrow(bd_emprego) / 24))
bd_emprego <- bd_emprego %>% select(uf, ano, nom_cnaediv, cod_cnaediv, emprego)

## Bd empresas
bd_massa <- import(here("1.dados/consulta83024330.csv"), encoding = "UTF-8")
bd_massa <- bd_massa %>% row_to_names(row_number = 1)
bd_massa <- bd_massa %>% clean_names()
bd_massa$uf2 <- rep(1:28,each = 14)

bd_massa$uf2 <- factor(bd_massa$uf2, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
                                            17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28),
                     labels = c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", 
                                "Tocantins", "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", 
                                "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia", "Minas Gerais", 
                                "Espírito Santo", "Rio de Janeiro", "São Paulo", "Paraná", 
                                "Santa Catarina", "Rio Grande do Sul", "Mato Grosso do Sul", 
                                "Mato Grosso", "Goiás", "Distrito Federal", "Total"))


bd_massa$uf <- bd_massa$uf2 
bd_massa <- bd_massa %>% select(-uf2) %>% filter(ano != "Total")
bd_massa <- bd_massa %>% pivot_longer(!c(uf, ano), names_to = "nom_cnaediv", values_to = "Massa Salarial")
bd_massa <- bd_massa %>% filter(nom_cnaediv != "total")
bd_massa$cod_cnaediv <- rep(10:33, times =  (nrow(bd_massa) / 24))
bd_massa <- bd_massa %>% select(uf, ano, nom_cnaediv, cod_cnaediv, `Massa Salarial`)


## Bd Salario mediko
bd_salario <- import(here("1.dados/consulta94810383.csv"), encoding = "UTF-8")
bd_salario <- bd_salario %>% row_to_names(row_number = 1)
bd_salario <- bd_salario %>% clean_names()
bd_salario$uf2 <- rep(1:28,each = 14)

bd_salario$uf2 <- factor(bd_salario$uf2, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
                                            17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28),
                     labels = c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", 
                                "Tocantins", "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", 
                                "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia", "Minas Gerais", 
                                "Espírito Santo", "Rio de Janeiro", "São Paulo", "Paraná", 
                                "Santa Catarina", "Rio Grande do Sul", "Mato Grosso do Sul", 
                                "Mato Grosso", "Goiás", "Distrito Federal", "Total"))


bd_salario$uf <- bd_salario$uf2 
bd_salario <- bd_salario %>% select(-uf2) %>% filter(ano != "Total")
bd_salario <- bd_salario %>% pivot_longer(!c(uf, ano), names_to = "nom_cnaediv", values_to = "salario")
bd_salario <- bd_salario %>% filter(nom_cnaediv != "total")
bd_salario$cod_cnaediv <- rep(10:33, times =  (nrow(bd_salario) / 24))
bd_salario <- bd_salario %>% select(uf, ano, nom_cnaediv, cod_cnaediv, salario)


### Consolidando os bancos de dados

bd <- bd_emp %>% left_join(bd_emprego, by = c("uf", "ano", "cod_cnaediv")) %>%
  left_join(bd_massa, by = c("uf", "ano", "cod_cnaediv")) %>% left_join(bd_salario, by = c("uf", "ano", "cod_cnaediv"))

bd <- bd %>% select("uf", "ano", "nom_cnaediv.x", "cod_cnaediv",  "empresa", "emprego",  "Massa Salarial", "salario")

## Lendo a ultima base 

## Receita
bd_receita <- import(here("1.dados/Tabela 1849.xlsx"),sheet = 2)
bd_receita$cod_cnaediv <- as.numeric(bd_receita$cod_cnaediv)
bd <- bd %>% left_join(bd_receita, by = c("uf", "ano", "cod_cnaediv"))
bd <- bd %>% select("uf", "ano", "nom_cnaediv.x","nom_cnaediv", "cod_cnaediv", "empresa", "emprego", 
                    "Massa Salarial", "salario",  "rlv")

rm(bd_emp, bd_emprego, bd_massa, bd_salario, bd_receita)

## Exportando a base de dados
export(bd, "3.Outputs/1.base_dados_consolidada.xlsx", overwrite = T)

