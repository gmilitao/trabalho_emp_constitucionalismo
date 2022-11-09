###--- Script de tratamento da base do CCP ----
# Início: 07/11/2022

library(tidyverse)
library(data.table)


# leitura da base

ccp_orig <- read.csv("./dados/ccpcnc_v4/ccpcnc/ccpcnc_v4.csv")

setDT(ccp_orig)


### Checagem de ordem ----

# ordem ok

#ccp_b_ordenada <- ccp_orig |>
#  arrange(country, year)

#chec_ccp_or <- ccp_orig

#ccp_b_ordenada <- ccp_b_ordenada |>
#  mutate(id = 1:nrow(ccp_b_ordenada))

#chec_ccp_or <- chec_ccp_or |>
#  mutate(id = 1:nrow(chec_ccp_or)) |>
#  select(cowcode, country, year, id) |>
#  left_join(ccp_b_ordenada |> select(cowcode, country, year, id),
#            by = (c("cowcode", "country", "year"))) |>
#  mutate(checagem = case_when(
#    id.x == id.y ~ "ok",
#    id.x != id.y ~ "ordem diferente"
#  ))

#chec_ccp_or |>
#  group_by(checagem) |>
#  count()


#chec_ccp_or |>
#  filter(is.na(id.y))

#rm(ccp_b_ordenada, chec_ccp_or)


### Checagem de filtros iniciais ----

## Criação da base de trabalho:

ccp_trab <- ccp_orig


# Para decisão se os casos de C_inforce =0 serão mantidos -> não há resultados nas variáveis dependentes para os casos de C_inforce =0. Portanto, serão retirados
# frequência cruzada com c_inforce e HOUSENUM

ccp_trab |>
  filter(syst==1) |>
  group_by(c_inforce, housenum) |>
  count()

# frequência cruzada com c_inforce e AMNDAMAJ

ccp_trab |>
  group_by(syst, c_inforce, amndamaj) |>
  count()

# frequência cruzada com c_inforce e INTERP_1

ccp_trab |>
  group_by(syst, c_inforce, interp_1) |>
  count()


### Criação de variáveis ----


# id único
ccp_trab <- ccp_trab |>
  mutate(uniqueid = 1:nrow(ccp_trab))

# identificação do primeiro sistema constitucional de cada país (variável primeiroSyst)
aux <- ccp_trab |>
  select(cowcode, country, year, syst, systid, c_inforce, uniqueid) |>
  mutate(primeiroSyst = 1)

base_primeiroSyst <- aux[c_inforce == 1, .SD[1], by= c("country")] |>
  select(uniqueid, primeiroSyst)

ccp_trab <- ccp_trab |>
  left_join(base_primeiroSyst, by = "uniqueid") |>
  mutate(primeiroSyst = replace_na(ccp_trab$primeiroSyst, 0))


# criação da identificação dos casos que serão trabalhados (variável validos)
# Serão trabalhados apenas os países-ano em que há novo sistema constitucional, e que, ao mesmo tempo,
# havia sistema constitucional anterior descrito no banco

ccp_trab <- ccp_trab |>
  mutate(validos = case_when(
    syst==1 & primeiroSyst == 0 ~ 1,
    TRUE ~ 0
  ))



# Ver o resultado da criação da primeiroSyst e validos

view(ccp_trab |>
       select(cowcode,
              country,
              year,
              syst,
              systid,
              c_inforce,
              uniqueid,
              primeiroSyst,
              validos
              ))


###- CRIANDO VARIÁVEIS DEPENDENTES ----

## Bicameralismo

# Códigos 96 nas variáveis legisl e housenum serão consideradas como 0 na variável dummy "bicameralismo",
# concofme comentários presentes abaixo

ccp_trab |>
  select(country, year,legisl, legisl_article, legisl_comments, validos) |>
  filter(legisl == 96 & validos ==1)

ccp_trab |>
  select(country, year, housenum, housenum_article, housenum_comments, validos) |>
  filter(housenum == 96 & validos ==1)

# criação da variável bicameralismo
ccp_trab <- ccp_trab |>
  mutate(bicameralismo = case_when(
    housenum == 3 ~ 1,
    TRUE ~ 0
  ))


# Rigidez da constituição (variável emend_dificil)
#  Considerando maioria de dois terços ou mais difícil (incluindo const. não emendáveis)

ccp_trab <- ccp_trab |>
  mutate(emend_dificil = case_when(
    amndapct == 3 | amndapct == 4 | amndapct == 5 ~ 1,
    TRUE ~ 0
  ))

# Judicial Review (variável controle_const)

ccp_trab <- ccp_trab |>
  mutate(controle_const = case_when(
    interp_1 == 1 | interp_2 == 1 | interp_3 == 1 | interp_4==1 ~ 1,
    TRUE ~ 0
  ))


### Casos Estranhos ----

# ANALISAR MELHOR 265 CASOS COM NA em amend e interp
# tem a ver com as variáveis coding_available e coding_imputed

# PARA A FINALIDADE DO TRABALHO, NÃO PODEREI USAR OS CASOS COM coding_available=0
# mesmo quando há imputação em coding_imputed=1. Para identificar path dependence,
# a imputação de informações a partir de constituições próximas afeta diretamente
# o resultado

ccp_trab |>
  filter(validos==1) |>
  group_by(coding_available, coding_imputed, bicameralismo) |>
  count()

ccp_trab |>
  filter(validos==1) |>
  group_by(coding_available, coding_imputed, emend_dificil) |>
  count()

ccp_trab |>
  filter(validos==1) |>
  group_by(coding_available, coding_imputed, controle_const) |>
  count()


# filtrando casos de C_INFORCE=0


#
