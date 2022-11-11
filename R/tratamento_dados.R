###--- Script de tratamento da base do CCP ----
# Início: 07/11/2022

library(tidyverse)
library(data.table)


# leitura da base

ccp_orig <- read.csv("./dados/ccpcnc_v4/ccpcnc/ccpcnc_v4.csv")

setDT(ccp_orig)


### Checagens ----

## Checagem de cowcode e country

# Para o cowcode 340, há três nomes de país diferentes, conforme informação abaixo.
# Nos demais casos, cada cowcode tem apenas um nome correspondente.
# Para a ordenação, será considerada a variável cowcode e year
ccp_orig |>
  select(cowcode, country) |>
  group_by(cowcode, country) |>
  count() |>
  ungroup() |>
  group_by(cowcode) |>
  mutate(n_cowcode = n()) |>
  filter(n_cowcode>1)

## Checagem de cowcode e year
# Há apenas um caso de cowcode com duplicidade de ano: cowcode 340, ano 2006
ccp_orig |>
  select(cowcode, year) |>
  group_by(cowcode, year) |>
  count() |>
  filter(n>1)
# As duas constituições do cowcode 340 (Serbia) em 2006 são constituições diferentes.
# Portanto, ambas serão mantidas na base de análise.


## Alteração da ordem

ccp_b_ordenada <- ccp_orig |>
  arrange(cowcode, year)

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


## Checagens sistemas constitucionais

# Checagem de consistência entre variáveis syst e systid -> teoricamente não pode
# haver mais de uma linha com syst==1 dentro de um mesmo systid


chec_syst <- ccp_orig[, .(cowcode,
                          country,
                          year,
                          syst,
                          systid,
                          c_inforce,
                          coding_available)]


chec_syst[, var_chec_syst := sum(syst), by = systid]

# sistemas constitucionais com inconsistência entre syst e systid:
incons_syst <- unique(chec_syst[!is.na(systid) & var_chec_syst ==0 | var_chec_syst>1],
       by= c("country","systid"))

#incons_syst |>
#  group_by(var_chec_syst) |>
#  count()
# temos 96 casos de inconsistência entre syst e systid, sendo 95 casos sem cód. 1 em syst
# e 1 caso (em Malta) com dois syst==1 em duas linhas diferentes.
# Portanto, para padronizar a variável syst, será necessário corrigi-la.


rm(chec_syst)



### Checagem de filtros iniciais ----

## Criação da base de trabalho:

ccp_trab <- ccp_orig


# Para decisão se os casos de C_inforce =0 serão mantidos -> não há resultados nas variáveis dependentes para os casos de C_inforce =0. Portanto, serão retirados
# frequência cruzada com c_inforce e HOUSENUM

#ccp_trab |>
#  filter(syst==1) |>
#  group_by(c_inforce, housenum) |>
#  count()

# frequência cruzada com c_inforce e AMNDAMAJ

#ccp_trab |>
#  group_by(syst, c_inforce, amndamaj) |>
#  count()

# frequência cruzada com c_inforce e INTERP_1

#ccp_trab |>
#  group_by(syst, c_inforce, interp_1) |>
#  count()


### CRIAÇÃO DE VARIÁVEIS AUXILIARES ----


## id único
ccp_trab <- ccp_trab |>
  mutate(uniqueid = 1:nrow(ccp_trab))

## Correção da VARIÁVEL SYST - identificação de um novo sistema constitucional
# variável corrigida será a "syst_co"

prim <- ccp_trab[!is.na(systid), .SD[1], by = systid] |>
  mutate(syst_co = 1) |>
  select(uniqueid, syst_co)


ccp_trab <- ccp_trab |>
  left_join(select(incons_syst, systid, var_chec_syst),
            by = "systid") |>
  left_join(prim, by = "uniqueid")

# ccriando base para checagem da syst_co - CHECAGEM OK
#chec_syst_co <- ccp_trab |>
#  select(cowcode,
#         country,
#         year,
#         syst,
#         syst_co,
#         systid,
#         c_inforce,
#         coding_available) |>
#  filter(!is.na(syst) & syst != syst_co)

# checando as diferenças entre syst e syst_co são exatamente os problemas
# que foram encontrados anteriormente
#dplyr::setdiff(select(chec_syst_co, -syst_co), select(incons_syst, -var_chec_syst)) # diferença na Serbia 2006 - são de fato 2 sistemas em 2006
#dplyr::setdiff(select(incons_syst, -var_chec_syst), select(chec_syst_co, -syst_co)) # diferença é Malta- ok


## identificação do primeiro sistema constitucional de cada país (variável primeiroSyst)
aux <- ccp_trab |>
  select(cowcode, country, year, syst_co, systid, c_inforce,coding_available, uniqueid) |>
  mutate(primeiroSyst = 1, ultimo_ano_syst=1)



# Para a criação de "primeiroSyst", são retiradas os casos de país-ano sem constituição
# vigente (c_inforce!=1) e CASOS COM coding_available=0 (em que a codificação não foi feita)
# mesmo quando há imputação em coding_imputed=1. Para identificar path dependence,
# a imputação de informações a partir de constituições próximas afeta diretamente
# o resultado, por isso imputações foram desconsideradas.
base_primeiroSyst <- aux[c_inforce == 1 & coding_available==1, .SD[1], by= c("country")] |>
  select(uniqueid, primeiroSyst)

ccp_trab <- ccp_trab |>
  left_join(base_primeiroSyst, by = "uniqueid") |>
  mutate(primeiroSyst = case_when(
    is.na(primeiroSyst) ~ 0,
    TRUE ~ primeiroSyst
  ))


# criação da variável que identifica o último ano de cada sistema constitucional codificado
# (variável ultimo_ano_syst).

base_ultimo_ano_syst <- aux[c_inforce==1 & coding_available==1, .SD[.N], by= "systid"] |>
  select(uniqueid, ultimo_ano_syst)

ccp_trab <- ccp_trab |>
  left_join(base_ultimo_ano_syst, by = "uniqueid") |>
  mutate(ultimo_ano_syst = case_when(
    is.na(ultimo_ano_syst) ~ 0,
    TRUE ~ ultimo_ano_syst
  ))

# criação da identificação dos casos que serão trabalhados (variável validos)
# Serão trabalhados apenas os países-ano em que há novo sistema constitucional, e que, ao mesmo tempo,
# havia sistema constitucional anterior descrito E CODIFICADO no banco

ccp_trab <- ccp_trab |>
  mutate(validos = case_when(
    syst_co==1 & primeiroSyst == 0 & coding_available==1 ~ 1,
    TRUE ~ 0
  ))



# Ver o resultado da criação da primeiroSyst e validos

view(ccp_trab |>
       select(cowcode,
              country,
              year,
              syst,
              syst_co,
              systid,
              c_inforce,
              coding_available,
              uniqueid,
              primeiroSyst,
              ultimo_ano_syst,
              validos
              ))

##OBS: na variável "validos" estão marcadas todas as constituições que têm ao menos uma constituição
# anterior codificada no banco. A constituição anterior não necessariamente será a imediatamente
# anterior, pois pode haver casos de constituições imediatamente anteriores que não foram codificadas.
# exemplo: No Afeganistão, a constituição anterior à de 1979 (systid==825) é a de
# 1977 (systid==823), pois a constituição de 1978 (systid==824) não foi codificada.


###- CRIAÇÃO DE VARIÁVEIS DEPENDENTES ----

## Bicameralismo

# Códigos 96 nas variáveis legisl e housenum serão consideradas como 0 na variável dummy "bicameralismo",
# concofme comentários presentes abaixo

#ccp_trab |>
#  select(country, year,legisl, legisl_article, legisl_comments, validos) |>
#  filter(legisl == 96 & validos ==1)

#ccp_trab |>
#  select(country, year, housenum, housenum_article, housenum_comments, validos) |>
#  filter(housenum == 96 & validos ==1)

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


### Frequências das variáveis dependentes ----

ccp_trab |>
  filter(validos==1) |>
  group_by(bicameralismo) |>
  count()


ccp_trab |>
  filter(validos==1) |>
  group_by(emend_dificil) |>
  count()

ccp_trab |>
  filter(validos==1) |>
  group_by(controle_const) |>
  count()

### CRIAÇÃO DAS VARIÁVEIS INDEPENDENTES ----


# bicameralismo, emend_dificil e controle_const na constituição codificada anterior

base_aux_independentes <- ccp_trab |>
  select(country, year, systid)


### VOLTAR AQUI PARA CORRIGIR CÓDIGO ABAIXO

ccp_trab[ultimo_ano_syst==1 | validos==1,
         bicameralismo_anterior := ifelse(validos==1,
                                          shift(bicameralismo,1),
                                          99),
         by = country]

## Ao criar a variável "bicameralismo_anterior", identifiquei que MALTA apresentava
#uma inconsistência no banco. Um mesmo systid (433) constava com duas linhas diferentes
# marcadas com syst==1.



# Ver o resultado da criação das variáveis

view(ccp_trab |>
       select(country,
              year,
              syst,
              systid,
              systyear,
              c_inforce,
              coding_available,
              primeiroSyst,
              ultimo_ano_syst,
              validos,
              bicameralismo,
              bicameralismo_anterior
       ))
