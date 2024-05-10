# Packages ====
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, read.dbc, janitor, geobr, lme4, scales,
               ggbreak, readxl, survminer, R0, gghighlight, brpop)

# Referência https://doi.org/10.1590/0102-311XPT261921
# Fonte dos DBCs: https://datasus.saude.gov.br/transferencia-de-arquivos
# Fonte do GeoSES: https://journals.plos.org/plosone/article/file?type=supplementary&id=10.1371/journal.pone.0232074.s003

# Databases ====
## Raw .dbc files ====
deng14 <- read.dbc("DENGBR14.dbc")
deng15 <- read.dbc("DENGBR15.dbc")
deng16 <- read.dbc("DENGBR16.dbc")
deng17 <- read.dbc("DENGBR17.dbc")
deng18 <- read.dbc("DENGBR18.dbc")
deng19 <- read.dbc("DENGBR19.dbc")
deng20 <- read.dbc("DENGBR20.dbc")
deng21 <- read.dbc("DENGBR21.dbc")
deng22 <- read.dbc("DENGBR22.dbc")
deng23 <- read.dbc("DENGBR23.dbc")

## Merging bases with equivalent lengths and removing originals ====
deng14a20 <- rbind(deng14,deng15,deng16,deng17,deng18,deng19,deng20)|>
  janitor::clean_names()
rm(deng14,deng15,deng16,deng17,deng18,deng19,deng20)

deng21a23 <- rbind(deng21,deng22,deng23)|>
  janitor::clean_names()|>
  select(-dt_digita, -migrado_w)
rm(deng21,deng22,deng23)

## Filters ====
### Dengue without warning signs (10), with warning signs (11) and with severity signs (12) ====
deng14a20 <- deng14a20|>
  filter(classi_fin %in% c(10, 11, 12))

deng21a23 <- deng21a23|>
  filter(classi_fin %in% c(10, 11, 12))

### Laboratory confirmed cases ====
deng14a20 <- deng14a20|>
  filter(criterio == 1)

deng21a23 <- deng21a23|>
  filter(criterio == 1)

# Now the ACTUALLY confirmed
labvars <- c("resul_prnt", "resul_soro", "resul_ns1", "resul_vi_n",
             "resul_pcr", "histopa_n", "imunoh_n")

deng14a20 <- deng14a20|>
  filter(!if_all(labvars, .fns = ~ is.na(.)|. == 4))

deng21a23 <- deng21a23|>
  filter(!if_all(labvars, .fns = ~ is.na(.)|. == 4))
rm(labvars)

## Compatibilizing, merging, selecting, reordering and mutating variables ====
deng14a20 <- deng14a20|>
  mutate(dt_nasc = str_sub(dt_nasc, start = 1, end = 4))|>
  rename(ano_nasc = dt_nasc)

dng_tot <- rbind(deng14a20,deng21a23)

dng_tot <- dng_tot|>
  dplyr::select(tp_not, id_agravo, dt_notific, sg_uf_not, id_municip, dt_sin_pri,
                nu_idade_n, ano_nasc, cs_sexo, cs_gestant, cs_raca, cs_escol_n,
                sg_uf, id_mn_resi, id_pais, dt_invest, id_ocupa_n, febre, mialgia,
                cefaleia, exantema, vomito, nausea, dor_costas, conjuntvit, artrite,
                artralgia, petequia_n, leucopenia, laco, dor_retro, diabetes,
                hematolog, hepatopat, renal, hipertensa, acido_pept, auto_imune,
                dt_soro, resul_soro, dt_ns1, resul_ns1, dt_viral, resul_vi_n, dt_pcr,
                resul_pcr, sorotipo, histopa_n, imunoh_n, hospitaliz, dt_interna, uf,
                municipio, classi_fin, criterio, evolucao, dt_obito, dt_encerra,
                alrm_hipot, alrm_plaq, alrm_vom, alrm_sang, alrm_hemat, alrm_abdom,
                alrm_letar, alrm_hepat, alrm_liq, dt_alrm, grav_pulso, grav_conv,
                grav_ench, grav_insuf, grav_taqui, grav_extre, grav_hipot, grav_hemat,
                grav_melen, grav_metro, grav_sang, grav_ast, grav_mioc, grav_consc,
                grav_orgao, dt_grav, mani_hemor, epistaxe, gengivo, metro, petequias,
                hematura, sangram, laco_n, plasmatico, evidencia, plaq_menor, con_fhd,
                complica, tp_sistema)

dng_tot <- dng_tot|>
  mutate(tp_idade = str_sub(nu_idade_n, start = 1, end = 1),
         val_idade = as.numeric(str_sub(nu_idade_n, start = 2, end = 4)),
         idade_anos = case_when(
           tp_idade == 4 ~ val_idade,
           tp_idade == 3 ~ val_idade/12,
           tp_idade == 2 ~ val_idade/365,
           tp_idade == 1 ~ val_idade/(365*24),
           TRUE ~ NA
         ))|>
  dplyr::select(-tp_idade, -val_idade)

start_date <- as.Date("2014-01-01")
end_date <- as.Date("2023-12-31")
n_breaks <- 4

dng_tot <- dng_tot|>
  mutate(across(starts_with("dt"),
                .fns = ~ as.Date(.)))|>
  filter(dt_sin_pri >= start_date & dt_sin_pri <= end_date)

## Adding the geo_ses variable to the dataframe ====
geo_ses <- read_excel("pone.0232074.s003.xls")|>
  janitor::clean_names()|>
  mutate(faixa_geoses = cut(geo_ses,
                            breaks = seq(-1, 1, length.out = n_breaks+1),
                            labels = str_to_upper(letters[n_breaks:1])),
         across(c(munic_code6, munic_code7), .fns = ~ factor(.)))|>
  dplyr::select(munic_code6, munic_code7, geo_ses, faixa_geoses)
cities <- data.frame(
  code_muni = factor(geobr::read_municipality()$code_muni),
  name_muni = geobr::read_municipality()$name_muni,
  code_state = geobr::read_municipality()$code_state,
  abbrev_state = geobr::read_municipality()$abbrev_state)
geo_ses <- left_join(geo_ses, cities, by = join_by(munic_code7 == code_muni))
dng_tot <- left_join(dng_tot, geo_ses, by = join_by(id_mn_resi == munic_code6))
rm(cities, geo_ses)

# Mixed-effects modeling ====
## Known risk factors + GeoSES versus outcome ====
# dng_glmer1 <- dng_tot|>
#   filter(if_all(diabetes:auto_imune, #Known risk factors should not be empty
#                 ~ !is.na(.)),
#          evolucao %in% c(1,2), # 1 = Cure, 2 = death attributed to dengue
#          cs_sexo %in% c("M", "F"))|>
#   mutate(evolucao = if_else(evolucao == 1, 1, 0),
#          across(diabetes:auto_imune,
#                 ~ if_else(.==1, 1, 0)),
#          cs_gestant = if_else(cs_gestant %in% c(1,2,3,4), 1, 0), # 1st, 2nd, 3rd trimester and unknown gestational age versus else
#          cs_raca = case_when(cs_raca == 1 ~ "B", #Whites
#                              cs_raca %in% c(2,3,4,5) ~ "NB", #Non-whites
#                              cs_raca == 9 ~ "I")) #Ignored race

# glmer1 <- lme4::glmer(evolucao ~ idade_anos + cs_sexo + cs_gestant + cs_raca +
#                         diabetes + hematolog + hepatopat + renal + hipertensa +
#                         acido_pept + auto_imune + (1|faixa_geoses),
#                       data = dng_glmer1,
#                       family = binomial)

# summary(glmer1)

# The call above took 15 hours to run and figure out I did not have enough RAM. Maybe a skip for now

## Risk factors + GeoSES versus outcome -- a different approach ====
dng_glmer2 <- dng_tot|>
  filter(if_all(diabetes:auto_imune, #Known risk factors should not be empty
                ~ !is.na(.)),
         evolucao %in% c(1,2), # 1 = Cure, 2 = death attributed to dengue
        cs_sexo %in% c("M", "F"))|>
  mutate(evolucao = if_else(evolucao == 1, 1, 0),
        across(diabetes:auto_imune,
               ~ if_else(.==1, 1, 0)),
         cs_gestant = if_else(cs_gestant %in% c(1,2,3,4), 1, 0), # 1st, 2nd, 3rd trimester and unknown gestational age versus else
         cs_raca = case_when(cs_raca == 1 ~ "B", #Whites
                             cs_raca %in% c(2,3,4,5) ~ "NB", #Non-whites
                             cs_raca == 9 ~ "I")) #Ignored race

keys <- dng_glmer2|>
  group_by(id_municip)|>
  summarise(n = n())|>
  filter(n > 1000)|>
  dplyr::select(id_municip)|>
  unlist() # Keys = cities where at least 1000 lab confirmed cases were notified throughout the study period

dng_glmer2 <- dng_glmer2|>
  filter(id_municip %in% keys)|>
  group_by(id_municip)|>
  slice_sample(n = 1000) # Randomly select 1000 cases from these cities to reduce our sample and model with a smaller RAM

glmer2 <- lme4::glmer(evolucao ~ idade_anos + cs_sexo + cs_gestant + cs_raca +
                        diabetes + hematolog + hepatopat + renal + hipertensa +
                        acido_pept + auto_imune + (1|faixa_geoses),
                      data = dng_glmer2,
                      family = binomial) # Hopefully this runs more smoothly

summary(glmer2)

## Risk factors versus WHO disease classification ====
# To be continued...