# Packages ====
if (!require("pacman")) install.packages("pacman")
pacman::p_load(read.dbc, tidyverse, janitor, readxl, geobr, scales, viridis,
               gtsummary, lme4, expss, gt, modelsummary)

# Reference https://doi.org/10.1590/0102-311XPT261921
# DBC source: https://datasus.saude.gov.br/transferencia-de-arquivos
# GeoSES source: https://journals.plos.org/plosone/article/file?type=supplementary&id=10.1371/journal.pone.0232074.s003

# Databases ====
## Raw .dbc files ====
dng14 <- read.dbc("DENGBR14.dbc")
dng15 <- read.dbc("DENGBR15.dbc")
dng16 <- read.dbc("DENGBR16.dbc")
dng17 <- read.dbc("DENGBR17.dbc")
dng18 <- read.dbc("DENGBR18.dbc")
dng19 <- read.dbc("DENGBR19.dbc")
dng20 <- read.dbc("DENGBR20.dbc")
dng21 <- read.dbc("DENGBR21.dbc")
dng22 <- read.dbc("DENGBR22.dbc")
dng23 <- read.dbc("DENGBR23.dbc")

## Merging bases with equivalent lengths and removing originals ====
dng14a20 <- rbind(dng14, dng15, dng16, dng17, dng18, dng19, dng20)|>
  janitor::clean_names()
rm(dng14, dng15, dng16, dng17, dng18, dng19, dng20)

dng21a23 <- rbind(dng21, dng22, dng23)|>
  janitor::clean_names()|>
  select(-dt_digita, -migrado_w)
rm(dng21, dng22, dng23)

## Filters ====
### Dengue without warning signs (10), with warning signs (11) and with severity signs (12) ====
dng14a20 <- dng14a20|>
  filter(classi_fin %in% c(10, 11, 12))

dng21a23 <- dng21a23|>
  filter(classi_fin %in% c(10, 11, 12))

### Laboratory confirmed cases ====
dng14a20 <- dng14a20|>
  filter(criterio == 1)

dng21a23 <- dng21a23|>
  filter(criterio == 1)

labvars <- c("resul_prnt", "resul_soro", "resul_ns1", "resul_vi_n",
             "resul_pcr", "histopa_n", "imunoh_n")

dng14a20 <- dng14a20|>
  filter(!if_all(labvars, .fns = ~ is.na(.)|. == 4))

dng21a23 <- dng21a23|>
  filter(!if_all(labvars, .fns = ~ is.na(.)|. == 4))
rm(labvars)

## Compatibilizing, merging, selecting, reordering and mutating variables ====
dng14a20 <- dng14a20|>
  mutate(dt_nasc = str_sub(dt_nasc, start = 1, end = 4))|>
  rename(ano_nasc = dt_nasc)

dng_tot <- rbind(dng14a20, dng21a23)

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
           TRUE ~ NA),
         cs_sexo = factor(cs_sexo, levels = c("F", "M")))|>
  dplyr::select(-tp_idade, -val_idade, -nu_idade_n)

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
  mutate(faixa_geoses = ordered(cut(geo_ses,
                                    breaks = seq(min(geo_ses),
                                                 max(geo_ses),
                                                 length.out = n_breaks+1),
                                    labels = str_to_upper(letters[n_breaks:1]))),
         across(c(munic_code6, munic_code7), .fns = ~ factor(.)))|>
  dplyr::select(munic_code6, munic_code7, geo_ses, faixa_geoses)
cities <- data.frame(
  code_muni = factor(geobr::read_municipality()$code_muni),
  name_muni = geobr::read_municipality()$name_muni,
  code_state = geobr::read_municipality()$code_state,
  abbrev_state = geobr::read_municipality()$abbrev_state)
geo_ses <- left_join(geo_ses, cities, by = join_by(munic_code7 == code_muni))
dng_tot <- left_join(dng_tot, geo_ses, by = join_by(id_mn_resi == munic_code6))

### Plotting GeoSES ====
geo_ses <- left_join(geo_ses, geobr::read_municipality()|>
                       select(code_muni, geom)|>
                       mutate(code_muni = as.factor(code_muni)), by = join_by(munic_code7 == code_muni))
map_absolute <- ggplot(geo_ses)+
  geom_sf(aes(fill = geo_ses,
              geometry = geom),
          color = "#00000000")+
  scale_fill_viridis(option = "inferno")+
  theme_void()+
  labs(fill = "GeoSES")+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5),
        legend.frame = element_rect(color = "black", linewidth = 1.2))

windowsFonts(OpenSans = windowsFont("Open Sans"))

png(filename = "idweek2024/map_absolute.png",
    width = 40, height = 50, units = 'cm', res = 600)
map_absolute+
  theme(legend.key.width = unit(2.5, "cm"),
        legend.key.height = unit(1.5, "cm"),
        text = element_text(size = 30, family = "OpenSans"))
dev.off()

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
  filter(n > 800)|>
  dplyr::select(id_municip)|>
  unlist() # Keys = cities where at least 800 confirmed cases were notified throughout the study period

dng_glmer2 <- dng_glmer2|>
  filter(id_municip %in% keys)|>
  group_by(id_municip)|>
  slice_sample(n = ceiling(250000/length(keys)))|> # Randomly select 460 cases from these cities to reduce our sample and model with a smaller RAM
  ungroup()

# load("C:/Users/glira/Documents/Análises, apresentações, textos em R/Dados públicos (AIH, SINAN)/Agravos de notificação/Dengue/.RData")

tbl1_1 <- dng_glmer2|>
  filter(classi_fin %in% c(10,11,12),
         cs_sexo %in% c("M", "F"))|>
  gtsummary::select(idade_anos, cs_sexo, cs_gestant, cs_raca,
                    diabetes:hipertensa)|>
  mutate(cs_sexo = factor(cs_sexo,
                          levels = c("F", "M"),
                          labels = c("Female", "Male")),
         cs_raca = factor(cs_raca,
                          levels = c("B", "NB", "I"),
                          labels = c("White", "Non-white", "Ignored")))|>
  expss::apply_labels(idade_anos = "Age", cs_sexo = "Biological sex", 
                      cs_gestant = "Pregnant", cs_raca = "Race",
                      diabetes = "Diabetes", hematolog = "Hematologic disease",
                      hepatopat = "Liver disease", renal = "Kidney disease",
                      hipertensa = "Hypertension") |> 
  tbl_summary(missing = "no") |> 
  modify_header(label ~ "")

gt::gtsave(as_gt(tbl1_1), filename = "idweek2024/table1_1.png")

tbl1_2 <- dng_glmer2|>
  filter(classi_fin %in% c(10,11,12),
         cs_sexo %in% c("M", "F"))|>
  gtsummary::select(acido_pept, auto_imune, faixa_geoses, classi_fin, evolucao)|>
  mutate(classi_fin = factor(classi_fin,
                             levels = c(10, 11, 12),
                             labels = c("Dengue without warning signs", "Dengue with warning signs", "Severe dengue")),
         evolucao = factor(evolucao,
                           levels = c(0,1),
                           labels = c("Death attributed to dengue", "Cured")),
         faixa_geoses = fct_rev(faixa_geoses))|>
  expss::apply_labels(acido_pept = "Peptic ulcer disease", auto_imune = "Autoimmune disease",
                      faixa_geoses = "GeoSES range", classi_fin = "Classification",
                      evolucao = "Outcome") |> 
  tbl_summary(missing = "no") |> 
  modify_header(label ~ "")

gt::gtsave(as_gt(tbl1_2), filename = "idweek2024/table1_2.png")

dng_glmer2|>
  gtsummary::select(idade_anos, cs_sexo, cs_gestant, cs_raca,
         diabetes:auto_imune, faixa_geoses, classi_fin)|>
  filter(classi_fin %in% c(10,11,12),
         cs_sexo %in% c("M", "F"))|>
  mutate(cs_sexo = factor(cs_sexo,
                          levels = c("F", "M"),
                          labels = c("Female", "Male")),
         cs_raca = factor(cs_raca,
                          levels = c("B", "NB", "I"),
                          labels = c("White", "Non-white", "Ignored")),
         classi_fin = factor(classi_fin,
                             levels = c(10, 11, 12),
                             labels = c("Dengue without warning signs", "Dengue with warning signs", "Severe dengue")),
         faixa_geoses = fct_rev(faixa_geoses))|>
  expss::apply_labels(idade_anos = "Age", cs_sexo = "Biological sex", 
                      cs_gestant = "Pregnant", cs_raca = "Race",
                      diabetes = "Diabetes", hematolog = "Hematologic disease",
                      hepatopat = "Liver disease", renal = "Kidney disease",
                      hipertensa = "Hypertension", acido_pept = "Peptic ulcer disease",
                      auto_imune = "Autoimmune disease", classi_fin = "Classification") |> 
  tbl_summary(by = faixa_geoses, missing = "no") |>
  modify_header(label ~ "") |>
  add_p()

dng_glmer2|>
  gtsummary::select(idade_anos, cs_sexo, cs_gestant, cs_raca,
                    diabetes:auto_imune, geo_ses, classi_fin)|>
  filter(classi_fin %in% c(10,11,12),
         cs_sexo %in% c("M", "F"))|>
  mutate(cs_sexo = factor(cs_sexo,
                          levels = c("F", "M"),
                          labels = c("Female", "Male")),
         cs_raca = factor(cs_raca,
                          levels = c("B", "NB", "I"),
                          labels = c("White", "Non-white", "Ignored")),
         classi_fin = factor(classi_fin,
                             levels = c(10, 11, 12),
                             labels = c("Dengue without warning signs", "Dengue with warning signs", "Severe dengue")))|>
  expss::apply_labels(idade_anos = "Age", cs_sexo = "Biological sex", 
                      cs_gestant = "Pregnancy status", cs_raca = "Race",
                      diabetes = "Diabetes", hematolog = "Hematologic disease",
                      hepatopat = "Liver disease", renal = "Kidney disease",
                      hipertensa = "Hypertension", acido_pept = "Peptic ulcer disease",
                      auto_imune = "Autoimmune disease", classi_fin = "Classification") |> 
  tbl_summary(by = classi_fin, missing = "no") |>
  modify_header(label ~ "") |>
  add_p()

glmer2 <- glmer(evolucao ~ idade_anos + cs_sexo + cs_gestant + cs_raca +
                  diabetes + hematolog + hepatopat + renal + hipertensa +
                  acido_pept + auto_imune + (1|faixa_geoses),
                data = dng_glmer2,
                family = binomial)

options(scipen = 0)

as.double(summary(glmer2)$varcor$faixa_geoses)|> # Shows low baseline variance
  format(scientific = F)
as.data.frame(cbind(odds = coef(summary(glmer2))[, "Estimate"],
                    p_vals = coef(summary(glmer2))[, "Pr(>|z|)"]))|>
  arrange(odds)|>
  format(scientific = F, width = 5)

# Proportional odds logistic regression ====
## Risk factors versus WHO disease classification ====
dng_polr <- dng_tot|>
  filter(if_all(diabetes:auto_imune, #Known risk factors should not be empty
                ~ !is.na(.)),
         classi_fin %in% c(10,11,12),
         cs_sexo %in% c("M", "F"))|>
  mutate(across(diabetes:auto_imune,
                ~ if_else(.==1, 1, 0)),
         classi_fin = ordered(classi_fin, levels = c("10", "11", "12")),
         cs_gestant = if_else(cs_gestant %in% c(1, 2, 3, 4), 1, 0), # 1st, 2nd, 3rd trimester and unknown gestational age versus else
         cs_raca = case_when(cs_raca == 1 ~ "B", #Whites
                             cs_raca %in% c(2, 3, 4, 5) ~ "NB", #Non-whites
                             cs_raca == 9 ~ "I")) |>  #Ignored race
  mutate(cs_sexo, cs_raca, classi_fin)

dng_polr <- dng_polr|>
  filter(id_municip %in% keys)|>
  group_by(id_municip)|>
  slice_sample(n = ceiling(250000/length(keys)))|> # Randomly select 443 cases from these cities to reduce our sample and model with a smaller RAM
  ungroup()

dng_polr <- dng_polr|>
  mutate(across(.cols = c(cs_sexo, cs_gestant, cs_raca, diabetes, hematolog,
                          hepatopat, renal, hipertensa, acido_pept, auto_imune),
                .fns = ~ factor(.))) |> 
  expss::apply_labels(cs_sexo = "Male", cs_gestant = "Pregnancy",
                      cs_raca = "Race (ignored)", cs_raca = "Race (non-white)",
                      diabetes = "Diabetes", hematolog = "Hematologic disease",
                      hepatopat = "Liver disease", renal = "Kidney disease",
                      hipertensa = "Hypertension", acido_pept = "Peptic ulcer disease",
                      auto_imune = "Autoimmune disease", geo_ses = "GeoSES",
                      faixa_geoses = "GeoSES (range)")

polr1 <- MASS::polr(classi_fin ~ idade_anos + cs_sexo + cs_gestant + cs_raca +
                      diabetes + hematolog + hepatopat + renal + hipertensa +
                      acido_pept + auto_imune,
                    data = dng_polr, Hess = TRUE)

cbind(odds = (exp(coef(summary(polr1))[, "Value"])),
      p_value = (1-pnorm(abs(coef(summary(polr1))[, "t value"]), 0, 1))*2)|>
  as.data.frame()|>
  arrange(odds)|>
  format(scientific = F)

polr2 <- MASS::polr(classi_fin ~ idade_anos + cs_sexo + cs_gestant + cs_raca +
                      diabetes + hematolog + hepatopat + renal + hipertensa +
                      acido_pept + auto_imune + geo_ses,
                    data = dng_polr, Hess = TRUE)

cbind(odds = (exp(coef(summary(polr2))[, "Value"])),
      p_value = (1-pnorm(abs(coef(summary(polr2))[, "t value"]), 0, 1))*2)|>
  as.data.frame()|>
  arrange(odds)|>
  format(scientific = F)

polr3 <- MASS::polr(classi_fin ~ idade_anos + cs_sexo + cs_gestant + cs_raca +
                      diabetes + hematolog + hepatopat + renal + hipertensa +
                      acido_pept + auto_imune + faixa_geoses,
                    data = dng_polr, Hess = TRUE)

cbind(odds = (exp(coef(summary(polr3))[, "Value"])),
      p_value = (1-pnorm(abs(coef(summary(polr3))[, "t value"]), 0, 1))*2)|>
  as.data.frame()|>
  arrange(odds)|>
  format(scientific = F)

tbl2 <- modelsummary(
  models = list(
    "GLM" = glmer2,
    "POLR w/o GeoSES" = polr1,
    "POLR w/ linear GeoSES" = polr2,
    "POLR w/ stratified GeoSES" = polr3),
  fmt = 2,
  estimate  = "{estimate} ({p.value})",
  statistic = NULL,
  coef_omit = c(1,14,15),
  coef_rename = c("idade_anos" = "Age", "cs_sexoM" = "Male", "cs_gestant" = "Pregnancy",
                  "cs_gestant1" = "Pregnancy", "cs_racaI" = "Race (ignored)",
                  "cs_racaNB" = "Race (non-white)", "diabetes" = "Diabetes",
                  "diabetes1" = "Diabetes", "hematolog" = "Hematologic disease",
                  "hematolog1" = "Hematologic disease", "hepatopat" = "Liver disease",
                  "hepatopat1" = "Liver disease", "renal" = "Kidney disease",
                  "renal1" = "Kidney disease", "hipertensa" = "Hypertension",
                  "hipertensa1" = "Hypertension", "acido_pept" = "Peptic ulcer disease",
                  "acido_pept1" = "Peptic ulcer disease", "auto_imune" = "Autoimmune disease",
                  "auto_imune1" = "Autoimmune disease", "geo_ses" = "GeoSES",
                  "faixa_geoses.L" = "GeoSES (range, linear regression)",
                  "faixa_geoses.Q" = "GeoSES (range, quadratic regression)",
                  "faixa_geoses.C" = "GeoSES (range, cubic regression)",
                  "SD (Intercept faixa_geoses)" = "SD (Intercept GeoSES)"),
  title = "Summary of model estimates",
  notes = "p-values for each estimate are indicated in parentheses",
  output = "idweek2024/table2.png")