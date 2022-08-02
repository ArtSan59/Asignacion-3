library(tidyverse)
library(readxl)
library(haven)
library(data.table)
library(sjlabelled)
library(sjPlot)
library(mice)
library(survey)
library(dplyr)



directorio <- getwd()
setwd(directorio)

personas <- read_sav("Data/encovi_personas2017_ds.sav") 

hogares <- read_sav("Data/encovi_hogares2017_ds.sav")


glimpse(personas)

view_df(personas)

new_names_pers <- c("id_hogar", "id_per", "parentesco", "edad", "sexo", 
                    "sit_conyu", "nivel_edu", "edu_ano_aprobado", "edu_sem_aprobado",
                    "tipo_edu", "sit_econo", "sector_eco", "cat_ocupa",
                    "trab_remun", "ing_laboral", "horas_trab", "ing_otro",
                    "ing_pension",
                    "pesop", "grp_edad", "anos_edu", "tipo_ciudad")

personas <- personas %>%
  setnames(old = colnames(.), new = new_names_pers) %>%
  mutate(across(.cols = c("id_hogar", "id_per"),
                .fns = as.character))

id_total <- personas %>%
  unite(id_hogar, id_per, col = "id", sep = "")
  

imputables <- personas %>%
  filter((sit_econo == 1 | sit_econo == 2) & (ing_laboral == 99 | ing_laboral <= 0))
  
personas[sit_econo==98] <- NA


donantes<- personas %>%
  filter((sit_econo == 1 | sit_econo == 2) & (ing_laboral != 99 | ing_laboral != 98 | ing_laboral != 0))%>%
  group_by(sexo, grp_edad, nivel_edu, sector_eco, tipo_ciudad) %>% 
  summarise(ing_laboral_imp  = weighted.mean( ing_laboral , pesop),
            n_imp = length(grp_edad))

data_imputada <- personas %>%
  left_join(donantes, by = c("sexo", "grp_edad", "nivel_edu", "sector_eco", "tipo_ciudad"))%>% 
  mutate(ing_laboral = ifelse( (ing_laboral == 99 | ing_laboral == 99 | ing_laboral <= 0),
                               yes = ing_laboral_imp,
                               no = ing_laboral))


