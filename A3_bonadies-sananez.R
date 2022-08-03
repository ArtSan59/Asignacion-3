#Nombre 1: Santiago Bonadies
#Nombre 2: Arturo Sananez

#Corremos los paquetes corridos en esta clase

library(tidyverse)
library(readxl)
library(haven)
library(data.table)
library(sjlabelled)
library(sjPlot)
library(mice)
library(survey)
library(dplyr)

#Seleccionamos la data

personas <- read_sav("Data/encovi_personas2017_ds.sav")  

#Le echamos un vistaso a la data en si

glimpse(personas)

#Vemos la composición de la encuesta

view_df(personas)

#Creamos vector con los nuevos nombres de columnas

new_names_pers <- c("id_hogar", "id_per", "parentesco", "edad", "sexo", 
                    "sit_conyu", "nivel_edu", "edu_ano_aprobado", "edu_sem_aprobado",
                    "tipo_edu", "sit_econo", "sector_eco", "cat_ocupa",
                    "trab_remun", "ing_laboral", "horas_trab", "ing_otro",
                    "ing_pension",
                    "pesop", "grp_edad", "anos_edu", "tipo_ciudad")

#Renombras las columnas

personas <- personas %>%
  setnames(old = colnames(.), new = new_names_pers) %>%
  mutate(across(.cols = c("id_hogar", "id_per"),
                .fns = as.character))

#Cree esta columna solo para ver el id de cada individuo o muestra, nos parecia interesante

id_total <- personas %>%
  unite(id_hogar, id_per, col = "id", sep = "")

#Creamos una lista con todas aquellos que son imputables

imputables <- personas %>%
  filter((sit_econo == 1 | sit_econo == 2) & (ing_laboral %in% c(99,98,0)))

View(imputables)

#Renombramos la columna de ingreso laboral para no tener dos columnas de ingreso laboral

imputables <- imputables %>%
  rename(ing_laboral_imp = ing_laboral)

View(imputables)

#Creamos los grupos de donantes según las variables usadas y escogemos uno de 
#Esas muestras en cada grupo de forma aleatoria
#Escogimos como variables adicionales: nivel de eduicación, sector económico y tipo de ciudad
#Estos se escogiron ya que para determinar el ingreso de estos de estos valores
#faltantes más exacto es necesario que coincidan de la mejor manera los perfiles.
#En el caso de nivel de educación es clave. Ya que este determina el nivel de capital humano, que al ser
#distinto, que al haber distintas ofertas y demandas para cada nivel, implican distintos salarios.
#En cuanto a sector económico es debido a que los distintos sectores económicos, manejan distintos sueldos
#debido a la demanda del mismo, la disponibilidad de mano de obra, especialización de la misma. 
#Además de los riesgos o requerimientos de cada sector.
#Los sueldos en el sector petrolero son distintos a los de la pesca. 
#Por último, el hecho de usar el tipo de ciudad es debido a que en los distintos centros poblacionales
#Concentran distintas escalas, ofertas y demandan, que establecen un nivel de precios ajustados a las anteriores.

donantes_1<- personas %>%
  filter((sit_econo == 1 | sit_econo == 2) & (!ing_laboral %in% c(98, 99, 0)))%>%
  group_by(sexo, grp_edad, nivel_edu, sector_eco, tipo_ciudad) %>%
  sample_n(1)

donantes_2<- personas %>%
  filter((sit_econo == 1 | sit_econo == 2) & (!ing_laboral %in% c(98, 99, 0)))%>%
  group_by(sexo, grp_edad, nivel_edu, sector_eco, tipo_ciudad) %>%
  summarise( n_imp = length(sexo))

donantes_2 <- donantes_2 %>%
  select("sexo", "grp_edad", "nivel_edu", "sector_eco", "tipo_ciudad", "n_imp")

View(donantes_1)
#Creamos una tabla con de los imputables pero incluyendo el ingreso del donante

imputados_ing <- imputables %>%
  left_join(donantes_1, by = c("sexo", "grp_edad", "nivel_edu", "sector_eco", "tipo_ciudad"))%>% 
  mutate(ing_laboral_imp = ifelse(ing_laboral_imp %in% c(98, 99, 0),
                                  yes = ing_laboral,
                                  no = ing_laboral_imp)) %>%
  select("id_hogar.x", "id_per.x", "sexo", "grp_edad", "nivel_edu", "sector_eco", "tipo_ciudad", "ing_laboral_imp", "ing_laboral")

View(imputados_ing)
View(personas)

imputados_ing <- imputados_ing %>%
  rename("id_hogar" = "id_hogar.x") %>%
  rename("id_per" = "id_per.x")

imputados_ing <- imputados_ing %>%
  left_join(donantes_2, by = c("sexo", "grp_edad", "nivel_edu", "sector_eco", "tipo_ciudad"))%>% 
  select("id_hogar", "id_per", "sexo", "grp_edad", "nivel_edu", "sector_eco",
         "tipo_ciudad", "ing_laboral_imp", "ing_laboral", "n_imp")

View(imputados_ing)

#Determinamos el numero de valores imputados exitosamente 
#contando los na, contando el total, y sacando la porporcion, lo que nos permite hallar el valor que buscamos

suma_na <- sum(is.na(imputados_ing$ing_laboral_imp))
suma_na
total <- length(imputados_ing$sexo)
total

proporcion_falla <- (suma_na/total)
proporcion_falla

proporcion_exito <- (1 - proporcion_falla)*100
proporcion_exito

#Modificamos la data original e incluímos esos ingresos imputados, a través de left join

data_imputada <- personas %>%
  left_join(imputados_ing, by = c("id_hogar", "id_per", "sexo", "grp_edad", "nivel_edu", "sector_eco", "tipo_ciudad", "ing_laboral")) %>%
  select("sexo", "grp_edad", "nivel_edu", "sector_eco", "tipo_ciudad", "ing_laboral_imp", "ing_laboral", "n_imp")

#"sexo", "grp_edad", "nivel_edu", "sector_eco", "tipo_ciudad"

View(data_imputada)


