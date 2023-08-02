library(tidyverse)
library(janitor)

PG <- notas_predichas

PG <- clean_names(PG)
names(PG)
PG_wider <- PG %>% 
     pivot_wider(names_from = asignatura, values_from = nota_predicha) %>%
     mutate(puntos_adicionales = 
                 case_when(EE == "A" & TdC == "A" ~ 3,
                           EE == "A" & TdC == "B" ~ 3,
                           EE == "A" & TdC == "C" ~ 2,
                           EE == "A" & TdC == "D" ~ 2,
                           EE == "B" & TdC == "A" ~ 3,
                           EE == "B" & TdC == "B" ~ 2,
                           EE == "B" & TdC == "C" ~ 2,
                           EE == "B" & TdC == "D" ~ 1,
                           EE == "C" & TdC == "A" ~ 2,
                           EE == "C" & TdC == "B" ~ 2,
                           EE == "C" & TdC == "C" ~ 1,
                           TRUE ~ 0)) %>% 
     clean_names()


write_csv(PG_wider,"PG_wider.csv")





PG_wider

PG_wider <- PG_wider %>% 
     mutate( puntos_totales =
                         rowSums(across(where(is.numeric)), na.rm = TRUE))

resultados <- notas_predichas_obtenidas

resultados <- clean_names(resultados)
names(resultados)

resultados_wider <- resultados %>% 
     select(., -asignatura_pg, -nota_predicha) %>% 
     pivot_wider(names_from = asignatura, values_from = nota_obt) %>%
     mutate(puntos_adicionales = 
                 case_when(EE == "A" & TdC == "A" ~ 3,
                           EE == "A" & TdC == "B" ~ 3,
                           EE == "A" & TdC == "C" ~ 2,
                           EE == "A" & TdC == "D" ~ 2,
                           EE == "B" & TdC == "A" ~ 3,
                           EE == "B" & TdC == "B" ~ 2,
                           EE == "B" & TdC == "C" ~ 2,
                           EE == "B" & TdC == "D" ~ 1,
                           EE == "C" & TdC == "A" ~ 2,
                           EE == "C" & TdC == "B" ~ 2,
                           EE == "C" & TdC == "C" ~ 1,
                           TRUE ~ 0))

write_csv(resultados_wider, "resultados_wider.csv")


names(resultados_wider)

resultados_wider <- resultados_wider %>% 
     mutate(puntos_adicionales = 
                 case_when(EE == "A" & TdC == "A" ~ 3,
                           EE == "A" & TdC == "B" ~ 3,
                           EE == "A" & TdC == "C" ~ 2,
                           EE == "A" & TdC == "D" ~ 2,
                           EE == "B" & TdC == "A" ~ 3,
                           EE == "B" & TdC == "B" ~ 2,
                           EE == "B" & TdC == "C" ~ 2,
                           EE == "B" & TdC == "D" ~ 1,
                           EE == "C" & TdC == "A" ~ 2,
                           EE == "C" & TdC == "B" ~ 2,
                           EE == "C" & TdC == "C" ~ 1,
                           TRUE ~ 0)) %>% 
     mutate( puntos_totales =
                  rowSums(across(where(is.numeric)), na.rm = TRUE))

View(resultados_wider)     
write_csv(resultados_wider, "resultados_wider.csv")

resultados2 <- resultados_wider_2 %>% 
     clean_names()

names(resultados2) 

resultados2 %>% 
     pivot_longer("ESPAÑOL A NM":"HISTORIA NM", names_to = "asignatura", values_to = "nota")


