# mejores puntajes 2023

mejores_puntajes <- resultados_generales2 %>% 
     clean_names() %>% 
     distinct(estudiante, puntos_totales,puntos_adicionales, diploma) %>% 
     filter(!is.na(puntos_totales)) %>% 
     arrange(desc(puntos_totales), desc(puntos_adicionales)) %>% 
     mutate(row_n = row_number(), .before = estudiante) %>% 
     select(row_n, estudiante:diploma)
     
tab1  
     mejores_puntajes %>% 
     slice(1:27) %>% 
     gt() %>% 
     cols_label(
          row_n = "Puesto",
          estudiante = "Estudiante",
          puntos_totales = "Puntos Obtenidos",
          puntos_adicionales = "Puntos Adic.",
          diploma = "Diploma") %>%
     cols_align(align = "left",columns = everything()) %>% 
     cols_width(
          puntos_totales ~ px(10),
          puntos_adicionales ~ px(5)
     ) %>% 
     tab_style(
          style = cell_text(weight = "bold", color = "black", size = 10),
          locations = cells_column_labels()
     ) %>% 
     tab_style(
          style = cell_text(color = "black", size = 9),
          locations = cells_body()) %>% 
     tab_style(style = list(
          cell_fill(color = "#fcd7c5"),
          cell_text(weight = "bold")),
          locations = cells_body(columns = diploma, 
                                 rows =  diploma == "NO")) %>%
     tab_options(table.font.size = pct(50),
                 heading.align = 'left',
                 heading.title.font.size = px(14), heading.title.font.weight = "bold") %>% 
     gt_color_rows(columns = row_n, domain = 1:53, palette = "ggsci::blue_material")  
  

tab2 
     mejores_puntajes %>%
     slice(28:53) %>%
     gt() %>%
     cols_label(
          row_n = "Puesto",
          estudiante = "Estudiante",
          puntos_totales = "Puntos Obtenidos",
          puntos_adicionales = "Puntos Adic.",
          diploma = "Diploma") %>%
     cols_align(align = "left",columns = everything()) %>% 
     cols_width(
          puntos_totales ~ px(10),
          puntos_adicionales ~ px(5)
     ) %>% 
     tab_style(
          style = cell_text(weight = "bold", color = "black", size = 10),
          locations = cells_column_labels()
     ) %>% 
     tab_style(
          style = cell_text(color = "black", size = 9),
          locations = cells_body()) %>% 
     tab_style(style = list(
          cell_fill(color = "#fcd7c5"),
          cell_text(weight = "bold")),
          locations = cells_body(columns = diploma, 
                                 rows =  diploma == "NO")) %>% 
     tab_options(table.font.size = pct(50),
                 heading.align = 'left',
                 heading.title.font.size = px(14), heading.title.font.weight = "bold") %>% 
     gt_color_rows(columns = row_n, domain = 1:53, palette = "ggsci::blue_material") 

listed_tables <- list(tab1, tab2)

gt_two_column_layout(listed_tables, vwidth = 1050)
     

resultados_generales2 %>% 
     distinct(estudiante, puntos_totales) %>% 
     arrange(desc(puntos_totales)) %>% 
     slice_head(n = 26) %>% 
     gt()


promedios_asignatura <-
     resultados_generales2 %>% 
     group_by(asignatura) %>% 
     summarise(nota_promedio = mean(nota, na.rm = TRUE))

porcentajes_aprobacion <- resultados_generales2 %>% 
     select(estudiante, asignatura, nota) %>% 
     drop_na(nota) %>% 
     group_by(asignatura) %>% 
     summarise(porcentaje = sum(nota >= 4) / n())

porcentajes_aprobacion_promedio_asignatura <-
     left_join(promedios_asignatura, porcentajes_aprobacion, by = "asignatura")

porcentajes_aprobacion_promedio_asignatura

write_csv(porcentajes_aprobacion_promedio_asignatura, "porcentajes_aprobacion_promedio_asignatura .csv")


promedio_asignatura_porcentaje_aprobacion <- read_csv("promedio_asignatura_porcentaje_aprobacion.csv")

promedio_asignatura_porcentaje_aprobacion%>% 
     gt() %>% 
     cols_label(
          asignatura = "Asignatura",
          nota_promedio = "Promedio GCB",
          porcentaje = "Porcentaje Aprobación") %>% 
     tab_header(title = md('**Resultados por asignaturas Diploma IB**')) %>% 
     fmt_number(columns = nota_promedio, decimals = 2) %>%
     fmt_percent(columns = porcentaje, 
                 rows = everything(),
                 decimals = 0) %>% 
     tab_style(
          style = cell_text(weight = "bold", color = "black"),
          locations = cells_column_labels()
     ) %>%
     tab_style(style = list(
          cell_fill(color = "#fcd7c5"),
          cell_text(weight = "bold")),
          locations = cells_body(columns = nota_promedio, 
                                 rows =  nota_promedio <= 4)) %>% 
     tab_style(style = list(
          cell_fill(color = "#fcd7c5"),
          cell_text(weight = "bold")),
          locations = cells_body(columns = porcentaje, 
                                 rows =  porcentaje <0.5)) %>% 
     tab_style(style = list(
          cell_fill(color = "#c8faf1"),
          cell_text(weight = "bold")),
          locations = cells_body(columns = porcentaje, 
                                 rows =  porcentaje > 0.80)) %>% 
     tab_options(table.font.size = 13,
                 heading.align = 'left', heading.title.font.size = px(18))

promedios_gcb_munidal <- promedios_gcb_munidal %>% 
     clean_names()
tabla_promedios_gcb_munidal <- promedios_gcb_munidal %>% 
     gt() %>% 
     cols_label(
          asignatura = "Asignatura",
          promedio_gcb_2023 = "GCB",
          promedio_mundial_2023 = "Mundial",
          promedio_nov_2022 = "GCB(2022)") %>% 
     tab_spanner(
          label = md("**Nota Promedio 2023**"),
          columns = 2:3) %>% 
     tab_header(title = md('**Resultados por asignaturas Diploma IB**')) %>% 
     fmt_number(columns = where(is.numeric), decimals = 2) %>% 
     sub_missing(missing_text = "-") %>% 
     tab_style(
          style = cell_text(weight = "bold", color = "black"),
          locations = cells_column_labels()
     ) %>% 
     tab_style(style = list(
          cell_fill(color = "lightcyan"),
          cell_text(weight = "bold")),
          locations = cells_body(columns = promedio_gcb_2023, 
                                 rows =  promedio_gcb_2023 >= promedio_mundial_2023)) %>% 
     tab_style(style = list(
          cell_fill(color = "#fcd7c5"),
          cell_text(weight = "bold")),
          locations = cells_body(columns = promedio_gcb_2023, 
                                 rows =  promedio_gcb_2023 <= 3.5)) %>% 
     tab_footnote(
          footnote = md("Los resultados en cyan corresponden a las asignaturas que obtuvieron promedios por encima del promedio mundial. Los resultados en rosa establecen las asignaturas con menores resultados en la convocatoria"),
          locations = cells_column_labels(columns = promedio_gcb_2023)
     ) %>% 
     opt_footnote_marks(marks = "standard") %>% 
     tab_options(table.font.size = 13,
                 heading.align = 'left', heading.title.font.size = px(18))

resultados_generales2 %>% 
     drop_na() %>%
     group_by(estudiante) %>% 
     summarize(asignaturas = n())
View(resultados_generales2)

resultados_generales2 %>% 
     filter(asignatura == "biología_ns") %>% 
     drop_na() %>% 
     summarize(n())


promedios_gcb_mundial %>% 
     select(asignatura, promedio_gcb_2023, promedio_nov_2022) %>% 
     mutate(variacion = (promedio_gcb_2023 - promedio_nov_2022)/promedio_nov_2022) %>% 
     gt() %>% 
     cols_label(
          asignatura = "Asignatura",
          promedio_gcb_2023 = "GCB 2023",
          promedio_nov_2022 = "GCB 2022",
          variacion = "Variación Porcentual") %>% 
     fmt_percent(columns = variacion, 
                 rows = everything(),
                 decimals = 2) %>% 
     tab_style(
          style = cell_text(weight = "bold", color = "black"),
          locations = cells_column_labels()
     )

promedios_gcb_mundial_longer %>% 
     filter(asignatura %in% grupo1_2) %>% 
     mutate(asignatura = factor(asignatura,
                                levels = c("ESPAÑOL A NM", "ESPAÑOL A NS", 
                                           "INGLÉS B NM", "INGLÉS B NS", "FRANCÉS AB NM"))) %>% 
     ggplot(aes(asignatura, promedio, fill = resultado)) + 
     geom_col(position = "dodge") +
     geom_text(
          aes(label = promedio), colour = "black", size = 3.5,
          family = "sans", fontface = "bold",
          vjust = 1.5, position = position_dodge(.9)) +
     theme_minimal() +
     scale_fill_discrete(labels=c('GCB 2023', 'Mundial 2023', 'GCB 2022')) +
     theme(
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(family = "Helvetica-Bold", face = "bold"),
          axis.title = element_blank(),
          axis.text = element_text(family = "Helvetica-Bold", face = "bold")
     )

### Promedios Mundiales

promedios_gcb_mundial_longer %>% 
     filter(asignatura %in% grupo1_2) %>% 
     mutate(asignatura = factor(asignatura,
                                levels = c("ESPAÑOL A NM", "ESPAÑOL A NS", "INGLÉS B NM", "INGLÉS B NS", "FRANCÉS AB NM"))) %>% 
     ggplot(aes(asignatura, promedio, fill = resultado)) + 
     geom_col(position = "dodge") +
     geom_text(
          aes(label = promedio), colour = "black", size = 3.5,
          family = "sans", fontface = "bold",
          vjust = 1.5, position = position_dodge(.9)) +
     theme_minimal() +
     scale_fill_manual(values = brewer.pal(3, "Set2"), labels = etiquetas) +
     theme(
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(family = "Helvetica-Bold", face = "bold"),
          axis.title = element_blank(),
          axis.text = element_text(family = "Helvetica-Bold", face = "bold")
     )

promedios_gcb_mundial_longer %>% 
     filter(asignatura %in% grupo4) %>% 
     mutate(asignatura = factor(asignatura, levels =
                             c("FÍSICA NM", "FÍSICA NS", "QUÍMICA NM",
                               "QUÍMICA NS", "BIOLOGÍA NS", "COMPUTER SCIENCE NM"))) %>% 
     ggplot(aes(asignatura, promedio, fill = resultado)) + 
     geom_col(position = "dodge") +
     geom_text(
          aes(label = promedio), colour = "black", size = 3.5,family = "sans",
                              fontface = "bold", vjust = 1.5, position = position_dodge(.9)) +
     theme_minimal() + 
     scale_x_discrete(labels = c("FÍSICA NM", "FÍSICA NS", "QUÍMICA NM",  "QUÍMICA NS", "BIOLOGÍA NS", "COMPUTER\nSCIENCE NM")) +
     scale_fill_manual(values = brewer.pal(3, "Set2"), labels = etiquetas) +
     theme(legend.title = element_blank(),
           legend.position = "bottom",
           legend.text = element_text(family = "Helvetica-Bold",
                                      face = "bold"),
           axis.title = element_blank(),
           axis.text = element_text(color = "black", family = "Helvetica-Bold",
                                    face = "bold"))

ggplot(alumnos_sexo, aes(Sexo)) +
     geom_bar(aes(fill = Sexo), width = 0.5) +
     geom_text(aes(label = after_stat(count)), size = 10,
               stat = "count", vjust = 1.5, colour = "white") +
     labs(y = "Número de Estudiantes") +
     theme_minimal() +
     scale_fill_grey() +
     theme(
          axis.title = element_text(color = "black", family = "sans", face = "bold"),
          axis.text = element_text(color = "black", family = "sans", face = "bold"),
          text = element_text(family = "sans", face = "bold")
     )

#distribuciones de calificaciones por asignatura
notas_predichas_obtenidas <- read_delim("notas_predichas_obtenidas.csv", 
                                        delim = "\t", escape_double = FALSE, 
                                        col_types = cols(Asignatura_PG = col_skip()), 
                                        trim_ws = TRUE)
notas_predichas_obtenidas2 <- notas_predichas_obtenidas %>% 
     mutate(Asignatura = if_else(Asignatura == "INFORMÁTICA NM", "COMPUTER SCIENCE SL", Asignatura))

distribucion_notas_asignatura_g12 <- notas_predichas_obtenidas2 %>% 
     clean_names() %>% 
     select(estudiante, asignatura, nota_obt) %>% 
     filter(asignatura != "EE" & asignatura != "TdC") %>% 
     filter(asignatura %in% grupo1_2) %>% 
     mutate(nota_obt = as.factor(nota_obt)) %>% 
     group_by(asignatura, nota_obt) %>% 
     tally() %>% 
     ggplot(aes(x = nota_obt, y = n, fill = asignatura, label = n)) +
     geom_col() +
     geom_text(size = 3, position = position_stack(vjust = 0.5)) +
     labs(x = "Calificación", y = "Estudiantes") +
     theme_minimal() +
     scale_fill_manual(values = c("#fd7f6f", "#7eb0d5", "#b2e061", "#bd7ebe", "#ffb55a", "#ffee65", "#beb9db", "#fdcce5", "#8bd3c7"),
                       labels = grupo1_2) +
     theme(
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(family = "Helvetica", size = 12),
          axis.title = element_text(color = "black", 
                                    family = "Helvetica-Bold", face = "bold"),
          axis.text = element_text(color = "black", 
                                   family = "Helvetica-Bold", face = "bold")
     )


# funcion grafico distribución de calificaciones

grupos_asignaturas <- list(c("ESPAÑOL A NM", "ESPAÑOL A NS", "INGLÉS B NM", "INGLÉS B NS", "FRANCÉS AB NM"),
              c("POLÍTICA GLOBAL NM", "POLÍTICA GLOBAL NS", "HISTORIA NM"),
              c("FÍSICA NM", "FÍSICA NS", "QUÍMICA NM", "QUÍMICA NS", "BIOLOGÍA NS", "COMPUTER SCIENCE SL"),
              c("MATEMÁTICAS NM", "MATEMÁTICAS NS"))

grafico_distribucion_notas <- function(GRUPO) {
     library(tidyverse)
     
     distribucion_notas_asignatura <- notas_predichas_obtenidas2 %>%
          clean_names() %>%
          select(estudiante, asignatura, nota_obt) %>%
          filter(asignatura != "EE" & asignatura != "TdC") %>%
          filter(asignatura %in% GRUPO) %>%
          mutate(nota_obt = as.factor(nota_obt)) %>%
          group_by(asignatura, nota_obt) %>%
          tally() %>%
          ggplot(aes(x = nota_obt, y = n, fill = asignatura, label = n)) +
          geom_col() +
          geom_text(size = 4,  position = position_stack(vjust = 0.5)) +
          labs(x = "Calificación", y = "Estudiantes") +
          theme_minimal() +
          scale_fill_brewer(palette ="Accent") +
          theme(
               legend.title = element_blank(),
               legend.position = "bottom",
               legend.text = element_text(family = "Helvetica", face = "bold", size = 9),
               axis.title = element_text(color = "black",
                                         family = "Helvetica-Bold", face = "bold"),
               axis.text = element_text(color = "black",
                                        family = "Helvetica-Bold", face = "bold")
          )
     
     return(distribucion_notas_asignatura)
}





## Dispersión de notas por asignatura
notas_componentes <- read_csv("M2023_061156.csv")
View(notas_componentes)

notas_componentes_clean <- notas_componentes %>% 
     clean_names() %>% 
     select(c(3,4,5,10,12,13,14,15,16,17)) %>%
     mutate(asignatura = if_else(asignatura == "INFORMÁTICA", 
                                 "COMPUTER SCIENCE", asignatura)) %>%
     mutate(asignatura = as_factor(asignatura),
            nivel = as_factor(nivel),
            componente = as_factor(componente)) %>% 
     rename(puntuacion_total = puntuacion_total_ajustada_de_la_asignatura) %>%
     filter(nivel != "Monografía" & nivel != "TdC") %>% 
     select(nombre, asignatura, nivel, componente, puntuacion_total)

g1 <- c("ESPAÑOL A: Literatura", "INGLÉS B", "FRANCÉS AB INITIO")
g3 <- c("POLÍTICA GLOBAL", "HISTORIA")
g4 <- c("BIOLOGÍA", "FÍSICA", "QUÍMICA", "COMPUTER SCIENCE")
g5 <- c("MATEMÁTICAS: ANÁLISIS Y ENFOQUES")

rango <- "En este rango de puntuación se encuentra\nel límite inferior del NIVEL 7\npara cada asignatura"

dispersion_notas_g1 <- notas_componentes_clean %>% 
     distinct(nombre, asignatura, nivel, puntuacion_total) %>% 
     filter(asignatura %in% c("ESPAÑOL A: Literatura", "INGLÉS B", "FRANCÉS AB INITIO")) %>% 
     mutate(asignatura = fct_relevel(asignatura, g1)) %>% 
     ggplot(aes(asignatura, puntuacion_total, fill = nivel)) +
     geom_boxplot(alpha = 0.6, outlier.shape=NA) +
     geom_point(position = position_jitterdodge(jitter.width = 0.8,
                                                dodge.width = 0.8),
                color = "brown", size = 1.8, alpha = 0.80) +
     #geom_point(fill= "bisque4", size = 2, alpha = 0.4, position=position_jitterdodge()) +
     geom_hline(yintercept = 75, linetype = "dashed", color = "darkorange", size = 1.25) + 
     geom_hline(yintercept = 85, linetype = "dashed", color = "darkorange", size = 1.25) +
     annotate(geom = "text", x = 2.5, y = 96, label = rango, hjust = 0) + 
     geom_curve(
          aes(x = 3.15, y = 93, xend = 3, yend = 81),
          arrow = arrow(length = unit(0.03, "npc"), type="closed"), # Describes arrow head (open or closed)
          colour = "#EC7014", size = 0.5, curvature = - 0.5, angle = 90) + # Anything other than 90 or 0
                                                                           # can look unusual
     scale_y_continuous(limits = c(30,100), n.breaks = 10) +
     labs(y = "Puntuación Total Obtenida") + 
     #scale_fill_viridis_d(alpha = 0.5) +
     theme_minimal() +
     theme(axis.title = element_text(family = "sans", face="bold", color = "black"),
           axis.title.x = element_blank(),
           axis.text = element_text(family = "sans", face="bold", color = "black"),
           legend.title = element_blank(),
           legend.position = "bottom",
           text = element_text(family = "sans", face = "bold", color = "black", size = 12)
     )
dispersion_notas_g3 <- notas_componentes_clean %>% 
     distinct(nombre, asignatura, nivel, puntuacion_total) %>% 
     filter(asignatura %in% g3)  %>% 
     mutate(asignatura = fct_relevel(asignatura, g3)) %>% 
     ggplot(aes(asignatura, puntuacion_total, fill = nivel)) +
     geom_boxplot(alpha = 0.6, outlier.shape=NA) +
     geom_point(position = position_jitterdodge(jitter.width = 0.8,
                                                dodge.width = 0.8),
                color = "brown", size = 1.8, alpha = 0.80) +
     #geom_point(fill= "bisque4", size = 2, alpha = 0.4, position=position_jitterdodge()) +
     geom_hline(yintercept = 75, linetype = "dashed", color = "darkorange", size = 1.25) + 
     geom_hline(yintercept = 85, linetype = "dashed", color = "darkorange", size = 1.25) +
     scale_y_continuous(limits = c(30,100), n.breaks = 10) +
     labs(y = "Puntuación Total Obtenida") + 
     #scale_fill_viridis_d(alpha = 0.5) +
     theme_minimal() +
     theme(axis.title = element_text(family = "sans", face="bold", color = "black"),
           axis.title.x = element_blank(),
           axis.text = element_text(family = "sans", face="bold", color = "black"),
           legend.title = element_blank(),
           legend.position = "bottom",
           text = element_text(family = "sans", face = "bold", color = "black", size = 12)
     )

dispersion_notas_g4 <-notas_componentes_clean %>% 
     distinct(nombre, asignatura, nivel, puntuacion_total) %>% 
     filter(asignatura %in% g4)  %>% 
     mutate(asignatura = fct_relevel(asignatura, g4)) %>% 
     ggplot(aes(asignatura, puntuacion_total, fill = nivel)) +
     geom_boxplot(alpha = 0.6, outlier.shape=NA) +
     geom_point(position = position_jitterdodge(jitter.width = 0.8,
                                                dodge.width = 0.8),
                color = "brown", size = 1.8, alpha = 0.80) +
     #geom_point(fill= "bisque4", size = 2, alpha = 0.4, position=position_jitterdodge()) +
     geom_hline(yintercept = 75, linetype = "dashed", color = "darkorange", size = 1.25) + 
     geom_hline(yintercept = 85, linetype = "dashed", color = "darkorange", size = 1.25) +
     scale_y_continuous(limits = c(30,100), n.breaks = 10) +
     labs(y = "Puntuación Total Obtenida") + 
     #scale_fill_viridis_d(alpha = 0.5) +
     theme_minimal() +
     theme(axis.title = element_text(family = "sans", face="bold", color = "black"),
           axis.title.x = element_blank(),
           axis.text = element_text(family = "sans", face="bold", color = "black"),
           legend.title = element_blank(),
           legend.position = "bottom",
           text = element_text(family = "sans", face = "bold", color = "black", size = 12)
     )

dispersion_notas_g5 <-notas_componentes_clean %>% 
     distinct(nombre, asignatura, nivel, puntuacion_total) %>% 
     filter(asignatura %in% g5)  %>% 
     mutate(asignatura = fct_relevel(asignatura, g4)) %>% 
     ggplot(aes(asignatura, puntuacion_total, fill = nivel)) +
     geom_boxplot(alpha = 0.6, outlier.shape=NA) +
     geom_point(position = position_jitterdodge(jitter.width = 0.8,
                                                dodge.width = 0.8),
                color = "brown", size = 1.8, alpha = 0.80) +
     #geom_point(fill= "bisque4", size = 2, alpha = 0.4, position=position_jitterdodge()) +
     geom_hline(yintercept = 75, linetype = "dashed", color = "darkorange", size = 1.25) + 
     geom_hline(yintercept = 85, linetype = "dashed", color = "darkorange", size = 1.25) +
     scale_y_continuous(limits = c(30,100), n.breaks = 10) +
     labs(y = "Puntuación Total Obtenida") + 
     #scale_fill_viridis_d(alpha = 0.5) +
     theme_minimal() +
     theme(axis.title = element_text(family = "sans", face="bold", color = "black"),
           axis.title.x = element_blank(),
           axis.text = element_text(family = "sans", face="bold", color = "black"),
           legend.title = element_blank(),
           legend.position = "bottom",
           text = element_text(family = "sans", face = "bold", color = "black", size = 12)
     )

### Tabla de puntos promedio
colnames(notas_componentes_clean)

puntuaciones_asignatura <- notas_componentes_clean %>% 
     group_by(asignatura, nivel) %>% 
     summarize(promedio_puntos_obtenidos = mean(puntuacion_total),
               desviacion = sd(puntuacion_total), .groups = "drop") 

puntuaciones_asignatura %>% 
     gt() %>% 
     cols_label(
          asignatura = "Asignatura",
          nivel = "Nivel",
          promedio_puntos_obtenidos = "Puntuación Promedio",
          desviacion = "Desviación estándar") %>%
     cols_align(align = "left",columns = everything()) %>% 
     tab_header(title = md('**Puntajes promedio por asignatura - Mayo 2023**')) %>% 
     fmt_number(columns = where(is.numeric), decimals = 2) %>% 
     tab_style(
          style = cell_text(weight = "bold", color = "black"),
          locations = cells_column_labels()
     ) %>% 
     tab_style(style = list(
          cell_fill(color = "lightcyan"),
          cell_text(weight = "bold")),
          locations = cells_body(columns = promedio_puntos_obtenidos, 
                                 rows =  promedio_puntos_obtenidos >= 50)) %>% 
     tab_style(style = list(
          cell_fill(color = "#fcd7c5"),
          cell_text(weight = "bold")),
          locations = cells_body(columns = promedio_puntos_obtenidos, 
                                 rows =  promedio_puntos_obtenidos < 40)) %>%
     tab_footnote(footnote = "El puntaje total es la suma de puntos obtenidos por cada estudiante en cada uno de los componentes de evaluación en su asignatura, ponderado por el peso que tiene cada uno de ellos en su calificación final.",
          locations = cells_title(groups = "title")
     ) %>% 
     tab_footnote(
          footnote = md("Los resultados en cyan corresponden a las asignaturas que obtuvieron los puntajes más altos durante la convocatiria"),
          locations = cells_column_labels(columns = promedio_puntos_obtenidos)) %>% 
     opt_footnote_marks(marks = "standard") %>% 
     tab_options(table.font.size = 13,
                 heading.align = 'left', heading.title.font.size = px(18)) %>% 
     gt_theme_pff()


     puntuaciones_asignatura2 <-read_csv("puntuaciones_asignatura.csv")
puntuaciones_asignatura2 <- read_excel("puntuaciones_asignaturas.xlsx")



### Monografías 
notas_componentes_clean2 <- notas_componentes %>% 
     clean_names() %>% 
     select(c(3,4,5,6,10,12,13,14,15,16,17)) %>%
     mutate(asignatura = if_else(asignatura == "INFORMÁTICA", 
                                 "COMPUTER SCIENCE", asignatura)) %>%
     mutate(asignatura = as_factor(asignatura),
            nivel = as_factor(nivel),
            componente = as_factor(componente)) %>% 
     rename(puntuacion_total = puntuacion_total_ajustada_de_la_asignatura) %>%
     filter(nivel == "Monografía") %>% 
     select(c(1,4,5,10, 11)) %>% 
     rename(calificacion = calificacion_final_de_la_asignatura) %>% 
     mutate(asignatura = as_factor(asignatura),
            calificacion = as_factor(calificacion))

## Distribucion Monografías
tabla_monografias <-notas_componentes_clean2 %>% 
     group_by(asignatura, idioma) %>% 
     tally() %>%
     ungroup() %>% 
     arrange(desc(n)) %>% 
     gt() %>% 
     cols_label(
          asignatura = "Asignatura",
          idioma = "Idioma",
          n = "Monografías<br>realizadas",
          .fn = md) %>%
     cols_align(align = "left",columns = 1:2) %>% 
     cols_align(align = "center",columns = 3) %>% 
     fmt_number(columns = where(is.numeric), decimals = 0) %>% 
     tab_style(
          style = cell_text(weight = "bold", color = "black"),
          locations = cells_column_labels()
     ) %>% 
     gt_theme_pff()

## Calificacion Monografias
calificacion_monografias <- notas_componentes_clean2 %>% 
     mutate(calificacion = factor(calificacion, levels = c("A", "B", "C", "D", "E"))) %>% 
     group_by(calificacion) %>% 
     summarise(estudiantes = n()) %>% 
     ungroup() %>% 
     ggplot(aes(x = calificacion, y = estudiantes)) +
     geom_bar(stat = "identity", fill = "aquamarine3") +
     geom_text(aes(label = estudiantes), size = 6, vjust = 1.4, colour = "black") +
     labs(x = "Calificación", y = "Número de estudiantes") +
     theme_minimal() +
     theme(axis.title = element_text(family = "sans", face="bold", color = "black"),
           axis.text = element_text(family = "sans", face="bold", color = "black", size = 14),
           legend.title = element_blank(),
           legend.position = "bottom")

calificacion_monografias

### distribucion puntos monografias

puntos_monografias 

notas_componentes %>% 
     clean_names() %>% 
     select(10, 3,4,6,16) %>% 
     filter(nivel == "Monografía") %>%
     mutate(asignatura = as_factor(asignatura)) %>% 
     ggplot(aes(fct_reorder(asignatura, puntuacion_total_ajustada_de_la_asignatura,
                            .fun = median),
                puntuacion_total_ajustada_de_la_asignatura)) +
     geom_boxplot(alpha = 0.2, outlier.shape=NA) +
     geom_point(aes(fill = idioma, color = idioma), position = position_jitterdodge(jitter.width = 0.8,
                                                                    dodge.width = 0.8),
                size = 1.8, alpha = 0.80) + 


     geom_hline(yintercept = 27, linetype = "dashed", color = "coral", size = 1) +
     annotate(geom = "text", x = 8.5, y = 28, label = "Calificación A", hjust = 0,
              size = 3, fontface = "bold", color ="black") +
     geom_hline(yintercept = 14, linetype = "dashed", color = "lightgreen", size = 1) +
     annotate(geom = "text", x = 8.5, y = 4, label = "Calificación C", hjust = 0,
              size = 3, fontface = "bold", color ="black") +
     scale_y_continuous(limits = c(0,40), n.breaks = 10) +
     labs(y = "Calificación Moderada") + 
     #scale_fill_viridis_d(alpha = 0.5) +
     theme_minimal() +
     coord_flip() +
     theme(axis.title.y = element_blank(),
           axis.title.x = element_text(family = "sans", face ="bold", color = "black", size = 10),
           axis.text = element_text(family = "sans", face ="bold", color = "black", size = 9),
           legend.title = element_blank(),
           legend.position = "bottom",
           #text = element_text(family = "sans", face = "bold", color = "black", size = 10)
     )


###
     
monografias22_23 <- read_delim("monografias.csv", 
                               delim = "\t", escape_double = FALSE, 
                               trim_ws = TRUE)

distribucion_notas_monografias22_23 <- monografias22_23 %>% 
     clean_names() %>% 
     pivot_longer(cols = starts_with("x"), names_to = "año", values_to = "monografias") %>% 
     group_by(calificacion, año) %>% 
     summarise(total_trabajos = sum(monografias), .groups = "drop") %>% 
     mutate(porcentaje = total_trabajos/sum(total_trabajos)) %>% 
     select(calificacion, año, porcentaje) %>% 
     pivot_wider(names_from = año, values_from = porcentaje) %>% 
     mutate(variacion = (x2023 - x2022)/(x2022)) 

variacion_monografias <- distribucion_notas_monografias22_23 %>% 
     gt() %>% 
     cols_label(
          calificacion = "Calificacion",
          x2022 = "Nov 2022",
          x2023 = "May 2023",
          variacion = "Variación") %>%
     cols_align(align = "left",columns = everything()) %>% 
     fmt_percent(columns = where(is.numeric), decimals = 1) %>% 
     sub_missing(columns = everything(),rows = everything(),
                 missing_text = "----") %>% 
     tab_style(
          style = cell_text(weight = "bold", color = "black"),
          locations = cells_column_labels()
     ) %>% 
     tab_options(table.font.size = 12,
                 heading.align = 'left', heading.title.font.size = px(16)) %>% 
     gt_theme_pff()
     
variacion_monografias

mejores_monografias 

notas_componentes_clean2 %>% 
     arrange(desc(puntuacion_total), calificacion) %>% 
     head(10) %>% 
     gt() %>% 
     cols_label(
          asignatura = "Asignatura",
          idioma = "Idioma",
          nombre = "Estudiante",
          puntuacion_total = "Puntuación",
          calificacion = "Calificación") %>%
     cols_align(align = "left",columns = everything()) %>% 
     tab_header(title = md("**Los 10 mejores trabajos de Monografía - Mayo 2023**")) %>% 
     tab_style(
          style = cell_text(weight = "bold", color = "black", size = 10),
          locations = cells_column_labels()
     ) %>% 
     tab_style(
          style = cell_text(color = "black", size = 9),
          locations = cells_body()) %>% 
     tab_options(table.font.size = 12,
                 heading.align = 'left',
                 heading.title.font.size = 16, heading.title.font.weight = "bold")

mejores_monografias

### Teoría del Conocimiento 
notas_componentes_clean3 <- notas_componentes %>% 
     clean_names() %>% 
     select(c(3,4,5,6,10,12,13,14,15,16,17)) %>%
     mutate(asignatura = if_else(asignatura == "INFORMÁTICA", 
                                 "COMPUTER SCIENCE", asignatura)) %>%
     mutate(asignatura = as_factor(asignatura),
            nivel = as_factor(nivel),
            componente = as_factor(componente)) %>% 
     rename(puntuacion_total = puntuacion_total_ajustada_de_la_asignatura) %>%
     filter(nivel == "TdC") %>% 
     select(c(1,4,5,10, 11)) %>% 
     rename(calificacion = calificacion_final_de_la_asignatura) %>% 
     mutate(asignatura = as_factor(asignatura),
            calificacion = as_factor(calificacion))

calificacion_tdc <- notas_componentes_clean3 %>% 
     mutate(calificacion = factor(calificacion, levels = c("A", "B", "C", "D", "E"))) %>% 
     group_by(calificacion) %>% 
     summarise(estudiantes = n()) %>% 
     ungroup() %>% 
     ggplot(aes(x = calificacion, y = estudiantes)) +
     geom_bar(stat = "identity", fill = "aquamarine3") +
     geom_text(aes(label = estudiantes), size = 6, vjust = 1.4, colour = "black") +
     labs(x = "Calificación", y = "Número de estudiantes") +
     theme_minimal() +
     theme(axis.title = element_text(family = "sans", face="bold", color = "black"),
           axis.text = element_text(family = "sans", face="bold", color = "black", size = 14),
           legend.title = element_blank(),
           legend.position = "bottom")

calificacion_tdc


## Notas predichas vs. notas obtenidas

resultados_asignaturas_2023 <- read_csv("resultados_asignaturas_2023.csv")

gg1 <- c("ESPAÑOL A Literatura")
gg2 <- c("INGLÉS B", "FRANCÉS AB.")
gg3 <- c("POLÍTICA GLOBAL", "HISTORIA")
gg4 <- c("FÍSICA", "QUÍMICA")
gg4_b <- c("BIOLOGÍA",  "COMPUTER SCIENCE")
gg5 <- c("MATEMÁTICAS: ANÁLISIS")

# mutate(across(all_of(c("asignatura", "nivel", 
                      # "calificacion_prevista", "calificacion")), as.factor)) 

predicha_obtenida_asignaturas <- resultados_asignaturas_2023  %>% 
     clean_names() %>%
     mutate(asignatura = if_else(asignatura == "INFORMÁTICA", 
                                 "COMPUTER SCIENCE", asignatura)) %>% 
     filter(nivel != "Monografía" & nivel != "TdC") %>% 
     select(nombre, asignatura, nivel, calificacion_prevista, calificacion) %>% 

## funcion para el desarrollo de los gráficos

plot_predicha_vs_obtenida <- function(data, GRUPO) {
     data %>%
          filter(asignatura %in% GRUPO) %>%
          ggplot(aes(x = calificacion_prevista, y = calificacion, color = nivel, shape = asignatura)) +
          geom_jitter(width = 0.5, height = 0.5, size = 3) +
          geom_abline(intercept = 0, slope = 1, color = "purple", linetype = "solid") +
          labs(x = "Nota prevista", y = "Nota obtenida") +
          theme_minimal() +
          scale_color_brewer(palette = "Dark2") +
          theme(
               axis.title = element_text(face = "bold"),
               axis.text = element_text(face = "bold"),
               legend.position = "bottom",
               legend.title = element_blank(),
               plot.title = element_text(face = "bold", family = "sans", size = 13)
          )
}

plot_predicha_vs_obtenida(predicha_obtenida_asignaturas, gg5)

### Diferencias previstas y obtenidas

predicha_obtenida_asignaturas %>% 
     mutate(asignatura = as_factor(asignatura),
            calificacion = as.numeric(calificacion),
            calificacion_prevista = as.numeric(calificacion_prevista)) %>% 
     mutate(diferencia = calificacion - calificacion_prevista) %>% 
     group_by(asignatura) %>% 
     summarize(promedio_diferencia = mean(diferencia)) %>% 
     ungroup() %>% 
     ggplot(aes(fct_reorder(asignatura, promedio_diferencia), promedio_diferencia, fill = asignatura)) +
     geom_col() +
     geom_text(aes(label = round(promedio_diferencia, 2)), size = 4,
               vjust = 1, hjust = 0.35, colour = "black") +
     geom_hline(yintercept = 0) +
     coord_flip() +
     labs(y = "Promedio Diferencia") + 
     #scale_fill_viridis_d(alpha = 0.5) +
     theme_minimal() +
     theme(axis.title = element_text(family = "sans", face="bold", color = "black"),
           axis.title.y = element_blank(),
           axis.text = element_text(family = "sans", face="bold", color = "black"),
           legend.title = element_blank(),
           legend.position = "none",
           text = element_text(family = "sans", face = "bold", color = "black", size = 12)
     )

#| label: fig-trabajos-internos
#| fig-cap: "Distribución de calificación de trabajos internos"

notas_TI <- notas_componentes %>% 
     clean_names() %>% 
     select(10,3,5,13,15) %>% 
     mutate(asignatura = ifelse(asignatura == "MATEMÁTICAS: ANÁLISIS Y ENFOQUES", 
                                "MATEMÁTICAS", asignatura)) %>% 
     mutate(across(all_of(c("asignatura", "componente")), as.factor)) %>% 
     filter(!str_detect(componente, "PRUEBA")) %>% 
     filter(componente != "MONOGRAFÍA" & componente != "TEORÍA DEL CONOCIMIENTO")

### anotaciones
rango_esp1<- "Puntos máximos del Ensayo NS"
rango_esp2<- "Puntos máximos de EOI"
rango_eng <- "Puntos máximos de EOI"
rango_gp_his1 <- "Puntos máximos TI y Extensión Oral GP"
rango_gp_his2 <- "Puntos máximos TI Historia"
rango_cien1 <- "Puntos máximos TI Ciencias (B,Q,M)"
rango_cien2 <- "Puntos máximos TI Computer Science"
rango_mat <- "Puntos máximos TI Matemáticas" 

español_TI  <-notas_TI %>% 
     filter(str_detect(asignatura, "ESPAÑOL")) %>% 
     ggplot(aes(asignatura, calificacion_moderada, fill = componente)) +
     geom_boxplot(alpha = 0.6, outlier.shape=NA) +
     geom_point(position = position_jitterdodge(jitter.width = 0.8,
                                                dodge.width = 0.8),
                color = "brown", size = 1.8, alpha = 0.80) +
     geom_hline(yintercept = 20, linetype = "dashed", color = "lightgreen", size = 1.25) + 
     geom_hline(yintercept = 40, linetype = "dashed", color = "coral", size = 1.25) +
     annotate(geom = "text", x = 0.2, y = 21, label = rango_esp1, hjust = 0,
              size = 2.5, color ="black", fontface = "bold") + 
     annotate(geom = "text", x = 0.2, y = 39, label = rango_esp2, hjust = 0,
              size = 2.5, color ="black", fontface = "bold") + 
     scale_y_continuous(limits = c(0,42), n.breaks = 5) +
     labs(y = "Calificación Moderada") + 
     #scale_fill_viridis_d(alpha = 0.5) +
     theme_minimal() +
     theme(axis.title = element_text(family = "sans", face="bold", color = "black"),
           axis.title.x = element_blank(),
           axis.text = element_text(family = "sans", face="bold", color = "black"),
           legend.title = element_blank(),
           legend.position = "none",
           text = element_text(family = "sans", face = "bold", color = "black", size = 12)
     )

english_frances_TI <- notas_TI %>% 
     filter(str_detect(asignatura, "INGLÉS|FRANCÉS")) %>% 
     ggplot(aes(asignatura, calificacion_moderada, fill = asignatura)) +
     geom_boxplot(alpha = 0.6, outlier.shape=NA) +
     geom_point(position = position_jitterdodge(jitter.width = 0.8,
                                                dodge.width = 0.8),
                color = "brown", size = 1.8, alpha = 0.80) +
     geom_hline(yintercept = 30, linetype = "dashed", color = "coral", size = 1.25) +
     annotate(geom = "text", x = 0.2, y = 31, label = rango_esp2, hjust = 0,
              size = 3, fontface = "bold", color ="black") + 
     scale_y_continuous(limits = c(0,35), n.breaks = 5) +
     labs(y = "Calificación Moderada") + 
     #scale_fill_viridis_d(alpha = 0.5) +
     theme_minimal() +
     theme(axis.title = element_text(family = "sans", face="bold", color = "black"),
           axis.title.x = element_blank(),
           axis.text = element_text(family = "sans", face="bold", color = "black"),
           legend.title = element_blank(),
           legend.position = "none",
           text = element_text(family = "sans", face = "bold", color = "black", size = 12)
     )

global_historia_TI <- notas_TI %>% 
     filter(str_detect(asignatura, "POLÍTICA|HISTORIA")) %>% 
     ggplot(aes(asignatura, calificacion_moderada, fill = asignatura, shape = componente)) +
     geom_boxplot(alpha = 0.6, outlier.shape=NA) +
     geom_point(aes(fill = componente, color = componente), position = position_jitterdodge(jitter.width = 0.8,
                                                dodge.width = 0.8),
                size = 1.8, alpha = 0.80) +
     geom_hline(yintercept = 20, linetype = "dashed", color = "coral", size = 1.25) +
     geom_hline(yintercept = 25, linetype = "dashed", color = "lightgreen", size = 1.25) +
     annotate(geom = "text", x = 0.2, y = 21, label = rango_gp_his1, hjust = 0,
              size = 3, fontface = "bold", color ="black") +
     annotate(geom = "text", x = 0.2, y = 26, label = rango_gp_his2, hjust = 0,
              size = 3, fontface = "bold", color ="black") +
     scale_y_continuous(limits = c(0,30), n.breaks = 5) +
     labs(y = "Calificación Moderada") + 
     #scale_fill_viridis_d(alpha = 0.5) +
     theme_minimal() +
     theme(axis.title = element_text(family = "sans", face="bold", color = "black"),
           axis.title.x = element_blank(),
           axis.text = element_text(family = "sans", face="bold", color = "black"),
           legend.title = element_blank(),
           legend.position = "none",
           text = element_text(family = "sans", face = "bold", color = "black", size = 12)
     )

ciencias_matematicas_TI 
notas_TI %>%
     filter(str_detect(asignatura, "BIOLOGÍA|FÍSICA|QUÍMICA|INFORMÁTICA|MATEMÁTICAS")) %>%
     mutate(asignatura = factor(asignatura, levels = c("BIOLOGÍA", "FÍSICA", 
                                                       "QUÍMICA", "INFORMÁTICA", "MATEMÁTICAS"))) %>% 
     ggplot(aes(asignatura, calificacion_moderada, fill = asignatura)) +
     geom_boxplot(alpha = 0.6, outlier.shape=NA) +
     geom_point(position = position_jitterdodge(jitter.width = 0.8,
                                                dodge.width = 0.8),
                color = "brown", size = 1.8, alpha = 0.80) +
     geom_hline(yintercept = 34, linetype = "dashed", color = "lightgreen", size = 1.25) +
     annotate(geom = "text", x = 0.2, y = 35, label = rango_cien2, hjust = 0,
              size = 3, fontface = "bold", color ="black") +
     geom_hline(yintercept = 24, linetype = "dashed", color = "coral", size = 1.25) +
     annotate(geom = "text", x = 0.2, y = 21, label = rango_mat, hjust = 0,
              size = 3, fontface = "bold", color ="black") +
     geom_hline(yintercept = 20, linetype = "dashed", color = "darkcyan", size = 1.25) +
     annotate(geom = "text", x = 0.2, y = 25, label = rango_cien1, hjust= 0,
              size = 3, fontface = "bold", color ="black") +
     scale_y_continuous(limits = c((min(notas_TI$calificacion_moderada) -1), 35), n.breaks = 5) +
     labs(y = "Calificación Moderada") + 
     #scale_fill_viridis_d(alpha = 0.5) +
     theme_minimal() +
     theme(axis.title = element_text(family = "sans", face="bold", color = "black"),
           axis.title.x = element_blank(),
           axis.text = element_text(family = "sans", face="bold", color = "black"),
           legend.title = element_blank(),
           legend.position = "none",
           text = element_text(family = "sans", face = "bold", color = "black", size = 12)
     )
## NOTAS TDC
notas_tdc <-notas_componentes |> 
  clean_names() |> 
  select(nombre, asignatura, calificacion_final_de_la_asignatura) |> 
  rename(calificacion = calificacion_final_de_la_asignatura) |> 
  mutate(calificacion = factor(calificacion,
                               levels = c("A", "B", "C", "D", "E"))) |> 
  filter(asignatura == "TEORÍA DEL CONOCIMIENTO") |> 
  distinct(nombre, asignatura, calificacion) |> 
  group_by(calificacion) |> 
  summarize(estudiantes = n()) |> 
  ggplot(aes(calificacion, estudiantes)) +
  geom_col()

# PUNTOS ADICIONALES
######