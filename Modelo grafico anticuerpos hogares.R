## Graficos finales con 5 puntos

## Librerias ----

library(readxl)
library(lubridate)
library(ggprism)
library(ggpubr)
library(rstatix)
library(ggrepel)
library(scales)
library(ggdist)
library(tidyverse)
library(Hmisc)

# PAra sacar la notacion cientifica de las etiquetas en los ejes.
options(scipen = 999)

## lectura y gestion de datos ----
setwd("D:/LabSEVA/PAMI/Analisis en R")

datos <- read_excel("Script Andres Rossi/Version 22 de Mayo de 2022/20220520 Respuesta inmune humoral vacuna COVID-19.xlsx", sheet = 4) %>% 
  filter(!is.na(ID))

datos <- datos %>% mutate(Lugar = if_else(str_detect(ID, pattern = "^A"), "MDP", "LA PLATA/LANUS"),
                          Edad_calculada = if_else(Lugar == "MDP", interval(Fecha_nacimiento, Fecha_muestra_0)%/%dyears(),Edad),
                          baseline = case_when(
                            Título_0 == "NR" ~ 0,
                            str_detect(string = Título_0,  "/") ~ as.numeric(str_sub(string = Título_0, start = str_locate(Título_0, "/")[,2]+1, end = nchar(Título_0))) 
                          ),
                          after_dosis1 = case_when(
                            Titulo_21 == "NR" ~ 0,
                            str_detect(string = Titulo_21,  "/") ~ as.numeric(str_sub(string = Titulo_21, start = str_locate(Titulo_21, "/")[,2]+1, end = nchar(Titulo_21))) 
                          ),
                          after_dosis2 = case_when(
                            Titulo_42 == "NR" ~ 0,
                            str_detect(string = Titulo_42,  "/") ~ as.numeric(str_sub(string = Titulo_42, start = str_locate(Titulo_42, "/")[,2]+1, end = nchar(Titulo_42))) 
                          ),
                          after_120 = case_when(
                            Título_120   == "NR" ~ 0,
                            str_detect(string = Título_120,  "/") ~ as.numeric(str_sub(string = Título_120, start = str_locate(Título_120, "/")[,2]+1, end = nchar(Título_120))) 
                          ),
                          after_180 = case_when(
                            Título_180 == "NR" ~ 0,
                            str_detect(string = Título_180,  "/") ~ as.numeric(str_sub(string = Título_180, start = str_locate(Título_180, "/")[,2]+1, end = nchar(Título_180))) 
                          ),
                          GrupoEdad = if_else(Edad_calculada < 80, "< 80 years", ">= 80 years"),
                          ANT_COVID = str_to_upper(ANT_COVID),
                          Vacuna = if_else(Vacuna == "Sputnik V", "Sputnik-V", Vacuna))

## AHR Traigo y Sumo los datos de Neutralizantes ----

neu <- read_excel("Script Andres Rossi/Version 22 de Mayo de 2022/Neutralizantes_20220425.xlsx")

datos_N <- datos %>% left_join(neu, by = c("ID" = "ID"))


datos_N <- datos_N %>% dplyr::select(ID, Lugar, SEXO, Edad_calculada, GrupoEdad, Vacuna, ANT_COVID,
                                     FECHA_HISOPADO, Tipo_diagnostico, COVID_durante_estudi, OBITO, Fecha_obito,
                                     baseline, after_dosis1, after_dosis2, after_120, after_180,
                                     Motivo_fin_seguimiento,Neu_post_d1,Neu_post_d2,Neu_post_120,Neu_post_180,Chequeado)  %>% 
  filter(Edad_calculada>=60) %>% mutate(ANT_COVID2 = if_else(baseline == 0, "NO", "SI"),
                                        Vacuna = factor(Vacuna, levels = c("Sputnik-V", "Sinopharm", "Oxford-Astra Zeneca")))
  



datos_largo1 <- datos_N %>% dplyr::select(ID, Lugar, SEXO, Edad_calculada, Vacuna, ANT_COVID, GrupoEdad, baseline, after_dosis1,
                                         after_dosis2, after_120, after_180,Neu_post_d1,Neu_post_d2,Neu_post_120,Neu_post_180, ANT_COVID2) %>% 
  pivot_longer(cols = c("baseline", starts_with("after")), names_to = "momento", values_to = "titulo") %>%
  mutate(titulo_cuali = if_else(titulo == 0, "NR", "R"),
         momento = factor(momento, levels = c("baseline", "after_dosis1", "after_dosis2", "after_120", "after_180")))

datos_largo2 <- datos_N %>% dplyr::select(ID, Lugar, SEXO, Edad_calculada, Vacuna, ANT_COVID, GrupoEdad, baseline, after_dosis1,
                                             after_dosis2, after_120, after_180,Neu_post_d1,Neu_post_d2,Neu_post_120,Neu_post_180, ANT_COVID2) %>% 
  pivot_longer(cols = c(starts_with("Neu")), names_to = "momen_Neu", values_to = "Valor_Neu") %>% 
  mutate(momen_Neu = factor(momen_Neu, levels = c("Neu_post_d1","Neu_post_d2","Neu_post_120","Neu_post_180"))) 




datos_largo1 <- datos_largo1 %>% mutate(ID = factor(ID), 
                                        momento = factor(momento)) 

datos_largo2 <- datos_largo2 %>% mutate(ID = factor(ID), 
                                        momen_Neu = factor(momen_Neu)) 

datos_largo1 <- as.data.frame(datos_largo1) %>% arrange(momento, ID) 

datos_largo2 <- as.data.frame(datos_largo2) %>% arrange(momen_Neu, ID) 

datos_largo1 <- datos_largo1 %>% 
  mutate(Vacuna = factor(Vacuna, levels = c("Sputnik-V", "Sinopharm", "Oxford-Astra Zeneca")),
         momento = case_when(
           momento == "baseline" ~ "Baseline",
           momento == "after_dosis1" ~ "21 days \n post dose 1",
           momento == "after_dosis2" ~ "21 days \n post dose 2",
           momento == "after_120" ~ "120 days \n post dose 1",
           momento == "after_180" ~ "180 days \n post dose 1"
         ),
         momento = factor(momento, levels = c("Baseline", 
                                              "21 days \n post dose 1", 
                                              "21 days \n post dose 2",
                                              "120 days \n post dose 1",
                                              "180 days \n post dose 1"
         )))


datos_largo2 <- datos_largo2 %>% 
  mutate(Vacuna = factor(Vacuna, levels = c("Sputnik-V", "Sinopharm", "Oxford-Astra Zeneca")),
         momen_Neu = case_when(
           momen_Neu == "Neu_post_d1" ~ "21 days \n post dose 1",
           momen_Neu == "Neu_post_d2" ~ "21 days \n post dose 2",
           momen_Neu == "Neu_post_120" ~ "120 days \n post dose 1",
           momen_Neu == "Neu_post_180" ~ "180 days \n post dose 1"
         ),
         momen_Neu = factor(momen_Neu, levels = c("21 days \n post dose 1", 
                                                  "21 days \n post dose 2",
                                                  "120 days \n post dose 1",
                                                  "180 days \n post dose 1"
         )))

## funciones de calculo de media geometrica e IC 95 % (metodo boostrap) ----


mgIC.media <- function(x) {
  mgIC <- mean_cl_boot(x, conf.int = .95, B = 10000)
  mgIC$y
}

mgIC.min <- function(x) {
  mgIC <- mean_cl_boot(x, conf.int = .95, B = 10000)
  mgIC$ymin
}

mgIC.max <- function(x) {
  mgIC <- mean_cl_boot(x, conf.int = .95, B = 10000)
  mgIC$ymax
}

## grafico Vacunas ----

stat.test_A <- datos_largo1 %>% 
  filter(!is.na(Vacuna), ANT_COVID2 == "NO", ANT_COVID == "NO", momento != "Baseline", !is.na(titulo)) %>% 
  group_by(momento) %>% 
  wilcox_test(titulo ~ Vacuna) %>%    
  add_x_position(x = "momento", dodge = 0.9) %>% 
  add_significance("p.adj")

grafico_media_geometrica <- datos_largo1 %>% 
  filter(!is.na(Vacuna), ANT_COVID2 == "NO", ANT_COVID == "NO", !is.na(titulo)) %>%  
  mutate(titulo = if_else(titulo == 0, 1, titulo)) %>% 
  ggplot(aes(x = momento, y = titulo)) +
  geom_violin(aes(color = Vacuna),scale = "width") +
  geom_jitter(aes(color = Vacuna), position = position_jitterdodge(jitter.width = 0.3, 
                                                                   jitter.height = 0.1,
                                                                   dodge.width = 0.9), size = 2, alpha=0.3) +
  geom_hline(yintercept=50, linetype="dashed", color = "gray60") +
  stat_summary(aes(group = Vacuna), fun = mgIC.media, geom = "point", color="firebrick4", 
               position = position_dodge(width = 0.9)) +
  stat_summary(aes(group = Vacuna), fun = mgIC.media, 
               fun.min = mgIC.min,
               fun.max = mgIC.max,
               geom = "errorbar", color="firebrick4", linetype = 2, width = 0.5, 
               position = position_dodge(width = 0.9)) +
  scale_y_continuous(trans=scales::log10_trans(),
                     breaks = c(1,50,200,800,3200,12800, 51200, 204800),
                     minor_breaks = c(100,400,1600,6400,25600,102400,409600),
                     guide = "prism_offset_minor" ) +
  scale_color_manual(values = c("darkorchid4", "forestgreen", "dodgerblue4"),
                     name = "Vaccine",
                     labels=c(paste0("Sputnik V"),
                              paste0("COVID-19 BIBP"),
                              paste0("AZD1222"))) +
  xlab("") + 
  ylab("Titer IgG anti-Spike") +
  theme_prism(
    base_fontface = "plain", 
    base_line_size = 0.7, 
    base_family = "Arial", 
    base_size = 12
  ) +
  scale_x_discrete(
    guide = guide_prism_bracket(width = 0.20), 
    labels = scales::wrap_format(12)
  ) +
  stat_pvalue_manual(stat.test_A, label = "p.adj.signif", y.position = log10(409600),
                     tip.length = 0.01, step.increase = 0.05, step.group.by = "momento",
                     size = 4) +
  theme(legend.position = "bottom",
        legend.margin = margin(-25,0,0,0),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(size = 10)) 

grafico_media_geometrica

## conteo para cada vacuna en cada momento

datos_largo1 %>% filter(!is.na(Vacuna), ANT_COVID2 == "NO", ANT_COVID == "NO", !is.na(titulo)) %>%  
  mutate(titulo = if_else(titulo == 0, 1, titulo)) %>% count(momento,Vacuna)

## exportacion del grafico

ggsave(plot = grafico_media_geometrica, 
       filename = "Script Andres Rossi/grafico con n diferentes.jpg", 
       units = "cm", 
       width = 20, 
       height = 10,
       dpi = 600)


Tabla_A <- datos_largo1 %>% 
  filter(!is.na(Vacuna), ANT_COVID2 == "NO", ANT_COVID == "NO", !is.na(titulo)) %>%  
  mutate(titulo = if_else(titulo == 0, 1, titulo)) %>% 
  group_by(momento, Vacuna) %>% 
  summarise(conteo = n(),media_geometrica = exp(mgIC.media(log(titulo))),
            IC_min = exp(mgIC.min(log(titulo))),
            IC_max = exp(mgIC.max(log(titulo))))

Tabla_A_conv <- datos_largo1 %>% 
  filter(!is.na(Vacuna), ANT_COVID2 != "NO" | ANT_COVID != "NO", !is.na(titulo)) %>%  
  mutate(titulo = if_else(titulo == 0, 1, titulo)) %>% 
  group_by(momento, Vacuna) %>%
  summarise(conteo = n(),media_geometrica = exp(mgIC.media(log(titulo))),
            IC_min = exp(mgIC.min(log(titulo))),
            IC_max = exp(mgIC.max(log(titulo))))  

## Grafico B ---- Genero vs Momento en No infectados ----

stat.testB <- datos_largo1 %>% 
  filter(!is.na(SEXO), ANT_COVID2 == "NO", ANT_COVID == "NO", momento != "Baseline", !is.na(titulo)) %>% 
  group_by(momento) %>% 
  wilcox_test(titulo ~ SEXO) %>% 
  add_x_position(x = "momento", dodge = 0.9) %>% 
  add_significance("p")


### grafico modelo

grafico_media_geometrica <- datos_largo1 %>% 
  filter(!is.na(SEXO), ANT_COVID2 == "NO", ANT_COVID == "NO", !is.na(titulo)) %>%  
  mutate(titulo = if_else(titulo == 0, 1, titulo)) %>% 
  ggplot(aes(x = momento, y = titulo)) +
  geom_violin(aes(color = SEXO),scale = "width") +
  geom_jitter(aes(color = SEXO), position = position_jitterdodge(jitter.width = 0.3, 
                                                                 jitter.height = 0.1,
                                                                 dodge.width = 0.9), size = 2, alpha=0.3) +
  geom_hline(yintercept=50, linetype="dashed", color = "gray60") +
  stat_summary(aes(group = SEXO), fun = mgIC.media, geom = "point", color="firebrick4", 
               position = position_dodge(width = 0.9)) +
  stat_summary(aes(group = SEXO), fun = mgIC.media, 
               fun.min = mgIC.min,
               fun.max = mgIC.max,
               geom = "errorbar", color="firebrick4", linetype = 2, width = 0.5, 
               position = position_dodge(width = 0.9)) +
  scale_y_continuous(trans=scales::log10_trans(),
                     breaks = c(1,50,200,800,3200,12800, 51200, 204800),
                     minor_breaks = c(100,400,1600,6400,25600,102400,409600),
                     guide = "prism_offset_minor" ) +
  scale_color_manual(values = c("darkorange3", "olivedrab4"),
                     name = "Gender",
                     labels=c(paste0("Female"),
                              paste0("Male"))) +
  xlab("") + 
  ylab("Titer IgG anti-Spike") +
  theme_prism(
    base_fontface = "plain", 
    base_line_size = 0.7, 
    base_family = "Arial", 
    base_size = 12
  ) +
  scale_x_discrete(
    guide = guide_prism_bracket(width = 0.20), 
    labels = scales::wrap_format(12)
  ) +
  stat_pvalue_manual(stat.testB, label = "p.signif", y.position = log10(409600),
                     tip.length = 0.01, step.increase = 0.05, step.group.by = "momento",
                     size = 4) +
  theme(legend.position = "bottom",
        legend.margin = margin(-25,0,0,0),  
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(size = 10)) 

grafico_media_geometrica

## conteo para cada Genero en cada momento

datos_largo1 %>% filter(!is.na(SEXO), ANT_COVID2 == "NO", ANT_COVID == "NO", !is.na(titulo)) %>%  
  mutate(titulo = if_else(titulo == 0, 1, titulo)) %>% count(momento,SEXO)

## tabla con media geometrica e IC 95%

Tabla_B<-datos_largo1 %>% 
  filter(!is.na(SEXO), ANT_COVID2 == "NO", ANT_COVID == "NO", !is.na(titulo)) %>%  
  mutate(titulo = if_else(titulo == 0, 1, titulo)) %>% 
  group_by(momento, SEXO) %>% 
  summarise(media_geometrica = exp(mgIC.media(log(titulo))),
            IC_min = exp(mgIC.min(log(titulo))),
            IC_max = exp(mgIC.max(log(titulo))))

## Grafico B2 ---- Genero vs Momento en Infectados ----


stat.testB2 <- datos_largo1 %>% 
  filter(!is.na(SEXO), ANT_COVID2 == "SI" | ANT_COVID == "SI", !is.na(titulo)) %>% 
  group_by(momento) %>% 
  wilcox_test(titulo ~ SEXO) %>% 
  add_x_position(x = "momento", dodge = 0.9) %>% 
  add_significance("p")




### grafico modelo

grafico_media_geometrica <- datos_largo1 %>% 
  filter(!is.na(SEXO), ANT_COVID2 == "SI" | ANT_COVID == "SI", !is.na(titulo)) %>%  
  mutate(titulo = if_else(titulo == 0, 1, titulo)) %>% 
  ggplot(aes(x = momento, y = titulo)) +
  geom_violin(aes(color = SEXO),scale = "width") +
  geom_jitter(aes(color = SEXO), position = position_jitterdodge(jitter.width = 0.3, 
                                                                 jitter.height = 0.1,
                                                                 dodge.width = 0.9), size = 2, alpha=0.3) +
  geom_hline(yintercept=50, linetype="dashed", color = "gray60") +
  stat_summary(aes(group = SEXO), fun = mgIC.media, geom = "point", color="firebrick4", 
               position = position_dodge(width = 0.9)) +
  stat_summary(aes(group = SEXO), fun = mgIC.media, 
               fun.min = mgIC.min,
               fun.max = mgIC.max,
               geom = "errorbar", color="firebrick4", linetype = 2, width = 0.5, 
               position = position_dodge(width = 0.9)) +
  scale_y_continuous(trans=scales::log10_trans(),
                     breaks = c(0,50,200,800,3200,12800, 51200, 204800, 819200),
                     guide = "prism_offset_minor" ) +
  scale_color_manual(values = c("tan4", "darkorange"),
                     name = "Gender",
                     labels=c(paste0("Female"),
                              paste0("Male"))) +
  xlab("") + 
  ylab("Titer IgG anti-Spike") +
  theme_prism(
    base_fontface = "plain", 
    base_line_size = 0.7, 
    base_family = "Arial", 
    base_size = 12
  ) +
  scale_x_discrete(
    guide = guide_prism_bracket(width = 0.20), 
    labels = scales::wrap_format(12)
  ) +
  stat_pvalue_manual(stat.testB2, label = "p.signif", y.position = log10(819200),
                     tip.length = 0.01, step.increase = 0.05, step.group.by = "momento",
                     size = 4) +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(size = 10)) 

grafico_media_geometrica

## conteo para cada Genero en cada momento

datos_largo1 %>% filter(!is.na(SEXO), ANT_COVID2 == "SI" | ANT_COVID == "SI", !is.na(titulo)) %>%  
  mutate(titulo = if_else(titulo == 0, 1, titulo)) %>% count(momento,SEXO)

## tabla con media geomíƒÂ©trica e IC 95%

Tabla_B2<-datos_largo1 %>% 
  filter(!is.na(SEXO), ANT_COVID2 == "SI" | ANT_COVID == "SI", !is.na(titulo)) %>%  
  mutate(titulo = if_else(titulo == 0, 1, titulo)) %>% 
  group_by(momento, SEXO) %>% 
  summarise(media_geometrica = exp(mgIC.media(log(titulo))),
            IC_min = exp(mgIC.min(log(titulo))),
            IC_max = exp(mgIC.max(log(titulo))))


## Grafico C ---- Vacuna vs Edad en No infectados ----


stat.test_C <- datos_largo1 %>% 
  filter(!is.na(GrupoEdad), ANT_COVID2 == "NO", ANT_COVID == "NO", momento != "Baseline", !is.na(titulo)) %>% 
  group_by(momento) %>% 
  wilcox_test(titulo ~ GrupoEdad) %>% 
  add_x_position(x = "momento", dodge = 0.9) %>% 
  add_significance("p")


### gríƒÂ¡fico modelo

grafico_media_geometrica <- datos_largo1 %>% 
  filter(!is.na(GrupoEdad), ANT_COVID2 == "NO", ANT_COVID == "NO", !is.na(titulo)) %>%  
  mutate(titulo = if_else(titulo == 0, 1, titulo)) %>% 
  ggplot(aes(x = momento, y = titulo)) +
  geom_violin(aes(color = GrupoEdad),scale = "width") +
  geom_jitter(aes(color = GrupoEdad), position = position_jitterdodge(jitter.width = 0.3, 
                                                                      jitter.height = 0.1,
                                                                      dodge.width = 0.9), size = 2, alpha=0.3) +
  geom_hline(yintercept=50, linetype="dashed", color = "gray60") +
  stat_summary(aes(group = GrupoEdad), fun = mgIC.media, geom = "point", color="firebrick4", 
               position = position_dodge(width = 0.9)) +
  stat_summary(aes(group = GrupoEdad), fun = mgIC.media, 
               fun.min = mgIC.min,
               fun.max = mgIC.max,
               geom = "errorbar", color="firebrick4", linetype = 2, width = 0.5, 
               position = position_dodge(width = 0.9)) +
  scale_y_continuous(trans=scales::log10_trans(),
                     breaks = c(1,50,200,800,3200,12800, 51200, 204800),
                     minor_breaks = c(100,400,1600,6400,25600,102400,409600),
                     guide = "prism_offset_minor" ) +
  scale_color_manual(values = c("goldenrod", "palevioletred4"),
                     name = "Age Group",
                     labels=c(paste0("< 80 years"),
                              paste0(">= 80 years"))) +
  xlab("") + 
  ylab("Titer IgG anti-Spike") +
  theme_prism(
    base_fontface = "plain", 
    base_line_size = 0.7, 
    base_family = "Arial", 
    base_size = 12
  ) +
  scale_x_discrete(
    guide = guide_prism_bracket(width = 0.20), 
    labels = scales::wrap_format(12)
  ) +
  stat_pvalue_manual(stat.test_C, label = "p.signif", y.position = log10(409600),
                     tip.length = 0.01, step.increase = 0.05, step.group.by = "momento",
                     size = 4) +
  theme(legend.position = "bottom",
        legend.margin = margin(-25,0,0,0),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(size = 10)) 

grafico_media_geometrica

## conteo para cada Edad en cada momento

datos_largo1 %>% filter(!is.na(GrupoEdad), ANT_COVID2 == "NO", ANT_COVID == "NO", !is.na(titulo)) %>%  
  mutate(titulo = if_else(titulo == 0, 1, titulo)) %>% count(momento,SEXO)

## tabla con media geomíƒÂ©trica e IC 95%

Tabla_C<-datos_largo1 %>% 
  filter(!is.na(GrupoEdad), ANT_COVID2 == "NO", ANT_COVID == "NO", !is.na(titulo)) %>%  
  mutate(titulo = if_else(titulo == 0, 1, titulo)) %>% 
  group_by(momento, GrupoEdad) %>% 
  summarise(media_geometrica = exp(mgIC.media(log(titulo))),
            IC_min = exp(mgIC.min(log(titulo))),
            IC_max = exp(mgIC.max(log(titulo))))


## Grafico C-2 ---- Vacuna vs Edad en infectados ----


stat.test_C2 <- datos_largo1 %>% 
  filter(!is.na(GrupoEdad), ANT_COVID2 == "SI"| ANT_COVID == "SI", !is.na(titulo)) %>% 
  group_by(momento) %>% 
  wilcox_test(titulo ~ GrupoEdad) %>% 
  add_x_position(x = "momento", dodge = 0.9) %>% 
  add_significance("p")


### gríƒÂ¡fico modelo

grafico_media_geometrica <- datos_largo1 %>% 
  filter(!is.na(GrupoEdad), ANT_COVID2 == "SI" | ANT_COVID == "SI", !is.na(titulo)) %>%  
  mutate(titulo = if_else(titulo == 0, 1, titulo)) %>% 
  ggplot(aes(x = momento, y = titulo)) +
  geom_violin(aes(color = GrupoEdad),scale = "width") +
  geom_jitter(aes(color = GrupoEdad), position = position_jitterdodge(jitter.width = 0.3, 
                                                                      jitter.height = 0.1,
                                                                      dodge.width = 0.9), size = 2, alpha=0.3) +
  geom_hline(yintercept=50, linetype="dashed", color = "gray60") +
  stat_summary(aes(group = GrupoEdad), fun = mgIC.media, geom = "point", color="firebrick4", 
               position = position_dodge(width = 0.9)) +
  stat_summary(aes(group = GrupoEdad), fun = mgIC.media, 
               fun.min = mgIC.min,
               fun.max = mgIC.max,
               geom = "errorbar", color="firebrick4", linetype = 2, width = 0.5, 
               position = position_dodge(width = 0.9)) +
  scale_y_continuous(trans=scales::log10_trans(),
                     breaks = c(0,50,200,800,3200,12800, 51200, 204800, 819200),
                     guide = "prism_offset_minor" ) +
  scale_color_manual(values = c("brown1", "dodgerblue"),
                     name = "Age Group",
                     labels=c(paste0("< 80 years"),
                              paste0(">= 80 years"))) +
  xlab("") + 
  ylab("Titer IgG anti-Spike") +
  theme_prism(
    base_fontface = "plain", 
    base_line_size = 0.7, 
    base_family = "Arial", 
    base_size = 12
  ) +
  scale_x_discrete(
    guide = guide_prism_bracket(width = 0.20), 
    labels = scales::wrap_format(12)
  ) +
  stat_pvalue_manual(stat.test_C2, label = "p.signif", y.position = log10(819200),
                     tip.length = 0.01, step.increase = 0.05, step.group.by = "momento",
                     size = 4) +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(size = 10)) 

grafico_media_geometrica

## conteo para cada Edad en cada momento

datos_largo1 %>% filter(!is.na(GrupoEdad), ANT_COVID2 == "SI"| ANT_COVID == "SI", !is.na(titulo)) %>%  
  mutate(titulo = if_else(titulo == 0, 1, titulo)) %>% count(momento,SEXO)

## tabla con media geomíƒÂ©trica e IC 95%

Tabla_C2<-datos_largo1 %>% 
  filter(!is.na(GrupoEdad), ANT_COVID2 == "SI"| ANT_COVID == "SI", !is.na(titulo)) %>%  
  mutate(titulo = if_else(titulo == 0, 1, titulo)) %>% 
  group_by(momento, GrupoEdad) %>% 
  summarise(media_geometrica = exp(mgIC.media(log(titulo))),
            IC_min = exp(mgIC.min(log(titulo))),
            IC_max = exp(mgIC.max(log(titulo))))


## Grafico D ---- Vacuna vs Neutralizantes ----


stat.test_D <- datos_largo2 %>% 
  filter(!is.na(Vacuna), ANT_COVID2 == "NO", ANT_COVID == "NO", !is.na(Valor_Neu)) %>% 
  group_by(momen_Neu) %>% 
  wilcox_test(Valor_Neu ~ Vacuna) %>%    
  add_x_position(x = "momen_Neu", dodge = 0.9) %>% 
  add_significance("p.adj")


### grafico modelo

grafico_media_geometrica <- datos_largo2 %>% 
  filter(!is.na(Vacuna), ANT_COVID2 == "NO", ANT_COVID == "NO", !is.na(Valor_Neu)) %>%  
  mutate(Valor_Neu = if_else(Valor_Neu < 1, 1, Valor_Neu)) %>% 
  ggplot(aes(x = momen_Neu, y = Valor_Neu)) +
  geom_violin(aes(color = Vacuna),scale = "width") +
  geom_jitter(aes(color = Vacuna), position = position_jitterdodge(jitter.width = 0.3, 
                                                                   jitter.height = 0.1,
                                                                   dodge.width = 0.9), size = 2, alpha=0.3) +
  stat_summary(aes(group = Vacuna), fun = mgIC.media, geom = "point", color="firebrick4", 
               position = position_dodge(width = 0.9)) +
  stat_summary(aes(group = Vacuna), fun = mgIC.media, 
               fun.min = mgIC.min,
               fun.max = mgIC.max,
               geom = "errorbar", color="firebrick4", linetype = 2, width = 0.5, 
               position = position_dodge(width = 0.9)) +
  scale_y_continuous(trans=scales::log10_trans(),
                     breaks = c(1,10,100,1000,10000,100000),
                     minor_breaks = c(3,30,300,3000,30000),
                     guide = "prism_offset_minor" ) +
  scale_color_manual(values = c("darkorchid4", "forestgreen", "dodgerblue4"),
                     name = "Vaccine",
                     labels=c(paste0("Sputnik V"),
                              paste0("COVID-19 BIBP"),
                              paste0("AZD1222"))) +
  xlab("") + 
  ylab("Neutralizing titer CoV2pp-GFP (IC50)") +
  theme_prism(
    base_fontface = "plain", 
    base_line_size = 0.7, 
    base_family = "Arial", 
    base_size = 12
  ) +
  scale_x_discrete(
    guide = guide_prism_bracket(width = 0.20), 
    labels = scales::wrap_format(12)
  ) +
  stat_pvalue_manual(stat.test_D, label = "p.adj.signif", y.position = log10(75000),
                     tip.length = 0.01, step.increase = 0.05, step.group.by = "momen_Neu",
                     size = 4) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.margin = margin(-25,0,0,0),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(size = 10)) 

grafico_media_geometrica


## conteo para cada Vacuna en cada momento

datos_largo2 %>% filter(!is.na(Vacuna), ANT_COVID2 == "NO", ANT_COVID == "NO", !is.na(Valor_Neu)) %>%  
   count(momen_Neu, Vacuna)

## tabla con media geomíƒÂ©trica e IC 95%

Tabla_D<- datos_largo2 %>% 
  filter(!is.na(Vacuna), ANT_COVID2 == "NO", ANT_COVID == "NO", !is.na(Valor_Neu)) %>%  
  mutate(Valor_Neu = if_else(Valor_Neu < 1 , 1, Valor_Neu)) %>% 
  group_by(momen_Neu, Vacuna) %>% 
  summarise(conteo = n(), media_geometrica = exp(mgIC.media(log(Valor_Neu))),
            IC_min = exp(mgIC.min(log(Valor_Neu))),
            IC_max = exp(mgIC.max(log(Valor_Neu))))



