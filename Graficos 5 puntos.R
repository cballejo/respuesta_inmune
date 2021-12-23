## Graficos finales con 5 puntos

library(readxl)
library(lubridate)
library(ggprism)
library(ggpubr)
library(rstatix)
library(ggrepel)
library(scales)
library(ggdist)
library(tidyverse)

## lectura y gestión de datos

datos <- read_excel("Relevamiento de datos - dic 2021.xlsx", sheet = 2) %>% 
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
                          GrupoEdad = if_else(Edad_calculada < 80, "< 80 años", ">= 80 años"),
                          ANT_COVID = str_to_upper(ANT_COVID),
                          Vacuna = if_else(Vacuna == "Sputnik V", "Sputnik-V", Vacuna))

datos <- datos %>% dplyr::select(ID, HOGAR, Lugar, SEXO, Edad_calculada, GrupoEdad, Vacuna, ANT_COVID,
                                 FECHA_HISOPADO, Tipo_diagnostico, COVID_durante_estudi, OBITO, Fecha_obito,
                                 baseline, after_dosis1, after_dosis2, after_120, after_180,
                                 Motivo_fin_seguimiento)   


datos_largo <- datos %>% dplyr::select(ID, Lugar, SEXO, Edad_calculada, Vacuna, ANT_COVID, GrupoEdad, baseline, after_dosis1, after_dosis2, after_120, after_180) %>% 
  pivot_longer(cols = c("baseline", starts_with("after")), names_to = "momento", values_to = "titulo") %>% 
  mutate(titulo_cuali = if_else(titulo == 0, "NR", "R"),
         momento = factor(momento, levels = c("baseline", "after_dosis1", "after_dosis2", "after_120", "after_180")))

excluir <- datos_largo %>% filter(is.na(titulo)) %>% dplyr::select(ID) %>% distinct()

datos1 <- datos %>% anti_join(excluir) %>% 
  mutate(ANT_COVID2 = if_else(baseline == 0, "NO", "SI"),
         Vacuna = factor(Vacuna, levels = c("Sputnik-V", "Sinopharm", "Oxford-Astra Zeneca")))

# datos1 <- datos %>% anti_join(excluir) %>% 
#   mutate(ANT_COVID2 = if_else(ANT_COVID == "NO" & baseline > 0, "SI", ANT_COVID))

datos_largo1 <- datos1 %>% dplyr::select(ID, Lugar, SEXO, Edad_calculada, Vacuna, ANT_COVID, ANT_COVID2, GrupoEdad, baseline, after_dosis1, after_dosis2, after_120, after_180) %>% 
  pivot_longer(cols = c("baseline", starts_with("after")), names_to = "momento", values_to = "titulo") %>% 
  mutate(titulo_cuali = if_else(titulo == 0, "NR", "R"),
         momento = factor(momento, levels = c("baseline", "after_dosis1", "after_dosis2", "after_120", "after_180")))

datos_largo1 <- datos_largo1 %>% mutate(ID = factor(ID), 
                                        momento = factor(momento)) 

datos_largo1 <- as.data.frame(datos_largo1) %>% arrange(momento, ID) 


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

### Graficos sobre vacunas ----

## Panel Figura 1

## Grafico A ----

med <- datos_largo1 %>% filter(!is.na(Vacuna), ANT_COVID2 == "NO") %>%
  group_by(Vacuna, momento) %>% 
  summarise(titulo = median(titulo, na.rm = T))

conteo <- datos1 %>% filter(!is.na(Vacuna), ANT_COVID2 == "NO") %>% 
  count(Vacuna)

vacuna_etiq <- c(
  `Sputnik-V` = paste0("Sputnik V\n(n = ",conteo[1,2],")"),
  `Sinopharm` = paste0("COVID-19 BIBP\n(n = ",conteo[2,2],")"),
  `Oxford-Astra Zeneca` = paste0("AZD1222\n(n = ",conteo[3,2],")")
)


stat.test <- datos_largo1 %>% filter(!is.na(Vacuna), ANT_COVID2 == "NO") %>% 
  group_by(Vacuna) %>% 
  pairwise_wilcox_test(titulo ~ momento, paired = T) %>% 
  mutate(id = row_number()) %>% 
  filter(id %in% c(2,5,8,10,12,15,18,20,22,25,28,30)) %>% 
  mutate(id = as.double(id),
         id = if_else(id == 2, 11, id),
         id = if_else(id == 12, 21, id),
         id = if_else(id == 22, 31, id)) %>% 
  arrange(id)
  

g1_panelA <- datos_largo1 %>% filter(!is.na(Vacuna), ANT_COVID2 == "NO") %>%  
  ggplot(aes(x = momento, y = titulo)) +
  geom_violin(scale = "width") +
  geom_jitter(aes(color = Vacuna), size = 2, width=0.1, height=0.8, alpha=0.3) +
  geom_hline(yintercept=50, linetype="dashed", color = "gray60") +
  stat_summary(fun=median, 
               fun.min = function(z) {quantile(z,0.25)},
               fun.max = function(z) {quantile(z,0.75)},
               geom="pointrange", color="darkred") +
  # geom_label(data = med, aes(x = momento, y = titulo, label = titulo), nudge_x = 0.05, hjust = 0, size = 3) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 2, sigma = 10),
                     breaks = c(0,50,200,800,3200,12800, 51200, 204800, 819200), 
                     guide = "prism_offset_minor", expand = expansion(mult = c(0.01, 0.1))) +
  scale_color_manual(values = c("darkorchid4", "forestgreen", "dodgerblue4")) +
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
    labels = scales::wrap_format(7)
  ) +
  facet_grid(.~Vacuna, labeller = as_labeller(vacuna_etiq)) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", y.position = log2(102400),
                     tip.length = 0.01, step.increase = 0.05, step.group.by = "Vacuna",
                     size = 4) +
  guides(color = "none")  +
  theme(plot.caption = element_text(hjust = 0))


## Grafico B ----

med <- datos_largo1 %>% filter(!is.na(Vacuna), ANT_COVID2 == "SI") %>%
  group_by(Vacuna, momento) %>% 
  summarise(titulo = median(titulo, na.rm = T))

conteo <- datos1 %>% filter(!is.na(Vacuna), ANT_COVID2 == "SI") %>% 
  count(Vacuna)

vacuna_etiq <- c(
  `Sputnik-V` = paste0("Sputnik V\n(n = ",conteo[1,2],")"),
  `Sinopharm` = paste0("COVID-19 BIBP\n(n = ",conteo[2,2],")"),
  `Oxford-Astra Zeneca` = paste0("AZD1222\n(n = ",conteo[3,2],")")
)


stat.test <- datos_largo1 %>% filter(!is.na(Vacuna), ANT_COVID2 == "SI") %>% 
  group_by(Vacuna) %>% 
  pairwise_wilcox_test(titulo ~ momento, paired = T)  %>% 
  mutate(id = row_number()) %>% 
  filter(id %in% c(2,5,8,10,12,15,18,20,22,25,28,30)) %>% 
  mutate(id = as.double(id),
         id = if_else(id == 2, 11, id),
         id = if_else(id == 12, 21, id),
         id = if_else(id == 22, 31, id)) %>% 
  arrange(id)

g2_panelB <- datos_largo1 %>% filter(!is.na(Vacuna), ANT_COVID2 == "SI") %>%  
  ggplot(aes(x = momento, y = titulo)) +
  geom_violin(scale = "width") +
  geom_hline(yintercept=50, linetype="dashed", color = "gray60") +
  geom_jitter(aes(color = Vacuna), size = 2, width=0.1, height=0.8, alpha=0.3) +
  stat_summary(fun=median, 
               fun.min = function(z) {quantile(z,0.25)},
               fun.max = function(z) {quantile(z,0.75)},
               geom="pointrange", color="darkred") +
  #geom_label(data = med, aes(x = momento, y = titulo, label = titulo), nudge_x = 0.05, hjust = 0, size = 3) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 2, sigma = 10),
                     breaks = c(0,50,200,800,3200,12800, 51200, 204800, 819200), guide = "prism_offset_minor") +
  scale_color_manual(values = c("darkorchid4", "forestgreen", "dodgerblue4")) +
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
    labels = scales::wrap_format(7)
  ) +
  facet_grid(.~Vacuna, labeller = as_labeller(vacuna_etiq)) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", y.position = log2(102400),
                     tip.length = 0.01, step.increase = 0.05,  step.group.by = "Vacuna",
                     size = 4) +
  guides(color = "none")  +
  theme(plot.caption = element_text(hjust = 0))

## Grafico C ----

med <- datos_largo1 %>% filter(!is.na(Vacuna), ANT_COVID2 == "NO") %>%
  group_by(Vacuna, momento) %>% 
  summarise(titulo = median(titulo, na.rm = T))

stat.test <- datos_largo1 %>% filter(!is.na(Vacuna), ANT_COVID2 == "NO", momento != "Baseline") %>% 
  group_by(momento) %>% 
  pairwise_wilcox_test(titulo ~ Vacuna, paired = F) %>% 
  add_x_position(x = "momento", dodge = 0.9) %>% 
  add_significance("p.adj")

stat.test <-  bind_rows(data.frame(momento = c("Baseline", "Baseline", "Baseline"),
                                   .y. = c("titulo", "titulo", "titulo"),
                                   x = c(1,1,1), xmin = c(0.7, 0.7, 1),
                                   xmax = c(1, 1.3, 1.3), p.adj.signif = c("ns", "ns", "ns")), stat.test)

conteo <- datos1 %>% filter(!is.na(Vacuna), ANT_COVID2 == "NO")  %>% 
  count(Vacuna)


g3_panelC <- datos_largo1 %>% filter(!is.na(Vacuna), ANT_COVID2 == "NO") %>%    
  ggplot(aes(x = momento, y = titulo)) +
  geom_violin(aes(color = Vacuna), scale = "width") +
  geom_hline(yintercept=50, linetype="dashed", color = "gray60") +
  geom_jitter(aes(color = Vacuna), position = position_jitterdodge(jitter.width = 0.3, 
                                                                   jitter.height = 0.6,
                                                                   dodge.width = 0.9), size = 2, alpha=0.3) +
  stat_summary(aes(group = Vacuna), fun=median, 
               fun.min = function(z) {quantile(z,0.25)},
               fun.max = function(z) {quantile(z,0.75)},
               geom="pointrange", color="darkred", position = position_dodge(width = 0.9)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 2, sigma = 10),
                     breaks = c(0,50,200,800,3200,12800, 51200, 204800, 819200), guide = "prism_offset_minor") +
  scale_color_manual(values = c("darkorchid4", "forestgreen", "dodgerblue4"), name = "Vacuna",
                     labels=c(paste0("Sputnik V\n(n = ", conteo[1,2],")"),
                              paste0("COVID-19 BIBP\n(n = ", conteo[2,2],")"),
                              paste0("AZD1222\n(n=", conteo[3,2],")"))) +
  #geom_label(data = med, aes(x = momento, y = titulo, label = titulo, group = Vacuna), position = position_dodge(width = 0.9),
  #           hjust = 0, size = 3)  +
  xlab("") + 
  ylab("Titer IgG anti-Spike") +
  theme_prism(
    base_fontface = "plain", 
    base_line_size = 0.7, 
    base_family = "Arial", 
    base_size = 12
  ) +
  theme(legend.title = element_text()) +
  scale_x_discrete(
    guide = guide_prism_bracket(width = 0.20), 
    labels = scales::wrap_format(12)
  ) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", xmin = "xmin", xmax = "xmax", y =log2(102400), tip.length = 0.01, step.increase = 0.05,
                     size = 4, step.group.by = "momento") +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") + 
  guides(color = guide_legend(title="Vaccine")) 


## Grafico D ----


med <- datos_largo1 %>% filter(!is.na(Vacuna), ANT_COVID2 == "SI") %>%
  group_by(Vacuna, momento) %>% 
  summarise(titulo = median(titulo, na.rm = T))

stat.test <- datos_largo1 %>% filter(!is.na(Vacuna), ANT_COVID2 == "SI") %>% 
  group_by(momento) %>% 
  pairwise_wilcox_test(titulo ~ Vacuna, paired = F) %>% 
  add_x_position(x = "momento", dodge = 0.9) %>% 
  add_significance("p")

# stat.test <-  bind_rows(data.frame(momento = c("Basal", "Basal", "Basal"), 
#                                    .y. = c("titulo", "titulo", "titulo"), 
#                                    x = c(1,1,1), xmin = c(0.7, 0.7, 1), 
#                                    xmax = c(1, 1.3, 1.3), p.signif = c("ns", "ns", "ns")), stat.test)

conteo <- datos1 %>% filter(!is.na(Vacuna), ANT_COVID2 == "SI")  %>%  
  mutate( Vacuna = factor(Vacuna, levels = c("Sputnik-V", "Sinopharm", "Oxford-Astra Zeneca"))) %>% 
  count(Vacuna)


g4_panelD <- datos_largo1 %>% filter(!is.na(Vacuna), ANT_COVID2 == "SI") %>%
  ggplot(aes(x = momento, y = titulo)) +
  geom_violin(aes(color = Vacuna), scale = "width") +
  geom_hline(yintercept=50, linetype="dashed", color = "gray60") +
  geom_jitter(aes(color = Vacuna), position = position_jitterdodge(jitter.width = 0.3, 
                                                                   jitter.height = 0.6,
                                                                   dodge.width = 0.9), size = 2, alpha=0.3) +
  stat_summary(aes(group = Vacuna), fun=median, 
               fun.min = function(z) {quantile(z,0.25)},
               fun.max = function(z) {quantile(z,0.75)},
               geom="pointrange", color="darkred", position = position_dodge(width = 0.9)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 2, sigma = 10),
                     breaks = c(0,50,200,800,3200,12800, 51200, 204800, 819200), guide = "prism_offset_minor") +
  scale_color_manual(values = c("darkorchid4", "forestgreen", "dodgerblue4"), name = "Vacuna",
                     labels=c(paste0("Sputnik V\n(n = ", conteo[1,2],")"),
                              paste0("COVID-19 BIBP\n(n = ", conteo[2,2],")"),
                              paste0("AZD1222\n(n=", conteo[3,2],")"))) +
  #geom_label(data = med, aes(x = momento, y = titulo, label = titulo, group = Vacuna), position = position_dodge(width = 0.9),
  #           hjust = 0, size = 3)  +
  xlab("") + 
  ylab("Titer IgG anti-Spike") +
  theme_prism(
    base_fontface = "plain", 
    base_line_size = 0.7, 
    base_family = "Arial", 
    base_size = 12
  ) +
  theme(legend.title = element_text()) +
  scale_x_discrete(
    guide = guide_prism_bracket(width = 0.20), 
    labels = scales::wrap_format(12)
  ) +
  stat_pvalue_manual(stat.test, label = "p.signif", 
                     xmin = "xmin", xmax = "xmax", 
                     y =log2(102400), tip.length = 0.01, 
                     step.increase = 0.05, step.group.by = "momento",
                     size = 4) +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") + 
  guides(color = guide_legend(title="Vaccine"))

## Panel ----

panel <- cowplot::plot_grid(g1_panelA, g2_panelB, g3_panelC, g4_panelD, ncol = 2,labels = c("A", "B", "C", "D"))

cowplot::ggsave2(plot = panel, filename = "panel_figura1_sin_neutralizantes.svg", device = "svg", units = "cm", width = 40, height = 30)

cowplot::ggsave2(plot = panel, filename = "panel_figura1_sin_neutralizantes.jpg", device = "jpeg", units = "cm", width = 50, height = 30, dpi = 600)

#############

### Figura 2 (grupo etario y sexo)

## Grafico 1 ----

med <- datos_largo1 %>% filter(!is.na(GrupoEdad), ANT_COVID2 == "NO") %>%
  group_by(GrupoEdad, momento) %>% 
  summarise(titulo = median(titulo, na.rm = T))


stat.test <- datos_largo1 %>% filter(!is.na(GrupoEdad), ANT_COVID2 == "NO", 
                                     momento != "Baseline") %>% 
  group_by(momento) %>% 
  wilcox_test(titulo ~ GrupoEdad, paired = F) %>% 
  add_x_position(x = "momento", dodge = 0.9) %>% 
  add_significance("p")

stat.test <-  bind_rows(data.frame(momento = "Baseline", 
                                   .y. = "titulo", 
                                   x = 1, 
                                   xmin = 0.775, 
                                   xmax = 1.23, p.signif = "ns"), stat.test)



conteo <- datos1 %>% filter(!is.na(GrupoEdad), ANT_COVID2 == "NO") %>%  
  count(GrupoEdad)


g1 <- datos_largo1 %>% filter(!is.na(GrupoEdad), ANT_COVID2 == "NO") %>% 
  ggplot(aes(x = momento, y = titulo)) +
  geom_violin(aes(fill = GrupoEdad),  scale = "width", alpha = 0, show.legend = F) +
  geom_jitter(aes(color = GrupoEdad), position = position_jitterdodge(jitter.width = 0.3, 
                                                                      jitter.height = 0.6,
                                                                      dodge.width = 0.9), size = 2, alpha=0.3) +
  stat_summary(aes(group = GrupoEdad), fun=median, 
               fun.min = function(z) {quantile(z,0.25)},
               fun.max = function(z) {quantile(z,0.75)},
               geom="pointrange", color="darkred", position = position_dodge2(width = 0.9)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 2, sigma = 10),
                     breaks = c(0,50,200,800,3200,12800, 51200, 204800, 819200), guide = "prism_offset_minor") +
  scale_color_manual(values = c("brown1", "dodgerblue"), name = "Grupo etario",
                     labels=c(paste0("< 80 years\n(n=", conteo[1,2],")"),
                              paste0(">= 80 years\n(n=", conteo[2,2],")"))) +
  #geom_label(data = med, aes(x = momento, y = titulo, label = titulo, group = GrupoEdad), position = position_dodge(width = 0.9),
  #           size = 3, hjust = 0)  +
  xlab("") + 
  ylab("Titer IgG anti-Spike") +
  theme_prism(
    base_fontface = "plain", 
    base_line_size = 0.7, 
    base_family = "Arial", 
    base_size = 12
  ) +
  theme(legend.title = element_text()) +
  scale_x_discrete(
    guide = guide_prism_bracket(width = 0.20), 
    labels = scales::wrap_format(14)
  ) +
  stat_pvalue_manual(stat.test, label = "p.signif", 
                     xmin = "xmin", xmax = "xmax", 
                     y =log2(102400), tip.length = 0.01, 
                     step.increase = 0.05, step.group.by = "momento",
                     size = 4) +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") + 
  guides(color = guide_legend(title="Age group"))

## Grafico 2 ----

med <- datos_largo1 %>% filter(!is.na(GrupoEdad), ANT_COVID2 == "SI") %>%
  group_by(GrupoEdad, momento) %>% 
  summarise(titulo = median(titulo, na.rm = T))


stat.test <- datos_largo1 %>% filter(!is.na(GrupoEdad), ANT_COVID2 == "SI") %>% 
  group_by(momento) %>% 
  wilcox_test(titulo ~ GrupoEdad, paired = F) %>% 
  add_x_position(x = "momento", dodge = 0.9) %>% 
  add_significance("p")




conteo <- datos1 %>% filter(!is.na(GrupoEdad), ANT_COVID2 == "SI") %>%  
  count(GrupoEdad)


g2 <- datos_largo1 %>% filter(!is.na(GrupoEdad), ANT_COVID2 == "SI") %>% 
  ggplot(aes(x = momento, y = titulo)) +
  geom_violin(aes(fill = GrupoEdad),  scale = "width", alpha = 0, show.legend = F) +
  geom_jitter(aes(color = GrupoEdad), position = position_jitterdodge(jitter.width = 0.3, 
                                                                      jitter.height = 0.6,
                                                                      dodge.width = 0.9), size = 2, alpha=0.3) +
  stat_summary(aes(group = GrupoEdad), fun=median, 
               fun.min = function(z) {quantile(z,0.25)},
               fun.max = function(z) {quantile(z,0.75)},
               geom="pointrange", color="darkred", position = position_dodge2(width = 0.9)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 2, sigma = 10),
                     breaks = c(0,50,200,800,3200,12800, 51200, 204800, 819200), guide = "prism_offset_minor") +
  scale_color_manual(values = c("brown1", "dodgerblue"), name = "Grupo etario",
                     labels=c(paste0("< 80 years\n(n=", conteo[1,2],")"),
                              paste0(">= 80 years\n(n=", conteo[2,2],")"))) +
  #geom_label(data = med, aes(x = momento, y = titulo, label = titulo, group = GrupoEdad), position = position_dodge(width = 0.9),
  #                 size = 3, hjust = 0)  +
  xlab("") + 
  ylab("Titer IgG anti-Spike") +
  theme_prism(
    base_fontface = "plain", 
    base_line_size = 0.7, 
    base_family = "Arial", 
    base_size = 12
  ) +
  theme(legend.title = element_text()) +
  scale_x_discrete(
    guide = guide_prism_bracket(width = 0.20), 
    labels = scales::wrap_format(14)
  ) +
  stat_pvalue_manual(stat.test, label = "p.signif", 
                     xmin = "xmin", xmax = "xmax", 
                     y =log2(102400), tip.length = 0.01, 
                     step.increase = 0.05, step.group.by = "momento",
                     size = 4) +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") + 
  guides(color = guide_legend(title="Age group"))



## Grafico 3 ----

med <- datos_largo1 %>% filter(!is.na(SEXO), ANT_COVID2 == "NO") %>%
  group_by(SEXO, momento) %>% 
  summarise(titulo = median(titulo, na.rm = T))


stat.test <- datos_largo1 %>% filter(!is.na(SEXO), ANT_COVID2 == "NO", momento != "Baseline") %>% 
  group_by(momento) %>% 
  wilcox_test(titulo ~ SEXO, paired = F) %>% 
  add_x_position(x = "momento", dodge = 0.9) %>% 
  add_significance("p")

stat.test <-  bind_rows(data.frame(momento = "Baseline", .y. = "titulo", x = 1, xmin = 0.775, xmax = 1.23, p.signif = "ns"), stat.test)



conteo <- datos1 %>% filter(!is.na(SEXO), ANT_COVID2 == "NO") %>%  
  count(SEXO)


g3 <- datos_largo1 %>% filter(!is.na(SEXO), ANT_COVID2 == "NO") %>% 
  mutate(SEXO = if_else(SEXO == "Femenino", "Female", "Male")) %>% 
  ggplot(aes(x = momento, y = titulo)) +
  geom_violin(aes(fill = SEXO),  scale = "width", alpha = 0, show.legend = F) +
  geom_jitter(aes(color = SEXO), position = position_jitterdodge(jitter.width = 0.3, 
                                                                 jitter.height = 0.6,
                                                                 dodge.width = 0.9), size = 2, alpha=0.3) +
  stat_summary(aes(group = SEXO), fun=median, 
               fun.min = function(z) {quantile(z,0.25)},
               fun.max = function(z) {quantile(z,0.75)},
               geom="pointrange", color="darkred", position = position_dodge2(width = 0.9)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 2, sigma = 10),
                     breaks = c(0,50,200,800,3200,12800, 51200, 204800, 819200), guide = "prism_offset_minor") +
  scale_color_manual(values = c("tan4", "darkorange"), name = "Gender",
                     labels=c(paste0("Female\n(n=", conteo[1,2],")"),
                              paste0("Male\n(n=", conteo[2,2],")"))) +
  #geom_label(data = med, aes(x = momento, y = titulo, label = titulo, group = SEXO), position = position_dodge(width = 0.9),
  #           size = 3, hjust = 0)  +
  xlab("") + 
  ylab("Titer IgG anti-Spike") +
  theme_prism(
    base_fontface = "plain", 
    base_line_size = 0.7, 
    base_family = "Arial", 
    base_size = 12
  ) +
  theme(legend.title = element_text()) +
  scale_x_discrete(
    guide = guide_prism_bracket(width = 0.20), 
    labels = scales::wrap_format(14)
  ) +
  stat_pvalue_manual(stat.test, label = "p.signif", 
                     xmin = "xmin", xmax = "xmax", 
                     y =log2(102400), tip.length = 0.01, 
                     step.increase = 0.05, step.group.by = "momento",
                     size = 4) +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") 



## Grafico 4 ----

med <- datos_largo1 %>% filter(!is.na(SEXO), ANT_COVID2 == "SI") %>%
  group_by(SEXO, momento) %>% 
  summarise(titulo = median(titulo, na.rm = T))


stat.test <- datos_largo1 %>% filter(!is.na(SEXO), ANT_COVID2 == "SI", 
                                     momento != "Baseline") %>% 
  group_by(momento) %>% 
  wilcox_test(titulo ~ SEXO, paired = F) %>% 
  add_x_position(x = "momento", dodge = 0.9) %>% 
  add_significance("p")

stat.test <-  bind_rows(data.frame(momento = "Baseline", .y. = "titulo", x = 1, xmin = 0.775, xmax = 1.23, p.signif = "ns"), stat.test)



conteo <- datos1 %>% filter(!is.na(SEXO), ANT_COVID2 == "SI") %>%  
  count(SEXO)


g4 <- datos_largo1 %>% filter(!is.na(SEXO), ANT_COVID2 == "SI") %>% 
  mutate(SEXO = if_else(SEXO == "Femenino", "Female", "Male")) %>% 
  ggplot(aes(x = momento, y = titulo)) +
  geom_violin(aes(fill = SEXO),  scale = "width", alpha = 0, show.legend = F) +
  geom_jitter(aes(color = SEXO), position = position_jitterdodge(jitter.width = 0.3, 
                                                                 jitter.height = 0.6,
                                                                 dodge.width = 0.9), size = 2, alpha=0.3) +
  stat_summary(aes(group = SEXO), fun=median, 
               fun.min = function(z) {quantile(z,0.25)},
               fun.max = function(z) {quantile(z,0.75)},
               geom="pointrange", color="darkred", position = position_dodge2(width = 0.9)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 2, sigma = 10),
                     breaks = c(0,50,200,800,3200,12800, 51200, 204800, 819200), guide = "prism_offset_minor") +
  scale_color_manual(values = c("tan4", "darkorange"), name = "Gender",
                     labels=c(paste0("Female\n(n=", conteo[1,2],")"),
                              paste0("Male\n(n=", conteo[2,2],")"))) +
  #geom_label(data = med, aes(x = momento, y = titulo, label = titulo, group = SEXO), position = position_dodge(width = 0.9),
  #           size = 3, hjust = 0)  +
  xlab("") + 
  ylab("Titer IgG anti-Spike") +
  theme_prism(
    base_fontface = "plain", 
    base_line_size = 0.7, 
    base_family = "Arial", 
    base_size = 12
  ) +
  theme(legend.title = element_text()) +
  scale_x_discrete(
    guide = guide_prism_bracket(width = 0.20), 
    labels = scales::wrap_format(14)
  ) +
  stat_pvalue_manual(stat.test, label = "p.signif", 
                     xmin = "xmin", xmax = "xmax", 
                     y =log2(102400), tip.length = 0.01, 
                     step.increase = 0.05, step.group.by = "momento",
                     size = 4) +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") 

## Panel ----

panel2 <- cowplot::plot_grid(g1, g2, g3, g4, ncol = 2,labels = c("A", "B", "C", "D"))

cowplot::ggsave2(plot = panel2, filename = "panel_figura2.svg", 
                 device = "svg", units = "cm", width = 35, height = 30)

cowplot::ggsave2(plot = panel2, filename = "panel_figura2.jpg", 
                 device = "jpeg", units = "cm", width = 35, height = 30, dpi = 600)

#############

#### Neutralizantes

neu <- read_excel("Neutralizantes.xlsx")

neu <- neu %>% left_join(datos1, by = c("ID" = "Id"))

neu <- neu %>%  pivot_longer(cols = 2:3, names_to = "dosis", values_to = "titulo") %>% 
  select(ID, HOGAR, Lugar, SEXO, Edad_calculada, GrupoEdad, Vacuna, ANT_COVID, ANT_COVID2, dosis, titulo, COVID_durante_estudi)

## Vacuna Sputnik

stat.test <- neu %>% filter(Vacuna == "Sputnik-V") %>%
  wilcox_test(titulo ~ dosis, paired = T) %>% 
  add_x_position(x = "dosis", dodge = 0.9) %>% 
  add_significance("p")

conteo <- neu %>% filter(Vacuna == "Sputnik-V", dosis == "Neut.1 dosis") %>%  
  count(Vacuna)

g5_panelE1 <- neu %>% filter(Vacuna == "Sputnik-V") %>%  
  mutate(dosis = case_when(
    dosis == "Neut.1 dosis" ~ "21 days \n post dose 1",
    dosis == "Neut.2 dosis" ~ "21 days \n post dose 2",
  ),
  dosis = factor(dosis, levels = c("21 days \n post dose 1", 
                                   "21 days \n post dose 2"))) %>% 
  ggplot(aes(x = dosis, y = titulo)) +
  geom_point(aes(color = Vacuna), size = 2) +
  geom_line(aes(group = ID), color = "dimgrey", size = 0.3) +
  scale_y_log10(guide = "prism_offset_minor", labels = scales::label_comma(accuracy = 1)) +
  scale_color_manual(values = "darkorchid4",
                     labels=c(paste0("Sputnik V\n(n = ", conteo[1,2],")"))) +
  xlab("") + 
  ylab("Neutralizing titer CoV2pp-GFP (IC50)") +
  theme_prism(
    base_fontface = "plain", 
    base_line_size = 0.7, 
    base_family = "Arial", 
    base_size = 12
  ) +
  stat_pvalue_manual(stat.test, label = "p.signif", xmin = "xmin", xmax = "xmax", y =log10(25000), tip.length = 0.01, step.increase = 0.05,
                     size = 4) +
  theme(plot.caption = element_text(hjust = 0)) +
  scale_x_discrete(
    guide = guide_prism_bracket(width = 0.20), 
    labels = scales::wrap_format(14)
  ) 

## Vacuna Sinopharm

stat.test <- neu %>% filter(Vacuna == "Sinopharm") %>%
  wilcox_test(titulo ~ dosis, paired = T) %>% 
  add_x_position(x = "dosis", dodge = 0.9) %>% 
  add_significance("p")

conteo <- neu %>% filter(Vacuna == "Sinopharm", dosis == "Neut.1 dosis") %>%  
  count(Vacuna)

g6_panelE2 <- neu %>% filter(Vacuna == "Sinopharm") %>%  
  mutate(dosis = case_when(
    dosis == "Neut.1 dosis" ~ "21 days \n post dose 1",
    dosis == "Neut.2 dosis" ~ "21 days \n post dose 2",
  ),
  dosis = factor(dosis, levels = c("21 days \n post dose 1", 
                                   "21 days \n post dose 2"))) %>% 
  ggplot(aes(x = dosis, y = titulo)) +
  geom_point(aes(color = Vacuna), size = 2) +
  geom_line(aes(group = ID), color = "dimgrey", size = 0.3) +
  scale_y_log10(guide = "prism_offset_minor", labels = scales::label_comma(accuracy = 1)) +
  scale_color_manual(values = "forestgreen",
                     labels=c(paste0("COVID-19 BIBP\n(n = ", conteo[1,2],")"))) +
  xlab("") + 
  ylab("Neutralizing titer CoV2pp-GFP (IC50)") +
  theme_prism(
    base_fontface = "plain", 
    base_line_size = 0.7, 
    base_family = "Arial", 
    base_size = 12
  ) +
  stat_pvalue_manual(stat.test, label = "p.signif", xmin = "xmin", xmax = "xmax", y =log10(25000), tip.length = 0.01, step.increase = 0.05,
                     size = 4) +
  theme(plot.caption = element_text(hjust = 0)) +
  scale_x_discrete(
    guide = guide_prism_bracket(width = 0.20), 
    labels = scales::wrap_format(14)
  ) 


## Vacuna AstraZeneca

stat.test <- neu %>% filter(Vacuna == "Oxford-Astra Zeneca") %>%
  wilcox_test(titulo ~ dosis, paired = T) %>% 
  add_x_position(x = "dosis", dodge = 0.9) %>% 
  add_significance("p")

conteo <- neu %>% filter(Vacuna == "Oxford-Astra Zeneca", dosis == "Neut.1 dosis") %>%  
  count(Vacuna)

g7_panelE3 <- neu %>% filter(Vacuna == "Oxford-Astra Zeneca") %>%  
  mutate(dosis = case_when(
    dosis == "Neut.1 dosis" ~ "21 days \n post dose 1",
    dosis == "Neut.2 dosis" ~ "21 days \n post dose 2",
  ),
  dosis = factor(dosis, levels = c("21 days \n post dose 1", 
                                   "21 days \n post dose 2"))) %>% 
  ggplot(aes(x = dosis, y = titulo)) +
  geom_point(aes(color = Vacuna), size = 2) +
  geom_line(aes(group = ID), color = "dimgrey", size = 0.3) +
  scale_y_log10(guide = "prism_offset_minor", labels = scales::label_comma(accuracy = 1)) +
  scale_color_manual(values = "dodgerblue4",
                     labels=c(paste0("AZD1222\n(n = ", conteo[1,2],")"))) +
  xlab("") + 
  ylab("Neutralizing titer CoV2pp-GFP (IC50)") +
  theme_prism(
    base_fontface = "plain", 
    base_line_size = 0.7, 
    base_family = "Arial", 
    base_size = 12
  ) +
  stat_pvalue_manual(stat.test, label = "p.signif", xmin = "xmin", xmax = "xmax", y =log10(25000), tip.length = 0.01, step.increase = 0.05,
                     size = 4) +
  theme(plot.caption = element_text(hjust = 0)) +
  scale_x_discrete(
    guide = guide_prism_bracket(width = 0.20), 
    labels = scales::wrap_format(14)
  ) 


####

library(ggbeeswarm)

## control de distribución (mediana y cuartiles 1 y 3)

neu %>% group_by(Vacuna, dosis) %>% 
  summarise(mediana = median(titulo),
            q1 = quantile(titulo, probs = 0.25),
            q3 = quantile(titulo, probs = 0.75))



stat.test <- neu %>% 
  mutate(Vacuna = factor(Vacuna, levels = c("Sputnik-V", "Sinopharm", "Oxford-Astra Zeneca"))) %>% 
  group_by(dosis) %>% 
  wilcox_test(titulo ~ Vacuna, paired = F) %>% 
  add_x_position(x = "dosis", dodge = 0.9) %>% 
  add_significance("p")

conteo <- neu %>% 
  filter(dosis == "Neut.1 dosis") %>% 
  mutate( Vacuna = factor(Vacuna, levels = c("Sputnik-V", "Sinopharm", "Oxford-Astra Zeneca"))) %>% 
  count(Vacuna)


g8_panelF <-  neu %>%    
  mutate(Vacuna = factor(Vacuna, levels = c("Sputnik-V", "Sinopharm", "Oxford-Astra Zeneca")),
         dosis = case_when(
           dosis == "Neut.1 dosis" ~ "21 days \n post dose 1",
           dosis == "Neut.2 dosis" ~ "21 days \n post dose 2",
         ),
         dosis = factor(dosis, levels = c("21 days \n post dose 1", 
                                          "21 days \n post dose 2")))  %>% 
  ggplot(aes(x = dosis, y = titulo)) +
  #geom_jitter(aes(color = Vacuna), position = position_jitterdodge(jitter.width = 0.3, 
  #                                                                 jitter.height = 0.6,
  #                                                                 dodge.width = 0.9), size = 2, alpha=0.3) +
  geom_beeswarm(aes(color = Vacuna), dodge.width = 0.9, 
                cex=2.5,
                size = 2.5,
                alpha = 0.7,
                #priority = "density",
                corral = "random",
                corral.width = 0.2) +
  # stat_summary(aes(group = Vacuna), fun.data = "median_hilow",
  #              #fun.min = function(z) {quantile(z, 0.25)},
  #              #fun.max = function(z) {quantile(z, 0.75)},
  #              geom="errorbar", color="black", position = position_dodge(width = 0.9)) +
  # 
  stat_summary(aes(group = Vacuna), fun = median,
               fun.min = function(z) {quantile(z,0.25)},
               fun.max = function(z) {quantile(z,0.75)},
               geom="errorbar", 
               color="darkred", 
               width = 0.5,
               position = position_dodge(width = 0.9)) +
  stat_summary(aes(group = Vacuna), fun = median,
               size = 3,
               geom = "point",
               color = "darkred",
               shape = 15,
               position = position_dodge(width = 0.9)) +
  scale_y_log10(guide = "prism_offset_minor", labels = scales::label_comma(accuracy = 1), breaks = c(1,10,100,1000,10000)) +
  scale_color_manual(values = c("darkorchid4", "forestgreen", "dodgerblue4"), name = "Vaccine",
                     labels=c(paste0("Sputnik-V\n(n=", conteo[1,2],")"),
                              paste0("COVID-19 BIBP\n(n=", conteo[2,2],")"),
                              paste0("AZD1222\n(n=", conteo[3,2],")"))) +
  xlab("") + 
  ylab("Neutralizing titer CoV2pp-GFP (IC50)") +
  theme_prism(
    base_fontface = "plain", 
    base_line_size = 0.7, 
    base_family = "Arial", 
    base_size = 12
  ) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", xmin = "xmin", xmax = "xmax", y =log10(25000), tip.length = 0.01, step.increase = 0.05,
                     size = 4) +
  theme(legend.title = element_text()) +
  scale_x_discrete(
    guide = guide_prism_bracket(width = 0.40), 
    labels = scales::wrap_format(13)
  ) +
  theme(plot.caption = element_text(hjust = 0))

# breaks = c(1,10,100,1000,10000, 100000)


## Panel ----

panel <- cowplot::plot_grid(g1_panelA, g2_panelB, g3_panelC, g4_panelD, g5_panelE1, g6_panelE2, g7_panelE3, g8_panelF,
                            ncol = 2,labels = c("A", "B", "C", "D", "E1", "E2", "E3", "F"))

cowplot::ggsave2(plot = panel, 
                 filename = "panel_figura1.svg", 
                 device = "svg", units = "cm",
                 width = 30, height = 50)

cowplot::ggsave2(plot = panel, 
                 filename = "panel_figura1.jpg", 
                 device = "jpeg", units = "cm", 
                 width = 30, height = 50, 
                 dpi = 600)


