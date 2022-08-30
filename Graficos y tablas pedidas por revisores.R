
## Librerias 

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
library(rcartocolor) 

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

neu <- neu[-c(72,113,135,142,144),]

# datos_N <- datos %>% inner_join(neu, by = c("ID" = "ID"))

datos_N <- datos %>% left_join(neu, by = c("ID" = "ID"))

datos_N <- datos_N %>% dplyr::select(ID, Lugar, SEXO, Edad_calculada, GrupoEdad, Vacuna, ANT_COVID,
                                     FECHA_HISOPADO, Tipo_diagnostico, COVID_durante_estudi, OBITO, Fecha_obito,
                                     baseline, after_dosis1, after_dosis2, after_120, after_180,
                                     Motivo_fin_seguimiento,Neu_post_d1,Neu_post_d2,Neu_post_120,Neu_post_180,Chequeado)  %>% 
  filter(Edad_calculada>=60) %>% mutate(ANT_COVID2 = if_else(baseline == 0, "NO", "SI"),
                                        Vacuna = factor(Vacuna, levels = c("Sputnik-V", "Sinopharm", "Oxford-Astra Zeneca")))




datos_largo1 <- datos_N %>% dplyr::select(ID, Lugar, SEXO, Edad_calculada, Vacuna, ANT_COVID, GrupoEdad, baseline, after_dosis1,
                                          after_dosis2, after_120, after_180, ANT_COVID2) %>% 
  pivot_longer(cols = c("baseline", starts_with("after")), names_to = "momento", values_to = "titulo") %>%
  mutate(titulo_cuali = if_else(titulo == 0, "NR", "R"))

datos_largo2 <- datos_N %>% dplyr::select(ID, Neu_post_d1,Neu_post_d2,Neu_post_120,Neu_post_180) %>% 
  pivot_longer(cols = c(starts_with("Neu")), names_to = "momento", values_to = "titulo_neu") %>% 
  mutate(momento = case_when(
    momento == "Neu_post_d1" ~ "after_dosis1",
    momento == "Neu_post_d2" ~ "after_dosis2",
    momento == "Neu_post_120" ~ "after_120",
    momento == "Neu_post_180" ~ "after_180"))




datos_completos <- datos_largo1 |> left_join(datos_largo2, by = c("ID", "momento"))

## control
datos_completos |> select(ID, momento, titulo, titulo_neu) |> View()

sum(!is.na(datos_completos$titulo_neu))

sum(!is.na(datos_largo2$titulo_neu))    

####

rm(datos, datos_largo1, datos_largo2, datos_N, neu)

#####

mi_pal <- rcartocolor::carto_pal(n = 12, name = "Vivid")[c(1, 6, 8, 10)]

#### Deming regression



deming.fit <- function(x, y, noise_ratio = sd(y)/sd(x)) {
  if(missing(noise_ratio) || is.null(noise_ratio)) noise_ratio <- eval(formals(sys.function(0))$noise_ratio) # this is just a complicated way to write `sd(y)/sd(x)`
  delta <-  noise_ratio^2
  x_name <- deparse(substitute(x))
  
  s_yy <- var(y)
  s_xx <- var(x)
  s_xy <- cov(x, y)
  beta1 <- (s_yy - delta*s_xx + sqrt((s_yy - delta*s_xx)^2 + 4*delta*s_xy^2)) / (2*s_xy)
  beta0 <- mean(y) - beta1 * mean(x) 
  
  res <- c(beta0 = beta0, beta1 = beta1)
  names(res) <- c("(Intercept)", x_name)
  class(res) <- "Deming"
  res
}

deming <- function(formula, data, R = 100, noise_ratio = NULL, ...){
  ret <- boot::boot(
    data = model.frame(formula, data), 
    statistic = function(data, ind) {
      data <- data[ind, ]
      args <- rlang::parse_exprs(colnames(data))
      names(args) <- c("y", "x")
      rlang::eval_tidy(rlang::expr(deming.fit(!!!args, noise_ratio = noise_ratio)), data, env = rlang::current_env())
    },
    R=R
  )
  class(ret) <- c("Deming", class(ret))
  ret  
}

predictdf.Deming <- function(model, xseq, se, level) {
  pred <- as.vector(tcrossprod(model$t0, cbind(1, xseq)))
  if(se) {
    preds <- tcrossprod(model$t, cbind(1, xseq))
    data.frame(
      x = xseq,
      y = pred,
      ymin = apply(preds, 2, function(x) quantile(x, probs = (1-level)/2)),
      ymax = apply(preds, 2, function(x) quantile(x, probs = 1-((1-level)/2)))
    )
  } else {
    return(data.frame(x = xseq, y = pred))
  }
}

####

library(correlation)

correlaciones <- datos_completos |> 
  filter(!is.na(titulo_neu), ANT_COVID2 == "NO", ANT_COVID == "NO") |> 
  mutate(titulo = if_else(titulo == 0, 1, titulo),
         momento = case_when(
           momento == "after_dosis1" ~ "21 days post dose 1",
           momento == "after_dosis2" ~ "21 days post dose 2",
           momento == "after_120" ~ "120 days post dose 1",
           momento == "after_180" ~ "180 days post dose 1"),
         momento = factor(momento, levels = c("21 days post dose 1", 
                                              "21 days post dose 2",
                                              "120 days post dose 1",
                                              "180 days post dose 1"))) |> 
  group_by(momento) |> 
  correlation(select = c("titulo", "titulo_neu"), method = "spearman") |> 
  rename("momento" = Group) |> 
  mutate(rho = round(rho, digits = 2),
         CI_low = round(CI_low, 2),
         CI_high = round(CI_high,2),
         p = if_else(p < 0.05, "p < 0.05", paste0("p = ",as.character(p))),
         salida = paste0("rho = ",
                         rho,
                         ", CI[",
                         CI_low, 
                         ", ",
                         CI_high,
                         "], "
                         ,p)) |> select(momento, salida)



datos_completos |>
  filter(!is.na(titulo_neu), ANT_COVID2 == "NO", ANT_COVID == "NO") |>
  mutate(titulo = if_else(titulo == 0, 1, titulo),
         momento = case_when(
           momento == "after_dosis1" ~ "21 days post dose 1",
           momento == "after_dosis2" ~ "21 days post dose 2",
           momento == "after_120" ~ "120 days post dose 1",
           momento == "after_180" ~ "180 days post dose 1"),
         momento = factor(momento, levels = c("21 days post dose 1",
                                              "21 days post dose 2",
                                              "120 days post dose 1",
                                              "180 days post dose 1"))) |>
  ggplot(aes(x = titulo, y = titulo_neu)) +
  geom_point(aes(color = momento)) +
  geom_smooth(method = deming, color = "black") +
  scale_x_continuous(trans=scales::log10_trans(),
                     breaks = c(1,50,200,800,3200,12800, 51200, 204800),
                     minor_breaks = c(100,400,1600,6400,25600,102400,409600),
                     guide = "prism_offset_minor" ) +
  scale_y_continuous(trans=scales::log10_trans(),
                     breaks = c(1,10,100,1000,10000,100000),
                     minor_breaks = c(3,30,300,3000,30000),
                     guide = "prism_offset_minor" ) +
  theme_prism(
    base_fontface = "plain",
    base_line_size = 0.7,
    base_family = "Arial",
    base_size = 12
  ) +
  scale_color_manual(values = mi_pal, guide = "none") +
  ylab("Neutralizing titer CoV2pp-GFP (IC50)") +
  xlab("Titer IgG anti-Spike") +
  facet_wrap(vars(momento), nrow = 2, as.table = F) +
  geom_text(data = correlaciones, aes(x = 40,
                                      label = salida,
                                      group = momento,
                                      y = 10000),
            size = 3, color = "black") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave(filename = "correlacion_Deming.jpg", dpi = 600, units = "cm", width = 18, height = 15)

ggsave(filename = "Grafico_revisor1_3.tiff",
       units = "px", 
       width = 2126,
       height = 1417,
       dpi = 300)

datos_completos |> 
  filter(!is.na(titulo_neu)) |> 
  mutate(titulo = if_else(titulo == 0, 1, titulo)) |> 
  group_by(momento) |> 
  summarise(media_log = mean(log10(titulo)),
            media = mean(titulo))


datos_completos |> 
  filter(!is.na(titulo_neu), ANT_COVID2 == "NO", ANT_COVID == "NO") |> 
  mutate(titulo = if_else(titulo == 0, 1, titulo),
         momento = case_when(
           momento == "after_dosis1" ~ "21 days post dose 1",
           momento == "after_dosis2" ~ "21 days post dose 2",
           momento == "after_120" ~ "120 days post dose 1",
           momento == "after_180" ~ "180 days post dose 1"),
         momento = factor(momento, levels = c("21 days post dose 1", 
                                              "21 days post dose 2",
                                              "120 days post dose 1",
                                              "180 days post dose 1"))) |> 
  count(momento)


######### Grafico edad vs anticuerpos

datos_completos |> select(Edad_calculada, Vacuna, momento, titulo, titulo_neu, ANT_COVID, ANT_COVID2) |> 
  filter(momento == "after_dosis2") |> 
  mutate(Edad = trunc(Edad_calculada)) |> 
 #filter(ANT_COVID2 == "NO", ANT_COVID == "NO") |> 
  mutate(titulo = if_else(titulo == 0, 1, titulo),
         momento = case_when(
           momento == "after_dosis1" ~ "21 days post dose 1",
           momento == "after_dosis2" ~ "21 days post dose 2",
           momento == "after_120" ~ "120 days post dose 1",
           momento == "after_180" ~ "180 days post dose 1"),
         momento = factor(momento, levels = c("21 days post dose 1", 
                                              "21 days post dose 2",
                                              "120 days post dose 1",
                                              "180 days post dose 1")),
         Vacuna = case_when(
           Vacuna == "Sputnik-V" ~ "Sputnik V",
           Vacuna == "Sinopharm" ~ "COVID-19 BIBP",
           Vacuna == "Oxford-Astra Zeneca" ~ "AZD1222"),
         Vacuna = factor(Vacuna, levels = c("Sputnik V", "COVID-19 BIBP", "AZD1222"))) |> 
  ggplot(aes(x = Edad, y = titulo)) + 
  geom_point(aes(color = Vacuna)) +
 # geom_smooth(method = "lm", se = F, color = "black") +
  scale_x_continuous(guide = "prism_offset_minor") +
  scale_y_continuous(trans=scales::log10_trans(),
                     breaks = c(1,50,200,800,3200,12800, 51200, 204800, 819200),
                     minor_breaks = c(100,400,1600,6400,25600,102400, 409600),
                     guide = "prism_offset_minor" ) +
  theme_prism(
    base_fontface = "plain", 
    base_line_size = 0.7, 
    base_family = "Arial", 
    base_size = 11
  ) + 
  scale_color_manual(values = c("darkorchid4", "forestgreen", "dodgerblue4"), guide = "none") +
  ylab("Titer IgG anti-Spike") +
  xlab("Age") +
  facet_wrap(vars(Vacuna), ncol = 1, as.table = F) +
  geom_text(data = corre_edad_titulo, aes(x = 70, 
                                      label = salida, 
                                      group = Vacuna, 
                                      y = 409600),
            size = 3, color = "black")
  
ggsave(filename = "corre_IgG_edad_sin_anticuerpos_con recta.jpg", 
       dpi = 600, 
       units = "cm", 
       width = 18, 
       height = 25)

ggsave(filename = "Grafico_revisor2_results_3a.tiff",
       units = "px", 
       width = 2126,
       height = 1417,
       dpi = 300)


corre_edad_titulo <- datos_completos |> select(Edad_calculada, Vacuna, momento, titulo, titulo_neu, ANT_COVID, ANT_COVID2) |> 
  filter(momento == "after_dosis2") |> 
  mutate(Edad = trunc(Edad_calculada)) |> 
 # filter(ANT_COVID2 == "NO", ANT_COVID == "NO") |> 
  mutate(titulo = if_else(titulo == 0, 1, titulo),
         Vacuna = case_when(
           Vacuna == "Sputnik-V" ~ "Sputnik V",
           Vacuna == "Sinopharm" ~ "COVID-19 BIBP",
           Vacuna == "Oxford-Astra Zeneca" ~ "AZD1222"),
         Vacuna = factor(Vacuna, levels = c("Sputnik V", "COVID-19 BIBP", "AZD1222"))) |> 
  group_by(Vacuna) |> 
  correlation(select = c("Edad", "titulo"), method = "spearman") |> 
  rename("Vacuna" = Group) |> 
  mutate(rho = round(rho, digits = 2),
         CI_low = round(CI_low, 2),
         CI_high = round(CI_high,2),
         p = if_else(p < 0.05, "p < 0.05", paste0("p = ",as.character(round(p,4)))),
         salida = paste0("rho = ",
                         rho,
                         ", CI[",
                         CI_low, 
                         ", ",
                         CI_high,
                         "], "
                         ,p)) |> select(Vacuna, salida)




######

######### Grafico edad vs IC50

datos_completos |> select(Edad_calculada, Vacuna, momento, titulo, titulo_neu, ANT_COVID, ANT_COVID2) |> 
  filter(momento == "after_dosis2") |> 
  mutate(Edad = trunc(Edad_calculada)) |> 
  #filter(ANT_COVID2 == "NO", ANT_COVID == "NO") |> 
  mutate(  momento = case_when(
           momento == "after_dosis1" ~ "21 days post dose 1",
           momento == "after_dosis2" ~ "21 days post dose 2",
           momento == "after_120" ~ "120 days post dose 1",
           momento == "after_180" ~ "180 days post dose 1"),
         momento = factor(momento, levels = c("21 days post dose 1", 
                                              "21 days post dose 2",
                                              "120 days post dose 1",
                                              "180 days post dose 1")),
         Vacuna = case_when(
           Vacuna == "Sputnik-V" ~ "Sputnik V",
           Vacuna == "Sinopharm" ~ "COVID-19 BIBP",
           Vacuna == "Oxford-Astra Zeneca" ~ "AZD1222"),
         Vacuna = factor(Vacuna, levels = c("Sputnik V", "COVID-19 BIBP", "AZD1222"))) |> 
  ggplot(aes(x = Edad, y = titulo_neu )) + 
  geom_point(aes(color = Vacuna)) +
 # geom_smooth(method = "lm", se = F, color = "black") +
  scale_x_continuous(guide = "prism_offset_minor") +
  scale_y_continuous(trans=scales::log10_trans(),
                     breaks = c(1,10,100,1000,10000,100000),
                     minor_breaks = c(3,30,300,3000,30000),
                     guide = "prism_offset_minor" ) +
  theme_prism(
    base_fontface = "plain", 
    base_line_size = 0.7, 
    base_family = "Arial", 
    base_size = 12
  ) + 
  scale_color_manual(values = c("darkorchid4", "forestgreen", "dodgerblue4"), guide = "none") +
  ylab("Titer IgG anti-Spike") +
  xlab("Age") +
  facet_wrap(vars(Vacuna), ncol = 1, as.table = F) +
  geom_text(data = corre_edad_titulo, aes(x = 70, 
                                          label = salida, 
                                          group = Vacuna, 
                                          y = 100000),
            size = 3, color = "black")

ggsave(filename = "corre_IC50_edad_todos_sin recta.jpg", 
       dpi = 600, 
       units = "cm", 
       width = 18, 
       height = 25)

ggsave(filename = "Grafico_revisor2_results_3b.tiff",
       units = "px", 
       width = 2126,
       height = 1417,
       dpi = 300)

corre_edad_titulo <- datos_completos |> select(Edad_calculada, Vacuna, momento, titulo, titulo_neu, ANT_COVID, ANT_COVID2) |> 
  filter(momento == "after_dosis2") |> 
  mutate(Edad = trunc(Edad_calculada)) |> 
 # filter(ANT_COVID2 == "NO", ANT_COVID == "NO") |> 
  mutate(  Vacuna = case_when(
           Vacuna == "Sputnik-V" ~ "Sputnik V",
           Vacuna == "Sinopharm" ~ "COVID-19 BIBP",
           Vacuna == "Oxford-Astra Zeneca" ~ "AZD1222"),
         Vacuna = factor(Vacuna, levels = c("Sputnik V", "COVID-19 BIBP", "AZD1222"))) |> 
  group_by(Vacuna) |> 
  correlation(select = c("Edad", "titulo_neu"), method = "spearman") |> 
  rename("Vacuna" = Group) |> 
  mutate(rho = round(rho, digits = 2),
         CI_low = round(CI_low, 2),
         CI_high = round(CI_high,2),
         p = if_else(p < 0.05, "p < 0.05", paste0("p = ",as.character(round(p,4)))),
         salida = paste0("rho = ",
                         rho,
                         ", CI[",
                         CI_low, 
                         ", ",
                         CI_high,
                         "], "
                         ,p)) |> select(Vacuna, salida)



datos_lm <- datos_completos |> select(Edad_calculada, Vacuna, momento, titulo, titulo_neu, ANT_COVID, ANT_COVID2) |> 
  filter(momento == "after_dosis2") |> 
  mutate(Edad = trunc(Edad_calculada)) |> 
  filter(ANT_COVID2 == "NO", ANT_COVID == "NO") |> 
  mutate(titulo = if_else(titulo == 0, 1, titulo)) 

reg_titulo <- lm(formula = log(titulo) ~ Edad, data = datos_lm)

summary(reg_titulo)

plot(reg_titulo)

library(gvlma)

gvlma(reg_titulo)

#### tabla

datos_N <- datos %>% left_join(neu, by = c("ID" = "ID"))

datos_N <- datos_N %>%  
  filter(Edad_calculada>=60) %>% mutate(ANT_COVID2 = if_else(baseline == 0, "NO", "SI"),
                                        Vacuna = factor(Vacuna, levels = c("Sputnik-V", "Sinopharm", "Oxford-Astra Zeneca")))

library(gtsummary)
library(flextable)

tabla <- datos_N |> select(SEXO, GrupoEdad, ANT_COVID, ANT_COVID2, Vacuna,
                  Diabetes, EPOC, `Obesidad severa`, `Insuf. Cardiaca`, HTA, 
                  `Enf. Renal crónica`, Inmunodeficiencia) |> 
  mutate(Exposicion_previa = if_else(ANT_COVID == "SI" | ANT_COVID2 == "SI", "Yes", "No")) |> 
  rename("Gender" = SEXO,
         "Age" = GrupoEdad,
         "Previous exposure \n to SARS-CoV-2" = Exposicion_previa,
         "Mellitus diabetes" = Diabetes,
         "COPD" = EPOC,
         "Severe obesity" = `Obesidad severa`,
         "Heart failure" = `Insuf. Cardiaca`,
         "HTN" = HTA,
         "Chronic kidney disease" = `Enf. Renal crónica`,
         "Immunodeficiency" = Inmunodeficiencia) |> 
  mutate(across(where(is.character), str_to_sentence),
          Gender = if_else(Gender == "Femenino", "Female", "Male"),
         `Mellitus diabetes` = if_else(`Mellitus diabetes` == "Si", "Yes", `Mellitus diabetes`),
         `Mellitus diabetes` = replace_na(`Mellitus diabetes`, "No data"),
          COPD = if_else(COPD == "Si", "Yes", COPD),
         COPD = replace_na(COPD, "No data"),
         `Severe obesity` = if_else(`Severe obesity` == "Si", "Yes", `Severe obesity`),
         `Severe obesity` = replace_na(`Severe obesity`, "No data"),
         `Heart failure` = if_else(`Heart failure` == "Si", "Yes", `Heart failure`),
         `Heart failure` = replace_na(`Heart failure`, "No data"),
          HTN = if_else(HTN == "Si", "Yes", HTN),
         HTN = replace_na(HTN, "No data"),
         `Chronic kidney disease` = if_else(`Chronic kidney disease` == "Si", "Yes", `Chronic kidney disease`),
         `Chronic kidney disease` = replace_na(`Chronic kidney disease`, "No data"),
         Immunodeficiency = if_else(Immunodeficiency == "Si", "Yes", Immunodeficiency),
         Immunodeficiency = replace_na(Immunodeficiency, "No data"),
         Vacuna = case_when(
           Vacuna == "Sputnik-V" ~ "Sputnik V",
           Vacuna == "Sinopharm" ~ "COVID-19 BIBP",
           Vacuna == "Oxford-Astra Zeneca" ~ "AZD1222")) |> 
  select(-ANT_COVID2, -ANT_COVID) |> 
  tbl_summary(by = Vacuna) |> 
  add_n() |> 
  add_p() |>   
  modify_header(label = "**Characterics**")  |> 
  bold_labels() |> 
  as_flex_table()

### omitiendo no data

tabla <- datos_N |> select(SEXO, GrupoEdad, ANT_COVID, ANT_COVID2, Vacuna,
                           Diabetes, EPOC, `Obesidad severa`, `Insuf. Cardiaca`, HTA, 
                           `Enf. Renal crónica`, Inmunodeficiencia) |> 
  mutate(Exposicion_previa = if_else(ANT_COVID == "SI" | ANT_COVID2 == "SI", "Yes", "No")) |> 
  rename("Gender" = SEXO,
         "Age" = GrupoEdad,
         "Previous exposure \n to SARS-CoV-2" = Exposicion_previa,
         "Mellitus diabetes" = Diabetes,
         "COPD" = EPOC,
         "Severe obesity" = `Obesidad severa`,
         "Heart failure" = `Insuf. Cardiaca`,
         "HTN" = HTA,
         "Chronic kidney disease" = `Enf. Renal crónica`,
         "Immunodeficiency" = Inmunodeficiencia) |> 
  mutate(across(where(is.character), str_to_sentence),
         Gender = if_else(Gender == "Femenino", "Female", "Male"),
         `Mellitus diabetes` = if_else(`Mellitus diabetes` == "Si", "Yes", `Mellitus diabetes`),
         COPD = if_else(COPD == "Si", "Yes", COPD),
         `Severe obesity` = if_else(`Severe obesity` == "Si", "Yes", `Severe obesity`),
         `Heart failure` = if_else(`Heart failure` == "Si", "Yes", `Heart failure`),
         HTN = if_else(HTN == "Si", "Yes", HTN),
         `Chronic kidney disease` = if_else(`Chronic kidney disease` == "Si", "Yes", `Chronic kidney disease`),
         Immunodeficiency = if_else(Immunodeficiency == "Si", "Yes", Immunodeficiency),
          Vacuna = case_when(
           Vacuna == "Sputnik-V" ~ "Sputnik V",
           Vacuna == "Sinopharm" ~ "COVID-19 BIBP",
           Vacuna == "Oxford-Astra Zeneca" ~ "AZD1222")) |> 
  select(-ANT_COVID2, -ANT_COVID) |> 
  tbl_summary(by = Vacuna, missing = "no") |> 
  add_n() |> 
  add_p() |>   
  modify_header(label = "**Characterics**")  |> 
  bold_labels() |> 
  as_flex_table()
  
save_as_docx(tabla, path = "tabla.docx")

# control de p

x <- matrix(data = c(28, 136, 33, 132, 161, 361), ncol = 3)

x <- matrix(data = c(69, 57, 251, 95, 108, 271), ncol = 2)

x <- matrix(data = c(80, 20, 80, 20, 80, 20), ncol = 3)

x

chisq.test(x)


datos_N |> select(SEXO, GrupoEdad, ANT_COVID, ANT_COVID2, Vacuna,
                  Diabetes, EPOC, `Obesidad severa`, `Insuf. Cardiaca`, HTA, 
                  `Enf. Renal crónica`, Inmunodeficiencia) |> 
  mutate(Exposicion_previa = if_else(ANT_COVID == "SI" | ANT_COVID2 == "SI", "Yes", "No")) |> 
  rename("Gender" = SEXO,
         "Age" = GrupoEdad,
         "Mellitus diabetes" = Diabetes,
         "COPD" = EPOC,
         "Severe obesity" = `Obesidad severa`,
         "Heart failure" = `Insuf. Cardiaca`,
         "HTN" = HTA,
         "Chronic kidney disease" = `Enf. Renal crónica`,
         "Immunodeficiency" = Inmunodeficiencia) |> 
  mutate(across(where(is.character), str_to_sentence),
         Gender = if_else(Gender == "Femenino", "Female", "Male"),
         `Mellitus diabetes` = if_else(`Mellitus diabetes` == "Si", "Yes", `Mellitus diabetes`),
         COPD = if_else(COPD == "Si", "Yes", COPD),
         `Severe obesity` = if_else(`Severe obesity` == "Si", "Yes", `Severe obesity`),
         `Heart failure` = if_else(`Heart failure` == "Si", "Yes", `Heart failure`),
         HTN = if_else(HTN == "Si", "Yes", HTN),
         `Chronic kidney disease` = if_else(`Chronic kidney disease` == "Si", "Yes", `Chronic kidney disease`),
         Immunodeficiency = if_else(Immunodeficiency == "Si", "Yes", Immunodeficiency),
         Vacuna = case_when(
           Vacuna == "Sputnik-V" ~ "Sputnik V",
           Vacuna == "Sinopharm" ~ "COVID-19 BIBP",
           Vacuna == "Oxford-Astra Zeneca" ~ "AZD1222")) |> 
  select(Vacuna, Exposicion_previa) |> 
  tbl_summary(by = Vacuna, missing = "no") |> 
  add_n() |> 
  add_p() |>   
  modify_header(label = "**Characterics**")  |> 
  bold_labels()



### tiempoes entre dosis

library(lubridate)


datos |> mutate(tiempo_entre_dosis = interval(Fecha_1r_dosis, Fecha_2da_dosis)/ddays(),
                tiempo_3era_extraccion = interval(Fecha_2da_dosis, Fecha_muestra_42)/ddays()) |> 
  select(ID, Vacuna, Fecha_1r_dosis, Fecha_2da_dosis, Fecha_muestra_0, Fecha_muestra_42, tiempo_entre_dosis, tiempo_3era_extraccion) |> View()

datos |> mutate(tiempo_entre_dosis = interval(Fecha_1r_dosis, Fecha_2da_dosis)/ddays(),
                tiempo_3era_extraccion = interval(Fecha_2da_dosis, Fecha_muestra_42)/ddays()) |> 
  group_by(Vacuna) |> 
  summarise(min_tiempo_entre_dosis = min(tiempo_entre_dosis, na.rm = T),
            max_tiempo_entre_dosis = max(tiempo_entre_dosis, na.rm = T),
            media_tiempo_entre_dosis = mean(tiempo_entre_dosis, na.rm = T),
            min_tiempo_3era_extraccion = min(tiempo_3era_extraccion, na.rm = T),
            max_tiempo_3era_extraccion = max(tiempo_3era_extraccion, na.rm = T),
            media_tiempo_3era_extraccion = mean(tiempo_3era_extraccion, na.rm = T)) |> View()


datos |> mutate(tiempo_entre_dosis = interval(Fecha_1r_dosis, Fecha_2da_dosis)/ddays(),
                tiempo_3era_extraccion = interval(Fecha_2da_dosis, Fecha_muestra_42)/ddays()) |> 
  filter(!is.na(Vacuna)) |> 
  group_by(Vacuna) |>
  summarise(mediana_dias = median(tiempo_3era_extraccion, na.rm = T),
            cuartil1_dias = quantile(tiempo_3era_extraccion, probs = 0.25, na.rm = T),
            cuartil3_dias = quantile(tiempo_3era_extraccion, probs = 0.75, na.rm = T)) |> 
  flextable()


datos |> mutate(tiempo_entre_dosis = interval(Fecha_1r_dosis, Fecha_2da_dosis)/ddays(),
                tiempo_3era_extraccion = interval(Fecha_2da_dosis, Fecha_muestra_42)/ddays()) |> 
  filter(!is.na(Vacuna)) |> 
  group_by(Vacuna) |> 
  summarise(mediana_entre_dosis = median(tiempo_entre_dosis, na.rm = T),
            cuartil1_entre_dosis = quantile(tiempo_entre_dosis, probs = 0.25, na.rm = T),
            cuartil3_entre_dosis = quantile(tiempo_entre_dosis, probs = 0.75, na.rm = T),
            RIC_entre_dosis = IQR(tiempo_entre_dosis, na.rm = T)) |> 
  flextable()
