# ----------------------------------------------------------------- #
# Script de test du schéma pour les données données par Mme Chevret #
# Créé le 28/09/2021, modifié le 28/09/2021                         #
# ----------------------------------------------------------------- #

# Packages et helpers ----

library(tidyverse)
library(lubridate)
devtools::load_all("E:/multibrasBOP2")
library(multibrasBOP2)
library(ggtext)

theme_set(theme_light() +
            theme(strip.background.x = element_blank(),
                  strip.background.y = element_blank(),
                  strip.text.x = element_textbox(
                    size = 14, face = "bold", linewidth = .8,
                    color = "black", fill = "transparent", box.color = "black",
                    halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(.5, "npc"),
                    padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)),
                  strip.text.y.right = element_textbox(
                    size = 14, face = "bold", linewidth = .8, orientation = "right-rotated",
                    color = "black", fill = "transparent", box.color = "black",
                    halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(.5, "npc"),
                    padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3))))


# Import of bases ----

load("Data/ex_clinique/data_example.Rdata")
fabinou_repf$Rando[fabinou_repf$Rando == "AZA+DEP"] <- "AZA+VPA"


# Analysis ----

## Calculation of thresholds ----
# prior multinomial = (.15, .25, .15, .45)
# prior eff = .40 // prior_tox = .30

### 2 interim analyses
seuils <- list()
seuils[["2_ana"]] <- deter_cutoff(alpha = .15,
                                  n_bras = 2,
                                  nsim_oc = 10000,
                                  ana_inter = c(40, 40),
                                  p_n = c(.15, .25, .15, .45),
                                  p_a = c(.15, .4, .05, .4),
                                  delta = c(0, 0),
                                  affich_mat = "No")
### 4 interim analyses 
seuils[["4_ana"]] <- deter_cutoff(alpha = .15,
                                  n_bras = 2,
                                  nsim_oc = 10000,
                                  ana_inter = c(20, 20, 20, 20),
                                  p_n = c(.15, .25, .15, .45),
                                  p_a = c(.15, .4, .05, .4),
                                  delta = c(0, 0),
                                  affich_mat = "No")

# Holm with 4 analyses
seuils[["holm"]] <- deter_cutoff_holm2(alpha = .15,
                   ana_inter = rep(20, 4),
                   n_bras = 2, 
                   p_n = c(.15, .25, .15, .45), p_a = c(.15, .4, .05, .4),
                   nsim_oc = 10000,
                   delta = c(0, 0),
                   affich_mat = "No")

save(seuils, file = "Data/ex_clinique/seuils2.Rdata")

load("Data/ex_clinique/seuils2.Rdata")
seuils <- lapply(seuils, "[[", 1)
# 77% power for 2 analyses and 74% for 4


## Follow the trial ----

table_ana <- fabinou_repf %>% 
  arrange(DATRAND) %>% 
  group_by(Rando) %>% 
  mutate(nb_pat = row_number()) %>% 
  filter(nb_pat %in% c(20, 40, 60, max(nb_pat))) %>% 
  mutate(analyse = case_when(nb_pat == 20 ~ 1,
                             nb_pat == 40 ~ 2,
                             nb_pat == 60 ~ 3,
                             TRUE ~ 4)) %>% 
  group_by(analyse) %>% 
  summarise(dat_point = max(DATRAND))

# Inclusion du numéro de patient
fabinou_rep4 <- fabinou_repf %>% 
  arrange(DATRAND) %>% 
  group_by(Rando) %>% 
  mutate(nb_pat = row_number(),
         analyse = case_when(DATRAND <= table_ana$dat_point[table_ana$analyse == 1] ~ 1,
                             DATRAND <= table_ana$dat_point[table_ana$analyse == 2] ~ 2,
                             DATRAND <= table_ana$dat_point[table_ana$analyse == 3] ~ 3,
                             TRUE ~ 4),
         adv_event = as.numeric(aRRET %in% c("progression", "rechute")),
         adv_event = if_else(is.na(adv_event), 0, adv_event)) %>% 
  ungroup()

text_pat <- fabinou_rep4 %>% 
  count(Rando, analyse) %>% 
  group_by(Rando) %>% 
  mutate(n = cumsum(n)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Rando, values_from = n) %>% 
  janitor::clean_names() %>% 
  transmute(analyse = analyse,
            nb_patients = paste0("# of patients:\nAZA ", aza, "\nAZA+VPA ", aza_vpa, "\nAZA+LEN ", aza_len))

# Comptes des patients à chaque analyse intermédiaire
resultats_4ana4 <- fabinou_rep4 %>% 
  group_by(Rando, analyse) %>% 
  summarise(Reponse = sum(Reponse),
            adv_event = sum(adv_event),
            nb_patients = n(),
            duree = max(DATRAND)) %>% 
  mutate(Reponse = cumsum(Reponse),
         adv_event = cumsum(adv_event),
         nb_patients = cumsum(nb_patients),
         no_adv_event = nb_patients - adv_event)

# Time of each analysis
fabinou_rep4 %>% 
  group_by(analyse) %>% 
  mutate(debut_periode = min(DATRAND)) %>% 
  group_by(Rando, analyse) %>% 
  summarise(Reponse = sum(Reponse),
            adv_event = sum(adv_event),
            nb_patients = n(),
            duree = max(DATRAND) - mean(debut_periode)) %>% 
  mutate(Reponse = cumsum(Reponse),
         adv_event = cumsum(adv_event),
         nb_patients = cumsum(nb_patients),
         no_adv_event = nb_patients - adv_event)
# Number of days for arm AZA+LEN
378 + 267 + 195 + 195
379 + 267 + 195 + 195# Number of days for treatments in our trial

379 + 267 + 414 + 395 + 195 + 195 + 195
# Total days for the original trial
fabinou_repf %>% filter(Rando %in% c("AZA", "AZA+DEP")) %>% pull(DATRAND) %>% max()

1785 - 1036
# 749 days less

# Critères d'arrêt
resultats_4ana4 <- resultats_4ana4 %>% 
  select(-adv_event) %>% 
  pivot_wider(names_from = Rando, values_from = c(Reponse, no_adv_event, nb_patients, duree), names_sep = "£") %>% 
  pivot_longer(cols = c(-analyse, -'Reponse£AZA', -'no_adv_event£AZA', -'nb_patients£AZA', -'duree£AZA')) %>% 
  separate("name", into = c("variable", "ttt"), sep = "£") %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  rename(reponse_cont = 'Reponse£AZA', no_adv_event_cont = 'no_adv_event£AZA', nb_patients_cont = 'nb_patients£AZA') %>% 
  mutate(p_eff = pmap_dbl(
    .l = list(Reponse = Reponse, nb_patients = nb_patients, reponse_cont = reponse_cont, nb_patients_cont = nb_patients_cont),
    .f = function(Reponse, nb_patients, reponse_cont, nb_patients_cont) {
      integrate(
        f = function(x) {
          (1 - pbeta(x, .4 + Reponse, .6 + nb_patients - Reponse)) *
            dbeta(x, .4 + reponse_cont, .6 + nb_patients_cont - reponse_cont)
        },
        lower = 0, upper = 1)$value
    }
  ),
  # Actually it's p_notox computed but for the rule of toxicity
  p_tox = pmap_dbl(
    .l = list(no_adv_event = no_adv_event, nb_patients = nb_patients, no_adv_event_cont = no_adv_event_cont, nb_patients_cont = nb_patients_cont),
    .f = function(no_adv_event, nb_patients, no_adv_event_cont, nb_patients_cont) {
      integrate(
        f = function(x) {
          (1 - pbeta(x, .7 + nb_patients - no_adv_event, .3 + no_adv_event)) *
            dbeta(x, .7 + nb_patients_cont - no_adv_event_cont, .3 + no_adv_event_cont)
        },
        lower = 0, upper = 1)$value
    }
  ),
  lambda = seuils[["4_ana"]]["C_"],
  gamm = seuils[["4_ana"]]["gamma"],
  seuil = if_else(analyse < 4,
                  lambda * ((nb_patients_cont + nb_patients) / 160) ^ gamm,
                  lambda),
  arret_futilite = as.numeric(p_eff < seuil),
  arret_toxicite = as.numeric(p_tox < seuil))
# With active arm threshold
resultats_4ana4 <- fabinou_rep4 %>% 
  group_by(Rando, analyse) %>% 
  summarise(Reponse = sum(Reponse),
            adv_event = sum(adv_event),
            nb_patients = n(),
            duree = max(DATRAND)) %>% 
  mutate(Reponse = cumsum(Reponse),
         adv_event = cumsum(adv_event),
         nb_patients = cumsum(nb_patients),
         no_adv_event = nb_patients - adv_event)
resultats_4ana4bis <- resultats_4ana4 %>% 
  select(-adv_event) %>% 
  pivot_wider(names_from = Rando, values_from = c(Reponse, no_adv_event, nb_patients, duree), names_sep = "£") %>% 
  pivot_longer(cols = c(-analyse, -'Reponse£AZA', -'no_adv_event£AZA', -'nb_patients£AZA', -'duree£AZA')) %>% 
  separate("name", into = c("variable", "ttt"), sep = "£") %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  rename(reponse_cont = 'Reponse£AZA', no_adv_event_cont = 'no_adv_event£AZA', nb_patients_cont = 'nb_patients£AZA') %>% 
  mutate(p_eff = pmap_dbl(
    .l = list(Reponse = Reponse, nb_patients = nb_patients, reponse_cont = reponse_cont, nb_patients_cont = nb_patients_cont),
    .f = function(Reponse, nb_patients, reponse_cont, nb_patients_cont) {
      integrate(
        f = function(x) {
          (1 - pbeta(x, .4 + Reponse, .6 + nb_patients - Reponse)) *
            dbeta(x, .4 + reponse_cont, .6 + nb_patients_cont - reponse_cont)
        },
        lower = 0, upper = 1)$value
    }
  ),
  p_tox = pmap_dbl(
    .l = list(no_adv_event = no_adv_event, nb_patients = nb_patients, no_adv_event_cont = no_adv_event_cont, nb_patients_cont = nb_patients_cont),
    .f = function(no_adv_event, nb_patients, no_adv_event_cont, nb_patients_cont) {
      integrate(
        f = function(x) {
          (1 - pbeta(x, .7 + nb_patients - no_adv_event, .3 + no_adv_event)) *
            dbeta(x, .7 + nb_patients_cont - no_adv_event_cont, .3 + no_adv_event_cont)
        },
        lower = 0, upper = 1)$value
    }
  ),
  lambda = seuils[["holm"]]["C_holm"],
  gamm = seuils[["holm"]]["gamma_holm"],
  lambda_mono = seuils[["holm"]]["C_mono"],
  gamm_mono = seuils[["holm"]]["gamma_mono"],
  continuer = NA, decision_eff = NA, decision_tox = NA)
for (i in 1:4) {
  if (i == 1) {
    nb_act <- 2
  } else {
    nb_act <- sum(resultats_4ana4bis$continuer[resultats_4ana4bis$analyse == (i - 1)])
  }
  if (i == 4) {
    seuil_arret <- max(seuils[["holm"]][c("C_holm", "C_mono")])
  } else {
    eta <- 3 - nb_act
    seuil_arret <- ((eta - seuils[["holm"]][["C_holm"]]) / eta) * ((resultats_4ana4bis$nb_patients_cont[resultats_4ana4bis$analyse == i] + resultats_4ana4bis$nb_patients[resultats_4ana4bis$analyse == i]) / 160) ^ seuils[["holm"]][["gamma_holm"]]
  }
  if (nb_act != 0) {
    resultats_4ana4bis$decision_eff[resultats_4ana4bis$analyse == i] <- as.numeric(resultats_4ana4bis$p_eff[resultats_4ana4bis$analyse == i] < seuil_arret)
    resultats_4ana4bis$decision_tox[resultats_4ana4bis$analyse == i] <- as.numeric(resultats_4ana4bis$p_tox[resultats_4ana4bis$analyse == i] < seuil_arret)
    resultats_4ana4bis$continuer[resultats_4ana4bis$analyse == i] <- !(resultats_4ana4bis$decision_eff[resultats_4ana4bis$analyse == i] | resultats_4ana4bis$decision_tox[resultats_4ana4bis$analyse == i])
  } else {
    resultats_4ana4bis$decision_eff[resultats_4ana4bis$analyse == i] <- 2
    resultats_4ana4bis$decision_tox[resultats_4ana4bis$analyse == i] <- 2
    resultats_4ana4bis$continuer[resultats_4ana4bis$analyse == i] <- FALSE
  }
}
resultats_4ana4bis
# Same result as the C_n^m threshold

fabinou_repf %>% 
  arrange(DATRAND) %>% 
  group_by(Rando) %>% 
  mutate(nb_recrut = 1,
         nb_recrut = cumsum(nb_recrut)) %>% 
  ungroup() %>% 
  ggplot(aes(x = DATRAND, y = nb_recrut, color = Rando)) +
  geom_line() +
  geom_point()

ggplot() +
  xlim(c(0, 1)) +
  geom_function(data = data.frame(mesure = "Efficacy", analyse = "1<sup>st</sup> interim analysis"), fun = ~ dbeta(.x, .4 + 8, .6 + 13), aes(color = "AZA"), size = 1) +
  geom_function(data = data.frame(mesure = "Efficacy", analyse = "1<sup>st</sup> interim analysis"), fun = ~ dbeta(.x, .4 + 12, .6 + 8), aes(color = "AZA+VPA"), size = .6) +
  geom_function(data = data.frame(mesure = "Efficacy", analyse = "1<sup>st</sup> interim analysis"), fun = ~ dbeta(.x, .4 + 6, .6 + 14), aes(color = "AZA+LEN"), size = .6) +
  geom_function(data = data.frame(mesure = "Toxicity", analyse = "1<sup>st</sup> interim analysis"), fun = ~ dbeta(.x, .4 + 11, .6 + 10), aes(color = "AZA"), size = 1) +
  geom_function(data = data.frame(mesure = "Toxicity", analyse = "1<sup>st</sup> interim analysis"), fun = ~ dbeta(.x, .4 + 10, .6 + 10), aes(color = "AZA+VPA"), size = .6) +
  geom_function(data = data.frame(mesure = "Toxicity", analyse = "1<sup>st</sup> interim analysis"), fun = ~ dbeta(.x, .4 + 13, .6 + 7), aes(color = "AZA+LEN"), size = .6) +
  geom_function(data = data.frame(mesure = "Efficacy", analyse = "2<sup>nd</sup> interim analysis"), fun = ~ dbeta(.x, .4 + 14, .6 + 26), aes(color = "AZA"), size = 1) +
  geom_function(data = data.frame(mesure = "Efficacy", analyse = "2<sup>nd</sup> interim analysis"), fun = ~ dbeta(.x, .4 + 18, .6 + 22), aes(color = "AZA+VPA"), size = .6) +
  geom_function(data = data.frame(mesure = "Efficacy", analyse = "2<sup>nd</sup> interim analysis"), fun = ~ dbeta(.x, .4 + 15, .6 + 25), aes(color = "AZA+LEN"), size = .6) +
  geom_function(data = data.frame(mesure = "Toxicity", analyse = "2<sup>nd</sup> interim analysis"), fun = ~ dbeta(.x, .4 + 23, .6 + 17), aes(color = "AZA"), size = 1) +
  geom_function(data = data.frame(mesure = "Toxicity", analyse = "2<sup>nd</sup> interim analysis"), fun = ~ dbeta(.x, .4 + 9, .6 + 31), aes(color = "AZA+VPA"), size = .6) +
  geom_function(data = data.frame(mesure = "Toxicity", analyse = "2<sup>nd</sup> interim analysis"), fun = ~ dbeta(.x, .4 + 11, .6 + 29), aes(color = "AZA+LEN"), size = .6) +
  geom_function(data = data.frame(mesure = "Efficacy", analyse = "3<sup>rd</sup> interim analysis"), fun = ~ dbeta(.x, .4 + 24, .6 + 38), aes(color = "AZA"), size = 1) +
  geom_function(data = data.frame(mesure = "Efficacy", analyse = "3<sup>rd</sup> interim analysis"), fun = ~ dbeta(.x, .4 + 26, .6 + 36), aes(color = "AZA+VPA"), size = .6) +
  geom_function(data = data.frame(mesure = "Efficacy", analyse = "3<sup>rd</sup> interim analysis"), fun = ~ dbeta(.x, .4 + 22, .6 + 38), aes(color = "AZA+LEN"), size = .6) +
  geom_function(data = data.frame(mesure = "Toxicity", analyse = "3<sup>rd</sup> interim analysis"), fun = ~ dbeta(.x, .4 + 20, .6 + 42), aes(color = "AZA"), size = 1) +
  geom_function(data = data.frame(mesure = "Toxicity", analyse = "3<sup>rd</sup> interim analysis"), fun = ~ dbeta(.x, .4 + 18, .6 + 44), aes(color = "AZA+VPA"), size = .6) +
  geom_function(data = data.frame(mesure = "Toxicity", analyse = "3<sup>rd</sup> interim analysis"), fun = ~ dbeta(.x, .4 + 18, .6 + 42), aes(color = "AZA+LEN"), size = .6) +
  geom_function(data = data.frame(mesure = "Efficacy", analyse = "Final analysis"), fun = ~ dbeta(.x, .3 + 34, .7 + 47), aes(color = "AZA"), size = 1) +
  geom_function(data = data.frame(mesure = "Efficacy", analyse = "Final analysis"), fun = ~ dbeta(.x, .3 + 33, .7 + 47), aes(color = "AZA+DEP"), size = .6) +
  geom_function(data = data.frame(mesure = "Efficacy", analyse = "Final analysis"), fun = ~ dbeta(.x, .3 + 32, .7 + 48), aes(color = "AZA+LEN"), size = .6) +
  geom_function(data = data.frame(mesure = "Toxicity", analyse = "Final analysis"), fun = ~ dbeta(.x, .3 + 23, .7 + 58), aes(color = "AZA"), size = 1) +
  geom_function(data = data.frame(mesure = "Toxicity", analyse = "Final analysis"), fun = ~ dbeta(.x, .3 + 22, .7 + 58), aes(color = "AZA+DEP"), size = .6) +
  geom_function(data = data.frame(mesure = "Toxicity", analyse = "Final analysis"), fun = ~ dbeta(.x, .3 + 21, .7 + 59), aes(color = "AZA+LEN"), size = .6) +
  scale_color_manual(name = "Treatment arm", values = c("#1b9e77", "#7570b3", "#d95f02")) +
  facet_grid(mesure ~ analyse) +
  labs(y = "Density",
       title = "Beta-laws in each arms at each analysis")

Graphe <- ggplot(resultats_4ana4 %>% 
         bind_rows(tibble(seuil = c(resultats_4ana4$seuil[1] - 0.1679898 * .25, resultats_4ana4$seuil[7] + 0.1497700 * .5, resultats_4ana4$seuil[2] - 0.1679898 * .25, resultats_4ana4$seuil[8] + 0.1581375 * .5),
                          analyse = c(.75, 4.5, .75, 4.5),
                          ttt = rep(c("AZA+VPA", "AZA+LEN"), each = 2)))) +
  geom_ribbon(aes(x = analyse, ymin = 0, ymax = seuil), fill = "#D9603B", alpha = .5) +
  geom_ribbon(aes(x = analyse, ymin = seuil, ymax = 1), fill = "#87E990", alpha = .5) +
  annotate(geom = "text", x = 3, y = .3, label = "Stop", color = "#7E3300", size = 5) +
  annotate(geom = "text", x = 3, y = .9, label = "Continue/Accept", color = "#01D758", size = 5) +
  geom_point(data = resultats_4ana4, aes(x = analyse, y = p_eff, color = ttt), size = 3) +
  geom_line(data = resultats_4ana4, aes(x = analyse, y = p_eff, color = ttt, linetype = "Efficacy"), size = 1) +
  geom_point(data = resultats_4ana4, aes(x = analyse, y = p_tox, color = ttt), size = 3) +
  geom_line(data = resultats_4ana4, aes(x = analyse, y = p_tox, color = ttt, linetype = "Toxicity"), size = 1) +
  geom_text(data = text_pat, aes(x = analyse - .15, y = .15, label = nb_patients), hjust = 0, size = 3.5) +
  scale_x_continuous(name = NULL, expand = c(0, 0), breaks = c(1, 2, 3, 4), 
                     label = function(x) paste0("Analysis n°", x)) +
  scale_y_continuous(name = "Pr(p<sub>eff,k</sub>>p<sub>eff,0</sub>|D<sub>n</sub>) or Pr(p<sub>tox,k</sub>&le;p<sub>tox,0</sub>|D<sub>n</sub>)", expand = c(0, 0)) +
  scale_color_manual(name = "Treatment arm", values = c("#7570b3", "#d95f02")) +
  labs(linetype = "Measure") +
  facet_wrap(vars(ttt)) +
  theme(axis.title.y = element_markdown(size = 14),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13))

ggsave(Graphe, units = "px", dpi = 800,
       filename = "Outputs/azaplus.eps",
       device = cairo_ps, height = 7000, width = 10000)
ggsave(Graphe, units = "in",
       filename = "Outputs/azaplus.jpeg",
       device = "jpeg", height = 10, width = 12)

