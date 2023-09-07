# Packages and helpers ----

devtools::load_all("C:/Users/gmulier/Documents/Github/multibrasBOP2/")
# library(multibrasBOP2)
library(tidyverse)
library(ggtext)
library(scales)
library(cowplot)
library(patchwork)
library(flextable)
library(officer)
library(glue)
library(progressr)

handlers(handler_progress(format = ":spin (:message) [:bar] :percent (:current/:total)",
                          complete = "-", current = "|", incomplete = " ", width = 150))

theme_set(theme_light() +
            theme(legend.text = element_markdown(),
                  axis.title.y = element_markdown(),
                  panel.grid.major = element_line(color = "darkgrey", size = .7),
                  axis.title.x = element_markdown(),
                  plot.caption = element_markdown(hjust = 0),
                  strip.background.x = element_blank(),
                  strip.text.x = element_textbox(
                    size = 14, face = "bold", linewidth = .8,
                    color = "black", fill = "transparent", box.color = "black",
                    halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(.5, "npc"),
                    padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3))))

table_scenar_ref <- tribble(~scenar, ~proba, ~num_scenar,
                            "Global H0", "[0](0.15, 0.30, 0.15, 0.40): [0](0.15, 0.30, 0.15, 0.40); [0](0.15, 0.30, 0.15, 0.40)", 1,
                            "LFC", "[1](0.18, 0.42, 0.02, 0.38): [0](0.15, 0.30, 0.15, 0.40); [0](0.15, 0.30, 0.15, 0.40)", 2,
                            "2 H1, 1H0", "[1](0.18, 0.42, 0.02, 0.38): [1](0.18, 0.42, 0.02, 0.38); [0](0.15, 0.30, 0.15, 0.40)", 3,
                            "3 H1", "[1](0.18, 0.42, 0.02, 0.38): [1](0.18, 0.42, 0.02, 0.38); [1](0.18, 0.42, 0.02, 0.38)", 4,
                            "3 better than H1", "[1](0.15, 0.50, 0.05, 0.30): [1](0.15, 0.50, 0.05, 0.30); [1](0.15, 0.50, 0.05, 0.30)", 5, 
                            "3 negative treatments", "[0](0.20, 0.40, 0.10, 0.30): [0](0.10, 0.35, 0.10, 0.45); [0](0.15, 0.30, 0.15, 0.40)", 6,
                            "3 negative treatments (2)", "[0](0.20, 0.50, 0.20, 0.10): [0](0.05, 0.35, 0.05, 0.55); [0](0.15, 0.30, 0.15, 0.40)", 7,
                            "1 works better than H1", "[1](0.15, 0.50, 0.05, 0.30): [0](0.15, 0.30, 0.15, 0.40); [0](0.15, 0.30, 0.15, 0.40)", 8,
                            "3 work and 1 better than H1", "[1](0.15, 0.50, 0.05, 0.30): [1](0.18, 0.42, 0.02, 0.38); [1](0.18, 0.42, 0.02, 0.38)", 9,
                            "2 work and 1 toxic", "[1](0.18, 0.42, 0.02, 0.38): [1](0.18, 0.42, 0.02, 0.38); [0](0.20, 0.50, 0.15, 0.15)", 10,
                            "2 work and 1 not effective", "[1](0.18, 0.42, 0.02, 0.38): [1](0.18, 0.42, 0.02, 0.38); [0](0.10, 0.35, 0.10, 0.45)", 11,
                            "1 works, 2 intermediate", "[1](0.18, 0.42, 0.02, 0.38): [0](0.15, 0.35, 0.10, 0.40); [0](0.15, 0.35, 0.10, 0.40)", 12,
                            "1 H0, 1 H1, 1 intermediate", "[0](0.15, 0.30, 0.15, 0.40): [1](0.18, 0.42, 0.02, 0.38); [0](0.15, 0.35, 0.10, 0.40)", 13) %>% 
  separate(proba, into = c("ttt1", "reste"), sep = ": ") %>% 
  separate(reste, into = c("ttt2", "ttt3"), sep = "; ") %>% 
  pivot_longer(cols = ttt1:ttt3) %>% 
  extract(col = value, into = c("rejet_essai", "value"), regex = "^\\[([0-1])\\](.*)$") %>% 
  mutate(rejet_essai = as.numeric(rejet_essai),
         etiquettes = case_when(name == "ttt1" ~ "Arm A",
                                name == "ttt2" ~ "Arm B",
                                name == "ttt3" ~ "Arm C"),
         etiquettes = factor(etiquettes, levels = c("Arm C", "Arm B", "Arm A")))
table_scenar_aggreg_ref <- tribble(~scenar, ~proba, ~num_scenar,
                                   "Global H0", "[0](<b>0.45</b>, <b>0.30</b>): [0](<b>0.45</b>, <b>0.30</b>); [0](<b>0.45</b>, <b>0.30</b>)", 1,
                                   "LFC", "[1](0.60, 0.20): [0](<b>0.45</b>, <b>0.30</b>); [0](<b>0.45</b>, <b>0.30</b>)", 2,
                                   "2 H1, 1H0", "[1](0.60, 0.20): [1](0.60, 0.20); [0](<b>0.45</b>, <b>0.30</b>)", 3,
                                   "3 H1", "[1](0.60, 0.20): [1](0.60, 0.20); [1](0.60, 0.20)", 4,
                                   "3 better than H1", "[1](0.65, 0.20): [1](0.65, 0.20); [1](0.65, 0.20)", 5, 
                                   "3 negative treatments", "[0](0.60, <b>0.30</b>): [0](<b>0.45</b>, 0.20); [0](<b>0.45</b>, <b>0.30</b>)", 6,
                                   "3 negative treatments (2)", "[0](0.70, <b>0.40</b>): [0](<b>0.40,</b> 0.10); [0](<b>0.45</b>, <b>0.30</b>)", 7,
                                   "1 works better than H1", "[1](0.65, 0.20): [0](<b>0.45</b>, <b>0.30</b>); [0](<b>0.45</b>, <b>0.30</b>)", 8,
                                   "3 work and 1 better than H1", "[1](0.65, 0.20): [1](0.60, 0.20); [1](0.60, 0.20)", 9,
                                   "2 work and 1 toxic", "[1](0.60, 0.20): [1](0.60, 0.20); [0](0.70, <b>0.35</b>)", 10,
                                   "2 work and 1 not effective", "[1](0.60, 0.20): [1](0.60, 0.20); [0](<b>0.45</b>, 0.20)", 11,
                                   "1 works, 2 intermediate", "[1](0.60, 0.20): [0](<b>0.50</b>, <b>0.25</b>); [0](<b>0.50</b>, <b>0.25</b>)", 12,
                                   "1 H0, 1 H1, 1 intermediate", "[0](<b>0.45</b>, <b>0.30</b>): [1](0.60, 0.20); [0](<b>0.50</b>, <b>0.25</b>)", 13) %>% 
  separate(proba, into = c("ttt1", "reste"), sep = ": ") %>% 
  separate(reste, into = c("ttt2", "ttt3"), sep = "; ") %>% 
  pivot_longer(cols = ttt1:ttt3) %>% 
  extract(col = value, into = c("rejet_essai", "value"), regex = "^\\[([0-1])\\](.*)$") %>% 
  mutate(rejet_essai = as.numeric(rejet_essai),
         etiquettes = case_when(name == "ttt1" ~ "Arm A",
                                name == "ttt2" ~ "Arm B",
                                name == "ttt3" ~ "Arm C"),
         etiquettes = factor(etiquettes, levels = c("Arm C", "Arm B", "Arm A")))
table_scenar_cont <- tribble(~scenar, ~proba, ~num_scenar,
                             "Global H0", "[0](0.30, 0.30, 0.10, 0.30): [0](0.30, 0.30, 0.10, 0.30); [0](0.30, 0.30, 0.10, 0.30)", 1,
                             "LFC", "[1](0.25, 0.50, 0.05, 0.20): [0](0.30, 0.30, 0.10, 0.30); [0](0.30, 0.30, 0.10, 0.30)", 2,
                             "2 H1, 1H0", "[1](0.25, 0.50, 0.05, 0.20): [1](0.25, 0.50, 0.05, 0.20); [0](0.30, 0.30, 0.10, 0.30)", 3,
                             "3 H1", "[1](0.25, 0.50, 0.05, 0.20): [1](0.25, 0.50, 0.05, 0.20); [1](0.25, 0.50, 0.05, 0.20)", 4,
                             "3 better than H1", "[1](0.20, 0.60, 0.05, 0.15): [1](0.20, 0.60, 0.05, 0.15); [1](0.20, 0.60, 0.05, 0.15)", 5, 
                             "3 negative treatments", "[0](0.20, 0.40, 0.10, 0.30): [0](0.25, 0.50, 0.15, 0.10); [0](0.30, 0.30, 0.10, 0.30)", 6,
                             "3 negative treatments (2)", "[0](0.20, 0.30, 0.05, 0.45): [0](0.30, 0.50, 0.15, 0.05); [0](0.30, 0.30, 0.10, 0.30)", 7,
                             "1 works better than H1", "[1](0.20, 0.60, 0.05, 0.15): [0](0.30, 0.30, 0.10, 0.30); [0](0.30, 0.30, 0.10, 0.30)", 8,
                             "3 work and 1 better than H1", "[1](0.20, 0.60, 0.05, 0.15): [1](0.25, 0.50, 0.05, 0.20); [1](0.25, 0.50, 0.05, 0.20)", 9,
                             "2 work and 1 toxic", "[1](0.25, 0.50, 0.05, 0.20): [1](0.25, 0.50, 0.05, 0.20); [0](0.25, 0.50, 0.20, 0.05)", 10,
                             "2 work and 1 not effective", "[1](0.25, 0.50, 0.05, 0.20): [1](0.25, 0.50, 0.05, 0.20); [0](0.20, 0.30, 0.05, 0.45)", 11,
                             "1 works, 2 intermediate", "[1](0.25, 0.50, 0.05, 0.20): [0](0.20, 0.50, 0.15, 0.15); [0](0.20, 0.50, 0.15, 0.15)", 12,
                             "1 works, 1 not, 1 intermediate", "[0](0.30, 0.30, 0.10, 0.30): [1](0.25, 0.50, 0.05, 0.20); [0](0.20, 0.50, 0.15, 0.15)", 13) %>% 
  separate(proba, into = c("ttt1", "reste"), sep = ": ") %>% 
  separate(reste, into = c("ttt2", "ttt3"), sep = "; ") %>% 
  pivot_longer(cols = ttt1:ttt3) %>% 
  extract(col = value, into = c("rejet_essai", "value"), regex = "^\\[([0-1])\\](.*)$") %>% 
  mutate(rejet_essai = as.numeric(rejet_essai),
         etiquettes = case_when(name == "ttt1" ~ "Arm A",
                                name == "ttt2" ~ "Arm B",
                                name == "ttt3" ~ "Arm C"),
         etiquettes = factor(etiquettes, levels = c("Arm C", "Arm B", "Arm A")))
table_scenar_aggreg_cont <- tribble(~scenar, ~proba, ~num_scenar,
                                    "Global H0", "[0](<b>0.60</b>, <b>0.40</b>): [0](<b>0.60</b>, <b>0.40</b>); [0](<b>0.60</b>, <b>0.40</b>)", 1,
                                    "LFC", "[1](0.75, 0.30): [0](<b>0.60</b>, <b>0.40</b>); [0](<b>0.60</b>, <b>0.40</b>)", 2,
                                    "2 H1, 1H0", "[1](0.75, 0.30): [1](0.75, 0.30); [0](<b>0.60</b>, <b>0.40</b>)", 3,
                                    "3 H1", "[1](0.75, 0.30): [1](0.75, 0.30); [1](0.75, 0.30)", 4,
                                    "3 better than H1", "[1](0.80, 0.25): [1](0.80, 0.25); [1](0.80, 0.25)", 5, 
                                    "3 negative treatments", "[0](<b>0.60</b>, 0.30): [0](0.75, <b>0.40</b>); [0](<b>0.60</b>, <b>0.40</b>)", 6,
                                    "3 negative treatments (2)", "[0](<b>0.50</b>, 0.25): [0](0.80, <b>0.45</b>); [0](<b>0.60</b>, <b>0.40</b>)", 7,
                                    "1 works better than H1", "[1](0.80, 0.25): [0](<b>0.60</b>, <b>0.40</b>); [0](<b>0.60</b>, <b>0.40</b>)", 8,
                                    "3 work and 1 better than H1", "[1](0.80, 0.25): [1](0.75, 0.30); [1](0.75, 0.30)", 9,
                                    "2 work and 1 toxic", "[1](0.75, 0.30): [1](0.75, 0.30); [0](0.75, <b>0.45</b>)", 10,
                                    "2 work and 1 not effective", "[1](0.75, 0.30): [1](0.75, 0.30); [0](<b>0.50</b>, 0.25)", 11,
                                    "1 works, 2 intermediate", "[1](0.75, 0.30): [0](<b>0.70</b>, <b>0.35</b>); [0](<b>0.70</b>, <b>0.35</b>)", 12,
                                    "1 works, 1 not, 1 intermediate", "[0](<b>0.60</b>, <b>0.40</b>): [1](0.75, 0.30); [0](<b>0.70</b>, <b>0.35</b>)", 13) %>% 
  separate(proba, into = c("ttt1", "reste"), sep = ": ") %>% 
  separate(reste, into = c("ttt2", "ttt3"), sep = "; ") %>% 
  pivot_longer(cols = ttt1:ttt3) %>% 
  extract(col = value, into = c("rejet_essai", "value"), regex = "^\\[([0-1])\\](.*)$") %>% 
  mutate(rejet_essai = as.numeric(rejet_essai),
         etiquettes = case_when(name == "ttt1" ~ "Arm A",
                                name == "ttt2" ~ "Arm B",
                                name == "ttt3" ~ "Arm C"),
         etiquettes = factor(etiquettes, levels = c("Arm C", "Arm B", "Arm A")))

simu <- TRUE # TRUE if you want to simulate the data
simu_seuils <- TRUE # TRUE if you want to simulate the thresholds
clear <- TRUE # TRUE to clear environment often

# I/ 30 patients for uncontrolled design ----

alpha <- .1
n_bras <- 3
p_n <- c(.15, .3, .15, .4)
p_a <- c(.18, .42, .02, .38)
nsim_oc <- 10000
nsim_essais <- 10000
ana_inter <- rep(10, 3)
methode <- 2L
p_ttt <- list("Global H0" = list(ttt1 = p_n, ttt2 = p_n, ttt3 = p_n),
              "LFC" = list(ttt1 = p_a, ttt2 = p_n, ttt3 = p_n),
              "2 H1, 1H0" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = p_n),
              "3 H1" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = p_a),
              "3 better than H1" = list(ttt1 = c(.15, .5, .05, .3), ttt2 = c(.15, .5, .05, .3), ttt3 = c(.15, .5, .05, .3)),
              "3 not effective" = list(ttt1 = c(.2, .4, .1, .3), ttt2 = c(.1, .35, .1, .45), ttt3 = p_n),
              "3 not effective with other endpoint" = list(ttt1 = c(.2, .5, .2, .1), ttt2 = c(.05, .35, .05, .55), ttt3 = p_n),
              "1 works better than H1" = list(ttt1 = c(.15, .5, .05, .3), ttt2 = p_n, ttt3 = p_n),
              "3 work and 1 better than H1" = list(ttt1 = c(.15, .5, .05, .3), ttt2 = p_a, ttt3 = p_a),
              "2 work and 1 toxic" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = c(.2, .5, .15, .15)),
              "2 work and 1 not effective" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = c(.1, .35, .1, .45)),
              "1 works, 2 intermediate" = list(ttt1 = p_a, ttt2 = c(.15, .35, .1, .4), ttt3 = c(.15, .35, .1, .4)),
              "1 works, 1 not, 1 intermediate" = list(ttt1 = p_n, ttt2 = p_a, ttt3 = c(.15, .35, .1, .4)))

## A/ Simulations ----

# Optimize the thresholds or retrieve them from storage
if (simu_seuils) {
  
  cat("Determine the thresholds hyperparameters.\n")
  
  cat("Varying threshold, multiarm.\n")
  cutoff_Cnmulti <- deter_cutoff(alpha = alpha,
                                 ana_inter = ana_inter,
                                 n_bras = n_bras,
                                 p_n = p_n, p_a = p_a,
                                 nsim_oc = nsim_oc,
                                 affich_mat = "No")
  
  cat("Varying threshold, monoarm.\n")
  cutoff_Cnmono <- deter_cutoff(alpha = alpha,
                                ana_inter = ana_inter,
                                p_n = p_n, p_a = p_a,
                                nsim_oc = nsim_oc,
                                affich_mat = "No")
  
  cat("Varying threshold, Bonferroni.\n")
  cutoff_Cnbonf <- deter_cutoff(alpha = alpha,
                                ana_inter = ana_inter,
                                n_bras = n_bras, bonf = TRUE,
                                p_n = p_n, p_a = p_a,
                                nsim_oc = nsim_oc,
                                affich_mat = "No")
  
  cat("Varying threshold, Holm.\n")
  cutoff_Cnholm <- deter_cutoff_holm2(alpha = alpha,
                                      ana_inter = ana_inter,
                                      n_bras = n_bras, 
                                      p_n = p_n, p_a = p_a,
                                      nsim_oc = nsim_oc,
                                      affich_mat = "No",
                                      cut_seq_mono = cutoff_Cnmono[[1]][["C_"]],
                                      power_seq_mono = cutoff_Cnmono[[1]][["gamma"]])
  
  cat("Constant Threshold, multiarm.\n")
  cutoff_Gammulti <- deter_constcutoff(alpha = alpha,
                                       ana_inter = ana_inter,
                                       n_bras = n_bras,
                                       p_n = p_n, p_a = p_a,
                                       nsim_oc = nsim_oc,
                                       affich_mat = "No")
  
  cat("Constant threshold, monoarms.\n")
  cutoff_Gammono <- deter_constcutoff(alpha = alpha,
                                      ana_inter = ana_inter,
                                      p_n = p_n, p_a = p_a,
                                      nsim_oc = nsim_oc,
                                      affich_mat = "No")
  
  cat("Constant threshold, Bonferroni.\n")
  cutoff_Gambonf <- deter_constcutoff(alpha = alpha / n_bras,
                                      ana_inter = ana_inter,
                                      p_n = p_n, p_a = p_a,
                                      nsim_oc = nsim_oc,
                                      affich_mat = "No")
  
  # Saving for later
  save(cutoff_Cnbonf, cutoff_Cnmono, cutoff_Cnmulti, cutoff_Gambonf, cutoff_Gammono, cutoff_Gammulti, cutoff_Cnholm,
       file = paste0("Data/simu_vsref/sens30_uncont_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
} else {
  
  # Else load the stored thresholds
  load("Data/simu_vsref/sens30_uncont_20230609.Rdata")
  
}

# Now that we have the thresholds, simulate the trials
if (simu) {
  
  cat("Operating characteristics for the 13 scenarios :\n")
  opchar_multivar <- list()
  opchar_monovar <- list()
  opchar_multicstt <- list()
  opchar_monocstt <- list()
  opchar_bonfevar <- list()
  opchar_bonfecstt <- list()
  opchar_holm <- list()
  for (i in names(p_ttt)) {
    
    cat(paste0("Simulation n°", i, "' : ", match(i, names(p_ttt)), " / ", length(p_ttt), ".\n"))
    
    cat("Generating the data.\n")
    tableau_ttt <- gen_patients_multinom(n_sim = nsim_essais,
                                         ana_inter = ana_inter,
                                         multinom_ttt = p_ttt[[i]],
                                         seed = 1993)
    
    cat("Varying threshold, multiarm.\n")
    opchar_multivar[[i]] <- opcharac_efftox(ana_inter = ana_inter,
                                            alpha = alpha,
                                            p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                            nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                            tableau_essais = tableau_ttt,
                                            cut_seq = cutoff_Cnmulti[[1]]["C_"], power_seq = cutoff_Cnmulti[[1]]["gamma"])
    
    cat("Varying threshold, Bonferroni.\n")
    opchar_bonfevar[[i]] <- opcharac_efftox(ana_inter = ana_inter,
                                            alpha = alpha,
                                            p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                            nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                            tableau_essais = tableau_ttt,
                                            cut_seq = cutoff_Cnbonf[[1]]["C_"], power_seq = cutoff_Cnbonf[[1]]["gamma"])
    
    cat("Varying threshold, monoarm.\n")
    opchar_monovar[[i]] <- list(ttt1 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt1, p_n = p_n, p_a = p_a,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt1"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt2 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt2, p_n = p_n, p_a = p_a,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt2"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt3 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt3, p_n = p_n, p_a = p_a,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt3"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]))
    
    cat("Constant threshold, multiarm.\n")
    opchar_multicstt[[i]] <- opcharac_efftox_const(ana_inter = ana_inter,
                                                   alpha = alpha,
                                                   p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                                   nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                   tableau_essais = tableau_ttt,
                                                   seq_eff = cutoff_Gammulti[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammulti[["seuils"]]["gamma_tox"])
    
    cat("Constant threshold, monoarm.\n")
    opchar_monocstt[[i]] <- list(ttt1 = opcharac_efftox_const(ana_inter = ana_inter,
                                                              alpha = alpha,
                                                              p_reel = p_ttt[[i]]$ttt1, p_n = p_n, p_a = p_a,
                                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                              tableau_essais = tableau_ttt %>% filter(ttt == "ttt1"),
                                                              seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]),
                                 ttt2 = opcharac_efftox_const(ana_inter = ana_inter,
                                                              alpha = alpha,
                                                              p_reel = p_ttt[[i]]$ttt2, p_n = p_n, p_a = p_a,
                                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                              tableau_essais = tableau_ttt %>% filter(ttt == "ttt2"),
                                                              seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]),
                                 ttt3 = opcharac_efftox_const(ana_inter = ana_inter,
                                                              alpha = alpha,
                                                              p_reel = p_ttt[[i]]$ttt3, p_n = p_n, p_a = p_a,
                                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                              tableau_essais = tableau_ttt %>% filter(ttt == "ttt3"),
                                                              seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]))
    
    
    
    cat("Constant threshold, Bonferroni.\n")
    opchar_bonfecstt[[i]] <- opcharac_efftox_const(ana_inter = ana_inter,
                                                   alpha = alpha,
                                                   p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                                   nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                   tableau_essais = tableau_ttt,
                                                   seq_eff = cutoff_Gambonf[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gambonf[["seuils"]]["gamma_tox"])
    
    cat("Varying threshold, Holm threshold.\n")
    opchar_holm[[i]] <- opcharac_efftox_holm2(ana_inter = ana_inter,
                                              alpha = alpha,
                                              p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                              tableau_essais = tableau_ttt,
                                              cut_seq_mono = cutoff_Cnholm[[1]][["C_mono"]], power_seq_mono = cutoff_Cnholm[[1]][["gamma_mono"]],
                                              cut_seq_holm = cutoff_Cnholm[[1]][["C_holm"]], power_seq_holm = cutoff_Cnholm[[1]][["gamma_holm"]])
    
  }
  
  # Saving raw data
  save(opchar_multivar, opchar_monovar, opchar_multicstt, opchar_monocstt, opchar_bonfevar, opchar_bonfecstt, opchar_holm,
       file = paste0("Data/simu_vsref/sens_bruts_13scenar_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
  if (clear) rm(tableau_ttt)
  
} else {
  
  # Sorry, raw results aren't included
  load("Data/simu_vsref/sens_bruts_13scenar_20230610.Rdata")
  
}


## B/ Results ----

# Regroupe data
tab_Cnmultiarm <- lapply(seq_along(opchar_multivar), function(x) {cbind(scenar = names(opchar_multivar)[x], opchar_multivar[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Multiarm C<sub>n</sub>")
tab_Cnbonferr <- lapply(seq_along(opchar_bonfevar), function(x) {cbind(scenar = names(opchar_bonfevar)[x], opchar_bonfevar[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Bonferroni C<sub>n</sub>")
tab_Cnmonoarm <- lapply(seq_along(opchar_monovar), function(x) {cbind(scenar = names(opchar_monovar)[x], do.call("rbind", lapply(seq_along(opchar_monovar[[x]]), function(index) {
  data.frame(ttt = names(opchar_monovar[[x]][index]),
             rejet_h0 = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["rejet_h0"]),
             arret_precoce = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce"]),
             arret_precoce_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_fut"]),
             arret_precoce_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_tox"]),
             arret_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_fut"]),
             arret_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_tox"]),
             tot_pat = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_pts"]),
             tot_eff = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_eff"]),
             tot_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_tox"]))
})))}) %>% 
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Monoarm C<sub>n</sub>",
         arret = 1 - rejet_h0)
tab_Gammultiarm <- lapply(seq_along(opchar_multicstt), function(x) {cbind(scenar = names(opchar_multicstt)[x], opchar_multicstt[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Multiarm &epsilon;")
tab_Gambonferr <- lapply(seq_along(opchar_bonfecstt), function(x) {cbind(scenar = names(opchar_bonfecstt)[x], opchar_bonfecstt[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Bonferroni &epsilon;")
tab_Gammonoarm <- lapply(seq_along(opchar_monocstt), function(x) {cbind(scenar = names(opchar_monocstt)[x], do.call("rbind", lapply(seq_along(opchar_monocstt[[x]]), function(index) {
  data.frame(ttt = names(opchar_monocstt[[x]][index]),
             rejet_h0 = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["rejet_h0"]),
             arret_precoce = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["arret_precoce"]),
             arret_precoce_fut = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["arret_precoce_fut"]),
             arret_precoce_tox = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["arret_precoce_tox"]),
             arret_fut = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["arret_fut"]),
             arret_tox = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["arret_tox"]),
             tot_pat = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["nb_pts"]),
             tot_eff = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["nb_eff"]),
             tot_tox = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["nb_tox"]))
})))}) %>% 
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Monoarm &epsilon;",
         arret = 1 - rejet_h0)
tab_Cnholm <- lapply(seq_along(opchar_holm), function(x) {cbind(scenar = names(opchar_holm)[x], opchar_holm[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Holm C<sub>n</sub>")
data_ggplot_13vsref <- rbind(tab_Cnmultiarm, tab_Cnbonferr, tab_Cnmonoarm, tab_Gammultiarm, tab_Gambonferr, tab_Gammonoarm, tab_Cnholm)
rm(tab_Cnmultiarm, tab_Cnbonferr, tab_Cnmonoarm, tab_Gammultiarm, tab_Gambonferr, tab_Gammonoarm, tab_Cnholm)
tab_Ralpha <- data.frame(
  methode = c("Multiarm C<sub>n</sub>", "Bonferroni C<sub>n</sub>", "Monoarm C<sub>n</sub>", "Multiarm &epsilon;", "Bonferroni &epsilon;", "Monoarm &epsilon;", "Holm C<sub>n</sub>"),
  global_alpha = c(as.numeric(opchar_multivar[["Global H0"]][["carac_globales"]]["rejet_glob"]),
                   as.numeric(opchar_multivar[["Global H0"]][["carac_globales"]]["rejet_glob"]),
                   lapply(opchar_monovar[["Global H0"]], "[[", "essais") %>% 
                     do.call(what = "rbind", args = .) %>% 
                     group_by(n_simu) %>% 
                     summarise(rejet_glob = sum(decision == "Accept the treatment") > 0) %>% 
                     pull(rejet_glob) %>% mean(),
                   as.numeric(opchar_multicstt[["Global H0"]][["carac_globales"]]["rejet_glob"]),
                   as.numeric(opchar_bonfecstt[["Global H0"]][["carac_globales"]]["rejet_glob"]),
                   lapply(opchar_monocstt[["Global H0"]], "[[", "essais") %>% 
                     do.call(what = "rbind", args = .) %>% 
                     group_by(n_simu) %>% 
                     summarise(rejet_glob = sum(decision == "Accept the treatment") > 0) %>% 
                     pull(rejet_glob) %>% mean(),
                   as.numeric(opchar_holm[["Global H0"]][["carac_globales"]]["rejet_glob"]))
)

data_ggplot_13vsref <- data_ggplot_13vsref %>%
  filter(methode != "Bonferroni C<sub>n</sub>") %>% 
  mutate(scenar = if_else(scenar == "1 works, 1 not, 1 intermediate", "1 H0, 1 H1, 1 intermediate", scenar),
         methode = str_replace_all(methode, "Multiarm", "Multi-arm"),
         methode = str_replace_all(methode, "Monoarm", "Single-arm")) %>% 
  mutate(rejet_h0_mcse = sqrt(rejet_h0 * (1 - rejet_h0) / 10000),
         scenar = factor(scenar, levels = c("Global H0", "LFC", "2 H1, 1H0", "3 H1", "3 better than H1",
                                            "3 not effective", "3 not effective with other endpoint", "1 works better than H1", 
                                            "3 work and 1 better than H1", "2 work and 1 toxic", "2 work and 1 not effective", 
                                            "1 works, 2 intermediate", "1 H0, 1 H1, 1 intermediate"),
                         labels = c("Global H0", "LFC", "2 H1, 1H0", "3 H1", "3 better than H1",
                                    "3 negative treatments", "3 negative treatments (2)", "1 works better than H1", 
                                    "3 work and 1 better than H1", "2 work and 1 toxic", "2 work and 1 not effective", 
                                    "1 works, 2 intermediate", "1 H0, 1 H1, 1 intermediate")),
         methode = str_replace_all(methode, "^Holm (.*)$", "Multi-arm \\1<sup>(aa)</sup>"), 
         methode = factor(methode, 
                          levels = c("Multi-arm C<sub>n</sub>", "Multi-arm C<sub>n</sub><sup>(aa)</sup>", "Single-arm C<sub>n</sub>", "Multi-arm &epsilon;", "Bonferroni &epsilon;", "Single-arm &epsilon;"),
                          labels = c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>m,a</sup>", "C<sub>n</sub><sup>s</sup>", "&epsilon;<sup>m</sup>", "Bonferroni &epsilon;", "&epsilon;<sup>s</sup>")))
data_ggplot_13vsref <- table_scenar_aggreg_ref %>% 
  right_join(data_ggplot_13vsref, by = c("scenar", "name" = "ttt")) %>% 
  mutate(num_scenar = factor(num_scenar, levels = c(13:1)))
plot1 <- ggplot(data_ggplot_13vsref %>% mutate(methode = droplevels(methode))) +
  geom_col(aes(x = num_scenar, y = rejet_h0, fill = etiquettes), color = "black",
           position = position_dodge2(width = .45)) +
  geom_vline(xintercept = c(1:12 + .5), color = "#b1b0b1", linetype = "dashed") +
  geom_richtext(data = tab_Ralpha %>% 
                  filter(methode != "Bonferroni C<sub>n</sub>") %>% 
                  mutate(methode = str_replace_all(methode, "Multiarm", "Multi-arm"),
                         methode = str_replace_all(methode, "Monoarm", "Single-arm"),
                         methode = str_replace_all(methode, "^Holm (.*)$", "Multi-arm \\1<sup>(aa)</sup>"),
                         methode = factor(methode, 
                                          levels = c("Multi-arm C<sub>n</sub>", "Multi-arm C<sub>n</sub><sup>(aa)</sup>", "Single-arm C<sub>n</sub>", "Multi-arm &epsilon;", "Bonferroni &epsilon;", "Single-arm &epsilon;"),
                                          labels = c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>m,a</sup>", "C<sub>n</sub><sup>s</sup>", "&epsilon;<sup>m</sup>", "Bonferroni &epsilon;", "&epsilon;<sup>s</sup>"))), 
                aes(x = 13, y = .35, 
                    label = paste0("Estimated FWER: ", global_alpha * 100, "%")), 
                hjust = 0, label.color = NA) +
  facet_wrap(vars(methode), ncol = 3) +
  theme(panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_continuous(breaks = seq(0, 1, .2), labels = percent_format(), expand = c(0, 0), limits = c(0, .9)) +
  scale_fill_manual(values = c("#FFFFFF", "#113C51", "#B4CB65")) +
  coord_flip() +
  labs(y = "Proportion of rejecting H<sub>0</sub>",
       x = NULL,
       fill = "Treatment arms")
plot2 <- ggplot(data_ggplot_13vsref %>% filter(methode %in% c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>s</sup>"))) +
  geom_text(aes(x = num_scenar, y = 1, label = paste0("Scenario ", num_scenar, ":")), hjust = 0) +
  geom_richtext(aes(x = num_scenar, y = 1.3, label = value, color = etiquettes), hjust = 0, position = position_dodge2(width = .9), fill = NA, label.color = NA) +
  geom_text(x = 14, y = 1.3, aes(label = "(P(Eff), P(Tox))"), color = "black", hjust = 0, fontface = "bold") +
  facet_wrap(vars(methode), ncol = 1) +
  scale_color_manual(values = c("black", "black", "black")) +
  coord_flip(clip = "off") +
  ylim(c(.9, 1.6)) +
  labs(x = NULL,
       y = " ") +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_textbox(
          size = 12, 
          color = "transparent", fill = "transparent", box.color = "transparent",
          halign = 0.5, linetype = 1, r = unit(3, "pt"), width = unit(0.75, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)))
plot_tot <- plot2 + plot1 + plot_layout(ncol = 2, widths = c(1, 3.5))
ggsave(plot_tot, filename = "Outputs/sensibilite/30ptsvsref.png", device = "png", height = 10, width = 15)


# II/ 30 patients for controlled design ----

alpha <- .1
n_bras <- 3
p_n <- c(.3, .3, .1, .3)
p_a <- c(.25, .5, .05, .2)
nsim_oc <- 10000
nsim_essais <- 10000
ana_inter <- rep(10, 3)
methode <- 2L
p_ttt <- list("Global H0" = list(ttt1 = p_n, ttt2 = p_n, ttt3 = p_n),
              "LFC" = list(ttt1 = p_a, ttt2 = p_n, ttt3 = p_n),
              "2 H1, 1H0" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = p_n),
              "3 H1" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = p_a),
              "3 better than H1" = list(ttt1 = c(.2, .6, .05, .15), ttt2 = c(.2, .6, .05, .15), ttt3 = c(.2, .6, .05, .15)),
              "3 not effective" = list(ttt1 = c(.25, .5, .15, .1), ttt2 = c(.2, .4, .1, .3), ttt3 = p_n),
              "3 not effective with other endpoint" = list(ttt1 = c(.3, .5, .15, .05), ttt2 = c(.2, .3, .05, .45), ttt3 = p_n),
              "1 works better than H1" = list(ttt1 = c(.2, .6, .05, .15), ttt2 = p_n, ttt3 = p_n),
              "3 work and 1 better than H1" = list(ttt1 = c(.2, .6, .05, .15), ttt2 = p_a, ttt3 = p_a),
              "2 work and 1 toxic" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = c(.25, .5, .2, .05)),
              "2 work and 1 not effective" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = c(.2, .3, .05, .45)),
              "1 works, 2 intermediate" = list(ttt1 = p_a, ttt2 = c(.2, .5, .15, .15), ttt3 = c(.2, .5, .15, .15)),
              "1 works, 1 not, 1 intermediate" = list(ttt1 = p_n, ttt2 = p_a, ttt3 = c(.2, .5, .15, .15)))

## A/ Simulations ----

# Optimize the thresholds or retrieve them from storage
if (simu_seuils) {
  
  cat("Determine the thresholds hyperparameters.\n")
  
  cat("Varying threshold, multiarm.\n")
  cutoff_Cnmulti <- deter_cutoff(alpha = alpha,
                                 ana_inter = ana_inter,
                                 n_bras = n_bras,
                                 p_n = p_n, p_a = p_a,
                                 nsim_oc = nsim_oc,
                                 delta = c(0, 0),
                                 affich_mat = "No")
  
  cat("Varying threshold, monoarm.\n")
  cutoff_Cnmono <- deter_cutoff(alpha = alpha,
                                ana_inter = ana_inter,
                                p_n = p_n, p_a = p_a,
                                nsim_oc = nsim_oc,
                                delta = c(0, 0),
                                affich_mat = "No")
  
  cat("Varying threshold, Holm.\n")
  cutoff_Cnholm <- deter_cutoff_holm2(alpha = alpha,
                                      ana_inter = ana_inter,
                                      n_bras = n_bras, 
                                      p_n = p_n, p_a = p_a,
                                      nsim_oc = nsim_oc,
                                      delta = c(0, 0),
                                      affich_mat = "No",
                                      cut_seq_mono = cutoff_Cnmono[[1]][["C_"]],
                                      power_seq_mono = cutoff_Cnmono[[1]][["gamma"]])
  
  cat("Varying threshold, Bonferroni.\n")
  cutoff_Cnbonf <- deter_cutoff(alpha = alpha,
                                ana_inter = ana_inter,
                                n_bras = n_bras, bonf = TRUE,
                                p_n = p_n, p_a = p_a,
                                nsim_oc = nsim_oc,
                                delta = c(0, 0),
                                affich_mat = "No")
  
  cat("Constant threshold, multiarm.\n")
  cutoff_Gammulti <- deter_constcutoff(alpha = alpha,
                                       ana_inter = ana_inter,
                                       n_bras = n_bras,
                                       p_n = p_n, p_a = p_a,
                                       nsim_oc = nsim_oc,
                                       delta = c(0, 0),
                                       affich_mat = "No")
  
  cat("Constant threshold, monoarm.\n")
  cutoff_Gammono <- deter_constcutoff(alpha = alpha,
                                      ana_inter = ana_inter,
                                      p_n = p_n, p_a = p_a,
                                      nsim_oc = nsim_oc,
                                      delta = c(0, 0),
                                      affich_mat = "No")
  
  cat("Constant threshold, Bonferroni.\n")
  cutoff_Gambonf <- deter_constcutoff(alpha = alpha,
                                      ana_inter = ana_inter,
                                      n_bras = n_bras, bonf = TRUE,
                                      p_n = p_n, p_a = p_a,
                                      nsim_oc = nsim_oc,
                                      delta = c(0, 0),
                                      affich_mat = "No")
  
  save(cutoff_Cnmulti, cutoff_Cnbonf, cutoff_Cnmono, cutoff_Gammulti, cutoff_Gambonf, cutoff_Gammono, cutoff_Cnholm,
       file = paste0("Data/simu_vscont/senscutoff_13scenar_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
} else {
  
  load("Data/simu_vscont/senscutoff_13scenar_20230610.Rdata")
  
}

# With the thresholds computed, we can simulate trials
if (simu) {
  
  cat("Operating characteristics:\n")
  opchar_multivar <- list()
  opchar_bonfvar <- list()
  opchar_monovar <- list()
  opchar_multicstt <- list()
  opchar_monocstt <- list()
  opchar_bonfcstt <- list()
  opchar_holm <- list()
  for (i in names(p_ttt)) {
    
    cat(paste0("Simulation n°", i, "' : ", match(i, names(p_ttt)), " / ", length(p_ttt), ".\n"))
    
    cat("Generate data of the trials.\n")
    tableau_ttt <- gen_patients_multinom(n_sim = nsim_essais, 
                                         ana_inter = ana_inter,
                                         multinom_ttt = p_ttt[[i]],
                                         multinom_cont = p_n,
                                         seed = 1993)
    
    cat("Varying threshold, multiarm.\n")
    opchar_multivar[[i]] <- opcharac_efftox(ana_inter = ana_inter,
                                            alpha = alpha,
                                            p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                            nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                            tableau_essais = tableau_ttt, delta = c(0, 0),
                                            cut_seq = cutoff_Cnmulti[[1]]["C_"], power_seq = cutoff_Cnmulti[[1]]["gamma"])
    
    cat("Varying threshold, Holm.\n")
    opchar_holm[[i]] <- opcharac_efftox_holm2(ana_inter = ana_inter,
                                              alpha = alpha,
                                              p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                              tableau_essais = tableau_ttt, delta = c(0, 0),
                                              cut_seq_mono = cutoff_Cnholm[[1]][["C_mono"]], power_seq_mono = cutoff_Cnholm[[1]][["gamma_mono"]],
                                              cut_seq_holm = cutoff_Cnholm[[1]][["C_holm"]], power_seq_holm = cutoff_Cnholm[[1]][["gamma_holm"]])
    
    cat("Varying threshold, Bonferroni.\n")
    opchar_bonfvar[[i]] <- opcharac_efftox(ana_inter = ana_inter,
                                           alpha = alpha,
                                           p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                           nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                           tableau_essais = tableau_ttt, delta = c(0, 0),
                                           cut_seq = cutoff_Cnbonf[[1]]["C_"], power_seq = cutoff_Cnbonf[[1]]["gamma"])
    
    cat("Varying threshold, monoarm.\n")
    opchar_monovar[[i]] <- list(ttt1 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt1, p_n = p_n, p_a = p_a,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt %in% c("ttt0", "ttt1")), delta = c(0, 0),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt2 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt2, p_n = p_n, p_a = p_a,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt %in% c("ttt0", "ttt2")), delta = c(0, 0),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt3 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt3, p_n = p_n, p_a = p_a,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt %in% c("ttt0", "ttt3")), delta = c(0, 0),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]))
    
    cat("Constant threshold, multiarm.\n")
    opchar_multicstt[[i]] <- opcharac_efftox_const(ana_inter = ana_inter,
                                                   alpha = alpha,
                                                   p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                                   nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                   tableau_essais = tableau_ttt, delta = c(0, 0),
                                                   seq_eff = cutoff_Gammulti[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammulti[["seuils"]]["gamma_tox"])
    
    cat("Constant threshold, Bonferroni.\n")
    opchar_bonfcstt[[i]] <- opcharac_efftox_const(ana_inter = ana_inter,
                                                  alpha = alpha,
                                                  p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                                  nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                  tableau_essais = tableau_ttt, delta = c(0, 0),
                                                  seq_eff = cutoff_Gambonf[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gambonf[["seuils"]]["gamma_tox"])
    
    cat("Constant threshold, monoarm.\n")
    opchar_monocstt[[i]] <- list(ttt1 = opcharac_efftox_const(ana_inter = ana_inter,
                                                              alpha = alpha,
                                                              p_reel = p_ttt[[i]]$ttt1, p_n = p_n, p_a = p_a,
                                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                              tableau_essais = tableau_ttt %>% filter(ttt %in% c("ttt0", "ttt1")), delta = c(0, 0),
                                                              seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]),
                                 ttt2 = opcharac_efftox_const(ana_inter = ana_inter,
                                                              alpha = alpha,
                                                              p_reel = p_ttt[[i]]$ttt2, p_n = p_n, p_a = p_a,
                                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                              tableau_essais = tableau_ttt %>% filter(ttt %in% c("ttt0", "ttt2")), delta = c(0, 0),
                                                              seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]),
                                 ttt3 = opcharac_efftox_const(ana_inter = ana_inter,
                                                              alpha = alpha,
                                                              p_reel = p_ttt[[i]]$ttt3, p_n = p_n, p_a = p_a,
                                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                              tableau_essais = tableau_ttt %>% filter(ttt %in% c("ttt0", "ttt3")), delta = c(0, 0),
                                                              seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]))
    
  }
  
  # Saving raw data
  save(opchar_multivar, opchar_bonfvar, opchar_monovar, opchar_multicstt, opchar_monocstt, opchar_bonfcstt, opchar_holm,
       file = paste0("Data/simu_vscont/resultatssens_brutscont_13scenar_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
} else {
  
  # If you have raw data stored, load them
  load("Data/simu_vscont/resultatssens_brutscont_13scenar_20230610.Rdata")
  
}


## B/ Results ----

tab_Cnmultiarm <- lapply(seq_along(opchar_multivar), function(x) {cbind(scenar = names(opchar_multivar)[x], opchar_multivar[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Multiarm C<sub>n</sub>")
tab_Cnholm <- lapply(seq_along(opchar_holm), function(x) {cbind(scenar = names(opchar_holm)[x], opchar_holm[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Holm C<sub>n</sub>")
tab_Cnbonferr <- lapply(seq_along(opchar_bonfvar), function(x) {cbind(scenar = names(opchar_bonfvar)[x], opchar_bonfvar[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Bonferroni C<sub>n</sub>")
tab_Cnmonoarm <- lapply(seq_along(opchar_monovar), function(x) {cbind(scenar = names(opchar_monovar)[x], do.call("rbind", lapply(seq_along(opchar_monovar[[x]]), function(index) {
  data.frame(ttt = names(opchar_monovar[[x]][index]),
             rejet_h0 = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["rejet_h0"]),
             arret_precoce = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce"]),
             arret_precoce_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_fut"]),
             arret_precoce_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_tox"]),
             arret_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_fut"]),
             arret_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_tox"]),
             tot_pat = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_pts"]),
             tot_eff = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_eff"]),
             tot_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_tox"]))
})))}) %>% 
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Monoarm C<sub>n</sub>",
         arret = 1 - rejet_h0)
tab_Gammultiarm <- lapply(seq_along(opchar_multicstt), function(x) {cbind(scenar = names(opchar_multicstt)[x], opchar_multicstt[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Multiarm &epsilon;")
tab_Gambonferr <- lapply(seq_along(opchar_bonfcstt), function(x) {cbind(scenar = names(opchar_bonfcstt)[x], opchar_bonfcstt[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Bonferroni &epsilon;")
tab_Gammonoarm <- lapply(seq_along(opchar_monocstt), function(x) {cbind(scenar = names(opchar_monocstt)[x], do.call("rbind", lapply(seq_along(opchar_monocstt[[x]]), function(index) {
  data.frame(ttt = names(opchar_monocstt[[x]][index]),
             rejet_h0 = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["rejet_h0"]),
             arret_precoce = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["arret_precoce"]),
             arret_precoce_fut = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["arret_precoce_fut"]),
             arret_precoce_tox = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["arret_precoce_tox"]),
             arret_fut = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["arret_fut"]),
             arret_tox = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["arret_tox"]),
             tot_pat = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["nb_pts"]),
             tot_eff = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["nb_eff"]),
             tot_tox = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["nb_tox"]))
})))}) %>% 
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Monoarm &epsilon;",
         arret = 1 - rejet_h0)
data_ggplot_13vscont <- rbind(tab_Cnmultiarm, tab_Cnbonferr, tab_Cnmonoarm, tab_Gammultiarm, tab_Gambonferr, tab_Gammonoarm, tab_Cnholm)
tab_Ralpha <- data.frame(
  methode = c("Multiarm C<sub>n</sub>", "Bonferroni C<sub>n</sub>", "Monoarm C<sub>n</sub>", "Multiarm &epsilon;", "Bonferroni &epsilon;", "Monoarm &epsilon;", "Holm C<sub>n</sub>"),
  global_alpha = c(as.numeric(opchar_multivar[["Global H0"]][["carac_globales"]]["rejet_glob"]),
                   as.numeric(opchar_bonfvar[["Global H0"]][["carac_globales"]]["rejet_glob"]),
                   lapply(opchar_monovar[["Global H0"]], "[[", "essais") %>% 
                     do.call(what = "rbind", args = .) %>% 
                     group_by(n_simu) %>% 
                     summarise(rejet_glob = sum(decision == "Accept the treatment") > 0) %>% 
                     pull(rejet_glob) %>% mean(),
                   as.numeric(opchar_multicstt[["Global H0"]][["carac_globales"]]["rejet_glob"]),
                   as.numeric(opchar_bonfcstt[["Global H0"]][["carac_globales"]]["rejet_glob"]),
                   lapply(opchar_monocstt[["Global H0"]], "[[", "essais") %>% 
                     do.call(what = "rbind", args = .) %>% 
                     group_by(n_simu) %>% 
                     summarise(rejet_glob = sum(decision == "Accept the treatment") > 0) %>% 
                     pull(rejet_glob) %>% mean(),
                   as.numeric(opchar_holm[["Global H0"]][["carac_globales"]]["rejet_glob"]))
)
rm(tab_Cnmultiarm, tab_Cnbonferr, tab_Cnmonoarm, tab_Gammultiarm, tab_Gambonferr, tab_Gammonoarm, tab_Cnholm)

data_ggplot_13vscont <- data_ggplot_13vscont %>% 
  mutate(rejet_h0_mcse = sqrt(rejet_h0 * (1 - rejet_h0) / 10000),
         scenar = factor(scenar, levels = c("Global H0", "LFC", "2 H1, 1H0", "3 H1", "3 better than H1",
                                            "3 not effective", "3 not effective with other endpoint", "1 works better than H1", 
                                            "3 work and 1 better than H1", "2 work and 1 toxic", "2 work and 1 not effective", 
                                            "1 works, 2 intermediate", "1 works, 1 not, 1 intermediate"),
                         labels = c("Global H0", "LFC", "2 H1, 1H0", "3 H1", "3 better than H1",
                                    "3 negative treatments", "3 negative treatments (2)", "1 works better than H1", 
                                    "3 work and 1 better than H1", "2 work and 1 toxic", "2 work and 1 not effective", 
                                    "1 works, 2 intermediate", "1 works, 1 not, 1 intermediate")),
         methode = str_replace_all(methode, "Multiarm", "Multi-arm"),
         methode = str_replace_all(methode, "Monoarm", "Single-arm"),
         methode = str_replace_all(methode, "^Holm (.*)$", "Multi-arm \\1<sup>(aa)</sup>"),
         methode = factor(methode, levels = c("Multi-arm C<sub>n</sub>", "Multi-arm C<sub>n</sub><sup>(aa)</sup>", "Single-arm C<sub>n</sub>", "Multi-arm &epsilon;", "Bonferroni &epsilon;", "Single-arm &epsilon;"))) %>% 
  filter(!is.na(methode))
data_ggplot_13vscont <- table_scenar_aggreg_cont %>% 
  right_join(data_ggplot_13vscont, by = c("scenar", "name" = "ttt")) %>% 
  mutate(num_scenar = factor(num_scenar, levels = c(13:1)))
plot1 <- ggplot(data_ggplot_13vscont) +
  geom_col(aes(x = num_scenar, y = rejet_h0, fill = etiquettes), color = "black",
           position = position_dodge2(width = .45)) +
  geom_vline(xintercept = c(1:12 + .5), color = "#b1b0b1", linetype = "dashed") +
  geom_richtext(data = tab_Ralpha %>% 
                  mutate(methode = str_replace_all(methode, "Multiarm", "Multi-arm"),
                         methode = str_replace_all(methode, "Monoarm", "Single-arm"),
                         methode = str_replace_all(methode, "^Holm (.*)$", "Multi-arm \\1<sup>(aa)</sup>"),
                         methode = factor(methode, levels = c("Multi-arm C<sub>n</sub>", "Multi-arm C<sub>n</sub><sup>(aa)</sup>", "Single-arm C<sub>n</sub>", "Multi-arm &epsilon;", "Bonferroni &epsilon;", "Single-arm &epsilon;"))) %>% 
                  filter(!is.na(methode)), 
                aes(x = 13, y = .35, 
                    label = paste0("Estimated FWER : ", global_alpha * 100, "%")), 
                hjust = 0, label.color = NA) +
  facet_wrap(vars(methode), ncol = 3) +
  theme(panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_continuous(breaks = seq(0, 1, .2), labels = percent_format(), expand = c(0, 0), limits = c(0, .9)) +
  scale_fill_manual(values = c("#AA3939", "#113C51", "#B4CB65")) +
  coord_flip() +
  labs(y = "Proportion of rejecting H<sub>0</sub>",
       x = NULL,
       fill = "Treatment arms")
plot2 <- ggplot(data_ggplot_13vscont %>% filter(methode %in% c("Multi-arm C<sub>n</sub>", "Single-arm C<sub>n</sub>"))) +
  geom_text(aes(x = num_scenar, y = 1, label = paste0("Scenario ", num_scenar, ":")), hjust = 0) +
  geom_richtext(aes(x = num_scenar, y = 1.3, label = value, color = etiquettes), hjust = 0, position = position_dodge2(width = .9), fill = NA, label.color = NA) +
  geom_text(x = 14, y = 1.3, aes(label = "(P(Eff), P(Tox))"), color = "black", hjust = 0, fontface = "bold") +
  facet_wrap(vars(methode), ncol = 1) +
  scale_color_manual(values = c("black", "black", "black")) +
  coord_flip(clip = "off") +
  ylim(c(.9, 1.8)) +
  labs(x = NULL,
       y = " ") +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.text.x = element_textbox(
          size = 12, 
          color = "transparent", fill = "transparent", box.color = "transparent",
          halign = 0.5, linetype = 1, r = unit(3, "pt"), width = unit(0.75, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)))
plot_tot <- plot2 + plot1 + plot_layout(ncol = 2, widths = c(1, 3.5)) + plot_annotation(subtitle = "Control arm: (0.60, 0.40)")
ggsave(plot_tot, filename = "Outputs/sensibilite/30ptsvscont.png", device = "png", height = 10, width = 15)


# III/ Neutral prior with ESS = 1 (uncontrolled setting) ----

alpha <- .1
n_bras <- 3
p_n <- c(.15, .3, .15, .4)
p_a <- c(.18, .42, .02, .38)
nsim_oc <- 10000
nsim_essais <- 10000
ana_inter <- rep(15, 4)
methode <- 2L
p_ttt <- list("Global H0" = list(ttt1 = p_n, ttt2 = p_n, ttt3 = p_n),
              "LFC" = list(ttt1 = p_a, ttt2 = p_n, ttt3 = p_n),
              "2 H1, 1H0" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = p_n),
              "3 H1" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = p_a),
              "3 better than H1" = list(ttt1 = c(.15, .5, .05, .3), ttt2 = c(.15, .5, .05, .3), ttt3 = c(.15, .5, .05, .3)),
              "3 not effective" = list(ttt1 = c(.2, .4, .1, .3), ttt2 = c(.1, .35, .1, .45), ttt3 = p_n),
              "3 not effective with other endpoint" = list(ttt1 = c(.2, .5, .2, .1), ttt2 = c(.05, .35, .05, .55), ttt3 = p_n),
              "1 works better than H1" = list(ttt1 = c(.15, .5, .05, .3), ttt2 = p_n, ttt3 = p_n),
              "3 work and 1 better than H1" = list(ttt1 = c(.15, .5, .05, .3), ttt2 = p_a, ttt3 = p_a),
              "2 work and 1 toxic" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = c(.2, .5, .15, .15)),
              "2 work and 1 not effective" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = c(.1, .35, .1, .45)),
              "1 works, 2 intermediate" = list(ttt1 = p_a, ttt2 = c(.15, .35, .1, .4), ttt3 = c(.15, .35, .1, .4)),
              "1 works, 1 not, 1 intermediate" = list(ttt1 = p_n, ttt2 = p_a, ttt3 = c(.15, .35, .1, .4)))


## A/ Simulations ----

# Optimize the thresholds or retrieve them from storage
if (simu_seuils) {
  
  cat("Determine the thresholds hyperparameters.\n")
  
  cat("Varying threshold, multiarm.\n")
  cutoff_Cnmulti <- deter_cutoff(alpha = alpha,
                                 ana_inter = ana_inter,
                                 n_bras = n_bras,
                                 p_n = p_n, p_a = p_a,
                                 prior = c(.25, .25, .25, .25),
                                 nsim_oc = nsim_oc,
                                 affich_mat = "No")
  
  cat("Varying threshold, monoarm.\n")
  cutoff_Cnmono <- deter_cutoff(alpha = alpha,
                                ana_inter = ana_inter,
                                p_n = p_n, p_a = p_a,
                                nsim_oc = nsim_oc,
                                prior = c(.25, .25, .25, .25),
                                affich_mat = "No")
  
  cat("Varying threshold, Holm.\n")
  cutoff_Cnholm <- deter_cutoff_holm2(alpha = alpha,
                                      ana_inter = ana_inter,
                                      n_bras = n_bras, 
                                      p_n = p_n, p_a = p_a,
                                      prior = c(.25, .25, .25, .25),
                                      nsim_oc = nsim_oc,
                                      affich_mat = "No",
                                      cut_seq_mono = cutoff_Cnmono[[1]][["C_"]],
                                      power_seq_mono = cutoff_Cnmono[[1]][["gamma"]])
  
  # Saving for later
  save(cutoff_Cnmulti, cutoff_Cnholm, cutoff_Cnmono,
       file = paste0("Data/simu_vsref/senspriorneut1_uncont_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
} else {
  
  # Else load the stored thresholds
  load("Data/simu_vsref/senspriorneut1_uncont_20230612.Rdata")
  
}

# Now that we have the thresholds, simulate the trials
if (simu) {
  
  cat("Operating characteristics for the 13 scenarios :\n")
  opchar_multivar <- list()
  opchar_monovar <- list()
  opchar_holm <- list()
  for (i in names(p_ttt)) {
    
    cat(paste0("Simulation n°", i, "' : ", match(i, names(p_ttt)), " / ", length(p_ttt), ".\n"))
    
    cat("Generating the data.\n")
    tableau_ttt <- gen_patients_multinom(n_sim = nsim_essais,
                                         ana_inter = ana_inter,
                                         multinom_ttt = p_ttt[[i]],
                                         seed = 1993)
    
    cat("Varying threshold, multiarm.\n")
    opchar_multivar[[i]] <- opcharac_efftox(ana_inter = ana_inter,
                                            alpha = alpha,
                                            p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                            prior = c(.25, .25, .25, .25),
                                            nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                            tableau_essais = tableau_ttt,
                                            cut_seq = cutoff_Cnmulti[[1]]["C_"], power_seq = cutoff_Cnmulti[[1]]["gamma"])
    
    cat("Varying threshold, monoarm.\n")
    opchar_monovar[[i]] <- list(ttt1 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt1, p_n = p_n, p_a = p_a,
                                                       prior = c(.25, .25, .25, .25),
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt1"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt2 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt2, p_n = p_n, p_a = p_a,
                                                       prior = c(.25, .25, .25, .25),
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt2"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt3 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt3, p_n = p_n, p_a = p_a,
                                                       prior = c(.25, .25, .25, .25),
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt3"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]))
    
    cat("Varying threshold, Holm threshold.\n")
    opchar_holm[[i]] <- opcharac_efftox_holm2(ana_inter = ana_inter,
                                              alpha = alpha,
                                              p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                              prior = c(.25, .25, .25, .25),
                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                              tableau_essais = tableau_ttt,
                                              cut_seq_mono = cutoff_Cnholm[[1]][["C_mono"]], power_seq_mono = cutoff_Cnholm[[1]][["gamma_mono"]],
                                              cut_seq_holm = cutoff_Cnholm[[1]][["C_holm"]], power_seq_holm = cutoff_Cnholm[[1]][["gamma_holm"]])
    
  }
  
  # Saving raw data
  save(opchar_multivar, opchar_monovar, opchar_holm,
       file = paste0("Data/simu_vsref/senspriorneut1_bruts_13scenar_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
  if (clear) rm(tableau_ttt)
  
} else {
  
  # Sorry, raw results aren't included
  load("Data/simu_vsref/senspriorneut1_bruts_13scenar_20230612.Rdata")
  
}


## B/ Results ----

# Regroupe data
tab_Cnmultiarm <- lapply(seq_along(opchar_multivar), function(x) {cbind(scenar = names(opchar_multivar)[x], opchar_multivar[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "C<sub>n</sub><sup>m</sup>")
tab_Cnmonoarm <- lapply(seq_along(opchar_monovar), function(x) {cbind(scenar = names(opchar_monovar)[x], do.call("rbind", lapply(seq_along(opchar_monovar[[x]]), function(index) {
  data.frame(ttt = names(opchar_monovar[[x]][index]),
             rejet_h0 = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["rejet_h0"]),
             arret_precoce = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce"]),
             arret_precoce_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_fut"]),
             arret_precoce_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_tox"]),
             arret_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_fut"]),
             arret_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_tox"]),
             tot_pat = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_pts"]),
             tot_eff = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_eff"]),
             tot_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_tox"]))
})))}) %>% 
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "C<sub>n</sub><sup>s</sup>",
         arret = 1 - rejet_h0)
tab_Cnholm <- lapply(seq_along(opchar_holm), function(x) {cbind(scenar = names(opchar_holm)[x], opchar_holm[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "C<sub>n</sub><sup>m,a</sup>")
data_ggplot_13vsref <- rbind(tab_Cnmultiarm, tab_Cnmonoarm, tab_Cnholm)
rm(tab_Cnmultiarm, tab_Cnmonoarm, tab_Cnholm)
tab_Ralpha <- data.frame(
  methode = c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>s</sup>", "C<sub>n</sub><sup>m,a</sup>"),
  global_alpha = c(as.numeric(opchar_multivar[["Global H0"]][["carac_globales"]]["rejet_glob"]),
                   lapply(opchar_monovar[["Global H0"]], "[[", "essais") %>% 
                     do.call(what = "rbind", args = .) %>% 
                     group_by(n_simu) %>% 
                     summarise(rejet_glob = sum(decision == "Accept the treatment") > 0) %>% 
                     pull(rejet_glob) %>% mean(),
                   as.numeric(opchar_holm[["Global H0"]][["carac_globales"]]["rejet_glob"]))
)

data_ggplot_13vsref <- data_ggplot_13vsref %>%
  mutate(scenar = if_else(scenar == "1 works, 1 not, 1 intermediate", "1 H0, 1 H1, 1 intermediate", scenar)) %>% 
  mutate(rejet_h0_mcse = sqrt(rejet_h0 * (1 - rejet_h0) / 10000),
         scenar = factor(scenar, levels = c("Global H0", "LFC", "2 H1, 1H0", "3 H1", "3 better than H1",
                                            "3 not effective", "3 not effective with other endpoint", "1 works better than H1", 
                                            "3 work and 1 better than H1", "2 work and 1 toxic", "2 work and 1 not effective", 
                                            "1 works, 2 intermediate", "1 H0, 1 H1, 1 intermediate"),
                         labels = c("Global H0", "LFC", "2 H1, 1H0", "3 H1", "3 better than H1",
                                    "3 negative treatments", "3 negative treatments (2)", "1 works better than H1", 
                                    "3 work and 1 better than H1", "2 work and 1 toxic", "2 work and 1 not effective", 
                                    "1 works, 2 intermediate", "1 H0, 1 H1, 1 intermediate")),
         methode = factor(methode, 
                          levels = c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>s</sup>", "C<sub>n</sub><sup>m,a</sup>")))
data_ggplot_13vsref <- table_scenar_aggreg_ref %>% 
  right_join(data_ggplot_13vsref, by = c("scenar", "name" = "ttt")) %>% 
  mutate(num_scenar = factor(num_scenar, levels = c(13:1)))
plot1 <- ggplot(data_ggplot_13vsref) +
  geom_col(aes(x = num_scenar, y = rejet_h0, fill = etiquettes), color = "black",
           position = position_dodge2(width = .45), show.legend = FALSE) +
  geom_vline(xintercept = c(1:12 + .5), color = "#b1b0b1", linetype = "dashed") +
  geom_richtext(data = tab_Ralpha, 
                aes(x = 13, y = .35, 
                    label = paste0("Estimated FWER: ", global_alpha * 100, "%")), 
                hjust = 0, label.color = NA) +
  facet_wrap(vars(methode), ncol = 3) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "darkgrey", size = .7),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_continuous(breaks = seq(0, 1, .2), labels = percent_format(), expand = c(0, 0), limits = c(0, .9)) +
  scale_fill_manual(values = c("#FFFFFF", "#113C51", "#B4CB65")) +
  coord_flip() +
  labs(y = "Proportion of conclusion to efficacy and no toxicity",
       x = NULL,
       fill = "Treatment arms")
plot2 <- ggplot(data_ggplot_13vsref %>% 
                  filter(methode %in% c("C<sub>n</sub><sup>m</sup>")) %>% 
                  mutate(etiquettes = factor(str_remove(etiquettes, "^Arm "), levels = c("C", "B", "A")),
                         label_scenar = if_else(num_scenar %in% c("10","11", "12", "13"), paste0("Scenario ", num_scenar, ": Arm"), paste0("Scenario   ", num_scenar, ": Arm")))) +
  geom_text(aes(x = num_scenar, y = 1.1, label = label_scenar), hjust = 0) +
  map(0:12, ~ annotate(geom = "segment", x = 0.7 + .x, xend = 1.3 + .x, y = 1.29, yend = 1.29)) +
  geom_richtext(aes(x = num_scenar, y = 1.3, label = etiquettes, color = etiquettes), hjust = 0, position = position_dodge2(width = .9), fill = NA, label.color = NA) +
  facet_wrap(vars(methode), ncol = 1) +
  scale_color_manual(values = c("black", "black", "black")) +
  coord_flip(clip = "off") +
  ylim(c(1, 1.35)) +
  labs(x = NULL,
       y = " ") +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_textbox(
          size = 12, 
          color = "transparent", fill = "transparent", box.color = "transparent",
          halign = 0.5, linetype = 1, r = unit(3, "pt"), width = unit(0.75, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)))
plot_tot <- plot2 + plot1 + plot_layout(ncol = 2, widths = c(1, 5))
ggsave(plot_tot, filename = "Outputs/sensibilite/priorneut1vsref.png", device = "png", height = 10, width = 15)

data_stackedbar <- data_ggplot_13vsref %>% 
  mutate(arret_final = arret - arret_precoce,
         nom_axe = paste0(num_scenar, ":", name),
         nom_axe = factor(nom_axe, levels = paste0(rep(13:1, each = 3), ":", rep(c("ttt3", "ttt2", "ttt1"), 13)), labels = paste0(rep(13:1, each = 3), ":", rep(c("Arm C", "Arm B", "Arm A"), 13)))) %>% 
  pivot_longer(cols = c(arret_precoce, arret_final), names_to = "type_arret", values_to = "pourcent") 
liste_plots <- map(as.character(unique(data_stackedbar$methode)),
                   function(x) {
                     ggplot(data_stackedbar %>% filter(methode == x)) +
                       geom_col(aes(x = nom_axe, y = pourcent, fill = type_arret), 
                                color = "#929494", position = "stack", width = 0.8) +
                       geom_vline(xintercept = c((1:12) * 3 + .5), color = "#666c6c", linetype = "dashed", size = 1) +
                       facet_wrap(vars(methode)) +
                       theme(panel.grid.major.y = element_blank(),
                             panel.grid.major.x = element_line(color = "darkgrey", size = .7),
                             axis.ticks.y = element_blank(),
                             axis.text.y = element_blank(),
                             panel.grid.minor.y = element_blank(),
                             legend.position = "right") +
                       scale_y_continuous(breaks = seq(0, .9, .2), labels = percent_format(), expand = c(0, 0), limits = c(0, 1)) +
                       scale_fill_manual(name = NULL, values = c("#b2df8a", "#a6cee3"), labels = c("Stopping at final analysis", "Stopping at interim analysis")) +
                       coord_flip() +
                       labs(y = "Stopping of the trial",
                            x = NULL)
                   })
legende <- get_legend(liste_plots[[1]])
plot_inter <- wrap_plots(map(liste_plots, ~ .x + theme(legend.position = "none")))
plot2 <- ggplot(data = data_stackedbar %>% 
                  filter(methode == "C<sub>n</sub><sup>m</sup>") %>%
                  mutate(etiqu = if_else(etiquettes == "Arm B", paste0("Scénario ", num_scenar, ":"), NA_character_))) +
  geom_text(aes(x = nom_axe, y = 1, label = etiqu), hjust = 0) +
  geom_text(aes(x = nom_axe, y = 1.15, label = etiquettes), hjust = 0) +
  geom_richtext(aes(x = nom_axe, y = 1.25, label = value), hjust = 0, fill = NA, label.color = NA) +
  annotate(geom = "segment", x = c((1:12) * 3 + .5), xend = c((1:12) * 3 + .5), y = 1, yend = 1.5,
           color = "#666c6c", linetype = "dashed", size = 1) +
  geom_text(x = 40, y = 1.275, aes(label = "(P(Eff), P(Tox))"), color = "black", hjust = 0, fontface = "bold") +
  facet_wrap(vars(methode), ncol = 1) +
  scale_color_manual(values = c("black", "black", "black")) +
  coord_flip(clip = "off") +
  ylim(c(.9, 1.6)) +
  labs(x = " ",
       y = " ") +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(colour = "transparent"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "transparent"),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_textbox(
          size = 12, 
          color = "transparent", fill = "transparent", box.color = "transparent",
          halign = 0.5, linetype = 1, r = unit(3, "pt"), width = unit(0.75, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)))
plot_cn <- (plot2 | wrap_plots(map(map(1:3, ~ liste_plots[[.x]]), ~ .x + theme(legend.position = "none")))) + plot_layout(widths = c(1.5, 3))
plot_tot <- (plot_cn | legende) + plot_layout(widths = c(2, 4, 1))
ggsave(plot_tot, filename = "Outputs/sensibilite/priorneut1vsref_ap.png", device = "png", height = 10, width = 15)


# IV/ Optimistic prior with ESS = 1 (uncontrolled setting) ----

alpha <- .1
n_bras <- 3
p_n <- c(.15, .3, .15, .4)
p_a <- c(.18, .42, .02, .38)
nsim_oc <- 10000
nsim_essais <- 10000
ana_inter <- rep(15, 4)
methode <- 2L
p_ttt <- list("Global H0" = list(ttt1 = p_n, ttt2 = p_n, ttt3 = p_n),
              "LFC" = list(ttt1 = p_a, ttt2 = p_n, ttt3 = p_n),
              "2 H1, 1H0" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = p_n),
              "3 H1" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = p_a),
              "3 better than H1" = list(ttt1 = c(.15, .5, .05, .3), ttt2 = c(.15, .5, .05, .3), ttt3 = c(.15, .5, .05, .3)),
              "3 not effective" = list(ttt1 = c(.2, .4, .1, .3), ttt2 = c(.1, .35, .1, .45), ttt3 = p_n),
              "3 not effective with other endpoint" = list(ttt1 = c(.2, .5, .2, .1), ttt2 = c(.05, .35, .05, .55), ttt3 = p_n),
              "1 works better than H1" = list(ttt1 = c(.15, .5, .05, .3), ttt2 = p_n, ttt3 = p_n),
              "3 work and 1 better than H1" = list(ttt1 = c(.15, .5, .05, .3), ttt2 = p_a, ttt3 = p_a),
              "2 work and 1 toxic" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = c(.2, .5, .15, .15)),
              "2 work and 1 not effective" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = c(.1, .35, .1, .45)),
              "1 works, 2 intermediate" = list(ttt1 = p_a, ttt2 = c(.15, .35, .1, .4), ttt3 = c(.15, .35, .1, .4)),
              "1 works, 1 not, 1 intermediate" = list(ttt1 = p_n, ttt2 = p_a, ttt3 = c(.15, .35, .1, .4)))


## A/ Simulations ----

# Optimize the thresholds or retrieve them from storage
if (simu_seuils) {
  
  cat("Determine the thresholds hyperparameters.\n")
  
  cat("Varying threshold, multiarm.\n")
  cutoff_Cnmulti <- deter_cutoff(alpha = alpha,
                                 ana_inter = ana_inter,
                                 n_bras = n_bras,
                                 p_n = p_n, p_a = p_a,
                                 prior = p_a,
                                 nsim_oc = nsim_oc,
                                 affich_mat = "No")
  
  cat("Varying threshold, monoarm.\n")
  cutoff_Cnmono <- deter_cutoff(alpha = alpha,
                                ana_inter = ana_inter,
                                p_n = p_n, p_a = p_a,
                                nsim_oc = nsim_oc,
                                prior = p_a,
                                affich_mat = "No")
  
  cat("Varying threshold, Holm.\n")
  cutoff_Cnholm <- deter_cutoff_holm2(alpha = alpha,
                                      ana_inter = ana_inter,
                                      n_bras = n_bras, 
                                      p_n = p_n, p_a = p_a,
                                      prior = p_a,
                                      nsim_oc = nsim_oc,
                                      affich_mat = "No",
                                      cut_seq_mono = cutoff_Cnmono[[1]][["C_"]],
                                      power_seq_mono = cutoff_Cnmono[[1]][["gamma"]])
  
  # Saving for later
  save(cutoff_Cnmono, cutoff_Cnmulti, cutoff_Cnholm,
       file = paste0("Data/simu_vsref/sensprioropt1_uncont_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
} else {
  
  # Else load the stored thresholds
  load("Data/simu_vsref/sensprioropt1_uncont_20230613.Rdata")
  
}

# Now that we have the thresholds, simulate the trials
if (simu) {
  
  cat("Operating characteristics for the 13 scenarios :\n")
  opchar_multivar <- list()
  opchar_monovar <- list()
  opchar_holm <- list()
  for (i in names(p_ttt)) {
    
    cat(paste0("Simulation n°", i, "' : ", match(i, names(p_ttt)), " / ", length(p_ttt), ".\n"))
    
    cat("Generating the data.\n")
    tableau_ttt <- gen_patients_multinom(n_sim = nsim_essais,
                                         ana_inter = ana_inter,
                                         multinom_ttt = p_ttt[[i]],
                                         seed = 1993)
    
    cat("Varying threshold, multiarm.\n")
    opchar_multivar[[i]] <- opcharac_efftox(ana_inter = ana_inter,
                                            alpha = alpha,
                                            p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                            prior = p_a,
                                            nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                            tableau_essais = tableau_ttt,
                                            cut_seq = cutoff_Cnmulti[[1]]["C_"], power_seq = cutoff_Cnmulti[[1]]["gamma"])
    
    cat("Varying threshold, monoarm.\n")
    opchar_monovar[[i]] <- list(ttt1 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt1, p_n = p_n, p_a = p_a,
                                                       prior = p_a,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt1"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt2 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt2, p_n = p_n, p_a = p_a,
                                                       prior = p_a,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt2"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt3 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt3, p_n = p_n, p_a = p_a,
                                                       prior = p_a,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt3"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]))
    
    cat("Varying threshold, Holm threshold.\n")
    opchar_holm[[i]] <- opcharac_efftox_holm2(ana_inter = ana_inter,
                                              alpha = alpha,
                                              p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                              prior = p_a,
                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                              tableau_essais = tableau_ttt,
                                              cut_seq_mono = cutoff_Cnholm[[1]][["C_mono"]], power_seq_mono = cutoff_Cnholm[[1]][["gamma_mono"]],
                                              cut_seq_holm = cutoff_Cnholm[[1]][["C_holm"]], power_seq_holm = cutoff_Cnholm[[1]][["gamma_holm"]])
    
  }
  
  # Saving raw data
  save(opchar_multivar, opchar_monovar, opchar_holm,
       file = paste0("Data/simu_vsref/sensprioropt1_bruts_13scenar_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
  if (clear) rm(tableau_ttt)
  
} else {
  
  # Sorry, raw results aren't included
  load("Data/simu_vsref/sensprioropt1_bruts_13scenar_20230613.Rdata")
  
}


## B/ Results ----

# Regroupe data
tab_Cnmultiarm <- lapply(seq_along(opchar_multivar), function(x) {cbind(scenar = names(opchar_multivar)[x], opchar_multivar[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "C<sub>n</sub><sup>m</sup>")
tab_Cnmonoarm <- lapply(seq_along(opchar_monovar), function(x) {cbind(scenar = names(opchar_monovar)[x], do.call("rbind", lapply(seq_along(opchar_monovar[[x]]), function(index) {
  data.frame(ttt = names(opchar_monovar[[x]][index]),
             rejet_h0 = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["rejet_h0"]),
             arret_precoce = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce"]),
             arret_precoce_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_fut"]),
             arret_precoce_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_tox"]),
             arret_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_fut"]),
             arret_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_tox"]),
             tot_pat = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_pts"]),
             tot_eff = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_eff"]),
             tot_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_tox"]))
})))}) %>% 
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "C<sub>n</sub><sup>s</sup>",
         arret = 1 - rejet_h0)
tab_Cnholm <- lapply(seq_along(opchar_holm), function(x) {cbind(scenar = names(opchar_holm)[x], opchar_holm[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "C<sub>n</sub><sup>m,a</sup>")
data_ggplot_13vsref <- rbind(tab_Cnmultiarm, tab_Cnmonoarm, tab_Cnholm)
rm(tab_Cnmultiarm, tab_Cnmonoarm, tab_Cnholm)
tab_Ralpha <- data.frame(
  methode = c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>s</sup>", "C<sub>n</sub><sup>m,a</sup>"),
  global_alpha = c(as.numeric(opchar_multivar[["Global H0"]][["carac_globales"]]["rejet_glob"]),
                   lapply(opchar_monovar[["Global H0"]], "[[", "essais") %>% 
                     do.call(what = "rbind", args = .) %>% 
                     group_by(n_simu) %>% 
                     summarise(rejet_glob = sum(decision == "Accept the treatment") > 0) %>% 
                     pull(rejet_glob) %>% mean(),
                   as.numeric(opchar_holm[["Global H0"]][["carac_globales"]]["rejet_glob"]))
)

data_ggplot_13vsref <- data_ggplot_13vsref %>%
  mutate(scenar = if_else(scenar == "1 works, 1 not, 1 intermediate", "1 H0, 1 H1, 1 intermediate", scenar)) %>% 
  mutate(rejet_h0_mcse = sqrt(rejet_h0 * (1 - rejet_h0) / 10000),
         scenar = factor(scenar, levels = c("Global H0", "LFC", "2 H1, 1H0", "3 H1", "3 better than H1",
                                            "3 not effective", "3 not effective with other endpoint", "1 works better than H1", 
                                            "3 work and 1 better than H1", "2 work and 1 toxic", "2 work and 1 not effective", 
                                            "1 works, 2 intermediate", "1 H0, 1 H1, 1 intermediate"),
                         labels = c("Global H0", "LFC", "2 H1, 1H0", "3 H1", "3 better than H1",
                                    "3 negative treatments", "3 negative treatments (2)", "1 works better than H1", 
                                    "3 work and 1 better than H1", "2 work and 1 toxic", "2 work and 1 not effective", 
                                    "1 works, 2 intermediate", "1 H0, 1 H1, 1 intermediate")),
         methode = factor(methode, 
                          levels = c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>s</sup>", "C<sub>n</sub><sup>m,a</sup>")))
data_ggplot_13vsref <- table_scenar_aggreg_ref %>% 
  right_join(data_ggplot_13vsref, by = c("scenar", "name" = "ttt")) %>% 
  mutate(num_scenar = factor(num_scenar, levels = c(13:1)))
plot1 <- ggplot(data_ggplot_13vsref) +
  geom_col(aes(x = num_scenar, y = rejet_h0, fill = etiquettes), color = "black",
           position = position_dodge2(width = .45), show.legend = FALSE) +
  geom_vline(xintercept = c(1:12 + .5), color = "#b1b0b1", linetype = "dashed") +
  geom_richtext(data = tab_Ralpha %>%
                  mutate(methode = factor(methode, 
                                          levels = c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>s</sup>", "C<sub>n</sub><sup>m,a</sup>"))), 
                aes(x = 13, y = .35, 
                    label = paste0("Estimated FWER: ", global_alpha * 100, "%")), 
                hjust = 0, label.color = NA) +
  facet_wrap(vars(methode), ncol = 2) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "darkgrey", size = .7),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_continuous(breaks = seq(0, 1, .2), labels = percent_format(), expand = c(0, 0), limits = c(0, .9)) +
  scale_fill_manual(values = c("#FFFFFF", "#113C51", "#B4CB65")) +
  coord_flip() +
  labs(y = "Proportion of conclusion to efficacy and no toxicity",
       x = NULL,
       fill = "Treatment arms")
plot2 <- ggplot(data_ggplot_13vsref %>% 
                  filter(methode %in% c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>s</sup>")) %>% 
                  mutate(etiquettes = factor(str_remove(etiquettes, "^Arm "), levels = c("C", "B", "A")),
                         label_scenar = if_else(num_scenar %in% c("10","11", "12", "13"), paste0("Scenario ", num_scenar, ": Arm"), paste0("Scenario   ", num_scenar, ": Arm")))) +
  geom_text(aes(x = num_scenar, y = 1.1, label = label_scenar), hjust = 0) +
  map(0:12, ~ annotate(geom = "segment", x = 0.7 + .x, xend = 1.3 + .x, y = 1.29, yend = 1.29)) +
  geom_richtext(aes(x = num_scenar, y = 1.3, label = etiquettes, color = etiquettes), hjust = 0, position = position_dodge2(width = .9), fill = NA, label.color = NA) +
  facet_wrap(vars(methode), ncol = 1) +
  scale_color_manual(values = c("black", "black", "black")) +
  coord_flip(clip = "off") +
  ylim(c(1, 1.35)) +
  labs(x = NULL,
       y = " ") +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_textbox(
          size = 12, 
          color = "transparent", fill = "transparent", box.color = "transparent",
          halign = 0.5, linetype = 1, r = unit(3, "pt"), width = unit(0.75, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)))
plot_tot <- plot2 + plot1 + plot_layout(ncol = 2, widths = c(1, 5))
ggsave(plot_tot, filename = "Outputs/sensibilite/prioropt1vsref.png", device = "png", height = 10, width = 15)

data_stackedbar <- data_ggplot_13vsref %>% 
  mutate(arret_final = arret - arret_precoce,
         nom_axe = paste0(num_scenar, ":", name),
         nom_axe = factor(nom_axe, levels = paste0(rep(13:1, each = 3), ":", rep(c("ttt3", "ttt2", "ttt1"), 13)), labels = paste0(rep(13:1, each = 3), ":", rep(c("Arm C", "Arm B", "Arm A"), 13)))) %>% 
  pivot_longer(cols = c(arret_precoce, arret_final), names_to = "type_arret", values_to = "pourcent") 
liste_plots <- map(as.character(unique(data_stackedbar$methode)),
                   function(x) {
                     ggplot(data_stackedbar %>% filter(methode == x)) +
                       geom_col(aes(x = nom_axe, y = pourcent, fill = type_arret), 
                                color = "#929494", position = "stack", width = 0.8) +
                       geom_vline(xintercept = c((1:12) * 3 + .5), color = "#666c6c", linetype = "dashed", size = 1) +
                       facet_wrap(vars(methode)) +
                       theme(panel.grid.major.y = element_blank(),
                             panel.grid.major.x = element_line(color = "darkgrey", size = .7),
                             axis.ticks.y = element_blank(),
                             axis.text.y = element_blank(),
                             panel.grid.minor.y = element_blank(),
                             legend.position = "right") +
                       scale_y_continuous(breaks = seq(0, .9, .2), labels = percent_format(), expand = c(0, 0), limits = c(0, 1)) +
                       scale_fill_manual(name = NULL, values = c("#b2df8a", "#a6cee3"), labels = c("Stopping at final analysis", "Stopping at interim analysis")) +
                       coord_flip() +
                       labs(y = "Stopping of the trial",
                            x = NULL)
                   })
legende <- get_legend(liste_plots[[1]])
plot_inter <- wrap_plots(map(liste_plots, ~ .x + theme(legend.position = "none")))
plot2 <- ggplot(data = data_stackedbar %>% 
                  filter(methode == "C<sub>n</sub><sup>m</sup>") %>%
                  mutate(etiqu = if_else(etiquettes == "Arm B", paste0("Scénario ", num_scenar, ":"), NA_character_))) +
  geom_text(aes(x = nom_axe, y = 1, label = etiqu), hjust = 0) +
  geom_text(aes(x = nom_axe, y = 1.15, label = etiquettes), hjust = 0) +
  geom_richtext(aes(x = nom_axe, y = 1.25, label = value), hjust = 0, fill = NA, label.color = NA) +
  annotate(geom = "segment", x = c((1:12) * 3 + .5), xend = c((1:12) * 3 + .5), y = 1, yend = 1.5,
           color = "#666c6c", linetype = "dashed", size = 1) +
  geom_text(x = 40, y = 1.275, aes(label = "(P(Eff), P(Tox))"), color = "black", hjust = 0, fontface = "bold") +
  facet_wrap(vars(methode), ncol = 1) +
  scale_color_manual(values = c("black", "black", "black")) +
  coord_flip(clip = "off") +
  ylim(c(.9, 1.6)) +
  labs(x = " ",
       y = " ") +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(colour = "transparent"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "transparent"),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_textbox(
          size = 12, 
          color = "transparent", fill = "transparent", box.color = "transparent",
          halign = 0.5, linetype = 1, r = unit(3, "pt"), width = unit(0.75, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)))
plot_cn <- (plot2 | wrap_plots(map(map(1:3, ~ liste_plots[[.x]]), ~ .x + theme(legend.position = "none")))) + plot_layout(widths = c(1.5, 3))
plot_tot <- (plot_cn | legende) + plot_layout(widths = c(2, 4, 1))
ggsave(plot_tot, filename = "Outputs/sensibilite/prioropt1vsref_ap.png", device = "png", height = 10, width = 15)


# V/ Prior uniforme (ESS = 5) ----

alpha <- .1
n_bras <- 3
p_n <- c(.15, .3, .15, .4)
p_a <- c(.18, .42, .02, .38)
nsim_oc <- 10000
nsim_essais <- 10000
ana_inter <- rep(15, 4)
methode <- 2L
p_ttt <- list("Global H0" = list(ttt1 = p_n, ttt2 = p_n, ttt3 = p_n),
              "LFC" = list(ttt1 = p_a, ttt2 = p_n, ttt3 = p_n),
              "2 H1, 1H0" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = p_n),
              "3 H1" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = p_a),
              "3 better than H1" = list(ttt1 = c(.15, .5, .05, .3), ttt2 = c(.15, .5, .05, .3), ttt3 = c(.15, .5, .05, .3)),
              "3 not effective" = list(ttt1 = c(.2, .4, .1, .3), ttt2 = c(.1, .35, .1, .45), ttt3 = p_n),
              "3 not effective with other endpoint" = list(ttt1 = c(.2, .5, .2, .1), ttt2 = c(.05, .35, .05, .55), ttt3 = p_n),
              "1 works better than H1" = list(ttt1 = c(.15, .5, .05, .3), ttt2 = p_n, ttt3 = p_n),
              "3 work and 1 better than H1" = list(ttt1 = c(.15, .5, .05, .3), ttt2 = p_a, ttt3 = p_a),
              "2 work and 1 toxic" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = c(.2, .5, .15, .15)),
              "2 work and 1 not effective" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = c(.1, .35, .1, .45)),
              "1 works, 2 intermediate" = list(ttt1 = p_a, ttt2 = c(.15, .35, .1, .4), ttt3 = c(.15, .35, .1, .4)),
              "1 works, 1 not, 1 intermediate" = list(ttt1 = p_n, ttt2 = p_a, ttt3 = c(.15, .35, .1, .4)))


## A/ Simulations ----

# Optimize the thresholds or retrieve them from storage
if (simu_seuils) {
  
  cat("Determine the thresholds hyperparameters.\n")
  
  cat("Varying threshold, multiarm.\n")
  cutoff_Cnmulti <- deter_cutoff(alpha = alpha,
                                 ana_inter = ana_inter,
                                 n_bras = n_bras,
                                 p_n = p_n, p_a = p_a,
                                 prior = c(.25, .25, .25, .25) * 5,
                                 nsim_oc = nsim_oc,
                                 affich_mat = "No")
  
  cat("Varying threshold, monoarm.\n")
  cutoff_Cnmono <- deter_cutoff(alpha = alpha,
                                ana_inter = ana_inter,
                                p_n = p_n, p_a = p_a,
                                nsim_oc = nsim_oc,
                                prior = c(.25, .25, .25, .25) * 5,
                                affich_mat = "No")
  
  cat("Varying threshold, Holm.\n")
  cutoff_Cnholm <- deter_cutoff_holm2(alpha = alpha,
                                      ana_inter = ana_inter,
                                      n_bras = n_bras, 
                                      p_n = p_n, p_a = p_a,
                                      prior = c(.25, .25, .25, .25) * 5,
                                      nsim_oc = nsim_oc,
                                      affich_mat = "No",
                                      cut_seq_mono = cutoff_Cnmono[[1]][["C_"]],
                                      power_seq_mono = cutoff_Cnmono[[1]][["gamma"]])
  
  # Saving for later
  save(cutoff_Cnmono, cutoff_Cnmulti, cutoff_Cnholm,
       file = paste0("Data/simu_vsref/senspriorneut5_uncont_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
} else {
  
  # Else load the stored thresholds
  load("Data/simu_vsref/senspriorneut5_uncont_20230613.Rdata")
  
}

# Now that we have the thresholds, simulate the trials
if (simu) {
  
  cat("Operating characteristics for the 13 scenarios :\n")
  opchar_multivar <- list()
  opchar_monovar <- list()
  opchar_holm <- list()
  for (i in names(p_ttt)) {
    
    cat(paste0("Simulation n°", i, "' : ", match(i, names(p_ttt)), " / ", length(p_ttt), ".\n"))
    
    cat("Generating the data.\n")
    tableau_ttt <- gen_patients_multinom(n_sim = nsim_essais,
                                         ana_inter = ana_inter,
                                         multinom_ttt = p_ttt[[i]],
                                         seed = 1993)
    
    cat("Varying threshold, multiarm.\n")
    opchar_multivar[[i]] <- opcharac_efftox(ana_inter = ana_inter,
                                            alpha = alpha,
                                            p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                            prior = c(.25, .25, .25, .25) * 5,
                                            nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                            tableau_essais = tableau_ttt,
                                            cut_seq = cutoff_Cnmulti[[1]]["C_"], power_seq = cutoff_Cnmulti[[1]]["gamma"])
    
    cat("Varying threshold, monoarm.\n")
    opchar_monovar[[i]] <- list(ttt1 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt1, p_n = p_n, p_a = p_a,
                                                       prior = c(.25, .25, .25, .25) * 5,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt1"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt2 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt2, p_n = p_n, p_a = p_a,
                                                       prior = c(.25, .25, .25, .25) * 5,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt2"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt3 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt3, p_n = p_n, p_a = p_a,
                                                       prior = c(.25, .25, .25, .25) * 5,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt3"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]))
    
    cat("Varying threshold, Holm threshold.\n")
    opchar_holm[[i]] <- opcharac_efftox_holm2(ana_inter = ana_inter,
                                              alpha = alpha,
                                              p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                              prior = c(.25, .25, .25, .25) * 5,
                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                              tableau_essais = tableau_ttt,
                                              cut_seq_mono = cutoff_Cnholm[[1]][["C_mono"]], power_seq_mono = cutoff_Cnholm[[1]][["gamma_mono"]],
                                              cut_seq_holm = cutoff_Cnholm[[1]][["C_holm"]], power_seq_holm = cutoff_Cnholm[[1]][["gamma_holm"]])
    
  }
  
  # Saving raw data
  save(opchar_multivar, opchar_monovar, opchar_holm,
       file = paste0("Data/simu_vsref/senspriorneut5_bruts_13scenar_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
  if (clear) rm(tableau_ttt)
  
} else {
  
  # Sorry, raw results aren't included
  load("Data/simu_vsref/senspriorneut5_bruts_13scenar_20230613.Rdata")
  
}

## B/ Results ----

# Regroupe data
tab_Cnmultiarm <- lapply(seq_along(opchar_multivar), function(x) {cbind(scenar = names(opchar_multivar)[x], opchar_multivar[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "C<sub>n</sub><sup>m</sup>")
tab_Cnmonoarm <- lapply(seq_along(opchar_monovar), function(x) {cbind(scenar = names(opchar_monovar)[x], do.call("rbind", lapply(seq_along(opchar_monovar[[x]]), function(index) {
  data.frame(ttt = names(opchar_monovar[[x]][index]),
             rejet_h0 = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["rejet_h0"]),
             arret_precoce = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce"]),
             arret_precoce_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_fut"]),
             arret_precoce_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_tox"]),
             arret_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_fut"]),
             arret_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_tox"]),
             tot_pat = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_pts"]),
             tot_eff = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_eff"]),
             tot_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_tox"]))
})))}) %>% 
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "C<sub>n</sub><sup>s</sup>",
         arret = 1 - rejet_h0)
tab_Cnholm <- lapply(seq_along(opchar_holm), function(x) {cbind(scenar = names(opchar_holm)[x], opchar_holm[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "C<sub>n</sub><sup>m,a</sup>")
data_ggplot_13vsref <- rbind(tab_Cnmultiarm, tab_Cnmonoarm, tab_Cnholm)
rm(tab_Cnmultiarm, tab_Cnmonoarm, tab_Cnholm)
tab_Ralpha <- data.frame(
  methode = c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>s</sup>", "C<sub>n</sub><sup>m,a</sup>"),
  global_alpha = c(as.numeric(opchar_multivar[["Global H0"]][["carac_globales"]]["rejet_glob"]),
                   lapply(opchar_monovar[["Global H0"]], "[[", "essais") %>% 
                     do.call(what = "rbind", args = .) %>% 
                     group_by(n_simu) %>% 
                     summarise(rejet_glob = sum(decision == "Accept the treatment") > 0) %>% 
                     pull(rejet_glob) %>% mean(),
                   as.numeric(opchar_holm[["Global H0"]][["carac_globales"]]["rejet_glob"]))
)

data_ggplot_13vsref <- data_ggplot_13vsref %>%
  mutate(scenar = if_else(scenar == "1 works, 1 not, 1 intermediate", "1 H0, 1 H1, 1 intermediate", scenar)) %>% 
  mutate(rejet_h0_mcse = sqrt(rejet_h0 * (1 - rejet_h0) / 10000),
         scenar = factor(scenar, levels = c("Global H0", "LFC", "2 H1, 1H0", "3 H1", "3 better than H1",
                                            "3 not effective", "3 not effective with other endpoint", "1 works better than H1", 
                                            "3 work and 1 better than H1", "2 work and 1 toxic", "2 work and 1 not effective", 
                                            "1 works, 2 intermediate", "1 H0, 1 H1, 1 intermediate"),
                         labels = c("Global H0", "LFC", "2 H1, 1H0", "3 H1", "3 better than H1",
                                    "3 negative treatments", "3 negative treatments (2)", "1 works better than H1", 
                                    "3 work and 1 better than H1", "2 work and 1 toxic", "2 work and 1 not effective", 
                                    "1 works, 2 intermediate", "1 H0, 1 H1, 1 intermediate")),
         methode = factor(methode, 
                          levels = c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>s</sup>", "C<sub>n</sub><sup>m,a</sup>")))
data_ggplot_13vsref <- table_scenar_aggreg_ref %>% 
  right_join(data_ggplot_13vsref, by = c("scenar", "name" = "ttt")) %>% 
  mutate(num_scenar = factor(num_scenar, levels = c(13:1)))
plot1 <- ggplot(data_ggplot_13vsref) +
  geom_col(aes(x = num_scenar, y = rejet_h0, fill = etiquettes), color = "black",
           position = position_dodge2(width = .45), show.legend = FALSE) +
  geom_vline(xintercept = c(1:12 + .5), color = "#b1b0b1", linetype = "dashed") +
  geom_richtext(data = tab_Ralpha %>%
                  mutate(methode = factor(methode, 
                                          levels = c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>s</sup>", "C<sub>n</sub><sup>m,a</sup>"))), 
                aes(x = 13, y = .35, 
                    label = paste0("Estimated FWER: ", global_alpha * 100, "%")), 
                hjust = 0, label.color = NA) +
  facet_wrap(vars(methode), ncol = 2) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "darkgrey", size = .7),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_continuous(breaks = seq(0, 1, .2), labels = percent_format(), expand = c(0, 0), limits = c(0, .9)) +
  scale_fill_manual(values = c("#FFFFFF", "#113C51", "#B4CB65")) +
  coord_flip() +
  labs(y = "Proportion of conclusion to efficacy and no toxicity",
       x = NULL,
       fill = "Treatment arms")
plot2 <- ggplot(data_ggplot_13vsref %>% 
                  filter(methode %in% c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>s</sup>")) %>% 
                  mutate(etiquettes = factor(str_remove(etiquettes, "^Arm "), levels = c("C", "B", "A")),
                         label_scenar = if_else(num_scenar %in% c("10","11", "12", "13"), paste0("Scenario ", num_scenar, ": Arm"), paste0("Scenario   ", num_scenar, ": Arm")))) +
  geom_text(aes(x = num_scenar, y = 1.1, label = label_scenar), hjust = 0) +
  map(0:12, ~ annotate(geom = "segment", x = 0.7 + .x, xend = 1.3 + .x, y = 1.29, yend = 1.29)) +
  geom_richtext(aes(x = num_scenar, y = 1.3, label = etiquettes, color = etiquettes), hjust = 0, position = position_dodge2(width = .9), fill = NA, label.color = NA) +
  facet_wrap(vars(methode), ncol = 1) +
  scale_color_manual(values = c("black", "black", "black")) +
  coord_flip(clip = "off") +
  ylim(c(1, 1.35)) +
  labs(x = NULL,
       y = " ") +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_textbox(
          size = 12, 
          color = "transparent", fill = "transparent", box.color = "transparent",
          halign = 0.5, linetype = 1, r = unit(3, "pt"), width = unit(0.75, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)))
plot_tot <- plot2 + plot1 + plot_layout(ncol = 2, widths = c(1, 5))
ggsave(plot_tot, filename = "Outputs/sensibilite/priorneut5vsref.png", device = "png", height = 10, width = 15)

data_stackedbar <- data_ggplot_13vsref %>% 
  mutate(arret_final = arret - arret_precoce,
         nom_axe = paste0(num_scenar, ":", name),
         nom_axe = factor(nom_axe, levels = paste0(rep(13:1, each = 3), ":", rep(c("ttt3", "ttt2", "ttt1"), 13)), labels = paste0(rep(13:1, each = 3), ":", rep(c("Arm C", "Arm B", "Arm A"), 13)))) %>% 
  pivot_longer(cols = c(arret_precoce, arret_final), names_to = "type_arret", values_to = "pourcent") 
liste_plots <- map(as.character(unique(data_stackedbar$methode)),
                   function(x) {
                     ggplot(data_stackedbar %>% filter(methode == x)) +
                       geom_col(aes(x = nom_axe, y = pourcent, fill = type_arret), 
                                color = "#929494", position = "stack", width = 0.8) +
                       geom_vline(xintercept = c((1:12) * 3 + .5), color = "#666c6c", linetype = "dashed", size = 1) +
                       facet_wrap(vars(methode)) +
                       theme(panel.grid.major.y = element_blank(),
                             panel.grid.major.x = element_line(color = "darkgrey", size = .7),
                             axis.ticks.y = element_blank(),
                             axis.text.y = element_blank(),
                             panel.grid.minor.y = element_blank(),
                             legend.position = "right") +
                       scale_y_continuous(breaks = seq(0, .9, .2), labels = percent_format(), expand = c(0, 0), limits = c(0, 1)) +
                       scale_fill_manual(name = NULL, values = c("#b2df8a", "#a6cee3"), labels = c("Stopping at final analysis", "Stopping at interim analysis")) +
                       coord_flip() +
                       labs(y = "Stopping of the trial",
                            x = NULL)
                   })
legende <- get_legend(liste_plots[[1]])
plot_inter <- wrap_plots(map(liste_plots, ~ .x + theme(legend.position = "none")))
plot2 <- ggplot(data = data_stackedbar %>% 
                  filter(methode == "C<sub>n</sub><sup>m</sup>") %>%
                  mutate(etiqu = if_else(etiquettes == "Arm B", paste0("Scénario ", num_scenar, ":"), NA_character_))) +
  geom_text(aes(x = nom_axe, y = 1, label = etiqu), hjust = 0) +
  geom_text(aes(x = nom_axe, y = 1.15, label = etiquettes), hjust = 0) +
  geom_richtext(aes(x = nom_axe, y = 1.25, label = value), hjust = 0, fill = NA, label.color = NA) +
  annotate(geom = "segment", x = c((1:12) * 3 + .5), xend = c((1:12) * 3 + .5), y = 1, yend = 1.5,
           color = "#666c6c", linetype = "dashed", size = 1) +
  geom_text(x = 40, y = 1.275, aes(label = "(P(Eff), P(Tox))"), color = "black", hjust = 0, fontface = "bold") +
  facet_wrap(vars(methode), ncol = 1) +
  scale_color_manual(values = c("black", "black", "black")) +
  coord_flip(clip = "off") +
  ylim(c(.9, 1.6)) +
  labs(x = " ",
       y = " ") +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(colour = "transparent"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "transparent"),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_textbox(
          size = 12, 
          color = "transparent", fill = "transparent", box.color = "transparent",
          halign = 0.5, linetype = 1, r = unit(3, "pt"), width = unit(0.75, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)))
plot_cn <- (plot2 | wrap_plots(map(map(1:3, ~ liste_plots[[.x]]), ~ .x + theme(legend.position = "none")))) + plot_layout(widths = c(1.5, 3))
plot_tot <- (plot_cn | legende) + plot_layout(widths = c(2, 4, 1))
ggsave(plot_tot, filename = "Outputs/sensibilite/priorneut5vsref_ap.png", device = "png", height = 10, width = 15)



# VI/ Prior pessimiste (ESS = 5) ----

alpha <- .1
n_bras <- 3
p_n <- c(.15, .3, .15, .4)
p_a <- c(.18, .42, .02, .38)
nsim_oc <- 10000
nsim_essais <- 10000
ana_inter <- rep(15, 4)
methode <- 2L
p_ttt <- list("Global H0" = list(ttt1 = p_n, ttt2 = p_n, ttt3 = p_n),
              "LFC" = list(ttt1 = p_a, ttt2 = p_n, ttt3 = p_n),
              "2 H1, 1H0" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = p_n),
              "3 H1" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = p_a),
              "3 better than H1" = list(ttt1 = c(.15, .5, .05, .3), ttt2 = c(.15, .5, .05, .3), ttt3 = c(.15, .5, .05, .3)),
              "3 not effective" = list(ttt1 = c(.2, .4, .1, .3), ttt2 = c(.1, .35, .1, .45), ttt3 = p_n),
              "3 not effective with other endpoint" = list(ttt1 = c(.2, .5, .2, .1), ttt2 = c(.05, .35, .05, .55), ttt3 = p_n),
              "1 works better than H1" = list(ttt1 = c(.15, .5, .05, .3), ttt2 = p_n, ttt3 = p_n),
              "3 work and 1 better than H1" = list(ttt1 = c(.15, .5, .05, .3), ttt2 = p_a, ttt3 = p_a),
              "2 work and 1 toxic" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = c(.2, .5, .15, .15)),
              "2 work and 1 not effective" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = c(.1, .35, .1, .45)),
              "1 works, 2 intermediate" = list(ttt1 = p_a, ttt2 = c(.15, .35, .1, .4), ttt3 = c(.15, .35, .1, .4)),
              "1 works, 1 not, 1 intermediate" = list(ttt1 = p_n, ttt2 = p_a, ttt3 = c(.15, .35, .1, .4)))


## A/ Simulations ----

# Optimize the thresholds or retrieve them from storage
if (simu_seuils) {
  
  cat("Determine the thresholds hyperparameters.\n")
  
  cat("Varying threshold, multiarm.\n")
  cutoff_Cnmulti <- deter_cutoff(alpha = alpha,
                                 ana_inter = ana_inter,
                                 n_bras = n_bras,
                                 p_n = p_n, p_a = p_a,
                                 prior = p_n * 5,
                                 nsim_oc = nsim_oc,
                                 affich_mat = "No")
  
  cat("Varying threshold, monoarm.\n")
  cutoff_Cnmono <- deter_cutoff(alpha = alpha,
                                ana_inter = ana_inter,
                                p_n = p_n, p_a = p_a,
                                nsim_oc = nsim_oc,
                                prior = p_n * 5,
                                affich_mat = "No")
  
  cat("Varying threshold, Holm.\n")
  cutoff_Cnholm <- deter_cutoff_holm2(alpha = alpha,
                                      ana_inter = ana_inter,
                                      n_bras = n_bras, 
                                      p_n = p_n, p_a = p_a,
                                      prior = p_n * 5,
                                      nsim_oc = nsim_oc,
                                      affich_mat = "No",
                                      cut_seq_mono = cutoff_Cnmono[[1]][["C_"]],
                                      power_seq_mono = cutoff_Cnmono[[1]][["gamma"]])
  
  # Saving for later
  save(cutoff_Cnmono, cutoff_Cnmulti, cutoff_Cnholm,
       file = paste0("Data/simu_vsref/senspriorpess5_uncont_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
} else {
  
  # Else load the stored thresholds
  load("Data/simu_vsref/sens30_uncont_20230609.Rdata")
  
}

# Now that we have the thresholds, simulate the trials
if (simu) {
  
  cat("Operating characteristics for the 13 scenarios :\n")
  opchar_multivar <- list()
  opchar_monovar <- list()
  opchar_holm <- list()
  for (i in names(p_ttt)) {
    
    cat(paste0("Simulation n°", i, "' : ", match(i, names(p_ttt)), " / ", length(p_ttt), ".\n"))
    
    cat("Generating the data.\n")
    tableau_ttt <- gen_patients_multinom(n_sim = nsim_essais,
                                         ana_inter = ana_inter,
                                         multinom_ttt = p_ttt[[i]],
                                         seed = 1993)
    
    cat("Varying threshold, multiarm.\n")
    opchar_multivar[[i]] <- opcharac_efftox(ana_inter = ana_inter,
                                            alpha = alpha,
                                            p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                            prior = p_n * 5,
                                            nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                            tableau_essais = tableau_ttt,
                                            cut_seq = cutoff_Cnmulti[[1]]["C_"], power_seq = cutoff_Cnmulti[[1]]["gamma"])
    
    cat("Varying threshold, monoarm.\n")
    opchar_monovar[[i]] <- list(ttt1 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt1, p_n = p_n, p_a = p_a,
                                                       prior = p_n * 5,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt1"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt2 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt2, p_n = p_n, p_a = p_a,
                                                       prior = p_n * 5,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt2"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt3 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt3, p_n = p_n, p_a = p_a,
                                                       prior = p_n * 5,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt3"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]))
    
    cat("Varying threshold, Holm threshold.\n")
    opchar_holm[[i]] <- opcharac_efftox_holm2(ana_inter = ana_inter,
                                              alpha = alpha,
                                              p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                              prior = p_n * 5,
                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                              tableau_essais = tableau_ttt,
                                              cut_seq_mono = cutoff_Cnholm[[1]][["C_mono"]], power_seq_mono = cutoff_Cnholm[[1]][["gamma_mono"]],
                                              cut_seq_holm = cutoff_Cnholm[[1]][["C_holm"]], power_seq_holm = cutoff_Cnholm[[1]][["gamma_holm"]])
    
  }
  
  # Saving raw data
  save(opchar_multivar, opchar_monovar, opchar_holm,
       file = paste0("Data/simu_vsref/senspriorpess5_bruts_13scenar_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
  if (clear) rm(tableau_ttt)
  
} else {
  
  # Sorry, raw results aren't included
  load("Data/simu_vsref/senspriorpess5_bruts_13scenar_20230613.Rdata")
  
}

## B/ Results ----

# Regroupe data
tab_Cnmultiarm <- lapply(seq_along(opchar_multivar), function(x) {cbind(scenar = names(opchar_multivar)[x], opchar_multivar[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "C<sub>n</sub><sup>m</sup>")
tab_Cnmonoarm <- lapply(seq_along(opchar_monovar), function(x) {cbind(scenar = names(opchar_monovar)[x], do.call("rbind", lapply(seq_along(opchar_monovar[[x]]), function(index) {
  data.frame(ttt = names(opchar_monovar[[x]][index]),
             rejet_h0 = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["rejet_h0"]),
             arret_precoce = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce"]),
             arret_precoce_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_fut"]),
             arret_precoce_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_tox"]),
             arret_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_fut"]),
             arret_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_tox"]),
             tot_pat = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_pts"]),
             tot_eff = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_eff"]),
             tot_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_tox"]))
})))}) %>% 
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "C<sub>n</sub><sup>s</sup>",
         arret = 1 - rejet_h0)
tab_Cnholm <- lapply(seq_along(opchar_holm), function(x) {cbind(scenar = names(opchar_holm)[x], opchar_holm[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "C<sub>n</sub><sup>m,a</sup>")
data_ggplot_13vsref <- rbind(tab_Cnmultiarm, tab_Cnmonoarm, tab_Cnholm)
rm(tab_Cnmultiarm, tab_Cnmonoarm, tab_Cnholm)
tab_Ralpha <- data.frame(
  methode = c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>s</sup>", "C<sub>n</sub><sup>m,a</sup>"),
  global_alpha = c(as.numeric(opchar_multivar[["Global H0"]][["carac_globales"]]["rejet_glob"]),
                   lapply(opchar_monovar[["Global H0"]], "[[", "essais") %>% 
                     do.call(what = "rbind", args = .) %>% 
                     group_by(n_simu) %>% 
                     summarise(rejet_glob = sum(decision == "Accept the treatment") > 0) %>% 
                     pull(rejet_glob) %>% mean(),
                   as.numeric(opchar_holm[["Global H0"]][["carac_globales"]]["rejet_glob"]))
)

data_ggplot_13vsref <- data_ggplot_13vsref %>%
  mutate(scenar = if_else(scenar == "1 works, 1 not, 1 intermediate", "1 H0, 1 H1, 1 intermediate", scenar)) %>% 
  mutate(rejet_h0_mcse = sqrt(rejet_h0 * (1 - rejet_h0) / 10000),
         scenar = factor(scenar, levels = c("Global H0", "LFC", "2 H1, 1H0", "3 H1", "3 better than H1",
                                            "3 not effective", "3 not effective with other endpoint", "1 works better than H1", 
                                            "3 work and 1 better than H1", "2 work and 1 toxic", "2 work and 1 not effective", 
                                            "1 works, 2 intermediate", "1 H0, 1 H1, 1 intermediate"),
                         labels = c("Global H0", "LFC", "2 H1, 1H0", "3 H1", "3 better than H1",
                                    "3 negative treatments", "3 negative treatments (2)", "1 works better than H1", 
                                    "3 work and 1 better than H1", "2 work and 1 toxic", "2 work and 1 not effective", 
                                    "1 works, 2 intermediate", "1 H0, 1 H1, 1 intermediate")),
         methode = factor(methode, 
                          levels = c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>s</sup>", "C<sub>n</sub><sup>m,a</sup>")))
data_ggplot_13vsref <- table_scenar_aggreg_ref %>% 
  right_join(data_ggplot_13vsref, by = c("scenar", "name" = "ttt")) %>% 
  mutate(num_scenar = factor(num_scenar, levels = c(13:1)))
plot1 <- ggplot(data_ggplot_13vsref) +
  geom_col(aes(x = num_scenar, y = rejet_h0, fill = etiquettes), color = "black",
           position = position_dodge2(width = .45), show.legend = FALSE) +
  geom_vline(xintercept = c(1:12 + .5), color = "#b1b0b1", linetype = "dashed") +
  geom_richtext(data = tab_Ralpha %>%
                  mutate(methode = factor(methode, 
                                          levels = c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>s</sup>", "C<sub>n</sub><sup>m,a</sup>"))), 
                aes(x = 13, y = .35, 
                    label = paste0("Estimated FWER: ", global_alpha * 100, "%")), 
                hjust = 0, label.color = NA) +
  facet_wrap(vars(methode), ncol = 2) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "darkgrey", size = .7),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_continuous(breaks = seq(0, 1, .2), labels = percent_format(), expand = c(0, 0), limits = c(0, .9)) +
  scale_fill_manual(values = c("#FFFFFF", "#113C51", "#B4CB65")) +
  coord_flip() +
  labs(y = "Proportion of conclusion to efficacy and no toxicity",
       x = NULL,
       fill = "Treatment arms")
plot2 <- ggplot(data_ggplot_13vsref %>% 
                  filter(methode %in% c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>s</sup>")) %>% 
                  mutate(etiquettes = factor(str_remove(etiquettes, "^Arm "), levels = c("C", "B", "A")),
                         label_scenar = if_else(num_scenar %in% c("10","11", "12", "13"), paste0("Scenario ", num_scenar, ": Arm"), paste0("Scenario   ", num_scenar, ": Arm")))) +
  geom_text(aes(x = num_scenar, y = 1.1, label = label_scenar), hjust = 0) +
  map(0:12, ~ annotate(geom = "segment", x = 0.7 + .x, xend = 1.3 + .x, y = 1.29, yend = 1.29)) +
  geom_richtext(aes(x = num_scenar, y = 1.3, label = etiquettes, color = etiquettes), hjust = 0, position = position_dodge2(width = .9), fill = NA, label.color = NA) +
  facet_wrap(vars(methode), ncol = 1) +
  scale_color_manual(values = c("black", "black", "black")) +
  coord_flip(clip = "off") +
  ylim(c(1, 1.35)) +
  labs(x = NULL,
       y = " ") +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_textbox(
          size = 12, 
          color = "transparent", fill = "transparent", box.color = "transparent",
          halign = 0.5, linetype = 1, r = unit(3, "pt"), width = unit(0.75, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)))
plot_tot <- plot2 + plot1 + plot_layout(ncol = 2, widths = c(1, 5))
ggsave(plot_tot, filename = "Outputs/sensibilite/priorpess5vsref.png", device = "png", height = 10, width = 15)

data_stackedbar <- data_ggplot_13vsref %>% 
  mutate(arret_final = arret - arret_precoce,
         nom_axe = paste0(num_scenar, ":", name),
         nom_axe = factor(nom_axe, levels = paste0(rep(13:1, each = 3), ":", rep(c("ttt3", "ttt2", "ttt1"), 13)), labels = paste0(rep(13:1, each = 3), ":", rep(c("Arm C", "Arm B", "Arm A"), 13)))) %>% 
  pivot_longer(cols = c(arret_precoce, arret_final), names_to = "type_arret", values_to = "pourcent") 
liste_plots <- map(as.character(unique(data_stackedbar$methode)),
                   function(x) {
                     ggplot(data_stackedbar %>% filter(methode == x)) +
                       geom_col(aes(x = nom_axe, y = pourcent, fill = type_arret), 
                                color = "#929494", position = "stack", width = 0.8) +
                       geom_vline(xintercept = c((1:12) * 3 + .5), color = "#666c6c", linetype = "dashed", size = 1) +
                       facet_wrap(vars(methode)) +
                       theme(panel.grid.major.y = element_blank(),
                             panel.grid.major.x = element_line(color = "darkgrey", size = .7),
                             axis.ticks.y = element_blank(),
                             axis.text.y = element_blank(),
                             panel.grid.minor.y = element_blank(),
                             legend.position = "right") +
                       scale_y_continuous(breaks = seq(0, .9, .2), labels = percent_format(), expand = c(0, 0), limits = c(0, 1)) +
                       scale_fill_manual(name = NULL, values = c("#b2df8a", "#a6cee3"), labels = c("Stopping at final analysis", "Stopping at interim analysis")) +
                       coord_flip() +
                       labs(y = "Stopping of the trial",
                            x = NULL)
                   })
legende <- get_legend(liste_plots[[1]])
plot_inter <- wrap_plots(map(liste_plots, ~ .x + theme(legend.position = "none")))
plot2 <- ggplot(data = data_stackedbar %>% 
                  filter(methode == "C<sub>n</sub><sup>m</sup>") %>%
                  mutate(etiqu = if_else(etiquettes == "Arm B", paste0("Scénario ", num_scenar, ":"), NA_character_))) +
  geom_text(aes(x = nom_axe, y = 1, label = etiqu), hjust = 0) +
  geom_text(aes(x = nom_axe, y = 1.15, label = etiquettes), hjust = 0) +
  geom_richtext(aes(x = nom_axe, y = 1.25, label = value), hjust = 0, fill = NA, label.color = NA) +
  annotate(geom = "segment", x = c((1:12) * 3 + .5), xend = c((1:12) * 3 + .5), y = 1, yend = 1.5,
           color = "#666c6c", linetype = "dashed", size = 1) +
  geom_text(x = 40, y = 1.275, aes(label = "(P(Eff), P(Tox))"), color = "black", hjust = 0, fontface = "bold") +
  facet_wrap(vars(methode), ncol = 1) +
  scale_color_manual(values = c("black", "black", "black")) +
  coord_flip(clip = "off") +
  ylim(c(.9, 1.6)) +
  labs(x = " ",
       y = " ") +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(colour = "transparent"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "transparent"),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_textbox(
          size = 12, 
          color = "transparent", fill = "transparent", box.color = "transparent",
          halign = 0.5, linetype = 1, r = unit(3, "pt"), width = unit(0.75, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)))
plot_cn <- (plot2 | wrap_plots(map(map(1:3, ~ liste_plots[[.x]]), ~ .x + theme(legend.position = "none")))) + plot_layout(widths = c(1.5, 3))
plot_tot <- (plot_cn | legende) + plot_layout(widths = c(2, 4, 1))
ggsave(plot_tot, filename = "Outputs/sensibilite/priorpess5vsref_ap.png", device = "png", height = 10, width = 15)


# VII/ Prior optimiste (ESS = 5) ----

alpha <- .1
n_bras <- 3
p_n <- c(.15, .3, .15, .4)
p_a <- c(.18, .42, .02, .38)
nsim_oc <- 10000
nsim_essais <- 10000
ana_inter <- rep(15, 4)
methode <- 2L
p_ttt <- list("Global H0" = list(ttt1 = p_n, ttt2 = p_n, ttt3 = p_n),
              "LFC" = list(ttt1 = p_a, ttt2 = p_n, ttt3 = p_n),
              "2 H1, 1H0" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = p_n),
              "3 H1" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = p_a),
              "3 better than H1" = list(ttt1 = c(.15, .5, .05, .3), ttt2 = c(.15, .5, .05, .3), ttt3 = c(.15, .5, .05, .3)),
              "3 not effective" = list(ttt1 = c(.2, .4, .1, .3), ttt2 = c(.1, .35, .1, .45), ttt3 = p_n),
              "3 not effective with other endpoint" = list(ttt1 = c(.2, .5, .2, .1), ttt2 = c(.05, .35, .05, .55), ttt3 = p_n),
              "1 works better than H1" = list(ttt1 = c(.15, .5, .05, .3), ttt2 = p_n, ttt3 = p_n),
              "3 work and 1 better than H1" = list(ttt1 = c(.15, .5, .05, .3), ttt2 = p_a, ttt3 = p_a),
              "2 work and 1 toxic" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = c(.2, .5, .15, .15)),
              "2 work and 1 not effective" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = c(.1, .35, .1, .45)),
              "1 works, 2 intermediate" = list(ttt1 = p_a, ttt2 = c(.15, .35, .1, .4), ttt3 = c(.15, .35, .1, .4)),
              "1 works, 1 not, 1 intermediate" = list(ttt1 = p_n, ttt2 = p_a, ttt3 = c(.15, .35, .1, .4)))


## A/ Simulations ----

# Optimize the thresholds or retrieve them from storage
if (simu_seuils) {
  
  cat("Determine the thresholds hyperparameters.\n")
  
  cat("Varying threshold, multiarm.\n")
  cutoff_Cnmulti <- deter_cutoff(alpha = alpha,
                                 ana_inter = ana_inter,
                                 n_bras = n_bras,
                                 p_n = p_n, p_a = p_a,
                                 prior = p_a * 5,
                                 nsim_oc = nsim_oc,
                                 affich_mat = "No")
  
  cat("Varying threshold, monoarm.\n")
  cutoff_Cnmono <- deter_cutoff(alpha = alpha,
                                ana_inter = ana_inter,
                                p_n = p_n, p_a = p_a,
                                nsim_oc = nsim_oc,
                                prior = p_a * 5,
                                affich_mat = "No")
  
  cat("Varying threshold, Holm.\n")
  cutoff_Cnholm <- deter_cutoff_holm2(alpha = alpha,
                                      ana_inter = ana_inter,
                                      n_bras = n_bras, 
                                      p_n = p_n, p_a = p_a,
                                      prior = p_a * 5,
                                      nsim_oc = nsim_oc,
                                      affich_mat = "No",
                                      cut_seq_mono = cutoff_Cnmono[[1]][["C_"]],
                                      power_seq_mono = cutoff_Cnmono[[1]][["gamma"]])
  
  # Saving for later
  save(cutoff_Cnmono, cutoff_Cnmulti, cutoff_Cnholm,
       file = paste0("Data/simu_vsref/sensprioropt5_uncont_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
} else {
  
  # Else load the stored thresholds
  load("Data/simu_vsref/sens30_uncont_20230609.Rdata")
  
}

# Now that we have the thresholds, simulate the trials
if (simu) {
  
  cat("Operating characteristics for the 13 scenarios :\n")
  opchar_multivar <- list()
  opchar_monovar <- list()
  opchar_holm <- list()
  for (i in names(p_ttt)) {
    
    cat(paste0("Simulation n°", i, "' : ", match(i, names(p_ttt)), " / ", length(p_ttt), ".\n"))
    
    cat("Generating the data.\n")
    tableau_ttt <- gen_patients_multinom(n_sim = nsim_essais,
                                         ana_inter = ana_inter,
                                         multinom_ttt = p_ttt[[i]],
                                         seed = 1993)
    
    cat("Varying threshold, multiarm.\n")
    opchar_multivar[[i]] <- opcharac_efftox(ana_inter = ana_inter,
                                            alpha = alpha,
                                            p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                            prior = p_a * 5,
                                            nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                            tableau_essais = tableau_ttt,
                                            cut_seq = cutoff_Cnmulti[[1]]["C_"], power_seq = cutoff_Cnmulti[[1]]["gamma"])
    
    cat("Varying threshold, monoarm.\n")
    opchar_monovar[[i]] <- list(ttt1 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt1, p_n = p_n, p_a = p_a,
                                                       prior = p_a * 5,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt1"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt2 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt2, p_n = p_n, p_a = p_a,
                                                       prior = p_a * 5,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt2"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt3 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt3, p_n = p_n, p_a = p_a,
                                                       prior = p_a * 5,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt3"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]))
    
    cat("Varying threshold, Holm threshold.\n")
    opchar_holm[[i]] <- opcharac_efftox_holm2(ana_inter = ana_inter,
                                              alpha = alpha,
                                              p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                              prior = p_a * 5,
                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                              tableau_essais = tableau_ttt,
                                              cut_seq_mono = cutoff_Cnholm[[1]][["C_mono"]], power_seq_mono = cutoff_Cnholm[[1]][["gamma_mono"]],
                                              cut_seq_holm = cutoff_Cnholm[[1]][["C_holm"]], power_seq_holm = cutoff_Cnholm[[1]][["gamma_holm"]])
    
  }
  
  # Saving raw data
  save(opchar_multivar, opchar_monovar, opchar_holm,
       file = paste0("Data/simu_vsref/sensprioropt5_bruts_13scenar_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
  if (clear) rm(tableau_ttt)
  
} else {
  
  # Sorry, raw results aren't included
  load("Data/simu_vsref/sensprioropt5_bruts_13scenar_20230613.Rdata")
  
}

## B/ Results ----

# Regroupe data
tab_Cnmultiarm <- lapply(seq_along(opchar_multivar), function(x) {cbind(scenar = names(opchar_multivar)[x], opchar_multivar[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "C<sub>n</sub><sup>m</sup>")
tab_Cnmonoarm <- lapply(seq_along(opchar_monovar), function(x) {cbind(scenar = names(opchar_monovar)[x], do.call("rbind", lapply(seq_along(opchar_monovar[[x]]), function(index) {
  data.frame(ttt = names(opchar_monovar[[x]][index]),
             rejet_h0 = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["rejet_h0"]),
             arret_precoce = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce"]),
             arret_precoce_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_fut"]),
             arret_precoce_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_tox"]),
             arret_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_fut"]),
             arret_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_tox"]),
             tot_pat = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_pts"]),
             tot_eff = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_eff"]),
             tot_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_tox"]))
})))}) %>% 
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "C<sub>n</sub><sup>s</sup>",
         arret = 1 - rejet_h0)
tab_Cnholm <- lapply(seq_along(opchar_holm), function(x) {cbind(scenar = names(opchar_holm)[x], opchar_holm[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "C<sub>n</sub><sup>m,a</sup>")
data_ggplot_13vsref <- rbind(tab_Cnmultiarm, tab_Cnmonoarm, tab_Cnholm)
rm(tab_Cnmultiarm, tab_Cnmonoarm, tab_Cnholm)
tab_Ralpha <- data.frame(
  methode = c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>s</sup>", "C<sub>n</sub><sup>m,a</sup>"),
  global_alpha = c(as.numeric(opchar_multivar[["Global H0"]][["carac_globales"]]["rejet_glob"]),
                   lapply(opchar_monovar[["Global H0"]], "[[", "essais") %>% 
                     do.call(what = "rbind", args = .) %>% 
                     group_by(n_simu) %>% 
                     summarise(rejet_glob = sum(decision == "Accept the treatment") > 0) %>% 
                     pull(rejet_glob) %>% mean(),
                   as.numeric(opchar_holm[["Global H0"]][["carac_globales"]]["rejet_glob"]))
)

data_ggplot_13vsref <- data_ggplot_13vsref %>%
  mutate(scenar = if_else(scenar == "1 works, 1 not, 1 intermediate", "1 H0, 1 H1, 1 intermediate", scenar)) %>% 
  mutate(rejet_h0_mcse = sqrt(rejet_h0 * (1 - rejet_h0) / 10000),
         scenar = factor(scenar, levels = c("Global H0", "LFC", "2 H1, 1H0", "3 H1", "3 better than H1",
                                            "3 not effective", "3 not effective with other endpoint", "1 works better than H1", 
                                            "3 work and 1 better than H1", "2 work and 1 toxic", "2 work and 1 not effective", 
                                            "1 works, 2 intermediate", "1 H0, 1 H1, 1 intermediate"),
                         labels = c("Global H0", "LFC", "2 H1, 1H0", "3 H1", "3 better than H1",
                                    "3 negative treatments", "3 negative treatments (2)", "1 works better than H1", 
                                    "3 work and 1 better than H1", "2 work and 1 toxic", "2 work and 1 not effective", 
                                    "1 works, 2 intermediate", "1 H0, 1 H1, 1 intermediate")),
         methode = factor(methode, 
                          levels = c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>s</sup>", "C<sub>n</sub><sup>m,a</sup>")))
data_ggplot_13vsref <- table_scenar_aggreg_ref %>% 
  right_join(data_ggplot_13vsref, by = c("scenar", "name" = "ttt")) %>% 
  mutate(num_scenar = factor(num_scenar, levels = c(13:1)))
plot1 <- ggplot(data_ggplot_13vsref) +
  geom_col(aes(x = num_scenar, y = rejet_h0, fill = etiquettes), color = "black",
           position = position_dodge2(width = .45), show.legend = FALSE) +
  geom_vline(xintercept = c(1:12 + .5), color = "#b1b0b1", linetype = "dashed") +
  geom_richtext(data = tab_Ralpha %>%
                  mutate(methode = factor(methode, 
                                          levels = c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>s</sup>", "C<sub>n</sub><sup>m,a</sup>"))), 
                aes(x = 13, y = .35, 
                    label = paste0("Estimated FWER: ", global_alpha * 100, "%")), 
                hjust = 0, label.color = NA) +
  facet_wrap(vars(methode), ncol = 2) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "darkgrey", size = .7),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_continuous(breaks = seq(0, 1, .2), labels = percent_format(), expand = c(0, 0), limits = c(0, .9)) +
  scale_fill_manual(values = c("#FFFFFF", "#113C51", "#B4CB65")) +
  coord_flip() +
  labs(y = "Proportion of conclusion to efficacy and no toxicity",
       x = NULL,
       fill = "Treatment arms")
plot2 <- ggplot(data_ggplot_13vsref %>% 
                  filter(methode %in% c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>s</sup>")) %>% 
                  mutate(etiquettes = factor(str_remove(etiquettes, "^Arm "), levels = c("C", "B", "A")),
                         label_scenar = if_else(num_scenar %in% c("10","11", "12", "13"), paste0("Scenario ", num_scenar, ": Arm"), paste0("Scenario   ", num_scenar, ": Arm")))) +
  geom_text(aes(x = num_scenar, y = 1.1, label = label_scenar), hjust = 0) +
  map(0:12, ~ annotate(geom = "segment", x = 0.7 + .x, xend = 1.3 + .x, y = 1.29, yend = 1.29)) +
  geom_richtext(aes(x = num_scenar, y = 1.3, label = etiquettes, color = etiquettes), hjust = 0, position = position_dodge2(width = .9), fill = NA, label.color = NA) +
  facet_wrap(vars(methode), ncol = 1) +
  scale_color_manual(values = c("black", "black", "black")) +
  coord_flip(clip = "off") +
  ylim(c(1, 1.35)) +
  labs(x = NULL,
       y = " ") +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_textbox(
          size = 12, 
          color = "transparent", fill = "transparent", box.color = "transparent",
          halign = 0.5, linetype = 1, r = unit(3, "pt"), width = unit(0.75, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)))
plot_tot <- plot2 + plot1 + plot_layout(ncol = 2, widths = c(1, 5))
ggsave(plot_tot, filename = "Outputs/sensibilite/prioropt5vsref.png", device = "png", height = 10, width = 15)

data_stackedbar <- data_ggplot_13vsref %>% 
  mutate(arret_final = arret - arret_precoce,
         nom_axe = paste0(num_scenar, ":", name),
         nom_axe = factor(nom_axe, levels = paste0(rep(13:1, each = 3), ":", rep(c("ttt3", "ttt2", "ttt1"), 13)), labels = paste0(rep(13:1, each = 3), ":", rep(c("Arm C", "Arm B", "Arm A"), 13)))) %>% 
  pivot_longer(cols = c(arret_precoce, arret_final), names_to = "type_arret", values_to = "pourcent") 
liste_plots <- map(as.character(unique(data_stackedbar$methode)),
                   function(x) {
                     ggplot(data_stackedbar %>% filter(methode == x)) +
                       geom_col(aes(x = nom_axe, y = pourcent, fill = type_arret), 
                                color = "#929494", position = "stack", width = 0.8) +
                       geom_vline(xintercept = c((1:12) * 3 + .5), color = "#666c6c", linetype = "dashed", size = 1) +
                       facet_wrap(vars(methode)) +
                       theme(panel.grid.major.y = element_blank(),
                             panel.grid.major.x = element_line(color = "darkgrey", size = .7),
                             axis.ticks.y = element_blank(),
                             axis.text.y = element_blank(),
                             panel.grid.minor.y = element_blank(),
                             legend.position = "right") +
                       scale_y_continuous(breaks = seq(0, .9, .2), labels = percent_format(), expand = c(0, 0), limits = c(0, 1)) +
                       scale_fill_manual(name = NULL, values = c("#b2df8a", "#a6cee3"), labels = c("Stopping at final analysis", "Stopping at interim analysis")) +
                       coord_flip() +
                       labs(y = "Stopping of the trial",
                            x = NULL)
                   })
legende <- get_legend(liste_plots[[1]])
plot_inter <- wrap_plots(map(liste_plots, ~ .x + theme(legend.position = "none")))
plot2 <- ggplot(data = data_stackedbar %>% 
                  filter(methode == "C<sub>n</sub><sup>m</sup>") %>%
                  mutate(etiqu = if_else(etiquettes == "Arm B", paste0("Scénario ", num_scenar, ":"), NA_character_))) +
  geom_text(aes(x = nom_axe, y = 1, label = etiqu), hjust = 0) +
  geom_text(aes(x = nom_axe, y = 1.15, label = etiquettes), hjust = 0) +
  geom_richtext(aes(x = nom_axe, y = 1.25, label = value), hjust = 0, fill = NA, label.color = NA) +
  annotate(geom = "segment", x = c((1:12) * 3 + .5), xend = c((1:12) * 3 + .5), y = 1, yend = 1.5,
           color = "#666c6c", linetype = "dashed", size = 1) +
  geom_text(x = 40, y = 1.275, aes(label = "(P(Eff), P(Tox))"), color = "black", hjust = 0, fontface = "bold") +
  facet_wrap(vars(methode), ncol = 1) +
  scale_color_manual(values = c("black", "black", "black")) +
  coord_flip(clip = "off") +
  ylim(c(.9, 1.6)) +
  labs(x = " ",
       y = " ") +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(colour = "transparent"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "transparent"),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_textbox(
          size = 12, 
          color = "transparent", fill = "transparent", box.color = "transparent",
          halign = 0.5, linetype = 1, r = unit(3, "pt"), width = unit(0.75, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)))
plot_cn <- (plot2 | wrap_plots(map(map(1:3, ~ liste_plots[[.x]]), ~ .x + theme(legend.position = "none")))) + plot_layout(widths = c(1.5, 3))
plot_tot <- (plot_cn | legende) + plot_layout(widths = c(2, 4, 1))
ggsave(plot_tot, filename = "Outputs/sensibilite/prioropt5vsref_ap.png", device = "png", height = 10, width = 15)


# VIII/ 30 patients with sustained power ----

## 1/ Non contrôlé ----

alpha <- .1
n_bras <- 3
p_n <- c(.15, .3, .15, .4)
p_a <- c(.1, .6, .05, .25)
nsim_oc <- 10000
nsim_essais <- 10000
ana_inter <- rep(10, 3)
methode <- 2L
p_ttt <- list("Global H0" = list(ttt1 = p_n, ttt2 = p_n, ttt3 = p_n),
              "LFC" = list(ttt1 = p_a, ttt2 = p_n, ttt3 = p_n),
              "2 H1, 1H0" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = p_n),
              "3 H1" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = p_a),
              "3 better than H1" = list(ttt1 = c(.07, .7, .03, .2), ttt2 = c(.07, .7, .03, .2), ttt3 = c(.07, .7, .03, .2)),
              "3 not effective" = list(ttt1 = c(.2, .5, .1, .2), ttt2 = c(.1, .35, .05, .5), ttt3 = p_n),
              "3 not effective with other endpoint" = list(ttt1 = c(.2, .6, .2, 0), ttt2 = c(.05, .35, .05, .55), ttt3 = p_n))

### A/ Simulations ----

# Optimize the thresholds or retrieve them from storage
if (simu_seuils) {
  
  cat("Determine the thresholds hyperparameters.\n")
  
  cat("Varying threshold, multiarm.\n")
  cutoff_Cnmulti <- deter_cutoff(alpha = alpha,
                                 ana_inter = ana_inter,
                                 n_bras = n_bras,
                                 p_n = p_n, p_a = p_a,
                                 nsim_oc = nsim_oc,
                                 affich_mat = "No")
  
  cat("Varying threshold, monoarm.\n")
  cutoff_Cnmono <- deter_cutoff(alpha = alpha,
                                ana_inter = ana_inter,
                                p_n = p_n, p_a = p_a,
                                nsim_oc = nsim_oc,
                                affich_mat = "No")
  
  cat("Varying threshold, Holm.\n")
  cutoff_Cnholm <- deter_cutoff_holm2(alpha = alpha,
                                      ana_inter = ana_inter,
                                      n_bras = n_bras, 
                                      p_n = p_n, p_a = p_a,
                                      nsim_oc = nsim_oc,
                                      affich_mat = "No",
                                      cut_seq_mono = cutoff_Cnmono[[1]][["C_"]],
                                      power_seq_mono = cutoff_Cnmono[[1]][["gamma"]])
  
  # Saving for later
  save(cutoff_Cnmono, cutoff_Cnmulti, cutoff_Cnholm,
       file = paste0("Data/simu_vsref/cutoff_sens30_uncont_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
} else {
  
  # Else load the stored thresholds
  load("Data/simu_vsref/cutoff_sens30_uncont_20230619.Rdata")
  
}

# Now that we have the thresholds, simulate the trials
if (simu) {
  
  cat("Operating characteristics for the 13 scenarios :\n")
  opchar_multivar <- list()
  opchar_monovar <- list()
  opchar_holm <- list()
  for (i in names(p_ttt)) {
    
    cat(paste0("Simulation n°", i, "' : ", match(i, names(p_ttt)), " / ", length(p_ttt), ".\n"))
    
    cat("Generating the data.\n")
    tableau_ttt <- gen_patients_multinom(n_sim = nsim_essais,
                                         ana_inter = ana_inter,
                                         multinom_ttt = p_ttt[[i]],
                                         seed = 1993)
    
    cat("Varying threshold, multiarm.\n")
    opchar_multivar[[i]] <- opcharac_efftox(ana_inter = ana_inter,
                                            alpha = alpha,
                                            p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                            nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                            tableau_essais = tableau_ttt,
                                            cut_seq = cutoff_Cnmulti[[1]]["C_"], power_seq = cutoff_Cnmulti[[1]]["gamma"])
    
    
    cat("Varying threshold, monoarm.\n")
    opchar_monovar[[i]] <- list(ttt1 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt1, p_n = p_n, p_a = p_a,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt1"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt2 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt2, p_n = p_n, p_a = p_a,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt2"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt3 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt3, p_n = p_n, p_a = p_a,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt3"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]))
    
    cat("Varying threshold, Holm threshold.\n")
    opchar_holm[[i]] <- opcharac_efftox_holm2(ana_inter = ana_inter,
                                              alpha = alpha,
                                              p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                              tableau_essais = tableau_ttt,
                                              cut_seq_mono = cutoff_Cnholm[[1]][["C_mono"]], power_seq_mono = cutoff_Cnholm[[1]][["gamma_mono"]],
                                              cut_seq_holm = cutoff_Cnholm[[1]][["C_holm"]], power_seq_holm = cutoff_Cnholm[[1]][["gamma_holm"]])
    
  }
  
  # Saving raw data
  save(opchar_multivar, opchar_monovar, opchar_holm,
       file = paste0("Data/simu_vsref/sens_30ptsuncont_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
  if (clear) rm(tableau_ttt)
  
} else {
  
  # Sorry, raw results aren't included
  load("Data/simu_vsref/sens_30ptsuncont_20230619.Rdata")
  
}


### B/ Results ----

# Regroup data
tab_Cnmultiarm <- lapply(seq_along(opchar_multivar), function(x) {cbind(scenar = names(opchar_multivar)[x], opchar_multivar[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Multiarm C<sub>n</sub>")
tab_Cnmonoarm <- lapply(seq_along(opchar_monovar), function(x) {cbind(scenar = names(opchar_monovar)[x], do.call("rbind", lapply(seq_along(opchar_monovar[[x]]), function(index) {
  data.frame(ttt = names(opchar_monovar[[x]][index]),
             rejet_h0 = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["rejet_h0"]),
             arret_precoce = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce"]),
             arret_precoce_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_fut"]),
             arret_precoce_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_tox"]),
             arret_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_fut"]),
             arret_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_tox"]),
             tot_pat = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_pts"]),
             tot_eff = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_eff"]),
             tot_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_tox"]))
})))}) %>% 
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Monoarm C<sub>n</sub>",
         arret = 1 - rejet_h0)
tab_Cnholm <- lapply(seq_along(opchar_holm), function(x) {cbind(scenar = names(opchar_holm)[x], opchar_holm[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Holm C<sub>n</sub>")
data_ggplot_13vsref <- rbind(tab_Cnmultiarm, tab_Cnmonoarm, tab_Cnholm)
rm(tab_Cnmultiarm, tab_Cnmonoarm, tab_Cnholm)
tab_Ralpha <- data.frame(
  methode = c("Multiarm C<sub>n</sub>", "Monoarm C<sub>n</sub>", "Holm C<sub>n</sub>"),
  global_alpha = c(as.numeric(opchar_multivar[["Global H0"]][["carac_globales"]]["rejet_glob"]),
                   lapply(opchar_monovar[["Global H0"]], "[[", "essais") %>% 
                     do.call(what = "rbind", args = .) %>% 
                     group_by(n_simu) %>% 
                     summarise(rejet_glob = sum(decision == "Accept the treatment") > 0) %>% 
                     pull(rejet_glob) %>% mean(),
                   as.numeric(opchar_holm[["Global H0"]][["carac_globales"]]["rejet_glob"]))
)

data_ggplot_13vsref <- data_ggplot_13vsref %>%
  mutate(scenar = if_else(scenar == "1 works, 1 not, 1 intermediate", "1 H0, 1 H1, 1 intermediate", scenar),
         methode = str_replace_all(methode, "Multiarm", "Multi-arm"),
         methode = str_replace_all(methode, "Monoarm", "Single-arm")) %>% 
  mutate(rejet_h0_mcse = sqrt(rejet_h0 * (1 - rejet_h0) / 10000),
         scenar = factor(scenar, levels = c("Global H0", "LFC", "2 H1, 1H0", "3 H1", "3 better than H1",
                                            "3 not effective", "3 not effective with other endpoint", "1 works better than H1", 
                                            "3 work and 1 better than H1", "2 work and 1 toxic", "2 work and 1 not effective", 
                                            "1 works, 2 intermediate", "1 H0, 1 H1, 1 intermediate"),
                         labels = c("Global H0", "LFC", "2 H1, 1H0", "3 H1", "3 better than H1",
                                    "3 negative treatments", "3 negative treatments (2)", "1 works better than H1", 
                                    "3 work and 1 better than H1", "2 work and 1 toxic", "2 work and 1 not effective", 
                                    "1 works, 2 intermediate", "1 H0, 1 H1, 1 intermediate")),
         methode = str_replace_all(methode, "^Holm (.*)$", "Multi-arm \\1<sup>(aa)</sup>"), 
         methode = factor(methode, 
                          levels = c("Multi-arm C<sub>n</sub>", "Multi-arm C<sub>n</sub><sup>(aa)</sup>", "Single-arm C<sub>n</sub>"),
                          labels = c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>m,a</sup>", "C<sub>n</sub><sup>s</sup>")))
data_ggplot_13vsref <- table_scenar_aggreg_ref %>% 
  right_join(data_ggplot_13vsref, by = c("scenar", "name" = "ttt")) %>% 
  mutate(num_scenar = factor(num_scenar, levels = c(7:1)))
plot1 <- ggplot(data_ggplot_13vsref %>% mutate(methode = droplevels(methode))) +
  geom_col(aes(x = num_scenar, y = rejet_h0, fill = etiquettes), color = "black",
           position = position_dodge2(width = .45)) +
  geom_vline(xintercept = c(1:6 + .5), color = "#b1b0b1", linetype = "dashed") +
  geom_richtext(data = tab_Ralpha %>% 
                  filter(methode != "Bonferroni C<sub>n</sub>") %>% 
                  mutate(methode = str_replace_all(methode, "Multiarm", "Multi-arm"),
                         methode = str_replace_all(methode, "Monoarm", "Single-arm"),
                         methode = str_replace_all(methode, "^Holm (.*)$", "Multi-arm \\1<sup>(aa)</sup>"),
                         methode = factor(methode, 
                                          levels = c("Multi-arm C<sub>n</sub>", "Multi-arm C<sub>n</sub><sup>(aa)</sup>", "Single-arm C<sub>n</sub>"),
                                          labels = c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>m,a</sup>", "C<sub>n</sub><sup>s</sup>"))), 
                aes(x = 7, y = .35, 
                    label = paste0("Estimated FWER: ", global_alpha * 100, "%")), 
                hjust = 0, label.color = NA) +
  facet_wrap(vars(methode), ncol = 3) +
  theme(panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_continuous(breaks = seq(0, 1, .2), labels = percent_format(), expand = c(0, 0), limits = c(0, 1)) +
  scale_fill_manual(values = c("#FFFFFF", "#113C51", "#B4CB65")) +
  coord_flip() +
  labs(y = "Proportion of rejecting H<sub>0</sub>",
       x = NULL,
       fill = "Treatment arms")
plot2 <- ggplot(data_ggplot_13vsref %>% filter(methode %in% c("C<sub>n</sub><sup>m</sup>"))) +
  geom_text(aes(x = num_scenar, y = 1, label = paste0("Scenario ", num_scenar, ":")), hjust = 0) +
  geom_richtext(aes(x = num_scenar, y = 1.3, label = rev(etiquettes)), hjust = 0, position = position_dodge2(width = .9), fill = NA, label.color = NA) +
  facet_wrap(vars(methode), ncol = 1) +
  scale_color_manual(values = c("black", "black", "black")) +
  coord_flip(clip = "off") +
  ylim(c(.9, 1.6)) +
  labs(x = NULL,
       y = " ") +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_textbox(
          size = 12, 
          color = "transparent", fill = "transparent", box.color = "transparent",
          halign = 0.5, linetype = 1, r = unit(3, "pt"), width = unit(0.75, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)))
plot_tot <- plot2 + plot1 + plot_layout(ncol = 2, widths = c(1, 3.5))
ggsave(plot_tot, filename = "Outputs/sensibilite/30ptspuissvsref.png", device = "png", height = 10, width = 15)


## 2/ Controlled ----

alpha <- .1
n_bras <- 3
p_n <- c(.3, .3, .1, .3)
p_a <- c(.15, .75, .05, .05)
nsim_oc <- 10000
nsim_essais <- 10000
ana_inter <- rep(10, 3)
methode <- 2L
p_ttt <- list("Global H0" = list(ttt1 = p_n, ttt2 = p_n, ttt3 = p_n),
              "LFC" = list(ttt1 = p_a, ttt2 = p_n, ttt3 = p_n),
              "2 H1, 1H0" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = p_n),
              "3 H1" = list(ttt1 = p_a, ttt2 = p_a, ttt3 = p_a),
              "3 better than H1" = list(ttt1 = c(.1, .8, .05, .05), ttt2 = c(.1, .8, .05, .05), ttt3 = c(.1, .8, .05, .05)),
              "3 not effective" = list(ttt1 = c(.2, .6, .2, 0), ttt2 = c(.1, .4, .1, .4), ttt3 = p_n),
              "3 not effective with other endpoint" = list(ttt1 = c(.4, .45, .05, .1), ttt2 = c(.1, .35, .05, .5), ttt3 = p_n))

### A/ Simulations ----

# Optimize the thresholds or retrieve them from storage
if (simu_seuils) {
  
  cat("Determine the thresholds hyperparameters.\n")
  
  cat("Varying threshold, multiarm.\n")
  cutoff_Cnmulti <- deter_cutoff(alpha = alpha,
                                 ana_inter = ana_inter,
                                 n_bras = n_bras,
                                 p_n = p_n, p_a = p_a,
                                 nsim_oc = nsim_oc,
                                 delta = c(0, 0),
                                 affich_mat = "No")
  
  cat("Varying threshold, monoarm.\n")
  cutoff_Cnmono <- deter_cutoff(alpha = alpha,
                                ana_inter = ana_inter,
                                p_n = p_n, p_a = p_a,
                                nsim_oc = nsim_oc,
                                delta = c(0, 0),
                                affich_mat = "No")
  
  cat("Varying threshold, Holm.\n")
  cutoff_Cnholm <- deter_cutoff_holm2(alpha = alpha,
                                      ana_inter = ana_inter,
                                      n_bras = n_bras, 
                                      p_n = p_n, p_a = p_a,
                                      nsim_oc = nsim_oc,
                                      delta = c(0, 0),
                                      affich_mat = "No",
                                      cut_seq_mono = cutoff_Cnmono[[1]][["C_"]],
                                      power_seq_mono = cutoff_Cnmono[[1]][["gamma"]])
  
  save(cutoff_Cnmulti, cutoff_Cnmono, cutoff_Cnholm,
       file = paste0("Data/sensibilte/senscutoff_cont_13scenar_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
} else {
  
  load("Data/simu_vscont/senscutoff_13scenar_20230619.Rdata")
  
}

# With the thresholds computed, we can simulate trials
if (simu) {
  
  cat("Operating characteristics:\n")
  opchar_multivar <- list()
  opchar_monovar <- list()
  opchar_holm <- list()
  for (i in names(p_ttt)) {
    
    cat(paste0("Simulation n°", i, "' : ", match(i, names(p_ttt)), " / ", length(p_ttt), ".\n"))
    
    cat("Generate data of the trials.\n")
    tableau_ttt <- gen_patients_multinom(n_sim = nsim_essais, 
                                         ana_inter = ana_inter,
                                         multinom_ttt = p_ttt[[i]],
                                         multinom_cont = p_n,
                                         seed = 1993)
    
    cat("Varying threshold, multiarm.\n")
    opchar_multivar[[i]] <- opcharac_efftox(ana_inter = ana_inter,
                                            alpha = alpha,
                                            p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                            nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                            tableau_essais = tableau_ttt, delta = c(0, 0),
                                            cut_seq = cutoff_Cnmulti[[1]]["C_"], power_seq = cutoff_Cnmulti[[1]]["gamma"])
    
    cat("Varying threshold, Holm.\n")
    opchar_holm[[i]] <- opcharac_efftox_holm2(ana_inter = ana_inter,
                                              alpha = alpha,
                                              p_reel = p_ttt[[i]], p_n = p_n, p_a = p_a,
                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                              tableau_essais = tableau_ttt, delta = c(0, 0),
                                              cut_seq_mono = cutoff_Cnholm[[1]][["C_mono"]], power_seq_mono = cutoff_Cnholm[[1]][["gamma_mono"]],
                                              cut_seq_holm = cutoff_Cnholm[[1]][["C_holm"]], power_seq_holm = cutoff_Cnholm[[1]][["gamma_holm"]])
    
    cat("Varying threshold, monoarm.\n")
    opchar_monovar[[i]] <- list(ttt1 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt1, p_n = p_n, p_a = p_a,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt %in% c("ttt0", "ttt1")), delta = c(0, 0),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt2 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt2, p_n = p_n, p_a = p_a,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt %in% c("ttt0", "ttt2")), delta = c(0, 0),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt3 = opcharac_efftox(ana_inter = ana_inter,
                                                       alpha = alpha,
                                                       p_reel = p_ttt[[i]]$ttt3, p_n = p_n, p_a = p_a,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt %in% c("ttt0", "ttt3")), delta = c(0, 0),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]))
    
  }
  
  # Saving raw data
  save(opchar_multivar, opchar_monovar, opchar_holm,
       file = paste0("Data/sensibilite/resultatssens_brutscont_13scenar_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
} else {
  
  # If you have raw data stored, load them
  load("Data/simu_vscont/resultatssens_brutscont_13scenar_20230619.Rdata")
  
}


### B/ Results ----

tab_Cnmultiarm <- lapply(seq_along(opchar_multivar), function(x) {cbind(scenar = names(opchar_multivar)[x], opchar_multivar[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Multiarm C<sub>n</sub>")
tab_Cnholm <- lapply(seq_along(opchar_holm), function(x) {cbind(scenar = names(opchar_holm)[x], opchar_holm[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Holm C<sub>n</sub>")
tab_Cnmonoarm <- lapply(seq_along(opchar_monovar), function(x) {cbind(scenar = names(opchar_monovar)[x], do.call("rbind", lapply(seq_along(opchar_monovar[[x]]), function(index) {
  data.frame(ttt = names(opchar_monovar[[x]][index]),
             rejet_h0 = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["rejet_h0"]),
             arret_precoce = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce"]),
             arret_precoce_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_fut"]),
             arret_precoce_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_tox"]),
             arret_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_fut"]),
             arret_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_tox"]),
             tot_pat = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_pts"]),
             tot_eff = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_eff"]),
             tot_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_tox"]))
})))}) %>% 
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Monoarm C<sub>n</sub>",
         arret = 1 - rejet_h0)
data_ggplot_13vscont <- rbind(tab_Cnmultiarm, tab_Cnmonoarm, tab_Cnholm)
tab_Ralpha <- data.frame(
  methode = c("Multiarm C<sub>n</sub>", "Monoarm C<sub>n</sub>", "Holm C<sub>n</sub>"),
  global_alpha = c(as.numeric(opchar_multivar[["Global H0"]][["carac_globales"]]["rejet_glob"]),
                   lapply(opchar_monovar[["Global H0"]], "[[", "essais") %>% 
                     do.call(what = "rbind", args = .) %>% 
                     group_by(n_simu) %>% 
                     summarise(rejet_glob = sum(decision == "Accept the treatment") > 0) %>% 
                     pull(rejet_glob) %>% mean(),
                   as.numeric(opchar_holm[["Global H0"]][["carac_globales"]]["rejet_glob"]))
)
rm(tab_Cnmultiarm, tab_Cnmonoarm, tab_Cnholm)

data_ggplot_13vscont <- data_ggplot_13vscont %>% 
  mutate(rejet_h0_mcse = sqrt(rejet_h0 * (1 - rejet_h0) / 10000),
         scenar = factor(scenar, levels = c("Global H0", "LFC", "2 H1, 1H0", "3 H1", "3 better than H1",
                                            "3 not effective", "3 not effective with other endpoint"),
                         labels = c("Global H0", "LFC", "2 H1, 1H0", "3 H1", "3 better than H1",
                                    "3 negative treatments", "3 negative treatments (2)")),
         methode = str_replace_all(methode, "Multiarm", "Multi-arm"),
         methode = str_replace_all(methode, "Monoarm", "Single-arm"),
         methode = str_replace_all(methode, "^Holm (.*)$", "Multi-arm \\1<sup>(aa)</sup>"),
         methode = factor(methode, levels = c("Multi-arm C<sub>n</sub>", "Multi-arm C<sub>n</sub><sup>(aa)</sup>", "Single-arm C<sub>n</sub>", "Multi-arm &epsilon;", "Bonferroni &epsilon;", "Single-arm &epsilon;"))) %>% 
  filter(!is.na(methode))
data_ggplot_13vscont <- table_scenar_aggreg_cont %>% 
  right_join(data_ggplot_13vscont, by = c("scenar", "name" = "ttt")) %>%
  mutate(num_scenar = factor(num_scenar, levels = c(7:1)))
plot1 <- ggplot(data_ggplot_13vscont) +
  geom_col(aes(x = num_scenar, y = rejet_h0, fill = etiquettes), color = "black",
           position = position_dodge2(width = .45)) +
  geom_vline(xintercept = c(1:6 + .5), color = "#b1b0b1", linetype = "dashed") +
  geom_richtext(data = tab_Ralpha %>% 
                  mutate(methode = str_replace_all(methode, "Multiarm", "Multi-arm"),
                         methode = str_replace_all(methode, "Monoarm", "Single-arm"),
                         methode = str_replace_all(methode, "^Holm (.*)$", "Multi-arm \\1<sup>(aa)</sup>"),
                         methode = factor(methode, levels = c("Multi-arm C<sub>n</sub>", "Multi-arm C<sub>n</sub><sup>(aa)</sup>", "Single-arm C<sub>n</sub>", "Multi-arm &epsilon;", "Bonferroni &epsilon;", "Single-arm &epsilon;"))) %>% 
                  filter(!is.na(methode)), 
                aes(x = 7, y = .35, 
                    label = paste0("Estimated FWER : ", global_alpha * 100, "%")), 
                hjust = 0, label.color = NA) +
  facet_wrap(vars(methode), ncol = 3) +
  theme(panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_continuous(breaks = seq(0, 1, .2), labels = percent_format(), expand = c(0, 0), limits = c(0, .95)) +
  scale_fill_manual(values = c("#AA3939", "#113C51", "#B4CB65")) +
  coord_flip() +
  labs(y = "Proportion of rejecting H<sub>0</sub>",
       x = NULL,
       fill = "Treatment arms")
plot2 <- ggplot(data_ggplot_13vscont %>% filter(methode %in% c("Multi-arm C<sub>n</sub>"))) +
  geom_text(aes(x = num_scenar, y = 1, label = paste0("Scenario ", num_scenar, ":")), hjust = 0) +
  geom_richtext(aes(x = num_scenar, y = 1.3, label = rev(etiquettes)), hjust = 0, position = position_dodge2(width = .9), fill = NA, label.color = NA) +
  facet_wrap(vars(methode), ncol = 1) +
  scale_color_manual(values = c("black", "black", "black")) +
  coord_flip(clip = "off") +
  ylim(c(.9, 1.8)) +
  labs(x = NULL,
       y = " ") +
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.text.x = element_textbox(
          size = 12, 
          color = "transparent", fill = "transparent", box.color = "transparent",
          halign = 0.5, linetype = 1, r = unit(3, "pt"), width = unit(0.75, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)))
plot_tot <- plot2 + plot1 + plot_layout(ncol = 2, widths = c(1, 3.5)) + plot_annotation(subtitle = "Control arm: (0.60, 0.40)")
ggsave(plot_tot, filename = "Outputs/sensibilite/30ptspuissvscont.png", device = "png", height = 10, width = 15)


# IX/ Impact of correlation between efficacy and toxicity ----

generer_corr_multi <- function(vec_inter,
                               eff,
                               tox) {
  
  data.frame(Pet = vec_inter) %>% 
    mutate(probas = map(Pet, ~ c(Pefftox = .x, Peffnotox = eff - .x, Pnoefftox = tox - .x, Pnoeffnotox = 1 - eff - tox + .x)),
           correlation = map_dbl(probas, ~ (.x[1] - eff * tox) / sqrt((eff - eff * eff) * (tox - tox * tox))))
  
}
calculate_corr <- function(Peff, Ptox, Pinter) {
  (Pinter - Peff * Ptox) / sqrt((Peff - Peff ^ 2) * (Ptox - Ptox ^ 2))
}
calculate_pinter <- function(Peff, Ptox, corr) {
  corr * sqrt((Peff - Peff ^ 2) * (Ptox - Ptox ^ 2)) + Peff * Ptox
}
generer_scenar_corr <- function(Peff, Ptox, Rmin, Rmax, nom_scenar = NULL) {
  
  if (is.null(nom_scenar)) nom_scenar <- paste0("Eff", round(Peff, 2), "/Tox", round(Ptox, 2))
  
  Pefftox  <- Peff * Ptox
  VecCorrs <- seq(Rmin, Rmax, length.out = 31)
  
  resultats <- tibble(
    scenar = nom_scenar,
    nom_corr = seq_along(VecCorrs),
    R = VecCorrs
  ) %>% 
    mutate(
      probas = map(R, \(correlation) {
        Pinter <- calculate_pinter(Peff, Ptox, correlation)
        return(setNames(round(c(Pinter, Peff - Pinter, Ptox - Pinter, 1 - Peff - Ptox + Pinter), 4), c("EffTox", "EffNotox", "NoeffTox", "NoeffNotox")))
      }),
      proba_temp = probas
    ) %>% 
    unnest_wider(proba_temp)
  
  
  return(resultats)
  
}

## 1/ Uncontrolled ----

alpha <- .1
n_bras <- 3
p_n <- c(.15, .3, .15, .4)
p_a <- c(.18, .42, .02, .38)
nsim_oc <- 10000
nsim_essais <- 10000
ana_inter <- rep(15, 4)
methode <- 2L

# Get all scenarios with all 31 correlations per scenario to plot them
p_ttt <- list("Global H0" = list(ttt1 = c(Peff = .45, Ptox = .3), ttt2 = c(Peff = .45, Ptox = .3), ttt3 = c(Peff = .45, Ptox = .3)),
              "LFC" = list(ttt1 = c(Peff = .6, Ptox = .2), ttt2 = c(Peff = .45, Ptox = .3), ttt3 = c(Peff = .45, Ptox = .3)),
              "2 H1, 1H0" = list(ttt1 = c(Peff = .6, Ptox = .2), ttt2 = c(Peff = .6, Ptox = .2), ttt3 = c(Peff = .45, Ptox = .3)),
              "3 H1" = list(ttt1 = c(Peff = .6, Ptox = .2), ttt2 = c(Peff = .6, Ptox = .2), ttt3 = c(Peff = .6, Ptox = .2)),
              "3 better than H1" = list(ttt1 = c(Peff = .65, Ptox = .2), ttt2 = c(Peff = .65, Ptox = .2), ttt3 = c(Peff = .65, Ptox = .2)),
              "3 not effective" = list(ttt1 = c(Peff = .6, Ptox = .3), ttt2 = c(Peff = .45, Ptox = .2), ttt3 = c(Peff = .45, Ptox = .3)))
noms_scenars <- names(p_ttt)
liste_scenars <- map2_dfr(
  p_ttt,
  noms_scenars,
  \(liste_probas, nom) {
    Rmins <- max(map_dbl(liste_probas, ~ calculate_corr(.x[["Peff"]], .x[["Ptox"]], max(0, .x[["Peff"]] + .x[["Ptox"]] - 1))))
    Rmaxs <- min(map_dbl(liste_probas, ~ calculate_corr(.x[["Peff"]], .x[["Ptox"]], min(.x[["Peff"]], .x[["Ptox"]]))))
    map_dfc(seq_along(liste_probas), \(proba) {
      generer_scenar_corr(liste_probas[[proba]][["Peff"]], liste_probas[[proba]][["Ptox"]], Rmins, Rmaxs, nom) %>% 
        'names<-'(paste0(names(.), "_", proba))
    })
  }
) %>% 
  mutate(p_ttt = pmap(list(ttt1 = probas_1, ttt2 = probas_2, ttt3 = probas_3), \(ttt1, ttt2, ttt3) list("ttt1" = ttt1, "ttt2" = ttt2, "ttt3" = ttt3))) %>% 
  select(scenar = scenar_1, R = R_1, p_ttt)

# Retrieve thresholds
load("Data/simu_vsref/cutoff_13scenar_20221202.Rdata")

if (simu) {
  
  opchar_multivar <- list()
  opchar_monovar <- list()
  opchar_multicstt <- list()
  opchar_monocstt <- list()
  opchar_bonfevar <- list()
  opchar_bonfecstt <- list()
  opchar_holm <- list()
  
  # Progress bar
  with_progress({
    PBarre <- progressor(steps = nrow(liste_scenars) * 8)
    for (i in seq_len(nrow(liste_scenars))) {
      
      PBarre(amount = 0, message = liste_scenars[["scenar"]][i])
      
      tableau_ttt <- gen_patients_multinom(n_sim = nsim_essais,
                                           ana_inter = ana_inter,
                                           multinom_ttt = liste_scenars[["p_ttt"]][[i]],
                                           seed = 1993) %>% suppressWarnings() %>% suppressMessages()
      PBarre()
      
      opchar_multivar[[i]] <- opcharac_efftox(ana_inter = ana_inter,
                                              alpha = alpha,
                                              p_reel = liste_scenars[["p_ttt"]][[i]], p_n = p_n, p_a = p_a,
                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                              tableau_essais = tableau_ttt,
                                              cut_seq = cutoff_Cnmulti[[1]]["C_"], power_seq = cutoff_Cnmulti[[1]]["gamma"]) %>% suppressWarnings() %>% suppressMessages()
      PBarre()
      
      opchar_bonfevar[[i]] <- opcharac_efftox(ana_inter = ana_inter,
                                              alpha = alpha,
                                              p_reel = liste_scenars[["p_ttt"]][[i]], p_n = p_n, p_a = p_a,
                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                              tableau_essais = tableau_ttt,
                                              cut_seq = cutoff_Cnbonf[[1]]["C_"], power_seq = cutoff_Cnbonf[[1]]["gamma"]) %>% suppressWarnings() %>% suppressMessages()
      PBarre()
      
      opchar_monovar[[i]] <- list(ttt1 = opcharac_efftox(ana_inter = ana_inter,
                                                         alpha = alpha,
                                                         p_reel = liste_scenars[["p_ttt"]][[i]]$ttt1, p_n = p_n, p_a = p_a,
                                                         nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                         tableau_essais = tableau_ttt %>% filter(ttt == "ttt1"),
                                                         cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]) %>% suppressWarnings() %>% suppressMessages(),
                                  ttt2 = opcharac_efftox(ana_inter = ana_inter,
                                                         alpha = alpha,
                                                         p_reel = liste_scenars[["p_ttt"]][[i]]$ttt2, p_n = p_n, p_a = p_a,
                                                         nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                         tableau_essais = tableau_ttt %>% filter(ttt == "ttt2"),
                                                         cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]) %>% suppressWarnings() %>% suppressMessages(),
                                  ttt3 = opcharac_efftox(ana_inter = ana_inter,
                                                         alpha = alpha,
                                                         p_reel = liste_scenars[["p_ttt"]][[i]]$ttt3, p_n = p_n, p_a = p_a,
                                                         nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                         tableau_essais = tableau_ttt %>% filter(ttt == "ttt3"),
                                                         cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]) %>% suppressWarnings() %>% suppressMessages())
      PBarre()
      
      opchar_multicstt[[i]] <- opcharac_efftox_const(ana_inter = ana_inter,
                                                     alpha = alpha,
                                                     p_reel = liste_scenars[["p_ttt"]][[i]], p_n = p_n, p_a = p_a,
                                                     nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                     tableau_essais = tableau_ttt,
                                                     seq_eff = cutoff_Gammulti[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammulti[["seuils"]]["gamma_tox"]) %>% suppressWarnings() %>% suppressMessages()
      PBarre()
      
      opchar_monocstt[[i]] <- list(ttt1 = opcharac_efftox_const(ana_inter = ana_inter,
                                                                alpha = alpha,
                                                                p_reel = liste_scenars[["p_ttt"]][[i]]$ttt1, p_n = p_n, p_a = p_a,
                                                                nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                                tableau_essais = tableau_ttt %>% filter(ttt == "ttt1"),
                                                                seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]) %>% suppressWarnings() %>% suppressMessages(),
                                   ttt2 = opcharac_efftox_const(ana_inter = ana_inter,
                                                                alpha = alpha,
                                                                p_reel = liste_scenars[["p_ttt"]][[i]]$ttt2, p_n = p_n, p_a = p_a,
                                                                nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                                tableau_essais = tableau_ttt %>% filter(ttt == "ttt2"),
                                                                seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]) %>% suppressWarnings() %>% suppressMessages(),
                                   ttt3 = opcharac_efftox_const(ana_inter = ana_inter,
                                                                alpha = alpha,
                                                                p_reel = liste_scenars[["p_ttt"]][[i]]$ttt3, p_n = p_n, p_a = p_a,
                                                                nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                                tableau_essais = tableau_ttt %>% filter(ttt == "ttt3"),
                                                                seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]) %>% suppressWarnings() %>% suppressMessages())
      PBarre()
      
      opchar_bonfecstt[[i]] <- opcharac_efftox_const(ana_inter = ana_inter,
                                                     alpha = alpha,
                                                     p_reel = liste_scenars[["p_ttt"]][[i]], p_n = p_n, p_a = p_a,
                                                     nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                     tableau_essais = tableau_ttt,
                                                     seq_eff = cutoff_Gambonf[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gambonf[["seuils"]]["gamma_tox"]) %>% suppressWarnings() %>% suppressMessages()
      PBarre()
      
      opchar_holm[[i]] <- opcharac_efftox_holm2(ana_inter = ana_inter,
                                                alpha = alpha,
                                                p_reel = liste_scenars[["p_ttt"]][[i]], p_n = p_n, p_a = p_a,
                                                nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                tableau_essais = tableau_ttt,
                                                cut_seq_mono = cutoff_Cnholm[[1]][["C_mono"]], power_seq_mono = cutoff_Cnholm[[1]][["gamma_mono"]],
                                                cut_seq_holm = cutoff_Cnholm[[1]][["C_holm"]], power_seq_holm = cutoff_Cnholm[[1]][["gamma_holm"]]) %>% suppressWarnings() %>% suppressMessages()
      PBarre()
    }
  })
  
  # Formatting data to avoid saving overheavy object
  tab_Cnmultiarm <- lapply(seq_along(opchar_multivar), function(x) {cbind(scenar = x, opchar_multivar[[x]]$carac_bras)}) %>%
    do.call(what = "rbind", args = .) %>% 
    mutate(methode = "C<sub>n</sub><sup>m</sup>")
  tab_Cnbonferr <- lapply(seq_along(opchar_bonfevar), function(x) {cbind(scenar = x, opchar_bonfevar[[x]]$carac_bras)}) %>%
    do.call(what = "rbind", args = .) %>% 
    mutate(methode = "C<sub>n</sub><sup>Bm</sup>")
  tab_Cnmonoarm <- lapply(seq_along(opchar_monovar), function(x) {cbind(scenar = x, do.call("rbind", lapply(seq_along(opchar_monovar[[x]]), function(index) {
    data.frame(ttt = names(opchar_monovar[[x]][index]),
               rejet_h0 = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["rejet_h0"]),
               arret_precoce = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce"]),
               arret_precoce_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_fut"]),
               arret_precoce_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_tox"]),
               arret_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_fut"]),
               arret_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_tox"]),
               tot_pat = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_pts"]),
               tot_eff = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_eff"]),
               tot_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_tox"]))
  })))}) %>% 
    do.call(what = "rbind", args = .) %>% 
    mutate(methode = "C<sub>n</sub><sup>s</sup>",
           arret = 1 - rejet_h0)
  tab_Gammultiarm <- lapply(seq_along(opchar_multicstt), function(x) {cbind(scenar = x, opchar_multicstt[[x]]$carac_bras)}) %>%
    do.call(what = "rbind", args = .) %>% 
    mutate(methode = "&epsilon;<sup>m</sup>")
  tab_Gambonferr <- lapply(seq_along(opchar_bonfecstt), function(x) {cbind(scenar = x, opchar_bonfecstt[[x]]$carac_bras)}) %>%
    do.call(what = "rbind", args = .) %>% 
    mutate(methode = "&epsilon;<sup>Bm</sup>")
  tab_Gammonoarm <- lapply(seq_along(opchar_monocstt), function(x) {cbind(scenar = x, do.call("rbind", lapply(seq_along(opchar_monocstt[[x]]), function(index) {
    data.frame(ttt = names(opchar_monocstt[[x]][index]),
               rejet_h0 = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["rejet_h0"]),
               arret_precoce = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["arret_precoce"]),
               arret_precoce_fut = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["arret_precoce_fut"]),
               arret_precoce_tox = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["arret_precoce_tox"]),
               arret_fut = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["arret_fut"]),
               arret_tox = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["arret_tox"]),
               tot_pat = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["nb_pts"]),
               tot_eff = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["nb_eff"]),
               tot_tox = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["nb_tox"]))
  })))}) %>% 
    do.call(what = "rbind", args = .) %>% 
    mutate(methode = "&epsilon;<sup>s</sup>",
           arret = 1 - rejet_h0)
  tab_Cnholm <- lapply(seq_along(opchar_holm), function(x) {cbind(scenar = x, opchar_holm[[x]]$carac_bras)}) %>%
    do.call(what = "rbind", args = .) %>% 
    mutate(methode = "C<sub>n</sub><sup>m,a</sup>")
  data_ggplot_13vsref <- rbind(tab_Cnmultiarm, tab_Cnbonferr, tab_Cnmonoarm, tab_Gammultiarm, tab_Gambonferr, tab_Gammonoarm, tab_Cnholm)
  data_ggplot_13vsref <- left_join(data_ggplot_13vsref %>% 
                                      rename(num_corr = scenar) %>% 
                                      mutate(clef_merge = rep(rep(1:186, each = 3), 7)),
                                    liste_scenars %>% 
                                      mutate(clef_merge = 1:186) %>% 
                                      select(-p_ttt))
  tab_Ralpha <- data.frame(
    methode = rep(c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>Bm</sup>", "C<sub>n</sub><sup>s</sup>", "&epsilon;<sup>m</sup>", "&epsilon;<sup>Bm</sup>", "&epsilon;<sup>s</sup>", "C<sub>n</sub><sup>m,a</sup>"), times = 31),
    num_corr = rep(1:31, each = 7),
    global_alpha = unlist(map(seq(1, 31, 1), \(globH) {
      c(as.numeric(opchar_multivar[[globH]][["carac_globales"]]["rejet_glob"]),
        as.numeric(opchar_bonfevar[[globH]][["carac_globales"]]["rejet_glob"]),
        lapply(opchar_monovar[[globH]], "[[", "essais") %>% 
          do.call(what = "rbind", args = .) %>% 
          group_by(n_simu) %>% 
          summarise(rejet_glob = sum(decision == "Accept the treatment") > 0) %>% 
          pull(rejet_glob) %>% mean(),
        as.numeric(opchar_multicstt[[globH]][["carac_globales"]]["rejet_glob"]),
        as.numeric(opchar_bonfecstt[[globH]][["carac_globales"]]["rejet_glob"]),
        lapply(opchar_monocstt[[globH]], "[[", "essais") %>% 
          do.call(what = "rbind", args = .) %>% 
          group_by(n_simu) %>% 
          summarise(rejet_glob = sum(decision == "Accept the treatment") > 0) %>% 
          pull(rejet_glob) %>% mean(),
        as.numeric(opchar_holm[[globH]][["carac_globales"]]["rejet_glob"]))
    })
    ))
  rm(tab_Cnmultiarm, tab_Cnbonferr, tab_Cnmonoarm, tab_Gammultiarm, tab_Gambonferr, tab_Gammonoarm)
  
  
  # Saving raw data
  save(data_ggplot_13vsref, tab_Ralpha,
       file = paste0("Data/sensibilite/varcorr_vsref_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
  if (clear) rm(tableau_ttt)
  
} else {
  
  # Sorry, raw results aren't included
  load("Data/sensibilite/varcorr_vsref_20230621.Rdata")
  
}

tab_Ralpha <- liste_scenars %>% 
  filter(scenar == "Global H0") %>% 
  mutate(num_corr = 1:31) %>% 
  select(num_corr, R) %>% 
  right_join(tab_Ralpha)
(Graphe1 <- ggplot(tab_Ralpha, aes(R, global_alpha, color = methode)) + 
    geom_point() + 
    geom_line() +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "FWER for uncontrolled design",
         y = "FWER", x = "Correlation") +
    coord_cartesian(ylim = c(0, .65)))
tab_Ralpha %>% 
  mutate(methode = str_remove_all(methode, "<sup>|</sup>|<sub>|</sub>")) %>% 
  pivot_wider(names_from = "methode", values_from = "global_alpha") %>% 
  print(n = Inf)
(Graphe2 <- data_ggplot_13vsref %>% 
    filter(scenar == "LFC", ttt == "ttt1") %>% 
    ggplot(aes(R, rejet_h0, color = methode)) + 
    geom_point() + 
    geom_line() +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Power for uncontrolled design",
         y = "Power", x = "Correlation") +
    coord_cartesian(ylim = c(0, .9)))

## 2/ Controlled ----

alpha <- .1
n_bras <- 3
p_n <- c(.3, .3, .1, .3)
p_a <- c(.25, .5, .05, .2)
nsim_oc <- 10000
nsim_essais <- 10000
ana_inter <- rep(15, 4)
methode <- 2L

# Get all scenarios with all 31 correlations per scenario to plot them
p_ttt <- list("Global H0" = list(ttt1 = c(Peff = .6, Ptox = .4), ttt2 = c(Peff = .6, Ptox = .4), ttt3 = c(Peff = .6, Ptox = .4)),
              "LFC" = list(ttt1 = c(Peff = .75, Ptox = .3), ttt2 = c(Peff = .6, Ptox = .4), ttt3 = c(Peff = .6, Ptox = .4)))
# "2 H1, 1H0" = list(ttt1 = c(Peff = .75, Ptox = .3), ttt2 = c(Peff = .75, Ptox = .3), ttt3 = c(Peff = .6, Ptox = .4)),
# "3 H1" = list(ttt1 = c(Peff = .75, Ptox = .3), ttt2 = c(Peff = .75, Ptox = .3), ttt3 = c(Peff = .75, Ptox = .3)),
# "3 better than H1" = list(ttt1 = c(Peff = .8, Ptox = .25), ttt2 = c(Peff = .8, Ptox = .25), ttt3 = c(Peff = .8, Ptox = .25)),
# "3 not effective" = list(ttt1 = c(Peff = .75, Ptox = .4), ttt2 = c(Peff = .6, Ptox = .3), ttt3 = c(Peff = .6, Ptox = .4)))
noms_scenars <- names(p_ttt)
liste_scenars <- map2_dfr(
  p_ttt,
  noms_scenars,
  \(liste_probas, nom) {
    liste_probas <- append(liste_probas, list("ttt0" = c(Peff = .6, Ptox = .4)), 0)
    Rmins <- max(map_dbl(liste_probas, ~ calculate_corr(.x[["Peff"]], .x[["Ptox"]], max(0, .x[["Peff"]] + .x[["Ptox"]] - 1))))
    Rmaxs <- min(map_dbl(liste_probas, ~ calculate_corr(.x[["Peff"]], .x[["Ptox"]], min(.x[["Peff"]], .x[["Ptox"]]))))
    map_dfc(seq_along(liste_probas), \(proba) {
      generer_scenar_corr(liste_probas[[proba]][["Peff"]], liste_probas[[proba]][["Ptox"]], Rmins, Rmaxs, nom) %>% 
        'names<-'(paste0(names(.), "_", proba))
    })
  }
) %>% 
  mutate(p_ttt = pmap(list(ttt1 = probas_2, ttt2 = probas_3, ttt3 = probas_4), \(ttt1, ttt2, ttt3) list("ttt1" = ttt1, "ttt2" = ttt2, "ttt3" = ttt3)),
         p_ttt0 = map(probas_1, \(ttt0) list("ttt0" = ttt0))) %>% 
  select(scenar = scenar_1, R = R_1, p_ttt0, p_ttt)

# Retrieve thresholds
load("Data/simu_vscont/cutoff_13scenar_20221206.Rdata")

if (simu) {
  
  opchar_multivar <- list()
  opchar_monovar <- list()
  opchar_multicstt <- list()
  opchar_monocstt <- list()
  opchar_bonfevar <- list()
  opchar_bonfecstt <- list()
  opchar_holm <- list()
  
  # Progress bar
  with_progress({
    PBarre <- progressor(steps = nrow(liste_scenars) * 8)
    for (i in seq_len(nrow(liste_scenars))) {
      
      PBarre(amount = 0, message = liste_scenars[["scenar"]][i])
      
      tableau_ttt <- gen_patients_multinom(n_sim = nsim_essais,
                                           ana_inter = ana_inter,
                                           multinom_ttt = liste_scenars[["p_ttt"]][[i]],
                                           multinom_cont = liste_scenars[["p_ttt0"]][[i]]$ttt0,
                                           seed = 1993) %>% suppressWarnings() %>% suppressMessages()
      PBarre()
      
      opchar_multivar[[i]] <- opcharac_efftox(ana_inter = ana_inter,
                                              alpha = alpha,
                                              p_reel = liste_scenars[["p_ttt"]][[i]], p_n = p_n, p_a = p_a,
                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                              tableau_essais = tableau_ttt, delta = c(0, 0),
                                              cut_seq = cutoff_Cnmulti[[1]]["C_"], power_seq = cutoff_Cnmulti[[1]]["gamma"]) %>% suppressWarnings() %>% suppressMessages()
      PBarre()

      opchar_bonfevar[[i]] <- opcharac_efftox(ana_inter = ana_inter,
                                              alpha = alpha,
                                              p_reel = liste_scenars[["p_ttt"]][[i]], p_n = p_n, p_a = p_a,
                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                              tableau_essais = tableau_ttt, delta = c(0, 0),
                                              cut_seq = cutoff_Cnbonf[[1]]["C_"], power_seq = cutoff_Cnbonf[[1]]["gamma"]) %>% suppressWarnings() %>% suppressMessages()
      PBarre()

      opchar_monovar[[i]] <- list(ttt1 = opcharac_efftox(ana_inter = ana_inter,
                                                         alpha = alpha,
                                                         p_reel = liste_scenars[["p_ttt"]][[i]]$ttt1, p_n = p_n, p_a = p_a,
                                                         nsim_oc = nsim_oc, nsim_essai = nsim_essais, delta = c(0, 0),
                                                         tableau_essais = tableau_ttt %>% filter(ttt %in% c("ttt0", "ttt1")),
                                                         cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]) %>% suppressWarnings() %>% suppressMessages(),
                                  ttt2 = opcharac_efftox(ana_inter = ana_inter,
                                                         alpha = alpha,
                                                         p_reel = liste_scenars[["p_ttt"]][[i]]$ttt2, p_n = p_n, p_a = p_a,
                                                         nsim_oc = nsim_oc, nsim_essai = nsim_essais, delta = c(0, 0),
                                                         tableau_essais = tableau_ttt %>% filter(ttt %in% c("ttt0", "ttt2")),
                                                         cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]) %>% suppressWarnings() %>% suppressMessages(),
                                  ttt3 = opcharac_efftox(ana_inter = ana_inter,
                                                         alpha = alpha,
                                                         p_reel = liste_scenars[["p_ttt"]][[i]]$ttt3, p_n = p_n, p_a = p_a,
                                                         nsim_oc = nsim_oc, nsim_essai = nsim_essais, delta = c(0, 0),
                                                         tableau_essais = tableau_ttt %>% filter(ttt %in% c("ttt0", "ttt3")),
                                                         cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]) %>% suppressWarnings() %>% suppressMessages())
      PBarre()

      opchar_multicstt[[i]] <- opcharac_efftox_const(ana_inter = ana_inter,
                                                     alpha = alpha,
                                                     p_reel = liste_scenars[["p_ttt"]][[i]], p_n = p_n, p_a = p_a,
                                                     nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                     tableau_essais = tableau_ttt, delta = c(0, 0),
                                                     seq_eff = cutoff_Gammulti[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammulti[["seuils"]]["gamma_tox"]) %>% suppressWarnings() %>% suppressMessages()
      PBarre()
      
      opchar_monocstt[[i]] <- list(ttt1 = opcharac_efftox_const(ana_inter = ana_inter,
                                                                alpha = alpha,
                                                                p_reel = liste_scenars[["p_ttt"]][[i]]$ttt1, p_n = p_n, p_a = p_a,
                                                                nsim_oc = nsim_oc, nsim_essai = nsim_essais, delta = c(0, 0),
                                                                tableau_essais = tableau_ttt %>% filter(ttt %in% c("ttt0", "ttt1")),
                                                                seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]) %>% suppressWarnings() %>% suppressMessages(),
                                   ttt2 = opcharac_efftox_const(ana_inter = ana_inter,
                                                                alpha = alpha,
                                                                p_reel = liste_scenars[["p_ttt"]][[i]]$ttt2, p_n = p_n, p_a = p_a,
                                                                nsim_oc = nsim_oc, nsim_essai = nsim_essais, delta = c(0, 0),
                                                                tableau_essais = tableau_ttt %>% filter(ttt %in% c("ttt0", "ttt2")),
                                                                seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]) %>% suppressWarnings() %>% suppressMessages(),
                                   ttt3 = opcharac_efftox_const(ana_inter = ana_inter,
                                                                alpha = alpha,
                                                                p_reel = liste_scenars[["p_ttt"]][[i]]$ttt3, p_n = p_n, p_a = p_a,
                                                                nsim_oc = nsim_oc, nsim_essai = nsim_essais, delta = c(0, 0),
                                                                tableau_essais = tableau_ttt %>% filter(ttt %in% c("ttt0", "ttt3")),
                                                                seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]) %>% suppressWarnings() %>% suppressMessages())
      PBarre()
      
      opchar_bonfecstt[[i]] <- opcharac_efftox_const(ana_inter = ana_inter,
                                                     alpha = alpha,
                                                     p_reel = liste_scenars[["p_ttt"]][[i]], p_n = p_n, p_a = p_a,
                                                     nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                     tableau_essais = tableau_ttt, delta = c(0, 0),
                                                     seq_eff = cutoff_Gambonf[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gambonf[["seuils"]]["gamma_tox"]) %>% suppressWarnings() %>% suppressMessages()
      PBarre()

      opchar_holm[[i]] <- opcharac_efftox_holm2(ana_inter = ana_inter,
                                                alpha = alpha,
                                                p_reel = liste_scenars[["p_ttt"]][[i]], p_n = p_n, p_a = p_a,
                                                nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                tableau_essais = tableau_ttt, delta = c(0, 0),
                                                cut_seq_mono = cutoff_Cnholm[[1]][["C_mono"]], power_seq_mono = cutoff_Cnholm[[1]][["gamma_mono"]],
                                                cut_seq_holm = cutoff_Cnholm[[1]][["C_holm"]], power_seq_holm = cutoff_Cnholm[[1]][["gamma_holm"]]) %>% suppressWarnings() %>% suppressMessages()
      PBarre()
    }
  })
  
  # Formatting data to avoid saving overheavy object
  tab_Cnmultiarm <- lapply(seq_along(opchar_multivar), function(x) {cbind(scenar = x, opchar_multivar[[x]]$carac_bras)}) %>%
    do.call(what = "rbind", args = .) %>% 
    mutate(methode = "C<sub>n</sub><sup>m</sup>")
  tab_Cnbonferr <- lapply(seq_along(opchar_bonfevar), function(x) {cbind(scenar = x, opchar_bonfevar[[x]]$carac_bras)}) %>%
    do.call(what = "rbind", args = .) %>% 
    mutate(methode = "C<sub>n</sub><sup>Bm</sup>")
  tab_Cnmonoarm <- lapply(seq_along(opchar_monovar), function(x) {cbind(scenar = x, do.call("rbind", lapply(seq_along(opchar_monovar[[x]]), function(index) {
    data.frame(ttt = names(opchar_monovar[[x]][index]),
               rejet_h0 = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["rejet_h0"]),
               arret_precoce = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce"]),
               arret_precoce_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_fut"]),
               arret_precoce_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_precoce_tox"]),
               arret_fut = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_fut"]),
               arret_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["arret_tox"]),
               tot_pat = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_pts"]),
               tot_eff = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_eff"]),
               tot_tox = as.numeric(opchar_monovar[[x]][[index]]$caracteristique["nb_tox"]))
  })))}) %>% 
    do.call(what = "rbind", args = .) %>% 
    mutate(methode = "C<sub>n</sub><sup>s</sup>",
           arret = 1 - rejet_h0)
  tab_Gammultiarm <- lapply(seq_along(opchar_multicstt), function(x) {cbind(scenar = x, opchar_multicstt[[x]]$carac_bras)}) %>%
    do.call(what = "rbind", args = .) %>% 
    mutate(methode = "&epsilon;<sup>m</sup>")
  tab_Gambonferr <- lapply(seq_along(opchar_bonfecstt), function(x) {cbind(scenar = x, opchar_bonfecstt[[x]]$carac_bras)}) %>%
    do.call(what = "rbind", args = .) %>% 
    mutate(methode = "&epsilon;<sup>Bm</sup>")
  tab_Gammonoarm <- lapply(seq_along(opchar_monocstt), function(x) {cbind(scenar = x, do.call("rbind", lapply(seq_along(opchar_monocstt[[x]]), function(index) {
    data.frame(ttt = names(opchar_monocstt[[x]][index]),
               rejet_h0 = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["rejet_h0"]),
               arret_precoce = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["arret_precoce"]),
               arret_precoce_fut = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["arret_precoce_fut"]),
               arret_precoce_tox = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["arret_precoce_tox"]),
               arret_fut = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["arret_fut"]),
               arret_tox = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["arret_tox"]),
               tot_pat = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["nb_pts"]),
               tot_eff = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["nb_eff"]),
               tot_tox = as.numeric(opchar_monocstt[[x]][[index]]$caracteristique["nb_tox"]))
  })))}) %>% 
    do.call(what = "rbind", args = .) %>% 
    mutate(methode = "&epsilon;<sup>s</sup>",
           arret = 1 - rejet_h0)
  tab_Cnholm <- lapply(seq_along(opchar_holm), function(x) {cbind(scenar = x, opchar_holm[[x]]$carac_bras)}) %>%
    do.call(what = "rbind", args = .) %>% 
    mutate(methode = "C<sub>n</sub><sup>m,a</sup>")
  data_ggplot_13vscont <- rbind(tab_Cnmultiarm, tab_Cnbonferr, tab_Cnmonoarm, tab_Gammultiarm, tab_Gambonferr, tab_Gammonoarm, tab_Cnholm)
  data_ggplot_13vscont <- left_join(data_ggplot_13vscont %>% 
                                      rename(num_corr = scenar) %>% 
                                      mutate(clef_merge = rep(rep(1:62, each = 3), 7)),
                                    liste_scenars %>% 
                                      mutate(clef_merge = 1:62) %>% 
                                      select(-p_ttt0, -p_ttt))
  tab_Ralpha <- data.frame(
    methode = rep(c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>Bm</sup>", "C<sub>n</sub><sup>s</sup>", "&epsilon;<sup>m</sup>", "&epsilon;<sup>Bm</sup>", "&epsilon;<sup>s</sup>", "C<sub>n</sub><sup>m,a</sup>"), times = 31),
    num_corr = rep(1:31, each = 7),
    global_alpha = unlist(map(seq(1, 31, 1), \(globH) {
      c(as.numeric(opchar_multivar[[globH]][["carac_globales"]]["rejet_glob"]),
        as.numeric(opchar_bonfevar[[globH]][["carac_globales"]]["rejet_glob"]),
        lapply(opchar_monovar[[globH]], "[[", "essais") %>% 
          do.call(what = "rbind", args = .) %>% 
          group_by(n_simu) %>% 
          summarise(rejet_glob = sum(decision == "Accept the treatment") > 0) %>% 
          pull(rejet_glob) %>% mean(),
        as.numeric(opchar_multicstt[[globH]][["carac_globales"]]["rejet_glob"]),
        as.numeric(opchar_bonfecstt[[globH]][["carac_globales"]]["rejet_glob"]),
        lapply(opchar_monocstt[[globH]], "[[", "essais") %>% 
          do.call(what = "rbind", args = .) %>% 
          group_by(n_simu) %>% 
          summarise(rejet_glob = sum(decision == "Accept the treatment") > 0) %>% 
          pull(rejet_glob) %>% mean(),
        as.numeric(opchar_holm[[globH]][["carac_globales"]]["rejet_glob"]))
    })
    ))
  rm(tab_Cnmultiarm, tab_Cnbonferr, tab_Cnmonoarm, tab_Gammultiarm, tab_Gambonferr, tab_Gammonoarm)
  
  
  # Saving raw data
  save(data_ggplot_13vscont, tab_Ralpha,
       file = paste0("Data/sensibilite/varcorr_vscont_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
  if (clear) rm(tableau_ttt)
  
} else {
  
  # Sorry, raw results aren't included
  load("Data/sensibilite/varcorr_vscont_20230621.Rdata")
  
}


tab_Ralpha <- liste_scenars %>% 
  filter(scenar == "Global H0") %>% 
  mutate(num_corr = 1:31) %>% 
  select(num_corr, R) %>% 
  right_join(tab_Ralpha)
(Graphe3 <- ggplot(tab_Ralpha, aes(R, global_alpha, color = methode)) + 
  geom_point() + 
  geom_line() +
    scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "FWER for controlled design",
       y = "FWER", x = "Correlation") +
    coord_cartesian(ylim = c(0, .65)))
tab_Ralpha %>% 
  mutate(methode = str_remove_all(methode, "<sup>|</sup>|<sub>|</sub>")) %>% 
  pivot_wider(names_from = "methode", values_from = "global_alpha") %>% 
  print(n = Inf)
(Graphe4 <- data_ggplot_13vscont %>% 
  filter(scenar == "LFC", ttt == "ttt1") %>% 
  ggplot(aes(R, rejet_h0, color = methode)) + 
  geom_point() + 
  geom_line() +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Power for controlled design",
         y = "Power", x = "Correlation") +
    coord_cartesian(ylim = c(0, .9)))

GrapheTot <- ((Graphe1 / Graphe2) | (Graphe3 / Graphe4)) + plot_layout(guides = "collect")
ggsave(GrapheTot, filename = "Outputs/sensibilite/correlation.png", device = "png", height = 10, width = 12)
