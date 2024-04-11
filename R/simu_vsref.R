# ---------------------------------------------------------------------------- #
# Simulations, storing and formatting of the results for uncontrolled settings #
# Créé le 26/04/2021, modifié le 04/12/2022                                    #
# ---------------------------------------------------------------------------- #

# Packages and helpers ----

# devtools::load_all("E:/multibrasBOP2/")
library(multibrasBOP2)
library(tidyverse)

simu <- TRUE # TRUE if you want to simulate the data
simu_seuils <- FALSE # TRUE if you want to simulate the thresholds
clear <- TRUE # TRUE to clear environment often

# I/ Simulation parameters ----

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


# II/ The 13 scenarios vs uncontrolled setting ----

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
       file = paste0("Data/simu_vsref/cutoff_13scenar_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
} else {
  
  # Else load the stored thresholds
  load("Data/simu_vsref/cutoff_13scenar_20221202.Rdata")
  
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
       file = paste0("Data/simu_vsref/resultats_bruts_13scenar_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
  if (clear) rm(tableau_ttt)
  
} else {
  
  # Sorry, raw results aren't included
  load("Data/simu_vsref/resultats_bruts_13scenar_20221205.Rdata")
  
}


## B/ Formatting the data ----

### Giant table with all the results

tab_Cnmulti <- lapply(seq_along(opchar_multivar), function(x) {cbind(scenar = names(opchar_multivar)[x], opchar_multivar[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Cnbonf <- lapply(seq_along(opchar_bonfevar), function(x) {cbind(scenar = names(opchar_bonfevar)[x], opchar_bonfevar[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Cnmono <- lapply(seq_along(opchar_monovar), function(x) {cbind(scenar = names(opchar_monovar)[x], do.call("rbind", lapply(opchar_monovar[[x]], "[[", "essais")))}) %>% 
  do.call(what = "rbind", args = .)

tab_Gammulti <- lapply(seq_along(opchar_multicstt), function(x) {cbind(scenar = names(opchar_multicstt)[x], opchar_multicstt[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Gambonf <- lapply(seq_along(opchar_bonfecstt), function(x) {cbind(scenar = names(opchar_bonfecstt)[x], opchar_bonfecstt[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Gammono <- lapply(seq_along(opchar_monocstt), function(x) {cbind(scenar = names(opchar_monocstt)[x], do.call("rbind", lapply(opchar_monocstt[[x]], "[[", "essais")))}) %>% 
  do.call(what = "rbind", args = .)

tab_Cnholm <- lapply(seq_along(opchar_holm), function(x) {cbind(scenar = names(opchar_holm)[x], opchar_holm[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

save(tab_Cnmulti, tab_Cnbonf, tab_Cnmono, tab_Gammulti, tab_Gambonf, tab_Gammono, tab_Cnholm,
     file = paste0("Data/simu_vsref/essais_bruts_bonfgam_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))

if (clear) rm(tab_Cnmulti, tab_Cnbonf, tab_Cnmono, tab_Gammulti, tab_Gambonf, tab_Gammono, tab_Cnholm)

### Arm by arm results

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

### FWER

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
                   as.numeric(opchar_bonfecstt[["Global H0"]][["carac_globales"]]["rejet_glob"]),
                   lapply(opchar_monocstt[["Global H0"]], "[[", "essais") %>% 
                     do.call(what = "rbind", args = .) %>% 
                     group_by(n_simu) %>% 
                     summarise(rejet_glob = sum(decision == "Accept the treatment") > 0) %>% 
                     pull(rejet_glob) %>% mean(),
                   as.numeric(opchar_holm[["Global H0"]][["carac_globales"]]["rejet_glob"]))
)

rm(tab_Cnmultiarm, tab_Cnbonferr, tab_Cnmonoarm, tab_Gammultiarm, tab_Gambonferr, tab_Gammonoarm)

save(data_ggplot_13vsref, tab_Ralpha, file = paste0("Data/simu_vsref/donnees_graphes_ref_", format(Sys.Date(), "%Y%m%d"), ".Rdata")) 

if (clear) rm(list = ls()[str_detect(ls(), "^cutoff|opchar|data")])
if (clear) rm(tab_Ralpha)


# III/ Different recruiting rythms ----

## A/ Additional simulation parameters ----

p_ttt <- list(ttt1 = p_n, ttt2 = p_a, ttt3 = c(.15, .35, .1, .4))
p_ttt_h0 <- list(ttt1 = p_n, ttt2 = p_n, ttt3 = p_n)

# Variation in recruitment rythms
tab_ana <- tibble(puissance = seq(0.25, 1.75, 0.025), 
                  N = 60) %>% 
  mutate(ana_inter_cum = map2(N, puissance, ~ round((.x * (c(1:4) / 4) ^ .y)))) %>%
  mutate(ana_inter = map(ana_inter_cum, ~ c(.x[1], diff(.x))))

## B/ Simulations ----

# Thresholds are the same as in II/
load("Data/simu_vsref/cutoff_13scenar_20221202.Rdata")

# Simulate the trials
if (simu) {
  
  # Simulate data 1 by 1 to ensure having the same patients despite different recruiting rythms
  tableau_ref <- gen_patients_multinom(n_sim = nsim_essais, 
                                       ana_inter = rep(1, 60),
                                       multinom_ttt = p_ttt,
                                       seed = 1993)
  tableau_nul <- gen_patients_multinom(n_sim = nsim_essais, 
                                       ana_inter = rep(1, 60),
                                       multinom_ttt = p_ttt_h0,
                                       seed = 1993)
  
  cat("Operating characteristics for the different scenarios:\n")
  opchar_multivar     <- list()
  opchar_monovar      <- list()
  opchar_multicstt    <- list()
  opchar_monocstt     <- list()
  opchar_bonfevar     <- list()
  opchar_bonfecstt    <- list()
  opchar_multivar_h0  <- list()
  opchar_monovar_h0   <- list()
  opchar_multicstt_h0 <- list()
  opchar_monocstt_h0  <- list()
  opchar_bonfevar_h0  <- list()
  opchar_bonfecstt_h0 <- list()
  opchar_holm <- list()
  opchar_holm_h0 <- list()
  for (i in seq_len(nrow(tab_ana))) {
    
    cat(paste0("Simulation n°", i, " / ", nrow(tab_ana), ".\n"))
    
    cat("Splice the data into interim analyses.\n")
    tableau_ttt <- modif_tab_patients(tableau_ref, tab_ana$ana_inter[[i]])
    tableau_ttt_h0 <- modif_tab_patients(tableau_nul, tab_ana$ana_inter[[i]])
    
    cat("Varying threshold, multiarm.\n")
    opchar_multivar[[i]] <- opcharac_efftox(ana_inter = tab_ana$ana_inter[[i]],
                                            alpha = alpha,
                                            p_reel = p_ttt, p_n = p_n, p_a = p_a,
                                            nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                            tableau_essais = tableau_ttt,
                                            cut_seq = cutoff_Cnmulti[[1]]["C_"], power_seq = cutoff_Cnmulti[[1]]["gamma"])
    
    cat("Variable threshold, Holm.\n")
    opchar_holm[[i]] <- opcharac_efftox_holm2(ana_inter = tab_ana$ana_inter[[i]],
                                              alpha = alpha,
                                              p_reel = p_ttt, p_n = p_n, p_a = p_a,
                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                              tableau_essais = tableau_ttt,
                                              cut_seq_mono = cutoff_Cnholm[[1]][["C_mono"]], power_seq_mono = cutoff_Cnholm[[1]][["gamma_mono"]],
                                              cut_seq_holm = cutoff_Cnholm[[1]][["C_holm"]], power_seq_holm = cutoff_Cnholm[[1]][["gamma_holm"]])
    
    cat("Varying threshold, Bonferroni.\n")
    opchar_bonfevar[[i]] <- opcharac_efftox(ana_inter = tab_ana$ana_inter[[i]],
                                            alpha = alpha,
                                            p_reel = p_ttt, p_n = p_n, p_a = p_a,
                                            nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                            tableau_essais = tableau_ttt,
                                            cut_seq = cutoff_Cnbonf[[1]]["C_"], power_seq = cutoff_Cnbonf[[1]]["gamma"])
    
    cat("Varying threshold, monoarm.\n")
    opchar_monovar[[i]] <- list(ttt1 = opcharac_efftox(ana_inter = tab_ana$ana_inter[[i]],
                                                       alpha = alpha,
                                                       p_reel = p_ttt$ttt1, p_n = p_n, p_a = p_a,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt1"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt2 = opcharac_efftox(ana_inter = tab_ana$ana_inter[[i]],
                                                       alpha = alpha,
                                                       p_reel = p_ttt$ttt2, p_n = p_n, p_a = p_a,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt2"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt3 = opcharac_efftox(ana_inter = tab_ana$ana_inter[[i]],
                                                       alpha = alpha,
                                                       p_reel = p_ttt$ttt3, p_n = p_n, p_a = p_a,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt3"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]))
    
    cat("Constant threshold, multiarm.\n")
    opchar_multicstt[[i]] <- opcharac_efftox_const(ana_inter = tab_ana$ana_inter[[i]],
                                                   alpha = alpha,
                                                   p_reel = p_ttt, p_n = p_n, p_a = p_a,
                                                   nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                   tableau_essais = tableau_ttt,
                                                   seq_eff = cutoff_Gammulti[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammulti[["seuils"]]["gamma_tox"])
    
    cat("Constant threshold, monoarm.\n")
    opchar_monocstt[[i]] <- list(ttt1 = opcharac_efftox_const(ana_inter = tab_ana$ana_inter[[i]],
                                                              alpha = alpha,
                                                              p_reel = p_ttt$ttt1, p_n = p_n, p_a = p_a,
                                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                              tableau_essais = tableau_ttt %>% filter(ttt == "ttt1"),
                                                              seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]),
                                 ttt2 = opcharac_efftox_const(ana_inter = tab_ana$ana_inter[[i]],
                                                              alpha = alpha,
                                                              p_reel = p_ttt$ttt2, p_n = p_n, p_a = p_a,
                                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                              tableau_essais = tableau_ttt %>% filter(ttt == "ttt2"),
                                                              seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]),
                                 ttt3 = opcharac_efftox_const(ana_inter = tab_ana$ana_inter[[i]],
                                                              alpha = alpha,
                                                              p_reel = p_ttt$ttt3, p_n = p_n, p_a = p_a,
                                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                              tableau_essais = tableau_ttt %>% filter(ttt == "ttt3"),
                                                              seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]))
    
    cat("Constant threshold, Bonferroni\n")
    opchar_bonfecstt[[i]] <- opcharac_efftox_const(ana_inter = tab_ana$ana_inter[[i]],
                                                   alpha = alpha,
                                                   p_reel = p_ttt, p_n = p_n, p_a = p_a,
                                                   nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                   tableau_essais = tableau_ttt,
                                                   seq_eff = cutoff_Gambonf[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gambonf[["seuils"]]["gamma_tox"])
    
    cat("Varying threshold, multiarm (H0).\n")
    opchar_multivar_h0[[i]] <- opcharac_efftox(ana_inter = tab_ana$ana_inter[[i]],
                                               alpha = alpha,
                                               p_reel = p_ttt_h0, p_n = p_n, p_a = p_a,
                                               nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                               tableau_essais = tableau_ttt_h0,
                                               cut_seq = cutoff_Cnmulti[[1]]["C_"], power_seq = cutoff_Cnmulti[[1]]["gamma"])
    
    cat("Varying threshold, Holm (H0).\n")
    opchar_holm_h0[[i]] <- opcharac_efftox_holm2(ana_inter = tab_ana$ana_inter[[i]],
                                                 alpha = alpha,
                                                 p_reel = p_ttt_h0, p_n = p_n, p_a = p_a,
                                                 nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                 tableau_essais = tableau_ttt_h0,
                                                 cut_seq_mono = cutoff_Cnholm[[1]][["C_mono"]], power_seq_mono = cutoff_Cnholm[[1]][["gamma_mono"]],
                                                 cut_seq_holm = cutoff_Cnholm[[1]][["C_holm"]], power_seq_holm = cutoff_Cnholm[[1]][["gamma_holm"]])
    
    cat("Varying threshold, Bonferroni (H0).\n")
    opchar_bonfevar_h0[[i]] <- opcharac_efftox(ana_inter = tab_ana$ana_inter[[i]],
                                               alpha = alpha,
                                               p_reel = p_ttt_h0, p_n = p_n, p_a = p_a,
                                               nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                               tableau_essais = tableau_ttt_h0,
                                               cut_seq = cutoff_Cnbonf[[1]]["C_"], power_seq = cutoff_Cnbonf[[1]]["gamma"])
    
    cat("Varying threshold, monoarm (H0).\n")
    opchar_monovar_h0[[i]] <- list(ttt1 = opcharac_efftox(ana_inter = tab_ana$ana_inter[[i]],
                                                          alpha = alpha,
                                                          p_reel = p_ttt_h0$ttt1, p_n = p_n, p_a = p_a,
                                                          nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                          tableau_essais = tableau_ttt_h0 %>% filter(ttt == "ttt1"),
                                                          cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                   ttt2 = opcharac_efftox(ana_inter = tab_ana$ana_inter[[i]],
                                                          alpha = alpha,
                                                          p_reel = p_ttt_h0$ttt2, p_n = p_n, p_a = p_a,
                                                          nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                          tableau_essais = tableau_ttt_h0 %>% filter(ttt == "ttt2"),
                                                          cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                   ttt3 = opcharac_efftox(ana_inter = tab_ana$ana_inter[[i]],
                                                          alpha = alpha,
                                                          p_reel = p_ttt_h0$ttt3, p_n = p_n, p_a = p_a,
                                                          nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                          tableau_essais = tableau_ttt_h0 %>% filter(ttt == "ttt3"),
                                                          cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]))
    
    cat("Constant threshold, multiarm (H0).\n")
    opchar_multicstt_h0[[i]] <- opcharac_efftox_const(ana_inter = tab_ana$ana_inter[[i]],
                                                      alpha = alpha,
                                                      p_reel = p_ttt_h0, p_n = p_n, p_a = p_a,
                                                      nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                      tableau_essais = tableau_ttt_h0,
                                                      seq_eff = cutoff_Gammulti[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammulti[["seuils"]]["gamma_tox"])
    
    cat("Constant threshold, monoarm (H0).\n")
    opchar_monocstt_h0[[i]] <- list(ttt1 = opcharac_efftox_const(ana_inter = tab_ana$ana_inter[[i]],
                                                                 alpha = alpha,
                                                                 p_reel = p_ttt_h0$ttt1, p_n = p_n, p_a = p_a,
                                                                 nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                                 tableau_essais = tableau_ttt_h0 %>% filter(ttt == "ttt1"),
                                                                 seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]),
                                    ttt2 = opcharac_efftox_const(ana_inter = tab_ana$ana_inter[[i]],
                                                                 alpha = alpha,
                                                                 p_reel = p_ttt_h0$ttt2, p_n = p_n, p_a = p_a,
                                                                 nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                                 tableau_essais = tableau_ttt_h0 %>% filter(ttt == "ttt2"),
                                                                 seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]),
                                    ttt3 = opcharac_efftox_const(ana_inter = tab_ana$ana_inter[[i]],
                                                                 alpha = alpha,
                                                                 p_reel = p_ttt_h0$ttt3, p_n = p_n, p_a = p_a,
                                                                 nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                                 tableau_essais = tableau_ttt_h0 %>% filter(ttt == "ttt3"),
                                                                 seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]))
    
    cat("Constant threshold, Bonferroni (H0).\n")
    opchar_bonfecstt_h0[[i]] <- opcharac_efftox_const(ana_inter = tab_ana$ana_inter[[i]],
                                                      alpha = alpha,
                                                      p_reel = p_ttt_h0, p_n = p_n, p_a = p_a,
                                                      nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                      tableau_essais = tableau_ttt_h0,
                                                      seq_eff = cutoff_Gambonf[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gambonf[["seuils"]]["gamma_tox"])
    
  }
  
  if (clear) rm(tableau_ttt, tableau_ttt_h0)
  
  save(opchar_multivar, opchar_monovar, opchar_multicstt, opchar_monocstt, opchar_bonfevar, opchar_bonfecstt, 
       opchar_holm, opchar_holm_h0,
       opchar_multivar_h0, opchar_monovar_h0, opchar_multicstt_h0, opchar_monocstt_h0, opchar_bonfevar_h0, opchar_bonfecstt_h0,
       file = paste0("Data/simu_vsref/resultats_bruts_12scenar_chgtrecrut_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
} else {
  
  # Sorry, raw results aren't included
  load("Data/simu_vsref/resultats_bruts_12scenar_chgtrecrut_20221205.Rdata")
  
}

## C/ Formatting ----

### Giant table with all the trials

puissance <- seq(0.25, 1.75, 0.025)

tab_Cnmulti <- lapply(seq_along(opchar_multivar), function(x) {cbind(exposant = puissance[[x]], opchar_multivar[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Cnholm <- lapply(seq_along(opchar_holm), function(x) {cbind(exposant = puissance[[x]], opchar_holm[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Cnbonf <- lapply(seq_along(opchar_bonfevar), function(x) {cbind(exposant = puissance[[x]], opchar_bonfevar[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Cnmono <- lapply(seq_along(opchar_monovar), function(x) {cbind(exposant = puissance[[x]], do.call("rbind", lapply(opchar_monovar[[x]], "[[", "essais")))}) %>% 
  do.call(what = "rbind", args = .)

tab_Gammulti <- lapply(seq_along(opchar_multicstt), function(x) {cbind(exposant = puissance[[x]], opchar_multicstt[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Gambonf <- lapply(seq_along(opchar_bonfecstt), function(x) {cbind(exposant = puissance[[x]], opchar_bonfecstt[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Gammono <- lapply(seq_along(opchar_monocstt), function(x) {cbind(exposant = puissance[[x]], do.call("rbind", lapply(opchar_monocstt[[x]], "[[", "essais")))}) %>% 
  do.call(what = "rbind", args = .)

tab_Cnmulti_h0 <- lapply(seq_along(opchar_multivar_h0), function(x) {cbind(exposant = puissance[[x]], opchar_multivar_h0[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Cnholm_h0 <- lapply(seq_along(opchar_holm_h0), function(x) {cbind(exposant = puissance[[x]], opchar_holm_h0[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Cnbonf_h0 <- lapply(seq_along(opchar_bonfevar_h0), function(x) {cbind(exposant = puissance[[x]], opchar_bonfevar_h0[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Cnmono_h0 <- lapply(seq_along(opchar_monovar_h0), function(x) {cbind(exposant = puissance[[x]], do.call("rbind", lapply(opchar_monovar_h0[[x]], "[[", "essais")))}) %>% 
  do.call(what = "rbind", args = .)

tab_Gammulti_h0 <- lapply(seq_along(opchar_multicstt_h0), function(x) {cbind(exposant = puissance[[x]], opchar_multicstt_h0[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Gambonf_h0 <- lapply(seq_along(opchar_bonfecstt_h0), function(x) {cbind(exposant = puissance[[x]], opchar_bonfecstt_h0[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Gammono_h0 <- lapply(seq_along(opchar_monocstt_h0), function(x) {cbind(exposant = puissance[[x]], do.call("rbind", lapply(opchar_monocstt_h0[[x]], "[[", "essais")))}) %>% 
  do.call(what = "rbind", args = .)

save(tab_Cnmulti, tab_Cnholm, tab_Cnbonf, tab_Cnmono, tab_Gammulti, tab_Gambonf, tab_Gammono, tab_Cnmulti_h0, tab_Cnholm_h0, tab_Cnbonf_h0, tab_Cnmono_h0, tab_Gammulti_h0, tab_Gambonf_h0, tab_Gammono_h0,
     file = paste0("Data/simu_vsref/essais_bruts_bonfgam_chgtrecrut_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))

if (clear) rm(tab_Cnmulti, tab_Cnbonf, tab_Cnmono, tab_Gammulti, tab_Gambonf, tab_Gammono, tab_Cnholm,
              tab_Cnmulti_h0, tab_Cnbonf_h0, tab_Cnmono_h0, tab_Gammulti_h0, tab_Gambonf_h0, tab_Gammono_h0, tab_Cnholm_h0)

### Results by arm

# Not H0
tab_Cnmultiarm <- lapply(seq_along(opchar_multivar), function(x) {cbind(exposant = puissance[[x]], opchar_multivar[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Multiarm C<sub>n</sub>")

tab_Cnholm <- lapply(seq_along(opchar_holm), function(x) {cbind(exposant = puissance[[x]], opchar_holm[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Holm C<sub>n</sub>")

tab_Cnbonferr <- lapply(seq_along(opchar_bonfevar), function(x) {cbind(exposant = puissance[[x]], opchar_bonfevar[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Bonferroni C<sub>n</sub>")

tab_Cnmonoarm <- lapply(seq_along(opchar_monovar), function(x) {cbind(exposant = puissance[[x]], do.call("rbind", lapply(seq_along(opchar_monovar[[x]]), function(index) {
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

tab_Gammultiarm <- lapply(seq_along(opchar_multicstt), function(x) {cbind(exposant = puissance[[x]], opchar_multicstt[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Multiarm &epsilon;")

tab_Gambonferr <- lapply(seq_along(opchar_bonfecstt), function(x) {cbind(exposant = puissance[[x]], opchar_bonfecstt[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Bonferroni &epsilon;")

tab_Gammonoarm <- lapply(seq_along(opchar_monocstt), function(x) {cbind(exposant = puissance[[x]], do.call("rbind", lapply(seq_along(opchar_monocstt[[x]]), function(index) {
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

data_ggplot_recrutvsref <- rbind(tab_Cnmultiarm, tab_Cnbonferr, tab_Cnmonoarm, tab_Gammultiarm, tab_Gambonferr, tab_Gammonoarm, tab_Cnholm)

rm(tab_Cnmultiarm, tab_Cnbonferr, tab_Cnmonoarm, tab_Gammultiarm, tab_Gambonferr, tab_Gammonoarm, tab_Cnholm)

# Global H0
tab_Cnmultiarm_h0 <- lapply(seq_along(opchar_multivar_h0), function(x) {cbind(exposant = puissance[[x]], opchar_multivar_h0[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Multiarm C<sub>n</sub>")

tab_Cnholm_h0 <- lapply(seq_along(opchar_holm_h0), function(x) {cbind(exposant = puissance[[x]], opchar_holm_h0[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Holm C<sub>n</sub>")

tab_Cnbonferr_h0 <- lapply(seq_along(opchar_bonfevar_h0), function(x) {cbind(exposant = puissance[[x]], opchar_bonfevar_h0[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Bonferroni C<sub>n</sub>")

tab_Cnmonoarm_h0 <- lapply(seq_along(opchar_monovar_h0), function(x) {cbind(exposant = puissance[[x]], do.call("rbind", lapply(seq_along(opchar_monovar_h0[[x]]), function(index) {
  data.frame(ttt = names(opchar_monovar_h0[[x]][index]),
             rejet_h0 = as.numeric(opchar_monovar_h0[[x]][[index]]$caracteristique["rejet_h0"]),
             arret_precoce = as.numeric(opchar_monovar_h0[[x]][[index]]$caracteristique["arret_precoce"]),
             arret_precoce_fut = as.numeric(opchar_monovar_h0[[x]][[index]]$caracteristique["arret_precoce_fut"]),
             arret_precoce_tox = as.numeric(opchar_monovar_h0[[x]][[index]]$caracteristique["arret_precoce_tox"]),
             arret_fut = as.numeric(opchar_monovar_h0[[x]][[index]]$caracteristique["arret_fut"]),
             arret_tox = as.numeric(opchar_monovar_h0[[x]][[index]]$caracteristique["arret_tox"]),
             tot_pat = as.numeric(opchar_monovar_h0[[x]][[index]]$caracteristique["nb_pts"]),
             tot_eff = as.numeric(opchar_monovar_h0[[x]][[index]]$caracteristique["nb_eff"]),
             tot_tox = as.numeric(opchar_monovar_h0[[x]][[index]]$caracteristique["nb_tox"]))
})))}) %>% 
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Monoarm C<sub>n</sub>",
         arret = 1 - rejet_h0)

tab_Gammultiarm_h0 <- lapply(seq_along(opchar_multicstt_h0), function(x) {cbind(exposant = puissance[[x]], opchar_multicstt_h0[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Multiarm &epsilon;")

tab_Gambonferr_h0 <- lapply(seq_along(opchar_bonfecstt_h0), function(x) {cbind(exposant = puissance[[x]], opchar_bonfecstt_h0[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Bonferroni &epsilon;")

tab_Gammonoarm_h0 <- lapply(seq_along(opchar_monocstt_h0), function(x) {cbind(exposant = puissance[[x]], do.call("rbind", lapply(seq_along(opchar_monocstt_h0[[x]]), function(index) {
  data.frame(ttt = names(opchar_monocstt_h0[[x]][index]),
             rejet_h0 = as.numeric(opchar_monocstt_h0[[x]][[index]]$caracteristique["rejet_h0"]),
             arret_precoce = as.numeric(opchar_monocstt_h0[[x]][[index]]$caracteristique["arret_precoce"]),
             arret_precoce_fut = as.numeric(opchar_monocstt_h0[[x]][[index]]$caracteristique["arret_precoce_fut"]),
             arret_precoce_tox = as.numeric(opchar_monocstt_h0[[x]][[index]]$caracteristique["arret_precoce_tox"]),
             arret_fut = as.numeric(opchar_monocstt_h0[[x]][[index]]$caracteristique["arret_fut"]),
             arret_tox = as.numeric(opchar_monocstt_h0[[x]][[index]]$caracteristique["arret_tox"]),
             tot_pat = as.numeric(opchar_monocstt_h0[[x]][[index]]$caracteristique["nb_pts"]),
             tot_eff = as.numeric(opchar_monocstt_h0[[x]][[index]]$caracteristique["nb_eff"]),
             tot_tox = as.numeric(opchar_monocstt_h0[[x]][[index]]$caracteristique["nb_tox"]))
})))}) %>% 
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Monoarm &epsilon;",
         arret = 1 - rejet_h0)

data_ggplot_recrutvsref_h0 <- rbind(tab_Cnmultiarm_h0, tab_Cnbonferr_h0, tab_Cnmonoarm_h0, tab_Gammultiarm_h0, tab_Gambonferr_h0, tab_Gammonoarm_h0, tab_Cnholm_h0)

rm(tab_Cnmultiarm_h0, tab_Cnbonferr_h0, tab_Cnmonoarm_h0, tab_Gammultiarm_h0, tab_Gambonferr_h0, tab_Gammonoarm_h0, tab_Cnholm_h0)

save(data_ggplot_recrutvsref, data_ggplot_recrutvsref_h0, file = paste0("Data/simu_vsref/donnees_graphes_ref_recrut_", format(Sys.Date(), "%Y%m%d"), ".Rdata")) 

if (clear) rm(list = ls()[!ls() %in% c("simu", "simu_seuils", "clear", "ana_inter", "alpha", "n_bras", "p_n", "p_a",
                                       "nsim_oc", "nsim_essais", "methode", "p_ttt", "p_ttt_h0")])


# IV/ Different number of patients ----

## A/ Additional simulation parameters ----

ana_inter <- lapply(5:25, function(x) rep(x, 4))

## B/ Simulations ----

# Still same thresholds as in II/
load("Data/simu_vsref/cutoff_13scenar_20221202.Rdata")

if (simu) {
  
  cat("Operating characteristics for the differents scenarios:\n")
  opchar_multivar     <- list()
  opchar_monovar      <- list()
  opchar_multicstt    <- list()
  opchar_monocstt     <- list()
  opchar_bonfevar     <- list()
  opchar_bonfecstt    <- list()
  opchar_holm <- list()
  opchar_multivar_h0  <- list()
  opchar_monovar_h0   <- list()
  opchar_multicstt_h0 <- list()
  opchar_monocstt_h0  <- list()
  opchar_bonfevar_h0  <- list()
  opchar_bonfecstt_h0 <- list()
  opchar_holm_h0 <- list()
  for (i in seq_along(ana_inter)) {
    
    cat(paste0("Simulation '", i, " / ", length(ana_inter), ".\n"))
    
    cat("Generating data.\n")
    tableau_ttt <- gen_patients_multinom(n_sim = nsim_essais, 
                                         ana_inter = ana_inter[[i]],
                                         multinom_ttt = p_ttt,
                                         seed = 1993)
    tableau_ttt_h0 <- gen_patients_multinom(n_sim = nsim_essais, 
                                            ana_inter = ana_inter[[i]],
                                            multinom_ttt = p_ttt_h0,
                                            seed = 1993)
    
    cat("Varying threshold, multiarm.\n")
    opchar_multivar[[i]] <- opcharac_efftox(ana_inter = ana_inter[[i]],
                                            alpha = alpha,
                                            p_reel = p_ttt, p_n = p_n, p_a = p_a,
                                            nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                            tableau_essais = tableau_ttt,
                                            cut_seq = cutoff_Cnmulti[[1]]["C_"], power_seq = cutoff_Cnmulti[[1]]["gamma"])
    
    cat("Varying threshold, Holm.\n")
    opchar_holm[[i]] <- opcharac_efftox_holm2(ana_inter = ana_inter[[i]],
                                              alpha = alpha,
                                              p_reel = p_ttt, p_n = p_n, p_a = p_a,
                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                              tableau_essais = tableau_ttt,
                                              cut_seq_mono = cutoff_Cnholm[[1]][["C_mono"]], power_seq_mono = cutoff_Cnholm[[1]][["gamma_mono"]],
                                              cut_seq_holm = cutoff_Cnholm[[1]][["C_holm"]], power_seq_holm = cutoff_Cnholm[[1]][["gamma_holm"]])
    
    cat("Varying threshold, Bonferroni.\n")
    opchar_bonfevar[[i]] <- opcharac_efftox(ana_inter = ana_inter[[i]],
                                            alpha = alpha,
                                            p_reel = p_ttt, p_n = p_n, p_a = p_a,
                                            nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                            tableau_essais = tableau_ttt,
                                            cut_seq = cutoff_Cnbonf[[1]]["C_"], power_seq = cutoff_Cnbonf[[1]]["gamma"])
    
    cat("Varying threshold, monoarm.\n")
    opchar_monovar[[i]] <- list(ttt1 = opcharac_efftox(ana_inter = ana_inter[[i]],
                                                       alpha = alpha,
                                                       p_reel = p_ttt$ttt1, p_n = p_n, p_a = p_a,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt1"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt2 = opcharac_efftox(ana_inter = ana_inter[[i]],
                                                       alpha = alpha,
                                                       p_reel = p_ttt$ttt2, p_n = p_n, p_a = p_a,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt2"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                ttt3 = opcharac_efftox(ana_inter = ana_inter[[i]],
                                                       alpha = alpha,
                                                       p_reel = p_ttt$ttt3, p_n = p_n, p_a = p_a,
                                                       nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                       tableau_essais = tableau_ttt %>% filter(ttt == "ttt3"),
                                                       cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]))
    
    cat("Constant threshold, multiarm.\n")
    opchar_multicstt[[i]] <- opcharac_efftox_const(ana_inter = ana_inter[[i]],
                                                   alpha = alpha,
                                                   p_reel = p_ttt, p_n = p_n, p_a = p_a,
                                                   nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                   tableau_essais = tableau_ttt,
                                                   seq_eff = cutoff_Gammulti[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammulti[["seuils"]]["gamma_tox"])
    
    cat("Constant threshold, monoarm.\n")
    opchar_monocstt[[i]] <- list(ttt1 = opcharac_efftox_const(ana_inter = ana_inter[[i]],
                                                              alpha = alpha,
                                                              p_reel = p_ttt$ttt1, p_n = p_n, p_a = p_a,
                                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                              tableau_essais = tableau_ttt %>% filter(ttt == "ttt1"),
                                                              seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]),
                                 ttt2 = opcharac_efftox_const(ana_inter = ana_inter[[i]],
                                                              alpha = alpha,
                                                              p_reel = p_ttt$ttt2, p_n = p_n, p_a = p_a,
                                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                              tableau_essais = tableau_ttt %>% filter(ttt == "ttt2"),
                                                              seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]),
                                 ttt3 = opcharac_efftox_const(ana_inter = ana_inter[[i]],
                                                              alpha = alpha,
                                                              p_reel = p_ttt$ttt3, p_n = p_n, p_a = p_a,
                                                              nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                              tableau_essais = tableau_ttt %>% filter(ttt == "ttt3"),
                                                              seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]))
    
    cat("Constant threshold, Bonferroni\n")
    opchar_bonfecstt[[i]] <- opcharac_efftox_const(ana_inter = ana_inter[[i]],
                                                   alpha = alpha,
                                                   p_reel = p_ttt, p_n = p_n, p_a = p_a,
                                                   nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                   tableau_essais = tableau_ttt,
                                                   seq_eff = cutoff_Gambonf[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gambonf[["seuils"]]["gamma_tox"])
    
    cat("Varying threshold, multiarm (H0).\n")
    opchar_multivar_h0[[i]] <- opcharac_efftox(ana_inter = ana_inter[[i]],
                                               alpha = alpha,
                                               p_reel = p_ttt_h0, p_n = p_n, p_a = p_a,
                                               nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                               tableau_essais = tableau_ttt_h0,
                                               cut_seq = cutoff_Cnmulti[[1]]["C_"], power_seq = cutoff_Cnmulti[[1]]["gamma"])
    
    cat("Varying threshold, Holm (H0).\n")
    opchar_holm_h0[[i]] <- opcharac_efftox_holm2(ana_inter = ana_inter[[i]],
                                                 alpha = alpha,
                                                 p_reel = p_ttt_h0, p_n = p_n, p_a = p_a,
                                                 nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                 tableau_essais = tableau_ttt_h0,
                                                 cut_seq_mono = cutoff_Cnholm[[1]][["C_mono"]], power_seq_mono = cutoff_Cnholm[[1]][["gamma_mono"]],
                                                 cut_seq_holm = cutoff_Cnholm[[1]][["C_holm"]], power_seq_holm = cutoff_Cnholm[[1]][["gamma_holm"]])
    
    cat("Varying threshold, Bonferroni (H0).\n")
    opchar_bonfevar_h0[[i]] <- opcharac_efftox(ana_inter = ana_inter[[i]],
                                               alpha = alpha,
                                               p_reel = p_ttt_h0, p_n = p_n, p_a = p_a,
                                               nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                               tableau_essais = tableau_ttt_h0,
                                               cut_seq = cutoff_Cnbonf[[1]]["C_"], power_seq = cutoff_Cnbonf[[1]]["gamma"])
    
    cat("Varying threshold, monoarm (H0).\n")
    opchar_monovar_h0[[i]] <- list(ttt1 = opcharac_efftox(ana_inter = ana_inter[[i]],
                                                          alpha = alpha,
                                                          p_reel = p_ttt_h0$ttt1, p_n = p_n, p_a = p_a,
                                                          nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                          tableau_essais = tableau_ttt_h0 %>% filter(ttt == "ttt1"),
                                                          cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                   ttt2 = opcharac_efftox(ana_inter = ana_inter[[i]],
                                                          alpha = alpha,
                                                          p_reel = p_ttt_h0$ttt2, p_n = p_n, p_a = p_a,
                                                          nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                          tableau_essais = tableau_ttt_h0 %>% filter(ttt == "ttt2"),
                                                          cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]),
                                   ttt3 = opcharac_efftox(ana_inter = ana_inter[[i]],
                                                          alpha = alpha,
                                                          p_reel = p_ttt_h0$ttt3, p_n = p_n, p_a = p_a,
                                                          nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                          tableau_essais = tableau_ttt_h0 %>% filter(ttt == "ttt3"),
                                                          cut_seq = cutoff_Cnmono[[1]]["C_"], power_seq = cutoff_Cnmono[[1]]["gamma"]))
    
    cat("Constant threshold, multiarm (H0).\n")
    opchar_multicstt_h0[[i]] <- opcharac_efftox_const(ana_inter = ana_inter[[i]],
                                                      alpha = alpha,
                                                      p_reel = p_ttt_h0, p_n = p_n, p_a = p_a,
                                                      nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                      tableau_essais = tableau_ttt_h0,
                                                      seq_eff = cutoff_Gammulti[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammulti[["seuils"]]["gamma_tox"])
    
    cat("Constant threshold, monoarm (H0).\n")
    opchar_monocstt_h0[[i]] <- list(ttt1 = opcharac_efftox_const(ana_inter = ana_inter[[i]],
                                                                 alpha = alpha,
                                                                 p_reel = p_ttt_h0$ttt1, p_n = p_n, p_a = p_a,
                                                                 nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                                 tableau_essais = tableau_ttt_h0 %>% filter(ttt == "ttt1"),
                                                                 seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]),
                                    ttt2 = opcharac_efftox_const(ana_inter = ana_inter[[i]],
                                                                 alpha = alpha,
                                                                 p_reel = p_ttt_h0$ttt2, p_n = p_n, p_a = p_a,
                                                                 nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                                 tableau_essais = tableau_ttt_h0 %>% filter(ttt == "ttt2"),
                                                                 seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]),
                                    ttt3 = opcharac_efftox_const(ana_inter = ana_inter[[i]],
                                                                 alpha = alpha,
                                                                 p_reel = p_ttt_h0$ttt3, p_n = p_n, p_a = p_a,
                                                                 nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                                 tableau_essais = tableau_ttt_h0 %>% filter(ttt == "ttt3"),
                                                                 seq_eff = cutoff_Gammono[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gammono[["seuils"]]["gamma_tox"]))
    
    cat("Constant threshold, Bonferroni (H0).\n")
    opchar_bonfecstt_h0[[i]] <- opcharac_efftox_const(ana_inter = ana_inter[[i]],
                                                      alpha = alpha,
                                                      p_reel = p_ttt_h0, p_n = p_n, p_a = p_a,
                                                      nsim_oc = nsim_oc, nsim_essai = nsim_essais,
                                                      tableau_essais = tableau_ttt_h0,
                                                      seq_eff = cutoff_Gambonf[["seuils"]]["gamma_eff"], seq_tox = cutoff_Gambonf[["seuils"]]["gamma_tox"])
    
  }
  
  save(opchar_multivar, opchar_monovar, opchar_multicstt, opchar_monocstt, opchar_bonfevar, opchar_bonfecstt,
       file = paste0("Data/simu_vsref/resultats_bruts_1scenar_chgtmax_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))
  
} else {
  
  # Sorry, raw results aren't included
  load("Data/simu_vsref/resultats_bruts_1scenar_chgtmax_20221205.Rdata")
  
}

if (clear) rm(tableau_ttt, tableau_ttt_h0)

## C/ Formatting ----

### Giant table with results for each trial

tab_Cnmulti <- lapply(seq_along(opchar_multivar), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), opchar_multivar[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Cnholm <- lapply(seq_along(opchar_holm), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), opchar_holm[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Cnbonf <- lapply(seq_along(opchar_bonfevar), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), opchar_bonfevar[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Cnmono <- lapply(seq_along(opchar_monovar), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), do.call("rbind", lapply(opchar_monovar[[x]], "[[", "essais")))}) %>% 
  do.call(what = "rbind", args = .)

tab_Gammulti <- lapply(seq_along(opchar_multicstt), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), opchar_multicstt[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Gambonf <- lapply(seq_along(opchar_bonfecstt), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), opchar_bonfecstt[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Gammono <- lapply(seq_along(opchar_monocstt), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), do.call("rbind", lapply(opchar_monocstt[[x]], "[[", "essais")))}) %>% 
  do.call(what = "rbind", args = .)

tab_Cnmulti_h0 <- lapply(seq_along(opchar_multivar_h0), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), opchar_multivar_h0[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Cnholm_h0 <- lapply(seq_along(opchar_holm_h0), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), opchar_holm_h0[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Cnbonf_h0 <- lapply(seq_along(opchar_bonfevar_h0), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), opchar_bonfevar_h0[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Cnmono_h0 <- lapply(seq_along(opchar_monovar_h0), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), do.call("rbind", lapply(opchar_monovar_h0[[x]], "[[", "essais")))}) %>% 
  do.call(what = "rbind", args = .)

tab_Gammulti_h0 <- lapply(seq_along(opchar_multicstt_h0), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), opchar_multicstt_h0[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Gambonf_h0 <- lapply(seq_along(opchar_bonfecstt_h0), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), opchar_bonfecstt_h0[[x]]$essais)}) %>% 
  do.call(what = "rbind", args = .)

tab_Gammono_h0 <- lapply(seq_along(opchar_monocstt_h0), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), do.call("rbind", lapply(opchar_monocstt_h0[[x]], "[[", "essais")))}) %>% 
  do.call(what = "rbind", args = .)

save(tab_Cnmulti, tab_Cnholm, tab_Cnbonf, tab_Cnmono, tab_Gammulti, tab_Gambonf, tab_Gammono, 
     tab_Cnmulti_h0, tab_Cnholm_h0, tab_Cnbonf_h0, tab_Cnmono_h0, tab_Gammulti_h0, tab_Gambonf_h0, tab_Gammono_h0,
     file = paste0("Data/simu_vsref/essais_bruts_bonfgam_chgtnb_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))

if (clear) rm(tab_Cnmulti, tab_Cnbonf, tab_Cnmono, tab_Gammulti, tab_Gambonf, tab_Gammono, tab_Cnholm_h0, tab_Cnholm,
              tab_Cnmulti_h0, tab_Cnbonf_h0, tab_Cnmono_h0, tab_Gammulti_h0, tab_Gambonf_h0, tab_Gammono_h0)

### Arm by arm results

tab_Cnmultiarm <- lapply(seq_along(opchar_multivar), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), opchar_multivar[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Multiarm C<sub>n</sub>")

tab_Cnholm <- lapply(seq_along(opchar_holm), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), opchar_holm[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Holm C<sub>n</sub>")

tab_Cnbonferr <- lapply(seq_along(opchar_bonfevar), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), opchar_bonfevar[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Bonferroni C<sub>n</sub>")

tab_Cnmonoarm <- lapply(seq_along(opchar_monovar), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), do.call("rbind", lapply(seq_along(opchar_monovar[[x]]), function(index) {
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

tab_Gammultiarm <- lapply(seq_along(opchar_multicstt), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), opchar_multicstt[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Multiarm &epsilon;")

tab_Gambonferr <- lapply(seq_along(opchar_bonfecstt), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), opchar_bonfecstt[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Bonferroni &epsilon;")

tab_Gammonoarm <- lapply(seq_along(opchar_monocstt), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), do.call("rbind", lapply(seq_along(opchar_monocstt[[x]]), function(index) {
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

data_ggplot_nbpatvsref <- rbind(tab_Cnmultiarm, tab_Cnbonferr, tab_Cnmonoarm, tab_Gammultiarm, tab_Gambonferr, tab_Gammonoarm, tab_Cnholm)

rm(tab_Cnmultiarm, tab_Cnbonferr, tab_Cnmonoarm, tab_Gammultiarm, tab_Gambonferr, tab_Gammonoarm, tab_Cnholm)

# Global H0
tab_Cnmultiarm_h0 <- lapply(seq_along(opchar_multivar_h0), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), opchar_multivar_h0[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Multiarm C<sub>n</sub>")

tab_Cnholm_h0 <- lapply(seq_along(opchar_holm_h0), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), opchar_holm_h0[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Holm C<sub>n</sub>")

tab_Cnbonferr_h0 <- lapply(seq_along(opchar_bonfevar_h0), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), opchar_bonfevar_h0[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Bonferroni C<sub>n</sub>")

tab_Cnmonoarm_h0 <- lapply(seq_along(opchar_monovar_h0), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), do.call("rbind", lapply(seq_along(opchar_monovar_h0[[x]]), function(index) {
  data.frame(ttt = names(opchar_monovar_h0[[x]][index]),
             rejet_h0 = as.numeric(opchar_monovar_h0[[x]][[index]]$caracteristique["rejet_h0"]),
             arret_precoce = as.numeric(opchar_monovar_h0[[x]][[index]]$caracteristique["arret_precoce"]),
             arret_precoce_fut = as.numeric(opchar_monovar_h0[[x]][[index]]$caracteristique["arret_precoce_fut"]),
             arret_precoce_tox = as.numeric(opchar_monovar_h0[[x]][[index]]$caracteristique["arret_precoce_tox"]),
             arret_fut = as.numeric(opchar_monovar_h0[[x]][[index]]$caracteristique["arret_fut"]),
             arret_tox = as.numeric(opchar_monovar_h0[[x]][[index]]$caracteristique["arret_tox"]),
             tot_pat = as.numeric(opchar_monovar_h0[[x]][[index]]$caracteristique["nb_pts"]),
             tot_eff = as.numeric(opchar_monovar_h0[[x]][[index]]$caracteristique["nb_eff"]),
             tot_tox = as.numeric(opchar_monovar_h0[[x]][[index]]$caracteristique["nb_tox"]))
})))}) %>% 
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Monoarm C<sub>n</sub>",
         arret = 1 - rejet_h0)

tab_Gammultiarm_h0 <- lapply(seq_along(opchar_multicstt_h0), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), opchar_multicstt_h0[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Multiarm &epsilon;")

tab_Gambonferr_h0 <- lapply(seq_along(opchar_bonfecstt_h0), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), opchar_bonfecstt_h0[[x]]$carac_bras)}) %>%
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Bonferroni &epsilon;")

tab_Gammonoarm_h0 <- lapply(seq_along(opchar_monocstt_h0), function(x) {cbind(nb_pat = sum(ana_inter[[x]]), do.call("rbind", lapply(seq_along(opchar_monocstt_h0[[x]]), function(index) {
  data.frame(ttt = names(opchar_monocstt_h0[[x]][index]),
             rejet_h0 = as.numeric(opchar_monocstt_h0[[x]][[index]]$caracteristique["rejet_h0"]),
             arret_precoce = as.numeric(opchar_monocstt_h0[[x]][[index]]$caracteristique["arret_precoce"]),
             arret_precoce_fut = as.numeric(opchar_monocstt_h0[[x]][[index]]$caracteristique["arret_precoce_fut"]),
             arret_precoce_tox = as.numeric(opchar_monocstt_h0[[x]][[index]]$caracteristique["arret_precoce_tox"]),
             arret_fut = as.numeric(opchar_monocstt_h0[[x]][[index]]$caracteristique["arret_fut"]),
             arret_tox = as.numeric(opchar_monocstt_h0[[x]][[index]]$caracteristique["arret_tox"]),
             tot_pat = as.numeric(opchar_monocstt_h0[[x]][[index]]$caracteristique["nb_pts"]),
             tot_eff = as.numeric(opchar_monocstt_h0[[x]][[index]]$caracteristique["nb_eff"]),
             tot_tox = as.numeric(opchar_monocstt_h0[[x]][[index]]$caracteristique["nb_tox"]))
})))}) %>% 
  do.call(what = "rbind", args = .) %>% 
  mutate(methode = "Monoarm &epsilon;",
         arret = 1 - rejet_h0)

data_ggplot_nbpatvsref_h0 <- rbind(tab_Cnmultiarm_h0, tab_Cnbonferr_h0, tab_Cnmonoarm_h0, tab_Gammultiarm_h0, tab_Gambonferr_h0, tab_Gammonoarm_h0, tab_Cnholm_h0)

rm(tab_Cnmultiarm_h0, tab_Cnbonferr_h0, tab_Cnmonoarm_h0, tab_Gammultiarm_h0, tab_Gambonferr_h0, tab_Gammonoarm_h0, tab_Cnholm_h0)

save(data_ggplot_nbpatvsref, data_ggplot_nbpatvsref_h0, file = paste0("Data/simu_vsref/donnees_graphes_ref_nbpat_", format(Sys.Date(), "%Y%m%d"), ".Rdata")) 

if (clear) rm(list = ls()[!(ls() %in% c("simu", "simu_seuil", "clear", "ana_inter", "alpha", "n_bras", "p_n", "p_a",
                                        "nsim_oc", "nsim_essais", "methode", "p_ttt", "p_ttt_h0"))])

# V/ Unbalance ----

## A/ Other additional parameters ----

mat_beta_xi <- matrix(c(1, 1, 0, 0, 0, 1, 0, 1), nrow = 2, byrow = TRUE)
vec_priors <- c(sum(p_n[mat_beta_xi[1, ] == 1]), sum(p_n[mat_beta_xi[2, ] == 1]))
phi <- vec_priors

# Still the same thresholds
load("Data/simu_vsref/cutoff_13scenar_20221202.Rdata")

## B/ Generate table of individualised patients ----

tableau_pts <- gen_patients_multinom(n_sim = nsim_essais, 
                                     ana_inter = rep(1, 80),
                                     multinom_ttt = p_ttt,
                                     multinom_cont = p_n,
                                     seed = 1993,
                                     methode = methode)
tableau_pts_h0 <- gen_patients_multinom(n_sim = nsim_essais, 
                                        ana_inter = rep(1, 80),
                                        multinom_ttt = p_ttt_h0,
                                        multinom_cont = p_n,
                                        seed = 1993,
                                        methode = methode)

## C/ Simulate unbalance with uniform law and get the results ----

# Helper functions
summarise_decision <- function(data, groupe, colonne, char_decision, intitule) {
  data %>%
    group_by({{groupe}}) %>%
    summarise("{{intitule}}" := mean({{colonne}} == char_decision))
}
summarise_ttt <- function(data, groupe, colonne, intitule = {{colonne}}) {
  data %>%
    group_by({{groupe}}) %>%
    summarise("{{intitule}}" := mean({{colonne}}))
}
summarise_detect <- function(data, groupe, col_decision, char_decision, intitule) {
  data %>%
    group_by({{groupe}}) %>%
    summarise("{{intitule}}" := mean(str_detect({{col_decision}}, char_decision)))
}
summarise_essai <- function(data, colonne, methode) {
  data_filt <- data %>% 
    filter({{colonne}} != "Continue") %>%
    group_by(n_simu, ttt) %>%
    arrange(n_simu, ttt, nb_ana) %>%
    slice(1) %>%
    ungroup()
  carac_gen <- data_filt %>%
    mutate(rejet_essai = if_else(ttt == "ttt2", 1, 0),
           rejet_h0 = as.numeric({{colonne}} == "Accept the treatment"),
           bonne_decision = rejet_h0 == rejet_essai) %>% 
    group_by(n_simu) %>%
    summarise(
      rejet_glob = sum(rejet_h0) > 0,
      prop_bonne_deci = mean(bonne_decision),
      .groups = "drop"
    ) %>%
    summarise(rejet_glob = mean(rejet_glob),
              prop_bonne_deci = mean(prop_bonne_deci))
  carac_bras <- summarise_decision(data_filt, ttt, {{colonne}}, "Accept the treatment", rejet_h0) %>%
    left_join(summarise_decision(data_filt, ttt, {{colonne}}, "Early stopping", arret_precoce), by = "ttt") %>%
    left_join(summarise_detect(data_filt, ttt, {{colonne}}, "Stop", arret), by = "ttt") %>%
    left_join(summarise_ttt(data_filt, ttt, tot_pat), by = "ttt") %>%
    left_join(summarise_ttt(data_filt, ttt, tot_eff), by = "ttt") %>%
    left_join(summarise_ttt(data_filt, ttt, tot_pat - tot_notox, tot_tox), by = "ttt")
  tab_carac <- cbind(Methode = methode, carac_gen, carac_bras)
  return(tab_carac)
}

# Generate interim analyses with little randomness on the number of patients
tableau_alea <- expand.grid(n_simu = 1:10000,
                            nb_ana = 1:4,
                            ttt = c("ttt1", "ttt2", "ttt3"),
                            ana_inter_0 = 15)
set.seed(1993)
tableau_alea <- tableau_alea %>% 
  arrange(n_simu, ttt, nb_ana) %>% 
  mutate(ana_inter_1 = ana_inter_0 + round(runif(n = 120000, min = -1.4999999, max = 1.4999999)),
         ana_inter_2 = ana_inter_0 + round(runif(n = 120000, min = -2.4999999, max = 2.4999999)),
         ana_inter_3 = ana_inter_0 + round(runif(n = 120000, min = -3.4999999, max = 3.4999999)),
         ana_inter_4 = ana_inter_0 + round(runif(n = 120000, min = -4.4999999, max = 4.4999999)),
         ana_inter_5 = ana_inter_0 + round(runif(n = 120000, min = -5.4999999, max = 5.4999999))) %>% 
  group_by(n_simu, ttt) %>% 
  mutate(across(ana_inter_0:ana_inter_5, ~ cumsum(.x)))

### 1) Not H0 ----

tab_deseq <- list()
for (i in 0:5) {
  
  tableau_pts_bis <- tableau_pts %>% 
    select(-nb_ana) %>% 
    left_join(select(tableau_alea, n_simu, ttt, nb_ana, !!sym(paste0("ana_inter_", i))), by = c("n_simu", "ttt", "tot_pat" = paste0("ana_inter_", i))) %>% 
    filter(!is.na(nb_ana))
  
  # Compute posterior probabilities and get decisions by designs 
  tableau_pts_bis <- tableau_pts_bis %>% 
    mutate(post_eff = 1 - pbeta(phi[1], vec_priors[1] + tot_eff, 1 - vec_priors[1] + tot_pat - tot_eff),
           post_notox = 1 - pbeta(phi[2], vec_priors[2] + tot_notox, 1 - vec_priors[2] + tot_pat - tot_notox)) %>% 
    group_by(n_simu, ttt) %>% 
    mutate(max_pat = 60) %>% 
    ungroup() %>% 
    mutate(seuil_cnmult = cutoff_Cnmulti[[1]]["C_"] * (tot_pat / max_pat) ^ cutoff_Cnmulti[[1]]["gamma"],
           decision_cnmult = case_when((nb_ana != max(nb_ana)) & post_eff >= seuil_cnmult & post_notox >= seuil_cnmult ~ "Continue",
                                       (nb_ana == max(nb_ana)) & post_eff >= seuil_cnmult & post_notox >= seuil_cnmult ~ "Accept the treatment",
                                       (nb_ana != max(nb_ana)) & (post_eff < seuil_cnmult | post_notox < seuil_cnmult) ~ "Early stopping",
                                       (nb_ana == max(nb_ana)) & (post_eff < seuil_cnmult | post_notox < seuil_cnmult) ~ "Stop"),
           seuil_cnbonf = cutoff_Cnbonf[[1]]["C_"] * (tot_pat / max_pat) ^ cutoff_Cnbonf[[1]]["gamma"],
           decision_cnbonf = case_when((nb_ana != max(nb_ana)) & post_eff >= seuil_cnbonf & post_notox >= seuil_cnbonf ~ "Continue",
                                       (nb_ana == max(nb_ana)) & post_eff >= seuil_cnbonf & post_notox >= seuil_cnbonf ~ "Accept the treatment",
                                       (nb_ana != max(nb_ana)) & (post_eff < seuil_cnbonf | post_notox < seuil_cnbonf) ~ "Early stopping",
                                       (nb_ana == max(nb_ana)) & (post_eff < seuil_cnbonf | post_notox < seuil_cnbonf) ~ "Stop"),
           seuil_cnmono = cutoff_Cnmono[[1]]["C_"] * (tot_pat / max_pat) ^ cutoff_Cnmono[[1]]["gamma"],
           decision_cnmono = case_when((nb_ana != max(nb_ana)) & post_eff >= seuil_cnmono & post_notox >= seuil_cnmono ~ "Continue",
                                       (nb_ana == max(nb_ana)) & post_eff >= seuil_cnmono & post_notox >= seuil_cnmono ~ "Accept the treatment",
                                       (nb_ana != max(nb_ana)) & (post_eff < seuil_cnmono | post_notox < seuil_cnmono) ~ "Early stopping",
                                       (nb_ana == max(nb_ana)) & (post_eff < seuil_cnmono | post_notox < seuil_cnmono) ~ "Stop"),
           decision_epsmult = case_when((nb_ana != max(nb_ana)) & post_eff >= cutoff_Gammulti[[1]]["gamma_eff"] & post_notox >= cutoff_Gammulti[[1]]["gamma_tox"] ~ "Continue",
                                        (nb_ana == max(nb_ana)) & post_eff >= cutoff_Gammulti[[1]]["gamma_eff"] & post_notox >= cutoff_Gammulti[[1]]["gamma_tox"] ~ "Accept the treatment",
                                        (nb_ana != max(nb_ana)) & (post_eff < cutoff_Gammulti[[1]]["gamma_eff"] | post_notox < cutoff_Gammulti[[1]]["gamma_tox"]) ~ "Early stopping",
                                        (nb_ana == max(nb_ana)) & (post_eff < cutoff_Gammulti[[1]]["gamma_eff"] | post_notox < cutoff_Gammulti[[1]]["gamma_tox"]) ~ "Stop"),
           decision_epsbonf = case_when((nb_ana != max(nb_ana)) & post_eff >= cutoff_Gambonf[[1]]["gamma_eff"] & post_notox >= cutoff_Gambonf[[1]]["gamma_tox"] ~ "Continue",
                                        (nb_ana == max(nb_ana)) & post_eff >= cutoff_Gambonf[[1]]["gamma_eff"] & post_notox >= cutoff_Gambonf[[1]]["gamma_tox"] ~ "Accept the treatment",
                                        (nb_ana != max(nb_ana)) & (post_eff < cutoff_Gambonf[[1]]["gamma_eff"] | post_notox < cutoff_Gambonf[[1]]["gamma_tox"]) ~ "Early stopping",
                                        (nb_ana == max(nb_ana)) & (post_eff < cutoff_Gambonf[[1]]["gamma_eff"] | post_notox < cutoff_Gambonf[[1]]["gamma_tox"]) ~ "Stop"),
           decision_epsmono = case_when((nb_ana != max(nb_ana)) & post_eff >= cutoff_Gammono[[1]]["gamma_eff"] & post_notox >= cutoff_Gammono[[1]]["gamma_tox"] ~ "Continue",
                                        (nb_ana == max(nb_ana)) & post_eff >= cutoff_Gammono[[1]]["gamma_eff"] & post_notox >= cutoff_Gammono[[1]]["gamma_tox"] ~ "Accept the treatment",
                                        (nb_ana != max(nb_ana)) & (post_eff < cutoff_Gammono[[1]]["gamma_eff"] | post_notox < cutoff_Gammono[[1]]["gamma_tox"]) ~ "Early stopping",
                                        (nb_ana == max(nb_ana)) & (post_eff < cutoff_Gammono[[1]]["gamma_eff"] | post_notox < cutoff_Gammono[[1]]["gamma_tox"]) ~ "Stop"))
  
  tableau_pts_bis <- split(tableau_pts_bis, tableau_pts_bis$n_simu)
  m <- 3
  tableau_pts_bis <- map_dfr(
    .x = tableau_pts_bis,
    .f = function(data) {
      data$nb_act <- NA_integer_
      data$continuer <- NA
      data$seuil_holm <- NA_real_
      for (i in seq_len(max(data$nb_ana))) {
        if (i == 1) data$nb_act[data$nb_ana == i] <- bras_act <- m else data$nb_act[data$nb_ana == i] <- bras_act <- sum(data$continuer[data$nb_ana == (i - 1)])
        nu <- 1 + m - bras_act
        data$seuil_holm[data$nb_ana == i] <- ((nu - cutoff_Cnholm[[1]][["C_holm"]]) / nu) * 
          (data$tot_pat[data$nb_ana == i] / data$max_pat[data$nb_ana == i]) ^ cutoff_Cnholm[[1]][["gamma_holm"]]
        if (i == max(data$nb_ana)) data$seuil_holm[data$nb_ana == i] <- pmax(data$seuil_holm[data$nb_ana == i],
                                                                             cutoff_Cnholm[[1]][["C_mono"]] * (data$tot_pat[data$nb_ana == i] / data$max_pat[data$nb_ana == i]) ^ cutoff_Cnholm[[1]][["gamma_mono"]])
        if (i == 1) {
          data$continuer[data$nb_ana == i] <- data$post_eff[data$nb_ana == i] > data$seuil_holm[data$nb_ana == i] & data$post_notox[data$nb_ana == i] > data$seuil_holm[data$nb_ana == i]
        } else {
          data$continuer[data$nb_ana == i] <- data$continuer[data$nb_ana == (i - 1)]
          data$continuer[data$continuer & data$nb_ana == i] <- data$post_eff[data$continuer & data$nb_ana == i] > data$seuil_holm[data$continuer & data$nb_ana == i] &
            data$post_notox[data$continuer & data$nb_ana == i] > data$seuil_holm[data$continuer & data$nb_ana == i]
        }
      }
      return(data)
    }
  )
  
  tableau_pts_bis <- tableau_pts_bis %>%
    mutate(decision_holm = dplyr::case_when((nb_ana != max(nb_ana)) & continuer ~ "Continue",
                                            (nb_ana == max(nb_ana)) & continuer ~ "Accept the treatment",
                                            (nb_ana != max(nb_ana)) & !continuer ~ "Early stopping",
                                            (nb_ana == max(nb_ana)) & !continuer ~ "Stop")) %>% 
    select(-continuer)
  
  # Operating characteristics
  tab_deseq[[i + 1]] <- rbind(summarise_essai(tableau_pts_bis, decision_cnmult, "Multiarm C<sub>n</sub>"),
                              summarise_essai(tableau_pts_bis, decision_cnmono, "Monoarm C<sub>n</sub>"),
                              summarise_essai(tableau_pts_bis, decision_cnbonf, "Bonferroni C<sub>n</sub>"),
                              summarise_essai(tableau_pts_bis, decision_epsmult, "Multiarm &epsilon;"),
                              summarise_essai(tableau_pts_bis, decision_epsmono, "Monoarm &epsilon;"),
                              summarise_essai(tableau_pts_bis, decision_epsbonf, "Bonferroni &epsilon;"),
                              summarise_essai(tableau_pts_bis, decision_holm, "Holm C<sub>n</sub>")) %>% 
    bind_cols(deseq = i)
  
}

tab_deseq <- do.call("rbind", tab_deseq)

### 2) H0 ----

tab_deseq_h0 <- list()
for (i in 0:5) {
  
  tableau_pts_bis <- tableau_pts_h0 %>% 
    select(-nb_ana) %>% 
    left_join(select(tableau_alea, n_simu, ttt, nb_ana, !!sym(paste0("ana_inter_", i))), by = c("n_simu", "ttt", "tot_pat" = paste0("ana_inter_", i))) %>% 
    filter(!is.na(nb_ana))
  
  # Compute posterior probabilities and get decisions by designs 
  tableau_pts_bis <- tableau_pts_bis %>% 
    mutate(post_eff = 1 - pbeta(phi[1], vec_priors[1] + tot_eff, 1 - vec_priors[1] + tot_pat - tot_eff),
           post_notox = 1 - pbeta(phi[2], vec_priors[2] + tot_notox, 1 - vec_priors[2] + tot_pat - tot_notox)) %>% 
    group_by(n_simu, ttt) %>% 
    mutate(max_pat = 60) %>% 
    ungroup() %>% 
    mutate(seuil_cnmult = cutoff_Cnmulti[[1]]["C_"] * (tot_pat / max_pat) ^ cutoff_Cnmulti[[1]]["gamma"],
           decision_cnmult = case_when((nb_ana != max(nb_ana)) & post_eff >= seuil_cnmult & post_notox >= seuil_cnmult ~ "Continue",
                                       (nb_ana == max(nb_ana)) & post_eff >= seuil_cnmult & post_notox >= seuil_cnmult ~ "Accept the treatment",
                                       (nb_ana != max(nb_ana)) & (post_eff < seuil_cnmult | post_notox < seuil_cnmult) ~ "Early stopping",
                                       (nb_ana == max(nb_ana)) & (post_eff < seuil_cnmult | post_notox < seuil_cnmult) ~ "Stop"),
           seuil_cnbonf = cutoff_Cnbonf[[1]]["C_"] * (tot_pat / max_pat) ^ cutoff_Cnbonf[[1]]["gamma"],
           decision_cnbonf = case_when((nb_ana != max(nb_ana)) & post_eff >= seuil_cnbonf & post_notox >= seuil_cnbonf ~ "Continue",
                                       (nb_ana == max(nb_ana)) & post_eff >= seuil_cnbonf & post_notox >= seuil_cnbonf ~ "Accept the treatment",
                                       (nb_ana != max(nb_ana)) & (post_eff < seuil_cnbonf | post_notox < seuil_cnbonf) ~ "Early stopping",
                                       (nb_ana == max(nb_ana)) & (post_eff < seuil_cnbonf | post_notox < seuil_cnbonf) ~ "Stop"),
           seuil_cnmono = cutoff_Cnmono[[1]]["C_"] * (tot_pat / max_pat) ^ cutoff_Cnmono[[1]]["gamma"],
           decision_cnmono = case_when((nb_ana != max(nb_ana)) & post_eff >= seuil_cnmono & post_notox >= seuil_cnmono ~ "Continue",
                                       (nb_ana == max(nb_ana)) & post_eff >= seuil_cnmono & post_notox >= seuil_cnmono ~ "Accept the treatment",
                                       (nb_ana != max(nb_ana)) & (post_eff < seuil_cnmono | post_notox < seuil_cnmono) ~ "Early stopping",
                                       (nb_ana == max(nb_ana)) & (post_eff < seuil_cnmono | post_notox < seuil_cnmono) ~ "Stop"),
           decision_epsmult = case_when((nb_ana != max(nb_ana)) & post_eff >= cutoff_Gammulti[[1]]["gamma_eff"] & post_notox >= cutoff_Gammulti[[1]]["gamma_tox"] ~ "Continue",
                                        (nb_ana == max(nb_ana)) & post_eff >= cutoff_Gammulti[[1]]["gamma_eff"] & post_notox >= cutoff_Gammulti[[1]]["gamma_tox"] ~ "Accept the treatment",
                                        (nb_ana != max(nb_ana)) & (post_eff < cutoff_Gammulti[[1]]["gamma_eff"] | post_notox < cutoff_Gammulti[[1]]["gamma_tox"]) ~ "Early stopping",
                                        (nb_ana == max(nb_ana)) & (post_eff < cutoff_Gammulti[[1]]["gamma_eff"] | post_notox < cutoff_Gammulti[[1]]["gamma_tox"]) ~ "Stop"),
           decision_epsbonf = case_when((nb_ana != max(nb_ana)) & post_eff >= cutoff_Gambonf[[1]]["gamma_eff"] & post_notox >= cutoff_Gambonf[[1]]["gamma_tox"] ~ "Continue",
                                        (nb_ana == max(nb_ana)) & post_eff >= cutoff_Gambonf[[1]]["gamma_eff"] & post_notox >= cutoff_Gambonf[[1]]["gamma_tox"] ~ "Accept the treatment",
                                        (nb_ana != max(nb_ana)) & (post_eff < cutoff_Gambonf[[1]]["gamma_eff"] | post_notox < cutoff_Gambonf[[1]]["gamma_tox"]) ~ "Early stopping",
                                        (nb_ana == max(nb_ana)) & (post_eff < cutoff_Gambonf[[1]]["gamma_eff"] | post_notox < cutoff_Gambonf[[1]]["gamma_tox"]) ~ "Stop"),
           decision_epsmono = case_when((nb_ana != max(nb_ana)) & post_eff >= cutoff_Gammono[[1]]["gamma_eff"] & post_notox >= cutoff_Gammono[[1]]["gamma_tox"] ~ "Continue",
                                        (nb_ana == max(nb_ana)) & post_eff >= cutoff_Gammono[[1]]["gamma_eff"] & post_notox >= cutoff_Gammono[[1]]["gamma_tox"] ~ "Accept the treatment",
                                        (nb_ana != max(nb_ana)) & (post_eff < cutoff_Gammono[[1]]["gamma_eff"] | post_notox < cutoff_Gammono[[1]]["gamma_tox"]) ~ "Early stopping",
                                        (nb_ana == max(nb_ana)) & (post_eff < cutoff_Gammono[[1]]["gamma_eff"] | post_notox < cutoff_Gammono[[1]]["gamma_tox"]) ~ "Stop"))
  
  tableau_pts_bis <- split(tableau_pts_bis, tableau_pts_bis$n_simu)
  m <- 3
  tableau_pts_bis <- map_dfr(
    .x = tableau_pts_bis,
    .f = function(data) {
      data$nb_act <- NA_integer_
      data$continuer <- NA
      data$seuil_holm <- NA_real_
      for (i in seq_len(max(data$nb_ana))) {
        if (i == 1) data$nb_act[data$nb_ana == i] <- bras_act <- m else data$nb_act[data$nb_ana == i] <- bras_act <- sum(data$continuer[data$nb_ana == (i - 1)])
        nu <- 1 + m - bras_act
        data$seuil_holm[data$nb_ana == i] <- ((nu - cutoff_Cnholm[[1]][["C_holm"]]) / nu) * 
          (data$tot_pat[data$nb_ana == i] / data$max_pat[data$nb_ana == i]) ^ cutoff_Cnholm[[1]][["gamma_holm"]]
        if (i == max(data$nb_ana)) data$seuil_holm[data$nb_ana == i] <- pmax(data$seuil_holm[data$nb_ana == i],
                                                                             cutoff_Cnholm[[1]][["C_mono"]] * (data$tot_pat[data$nb_ana == i] / data$max_pat[data$nb_ana == i]) ^ cutoff_Cnholm[[1]][["gamma_mono"]])
        if (i == 1) {
          data$continuer[data$nb_ana == i] <- data$post_eff[data$nb_ana == i] > data$seuil_holm[data$nb_ana == i] & data$post_notox[data$nb_ana == i] > data$seuil_holm[data$nb_ana == i]
        } else {
          data$continuer[data$nb_ana == i] <- data$continuer[data$nb_ana == (i - 1)]
          data$continuer[data$continuer & data$nb_ana == i] <- data$post_eff[data$continuer & data$nb_ana == i] > data$seuil_holm[data$continuer & data$nb_ana == i] &
            data$post_notox[data$continuer & data$nb_ana == i] > data$seuil_holm[data$continuer & data$nb_ana == i]
        }
      }
      return(data)
    }
  )
  
  tableau_pts_bis <- tableau_pts_bis %>%
    mutate(decision_holm = dplyr::case_when((nb_ana != max(nb_ana)) & continuer ~ "Continue",
                                            (nb_ana == max(nb_ana)) & continuer ~ "Accept the treatment",
                                            (nb_ana != max(nb_ana)) & !continuer ~ "Early stopping",
                                            (nb_ana == max(nb_ana)) & !continuer ~ "Stop")) %>% 
    select(-continuer)
  
  # Operating characteristics
  tab_deseq_h0[[i + 1]] <- rbind(summarise_essai(tableau_pts_bis, decision_cnmult, "Multiarm C<sub>n</sub>"),
                                 summarise_essai(tableau_pts_bis, decision_cnmono, "Monoarm C<sub>n</sub>"),
                                 summarise_essai(tableau_pts_bis, decision_cnbonf, "Bonferroni C<sub>n</sub>"),
                                 summarise_essai(tableau_pts_bis, decision_epsmult, "Multiarm &epsilon;"),
                                 summarise_essai(tableau_pts_bis, decision_epsmono, "Monoarm &epsilon;"),
                                 summarise_essai(tableau_pts_bis, decision_epsbonf, "Bonferroni &epsilon;"),
                                 summarise_essai(tableau_pts_bis, decision_holm, "Holm C<sub>n</sub>")) %>% 
    bind_cols(deseq = i)
  
}

tab_deseq_h0 <- do.call("rbind", tab_deseq_h0)

save(tab_deseq, tab_deseq_h0,
     file = paste0("Data/simu_vsref/donnees_graphes_ref_alea_", format(Sys.Date(), "%Y%m%d"), ".Rdata"))


