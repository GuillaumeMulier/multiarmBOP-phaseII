# ----------------------------------------- #
# Thresholds figures                        #
# Créé le 15/06/2021, modifié le 25/01/2023 #
# ----------------------------------------- #

# Packages et helpers ----

library(tidyverse)
library(ggtext)
library(scales)
library(cowplot)
library(patchwork)
library(flextable)
library(officer)
library(glue)
library(grid)

theme_set(theme_light(base_size = 16) +
            theme(legend.text = element_markdown(),
                  plot.title = element_markdown(),
                  axis.title.y = element_markdown(size = 12),
                  axis.title.x = element_text(size = 12),
                  plot.caption = element_markdown(hjust = 0),
                  panel.grid.major = element_line(color = "darkgrey", size = .7)))

calcul_cn <- function(abscisse, N, C, Gamm) 1 - C * (abscisse / N) ^ Gamm
calcul_cn_holm <- function(abscisse, BrasAct, N, C, Gamm) 1 - ((1 + 3 - BrasAct - C) / (1 + 3 - BrasAct)) * (abscisse / N) ^ Gamm

sauvegarder <- FALSE # Set to TRUE if you want to save the outputs
couleur <- "anglais"


# I/ Uncontrolled setting ----

# All thresholds except Holm because it varies with the number of active arms
plot_seuils <- ggplot() +
  geom_function(fun = ~ calcul_cn(.x, 60, .78, .9), aes(color = "C<sub>n</sub><sup>m</sup>"), size = 1) +
  geom_function(fun = ~ calcul_cn(.x, 60, .78, .9) + .005, aes(color = "Bonferroni C<sub>n</sub>"), size = 1) +
  geom_function(fun = ~ calcul_cn(.x, 60, .63, .93), aes(color = "C<sub>n</sub><sup>s</sup>"), size = 1) +
  annotate(geom = "point", x = seq(15, 60, 15), y = calcul_cn(abscisse = seq(15, 60, 15), N = 60, C = .78, Gamm = .9), color = "#2b4ebb", size = 2.5) +
  annotate(geom = "point", x = seq(15, 60, 15), y = calcul_cn(abscisse = seq(15, 60, 15), N = 60, C = .78, Gamm = .9) + .005, color = "#40a746", size = 2.5) +
  annotate(geom = "point", x = seq(15, 60, 15), y = calcul_cn(abscisse = seq(15, 60, 15), N = 60, C = .63, Gamm = .93), color = "#c63b1d", size = 2.5) +
  geom_hline(aes(color = "&epsilon;<sup>m</sup>", linetype = "&epsilon;<sub>efficacy</sub>", yintercept = 1 - .71), size = 1) +
  annotate(geom = "point", x = seq(15, 60, 15), y = 1 - .71, color = "#a71eb7", size = 2.5) +
  geom_hline(aes(color = "&epsilon;<sup>m</sup>", linetype = "&epsilon;<sub>non toxicity</sub>", yintercept = 1 - .43), size = 1) +
  annotate(geom = "point", x = seq(15, 60, 15), y = 1 - .43, color = "#a71eb7", size = 2.5) +
  geom_hline(aes(color = "Bonferroni constant &epsilon;", linetype = "&epsilon;<sub>efficacy</sub>", yintercept = 1.0025 - .71), size = 1) +
  annotate(geom = "point", x = seq(15, 60, 15), y = 1.0025 - .71, color = "#000e0e", size = 2.5) +
  geom_hline(aes(color = "Bonferroni constant &epsilon;", linetype = "&epsilon;<sub>non toxicity</sub>", yintercept = 1 - .41), size = 1) +
  annotate(geom = "point", x = seq(15, 60, 15), y = 1 - .41, color = "#000e0e", size = 2.5) +
  geom_hline(aes(color = "&epsilon;<sup>s</sup>", linetype = "&epsilon;<sub>efficacy</sub>", yintercept = 1 - .29), size = 1) +
  annotate(geom = "point", x = seq(15, 60, 15), y = 1 - .29, color = "#dc7f16", size = 2.5) +
  geom_hline(aes(color = "&epsilon;<sup>s</sup>", linetype = "&epsilon;<sub>non toxicity</sub>", yintercept = 1 - .63), size = 1) +
  annotate(geom = "point", x = seq(15, 60, 15), y = 1 - .63, color = "#dc7f16", size = 2.5) +
  scale_color_manual(name = "Type of threshold", values = c("#40a746", "#000e0e", "#c63b1d", "#dc7f16", "#2b4ebb", "#a71eb7")) +
  scale_linetype_manual(name = "Constant threshold", values = c("solid", "dotted")) +
  scale_x_continuous(name = "Number of patients", expand = c(0, 1), breaks = seq(0, 60, 15), limits = c(0, 60)) +
  scale_y_continuous(name = "P(p<sub>eff,k</sub> < &phi;<sub>eff</sub> | D<sub>n,k</sub>) or P(p<sub>no tox,k</sub> < &phi;<sub>no tox</sub> | D<sub>n,k</sub>)", limits = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, .2)) +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Threshold during the trial",
       caption = "Stop if :<br>
       - Pr(p<sub>eff,k</sub> < &phi;<sub>eff</sub> | D<sub>n,k</sub>) > C<sub>n</sub> or P(p<sub>no tox,k</sub> < &phi;<sub>no tox</sub> | D<sub>n,k</sub>) > C<sub>n</sub><br>
       - Pr(p<sub>eff,k</sub> < &phi;<sub>eff</sub> | D<sub>n,k</sub>) > &epsilon;<sub>efficacy</sub> or P(p<sub>no tox,k</sub> < &phi;<sub>no tox</sub> | D<sub>n,k</sub>) > &epsilon;<sub>non toxicity</sub>")
if (sauvegarder) {ggsave(plot_seuils,
                         filename = "Outputs/simu_vsref/seuils.png",
                         device = "png", height = 10, width = 8)}

# Holm threshold
plot_seuils_holm <- ggplot() +
  geom_function(fun = ~ calcul_cn_holm(.x, 3, 60, .535, .8), aes(color = "3 active arms"), size = 1) +
  geom_function(fun = ~ calcul_cn_holm(.x, 2, 60, .535, .8), aes(color = "2 active arms"), size = 1) +
  geom_function(fun = ~ calcul_cn_holm(.x, 1, 60, .535, .8), aes(color = "1 active arm"), size = 1) +
  annotate(geom = "point", x = seq(15, 60, 15), y = calcul_cn_holm(abscisse = seq(15, 60, 15), BrasAct = 3, N = 60, C = .535, Gamm = .8), color = "#c63b1d", size = 2.5) +
  annotate(geom = "point", x = seq(15, 60, 15), y = calcul_cn_holm(abscisse = seq(15, 60, 15), BrasAct = 2, N = 60, C = .535, Gamm = .8), color = "#000e0e", size = 2.5) +
  annotate(geom = "point", x = seq(15, 60, 15), y = calcul_cn_holm(abscisse = seq(15, 60, 15), BrasAct = 1, N = 60, C = .535, Gamm = .8), color = "#40a746", size = 2.5) +
  scale_color_manual(name = "Number of active arms", values = c("#40a746", "#000e0e", "#c63b1d")) +
  scale_x_continuous(name = "Number of patients", expand = c(0, 1), breaks = seq(0, 60, 15), limits = c(0, 60)) +
  scale_y_continuous(name = "P(p<sub>eff,k</sub> < &phi;<sub>eff</sub> | D<sub>n,k</sub>) or P(p<sub>no tox,k</sub> < &phi;<sub>no tox</sub> | D<sub>n,k</sub>)", limits = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, .2)) +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Holm threshold during the trial",
       caption = "Stop if :<br>
       - Pr(p<sub>eff,k</sub> < &phi;<sub>eff</sub> | D<sub>n,k</sub>) > C<sub>n</sub> or P(p<sub>no tox,k</sub> < &phi;<sub>no tox</sub> | D<sub>n,k</sub>) > C<sub>n</sub>")
if (sauvegarder) {ggsave(plot_seuils_holm,
                         filename = "Outputs/simu_vsref/seuils_holm.png",
                         device = "png", height = 10, width = 8)}

# II/ Controlled settings ----

# Without Holm threshold
plot_seuils <- ggplot() +
  geom_function(fun = ~ calcul_cn(.x, 60, .715, .96), aes(color = "C<sub>n</sub><sup>m</sup>"), size = 1) +
  geom_function(fun = ~ calcul_cn(.x, 60, .715, .96) + .005, aes(color = "Bonferroni C<sub>n</sub>"), size = 1) +
  geom_function(fun = ~ calcul_cn(.x, 60, .575, .98), aes(color = "C<sub>n</sub><sup>s</sup>"), size = 1) +
  annotate(geom = "point", x = c(15, 30, 45, 60), y = calcul_cn(abscisse = c(15, 30, 45, 60), N = 60, C = .715, Gamm = .96), color = "#2b4ebb", size = 2.5) +
  annotate(geom = "point", x = c(15, 30, 45, 60), y = calcul_cn(abscisse = c(15, 30, 45, 60), N = 60, C = .715, Gamm = .96) + .005, color = "#40a746", size = 2.5) +
  annotate(geom = "point", x = c(15, 30, 45, 60), y = calcul_cn(abscisse = c(15, 30, 45, 60), N = 60, C = .575, Gamm = .98), color = "#c63b1d", size = 2.5) +
  geom_hline(aes(color = "&epsilon;<sup>m</sup>", linetype = "&epsilon;<sub>efficacy</sub>", yintercept = 1 - .61), size = 1) +
  annotate(geom = "point", x = c(15, 30, 45, 60), y = 1 - .61, color = "#a71eb7", size = 2.5) +
  geom_hline(aes(color = "&epsilon;<sup>m</sup>", linetype = "&epsilon;<sub>non toxicity</sub>", yintercept = 1 - .43), size = 1) +
  annotate(geom = "point", x = c(15, 30, 45, 60), y = 1 - .43, color = "#a71eb7", size = 2.5) +
  geom_hline(aes(color = "Bonferroni constant &epsilon;", linetype = "&epsilon;<sub>efficacy</sub>", yintercept = 1 - .65), size = 1) +
  annotate(geom = "point", x = c(15, 30, 45, 60), y = 1 - .65, color = "#000e0e", size = 2.5) +
  geom_hline(aes(color = "Bonferroni constant &epsilon;", linetype = "&epsilon;<sub>non toxicity</sub>", yintercept = 1.0025 - .43), size = 1) +
  annotate(geom = "point", x = c(15, 30, 45, 60), y = 1.0025 - .43, color = "#000e0e", size = 2.5) +
  geom_hline(aes(color = "&epsilon;<sup>s</sup>", linetype = "&epsilon;<sub>efficacy</sub>", yintercept = 1.0025 - .61), size = 1) +
  annotate(geom = "point", x = c(15, 30, 45, 60), y = 1.0025 - .61, color = "#dc7f16", size = 2.5) +
  geom_hline(aes(color = "&epsilon;<sup>s</sup>", linetype = "&epsilon;<sub>non toxicity</sub>", yintercept = 1 - .15), size = 1) +
  annotate(geom = "point", x = c(15, 30, 45, 60), y = 1 - .15, color = "#dc7f16", size = 2.5) +
  scale_color_manual(name = "Type of threshold", values = c("#40a746", "#000e0e", "#c63b1d", "#dc7f16", "#2b4ebb", "#a71eb7")) +
  scale_linetype_manual(name = "Constant threshold", values = c("solid", "dotted")) +
  scale_x_continuous(name = "Number of patients", expand = c(0, 1), breaks = c(0, 15, 30, 45, 60), limits = c(0, 60)) +
  scale_y_continuous(name = "Pr(p<sub>eff,k</sub> < p<sub>eff,0</sub> | D<sub>n</sub>) or Pr(p<sub>no tox,k</sub> < p<sub>no tox,0</sub> | D<sub>n</sub>)", limits = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, .2)) +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Threshold during the trial",
       caption = "Stop if :<br>
       - Pr(p<sub>eff,k</sub> < p<sub>eff,0</sub> | D<sub>n</sub>) > C<sub>n</sub> or Pr(p<sub>no tox,k</sub> < p<sub>no tox,0</sub> | D<sub>n</sub>) > C<sub>n</sub><br>
       - Pr(p<sub>eff,k</sub> < p<sub>eff,0</sub> | D<sub>n</sub>) > &epsilon;<sub>efficacy</sub> or Pr(p<sub>no tox,k</sub> < p<sub>no tox,0</sub> | D<sub>n</sub>) > &epsilon;<sub>non toxicity</sub>")
if (sauvegarder) {ggsave(plot_seuils,
                         filename = "Outputs/simu_vscont/seuils.png",
                         device = "png", height = 10, width = 8)}

# Holm threshold only
plot_seuils_holm <- ggplot() +
  geom_function(fun = ~ calcul_cn_holm(.x, 3, 60, .55, .99), aes(color = "3 active arms"), size = 1) +
  geom_function(fun = ~ calcul_cn_holm(.x, 2, 60, .55, .99), aes(color = "2 active arms"), size = 1) +
  geom_function(fun = ~ calcul_cn_holm(.x, 1, 60, .55, .99), aes(color = "1 active arm"), size = 1) +
  annotate(geom = "point", x = seq(15, 60, 15), y = calcul_cn_holm(abscisse = seq(15, 60, 15), BrasAct = 3, N = 60, C = .55, Gamm = .99), color = "#c63b1d", size = 2.5) +
  annotate(geom = "point", x = seq(15, 60, 15), y = calcul_cn_holm(abscisse = seq(15, 60, 15), BrasAct = 2, N = 60, C = .55, Gamm = .99), color = "#000e0e", size = 2.5) +
  annotate(geom = "point", x = seq(15, 60, 15), y = calcul_cn_holm(abscisse = seq(15, 60, 15), BrasAct = 1, N = 60, C = .55, Gamm = .99), color = "#40a746", size = 2.5) +
  scale_color_manual(name = "Number of active arms", values = c("#40a746", "#000e0e", "#c63b1d")) +
  scale_x_continuous(name = "Number of patients", expand = c(0, 1), breaks = seq(0, 60, 15), limits = c(0, 60)) +
  scale_y_continuous(name = "P(p<sub>eff,k</sub> < &phi;<sub>eff</sub> | D<sub>n,k</sub>) or P(p<sub>no tox,k</sub> < &phi;<sub>no tox</sub> | D<sub>n,k</sub>)", limits = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, .2)) +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Holm threshold during the trial",
       caption = "Stop if :<br>
       - Pr(p<sub>eff,k</sub> < &phi;<sub>eff</sub> | D<sub>n,k</sub>) > C<sub>n</sub> or P(p<sub>no tox,k</sub> < &phi;<sub>no tox</sub> | D<sub>n,k</sub>) > C<sub>n</sub>")
if (sauvegarder) {ggsave(plot_seuils_holm,
                         filename = "Outputs/simu_vscont/seuils_holm.png",
                         device = "png", height = 10, width = 8)}


# III/ All designs on one plot ----

## A/ Uncontrolled settings ----

# Futility
plot_fut_unc <- ggplot() +
  annotate("ribbon", x = c(seq(0, 59, 1), 59, 60), 
           ymin = c(calcul_cn_holm(seq(0, 29, 1), 3, 60, .535, .8), calcul_cn_holm(c(seq(30, 59, 1), 59, 60), 1, 60, .535, .8)), 
           ymax = c(calcul_cn_holm(seq(0, 59, 1), 3, 60, .535, .8), calcul_cn(c(59, 60), 60, .63, .93)), alpha = .3, fill = "#40a746") +
  geom_function(fun = ~ calcul_cn(.x, 60, .78, .9), aes(color = "C<sub>n</sub><sup>m</sup>"), size = 1) +
  geom_function(fun = ~ calcul_cn(.x, 60, .63, .93), aes(color = "C<sub>n</sub>"), size = 1) +
  geom_line(mapping = aes(x = c(seq(0, 59, 1), 59, 60), 
           y = c(calcul_cn_holm(seq(0, 29, 1), 3, 60, .535, .8), calcul_cn_holm(c(seq(30, 59, 1), 59, 60), 1, 60, .535, .8)),
           color = "C<sub>n</sub><sup>m,a</sup>"), size = 1) +
  geom_line(mapping = aes(x = c(seq(0, 59, 1), 59, 60), 
           y = c(calcul_cn_holm(seq(0, 59, 1), 3, 60, .535, .8), calcul_cn(c(59, 60), 60, .63, .93)),
           color = "C<sub>n</sub><sup>m,a</sup>"), size = 1) +
  # geom_function(fun = ~ ifelse(.x < 59, calcul_cn_holm(.x, 3, 60, .535, .8), calcul_cn(.x, 60, .63, .93)), aes(color = "C<sub>n</sub><sup>m,a</sup>"), size = 1) +
  # geom_function(fun = ~ ifelse(.x < 30, calcul_cn_holm(.x, 3, 60, .535, .8), calcul_cn_holm(.x, 1, 60, .535, .8)), 
  #               aes(color = "C<sub>n</sub><sup>m,a</sup>"), size = 1) +
  geom_point(data = data.frame(x = seq(15, 60, 15), y = calcul_cn(abscisse = seq(15, 60, 15), N = 60, C = .78, Gamm = .9)),
             aes(x = x, y = y, color = "C<sub>n</sub><sup>m</sup>", shape = "C<sub>n</sub><sup>m</sup>"), size = 4) +
  geom_point(data = data.frame(x = seq(0, 60, 15), y = NA_real_),
             aes(x = x, y = y, color = "C<sub>n</sub><sup>m,a</sup>", shape = "C<sub>n</sub><sup>m,a</sup>"), size = 4) +
  geom_point(data = data.frame(x = seq(15, 60, 15), y = calcul_cn(abscisse = seq(15, 60, 15), N = 60, C = .63, Gamm = .93)),
             aes(x = x, y = y, color = "C<sub>n</sub>", shape = "C<sub>n</sub>"), size = 4) +
  geom_segment(aes(color = "&epsilon;<sup>m</sup>", x = 0, xend = 60, y = 1 - .71, yend = 1 - .71), size = 1) +
  geom_point(data = data.frame(x = seq(15, 60, 15), y = 1 - .71),
             aes(x = x, y = y, color = "&epsilon;<sup>m</sup>", shape = "&epsilon;<sup>m</sup>"), size = 4) +
  scale_shape_manual(name = if (couleur != "français") {"Type of threshold"} else {"Type de seuil"},
                     values = c(19, 17, NA, 18),
                     labels = if (couleur != "français") {waiver()} else {c("C<sub>n</sub> AA", "C<sub>n</sub> mono-bras", "C<sub>n</sub> multi-bras", "&epsilon; multi-bras")}) +
  scale_color_manual(name = if (couleur != "français") {"Type of threshold"} else {"Type de seuil"}, 
                     values = c("#c63b1d", "darkblue", "#40a746", "#dc7f16"),
                     labels = if (couleur != "français") {waiver()} else {c("C<sub>n</sub> AA", "C<sub>n</sub> mono-bras", "C<sub>n</sub> multi-bras", "&epsilon; multi-bras")}) +
  scale_x_continuous(name = if (couleur != "français") {"Number of enrolled patients\nin each arm"} else {"Nombre de patients recrutés dans chaque bras"}, 
                     expand = c(0, 1), breaks = seq(0, 60, 15), limits = c(0, 60)) +
  scale_y_continuous(name = "C<sub>n</sub> / &epsilon;<sub>Eff</sub>", limits = c(0, 1), expand = c(0, .01), breaks = seq(0, 1, .2)) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text = element_markdown(size = 14),
        legend.title = element_markdown(size = 20)) +
  guides(shape = guide_legend(override.aes = list(size = c(5, 5, 1.25, 5), linetype = c(0, 0, 1, 0))))
# legende <- get_legend(plot_fut_unc)
legende <- ggplotGrob(plot_fut_unc)$grob[[17]]


# Toxicity
plot_tox_unc <- ggplot() +
  annotate("ribbon", x = c(seq(0, 59, 1), 59, 60), 
           ymin = c(calcul_cn_holm(seq(0, 29, 1), 3, 60, .535, .8), calcul_cn_holm(c(seq(30, 59, 1), 59, 60), 1, 60, .535, .8)), 
           ymax = c(calcul_cn_holm(seq(0, 59, 1), 3, 60, .535, .8), calcul_cn(c(59, 60), 60, .63, .93)), alpha = .3, fill = "#40a746") +
  geom_function(fun = ~ calcul_cn(.x, 60, .78, .9), aes(color = "C<sub>n</sub><sup>m</sup>"), size = 1) +
  geom_function(fun = ~ calcul_cn(.x, 60, .63, .93), aes(color = "C<sub>n</sub>"), size = 1) +
  geom_line(mapping = aes(x = c(seq(0, 59, 1), 59, 60), 
                          y = c(calcul_cn_holm(seq(0, 29, 1), 3, 60, .535, .8), calcul_cn_holm(c(seq(30, 59, 1), 59, 60), 1, 60, .535, .8)),
                          color = "C<sub>n</sub><sup>m,a</sup>"), size = 1) +
  geom_line(mapping = aes(x = c(seq(0, 59, 1), 59, 60), 
                          y = c(calcul_cn_holm(seq(0, 59, 1), 3, 60, .535, .8), calcul_cn(c(59, 60), 60, .63, .93)),
                          color = "C<sub>n</sub><sup>m,a</sup>"), size = 1) +
  # geom_function(fun = ~ ifelse(.x < 59, calcul_cn_holm(.x, 3, 60, .535, .8), calcul_cn(.x, 60, .63, .93)), aes(color = "C<sub>n</sub><sup>m,a</sup>"), size = 1) +
  # geom_function(fun = ~ ifelse(.x < 30, calcul_cn_holm(.x, 3, 60, .535, .8), calcul_cn_holm(.x, 1, 60, .535, .8)), aes(color = "C<sub>n</sub><sup>m,a</sup>"), size = 1) +
  geom_point(data = data.frame(x = seq(15, 60, 15), y = calcul_cn(abscisse = seq(15, 60, 15), N = 60, C = .78, Gamm = .9)),
             aes(x = x, y = y, color = "C<sub>n</sub><sup>m</sup>", shape = "C<sub>n</sub><sup>m</sup>"), size = 4) +
  geom_point(data = data.frame(x = seq(0, 60, 15), y = NA_real_),
             aes(x = x, y = y, color = "C<sub>n</sub><sup>m,a</sup>", shape = "C<sub>n</sub><sup>m,a</sup>"), size = 4) +
  geom_point(data = data.frame(x = seq(15, 60, 15), y = calcul_cn(abscisse = seq(15, 60, 15), N = 60, C = .63, Gamm = .93)),
             aes(x = x, y = y, color = "C<sub>n</sub>", shape = "C<sub>n</sub>"), size = 4) +
  geom_segment(aes(color = "&epsilon;<sup>m</sup>", x = 0, xend = 60, y = 1 - .43, yend = 1 - .43), size = 1) +
  geom_point(data = data.frame(x = seq(15, 60, 15), y = 1 - .43),
             aes(x = x, y = y, color = "&epsilon;<sup>m</sup>", shape = "&epsilon;<sup>m</sup>"), size = 4) +
  scale_shape_manual(name = if (couleur != "français") {"Type of threshold"} else {"Type de seuil"},
                     values = c(19, 17, NA, 18),
                     labels = if (couleur != "français") {waiver()} else {c("C<sub>n</sub> AA", "C<sub>n</sub> mono-bras", "C<sub>n</sub> multi-bras", "&epsilon; multi-bras")}) +
  scale_color_manual(name = if (couleur != "français") {"Type of threshold"} else {"Type de seuil"}, 
                     values = c("#c63b1d", "darkblue", "#40a746", "#dc7f16"),
                     labels = if (couleur != "français") {waiver()} else {c("C<sub>n</sub> AA", "C<sub>n</sub> mono-bras", "C<sub>n</sub> multi-bras", "&epsilon; multi-bras")}) +
  scale_x_continuous(name = if (couleur != "français") {"Number of enrolled patients\nin each arm"} else {"Nombre de patients recrutés dans chaque bras"}, 
                     expand = c(0, 1), breaks = seq(0, 60, 15), limits = c(0, 60)) +
  scale_y_continuous(name = "C<sub>n</sub> / &epsilon;<sub>Tox</sub>", limits = c(0, 1), expand = c(0, .01), breaks = seq(0, 1, .2)) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        legend.text = element_markdown(size = 14),
        legend.title = element_markdown(size = 20))

## B/ Controlled settings ----

# Futility
plot_fut_con <-   ggplot() +
  annotate("ribbon", x = c(seq(0, 59, 1), 59, 60), 
           ymin = c(calcul_cn_holm(seq(0, 29, 1), 3, 60, .55, .99), calcul_cn_holm(c(seq(30, 59, 1), 59, 60), 1, 60, .55, .99)),
           ymax = c(calcul_cn_holm(seq(0, 59, 1), 3, 60, .55, .99), calcul_cn(c(59, 60), 60, .575, .98)), alpha = .3, fill = "#40a746") +
  geom_function(fun = ~ calcul_cn(.x, 60, .715, .96), aes(color = "C<sub>n</sub><sup>m</sup>"), size = 1) +
  # geom_function(fun = ~ calcul_cn(.x, 60, .575, .98), aes(color = "C<sub>n</sub><sup>s</sup>"), size = 1) +
  geom_line(mapping = aes(x = c(seq(0, 59, 1), 59, 60), 
                          y = c(calcul_cn_holm(seq(0, 29, 1), 3, 60, .55, .99), calcul_cn_holm(c(seq(30, 59, 1), 59, 60), 1, 60, .575, .98)),
                          color = "C<sub>n</sub><sup>m,a</sup>"), size = 1) +
  geom_line(mapping = aes(x = c(seq(0, 59, 1), 59, 60), 
                          y = c(calcul_cn_holm(seq(0, 59, 1), 3, 60, .55, .99), calcul_cn(c(59, 60), 60, .575, .98)),
                          color = "C<sub>n</sub><sup>m,a</sup>"), size = 1) +
  # geom_function(fun = ~ ifelse(.x < 59, calcul_cn_holm(.x, 3, 60, .55, .99), calcul_cn(.x, 60, .575, .98)), aes(color = "C<sub>n</sub><sup>m,a</sup>"), size = 1) +
  # geom_function(fun = ~ ifelse(.x < 30, calcul_cn_holm(.x, 3, 60, .55, .99), calcul_cn_holm(.x, 1, 60, .55, .99)),
  #                              aes(color = "C<sub>n</sub><sup>m,a</sup>"), size = 1) +
  geom_point(data = data.frame(x = seq(15, 60, 15), y = calcul_cn(abscisse = seq(15, 60, 15), N = 60, C = .715, Gamm = .96)),
             aes(x = x, y = y, color = "C<sub>n</sub><sup>m</sup>", shape = "C<sub>n</sub><sup>m</sup>"), size = 4) +
  geom_point(data = data.frame(x = seq(0, 60, 15), y = NA_real_),
             aes(x = x, y = y, color = "C<sub>n</sub><sup>m,a</sup>", shape = "C<sub>n</sub><sup>m,a</sup>"), size = 4) +
  # geom_point(data = data.frame(x = seq(15, 60, 15), y = calcul_cn(abscisse = seq(15, 60, 15), N = 60, C = .575, Gamm = .98)),
  #            aes(x = x, y = y, color = "C<sub>n</sub><sup>s</sup>", shape = "C<sub>n</sub><sup>s</sup>"), size = 4) +
  geom_segment(aes(color = "&epsilon;<sup>m</sup>", x = 0, xend = 60, y = 1 - .61, yend = 1 - .61), size = 1) +
  geom_point(data = data.frame(x = seq(15, 60, 15), y = 1 - .61),
             aes(x = x, y = y, color = "&epsilon;<sup>m</sup>", shape = "&epsilon;<sup>m</sup>"), size = 4) +
  scale_shape_manual(name = if (couleur != "français") {"Type of threshold"} else {"Type de seuil"},
                     values = c(19, NA, 17),
                     labels = if (couleur != "français") {waiver()} else {c("C<sub>n</sub> AA", "C<sub>n</sub> mono-bras", "C<sub>n</sub> multi-bras", "&epsilon; multi-bras")}) +
  scale_color_manual(name = if (couleur != "français") {"Type of threshold"} else {"Type de seuil"}, 
                     values = c("#c63b1d", "#40a746", "#dc7f16"),
                     labels = if (couleur != "français") {waiver()} else {c("C<sub>n</sub> AA", "C<sub>n</sub> mono-bras", "C<sub>n</sub> multi-bras", "&epsilon; multi-bras")}) +
  scale_x_continuous(name = if (couleur != "français") {"Number of enrolled patients in each arm"} else {"Nombre de patients recrutés dans chaque bras"}, 
                     expand = c(0, 1), breaks = seq(0, 60, 15), limits = c(0, 60)) +
  scale_y_continuous(name = "C<sub>n</sub> / &epsilon;<sub>Eff</sub>", limits = c(0, 1), expand = c(0, .01), breaks = seq(0, 1, .2)) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        legend.text = element_markdown(size = 14),
        legend.title = element_markdown(size = 20)) 

# Toxicity
plot_tox_con <- ggplot() +
  annotate("ribbon", x = c(seq(0, 59, 1), 59, 60), 
           ymin = c(calcul_cn_holm(seq(0, 29, 1), 3, 60, .55, .99), calcul_cn_holm(c(seq(30, 59, 1), 59, 60), 1, 60, .55, .99)), 
           ymax = c(calcul_cn_holm(seq(0, 59, 1), 3, 60, .55, .99), calcul_cn(c(59, 60), 60, .575, .98)), alpha = .3, fill = "#40a746") +
  geom_function(fun = ~ calcul_cn(.x, 60, .715, .96), aes(color = "C<sub>n</sub><sup>m</sup>"), size = 1) +
  # geom_function(fun = ~ calcul_cn(.x, 60, .575, .98), aes(color = "C<sub>n</sub><sup>s</sup>"), size = 1) +
  geom_line(mapping = aes(x = c(seq(0, 59, 1), 59, 60), 
                          y = c(calcul_cn_holm(seq(0, 29, 1), 3, 60, .55, .99), calcul_cn_holm(c(seq(30, 59, 1), 59, 60), 1, 60, .575, .98)),
                          color = "C<sub>n</sub><sup>m,a</sup>"), size = 1) +
  geom_line(mapping = aes(x = c(seq(0, 59, 1), 59, 60), 
                          y = c(calcul_cn_holm(seq(0, 59, 1), 3, 60, .55, .99), calcul_cn(c(59, 60), 60, .575, .98)),
                          color = "C<sub>n</sub><sup>m,a</sup>"), size = 1) +
  # geom_function(fun = ~ ifelse(.x < 59, calcul_cn_holm(.x, 3, 60, .55, .99), calcul_cn(.x, 60, .575, .98)), aes(color = "C<sub>n</sub><sup>m,a</sup>"), size = 1) +
  # geom_function(fun = ~ ifelse(.x < 30, calcul_cn_holm(.x, 3, 60, .55, .99), calcul_cn_holm(.x, 1, 60, .55, .99)), 
  #               aes(color = "C<sub>n</sub><sup>m,a</sup>"), size = 1) +
  geom_point(data = data.frame(x = seq(15, 60, 15), y = calcul_cn(abscisse = seq(15, 60, 15), N = 60, C = .715, Gamm = .96)),
             aes(x = x, y = y, color = "C<sub>n</sub><sup>m</sup>", shape = "C<sub>n</sub><sup>m</sup>"), size = 4) +
  geom_point(data = data.frame(x = seq(0, 60, 15), y = NA_real_),
             aes(x = x, y = y, color = "C<sub>n</sub><sup>m,a</sup>", shape = "C<sub>n</sub><sup>m,a</sup>"), size = 4) +
  # geom_point(data = data.frame(x = seq(15, 60, 15), y = calcul_cn(abscisse = seq(15, 60, 15), N = 60, C = .575, Gamm = .98)),
  #            aes(x = x, y = y, color = "C<sub>n</sub><sup>s</sup>", shape = "C<sub>n</sub><sup>s</sup>"), size = 4) +
  geom_segment(aes(color = "&epsilon;<sup>m</sup>", x = 0, xend = 60, y = 1 - .43, yend = 1 - .43), size = 1) +
  geom_point(data = data.frame(x = seq(15, 60, 15), y = 1 - .43),
             aes(x = x, y = y, color = "&epsilon;<sup>m</sup>", shape = "&epsilon;<sup>m</sup>"), size = 4) +
  scale_shape_manual(name = if (couleur != "français") {"Type of threshold"} else {"Type de seuil"},
                     values = c(19, NA, 17),
                     labels = if (couleur != "français") {waiver()} else {c("C<sub>n</sub> AA", "C<sub>n</sub> mono-bras", "C<sub>n</sub> multi-bras", "&epsilon; multi-bras")}) +
  scale_color_manual(name = if (couleur != "français") {"Type of threshold"} else {"Type de seuil"}, 
                     values = c("#c63b1d", "#40a746", "#dc7f16"),
                     labels = if (couleur != "français") {waiver()} else {c("C<sub>n</sub> AA", "C<sub>n</sub> mono-bras", "C<sub>n</sub> multi-bras", "&epsilon; multi-bras")}) +
  scale_x_continuous(name = if (couleur != "français") {"Number of enrolled patients in each arm"} else {"Nombre de patients recrutés dans chaque bras"}, 
                     expand = c(0, 1), breaks = seq(0, 60, 15), limits = c(0, 60)) +
  scale_y_continuous(name = "C<sub>n</sub> / &epsilon;<sub>Tox</sub>", limits = c(0, 1), expand = c(0, .01), breaks = seq(0, 1, .2)) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        legend.text = element_markdown(size = 14),
        legend.title = element_markdown(size = 20))

## C/ All 4 plots ----

col_milieu <- (plot_spacer() / 
                 textGrob(label = "Futility stopping", gp = gpar(fontface = "bold", cex = 1.2)) /
                 (plot_fut_unc + theme(legend.position = "none"))) +
  plot_layout(heights = c(.1, .4, 7))
col_droite <- (plot_spacer() / 
                 textGrob(label = "Toxicity stopping", gp = gpar(fontface = "bold", cex = 1.2)) /
                 plot_tox_unc) +
  plot_layout(heights = c(.1, .4, 7))
plot_seuils <- (col_milieu | col_droite) + plot_layout(widths = c(7, 7))
plot_seuils <- (plot_seuils / legende) + plot_layout(heights = c(9, 1))  +
  plot_annotation(tag_levels = list(c("", "A", "", "B", ""))) &
  theme(plot.tag = element_text(face = "bold", size = 15))
# col_gauche <- (plot_spacer() / plot_spacer() / 
#                  textGrob(label = "Uncontrolled setting", rot = 90, gp = gpar(fontface = "bold", cex = 1.2)) / 
#                  textGrob(label = "Controlled setting", rot = 90, gp = gpar(fontface = "bold", cex = 1.2))) +
#   plot_layout(heights = c(.1, .4, 7, 7))
# col_milieu <- (plot_spacer() / 
#                  textGrob(label = "Futility stopping", gp = gpar(fontface = "bold", cex = 1.2)) /
#                  (plot_fut_unc + theme(legend.position = "none")) /
#                  plot_fut_con) +
#   plot_layout(heights = c(.1, .4, 7, 7))
# col_droite <- (plot_spacer() / 
#                  textGrob(label = "Toxicity stopping", gp = gpar(fontface = "bold", cex = 1.2)) /
#                  plot_tox_unc /
#                  plot_tox_con) +
#   plot_layout(heights = c(.1, .4, 7, 7))
# plot_seuils <- (col_gauche | col_milieu | col_droite) + plot_layout(widths = c(.5, 7, 7))
# plot_seuils <- (plot_seuils / legende) + plot_layout(heights = c(9, 1))  +
#   plot_annotation(tag_levels = list(c("", "", "", "A", "C", "", "B", "D", ""))) &
#   theme(plot.tag = element_text(face = "bold", size = 15))
if (sauvegarder) {
  ggsave(plot_seuils,
         filename = "Outputs/seuils.png",
         device = "png", height = 11, width = 10)
  ggsave(plot_seuils,
         filename = "Outputs/seuils_pres.jpeg",
         device = "jpeg", height = 10, width = 15, units = "cm")
  ggsave(plot = plot_seuils, device = cairo_ps, units = "px",
         filename = "Outputs/seuils.eps", dpi = 800, width = 8000, height = 7000)
}


# IV/ Spaghetti plots for all possibilities for Holm Cn ----

# Uncontrolled setting
holm_uncon <- expand.grid(a0 = 3, a15 = 3, a30 = 1:3, a45 = 1:3, a60 = 1:3) %>% 
  filter(a45 <= a30, a60 <= a45) %>% 
  mutate(group = row_number()) %>% 
  pivot_longer(-group, names_pattern = "^a(\\d{1,2})$", names_to = "nb_pts", values_to = "bras_act", names_transform = list(nb_pts = as.integer)) %>% 
  mutate(seuil = calcul_cn_holm(nb_pts, bras_act, 60, .535, .8) - group * .001) %>% 
  ggplot(aes(nb_pts, seuil, group = group, color = factor(group))) +
  geom_line(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  ylim(0, 1) +
  labs(title = "Uncontrolled design",
       y = "Holm C<sub>n</sub>",
       x = "Number of patients at analysis")
# Controlled setting
holm_con <- expand.grid(a0 = 3, a15 = 3, a30 = 1:3, a45 = 1:3, a60 = 1:3) %>% 
  filter(a45 <= a30, a60 <= a45) %>% 
  mutate(group = row_number()) %>% 
  pivot_longer(-group, names_pattern = "^a(\\d{1,2})$", names_to = "nb_pts", values_to = "bras_act", names_transform = list(nb_pts = as.integer)) %>% 
  mutate(seuil = calcul_cn_holm(nb_pts, bras_act, 60, .55, .99) - group * .001) %>% 
  ggplot(aes(nb_pts, seuil, group = group, color = factor(group))) +
  geom_line(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  ylim(0, 1) +
  labs(title = "Controlled design",
       y = "Holm C<sub>n</sub>",
       x = "Number of patients at analysis")
# Whole plot
plot_total <- holm_uncon | holm_con
if (sauvegarder) {
  ggsave(plot_total,
         filename = "Outputs/seuils_holm.png",
         device = "png", height = 11, width = 10)
  ggsave(plot = plot_total, device = cairo_ps, units = "px",
         filename = "Outputs/seuils_holm.eps", dpi = 800, width = 8000, height = 7000)
}
