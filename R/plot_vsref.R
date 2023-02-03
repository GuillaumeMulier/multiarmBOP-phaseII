# ---------------------------------------------- #
# Generation of figures in uncontrolled settings #
# Créé le 26/04/2021, modifié le 24/01/2023      #
# ---------------------------------------------- #

# Packages and helpers ----

library(tidyverse)
library(ggtext)
library(scales)
library(cowplot)
library(patchwork)
library(flextable)
library(officer)
library(glue)

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

table_scenar <- tribble(~scenar, ~proba, ~num_scenar,
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
table_scenar_aggreg <- tribble(~scenar, ~proba, ~num_scenar,
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

clear <- TRUE # TRUE if you want to remove unused objects as the script goes on
sauvegarder <- TRUE # TRUE if you want to save the figures for the article


# I/ 13 scenarios with uncontrolled design ----

load("Data/simu_vsref/donnees_graphes_ref_20221205.Rdata")

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

data_ggplot_13vsref <- table_scenar_aggreg %>% 
  right_join(data_ggplot_13vsref, by = c("scenar", "name" = "ttt")) %>% 
  mutate(num_scenar = factor(num_scenar, levels = c(13:1)))

## Proportion of reject of H0 ----

# With the 6 thresholds
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

# Only 4 thresholds for the article
plot1 <- ggplot(data_ggplot_13vsref %>% filter(!methode %in% c("Bonferroni C<sub>n</sub>", "Bonferroni &epsilon;", "&epsilon;<sup>s</sup>")) %>%
                  mutate(methode = droplevels(methode))) +
  geom_col(aes(x = num_scenar, y = rejet_h0, fill = etiquettes), color = "black",
           position = position_dodge2(width = .45), show.legend = FALSE) +
  geom_vline(xintercept = c(1:12 + .5), color = "#b1b0b1", linetype = "dashed") +
  geom_richtext(data = tab_Ralpha %>%
                  filter(!methode %in% c("Bonferroni C<sub>n</sub>", "Bonferroni &epsilon;", "Monoarm &epsilon;")) %>% 
                  mutate(methode = str_replace_all(methode, "Multiarm", "Multi-arm"),
                         methode = str_replace_all(methode, "Monoarm", "Single-arm"),
                         methode = str_replace_all(methode, "^Holm (.*)$", "Multi-arm \\1<sup>(aa)</sup>"), 
                         methode = factor(methode, 
                                          levels = c("Multi-arm C<sub>n</sub>", "Multi-arm C<sub>n</sub><sup>(aa)</sup>", "Single-arm C<sub>n</sub>", "Multi-arm &epsilon;", "Bonferroni &epsilon;", "Single-arm &epsilon;"),
                                          labels = c("C<sub>n</sub><sup>m</sup>", "C<sub>n</sub><sup>m,a</sup>", "C<sub>n</sub><sup>s</sup>", "&epsilon;<sup>m</sup>", "Bonferroni &epsilon;", "&epsilon;<sup>s</sup>"))), 
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
if (sauvegarder) {
  ggsave(plot_tot,
         filename = "Outputs/simu_vsref/plot_13scenar_barplot_2.png",
         device = "png", height = 13, width = 16)
  ggsave(plot_tot,
         filename = "Outputs/simu_vsref/plot_13scenar_barplot_2.jpeg",
         device = "jpeg", height = 13, width = 16)
  ggsave(plot_tot, units = "px", dpi = 800,
         filename = "Outputs/simu_vsref/plot_13scenar_barplot_2.eps",
         device = cairo_ps, height = 10000, width = 12500)
}
if (clear) rm(plot1, plot2, plot_tot)


## Stoppings ----

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
                  filter(methode =="Multi-arm C<sub>n</sub>") %>%
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
plot_epsi <- (plot2 | wrap_plots(map(map(4:6, ~ liste_plots[[.x]]), ~ .x + theme(legend.position = "none")))) + plot_layout(widths = c(1.5, 3))
plot_tot_int <- plot_cn / plot_epsi
plot_tot <- (plot_tot_int | legende) + plot_layout(widths = c(6, 1))
if (clear) rm(liste_plots, plot_cn, plot_epsi, plot_inter, plot_tot_int, plot_tot, plot2, legende, data_stackedbar)

## Reason for stoppings ----

data_stackedbar <- data_ggplot_13vsref %>% 
  filter(!str_detect(methode, "Bonferroni"), methode != "Single-arm &epsilon;") %>% 
  mutate(methode = str_replace_all(methode, "^Holm (.*)$", "Multi-arm \\1<sup>(aa)</sup>"),
         methode = factor(methode, levels = c("Multi-arm C<sub>n</sub>", "Multi-arm C<sub>n</sub><sup>(aa)</sup>", "Single-arm C<sub>n</sub>", "Multi-arm &epsilon;", "Bonferroni &epsilon;", "Single-arm &epsilon;")),
         arret_both = arret_fut + arret_tox - arret,
         arret_fut = arret_fut - arret_both,
         arret_tox = arret_tox - arret_both,
         nom_axe = paste0(num_scenar, ":", name),
         nom_axe = factor(nom_axe, levels = paste0(rep(13:1, each = 3), ":", rep(c("ttt3", "ttt2", "ttt1"), 13)), labels = paste0(rep(13:1, each = 3), ":", rep(c("Arm C", "Arm B", "Arm A"), 13)))) %>% 
  pivot_longer(cols = c(arret_fut, arret_tox, arret_both), names_to = "type_arret", values_to = "pourcent") 

liste_plots <- map(as.character(unique(data_stackedbar$methode))[c(1, 4, 2, 3)],
                   function(x) {
                     ggplot(data_stackedbar %>% filter(methode == x)) +
                       geom_col(aes(x = nom_axe, y = pourcent, fill = type_arret), 
                                color = "#000000", position = "stack", width = 0.8) +
                       geom_vline(xintercept = c((1:12) * 3 + .5), color = "#666c6c", linetype = "dashed", size = 1) +
                       facet_wrap(vars(methode)) +
                       theme(panel.grid.major.y = element_blank(),
                             panel.grid.major.x = element_line(color = "darkgrey", size = .7),
                             axis.ticks.y = element_blank(),
                             axis.text.y = element_blank(),
                             panel.grid.minor.y = element_blank(),
                             legend.position = "right") +
                       scale_y_continuous(breaks = seq(0, .9, .2), labels = percent_format(), expand = c(0, 0), limits = c(0, 1)) +
                       scale_fill_manual(name = NULL, values = c("#FFFFFF", "#113C51", "#B4CB65"), labels = c("Stopping for futility and toxicity", "Stopping for futility alone", "Stopping for toxicity alone")) +
                       coord_flip() +
                       labs(y = "Percentage of trial's stopping",
                            x = NULL)
                   })
legende <- get_legend(liste_plots[[1]])
plot_inter <- wrap_plots(map(liste_plots, ~ .x + theme(legend.position = "none")))
plot2 <- ggplot(data_stackedbar %>% 
                  filter(methode %in% c("Multi-arm C<sub>n</sub>"), type_arret == "arret_fut") %>% 
                  mutate(etiquettes = factor(str_remove(etiquettes, "^Arm "), levels = c("C", "B", "A")),
                         label_scenar = if_else(num_scenar %in% c("10","11", "12", "13"), paste0("Scenario ", num_scenar, ": Arm"), paste0("Scenario   ", num_scenar, ": Arm")),
                         label_scenar = if_else(name == "ttt2", label_scenar, " "))) +
  geom_text(aes(x = nom_axe, y = 1.1, label = label_scenar), hjust = 0) +
  map(0:12 * 3, ~ annotate(geom = "segment", x = 1 + .x, xend = 3 + .x, y = 1.29, yend = 1.29)) +
  geom_richtext(aes(x = nom_axe, y = 1.3, label = etiquettes, color = etiquettes), hjust = 0, position = position_dodge2(width = .9), fill = NA, label.color = NA) +
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
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text.x = element_textbox(
          size = 12, 
          color = "transparent", fill = "transparent", box.color = "transparent",
          halign = 0.5, linetype = 1, r = unit(3, "pt"), width = unit(0.75, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)))
plot_cn <- (plot2 | wrap_plots(map(map(1:2, ~ liste_plots[[.x]]), ~ .x + theme(legend.position = "none")))) + plot_layout(widths = c(.8, 3))
plot_epsi <- (plot2 | wrap_plots(map(map(3:4, ~ liste_plots[[.x]]), ~ .x + theme(legend.position = "none")))) + plot_layout(widths = c(.8, 3))
plot_tot_int <- plot_cn / plot_epsi
plot_tot <- (plot_tot_int | legende) + plot_layout(widths = c(6, 1))
if (sauvegarder) {
  ggsave(plot_tot,
         filename = "Outputs/simu_vsref/plot_13scenar_barplot_futtox.png",
         device = "png", height = 13, width = 16)
  ggsave(plot_tot,
         filename = "Outputs/simu_vsref/plot_13scenar_barplot_futtox.jpeg",
         device = "jpeg", height = 13, width = 16)
  ggsave(plot_tot, units = "px", dpi = 800,
         filename = "Outputs/simu_vsref/plot_13scenar_barplot_futtox.eps",
         device = cairo_ps, height = 9000, width = 12500)
}
if (clear) rm(liste_plots, plot_cn, plot_epsi, plot_inter, plot_tot_int, plot_tot, plot2, legende, data_stackedbar)

## Reason of early stoppings ----

data_stackedbar <- data_ggplot_13vsref %>% 
  filter(methode != "Bonferroni C<sub>n</sub>") %>% 
  mutate(arret_precoce_both = arret_precoce_fut + arret_precoce_tox - arret_precoce,
         arret_precoce_fut = arret_precoce_fut - arret_precoce_both,
         arret_precoce_tox = arret_precoce_tox - arret_precoce_both,
         nom_axe = paste0(num_scenar, ":", name),
         nom_axe = factor(nom_axe, levels = paste0(rep(13:1, each = 3), ":", rep(c("ttt3", "ttt2", "ttt1"), 13)), labels = paste0(rep(13:1, each = 3), ":", rep(c("Arm C", "Arm B", "Arm A"), 13)))) %>% 
  pivot_longer(cols = c(arret_precoce_fut, arret_precoce_tox, arret_precoce_both), names_to = "type_arret", values_to = "pourcent") 

liste_plots <- map(as.character(unique(data_stackedbar$methode)),
                   function(x) {
                     ggplot(data_stackedbar %>% filter(methode == x)) +
                       geom_col(aes(x = nom_axe, y = pourcent, fill = type_arret), 
                                color = "#929494", position = "stack", width = 0.8) +
                       geom_vline(xintercept = c((1:13) * 3 + .5), color = "#666c6c", linetype = "dashed", size = 1) +
                       facet_wrap(vars(methode)) +
                       theme(panel.grid.major.y = element_blank(),
                             axis.ticks.y = element_blank(),
                             axis.text.y = element_blank(),
                             panel.grid.minor.y = element_blank(),
                             legend.position = "right") +
                       scale_y_continuous(breaks = seq(0, .9, .2), labels = percent_format(), expand = c(0, 0), limits = c(0, 1)) +
                       scale_fill_manual(name = NULL, values = c("#1b9e77", "#d95f02", "#7570b3"), labels = c("Early stopping for futility and toxicity", "Early stopping for futility", "Early stopping for toxicity")) +
                       coord_flip() +
                       labs(y = "Early stopping of the trial",
                            x = NULL)
                   })
legende <- get_legend(liste_plots[[1]])
plot_inter <- wrap_plots(map(liste_plots, ~ .x + theme(legend.position = "none")))
plot2 <- ggplot(data = data_stackedbar %>% 
                  filter(methode =="Multi-arm C<sub>n</sub>") %>%
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
plot_epsi <- (plot2 | wrap_plots(map(map(4:6, ~ liste_plots[[.x]]), ~ .x + theme(legend.position = "none")))) + plot_layout(widths = c(1.5, 3))
plot_tot_int <- plot_cn / plot_epsi
plot_tot <- (plot_tot_int | legende) + plot_layout(widths = c(6, 1)) + plot_annotation(title = "Early stopping for futility/toxicity")
if (clear) rm(liste_plots, plot_cn, plot_epsi, plot_inter, plot_tot_int, plot_tot, plot2, legende, data_stackedbar)

## Number of patients ----

plot1 <- ggplot(data_ggplot_13vsref) +
  geom_col(aes(x = num_scenar, y = tot_pat, fill = etiquettes), color = "black",
           position = position_dodge2(width = .45)) +
  geom_vline(xintercept = c(1:12 + .5), color = "#b1b0b1", linetype = "dashed") +
  facet_wrap(vars(methode), ncol = 3) +
  theme(panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_continuous(breaks = seq(0, 60, 15), expand = c(0, 0), limits = c(0, 65)) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb")) +
  coord_flip() +
  labs(y = "Mean number of patient per trial",
       x = NULL,
       fill = "Treatment arms")
plot2 <- ggplot(data_ggplot_13vsref %>% filter(methode %in% c("Multi-arm C<sub>n</sub>", "Single-arm C<sub>n</sub>"))) +
  geom_text(aes(x = num_scenar, y = 1, label = paste0("Scenario ", num_scenar, ":")), hjust = 0) +
  geom_richtext(aes(x = num_scenar, y = 1.3, label = value, color = etiquettes), hjust = 0, position = position_dodge2(width = .9), fill = NA, label.color = NA) +
  geom_text(x = 14, y = 1.325, aes(label = "(P(Eff), P(Tox))"), color = "black", hjust = 0, fontface = "bold") +
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
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_textbox(
          size = 12, 
          color = "transparent", fill = "transparent", box.color = "transparent",
          halign = 0.5, linetype = 1, r = unit(3, "pt"), width = unit(0.75, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)))
plot_tot <- plot2 + plot1 + plot_layout(ncol = 2, widths = c(1, 3.5))
if (clear) rm(plot1, plot2, plot_tot, data_ggplot_13vsref, tab_Ralpha)


# II/ Varying the recruitment rythm ----

load("Data/simu_vsref/donnees_graphes_ref_recrut_20221205.Rdata")

## Proportion of reject of H0 ----

data_ggplot_recrutvsref <- mutate(data_ggplot_recrutvsref,
                                  methode = str_replace_all(methode, "Multiarm", "Multi-arm"),
                                  methode = str_replace_all(methode, "Monoarm", "Single-arm"),
                                  methode = str_replace_all(methode, "^Holm (.*)$", "Multi-arm \\1<sup>(aa)</sup>"),
                                  methode = factor(methode, levels = c("Multi-arm C<sub>n</sub>", "Multi-arm C<sub>n</sub><sup>(aa)</sup>", "Single-arm C<sub>n</sub>", "Multi-arm &epsilon;", "Bonferroni &epsilon;", "Single-arm &epsilon;"))) %>% 
  filter(!is.na(methode))

plot_recrut <- ggplot(data_ggplot_recrutvsref, aes(x = exposant, y = rejet_h0, color = ttt)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(vars(methode), ncol = 3) +
  scale_color_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"), 
                     labels = c("Arm A: (0.15, 0.30, 0.15, 0.40)", "Arm B: (0.18, 0.42, 0.02, 0.38)", "Arm C: (0.15, 0.35, 0.10, 0.40)"),
                     name = "Simulation probabilities") +
  scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1), labels = scales::percent_format()) +
  theme(legend.title = element_text(face = "bold"),
        plot.caption = element_markdown(size = 10, hjust = 1),
        plot.title = element_markdown()) +
  labs(x = "&psi;",
       y = "Proportion of rejec H<sub>0</sub>",
       caption = "Thresholds were optimized for a trial consisting of 60 patients per arm with an interim analysis each 15 patients recruited in each arm.<br>
       Patients enrolled at each interim analysis are given by n<sub>k</sub>=60*(k/4)<sup>&psi;</sup>",
       title = "Reject of H<sub>0</sub> relative to the recruitment's rythm")
if (clear) rm(plot_recrut)

## Mean number of patients ----

plot_pts <- ggplot(data_ggplot_recrutvsref, aes(x = exposant, y = tot_pat, color = ttt)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(vars(methode), ncol = 3) +
  scale_color_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"), 
                     labels = c("Arm A: (0.15, 0.30, 0.15, 0.40)", "Arm B: (0.18, 0.42, 0.02, 0.38)", "Arm C: (0.15, 0.35, 0.10, 0.40)"),
                     name = "Simulation probabilities") +
  scale_y_continuous(breaks = seq(0, 60, 15), limits = c(0, 60), labels = function(x) paste0(x, " pts")) +
  theme(legend.title = element_text(face = "bold"),
        plot.caption = element_markdown(size = 10, hjust = 1)) +
  labs(x = "&psi;",
       y = "Mean number of patients",
       caption = "Thresholds were optimized for a trial consisting of 60 patients per arm with an interim analysis each 15 patients recruited in each arm.<br>
       Patients enrolled at each interim analysis are given by n<sub>k</sub>=60*(k/4)<sup>&psi;</sup>",
       title = "Number of recruited patients relative to the recruitment's rythm")
if (clear) rm(plot_pts)

## Correct selection per arm ----

if (FALSE) { # Run only if you have the raw data
  
  # Load raw data
  load("Data/simu_vsref/essais_bruts_bonfgam_chgtrecrut_20221205.Rdata")
  
  # Result with 1 H1, 1 H0 and 1 intermediate arm
  tab_resume_ori <- rbind(cbind(methode = "Multi-arm C<sub>n</sub>", tab_Cnmulti),
                          cbind(methode = "AA C<sub>n</sub>", tab_Cnholm %>% select(-nb_act, -continuer) %>% mutate(seuil_tox = NA_real_)),
                          cbind(methode = "Single-arm C<sub>n</sub>", tab_Cnmono),
                          cbind(methode = "Multi-arm &epsilon;", tab_Gammulti),
                          cbind(methode = "Bonferroni &epsilon;", tab_Gambonf),
                          cbind(methode = "Single-arm &epsilon;", tab_Gammono)) %>% 
    mutate(methode = factor(methode, levels = c("Multi-arm C<sub>n</sub>", "AA C<sub>n</sub>", "Single-arm C<sub>n</sub>", "Multi-arm &epsilon;", "Bonferroni &epsilon;", "Single-arm &epsilon;")))
  tab_resume <- tab_resume_ori %>% 
    mutate(rejet_essai = if_else(ttt == "ttt2", 1, 0),
           rejet_h0 = as.numeric(decision == "Accept the treatment"),
           bonne_decision = rejet_h0 == rejet_essai) %>% 
    group_by(methode, exposant, n_simu) %>% 
    summarise(prop_bonne_deci = mean(bonne_decision)) %>% 
    summarise(prop_bonne_deci = mean(prop_bonne_deci)) %>% 
    ungroup()
  rm(tab_Cnbonf, tab_Cnmono, tab_Cnmulti, tab_Gambonf, tab_Gammono, tab_Gammulti, tab_Cnholm, tab_resume_ori)
  
  # Results under global null hypothesis
  tab_h0 <- rbind(cbind(methode = "Multi-arm C<sub>n</sub>", tab_Cnmulti_h0),
                  cbind(methode = "AA C<sub>n</sub>", tab_Cnholm_h0 %>% select(-nb_act, -continuer) %>% mutate(seuil_tox = NA_real_)),
                  cbind(methode = "Single-arm C<sub>n</sub>", tab_Cnmono_h0),
                  cbind(methode = "Multi-arm &epsilon;", tab_Gammulti_h0),
                  cbind(methode = "Bonferroni &epsilon;", tab_Gambonf_h0),
                  cbind(methode = "Single-arm &epsilon;", tab_Gammono_h0)) %>% 
    mutate(methode = factor(methode, levels = c("Multi-arm C<sub>n</sub>", "AA C<sub>n</sub>", "Single-arm C<sub>n</sub>", "Multi-arm &epsilon;", "Bonferroni &epsilon;", "Single-arm &epsilon;")))
  tab_h0 <- tab_h0 %>%
    group_by(methode, exposant, n_simu) %>%
    summarise(rejet_glob = sum(decision == "Accept the treatment") > 0) %>%
    summarise(rejet_glob = mean(rejet_glob)) %>% 
    ungroup()
  rm(tab_Cnholm_h0, tab_Cnmulti_h0, tab_Cnbonf_h0, tab_Cnmono_h0, tab_Gammulti_h0, tab_Gammono_h0, tab_Gambonf_h0)
  
  save(tab_resume, tab_h0, file = paste0("Data/simu_vsref/recrut_rawprocessed_", format(Sys.Date(), "%Y%m%d"), ".RData"))
  
} else { # Or use the stored data
  
  load("Data/simu_vsref/recrut_rawprocessed_20230113.RData")
  
}

# Table with the different accrual rates
tab_ana <- tibble(puissance = seq(0.25, 1.75, 0.025), 
                  N = 60) %>% 
  mutate(ana_inter_cum = map2(N, puissance, ~ round((.x * (c(1:4) / 4) ^ .y)))) %>%
  mutate(ana_inter = map(ana_inter_cum, ~ c(.x[1], diff(.x))))
tab_ana <- tab_ana %>% 
  filter(puissance %in% seq(.25, 1.75, .25)) %>% 
  unnest_longer(ana_inter) %>% 
  group_by(puissance) %>% 
  mutate(n_ana = row_number()) %>% 
  ungroup()

# The big plot made with multiple plots
## Number of patients
plot_anainter <- tab_ana %>% 
  mutate(n_ana = paste0("n", n_ana),
         n_ana = factor(n_ana, levels = paste0("n", 4:1))) %>% 
  ggplot(aes(x = puissance, y = n_ana, label = ana_inter)) +
  geom_text() +
  labs(y = " ") +
  coord_cartesian(xlim = c(.25, 1.75), ylim = c(1.25, 3.75)) +
  scale_x_continuous(name = "&psi;", breaks = seq(.25, 1.75, .25)) +
  theme(panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        panel.border = element_blank(),
        axis.text.y = element_text(face = "bold", color = "black", size = 15),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "black"),
        axis.line.x = element_line(color = "black", arrow = arrow(length = unit(.25, "npc"))))
# Percent of correct decision for constant thresholds
plot_pourcentdeci_cstt_1 <- ggplot(tab_resume %>%
                                     filter(methode != "Bonferroni &epsilon;") %>% 
                                     mutate(seuil = if_else(str_detect(methode, "&epsilon;"), "Constant threshold", "Varying threshold"),
                                            methode = str_replace_all(methode, "^(.+?) .*$", "\\1"),
                                            methode = factor(methode)) %>% 
                                     mutate(prop_bonne_deci = if_else(seuil == "Constant threshold", prop_bonne_deci, NA_real_),
                                            seuil = "Constant threshold") %>% 
                                     filter(!(is.na(prop_bonne_deci) & methode %in% c("Single-arm", "Multi-arm"))) %>% 
                                     arrange(),
                                   aes(x = exposant, y = prop_bonne_deci, color = methode, shape = methode)) +
  geom_line() +
  geom_point(size = 2) +
  annotate(geom = "rect", xmin = .225, xmax = 1.775, ymin = .7, ymax = .85, color = "black", fill = "transparent", linetype = "dashed") +
  annotate(geom = "segment", x = .225, xend = .275, y = .7, yend = .6, color = "black", linetype = "dashed") +
  annotate(geom = "segment", x = 1.775, xend = 1.725, y = .7, yend = .6, color = "black", linetype = "dashed") +
  annotate(geom = "rect", xmin = .275, xmax = 1.725, ymin = .6, ymax = 0, color = "grey20", fill = "transparent") +
  facet_wrap(vars(seuil)) +
  scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, .85), labels = percent_format()) +
  theme(legend.position = "bottom",
        legend.title = element_markdown(size = 18),
        legend.text = element_markdown(size = 15),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_color_manual(name = "Method", values = c("#AA3939", "#113C51", "#118D2C")) +
  labs(x = NULL,
       y = "Percentage of correct selection",
       shape = "Method") +
  coord_cartesian(xlim = c(.25, 1.75)) +
  guides(shape = guide_legend(override.aes = list(size = 3, linetype = 0)))
legende <- get_legend(plot_pourcentdeci_cstt_1)
plot_pourcentdeci_cstt_2 <- ggplot(tab_resume %>%
                                     filter(methode != "Bonferroni &epsilon;") %>% 
                                     mutate(seuil = if_else(str_detect(methode, "&epsilon;"), "Constant threshold", "Varying threshold"),
                                            methode = str_replace_all(methode, "^(.+?) .*$", "\\1"),
                                            methode = factor(methode)) %>% 
                                     mutate(prop_bonne_deci = if_else(seuil == "Constant threshold", prop_bonne_deci, NA_real_),
                                            seuil = "Constant threshold") %>% 
                                     filter(!(is.na(prop_bonne_deci) & methode %in% c("Single-arm", "Multi-arm"))),
                                   aes(x = exposant, y = prop_bonne_deci, color = methode, shape = methode)) +
  geom_line() +
  geom_point(size = 2) +
  scale_y_continuous(breaks = seq(.7, .85, .05), limits = c(.7, .85), labels = percent_format()) +
  scale_color_manual(name = "Method", values = c("#AA3939", "#113C51", "#118D2C")) +
  theme(legend.position = "none") +
  labs(x = NULL,
       y = NULL)
plot_pourcentdeci_cstt <- (plot_pourcentdeci_cstt_1 + theme(legend.position = "none")) + 
  inset_element(plot_pourcentdeci_cstt_2, .061, .047, .939, .686)
plot_pourcentdeci_cstt <- plot_pourcentdeci_cstt / plot_anainter +
  plot_layout(heights = c(4, 1))
# Percent of correct decision for varying threshold
plot_pourcentdeci_var_1 <- ggplot(tab_resume %>%
                                    filter(methode != "Bonferroni &epsilon;") %>% 
                                    mutate(seuil = if_else(str_detect(methode, "&epsilon;"), "Constant threslhold", "Varying threshold"),
                                           methode = str_replace_all(methode, "^(.+?) .*$", "\\1"),
                                           methode = factor(methode)) %>% 
                                    mutate(prop_bonne_deci = if_else(seuil == "Varying threshold", prop_bonne_deci, NA_real_),
                                           seuil = "Varying threshold") %>% 
                                    filter(!(is.na(prop_bonne_deci) & methode %in% c("Single-arm", "Multi-arm"))),
                                  aes(x = exposant, y = prop_bonne_deci, color = methode, shape = methode)) +
  geom_line() +
  geom_point(size = 2) +
  annotate(geom = "rect", xmin = .225, xmax = 1.775, ymin = .7, ymax = .85, color = "black", fill = "transparent", linetype = "dashed") +
  annotate(geom = "segment", x = .225, xend = .275, y = .7, yend = .6, color = "black", linetype = "dashed") +
  annotate(geom = "segment", x = 1.775, xend = 1.725, y = .7, yend = .6, color = "black", linetype = "dashed") +
  annotate(geom = "rect", xmin = .275, xmax = 1.725, ymin = .6, ymax = 0, color = "grey20", fill = "transparent") +
  facet_wrap(vars(seuil)) +
  coord_cartesian(xlim = c(.25, 1.75)) +
  scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, .85), labels = percent_format()) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_color_manual(name = "Method", values = c("#AA3939", "#113C51", "#118D2C")) +
  labs(x = NULL,
       y = "Percentage of correct selection",
       shape = "Method")
plot_pourcentdeci_var_2 <- ggplot(tab_resume %>%
                                    filter(methode != "Bonferroni &epsilon;") %>% 
                                    mutate(seuil = if_else(str_detect(methode, "&epsilon;"), "Constant threslhold", "Varying threshold"),
                                           methode = str_replace_all(methode, "^(.+?) .*$", "\\1"),
                                           methode = factor(methode)) %>% 
                                    mutate(prop_bonne_deci = if_else(seuil == "Varying threshold", prop_bonne_deci, NA_real_),
                                           seuil = "Varying threshold") %>% 
                                    filter(!(is.na(prop_bonne_deci) & methode %in% c("Single-arm", "Multi-arm"))),
                                  aes(x = exposant, y = prop_bonne_deci, color = methode, shape = methode)) +
  geom_line() +
  geom_point(size = 2) +
  scale_y_continuous(breaks = seq(.7, .85, .05), limits = c(.7, .85), labels = percent_format()) +
  scale_color_manual(name = "Method", values = c("#AA3939", "#113C51", "#118D2C")) +
  theme(legend.position = "none") +
  labs(x = NULL,
       y = NULL)
plot_pourcentdeci_var <- plot_pourcentdeci_var_1 + 
  inset_element(plot_pourcentdeci_var_2, .061, .047, .939, .686)
plot_pourcentdeci_var <- plot_pourcentdeci_var / plot_anainter +
  plot_layout(heights = c(4, 1))
# Final plot
plot_pourcentdeci <- (plot_pourcentdeci_cstt | plot_pourcentdeci_var) / legende +
  plot_layout(heights = c(15, 1))
if (sauvegarder) {
  ggsave(filename = "Outputs/simu_vsref/plot_recrut_pourcentdeci.png",
         plot = plot_pourcentdeci, device = "png", height = 8, width = 12)
  ggsave(filename = "Outputs/simu_vsref/plot_recrut_pourcentdeci.jpeg",
         plot = plot_pourcentdeci, device = "jpeg", height = 8, width = 12)
  ggsave(filename = "Outputs/simu_vsref/plot_recrut_pourcentdeci.eps", units = "px", dpi = 800,
         plot = plot_pourcentdeci, device = cairo_ps, height = 6000, width = 9000)
}
if (clear) rm(plot_pourcentdeci, plot_pourcentdeci_var, plot_pourcentdeci_var_1, plot_pourcentdeci_var_2, plot_pourcentdeci_cstt,
              plot_pourcentdeci_cstt_1, plot_pourcentdeci_cstt_2, plot_anainter, legende, data_ggplot_recrutvsref, data_ggplot_recrutvsref_h0, tab_ana)

## FWER ----

alpha_recrut <- ggplot(tab_h0, aes(x = exposant, y = rejet_glob, color = methode)) +
  geom_line() +
  geom_point() +
  expand_limits(y = 0) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Exponent",
       y = "Estimated FWER",
       color = "Threshold")
if (clear) rm(tab_h0, alpha_recrut)


# III/ Varying the total number of patients ----

## Proportion of reject of H0 ----

load("Data/simu_vsref/donnees_graphes_ref_nbpat_20221205.Rdata")
data_ggplot_nbpatvsref <- mutate(data_ggplot_nbpatvsref,
                                 methode = str_replace_all(methode, "Monoarm", "Single-arm"),
                                 methode = str_replace_all(methode, "Multiarm", "Multi-arm"), 
                                 methode = str_replace_all(methode, "^Holm (.*)$", "Multi-arm \\1<sup>(aa)</sup>"), 
                                 methode = factor(methode, levels = c("Multi-arm C<sub>n</sub>", "Multi-arm C<sub>n</sub><sup>(aa)</sup>", "Single-arm C<sub>n</sub>", "Multi-arm &epsilon;", "Bonferroni &epsilon;", "Single-arm &epsilon;")),
                                 methode = droplevels(methode)) %>% 
  filter(nb_pat <= 60, !is.na(methode))

# 6 thresholds
plot_recrut <- ggplot(data_ggplot_nbpatvsref, aes(x = nb_pat, y = rejet_h0, color = ttt)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(vars(methode), ncol = 3) +
  scale_color_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"), 
                     labels = c("Arm A: (0.15, 0.30, 0.15, 0.40)", "Arm B: (0.18, 0.42, 0.02, 0.38)", "Arm C: (0.15, 0.35, 0.10, 0.40)"),
                     name = "Simulation probabilities") +
  scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1), labels = scales::percent_format()) +
  theme(legend.title = element_text(face = "bold"),
        plot.caption = element_markdown(size = 10, hjust = 1),
        plot.title = element_markdown()) +
  labs(x = "Maximum number of patients",
       y = "Proportion of rejecting H<sub>0</sub>",
       caption = "Thresholds were optimized for a trial consisting of 60 patients per arm with an interim analysis each 15 patients recruited in each arm.",
       title = "Reject of H<sub>0</sub> relative to the maximum sample size")
# The 2 presented in the paper
plot_recrut2 <- ggplot(data_ggplot_nbpatvsref %>% filter(str_detect(methode, "Multi-arm"), !str_detect(methode, "(aa)")), 
                       aes(x = nb_pat, y = rejet_h0)) +
  geom_line(aes(color = ttt)) +
  geom_point(aes( color = ttt, shape = ttt), size = 1.5) +
  facet_wrap(vars(methode), ncol = 3) +
  annotate(geom = "text", y = 0, x = 50, label = "Arm A", color = "#AA3939") +
  geom_text(data = data.frame(xx = 50, yy = c(.65, .8), label = "Arm B", methode = factor(c("Multi-arm &epsilon;", "Multi-arm C<sub>n</sub>"), levels = c("Multi-arm &epsilon;", "Multi-arm C<sub>n</sub>"))),
            aes(y = yy, x = xx), label = "Arm B", color = "#113C51") +
  annotate(geom = "text", y = .25, x = 50, label = "Arm C", color = "#B4CB65") +
  scale_color_manual(values = c("#AA3939", "#113C51", "#B4CB65"), 
                     labels = c("Arm A: (0.15, 0.30, 0.15, 0.40)", "Arm B: (0.18, 0.42, 0.02, 0.38)", "Arm C: (0.15, 0.35, 0.10, 0.40)"),
                     name = "Simulation probabilities") +
  scale_shape_manual(values = c(15, 16, 17), 
                     labels = c("Arm A: (0.15, 0.30, 0.15, 0.40)", "Arm B: (0.18, 0.42, 0.02, 0.38)", "Arm C: (0.15, 0.35, 0.10, 0.40)"),
                     name = "Simulation probabilities") +
  scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1), labels = scales::percent_format()) +
  theme(legend.title = element_text(face = "bold"),
        legend.position = "none",
        plot.caption = element_markdown(size = 10, hjust = 1)) +
  labs(x = "Maximum number of patients in each arm",
       y = "Proportion of conclusion to efficacy and no toxicity") 
if (sauvegarder) {
  ggsave(filename = "Outputs/simu_vsref/plot_nbpat_rejeth0_2.png",
         plot = plot_recrut2, device = "png", height = 5, width = 8)
  ggsave(filename = "Outputs/simu_vsref/plot_nbpat_rejeth0_2.jpeg",
         plot = plot_recrut2, device = "jpeg", height = 5, width = 8)
  ggsave(filename = "Outputs/simu_vsref/plot_nbpat_rejeth0_2.eps",
         plot = plot_recrut2, device = cairo_ps, height = 2000, width = 3000, units = "px")
}
if (clear) rm(plot_recrut, plot_recrut2)

## Mean number of patients ----

plot_pts <- ggplot(data_ggplot_nbpatvsref, aes(x = nb_pat, y = tot_pat, color = ttt)) +
  geom_line() +
  geom_point(size = 1) +
  annotate(geom = "segment", x = 20, y = 20, xend = 60, yend = 60, linetype = "dashed", color = "black") +
  facet_wrap(vars(methode), ncol = 3) +
  scale_color_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"), 
                     labels = c("Arm A: (0.15, 0.30, 0.15, 0.40)", "Arm B: (0.18, 0.42, 0.02, 0.38)", "Arm C: (0.15, 0.35, 0.10, 0.40)"),
                     name = "Simulation probabilities") +
  scale_y_continuous(breaks = seq(0, 60, 20), limits = c(0, 60), labels = function(x) paste0(x, " pts")) +
  theme(legend.title = element_text(face = "bold"),
        plot.caption = element_markdown(size = 10, hjust = 1)) +
  labs(x = "Maximum number of patients",
       y = "Mean number of patients",
       caption = "Thresholds were optimized for a trial consisting of 60 patients per arm with an interim analysis each 15 patients recruited in each arm.",
       title = "Number of recruited patients relative to the maximum sample size")
if (clear) rm(plot_pts)

## Percentage of correct selection ----

if (FALSE) { # Again, this part is for when you have raw data
  
  load("Data/simu_vsref/essais_bruts_bonfgam_chgtnb_20221205.Rdata")
  
  # 1 H0, 1 H1 and 1 intermediate scenario
  tab_resume_ori <- rbind(cbind(methode = "Multi-arm C<sub>n</sub>", tab_Cnmulti),
                          cbind(methode = "AA C<sub>n</sub>", tab_Cnholm %>% select(-nb_act, -continuer) %>% mutate(seuil_tox = NA_real_)),
                          cbind(methode = "Single-arm C<sub>n</sub>", tab_Cnmono),
                          cbind(methode = "Multi-arm &epsilon;", tab_Gammulti),
                          cbind(methode = "Bonferroni &epsilon;", tab_Gambonf),
                          cbind(methode = "Single-arm &epsilon;", tab_Gammono)) %>% 
    mutate(methode = factor(methode, levels = c("Multi-arm C<sub>n</sub>", "AA C<sub>n</sub>", "Single-arm C<sub>n</sub>", "Multi-arm &epsilon;", "Bonferroni &epsilon;", "Single-arm &epsilon;")))
  tab_resume <- tab_resume_ori %>% 
    mutate(rejet_essai = if_else(ttt == "ttt2", 1, 0),
           rejet_h0 = as.numeric(decision == "Accept the treatment"),
           bonne_decision = rejet_h0 == rejet_essai) %>% 
    group_by(methode, nb_pat, n_simu) %>% 
    summarise(prop_bonne_deci = mean(bonne_decision)) %>% 
    summarise(prop_bonne_deci = mean(prop_bonne_deci)) %>% 
    ungroup()
  rm(tab_Cnbonf, tab_Cnmono, tab_Cnmulti, tab_Gambonf, tab_Gammono, tab_Gammulti, tab_Cnholm, tab_resume_ori)
  
  # Global H0
  tab_h0 <- rbind(cbind(methode = "Multi-arm C<sub>n</sub>", tab_Cnmulti_h0),
                  cbind(methode = "AA C<sub>n</sub>", tab_Cnholm_h0 %>% select(-continuer, -nb_act) %>% mutate(seuil_tox = NA_real_)),
                  cbind(methode = "Single-arm C<sub>n</sub>", tab_Cnmono_h0),
                  cbind(methode = "Multi-arm &epsilon;", tab_Gammulti_h0),
                  cbind(methode = "Bonferroni &epsilon;", tab_Gambonf_h0),
                  cbind(methode = "Single-arm &epsilon;", tab_Gammono_h0)) %>% 
    mutate(methode = factor(methode, levels = c("Multi-arm C<sub>n</sub>", "AA C<sub>n</sub>", "Single-arm C<sub>n</sub>", "Multi-arm &epsilon;", "Bonferroni &epsilon;", "Single-arm &epsilon;")))
  tab_h0 <- tab_h0 %>%
    group_by(methode, nb_pat, n_simu) %>%
    summarise(rejet_glob = sum(decision == "Accept the treatment") > 0) %>%
    summarise(rejet_glob = mean(rejet_glob)) %>% 
    ungroup()
  rm(tab_Cnbonf_h0, tab_Cnmono_h0, tab_Cnmulti_h0, tab_Gambonf_h0, tab_Gammono_h0, tab_Gammulti_h0, tab_Cnholm_h0)
  
  save(tab_resume, tab_h0, file = paste0("Data/simu_vsref/chgtnb_rawprocessed_", format(Sys.Date(), "%Y%m%d"), ".RData"))
  
} else { # Or use the stored data
  
  load("Data/simu_vsref/chgtnb_rawprocessed_20230113.RData")
  
}

plot_pourcentdeci <- ggplot(tab_resume %>%
                              mutate(seuil = if_else(str_detect(methode, "&epsilon;"), "Constant threslhold", "Varying threshlod"),
                                     methode = str_replace_all(methode, "^(.+?) .*$", "\\1")),
                            aes(x = nb_pat, y = prop_bonne_deci, color = methode)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(vars(seuil)) +
  scale_y_continuous(breaks = seq(.7, .85, .05), limits = c(.675, .875)) +
  scale_color_manual(name = "Method", values = c("#1b9e77", "#94007e", "#d95f02", "#7570b3")) +
  theme(plot.caption = element_markdown(size = 10, hjust = 1)) +
  labs(x = "Maximum sample size",
       y = "Percentage of correct selection",
       caption = "Thresholds were optimized for a trial consisting of 60 patients per arm with an interim analysis each 15 patients recruited in each arm.",
       title = "Good conclusion relative to the maximum sample size")
if (clear) rm(plot_pourcentdeci)

## FWER ----

alpha_nbtot <- ggplot(tab_h0, aes(x = nb_pat, y = rejet_glob, color = methode)) +
  geom_line() +
  geom_point() +
  expand_limits(y = 0) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Maximum number of patients in each arm",
       y = "Estimated FWER",
       color = "Threshold")
if (clear) rm(data_ggplot_nbpatvsref, data_ggplot_nbpatvsref_h0, alpha_nbtot, tab_h0, tab_resume)

# IV/ Unbalance at planned analyses ----

## Proportion of reject of H0 ----

load("Data/simu_vsref/donnees_graphes_ref_alea_20221205.Rdata")

tab_deseq <- mutate(tab_deseq,
                    Methode = str_replace_all(Methode, "Multiarm", "Multi-arm"),
                    Methode = str_replace_all(Methode, "Monoarm", "Single-arm"), 
                    Methode = str_replace_all(Methode, "^Holm (.*)$", "Multi-arm \\1<sup>(aa)</sup>"), 
                    Methode = factor(Methode, levels = c("Multi-arm C<sub>n</sub>", "Multi-arm C<sub>n</sub><sup>(aa)</sup>", "Single-arm C<sub>n</sub>", "Multi-arm &epsilon;", "Bonferroni &epsilon;", "Single-arm &epsilon;"))) %>% 
  filter(!is.na(Methode))

plot_recrut <- ggplot(tab_deseq, aes(x = deseq, y = rejet_h0, color = ttt)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(vars(Methode), ncol = 3) +
  scale_color_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"), 
                     labels = c("Arm A: (0.15, 0.30, 0.15, 0.40)", "Arm B: (0.18, 0.42, 0.02, 0.38)", "Arm C: (0.15, 0.35, 0.10, 0.40)"),
                     name = "Simulation probabilities") +
  scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1), labels = scales::percent_format()) +
  theme(legend.title = element_text(face = "bold"),
        plot.caption = element_markdown(size = 10, hjust = 1),
        plot.title = element_markdown()) +
  labs(x = "Maximum unbalance in patients' count at each interim analysis",
       y = "Proportion of rejecting H<sub>0</sub>",
       caption = "Thresholds were optimized for a trial consisting of 60 patients per arm with an interim analysis each 15 patients recruited in each arm.",
       title = "Reject of H<sub>0</sub> relative to the unbalance at interim analyses")
if (clear) rm(plot_recrut)

## Mean number of patients ----

plot_pts <- ggplot(tab_deseq, aes(x = deseq, y = tot_pat, color = ttt)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(vars(Methode), ncol = 3) +
  scale_color_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"), 
                     labels = c("Arm A: (0.15, 0.30, 0.15, 0.40)", "Arm B: (0.18, 0.42, 0.02, 0.38)", "Arm C: (0.15, 0.35, 0.10, 0.40)"),
                     name = "Simulation probabilities") +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100), labels = function(x) paste0(x, " pts")) +
  theme(legend.title = element_text(face = "bold"),
        plot.caption = element_markdown(size = 10, hjust = 1)) +
  labs(x = "Maximum unbalance in patients' count at each interim analysis",
       y = "Mean number of patients",
       caption = "Thresholds were optimized for a trial consisting of 60 patients per arm with an interim analysis each 15 patients recruited in each arm.",
       title = "Number of recruited patients relative to the unbalance at interim analyses")
if (clear) rm(plot_pts)

## FWER ----

alpha_alea <- ggplot(tab_deseq_h0, aes(x = deseq, y = rejet_glob, color = Methode)) +
  geom_line() +
  geom_point() +
  expand_limits(y = 0) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Maximum unbalance in patients' count at each interim analysis",
       y = "Estimated FWER",
       color = "Threshold")
if (clear) rm(tab_deseq, tab_deseq_h0, alpha_alea)
