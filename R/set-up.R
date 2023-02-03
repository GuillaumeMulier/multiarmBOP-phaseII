# ------------------------------------- #
# Install the packages used in the code #
# ------------------------------------- #

pkgs <- c("rpact", "clinfun", "devtools", "tidyverse", "ggtext", "scales", "cowplot", "patchwork", 
          "flextable", "officer", "glue", "grid")

lapply(X = pkgs[!pkgs %in% installed.packages()],
       FUN = install.packages)

rm(pkgs)

if (!require(multibrasBOP2)) devtools::install_github("GuillaumeMulier/multibrasBOP2")
