# multiarmBOP-phaseII

## Objective

Adapt BOP2 design (from *Zhou et al.*) in multiarm settings.

## Description of the files

The github repository [multibrasBOP2](https://www.github.com/GuillaumeMulier/multibrasBOP2) can be installed with `devtools::install_github("GuillaumeMulier/multibrasBOP2")` and provide the functions used to determine the thresholds, simulate the trials and summarise the results.

All paths are relative to the root of the project.

Sorry, due to size, raw results aren't included in data.
You'll have to rerun the simulations or else take synthetic data.
And also, figures aren't included in the repository.

This repository is organized as follows:

- multiarmBOP-phaseII.RProj = RProj file to open the project and work in relative path;
- R = R scripts:
    - compute_fixed_nsn.R = compute number needed to treat of fixed design in appendix;
    - exemple_azaplus.R = analysis on AZA-PLUS trial;
    - plot_seuils.R = plotting the thresholds in function of recruitment;
    - plot_vscont.R = figures of article, section controlled design;
    - plot_vsref.R = figures of article, section uncontrolled design;
    - set-up.R = install the packages required for the scripts;
    - simu_sens.R = sensitivity simulations asked by reviewers;
    - simu_vscont.R = simulations for controlled setting;
    - simu_vsref.R = simulations for uncontrolled setting.
- Data = data files:
    - ex_clinique = data for AZA-PLUS results;
    - simu_vsref = data for simulations in uncontrolled setting;
    - simu_vscont = data for simulations in controlled setting.

