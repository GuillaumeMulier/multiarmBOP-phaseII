# Number needed to treat computation

library(rpact)
library(clinfun)

# Uncontrolled settings

design <- getDesignGroupSequential(kMax = 1, alpha = 0.1, beta = .3)
summary(getSampleSizeRates(design, normalApproximation = FALSE, thetaH0 = 0.3, pi1 = 0.42, groups = 1))


# Controlled settings

design <- getDesignGroupSequential(kMax = 1, alpha = 0.1, beta = .3)
summary(getSampleSizeRates(design, pi1 = 0.3, pi2 = 0.5, allocationRatioPlanned = 1))
