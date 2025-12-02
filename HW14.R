library(tidyverse)
library(lmtest)
library(AICcmodavg)

## Reading in Rebekkah's data from week 11
birdsong <- read.csv("LaBlue_birdsong_HW11.csv")

## Objective 1

# Fitting full and reduced models
full_birdsong_mod <- lm(total_phrases ~ avg_db*acquisition_method, data = birdsong)
reduced_birdsong_mod <- lm(total_phrases ~ avg_db+acquisition_method, data = birdsong)

# calculating negative log likelihood for each
logLik(full_birdsong_mod)
logLik(reduced_birdsong_mod)
## The full model has the lower NLL

# Comparing models using LRT
lrtest(full_birdsong_mod, reduced_birdsong_mod)
## The full model is preferred, meaning the effect of noise pollution on the number 
## of phrases in birds is dependent on whether they acquire phrases through social
## or innate means. This is the same conclusion I drew from the backwards selection
## method in week 11. 


## Objective 2

# Making single variable, and intercept models. the full model from objective 1 
# is still the full model, and the reduced model shows the main effects.
avgDB_birdsong_mod <- lm(total_phrases ~ avg_db, data = birdsong)
acquisition_birdsong_mod <- lm(total_phrases ~ acquisition_method, data = birdsong)
intercept_birdsong_mod <- lm(total_phrases ~ 1, data = birdsong)

# building an AIC table to compare the models
all_models <- list(full_birdsong_mod, reduced_birdsong_mod, avgDB_birdsong_mod, 
                   acquisition_birdsong_mod, intercept_birdsong_mod)
mod_names <- c("full", "main.effects", "avg.db", "acquisition.method", 
               "intercept.only")
AIC.comparison<- data.frame(aictab(cand.set = all_models, modnames = mod_names))

# Comparing models in table
print(AIC.comparison)
## The only model supported by AIC is the full model, meaning the effect of 
## noise pollution on the number of phrases in birds is dependent on whether they 
## acquire phrases through social or innate means.
