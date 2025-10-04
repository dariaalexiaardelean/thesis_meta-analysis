load("dataset.Rdata")
library(meta)
library(metafor)
library(tidyverse)
library(ggplot2)

estimate <- escalc(
  measure = "SMDH", 
  m1i = dataset$m.i, sd1i = dataset$sd.i, n1i = dataset$n.i,
  m2i = dataset$m.c, sd2i = dataset$sd.c, n2i = dataset$n.c,
  data = dataset
)

dataset <- dataset %>%
          mutate(yi = estimate$yi) %>%
          mutate(vi = estimate$vi)

######################################
# Random effects model meta-analysis #
######################################

meta.cog <- rma(yi, vi, data = dataset, method = "REML")  
summary(meta.cog)

########################
# Prediction intervals #
########################

predict(meta.cog, digits = 3)


###############
# Forest plot #
###############
forest(meta.cog,
       slab = dataset$study,                      
       xlab = "Effect size (SMD)",       
       main = "Forest Plot",
       psize = 1.2,
       cex = 0.85,
       header = FALSE,
       digits = 2,
       shade="zebra")
text(-5.4, length(dataset$yi) + 2, "Study", font = 2, cex = 0.85)
text(4.4,  length(dataset$yi) + 2, "Estimate (95% CI)", font = 2, cex = 0.85)
abline(v = 0, lty = 2)

####################
# Publication bias #
####################
funnel(meta.cog, main = "Funnel Plot")

# Egger's Test
regtest(meta.cog, model = "rma", predictor = "sei") # random effects model
regtest(meta.cog, model = "lm", predictor = "sei") #classical

#####################
# Subgroup analyses #
#####################

m.gen <- metagen(TE = yi, #adapted from Doing meta-analysis in R
                 seTE = sqrt(vi),
                 studlab = study,
                 data = dataset,
                 sm = "SMD",
                 common = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 method.random.ci = "HK")
save(m.gen, file = "m.gen.rda")

#Intervention Type
by.intervention <- update(m.gen, 
                    subgroup = type.i, 
                    tau.common = FALSE) 
print(by.intervention)
forest(by.intervention, print.byvar = TRUE)

#Control Type
by.control <- update(m.gen,
                  subgroup = type.c,
                  tau.common = FALSE)
print(by.control)
forest(by.control, print.byvar = TRUE)

#MCI Type
by.mci <- update(m.gen,
                     subgroup = mci.type,
                     tau.common = FALSE)
print(by.mci) # 0 = mixed/other; 1 = amnesticMCI
forest(by.mci, print.byvar = TRUE) 

#Criteria
by.criteria <- update(m.gen,
                 subgroup = established.criteria,
                 tau.common = FALSE)
print(by.criteria) # 0 = authors' definition; 1 = established criteria
forest(by.criteria, print.byvar = TRUE)  
#Delivery
by.delivery <- update(m.gen, subgroup = delivery, subset = !is.na(delivery), 
                      tau.common = FALSE)
print(by.delivery) # 1 = traditional; 2 = digital
forest(by.delivery, print.byvar = TRUE)

###################
# Meta-regression #
###################

meta.reg.age <- metareg(m.gen, ~ m_age)
meta.reg.age

meta.reg.ed <- metareg(m.gen, ~ m_ed)
meta.reg.ed

meta.reg.depr <- metareg(m.gen, ~ depr_mean) #raw scores
meta.reg.depr

meta.reg.depr2 <- metareg(m.gen, ~ depr_pomp_mean) #pomp 
meta.reg.depr2

#Meta-regression Plots
scatter_plot <- function(xvar, xlab) {
  ggplot(dataset, aes(x = {{xvar}}, y = yi)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal() +
    labs(x = xlab, y = "Effect estimate (Hedges' g)",
         title = paste("Meta-regression:", xlab, "vs Effect estimate"))
}

scatter_plot(m_age, "Baseline mean age")
scatter_plot(m_ed,  "Baseline mean education (years)")
scatter_plot(depr_pomp_mean, "Baseline depressive symptoms (POMP)")
