library(tidyverse)

mydata <- read.csv("aggregate data.csv")
         
#######################
# Mean change per arm #
#######################

mydata$m_c.i <- ifelse(!is.na(mydata$m_c.i), mydata$m_c.i,mydata$m_p.i - mydata$m_b.i)

mydata$m_c.c <- ifelse(!is.na(mydata$m_c.c), mydata$m_c.c,mydata$m_p.c - mydata$m_b.c)

#####################
# SD change per arm #
#####################
r <- 0.5 #assumed correlation coefficent

mydata$sd_c.i <- ifelse(!is.na(mydata$sd_c.i),mydata$sd_c.i,
                        sqrt(mydata$sd_b.i^2 + mydata$sd_p.i^2 - 2*r*mydata$sd_b.i*mydata$sd_p.i))

mydata$sd_c.c <- ifelse(!is.na(mydata$sd_c.c),mydata$sd_c.c,
                        sqrt(mydata$sd_b.c^2 + mydata$sd_p.c^2 - 2*r*mydata$sd_b.c*mydata$sd_p.c))

######################
# Depression scores #
#####################

# Pooled Number
mydata$n.t <- mydata$n.i + mydata$n.c 

# Pooled Mean
mydata$depr_mean_calc <- (mydata$n.i*mydata$depr_m_i + mydata$n.c*mydata$depr_m_c) / mydata$n.t

# Pooled SD 
within  <- (mydata$n.i - 1)*mydata$depr_sd_i^2 + (mydata$n.c - 1)*mydata$depr_sd_c^2
between <-  mydata$n.i*(mydata$depr_m_i - mydata$depr_mean_calc)^2 +
  mydata$n.c*(mydata$depr_m_c - mydata$depr_mean_calc)^2
mydata$depr_sd_calc <- sqrt( (within + between) / (mydata$n.t - 1) )

# use already provided score per study; otherwise use pooled
mydata$depr_mean <- ifelse(!is.na(mydata$depr_m_t), mydata$depr_m_t, mydata$depr_mean_calc)
mydata$depr_sd   <- ifelse(!is.na(mydata$depr_sd_t), mydata$depr_sd_t, mydata$depr_sd_calc)

#POMP conversion (0–100) 

scale_lookup <- data.frame(
  depr_scale = c("BDI","MADRS","HADS_D","CES_D","CSDD","GDS_30","GDS_15","GDS_12","GDS","HAMD"),
  min = 0,
  max = c(63,      60,      21,       60,      38,      30,       15,       12,    15,   52)  # GDS and HAMD max are assumed!! I need to contact authors 
)

mydata <- merge(mydata, scale_lookup, by = "depr_scale", all.x = TRUE)

rng <- (mydata$max - mydata$min)
mydata$depr_pomp_mean <- ((mydata$depr_mean - mydata$min) / rng) * 100
mydata$depr_pomp_sd   <-  mydata$depr_sd * (100 / rng)

#############
# Mean age #
############

mydata$m_age <- ifelse(!is.na(mydata$age_m.t), mydata$age_m.t,
                       (mydata$age_m.i + mydata$age_m.c)/2) 

########################
# Mean education level #
########################

mydata$m_ed <- ifelse(!is.na(mydata$ed_m.t), mydata$ed_m.t,
                       (mydata$ed_m.i + mydata$ed_m.c)/2)

###########
# Dataset #
###########
dataset <- mydata %>%
  mutate(m.i = m_c.i) %>%
  mutate(m.c = m_c.c) %>%
  mutate(sd.i = sd_c.i) %>%
  mutate(sd.c = sd_c.c) %>%
  select(study, m.i, m.c, sd.i, sd.c, n.i, n.c, m_age, m_ed, type.i, type.c,
         depr_mean, depr_pomp_mean, mci.type, delivery, established.criteria) %>%
  print()

dataset <- dataset %>% 
  mutate(type.i = factor(type.i,
    levels = c(1, 2),
    labels = c("Standalone cognitive interventions",
               "Multicomponent interventions"))) %>% 
  mutate(type.c = factor(type.c,
                    levels = c(0, 1),
                    labels = c("Passive control",
                               "Active control")))

save(dataset, file = "dataset.Rdata")
