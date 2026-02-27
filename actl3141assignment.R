#### Assignment ####

# Load libraries
library(dplyr)
library(survival)
library(car)
library(survminer)
library(ggplot2)

#### Data exploration / cleaning ####
# Import data
flu <- read.csv("dataBrasilInfluenza.csv", header = T)
covid <- read.csv("dataBrasilCovid.csv", header  = T)

# Modify structure of variables
flu$dateBirth <- as.Date(flu$dateBirth, format = "%Y-%m-%d")
flu$dateHosp <- as.Date(flu$dateHosp, format = "%Y-%m-%d")
flu$dateEndObs <- as.Date(flu$dateEndObs, format = "%Y-%m-%d")
flu$death <- as.logical(flu$death)
covid$dateBirth <- as.Date(covid$dateBirth, format = "%Y-%m-%d")
covid$dateHosp <- as.Date(covid$dateHosp, format = "%Y-%m-%d")
covid$dateEndObs <- as.Date(covid$dateEndObs, format = "%Y-%m-%d")
covid$death <- as.logical(covid$death)

# Minimum age for both vaccines is 6 months.
flu$influenzaVacine[is.na(flu$influenzaVacine) & flu$ageHosp <= 0.5] <- "No"
flu$covidVacine[is.na(flu$covidVacine) & flu$ageHosp <= 0.5] <- "No"
covid$influenzaVacine[is.na(covid$influenzaVacine) & covid$ageHosp <= 0.5] <- "No"
covid$covidVacine[is.na(covid$covidVacine) & covid$ageHosp <= 0.5] <- "No"
flu$covidVacine[flu$dateHosp < "2021-01-17"] <- "No"


# Data cleaning
covid$race <- dplyr::recode(covid$race,
                     "Branca" = "White",
                     "Parda" = "Mixed",
                     "Preta" = "Black",
                     "Amarela" = "Asian",
                     "Indígena" = "Indigenous")
flu$race <- dplyr::recode(flu$race,
                     "Branca" = "White",
                     "Parda" = "Mixed",
                     "Preta" = "Black",
                     "Amarela" = "Asian",
                     "Indígena" = "Indigenous")

logical <- c("cardio", "pneumopathy", "immuno", "renal", "obesity")
covid[logical] <- lapply(covid[logical], as.logical)
flu[logical] <- lapply(flu[logical], as.logical)
covid$race <- as.factor(covid$race)
flu$race <- as.factor(flu$race)

# Create new variables
flu$ageHosp <- as.numeric((flu$dateHosp - flu$dateBirth) / 365)
covid$ageHosp <- as.numeric((covid$dateHosp - covid$dateBirth) / 365)

flu$ageEndObs <- as.numeric((flu$dateEndObs - flu$dateBirth) / 365)
covid$ageEndObs <- as.numeric((covid$dateEndObs - covid$dateBirth) / 365)

flu$intAgeHosp <- as.integer(flu$ageHosp)
covid$intAgeHosp <- as.integer(covid$ageHosp)

flu$ageBin <- cut(flu$ageHosp,
                  breaks = c(0, 20, 40, 60, 80, Inf),
                  labels = c("[0, 20)", "[20, 40)", "[40, 60)", "[60, 80)", "[80, ∞)"),
                  right = FALSE,   # Makes intervals left-closed, right-open: [a, b)
                  include.lowest = TRUE)

  

flu$timeHosp <- as.numeric(flu$dateEndObs - flu$dateHosp)
covid$timeHosp <- as.numeric(covid$dateEndObs - covid$dateHosp)

covid$influenzaVacine <- ifelse(is.na(covid$influenzaVacine) == T, 
                                "Unknown", 
                                covid$influenzaVacine)
covid$covidVacine <- ifelse(is.na(covid$covidVacine) == T, 
                            "Unknown", 
                            covid$covidVacine)
flu$influenzaVacine <- ifelse(is.na(flu$influenzaVacine), 
                              "Unknown", 
                              flu$influenzaVacine)
flu$covidVacine <- ifelse(is.na(flu$covidVacine), 
                          "Unknown", 
                          flu$covidVacine)

covid$influenzaVacine <- as.factor(covid$influenzaVacine)
covid$covidVacine <- as.factor(covid$covidVacine)
flu$influenzaVacine <- as.factor(flu$influenzaVacine)
flu$covidVacine <- as.factor(flu$covidVacine)

# Remove invalid data
covid <- covid %>%
  filter(ageHosp >= 0, ageEndObs >= 0)
flu <- flu %>%
  filter(ageHosp >= 0, ageEndObs >= 0)


# Data exploration
summary(flu$ageEndObs[flu$death == "TRUE"])
summary(flu$ageEndObs[flu$death == "FALSE"])
summary(covid$ageEndObs[covid$death == "TRUE"])
summary(covid$ageEndObs[covid$death == "FALSE"])

png("histflucomp.png", width = 800, height = 600)
hist(flu$ageEndObs[flu$death == "TRUE"], breaks = 20,
     main = "Histogram of Flu Patient Age",
     xlab = "Age", col = rgb(1, 0, 0, 0.5), probability = T)

hist(flu$ageEndObs[flu$death == "FALSE"], breaks = 20, 
     col = rgb(0, 1, 0, 0.5), probability = T, add = T)

legend("topright", legend = c("Death", "Survive"), lwd = 2, 
       col = c(rgb(1, 0, 0, 0.5), rgb(0, 1, 0, 0.5)))
dev.off()

png("histcovidcomp.png", width = 800, height = 600)
hist(covid$ageEndObs[covid$death == "TRUE"], breaks = 20,
     main = "Histogram of Covid Patient Age",
     xlab = "Age", col = rgb(1, 0, 0, 0.5), probability = T)

hist(covid$ageEndObs[covid$death == "FALSE"], breaks = 20, 
     col = rgb(0, 1, 0, 0.5), probability = T, add = T)
legend("topright", legend = c("Death", "Survive"), lwd = 2, 
       col = c(rgb(1, 0, 0, 0.5), rgb(0, 1, 0, 0.5)))
dev.off()

png("histcoviddays.png", height = 600, width = 800)
hist(covid$timeHosp[covid$death == T], breaks = 50, probability = T,
     main = "Histogram of Covid Time in Hospital",
     xlab = "Days in Hospital", col = rgb(1, 0, 0, 0.5))
hist(covid$timeHosp[covid$death == F], breaks = 50, probability = T, add = T,
     col = rgb(0, 1, 0, 0.5))
dev.off()


png("histfludays.png", height = 600, width = 800)
hist(flu$timeHosp[flu$death == T], breaks = 50, probability = T,
     main = "Histogram of Flu Time in Hospital",
     xlab = "Days in Hospital", col = rgb(1, 0, 0, 0.5))
hist(flu$timeHosp[flu$death == F], breaks = 50, probability = T, add = T,
     col = rgb(0, 1, 0, 0.5))
dev.off()

hist(covid$timeHosp, breaks = 50)

sum(covid$death == T) / nrow(covid)
sum(covid$cardio == T) / nrow(covid)
sum(covid$immuno == T) / nrow(covid)
sum(covid$pneumopathy == T) / nrow(covid)
sum(covid$renal == T) / nrow(covid)
sum(covid$obesity == T) / nrow(covid)
sum(covid$covidVacine == "Yes") / nrow(covid)

sum(flu$death == T) / nrow(flu)
sum(flu$cardio == T) / nrow(flu)
sum(flu$immuno == T) / nrow(flu)
sum(flu$pneumopathy == T) / nrow(flu)
sum(flu$renal == T) / nrow(flu)
sum(flu$obesity == T) / nrow(flu)
sum(flu$influenzaVacine == "Yes") / nrow(flu)


summary(flu)
summary(covid)

sum(covid$death[covid$pneumopathy == "TRUE"] == "TRUE") / 
  sum(covid$pneumopathy == "TRUE")
sum(covid$death[covid$pneumopathy == "FALSE"] == "TRUE") / 
  sum(covid$pneumopathy == "FALSE")



summary(covid)
summary(flu)

png("hist.png", width = 800, height = 1200)
par(mfrow = c(2, 1))
hist(covid$ageHosp, col = '#F8766D', breaks = 20,
     xlab = "Age at Hospitilisation", 
     main = "Histogram of Age at Hospitilisation (Covid)")


hist(flu$ageHosp, col = '#00BFC4', breaks = 20,
     xlab = "Age at Hospitilisation", 
     main = "Histogram of Age at Hospitilisation (Flu)")
dev.off()
# "#F8766D", "Flu" = "#00BFC4"


mean(covid$ageEndObs[covid$death == "TRUE"])
mean(covid$ageEndObs[covid$death == "FALSE"])



summary(covid$ageHosp)
summary(covid$ageEndObs[covid$death == "TRUE"])
summary(covid$ageEndObs[covid$death == "FALSE"])
summary(covid$cardio)
summary(covid$pneumopathy)
summary(covid$immuno)

# Statistical tests on comorbidities ####
t.test(covid$ageEndObs[covid$death == "TRUE"], 
       covid$ageEndObs[covid$death == "FALSE"],
       var.equal = F)

pneuCovidDeaths <- table(covid$pneumopathy, covid$death)
prop.test(pneuCovidDeaths)

immCovidDeaths <- table(covid$immuno, covid$death)
prop.test(immCovidDeaths)

obeCovidDeaths <- table(covid$obesity, covid$death)
prop.test(obeCovidDeaths)

cardioCovidDeaths <- table(covid$cardio, covid$death)
prop.test(cardioCovidDeaths)

t.test(flu$ageEndObs[flu$death == "TRUE"], 
       flu$ageEndObs[flu$death == "FALSE"], 
       var.equal = FALSE)

pneuFluDeaths <- table(flu$pneumopathy, flu$death)
prop.test(pneuFluDeaths)

immFluDeaths <- table(flu$immuno, flu$death)
prop.test(immFluDeaths)

obeFluDeaths <- table(flu$obesity, flu$death)
prop.test(obeFluDeaths)

cardioFluDeaths <- table(flu$cardio, flu$death)
prop.test(cardioFluDeaths)



# Create KM estimates ####
covidSurv <- Surv(time = covid$timeHosp,
                  event = covid$death)
fluSurv <- Surv(time = flu$timeHosp,
                event = flu$death)
kmFluObe <- survfit(fluSurv ~ flu$obesity)

png("kmfluobe.png", width = 800, height = 600)
ggsurvplot(kmFluObe, data = flu,
           xlab = "Days in Hospital", ylab = "S(x)",
           title = "Flu Survival Curve (KM)", 
           legend = "right",
           legend.title = "Obesity Status", 
           legend.labs = c("No", "Yes"))
dev.off()

kmFluRace <- survfit(fluSurv ~ flu$race)

png("kmflurace.png", width = 800, height = 600)
ggsurvplot(kmFluRace, data = flu,
           xlab = "Days in Hospital", ylab = "S(x)",
           title = "Flu Survival Curve (KM)", 
           legend = "right",
           legend.title = "Race",
           legend.labs = c("Asian", "Black", "Indigenous",
                           "Mixed", "White"))
dev.off()

kmFluInfVac <- survfit(fluSurv ~ flu$influenzaVacine)

png("kmflufluvac.png", width = 800, height = 600)
ggsurvplot(kmFluInfVac, data = flu, 
           xlab = "Days in Hospital", ylab = "S(x)",
           title = "Flu Survival Curve (KM)", 
           legend = "right",
           legend.title = "",
           legend.labs = c("Not Vaccinated", "Unknown", "Vaccinated"))
dev.off()

kmFluAge <- survfit(fluSurv ~ flu$ageBin)

png("kmfluage.png", width = 800, height = 600)
ggsurvplot(kmFluAge, data = flu,
           xlab = "Days in Hospital", ylab = "S(x)", 
           title = "Flu Survival Curve (KM)", 
           legend = "right",
           legend.title = "Age",
           legend.labs = levels(flu$ageBin))
dev.off()

(logrankobe <- survdiff(fluSurv ~ obesity, data = flu, rho = 0))
(petopetoobe <- survdiff(fluSurv ~ obesity, data = flu, rho = 1))

(logrankrace <- survdiff(fluSurv ~ race, data = flu, rho = 0))
(petopetorace <- survdiff(fluSurv ~ race, data = flu, rho = 1))

(logrankvac <- survdiff(fluSurv ~ influenzaVacine, data = flu, rho = 0))
(petopetovac <- survdiff(fluSurv ~ influenzaVacine, data = flu, rho = 1))

(logrankage <- survdiff(fluSurv ~ ageBin, data = flu, rho = 0))
(petopetoage <- survdiff(fluSurv ~ ageBin, data = flu, rho = 1))


kmCovid <- survfit(covidSurv ~ 1)
kmFlu <- survfit(fluSurv ~ 1)





#### Cox Stuff ####
coxCovid <- coxph(covidSurv ~ obesity + ageHosp + cardio + pneumopathy +
                    immuno + race + influenzaVacine + covidVacine + renal, 
                  data = covid, method = "breslow")

summary(coxCovid)
cox.zph(coxCovid)

coxCovid2 <- coxph(covidSurv ~ obesity + ageHosp + cardio + pneumopathy +
                     immuno + covidVacine + renal, 
                   data = covid, method = "breslow")
summary(coxCovid2)
cox.zph(coxCovid2)

coxCovid3 <- coxph(covidSurv ~ ageHosp + covidVacine,
                   data = covid, method = "breslow")
summary(coxCovid3)
cox.zph(coxCovid3)
baseCovid <- basehaz(coxCovid, centered = FALSE)

coxSnellCovid <- predict(coxCovid, type = "expected")
residNAcovid <- survfit(Surv(coxSnellCovid, death) ~ 1, data = covid, type = "fh")

png("coxsnellresidcovid.png", width = 800, height = 600)
plot(residNAcovid$time, -log(residNAcovid$surv), type = "s",
     xlab = "Cox-Snell Residuals",
     ylab = "Predicted Cumulative Hazard",
     main = "Cox-Snell Residuals Diagnostic (Covid)")
abline(0, 1, col = "red", lty = 2)
dev.off()




cox.zph(coxCovid)

head(coxSnellCovid)
summary(coxCovid)


coxFlu <- coxph(fluSurv ~ obesity + ageHosp + cardio + pneumopathy +
                  immuno + race + influenzaVacine + covidVacine + renal, 
                data = flu, method = "breslow")
coxFlu2 <- coxph(fluSurv ~ obesity + ageHosp + race + influenzaVacine, 
                 data = flu)

summary(coxFlu)
cox.zph(coxFlu)
summary(coxFlu2)
cox.zph(coxFlu2)
baseFlu <- basehaz(coxFlu, centered = FALSE)

coxSnellFlu <- predict(coxFlu, type = "expected")
residNAflu <- survfit(Surv(coxSnellFlu, death) ~ 1, data = flu, type = "fh")

png("coxsnellresidflu.png", width = 800, height = 600)
plot(residNAflu$time, -log(residNAflu$surv), type = "s",
     xlab = "Cox-Snell Residuals",
     ylab = "Predicted Cumulative Hazard",
     main = "Cox-Snell Residuals Diagnostic (Flu)")
abline(0, 1, col = "red", lty = 2)
dev.off()

summary(coxFlu)
summary(coxFlu2)



#### Flu vs COVID ####



covid$source <- "Covid"
flu$source <- "Flu"


combined <- bind_rows(
  covid %>% select(timeHosp, death, source),
  flu %>% select(timeHosp, death, source)
)

combinedSurv <- Surv(time = combined$timeHosp, event = combined$death)



kmCombined <- survfit(combinedSurv ~ source, data = combined)

survdiff(combinedSurv ~ source, data = combined, rho = 0)
survdiff(combinedSurv ~ source, data = combined, rho = 1)

png("kmcovidflu.png", width = 800, height = 600)
ggsurvplot(kmCombined, data = combined, xlab = "Days in Hospital",
           ylab = "S(x)", title = "KM Survival Curve Covid vs Flu",
           legend = "right", legend.labs = c("Covid", "Flu"),
           legend.title = "")
dev.off()

combinedCox <- coxph(combinedSurv ~ source,
                     data = combined, method = "breslow")
summary(combinedCox)

cox.zph(combinedCox)

coxSnellCom <- predict(combinedCox, type = "expected")
residNAcombined <- survfit(Surv(coxSnellCom, death) ~ 1, data = combined)

png("coxsnellresidcom.png", width = 800, height = 600)
plot(residNAcombined$time, -log(residNAcombined$surv), type = "s",
     xlab = "Cox-Snell Residuals",
     ylab = "Predicted Cumulative Hazard",
     main = "Cox-Snell Residuals Diagnostic (Combined)")
abline(0, 1, col = "red", lty = 2)
dev.off()

combined$lp <- predict(combinedCox, type = "lp")
base_haz <- basehaz(combinedCox, centered = FALSE)

lp_group <- aggregate(lp ~ source, data = combined, FUN = mean)
haz_with_group <- base_haz %>%
  mutate(hazard_Covid = hazard * exp(lp_group$lp[lp_group$source == "Covid"]),
         hazard_Flu   = hazard * exp(lp_group$lp[lp_group$source == "Flu"]))

png("coxcovidfluhaz.png", width = 800, height = 600)
ggplot(haz_with_group, aes(x = time)) +
  geom_step(aes(y = hazard_Covid, color = "Covid")) +
  geom_step(aes(y = hazard_Flu, color = "Flu")) +
  labs(x = "Time", y = "Estimated Hazard",
       title = "Estimated Hazard Function by Source",
       color = "Group") +
  theme_minimal()
dev.off()




surv_est <- survfit(combinedCox, newdata = data.frame(source = c("Covid", "Flu")))

png("coxcovidflusurv.png", width = 800, height = 600)
ggsurvplot(surv_est,
           data = combined,
           legend = "right",
           legend.title = "Source",
           legend.labs = c("Covid", "Flu"),
           xlab = "Time",
           ylab = "Survival Probability",
           title = "Survival Function by Source")
dev.off()


summary(combinedCox)
cox.zph(combinedCox)

png("coxbasehaz.png", width = 800, height = 600)
ggplot() +
  geom_step(data = baseCovid, aes(x = time, y = hazard, color = "Covid")) +
  geom_step(data = baseFlu, aes(x = time, y = hazard, color = "Flu")) +
  scale_color_manual(
    name = "Group",
    values = c("Covid" = "#F8766D", "Flu" = "#00BFC4"),
    labels = c("Covid", "Flu")
  ) +
  labs(
    title = "Cox Baseline Hazard Function",
    x = "Days in Hospital",
    y = "Cumulative Baseline Hazard"
  ) +
  theme_minimal()
dev.off()


png("coxbasesurv.png", width = 800, height = 600)
ggplot() +
  geom_step(data = baseCovid, aes(x = time, y = exp(-hazard), 
                                  colour = "Covid")) +
  geom_step(data = baseFlu, aes(x = time, y = exp(-hazard), 
                                colour = "Flu")) +
  scale_color_manual(
    name = "Group",
    values = c("Covid" = "#F8766D", "Flu" = "#00BFC4"),
    labels = c("Covid", "Flu")
  ) +
  labs(
    title = "Cox Baseline Survival Function",
    x = "Days in Hospital",
    y = "Baseline Survival Function"
  ) +
  ylim(0, 1) +
  theme_minimal()
dev.off()

