#load packages

library(lavaan)
library(ggplot2)
library(psych)
library(semTools)
library(semPlot)
library(igraph)
library(tidySEM)
library(dplyr)

#cfa model

kmodel <- ' factor1 =~ ac + ae + ai + aj + al + an + ao + ap + ar
            factor2 =~ bb + bd + be '

fit <- cfa(model = kmodel, data = R_600)
summary(fit, standardized=T)
semPaths(fit, "std")
fitmeasures(fit, c("tli","cfi","srmr","rmsea" ))
graph_sem(model = fit)
lay <- get_layout("","","factor1","factor2","","",
                  "ac","ae", "ag", "ai","aj","al","an","ao", "ap", "ar","bb","bd","be", rows = 3)
graph_sem(fit, layout = lay)


#item response theory

library("mirt")
model.kar <- 'karanitems = 1-33'
results.rsm <- mirt(data = irtres, model = model.kar, itemtype ="rsm", verbose=F)
coef.rsm <- coef(results.rsm, simplify=TRUE)
items.rsm <- as.data.frame(coef.rsm$items)
print(items.rsm)
plot(results.rsm, type = 'trace', which.items = c(3,5,9,10,12,14,15,16,18,28,30,31), 
     main = "", par.settings = simpleTheme(lty=1:4,lwd=2),
     auto.key=list(points=FALSE,lines=TRUE, columns=4))

#measurement invariance

#onfigural invariance

configural <- cfa(kmodel, data = misex, group = "Sex", check.gradient = F)
summary(configural, fit.measures = T)

#weak/metric invariance
weak.invariance <- cfa(kmodel, data = misex, group = "Sex", group.equal = "loadings")
summary(weak.invariance, fit.measures=TRUE)
anova(weak.invariance, configural)
fit.stats <-rbind(fitmeasures(configural, fit.measures = c("chisq", "df", "rmsea", "srmr", "tli", "cfi", "aic")),
                  fitmeasures(weak.invariance, fit.measures = c("chisq", "df", "rmsea", "srmr", "tli", "cfi", "aic")))
rownames(fit.stats) <- c("configural", "weak.invariance")
fit.stats

#STRONG invariance
strong.invariance <- cfa(kmodel, data = misex, group = "Sex", group.equal = c( "loadings", "intercepts"))
summary(strong.invariance, fit.measures=TRUE)
anova(strong.invariance, weak.invariance)
lavTestScore(strong.invariance)
parTable(strong.invariance)

#FREEING THE PARTIALS
strong.invariance.x11 <- cfa(kmodel, data = misex, group = "Sex", group.equal =c( "loadings", "intercepts"), group.partial=c("x11 ~ 1"))
lavTestScore(strong.invariance.x11)
#after freeing level1
strong.invariance.x11x22 <- cfa(kmodel, data = misex, group = "Sex", group.equal = c("loadings", "intercepts"), group.partial = c("x11 ~ 1", "x22~ 1"))
lavTestScore(strong.invariance.x11x22)
#after level2
strong.invariance.x11x22 <- cfa(kmodel, data = misex, group = "Sex", group.equal = c("loadings", "intercepts"), group.partial = c("x11 ~ 1", "x22~ 1"))
anova(strong.invariance.x11x22, weak.invariance)
#scalar invariance
fit.stats2 <-rbind(fitmeasures(strong.invariance, fit.measures = c("chisq", "df", "rmsea", "srmr", "tli", "cfi", "aic")),
                   fitmeasures(strong.invariance.x11x22, fit.measures = c("chisq", "df", "rmsea", "srmr", "tli", "cfi", "aic")))
rownames(fit.stats2) <- c("strong", "strong with x11 x22")
fit.stats <- rbind(fit.stats, fit.stats2)
round(fit.stats, 4)

#strict invariance

strict.invariance.x11x22 <- cfa(kmodel, data = misex, group = "Sex", group.equal = c( "loadings", "intercepts", "residuals"), group.partial = c("x11 ~ 1", "x22 ~ 1"))
anova(strong.invariance.x11x22, strict.invariance.x11x22)

fit.stats <- rbind(fit.stats, fitmeasures(strict.invariance.x11x22, fit.measures = c("chisq", "df", "rmsea", "srmr", "tli", "cfi", "aic")))
rownames(fit.stats)[5] <- "strict invariance"
round(fit.stats, 4)
