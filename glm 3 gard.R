#3garden GLM -- include type?
fit3a <- glm(cbind(surv, death) ~ pop + type + garden + 
               pop:garden + type:garden, 
             data = surv3counts, family = "quasibinomial")
anova(fit3a, test = "F")
#doesn't look like type:garden should be kept

fit3b <- glm(cbind(surv, death) ~ pop + type + garden + 
               pop:garden, 
             data = surv3counts, family = "quasibinomial")
anova(fit3b, test="F")

anova(fit3a, fit3b, test = "F")
#confirmed, not significant, we can remove type:garden
#now can re remove type

fit3c <- glm(cbind(surv, death) ~ pop  + garden + pop:garden, data = surv3counts, family = "quasibinomial")
anova(fit3b, fit3c, test = "F")
#yes type can be removed from the model
#so the final, simplist model has pop, garden and pop:garden
anova(fit3c, test = "F")


#====================================================================#
fit3x <- glm(cbind(surv, death) ~ pop + type + garden + 
               pop:garden + type:garden + garden:type:pop, 
             data = surv3counts, family = "quasibinomial")
#remove 3way interactions due to singularity

fit3e <- glm(cbind(surv, death) ~ pop + pop:type + garden +
              pop:type:garden,
              data = surv3counts, family = "quasibinomial")
#remove 3way interactions due to singularity

fit3f <- glm(cbind(surv, death) ~ pop + pop:type + garden +
               garden:pop:type + garden:pop,
             data = surv3counts, family = "quasibinomial")
#did not converge

fit3g <- glm(cbind(surv, death) ~ pop + pop:type + garden,
             data = surv3counts, family = "quasibinomial")
anova(fit3g, test = "F")

fit3y <- glm(cbind(surv, death) ~ pop + pop:type + garden + garden:pop,
             data = surv3counts, family = "quasibinomial")
#did not converge

fit3z <- glm(cbind(surv, death) ~ pop + type + garden,
             data = surv3counts, family = "quasibinomial")
anova(fit3z, test = "F")

#====================================================================#

#Look at the random model
library(lme4)
glmer(cbind(surv, death) ~ garden + (1|pop) + (1|garden:pop), data = surv3counts, family = "binomial")

glmer(cbind(surv, death) ~ garden + (1|pop:type) + (1|pop) + (1|garden:pop), data = surv3counts, family = "binomial")

gfit <- glmer(cbind(surv, death) ~ garden + (1|pop:type) + (1|pop) + (1|garden:pop) + (1|garden:pop:type), data = surv3counts, family = "binomial")
summary(gfit)
anova(gfit)
drop1(gfit, test=”Chisq”)

gfit2 <- glmer(cbind(surv, death) ~ garden + (1|pop) + (1|garden:pop), data = surv3counts, family = "binomial")

anova(gfit, gfit2, test = "Chisq")
#including type does has a significance of zero
#====================================================================#