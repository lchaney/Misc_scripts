surv3counts <- surv3d %>% group_by(pop, type, garden) %>% summarise(death = sum(death), total = n()) %>% mutate(surv = total - death, propdead = death / total)

fit3 <- glm(cbind(surv, death) ~ pop + type + garden + pop:garden + type:garden + garden:type:pop, data = surv3counts, family = "quasibinomial")
#remove 3way interactions due to singularity
fit3a <- glm(cbind(surv, death) ~ pop + type + garden + pop:garden + type:garden, data = surv3counts, family = "quasibinomial")
#doesn't look like type:garden should be kept
fit3b <- glm(cbind(surv, death) ~ pop + type + garden + pop:garden, data = surv3counts, family = "quasibinomial")
anova(fit3a, fit3b, test = "F")
#confirmed, not significant, we can remove type:garden
#now can re remove type
fit3c <- glm(cbind(surv, death) ~ pop  + garden + pop:garden, data = surv3counts, family = "quasibinomial")
anova(fit3b, fit3c, test = "F")
#yes type can be removed from the model
#so the final, simplist model has pop, garden and pop:garden
anova(fit3c, test = "F")



s3c <- surv3counts %>% ungroup() %>% arrange(garden, propdead)
s33c <- as.data.frame(s3c)
s33c$popnum <- with(s33c, factor(pop, levels = pop[order(garden, propdead)], ordered = TRUE))
s33c$popnum <- as.integer(s33c$popnum)


ggplot(data = s33c, aes(x = garden, y = propdead, group = pop, color = popnum)) + 
  stat_summary(fun.y = mean, geom = "line") + 
  scale_color_gradientn(colours = c("red","violet","blue")) + 
  theme(legend.position = "none") + 
  labs(x = "Garden", y = "Proportion Dead")

  
View(surv3counts[,-(4:6)] %>% spread(garden, propdead))


library("directlabels")
ggplot(data = s33c, aes(x = garden, y = propdead, group = pop, color = popnum)) + 
  stat_summary(fun.y = mean, geom = "line") + 
  scale_color_gradientn(colours = c("red","violet","blue")) + 
  theme(legend.position = "none") +
  labs(x = "Garden", y = "Proportion Dead") +
  geom_text(aes(label = pop), alpha = .5) 
            #position = position_jitter())
  #geom_dl(aes(label = pop), method = "first.points") +
  #geom_dl(aes(label = pop), method = "last.points")


ggplot(data = s33c, aes(x = garden, y = propdead, group = pop, color = popnum, linetype = type)) + 
  stat_summary(fun.y = mean, geom = "line") + 
  #  scale_linetype_discrete(aes(type)) +
  scale_color_gradientn(colours = c("red","violet","blue")) + 
  #  theme(legend.position = "none") +
  labs(x = "Garden", y = "Proportion Dead")


####LETS SEE IF WE CAN ADD CLIMATE TO THIS

head(surv3counts)
head(popdat[,-(3:6)])

surv3clim <- merge(surv3counts, popdat[,-(3:6)], by = "pop")


#################
# Gives Rsq + all predictor variables used.
glmrsq2 <- function( model, ... ){
  
  cbind( deparse(model$formula[[3]]), (1-exp((model$dev - model$null)/model$df.null)) / (1-exp(-model$null/model$df.null)))
  
}


# Function for Sorting & ordering the output
sorter.r <- function( models, ... ) {
  glmrsq2.results <- data.frame(do.call(rbind, lapply(models,  glmrsq2)))
  glmrsq2.results[,2] <- as.numeric(as.character(glmrsq2.results[,2]))
  glmrsq2.results[ order(glmrsq2.results[,2], decreasing=TRUE), ]
}

#################

climvars <- names((surv3clim)[,c("adi", "adimindd0", "d100", "dd0", "dd5", "fday", "ffp", "gsdd5", "gsp", "pratio", "gspdd5", "gspmtcm", "gsptd", "map", "mapdd5", "mapmtcm", "maptd", "mat", "mmindd0", "mmax", "mmin",      "mtcm", "mtcmgsp", "mtcmmap", "sday", "sdi", "sdimindd0", "tdgsp", "tdiff", "tdmap", "smrpb", "sprp", "winp", "smrp", "sdimtcm", "dd0map", "dd0gsp")])

models1 <- lapply(climvars, function(x) {
  glm(substitute(cbind(surv, death) ~ i + garden, list(i = as.name(x))), data = surv3clim, family=quasibinomial)
})

sorter.r(models1)


#30     tdmap + garden 0.8531090
#33      winp + garden 0.8489541

models2 <- lapply(climvars, function(x) {
  glm(substitute(cbind(surv, death) ~ i + tdmap + garden, list(i = as.name(x))), data = surv3clim, family=quasibinomial)
})

sorter.r(models2)

#11    gspdd5 + tdmap + garden 0.8869176
#34      smrp + tdmap + garden 0.8762302

anova(glm(cbind(surv, death) ~ gspdd5 + tdmap + garden, data = surv3clim, family = "quasibinomial"), test = "F")





library(RColorBrewer)
colourCount = length(unique(surv3counts$pop))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

ggplot(data = surv3counts, aes(x = garden, y = propdead, group = pop, color = pop)) + 
  stat_summary(fun.y=mean, geom="line") + 
  scale_color_manual(values = getPalette(colourCount)) + 
  theme(legend.position="none")

ggplot(data = surv3counts, aes(x = garden, y = propdead, group = pop, color = as.integer(pop), order = propdead)) + 
  stat_summary(fun.y=mean, geom="line") + 
  scale_color_gradientn(colours = rainbow(7)) + 
  theme(legend.position="none")






ggplot(data = surv3counts, aes(x = garden, y=propdead, color = as.integer(pop))) + geom_jitter() + scale_color_gradient()




p3g <- ggplot(data = surv3counts, aes(x = pop, y = propdead, color = garden)) + geom_point()
p3g <- ggplot(data = surv3counts, aes(x = garden, y = propdead, group = pop)) + stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y=mean, geom="line")

ggplot(data = surv3counts, aes(x = garden, y = propdead, group = pop, color = pop)) + stat_summary(fun.y=mean, geom="line") + scale_color_grey() + theme(legend.position="none")

#with text of means
ggplot(data = surv3counts, aes(x = garden, y = propdead, color = pop, group = pop)) + stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y=mean, geom="line") + stat_summary(fun.y=mean, geom="text",aes(label=round(..y..,2)))