#aggregate population level data with counts of survival and dead
  popdeath2 <- aggregate(death ~ pop, sum, data = svdat) #sum will give you number dead
  poptotal2 <- aggregate(death ~ pop, length, data = svdat) #length will give total number
    names(poptotal2) <- c("pop", "total") #rename to meaningful terms (death is really total)
  popst2 <- merge(popdeath2, poptotal2) #merge death and total by population
  popst2$surv <- popst2$total - popst2$death #get variable of number survived
  popst2$propDead <- popst2$death / popst2$total #calculate propotion dead
  pop2 <- aggregate(. ~ pop, mean, data = svdat[,-c(1, 3:5, 7:9)])#aggregate climate variables by pop
  #mean was chosen, but the climate variables are all the same by pop
  popdat2 <- merge(popst2, pop2, by = c("pop"), all = TRUE) #merge into one data set
  #this seperates by population and type -- so pops with multiple types are still seperated

  rm(list = c("popdeath2", "poptotal2", "popst2", "pop2"))
  #remove the inbetween data frames created so as not to clutter global environment
  #is there an alternative way to do this?
  
  
  popdata <- popdat %>% filter(total > 2)
  
  
  popdat3 <- popdat2 %>% filter(total >= 5)
  
  popdat4 <- popdat2 %>% filter(total > 5)
  
 ##############################################################################################################
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
  #################
  #popdat
  #regular data (so 59 populations)
  
  climvars <- names((popdat)[,c("adi", "adimindd0", "d100", "dd0", "dd5", "fday", "ffp", "gsdd5", "gsp", "pratio", "gspdd5", "gspmtcm", "gsptd", "map", "mapdd5", "mapmtcm", "maptd", "mat", "mmindd0", "mmax", "mmin",      
                                "mtcm", "mtcmgsp", "mtcmmap", "sday", "sdi", "sdimindd0", "tdgsp", "tdiff", "tdmap", "smrpb", "sprp", "winp", "smrp", "sdimtcm", "dd0map", "dd0gsp")])
  
  models1 <- lapply(climvars, function(x) {
    glm(substitute(cbind(surv, death) ~ i + type, list(i = as.name(x))), data = popdat, family=quasibinomial)
  })
  
  sorter.r(models1)
  
  #12   gspmtcm + type 0.6782123
  #16   mapmtcm + type 0.6766355
 
  models2 <- lapply(climvars, function(x) {
    glm(substitute(cbind(surv, death) ~ i + gspmtcm + type, list(i = as.name(x))), data = popdat, family=quasibinomial)
  })
  
  sorter.r(models2)
  
  #25      sday + gspmtcm + type 0.7573906
  #7        ffp + gspmtcm + type 0.7510739
  
  anova(glm(cbind(surv, death) ~ gspmtcm + sday + type, data = popdat, family = "quasibinomial"), test = "F")
   
  #################
  
  models1 <- lapply(climvars, function(x) {
    glm(substitute(cbind(surv, death) ~ i + type, list(i = as.name(x))), data = popdata, family=quasibinomial)
  })
  
  sorter.r(models1)
  
  #29     tdiff + type 0.7472775
  #12   gspmtcm + type 0.6889839
  
  models2 <- lapply(climvars, function(x) {
    glm(substitute(cbind(surv, death) ~ i + tdiff + type, list(i = as.name(x))), data = popdata, family=quasibinomial)
  })
  
  sorter.r(models2)
  
  #12   gspmtcm + tdiff + type 0.7775329
  #16   mapmtcm + tdiff + type 0.7745087
  
  anova(glm(cbind(surv, death) ~ gspmtcm + tdiff + type, data = popdata, family = "quasibinomial"), test = "F")
  
  
  models2 <- lapply(climvars, function(x) {
    glm(substitute(cbind(surv, death) ~ i + gspmtcm + type, list(i = as.name(x))), data = popdata, family=quasibinomial)
  })
  
  sorter.r(models2)
  
  #25      sday + gspmtcm + type 0.7805851
  #29     tdiff + gspmtcm + type 0.7775329
  
  #################
  #popdat2
  #this has no type (so only 55 populations)

  models1 <- lapply(climvars, function(x) {
    glm(substitute(cbind(surv, death) ~ i + type, list(i = as.name(x))), data = popdat2, family=quasibinomial)
  })
  
  sorter.r(models1)
  
  
  #16   mapmtcm + type 0.6985210
  #12   gspmtcm + type 0.6975768
  
  models2 <- lapply(climvars, function(x) {
    glm(substitute(cbind(surv, death) ~ i + mapmtcm + type, list(i = as.name(x))), data = popdat2, family=quasibinomial)
  })
  
  sorter.r(models2)
  
  #25      sday + mapmtcm + type 0.7899619
  #7        ffp + mapmtcm + type 0.7819911
  
  anova(glm(cbind(surv, death) ~ sday + mapmtcm + as.factor(type), data = popdat2, family = "quasibinomial"), test = "F")
  
  
  models2 <- lapply(climvars, function(x) {
    glm(substitute(cbind(surv, death) ~ i + gspmtcm + type, list(i = as.name(x))), data = popdat2, family=quasibinomial)
  })
  
  sorter.r(models2)
  
  #25      sday + gspmtcm + type 0.7568107
  #7        ffp + gspmtcm + type 0.7496886
  
  anova(glm(cbind(surv, death) ~ sday + gspmtcm + as.factor(type), data = popdat2, family = "quasibinomial"), test = "F")
  
  #################
  #################
  #popdat3
  #this has no type and small sample sizes removed (so only 55 populations)
  
  models1 <- lapply(climvars, function(x) {
    glm(substitute(cbind(surv, death) ~ i + type, list(i = as.name(x))), data = popdat3, family=quasibinomial)
  })
  
  sorter.r(models1)
  
  
  #22      mtcm + type 0.7456404
  #12   gspmtcm + type 0.7436787
  #16   mapmtcm + type 0.7394742
  
  models2 <- lapply(climvars, function(x) {
    glm(substitute(cbind(surv, death) ~ i + mtcm + type, list(i = as.name(x))), data = popdat3, family=quasibinomial)
  })
  
  sorter.r(models2)
  
 # 25      sday + mtcm + type 0.8192361
#  7        ffp + mtcm + type 0.8188447
  
  
  anova(glm(cbind(surv, death) ~ sday + gspmtcm + as.factor(type), data = popdat3, family = "quasibinomial"), test = "F")
  
  
  models3 <- lapply(climvars, function(x) {
    glm(substitute(cbind(surv, death) ~ i + gspmtcm + type, list(i = as.name(x))), data = popdat3, family=quasibinomial)
  })
  
  sorter.r(models3)
  
#  25      sday + gspmtcm + type 0.7949958
#  7        ffp + gspmtcm + type 0.7897399
#  29     tdiff + gspmtcm + type 0.7890218
  
#  4        dd0 + sday + gspmtcm + type 0.8415360
#  16   mapmtcm + sday + gspmtcm + type 0.8256075
#  37    dd0gsp + sday + gspmtcm + type 0.8244375
  
  #33      winp + dd0 + sday + gspmtcm + type 0.8642939
  #17     maptd + dd0 + sday + gspmtcm + type 0.8581300
  #14       map + dd0 + sday + gspmtcm + type 0.8569250
  
  
  
  #################
  
  #popdat 3 and no type
  
  models1 <- lapply(climvars, function(x) {
    glm(substitute(cbind(surv, death) ~ i, list(i = as.name(x))), data = popdat3, family=quasibinomial)
  })
  
  sorter.r(models1)
  
  #29     tdiff 6.056672e-01
  #24   mtcmmap 4.635577e-01
  
  models2 <- lapply(climvars, function(x) {
    glm(substitute(cbind(surv, death) ~ i + tdiff, list(i = as.name(x))), data = popdat3, family=quasibinomial)
  })
  
  sorter.r(models2)
  
  #33      winp + tdiff 0.6251354
  #10    pratio + tdiff 0.6206023
  
  anova(glm(cbind(surv, death) ~ winp + tdiff, data = popdat3, family = "quasibinomial"), test = "F")
  
  
  
  #################
  #################
  #popdat4
  #this has no type and small sample sizes removed (so only 55 populations)
  
  models1 <- lapply(climvars, function(x) {
    glm(substitute(cbind(surv, death) ~ i + type, list(i = as.name(x))), data = popdat4, family=quasibinomial)
  })
  
  sorter.r(models1)
  
  #29     tdiff + type 0.7536711
  #12   gspmtcm + type 0.7529021
  
  models2 <- lapply(climvars, function(x) {
    glm(substitute(cbind(surv, death) ~ i + tdiff + type, list(i = as.name(x))), data = popdat4, family=quasibinomial)
  })
  
  sorter.r(models2)
  
  #12   gspmtcm + tdiff + type 0.8027315
  #20      mmax + tdiff + type 0.8005179
  
  
  
  
  ################################### ################################### ################################### ################################### ###################################
  ################################### ################################### ################################### ################################### ###################################
  gar3clim <- merge(as.data.frame(surv3counts), popdat[,-c(2:6)]) 
  
  models1 <- lapply(climvars, function(x) {
    glm(substitute(cbind(surv, death) ~ i + type + garden, list(i = as.name(x))), data = gar3clim, family=quasibinomial)
  })
  
  sorter.r(models1)
  
  #16   mapmtcm + type + garden 0.9171902
  #13     gsptd + type + garden 0.9163875
  #12   gspmtcm + type + garden 0.9162090
  
  models2 <- lapply(climvars, function(x) {
    glm(substitute(cbind(surv, death) ~ i + mapmtcm + type + garden, list(i = as.name(x))), data = gar3clim, family=quasibinomial)
  })
  
  sorter.r(models2)
  

#  7        ffp + mapmtcm + type + garden 0.9255671
#  25      sday + mapmtcm + type + garden 0.9255453
#  8      gsdd5 + mapmtcm + type + garden 0.9248782