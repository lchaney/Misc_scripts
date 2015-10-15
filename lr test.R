#now posthoc comparisons between each garden
#how many comparisons?
gardensize <- length(unique(surv3d$garden))

#function that will do pairwise comparisons (see here)
#http://r.789695.n4.nabble.com/Kaplan-Meier-Post-Hoc-td4647363.html
#http://stats.stackexchange.com/questions/36352/post-hoc-analysis-for-logrank-test
#http://stackoverflow.com/questions/17338774/r-formula-how-to-constrain-calculations-to-two-groups-using-formula/17339707

gardenlrchisqtable <- matrix(0., gardensize, gardensize) 
for (i in 1:gardensize) { 
  for (j in (1:gardensize)[-i]) {
    temp <- survdiff(Surv(time, death) ~ garden, data = surv3d,
                     subset = (garden %in% (unique(garden))[c(i,j)]))
    gardenlrchisqtable[i,j] <- temp$chisq
  }
} 
rownames(gardenlrchisqtable) <- unique(surv3d$garden)
colnames(gardenlrchisqtable) <- unique(surv3d$garden)

gardenlrchisqtable[lower.tri(gardenlrchisqtable, diag = TRUE)] <- NA
print(gardenlrchisqtable, na.print = "")

#and p-values for the table
gardenpval_lrchisqtagible <- round(pchisq(gardenlrchisqtable, 1, lower.tail = FALSE), 5)

gardenpval <- pchisq(gardenlrchisqtable, 1, lower.tail = FALSE)
correctedgardenpval <- p.adjust(gardenpval, method = "bonferroni")

correctedgardenpval_lrchisqtagible <- matrix(round(correctedgardenpval, 5), gardensize, gardensize)  
rownames(correctedgardenpval_lrchisqtagible) <- unique(surv3d$garden)
colnames(correctedgardenpval_lrchisqtagible) <- unique(surv3d$garden)

gardenpval
correctedgardenpval_lrchisqtagible











####

matrix(round(p.adjust(pchisq(lrchisqtable, 1, lower.tail = FALSE), method = "fdr"), 5), typesize, typesize)
print(ltabl, na.print = "")

###



lrtable <- function(level,...) {
  #how many comparisons -- unique strata
  size <- length(unique(data$level))
  
  #log rank test to see if there is a difference in survival
  mod <- survdiff(Surv(time, death) ~ level, data = data)
  
  print(mod)
  
  lrx2table <- matrix(0., size, size) 
  for (i in 1:size) { 
    for (j in (1:size)[-i]) {
      temp <- survdiff(Surv(time, death) ~ level, data = data, subset = (level %in% (unique(level))[c(i,j)]))
      lrx2table[i,j] <- temp$chisq
    }
  } 
  
  #lr chi square table
  rownames(lrx2table) <- unique(data$level)
  colnames(lrx2table) <- unique(data$level)
  lrx2table[lower.tri(lrx2table, diag = TRUE)] <- NA
  
  #print chi square values
  print(lrx2table, na.print = "")
  
  #calculate p-values
  pvtable <- p.adjust(pchisq(lrx2table, 1, lower.tail = FALSE))
  matrix(round(pvtable, 5), size, size)
}


lrtable(level = garden, data = surv3d, method = "fdr")







#########
#Population LR Test
#now posthoc comparisons between each type
#how many comparisons?
popsize <- length(unique(svdat$pop))

#function that will do pairwise comparisons (see here)
#http://r.789695.n4.nabble.com/Kaplan-Meier-Post-Hoc-td4647363.html
#http://stats.stackexchange.com/questions/36352/post-hoc-analysis-for-logrank-test
#http://stackoverflow.com/questions/17338774/r-formula-how-to-constrain-calculations-to-two-groups-using-formula/17339707

lrchisqtable <- matrix(0., popsize, popsize) 
for (i in 1:popsize) { 
  for (j in (1:popsize)[-i]) {
    temp <- survdiff(Surv(time, death) ~ pop, data = svdat,
                     subset = (pop %in% (unique(pop))[c(i,j)]))
    lrchisqtable[i,j] <- temp$chisq
  }
} 
rownames(lrchisqtable) <- unique(svdat$pop)
colnames(lrchisqtable) <- unique(svdat$pop)

lrchisqtable[lower.tri(lrchisqtable, diag = TRUE)] <- NA

#and p-values for the table
pval_lrchisqtagible <- round(pchisq(lrchisqtable, 1, lower.tail = FALSE), 5)
pval_lrchisqtagiblefdr <- round(p.adjust(pchisq(lrchisqtable, 1, lower.tail = FALSE), method = "fdr"), 5)
