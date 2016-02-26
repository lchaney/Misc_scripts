###########GRAPHING POLYNOMIAL################
######## ######## START HERE ######## ######## 
# this explores DOFF TERM
mod <- lm(RelFit ~ I(DOFF^2), data= outmgnewgapatall4)
dat_mod <- data.frame(
  DOFF = seq(min(outmgnewgapatall4$DOFF, na.rm  = TRUE),
             max(outmgnewgapatall4$DOFF, na.rm=TRUE), by = 0.5))
dat_mod$yhat <- predict( mod, dat_mod)
plot(RelFit ~ DOFF, data= outmgnewgapatall4)
lines( x=dat_mod$DOFF, y=dat_mod$yhat)

summary(lm(RelFit ~ Grab + DOFF + RgrSumT + Grab:DOFF + Grab:RgrSumT + DOFF:RgrSumT + Tolerance + I(Grab^2) + I(RgrSumT^2) + I(DOFF^2) + I(Tolerance^2), outmgnewgapatall4))
# NOTE THE SLIGHTLY CURVI-LINEAR LINE (NEGATIVE) AND THE REG'N TERM -0.06.



library(fields)

xRG <- cbind(outmgnewgapatall4$RgrSumT, outmgnewgapatall4$Grab)
xRD <- cbind(outmgnewgapatall4$RgrSumT, outmgnewgapatall4$DOFF)
xDG <- cbind(outmgnewgapatall4$DOFF, outmgnewgapatall4$Grab)

outRG <- Tps(xRG, outmgnewgapatall4$RelFit)
outRD <- Tps(xRD, outmgnewgapatall4$RelFit)
outDG <- Tps(xDG, outmgnewgapatall4$RelFit)


surface(outRG, type="p", xlab="Growth", ylab="Grab", zlab="Fitness", add.legend=FALSE, theta=90)
surface(outRG, type="C", xlab="Growth", ylab="Grab", add.legend=FALSE)

surface(outRD, type="p", xlab="Growth", ylab="Phenology", zlab="Fitness", add.legend=FALSE, theta=210)
surface(outRD, type="C", xlab="Growth", ylab="Phenology", add.legend=FALSE)

surface(outDG, type="p", xlab="Phenology", ylab="Grab", zlab="Fitness", add.legend=FALSE, theta=55)
surface(outDG, type="C", xlab="Phenology", ylab="Grab", add.legend=FALSE)





###What rotation looks best?###
surface(outRG, type="p", xlab="Growth", ylab="Grab", zlab="RelFit")
surface(outRG, type="p", xlab="Growth", ylab="Grab", zlab="RelFit", theta=0)
surface(outRG, type="p", xlab="Growth", ylab="Grab", zlab="RelFit", theta=45)
****
surface(outRG, type="p", xlab="Growth", ylab="Grab", zlab="RelFit", theta=90)
surface(outRG, type="p", xlab="Growth", ylab="Grab", zlab="RelFit", theta=135)
surface(outRG, type="p", xlab="Growth", ylab="Grab", zlab="RelFit", theta=180)
surface(outRG, type="p", xlab="Growth", ylab="Grab", zlab="RelFit", theta=225)
surface(outRG, type="p", xlab="Growth", ylab="Grab", zlab="RelFit", theta=270) # *
surface(outRG, type="p", xlab="Growth", ylab="Grab", zlab="RelFit", theta=315)
surface(outRG, type="p", xlab="Growth", ylab="Grab", zlab="RelFit", theta=360)


surface(outRD, type="p", xlab="Growth", ylab="Phenology", zlab="RelFit")
surface(outRD, type="p", xlab="Growth", ylab="Phenology", zlab="RelFit", theta=0)
surface(outRD, type="p", xlab="Growth", ylab="Phenology", zlab="RelFit", theta=45)
surface(outRD, type="p", xlab="Growth", ylab="Phenology", zlab="RelFit", theta=90)
surface(outRD, type="p", xlab="Growth", ylab="Phenology", zlab="RelFit", theta=135)
surface(outRD, type="p", xlab="Growth", ylab="Phenology", zlab="RelFit", theta=180)
surface(outRD, type="p", xlab="Growth", ylab="Phenology", zlab="RelFit", theta=225)
surface(outRD, type="p", xlab="Growth", ylab="Phenology", zlab="RelFit", theta=270)
surface(outRD, type="p", xlab="Growth", ylab="Phenology", zlab="RelFit", theta=315)
surface(outRD, type="p", xlab="Growth", ylab="Phenology", zlab="RelFit", theta=360)


surface(outDG, type="p", xlab="Phenology", ylab="Grab", zlab="RelFit")
surface(outDG, type="p", xlab="Phenology", ylab="Grab", zlab="RelFit", theta=0)
***
surface(outDG, type="p", xlab="Phenology", ylab="Grab", zlab="RelFit", theta=45) #*
surface(outDG, type="p", xlab="Phenology", ylab="Grab", zlab="RelFit", theta=90)
surface(outDG, type="p", xlab="Phenology", ylab="Grab", zlab="RelFit", theta=180)
surface(outDG, type="p", xlab="Phenology", ylab="Grab", zlab="RelFit", theta=225)
surface(outDG, type="p", xlab="Phenology", ylab="Grab", zlab="RelFit", theta=270)
surface(outDG, type="p", xlab="Phenology", ylab="Grab", zlab="RelFit", theta=315)
surface(outDG, type="p", xlab="Phenology", ylab="Grab", zlab="RelFit", theta=360)






setEPS()
postscript("~/Documents/2011FieldProject/Writing/Manuscript Prep/Figures/corseln_growthgrab_surface.eps")
surface(outRG, type="p", xlab="Growth", ylab="Grab", zlab="Fitness", add.legend=FALSE, theta=90)

postscript("~/Documents/2011FieldProject/Writing/Manuscript Prep/Figures/corseln_growthgrab_contour.eps")
surface(outRG, type="C", xlab="Growth", ylab="Grab", add.legend=FALSE)

postscript("~/Documents/2011FieldProject/Writing/Manuscript Prep/Figures/corseln_growthphen_suface.eps")
surface(outRD, type="p", xlab="Growth", ylab="Phenology", zlab="Fitness", add.legend=FALSE, theta=210)

postscript("~/Documents/2011FieldProject/Writing/Manuscript Prep/Figures/corseln_growthphen_contour.eps")
surface(outRD, type="C", xlab="Growth", ylab="Phenology", add.legend=FALSE)

postscript("~/Documents/2011FieldProject/Writing/Manuscript Prep/Figures/corseln_phengrab_surface.eps")
surface(outDG, type="p", xlab="Phenology", ylab="Grab", zlab="Fitness", add.legend=FALSE, theta=55)

postscript("~/Documents/2011FieldProject/Writing/Manuscript Prep/Figures/corseln_phengrab_contour.eps")
surface(outDG, type="C", xlab="Phenology", ylab="Grab", add.legend=FALSE)

dev.off()