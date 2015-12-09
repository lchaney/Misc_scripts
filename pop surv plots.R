sfit_typeE <- survfit(Surv(time, death) ~ strata(type), data=sdat_E)
sfit_typeE_T4x <- survfit(Surv(time, death) ~ strata(pop), data=sdat_E[which(sdat_E$type == "T4x"),])
sfit_typeE_T2x <- survfit(Surv(time, death) ~ strata(pop), data=sdat_E[which(sdat_E$type == "T2x"),])
sfit_typeE_W4x <- survfit(Surv(time, death) ~ strata(pop), data=sdat_E[which(sdat_E$type == "W4x"),])
sfit_typeE_V2x <- survfit(Surv(time, death) ~ strata(pop), data=sdat_E[which(sdat_E$type == "V2x"),])
sfit_typeE_V4x <- survfit(Surv(time, death) ~ strata(pop), data=sdat_E[which(sdat_E$type == "V4x"),])

pE <- ggsurv_m(sfit_typeE)
pE_T4x <- ggsurv_m(sfit_typeE_T4x)
pE_T2x <- ggsurv_m(sfit_typeE_T2x)
pE_W4x <- ggsurv_m(sfit_typeE_W4x)
pE_V2x <- ggsurv_m(sfit_typeE_V2x)
pE_V4x <- ggsurv_m(sfit_typeE_V4x)

Ephraim_Surv <- plot_grid(pE, pE_T4x, pE_T2x, pE_W4x, pE_V2x, pE_V4x, labels = c("All", "T4x", "T2x", "W4x", "V2x", "V4x"), ncol = 3)


summary(sfit_typeE)