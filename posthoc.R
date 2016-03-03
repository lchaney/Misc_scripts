anova(lm(RelFit ~ DOFF + DOFF:Treatment + Treatment,
         data = mgdat2[ which (mgdat2$Treatment == "Antibiotic" | mgdat2$Treatment == "Inoculum"), ]))

anova(lm(RelFit ~ DOFF + DOFF:Treatment + Treatment,
         data = mgdat2[ which (mgdat2$Treatment == "Antibiotic" | mgdat2$Treatment == "Field"), ]))

anova(lm(RelFit ~ DOFF + DOFF:Treatment + Treatment,
         data = mgdat2[ which (mgdat2$Treatment == "Antibiotic" | mgdat2$Treatment == "Autoclave"), ]))

anova(lm(RelFit ~ DOFF + DOFF:Treatment + Treatment,
         data = mgdat2[ which (mgdat2$Treatment == "Inoculum" | mgdat2$Treatment == "Field"), ]))

anova(lm(RelFit ~ DOFF + DOFF:Treatment + Treatment,
         data = mgdat2[ which (mgdat2$Treatment == "Inoculum" | mgdat2$Treatment == "Autoclave"), ]))

anova(lm(RelFit ~ DOFF + DOFF:Treatment + Treatment,
         data = mgdat2[ which (mgdat2$Treatment == "Field" | mgdat2$Treatment == "Autoclave"), ]))







anova(lm(RelFit ~ DOFF + LeafSum34 + HeightRGRC +
           DOFF:Treatment + LeafSum34:Treatment + 
           HeightRGRC:Treatment + Treatment,
         data = mgdat2[ which (mgdat2$Treatment == "Antibiotic" | mgdat2$Treatment == "Inoculum"), ]))

anova(lm(RelFit ~ DOFF + LeafSum34 + HeightRGRC +
           DOFF:Treatment + LeafSum34:Treatment + 
           HeightRGRC:Treatment + Treatment,
         data = mgdat2[ which (mgdat2$Treatment == "Antibiotic" | mgdat2$Treatment == "Field"), ]))

anova(lm(RelFit ~ DOFF + LeafSum34 + HeightRGRC +
           DOFF:Treatment + LeafSum34:Treatment + 
           HeightRGRC:Treatment + Treatment,
         data = mgdat2[ which (mgdat2$Treatment == "Antibiotic" | mgdat2$Treatment == "Autoclave"), ]))

anova(lm(RelFit ~ DOFF + LeafSum34 + HeightRGRC +
           DOFF:Treatment + LeafSum34:Treatment + 
           HeightRGRC:Treatment + Treatment,
         data = mgdat2[ which (mgdat2$Treatment == "Inoculum" | mgdat2$Treatment == "Field"), ]))

anova(lm(RelFit ~ DOFF + LeafSum34 + HeightRGRC +
           DOFF:Treatment + LeafSum34:Treatment + 
           HeightRGRC:Treatment + Treatment,
         data = mgdat2[ which (mgdat2$Treatment == "Inoculum" | mgdat2$Treatment == "Autoclave"), ]))

anova(lm(RelFit ~ DOFF + LeafSum34 + HeightRGRC +
           DOFF:Treatment + LeafSum34:Treatment + 
           HeightRGRC:Treatment + Treatment,
         data = mgdat2[ which (mgdat2$Treatment == "Field" | mgdat2$Treatment == "Autoclave"), ]))







anova(lm(RelFit ~ DOFF + LeafSum34 + HeightRGRC +
           DOFF:LeafSum34 + DOFF:HeightRGRC +
           LeafSum34:HeightRGRC + I(DOFF^2) +
           I(LeafSum34^2) + I(HeightRGRC^2) +
           DOFF:Treatment + LeafSum34:Treatment + 
           HeightRGRC:Treatment + DOFF:LeafSum34:Treatment + 
           DOFF:HeightRGRC:Treatment +
           LeafSum34:HeightRGRC:Treatment + I(DOFF^2):Treatment +
           I(LeafSum34^2):Treatment + I(HeightRGRC^2):Treatment +
           Treatment,
         data = mgdat2[ which (mgdat2$Treatment == "Antibiotic" | mgdat2$Treatment == "Inoculum"), ]))

anova(lm(RelFit ~ DOFF + LeafSum34 + HeightRGRC +
           DOFF:LeafSum34 + DOFF:HeightRGRC +
           LeafSum34:HeightRGRC + I(DOFF^2) +
           I(LeafSum34^2) + I(HeightRGRC^2) +
           DOFF:Treatment + LeafSum34:Treatment + 
           HeightRGRC:Treatment + DOFF:LeafSum34:Treatment + 
           DOFF:HeightRGRC:Treatment +
           LeafSum34:HeightRGRC:Treatment + I(DOFF^2):Treatment +
           I(LeafSum34^2):Treatment + I(HeightRGRC^2):Treatment +
           Treatment,
         data = mgdat2[ which (mgdat2$Treatment == "Antibiotic" | mgdat2$Treatment == "Field"), ]))

anova(lm(RelFit ~DOFF + LeafSum34 + HeightRGRC +
           DOFF:LeafSum34 + DOFF:HeightRGRC +
           LeafSum34:HeightRGRC + I(DOFF^2) +
           I(LeafSum34^2) + I(HeightRGRC^2) +
           DOFF:Treatment + LeafSum34:Treatment + 
           HeightRGRC:Treatment + DOFF:LeafSum34:Treatment + 
           DOFF:HeightRGRC:Treatment +
           LeafSum34:HeightRGRC:Treatment + I(DOFF^2):Treatment +
           I(LeafSum34^2):Treatment + I(HeightRGRC^2):Treatment +
           Treatment,
         data = mgdat2[ which (mgdat2$Treatment == "Antibiotic" | mgdat2$Treatment == "Autoclave"), ]))

anova(lm(RelFit ~ DOFF + LeafSum34 + HeightRGRC +
           DOFF:LeafSum34 + DOFF:HeightRGRC +
           LeafSum34:HeightRGRC + I(DOFF^2) +
           I(LeafSum34^2) + I(HeightRGRC^2) +
           DOFF:Treatment + LeafSum34:Treatment + 
           HeightRGRC:Treatment + DOFF:LeafSum34:Treatment + 
           DOFF:HeightRGRC:Treatment +
           LeafSum34:HeightRGRC:Treatment + I(DOFF^2):Treatment +
           I(LeafSum34^2):Treatment + I(HeightRGRC^2):Treatment +
           Treatment,
         data = mgdat2[ which (mgdat2$Treatment == "Inoculum" | mgdat2$Treatment == "Field"), ]))

anova(lm(RelFit ~ DOFF + LeafSum34 + HeightRGRC +
           DOFF:LeafSum34 + DOFF:HeightRGRC +
           LeafSum34:HeightRGRC + I(DOFF^2) +
           I(LeafSum34^2) + I(HeightRGRC^2) +
           DOFF:Treatment + LeafSum34:Treatment + 
           HeightRGRC:Treatment + DOFF:LeafSum34:Treatment + 
           DOFF:HeightRGRC:Treatment +
           LeafSum34:HeightRGRC:Treatment + I(DOFF^2):Treatment +
           I(LeafSum34^2):Treatment + I(HeightRGRC^2):Treatment +
           Treatment,
         data = mgdat2[ which (mgdat2$Treatment == "Inoculum" | mgdat2$Treatment == "Autoclave"), ]))

anova(lm(RelFit ~ DOFF + LeafSum34 + HeightRGRC +
           DOFF:LeafSum34 + DOFF:HeightRGRC +
           LeafSum34:HeightRGRC + I(DOFF^2) +
           I(LeafSum34^2) + I(HeightRGRC^2) +
           DOFF:Treatment + LeafSum34:Treatment + 
           HeightRGRC:Treatment + DOFF:LeafSum34:Treatment + 
           DOFF:HeightRGRC:Treatment +
           LeafSum34:HeightRGRC:Treatment + I(DOFF^2):Treatment +
           I(LeafSum34^2):Treatment + I(HeightRGRC^2):Treatment +
           Treatment,
         data = mgdat2[ which (mgdat2$Treatment == "Field" | mgdat2$Treatment == "Autoclave"), ]))

