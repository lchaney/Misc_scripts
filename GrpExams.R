#CCBIO INSITES @ HHMI Analysis

library(readr)

All <- read_csv("~/Dropbox/Snow/Pedalogical Research/Data/All Scores Abrv.csv")

source('~/GitHub/corrPlot_v2/corrPlot_v2.R')

corrPlot2(All[,4:9])


library(ggplot2)

anova(lm(Total_Course_Score ~ Exam_Ind_Prop + Section + Exam_Grp_Prop, All))

anova(lm(Total_Course_Score ~ Exam_Ind_Prop*Section*Exam_Grp_Prop, All))


anova(lm(Final_Exam_Prop ~ Exam_Ind_Prop + Section + Exam_Grp_Prop, All))


anova(lm(Final_Exam_Prop ~ Exam_Ind_Prop + Section + Exam_Grp_Prop, All[which(All$Cat=='High'), ]))
anova(lm(Final_Exam_Prop ~ Exam_Ind_Prop + Section + Exam_Grp_Prop, All[which(All$Cat=='Medium'), ]))
anova(lm(Final_Exam_Prop ~ Exam_Ind_Prop + Section + Exam_Grp_Prop, All[which(All$Cat=='Low'), ]))

anova(lm(Final_Exam_Prop ~ Section, All))

