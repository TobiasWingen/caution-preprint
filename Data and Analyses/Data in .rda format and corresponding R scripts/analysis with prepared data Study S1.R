#require packages
library(psych)
library(lsr)
library(yarrr)
library(plyr)
library(effsize)
library(lme4)
library(lmerTest)
library(tidyr)
library(car)
library(TOSTER)
library(pwr)
library(lavaan)


#power analysis
#for equivalence test 
powerTOSTtwo(alpha=.05, statistical_power = .95, low_eqbound_d=-0.3, high_eqbound_d=0.3)
289*3
power.t.test(n = 289, power = .95, type = "two.sample", alternative = "one.sided")

#load data
load("data_prepared_S1.rda")
#prepared data was saved here as csv
#write.csv2(data_prepared, file = "data_prepared_S1.csv")

#rename
data_ana<-data_prepared


#age
describe(data_ana$age)
#gender
table(data_ana$gender)

#percentage female
301/(301+541+1)

#hypothesis test
describeBy(data_ana$credibility, group = data_ana$condition)

#hypothesis test: do credibility ratings differ for preprints and peer review?
data_no_info<-subset(data_ana, condition != "preprint_info")
t.test(data_no_info$credibility~data_no_info$condition, na.omit = T, var.equal = T)

effsize::cohen.d(data_no_info$credibility[data_no_info$condition=="peer-review"], data_ana$credibility[data_ana$condition=="preprint_noinfo"],pooled=TRUE,paired=FALSE,na.rm=T, hedges.correction=FALSE,conf.level=0.95,noncentral=FALSE)
#equivalence test do credibility ratings differ for preprints and peer review?
#get relevant values for syntax
values_equiv_test<-describeBy(data_no_info$credibility,data_no_info$condition)
values_equiv_test$`peer-review`$mean
values_equiv_test$preprint_noinfo$vars
TOSTtwo(m1=values_equiv_test$`peer-review`$mean
        , m2 = values_equiv_test$preprint_noinfo$mean, sd1 = values_equiv_test$`peer-review`$sd, sd2 = values_equiv_test$preprint_noinfo$sd, n1 = values_equiv_test$`peer-review`$n, n2 = values_equiv_test$preprint_noinfo$n, low_eqbound=-0.3, high_eqbound=0.3, alpha = 0.05, var.equal=TRUE, plot = F)

#do ratings differ in the preprint with additional information-condition

#compare preprints
data_preprint<-subset(data_ana, condition != "peer-review")
t.test(data_preprint$credibility~data_preprint$condition, na.omit = T, var.equal = T, alternative = "less")

effsize::cohen.d(data_preprint$credibility[data_preprint$condition=="preprint_info"], data_preprint$credibility[data_preprint$condition=="preprint_noinfo"],pooled=TRUE,paired=FALSE,na.rm=T, hedges.correction=FALSE,conf.level=0.95,noncentral=FALSE)
#equivalence test
values_equiv_test2<-describeBy(data_preprint$credibility,data_preprint$condition)

TOSTtwo(m1=values_equiv_test2$preprint_info$mean
        , m2 = values_equiv_test2$preprint_noinfo$mean, sd1 = values_equiv_test2$preprint_info$sd, sd2 = values_equiv_test2$preprint_noinfo$sd, n1 = values_equiv_test2$preprint_info$n, n2 = values_equiv_test2$preprint_noinfo$n, low_eqbound=-0.3, high_eqbound=0.3, alpha = 0.05, var.equal=TRUE, plot = F)
#do preprints with info differ from peer-reviewed articles
data_vspeer<-subset(data_ana, condition != "preprint_noinfo")
t.test(data_vspeer$credibility~data_vspeer$condition, na.omit = T, var.equal = T, alternative = "greater")

effsize::cohen.d(data_vspeer$credibility[data_vspeer$condition=="peer-review"], data_vspeer$credibility[data_vspeer$condition=="preprint_info"],pooled=TRUE,paired=FALSE,na.rm=T, hedges.correction=FALSE,conf.level=0.95,noncentral=FALSE)

values_equiv_test3<-describeBy(data_vspeer$credibility,data_vspeer$condition)

TOSTtwo(m1=values_equiv_test3$preprint_info$mean
        , m2 = values_equiv_test3$`peer-review`$mean, sd1 = values_equiv_test3$preprint_info$sd, sd2 = values_equiv_test3$`peer-review`$sd, n1 = values_equiv_test3$preprint_info$n, n2 = values_equiv_test3$`peer-review`$n, low_eqbound=-0.3, high_eqbound=0.3, alpha = 0.05, var.equal=TRUE, plot = F)


data_plot<-data_ana
data_plot$condition[data_plot$condition == "peer-review"] <- "Peer-Reviewed Article"
data_plot$condition[data_plot$condition == "preprint_info"] <- "Preprint: Authors' Explanation"
data_plot$condition[data_plot$condition == "preprint_noinfo"] <- "Preprint: Limited Information"
#jpeg(filename="plot_study_S1.jpg", width=22.6, height=15.6, units="cm", res = 500)

pirateplot(credibility~condition,data=data_plot,
           
           #methods for "error bars"
           inf.method = "se",
           #x-lab description
           xlab="Published as",
           #y-lab description
           ylab="Perceived Credibility (1-7)",
           #sort "bars" by mean
           sortx = "mean",
           #some graphical adjustment to text and point sizes
           point.o=0,
           cex.lab = 1.3,
           cex.names=1.1)

dev.off()
#differences in potential mediators
describeBy(data_vspeer$quality_control,data_vspeer$condition)
t.test(data_vspeer$quality_control~data_vspeer$condition, na.omit = T, var.equal = T, alternative = "greater")

effsize::cohen.d(data_vspeer$quality_control[data_vspeer$condition=="peer-review"], data_vspeer$quality_control[data_vspeer$condition=="preprint_info"],pooled=TRUE,paired=FALSE,na.rm=T, hedges.correction=FALSE,conf.level=0.95,noncentral=FALSE)

describeBy(data_vspeer$standard_procedure,data_vspeer$condition)
t.test(data_vspeer$standard_procedure~data_vspeer$condition, na.omit = T, var.equal = T, alternative = "greater")

effsize::cohen.d(data_vspeer$standard_procedure[data_vspeer$condition=="peer-review"], data_vspeer$standard_procedure[data_vspeer$condition=="preprint_info"],pooled=TRUE,paired=FALSE,na.rm=T, hedges.correction=FALSE,conf.level=0.95,noncentral=FALSE)

