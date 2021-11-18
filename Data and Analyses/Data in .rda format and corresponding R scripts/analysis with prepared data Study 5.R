#install packages
library(psych)
library(yarrr)
library(effsize)
library(TOSTER)
library(pwr)
library(lavaan)
library(interactions)

#power analysis
#for equivalence test 
powerTOSTtwo(alpha=.05, N = 250, low_eqbound_d=-0.3, high_eqbound_d=0.3)
power.t.test(n = 250, power = .95, type = "two.sample", alternative = "one.sided")

#load data
load("data_prepared_Study5.rda")
#prepared data was saved here as csv

#sensitivity analysis
table(data_prepared$condition)
pwr.t2n.test(n1 = 163, n2 = 183 , sig.level = .05, power =.95)
pwr.t2n.test(n1 = 163, n2 = 183 , sig.level = .05, power =.80)

#write.csv2(data_prepared, file = "data_prepared_Study5.csv")

#rename
data_ana<-data_prepared

#age
describe(data_ana$age)
#gender
table(data_ana$gender)

#hypothesis test
describeBy(data_ana$credibility, group = data_ana$condition)

#hypothesis test: do credibility ratings differ for preprints and peer review?

data_no_info<-subset(data_ana, condition == "peer-review" | condition == "preprint_noinfo" )
t.test(data_no_info$credibility~data_no_info$condition, na.omit = T, var.equal = T)


effsize::cohen.d(data_no_info$credibility[data_no_info$condition=="peer-review"], data_ana$credibility[data_ana$condition=="preprint_noinfo"],pooled=TRUE,paired=FALSE,na.rm=T, hedges.correction=FALSE,conf.level=0.95,noncentral=FALSE)

#compare preprints authors info
#with peer-review
t.test(data_ana$credibility[data_ana$condition=="preprint_info_authors"],data_ana$credibility[data_ana$condition=="peer-review"], var.equal = T, alternative = "less")
effsize::cohen.d(data_ana$credibility[data_ana$condition=="preprint_info_authors"], data_ana$credibility[data_ana$condition=="peer-review"],pooled=TRUE,paired=FALSE,na.rm=T, hedges.correction=FALSE,conf.level=0.95,noncentral=FALSE)
#compare preprints external info
#with peer-review
t.test(data_ana$credibility[data_ana$condition=="preprint_info_external"],data_ana$credibility[data_ana$condition=="peer-review"], var.equal = T, alternative = "less")
effsize::cohen.d(data_ana$credibility[data_ana$condition=="preprint_info_external"], data_ana$credibility[data_ana$condition=="peer-review"],pooled=TRUE,paired=FALSE,na.rm=T, hedges.correction=FALSE,conf.level=0.95,noncentral=FALSE)

#do the different explanations differ from each other?
#external vs. authors
data_extvsint<-subset(data_ana, condition == "preprint_info_authors" | condition == "preprint_info_external" )
t.test(data_extvsint$credibility~data_extvsint$condition, na.omit = T, var.equal = T)

effsize::cohen.d(data_extvsint$credibility[data_extvsint$condition=="preprint_info_authors"], data_extvsint$credibility[data_extvsint$condition=="preprint_info_external"],pooled=TRUE,paired=FALSE,na.rm=T, hedges.correction=FALSE,conf.level=0.95,noncentral=FALSE)
#equivalence test 
values_equiv_test<-describeBy(data_extvsint$credibility,data_extvsint$condition)
TOSTtwo(m1=values_equiv_test$preprint_info_authors$mean
        , m2 = values_equiv_test$preprint_info_external$mean, sd1 = values_equiv_test$preprint_info_authors$sd, sd2 = values_equiv_test$preprint_info_external$sd, n1 = values_equiv_test$preprint_info_authors$n, n2 = values_equiv_test$preprint_info_external$n, low_eqbound=-0.3, high_eqbound=0.3, alpha = 0.05, var.equal=TRUE, plot = F)


#external vs. limited info
data_extvsno<-subset(data_ana, condition == "preprint_info_external" | condition == "preprint_noinfo" )
t.test(data_extvsno$credibility~data_extvsno$condition, na.omit = T, var.equal = T)

effsize::cohen.d(data_extvsno$credibility[data_extvsno$condition=="preprint_noinfo"], data_extvsno$credibility[data_extvsno$condition=="preprint_info_external"],pooled=TRUE,paired=FALSE,na.rm=T, hedges.correction=FALSE,conf.level=0.95,noncentral=FALSE)
#equivalence test 
values_equiv_test<-describeBy(data_extvsno$credibility,data_extvsno$condition)
TOSTtwo(m1=values_equiv_test$preprint_noinfo$mean
        , m2 = values_equiv_test$preprint_info_external$mean, sd1 = values_equiv_test$preprint_noinfo$sd, sd2 = values_equiv_test$preprint_info_external$sd, n1 = values_equiv_test$preprint_noinfo$n, n2 = values_equiv_test$preprint_info_external$n, low_eqbound=-0.3, high_eqbound=0.3, alpha = 0.05, var.equal=TRUE, plot = F)



#authors vs. limited info
data_intvsno<-subset(data_ana, condition == "preprint_info_authors" | condition == "preprint_noinfo" )
t.test(data_intvsno$credibility~data_intvsno$condition, na.omit = T, var.equal = T)

effsize::cohen.d(data_intvsno$credibility[data_intvsno$condition=="preprint_noinfo"], data_intvsno$credibility[data_intvsno$condition=="preprint_info_authors"],pooled=TRUE,paired=FALSE,na.rm=T, hedges.correction=FALSE,conf.level=0.95,noncentral=FALSE)
#equivalence test 
values_equiv_test<-describeBy(data_intvsno$credibility,data_intvsno$condition)
TOSTtwo(m1=values_equiv_test$preprint_noinfo$mean
        , m2 = values_equiv_test$preprint_info_authors$mean, sd1 = values_equiv_test$preprint_noinfo$sd, sd2 = values_equiv_test$preprint_info_authors$sd, n1 = values_equiv_test$preprint_noinfo$n, n2 = values_equiv_test$preprint_info_authors$n, low_eqbound=-0.3, high_eqbound=0.3, alpha = 0.05, var.equal=TRUE, plot = F)



data_plot<-data_ana
data_plot$condition[data_plot$condition == "peer-review"] <- "Peer-Reviewed Article"
data_plot$condition[data_plot$condition == "preprint_info_external"] <- "Preprint: Ext. Explanation"

data_plot$condition[data_plot$condition == "preprint_info_authors"] <- "Preprint: Authors' Explanation"
data_plot$condition[data_plot$condition == "preprint_noinfo"] <- "Preprint: Limited Information"

#jpeg(filename="plot_study_5.jpg", width=22.6, height=15.6, units="cm", res = 500)

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
           cex.names=0.8)

#dev.off()

#mediators
describeBy(data_ana$quality_control, group = data_ana$condition)
describeBy(data_ana$standard_procedure, group = data_ana$condition)



#do the different explanations differ from each other regarding quality control?

#external vs. limited info
data_extvsno<-subset(data_ana, condition == "preprint_info_external" | condition == "preprint_noinfo" )
t.test(data_extvsno$quality_control~data_extvsno$condition, na.omit = T, var.equal = T, alternative = "less")
effsize::cohen.d(data_extvsno$quality_control[data_extvsno$condition=="preprint_noinfo"], data_extvsno$quality_control[data_extvsno$condition=="preprint_info_external"],pooled=TRUE,paired=FALSE,na.rm=T, hedges.correction=FALSE,conf.level=0.95,noncentral=FALSE)

#authors vs. limited info
data_intvsno<-subset(data_ana, condition == "preprint_info_authors" | condition == "preprint_noinfo" )
t.test(data_intvsno$quality_control~data_intvsno$condition, na.omit = T, var.equal = T, alternative = "less")

effsize::cohen.d(data_intvsno$quality_control[data_intvsno$condition=="preprint_noinfo"], data_intvsno$quality_control[data_intvsno$condition=="preprint_info_authors"],pooled=TRUE,paired=FALSE,na.rm=T, hedges.correction=FALSE,conf.level=0.95,noncentral=FALSE)


#do the different explanations differ from each other regarding standard procedure?

#external vs. limited info
data_extvsno<-subset(data_ana, condition == "preprint_info_external" | condition == "preprint_noinfo" )
t.test(data_extvsno$standard_procedure~data_extvsno$condition, na.omit = T, var.equal = T, alternative = "less")
effsize::cohen.d(data_extvsno$standard_procedure[data_extvsno$condition=="preprint_noinfo"], data_extvsno$standard_procedure[data_extvsno$condition=="preprint_info_external"],pooled=TRUE,paired=FALSE,na.rm=T, hedges.correction=FALSE,conf.level=0.95,noncentral=FALSE)

#authors vs. limited info
data_intvsno<-subset(data_ana, condition == "preprint_info_authors" | condition == "preprint_noinfo" )
t.test(data_intvsno$standard_procedure~data_intvsno$condition, na.omit = T, var.equal = T, alternative = "less")

effsize::cohen.d(data_intvsno$standard_procedure[data_intvsno$condition=="preprint_noinfo"], data_intvsno$standard_procedure[data_intvsno$condition=="preprint_info_authors"],pooled=TRUE,paired=FALSE,na.rm=T, hedges.correction=FALSE,conf.level=0.95,noncentral=FALSE)


#moderation analyses
#dummy code condition (info vs peer-review)
data_med_vspeer<-subset(data_ana, condition == "preprint_info_external" | condition == "preprint_info_authors" |  condition == "peer-review")
data_med_vspeer$con_dummy<-data_med_vspeer$condition
data_med_vspeer$con_dummy[data_med_vspeer$con_dummy == "preprint_info_authors"] <- 1
data_med_vspeer$con_dummy[data_med_vspeer$con_dummy == "preprint_info_external"] <- 1
data_med_vspeer$con_dummy[data_med_vspeer$con_dummy == "peer-review"] <- 0

#seed set for reproducibility of results reported in the paper
set.seed(1)
#print information on R, lavaan and the randomizer function for reproducibility 
R.Version()
sessionInfo()
RNGkind()

data_mod<-data_med_vspeer
#create centered variable for ease of interpretation
data_mod$educ_center<-scale(data_mod$education, scale = F)
data_mod$fam_center<-scale(data_mod$familiar, scale = F)
#moderation with education
lm1<-lm(credibility~con_dummy*education,data=data_mod)
summary(lm1)
#moderation with familiarity
lm2<-lm(credibility~con_dummy*fam_center,data=data_mod)
summary(lm2)
#visualization
#cat_plot(lm2, pred = fam_center, modx = con_dummy)

#mediation analyses
#parallel mediation model
set.seed(1)
model1 <- ' # direct effect
credibility ~ a*con_dummy
# mediators
quality_control ~ d*con_dummy
credibility ~ e*quality_control
standard_procedure ~ f*con_dummy
credibility ~ g*standard_procedure
# indirect effect (d*e) (f*g)
de := d*e
fg := f*g
# total effect
total := a +(d*e)+(f*g)'
#fit the model
fit1 <- sem(model1, data = data_med_vspeer, se = "bootstrap", bootstrap = 10000)
#show the results
summary(fit1, standardized = T, ci = T)
