#install packages
library(psych)
library(yarrr)
library(effsize)
library(TOSTER)
library(pwr)
#load data
load("data_prepared_Study2.rda")
#prepared data was saved here as csv
#write.csv2(data_prepared, file = "data_prepared_Study2.csv")

#rename
data_final<-data_prepared

#power analysis
#for equivalence test 
powerTOSTtwo(alpha=.05, statistical_power = .95, low_eqbound_d=-0.3, high_eqbound_d=0.3)

pwr.t2n.test(n1 = 289, n2 = 289, sig.level = .05, power =.95)

#sensitivity analysis
table(data_final$condition)
pwr.t2n.test(n1 = 240, n2 = 226, sig.level = .05, power =.80)
pwr.t2n.test(n1 = 240, n2 = 226, sig.level = .05, power =.95)


#age
describe(data_final$age)
#gender
table(data_final$gender)

#percentage female (female ==2)
table(data_final$gender)[3]/(table(data_final$gender)[2]+table(data_final$gender)[3]+table(data_final$gender)[4])


#hypothesis test
describeBy(data_final$credibility, group = data_final$condition)

#hypothesis test: do credibility ratings differ for preprints and peer review?
t.test(data_final$credibility~data_final$condition, na.omit = T, var.equal = T)

effsize::cohen.d(data_final$credibility[data_final$condition=="peer-review"], data_final$credibility[data_final$condition=="preprint_noinfo"],pooled=TRUE,paired=FALSE,na.rm=T, hedges.correction=FALSE,conf.level=0.95,noncentral=FALSE)


#equivalence test
#get relevant values for syntax
values_equiv_test<-describeBy(data_final$credibility,data_final$condition)
TOSTtwo(m1=values_equiv_test$`peer-review`$mean
        , m2 = values_equiv_test$preprint_noinfo$mean, sd1 = values_equiv_test$`peer-review`$sd, sd2 = values_equiv_test$preprint_noinfo$sd, n1 = values_equiv_test$`peer-review`$n, n2 = values_equiv_test$preprint_noinfo$n, low_eqbound=-0.3, high_eqbound=0.3, alpha = 0.05, var.equal=TRUE)

#plotting
data_plot<-data_final
data_plot$condition[data_plot$condition == "peer-review"] <- "Peer-Reviewed Article"
data_plot$condition[data_plot$condition == "preprint_noinfo"] <- "Preprint"
#jpeg(filename="Fig3.jpg", width=16.6, height=16.6, units="cm", res = 500)
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
           point.o=1,
           cex.lab = 1.5,
           cex.names=1.3)

#dev.off()