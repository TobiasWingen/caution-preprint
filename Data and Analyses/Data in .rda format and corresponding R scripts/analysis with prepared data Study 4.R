#install packages
library(psych)
library(yarrr)
library(effsize)
library(TOSTER)
library(pwr)
library(lavaan)


#power analyses
power.t.test(power = .8, delta = 0.5, sd = 1, alternative = "one.sided")

#load data
load("data_prepared_Study4.rda")
#prepared data was saved here as csv
#write.csv2(data_prepared, file = "data_prepared_Study4.csv")

#rename
data_ana<-data_prepared
#age
describe(data_ana$age)
#gender
table(data_ana$gender)
#percentage female
table(data_ana$gender)[1]/(table(data_ana$gender)[1]+table(data_ana$gender)[2])


#hypothesis test
describeBy(data_ana$credibility, group = data_ana$condition)

#hypothesis test: do credibility ratings differ for preprints and peer review?
t.test(data_ana$credibility~data_ana$condition, alternative = "greater", na.omit = T, var.equal = T)

effsize::cohen.d(data_ana$credibility[data_ana$condition=="Manipulation_peerreview"], data_ana$credibility[data_ana$condition=="Manipulation_preprint"],pooled=TRUE,paired=FALSE,na.rm=T, hedges.correction=FALSE,conf.level=0.95,noncentral=FALSE)

data_plot<-data_ana
data_plot$condition[data_plot$condition == "Manipulation_peerreview"] <- "Peer-Reviewed Article"
data_plot$condition[data_plot$condition == "Manipulation_preprint"] <- "Preprint"

#jpeg(filename="Fig.4.jpg", width=16.6, height=16.6, units="cm", res = 500)
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
           cex.names=1.3,
           jitter.val = .1)

#dev.off()
