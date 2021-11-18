#install packages
library(psych)
library(lsr)
library(yarrr)
library(tidyr)
library(plyr)

#load data
load("data_prepared_Study3.rda")
#prepared data was saved here as csv
#write.csv2(data_prepared, file = "data_prepared_Study3.csv")

#rename
data_ana<-data_prepared
#age
describe(data_ana$age)
#gender
#percentage female
table(data_ana$gender)[1]/(table(data_ana$gender)[2]+table(data_ana$gender)[1])
#hypothesis test
describe(data_ana$credibility_peer)
describe(data_ana$credibility_pre)


#hypothesis test: do credibility ratings differ for preprints and peer review?
t.test(data_ana$credibility_peer, data_ana$credibility_pre, paired = TRUE, alternative = "greater", na.omit = T)
cohensD(data_ana$credibility_peer, data_ana$credibility_pre, method = "paired")       

#plot
#turn data into long format
data_long <- gather(data_ana, condition, measurement, c("credibility_peer","credibility_pre"), factor_key=TRUE)

#rename factor levels
data_long$condition2<-revalue(data_long$condition, c("credibility_pre"="Preprint", "credibility_peer"="Peer-Reviewed Article"))
#jpeg(filename="plot_study_3.jpg", width=16.6, height=16.6, units="cm", res = 500)
pirateplot(measurement~condition2,data=data_long,
           
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

