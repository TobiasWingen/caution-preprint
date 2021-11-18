#psyarxiv####
data_imp<-read.csv("PsyArxiv.csv", stringsAsFactors = F)
data1<-data_imp
#exclude peer-reviewed and non-english manuscripts
#number of papers that were excluded because peer-reviewed
table(data1$peer.reviewed)
peer_reviewed_arxiv<-15
#english
table(data1$english)
#exclude peer-reviewed and non-english papers from the preprint database
data2<-subset(data1, peer.reviewed == 0)

#descriptives
#Number of preprints informing their readers that:
#They are a preprint (or similar).
table(data2$preprint)
prop.table(table(data2$preprint))
15+10
#Are not peer-reviewed.
# (0 = "no", 1 = "yes", 2 = "under review", 3 = "submitted for publication")
table(data2$no_peerreview)
12+6
#Peer-review serves as a quality control measure.
table(data2$quality_control)

#Peer-review is part of the typical publication process.
table(data2$standard_procedure)

#Their findings might be preliminary (or similar).
table(data2$preliminary)


#osf preprints####
data_imp_osf<-read.csv("OSF preprints.csv", stringsAsFactors = F)
data1_osf<-data_imp_osf
#exclude peer-reviewed and non-english manuscripts
#number of papers that were excluded because peer-reviewed
table(data1_osf$peer.reviewed)
peer_reviewed_osf<-48
peer_reviewed_arxiv+peer_reviewed_osf
#english
table(data1_osf$english)

#exclude papers that are either supplemental materials, a scan of a paper, and finally empty (because the authors removed the original version)
data1_osf$comments[c(27:30)]
data1_osf$comments[c(126,153)]
data1_osf$comments[c(134)]
#number of exclusions
length(data1_osf$comments[c(27:30)])+length(data1_osf$comments[c(126,153)])+length(data1_osf$comments[c(134)])

data1a<-data1_osf[-c(27:30,126,134,153),]
#exclude peer-reviewed and non-english papers from the preprint database
#exclude papers that are peer-review
data1b<-subset(data1a, peer.reviewed == 0)
#exclude non-english papers 
data1c<-subset(data1b, english == 1)

#create dataset for analyses
data2_osf<-data1c
#descriptives
#Number of preprints informing their readers that:
#They are a preprint (or similar).
table(data2_osf$preprint)
12 + 18 
#Are not peer-reviewed.
# (0 = "no", 1 = "yes", 2 = "under review", 3 = "submitted for publication")
table(data2_osf$no_peerreview)
10+3
#Peer-review serves as a quality control measure.
table(data2_osf$quality_control)

#Peer-review is part of the typical publication process.
table(data2_osf$standard_procedure)

#Their findings might be preliminary (or similar).
table(data2_osf$preliminary)

#combined analyses####
#full dataset samplesize
nrow(data_imp)+nrow(data_imp_osf)

#exlusions for being peer-reviewed across samples
peer_reviewed_arxiv+peer_reviewed_osf

#create combined dataset
combined_data<-rbind(data2,data2_osf)
#sample size 
nrow(combined_data)

#descriptives
#Number of preprints informing their readers that:
#They are a preprint (or similar).
table(combined_data$preprint)
27+28
#percentage of preprints informing: They are a preprint 
100*(table(combined_data$preprint)[2] + table(combined_data$preprint)[3])/sum(table(combined_data$preprint))
#Are not peer-reviewed.
# (0 = "no", 1 = "yes", 2 = "under review", 3 = "submitted for publication")
table(combined_data$no_peerreview)
22+9
#percentage of preprints informing: They are not peer-reviewed 
100*(table(combined_data$no_peerreview)[2] + table(combined_data$no_peerreview)[3])/sum(table(combined_data$no_peerreview))
#Peer-review is part of the typical publication process.
table(combined_data$standard_procedure)
#Peer-review serves as a quality control measure.
table(combined_data$quality_control)
#Their findings might be preliminary (or similar).
table(combined_data$preliminary)

