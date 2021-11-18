library(ggplot2)
library(ggthemes)
library(extrafont)
#create the introductiory figure
#create the dataframe
Server<-c(rep("OSF Preprints",4),rep("PsyArXiv",4))
Year<-c(c(2017:2020),c(2017:2020))
Number<-c(676,1690,4380,8870,441,937,1570,4660)
data<-data.frame(Server,Year,Number)
#create the figure, largely following https://t-redactyl.io/blog/2015/12/creating-plots-in-r-using-ggplot2-part-1-line-plots.html
#jpeg(filename="plot_intro.jpg", width=16.6, height=16.6, units="cm", res = 500)
p1 <- ggplot() + theme_bw() +
  geom_line(aes(y = Number, x = Year, colour = Server), size=2, data = data,
            stat="identity") +
  theme(text = element_text(size=16),panel.grid.major = element_blank(),legend.text=element_text(size=16),panel.grid.minor = element_blank(),legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
    scale_x_continuous(limits = c(2017, 2020),breaks=seq(2017,2020,1)) +
  scale_y_continuous(limits = c(0, 10000), breaks=seq(0,10000,1000)) +
  labs(x="Year", y="Number of Published Manuscripts")
p1+scale_color_manual(values=c("#FAB3B9", "#B8CEE6"))
#dev.off()

