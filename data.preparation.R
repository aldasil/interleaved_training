library(plyr)
library(gridExtra)
library(effectsize)

#setwd("")

data<-read.csv('data.csv',header=T)

data$session<-factor(data$session,levels=c('0h','30m','24h'))
data$group<-factor(data$group,levels=c('L24L24L','TL24hrL','intlv3','intlv1'))
data$day<-as.factor(data$day)

experimental_conditions = c('TL24hrL','intlv3','intlv1','L24L24L')
mean_data = ddply(data,.(sid,group,day),summarise,thresh=mean(thresh,na.rm = T))
mean_data_wide<-reshape(mean_data,direction = 'wide',timevar = 'day',idvar = c('sid','group'))
mean_data_widesum<-ddply(mean_data_wide,.(group),summarise,thresh1=mean(thresh.1,na.rm = T),thresh2=mean(thresh.2,na.rm = T),n1=length(thresh.1),n2=length(thresh.2))


offline_data = subset(data,day==1&time=='last'|day==2&time=='first')
offline_data_wide<-reshape(offline_data,direction = 'wide',timevar = 'day',idvar = c('sid','group'))
offline_data_widesum<-ddply(offline_data_wide,.(group),summarise,thresh1=mean(thresh.1,na.rm = T),thresh2=mean(thresh.2,na.rm = T))
offline_data$time<-factor(offline_data$time,levels=c('last','first'))

within_data = subset(data,day==1)
within_data$logblock_top=log(within_data$block_top)

indislope<-ddply(within_data,c('sid','group','stimulus'),function(d){
  m<-lm(thresh~log(block_top),d)
  d$slope = coef(m)[2]
  d$r = summary(m)$r.squared
  d
})
indislope_data<-subset(indislope,time=='first')
indislope_datasum<-ddply(indislope_data,.(group),summarise,thresh=mean(thresh,na.rm = T),slope=mean(slope,na.rm = T))



#### ANALYSIS #######
source("analysis.R")


#ANCOVA - group difference in final value and pairwise comparison
#across-day
ancova_test(mean_data_wide)

#offline
ancova_test(offline_data_wide)

#within-session
ancova_test(indislope_data,slope=T)


#correlation
correlate_measures()


#individual slope
individual_slope(mean_data_wide)

individual_slope(offline_data_wide)

individual_slope(indislope_data,slope=T)


###approximate d from t - https://cran.r-project.org/web/packages/effectsize/vignettes/effectsize.html
###
t_to_d(t = 6.93, df_error = 50) #get numbers from test results


#####FIGURES #########


f1a<-plot_bar(mean_data_wide,'Across-day\nimprovement (dB)','Across-day')
f2a<-plot_bar(offline_data_wide,'Offline\nimprovement (dB)','Offline')
f3a<-plot_bar(indislope_data,'Within-session\nimprovement (Slope)','Within-session',slope=T)

f1b<-plot_scatter(mean_data_wide,'L Threshold day 1 (dB)','L Threshold day 2 (dB)','Across-day',1,0)
f2b<-plot_scatter(offline_data_wide,'Day 1 last block threshold(dB)','Day 2 first block threshold (dB)','Offline',1,0)
f3b<-plot_scatter(indislope_data,'Day 1 first block threshold (dB)','Slope','Within-session',0,0,slope=T)

f1b<-f1b+xlim(1.4,6.5)+ylim(1.4,8)

f3b<-f3b+ylim(-2.5,3.3)

f1<-grid.arrange(f1a,f2a,f3a,ncol=1)
f2<-grid.arrange(f1b,f2b,f3b,ncol=1)


#output
pdf(paste("output-figures" ,Sys.Date(), "pdf",sep='.'), width=7,height=5)
grid.arrange(f1,f2,nrow=1,widths=c(1,1))
dev.off()
