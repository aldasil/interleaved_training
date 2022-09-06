library(tidyverse)
library(rstatix)
library(broom)
library(emmeans)
library(multcomp)
library(car)
library(Hmisc)
library(ggplot2)


#ancova

center = function(xs){
  xs - mean(xs)
}

ancova_test<-function(data,slope=F){
  if(slope==T){
    data$thresh.2=data$slope
    data$thresh.1=data$thresh
  }
  

  
  m0<-lm(thresh.2 ~ thresh.1*group,data)
  print(anova(m0))
  print(partial_eta_squared(m0))
  
  m<- lm(thresh.2 ~ center(thresh.1) + group,data)
  print(summary(m))
  
  print(Anova(m)) ## tests whether there is a difference across groups
  
  

    K=rbind('L24L24L'=c(1,0,0,0,0),
            'TL24hrL'=c(1,0,1,0,0),
            'intlv3'=c(1,0,0,1,0),
            'intlv1'=c(1,0,0,0,1))
  if(slope==T){
    mc = glht(m,K,rhs=rep(0,nrow(K)))
  }else{
    mc = glht(m,K,rhs=rep(mean(data$thresh.1),nrow(K)))}
  print(summary(mc))
  
  e<-emmeans(m,'group',infer=T)
  e1<-as.data.frame(e)
  e1$start<-mean(data$thresh.1)
  if(slope==T){
    e1$imprv<--e1$emmean
  }else{
    e1$imprv<-e1$start-e1$emmean}
  print(e1)
  
  
  mc = glht(m, mcp(group="Tukey")) #tells you what groups differ from one another
  e<-pairs(e,adjust='none')
  print(summary(mc))
  print(e)
  
  
}

correlate_measures<-function(){
  data1<-mean_data_wide
  data2<-offline_data_wide
  data3<-indislope_data
  data1$imprv_a<-data1$thresh.1-data1$thresh.2
  data2$imprv_o<-data2$thresh.1-data2$thresh.2
  data1<-subset(data1,select=c('sid','imprv_a'))
  data2<-subset(data2,select=c('sid','imprv_o'))
  data3<-subset(data3,select=c('sid','slope'))
  data_cor1<-merge(data1,data2,by='sid')
  data_cor<-merge(data_cor1,data3,by='sid')
  rcorr(as.matrix(data_cor[2:4]))
}




individual_slope<-function(data,slope=F){
  if(slope==T){
    data$thresh.2=data$slope
    data$thresh.1=data$thresh
  }
  #across-day learning
  # correlation between day 1 and day 2 depend on group or not
  a1<-lm(thresh.2 ~ thresh.1 * group,data)
  print(anova(a1)) #interaction not sig, meaning correlation independent of group. same slope across group.
  #eta_sq(a1)
  ##slope estimates:
  as1<-lm(thresh.2 ~ thresh.1 ,data)
  print(summary(as1))
  if(slope==T){
    et<-emtrends(a1,'group',var='thresh.1')
    print(et)
    print(pairs(et,adj = 'Tukey'))
  }
}

plot_bar<-function(data,yaxis,figure_no,slope=F){
  if(slope==T){
    data$thresh.2=data$slope
    data$thresh.1=data$thresh
  }
  
  m = lm(thresh.2 ~ center(thresh.1) + group,data)
  
  
  e1<-as.data.frame(emmeans(m,'group',infer=T))
  e1$start<-mean(data$thresh.1)
  if(slope==T){
    e1$imprv<--e1$emmean
  }else{
    e1$imprv<-e1$start-e1$emmean
  }
  
  
  e1$group<-factor(e1$group,levels=c('L24L24L','TL24hrL','intlv3','intlv1'),
                   labels = c('L-only','TL-blocked','TL-slow-\ninterleaved','TL-fast-\ninterleaved'))
  p1<-ggplot(data=e1,aes(x=group))+
    geom_col(aes(y=imprv,fill=group),color='black')+
    geom_errorbar(aes(ymin=imprv-SE,ymax=imprv+SE),width=0.2)+
    ylab(yaxis)+
    xlab('Group')+
    scale_fill_manual(values=c('white','black','gray40','gray80'))+
    theme(legend.position='none')+
    theme_classic(base_size=10)+
    theme(legend.position='none')
  return(p1)
  
  
}







plot_scatter<-function(df,xaxis,yaxis,figure_no,line_slope,line_intercept,slope=F){
  if(slope==T){
    df$result=df$slope
    df$pretest=df$thresh
  }else{
    df$result=df$thresh.2
    df$pretest=df$thresh.1
  }
  
  df$group<-factor(df$group,levels=c('L24L24L','TL24hrL','intlv3','intlv1'),
                   labels = c('L-only','TL blocked','TL slow-\ninterleaved','TL fast-\ninterleaved'))
  summary<-ddply(df,.(group),summarise,pretest=mean(pretest,na.rm = T),result=mean(result,na.rm = T))
  p1<-ggplot(df,aes(x=pretest,y=result,fill=group,group=group))+
    geom_point(size=1,alpha=0.7,shape=21)+
    geom_smooth(formula = y~x,se=F,method='lm',color='black',size=0.5)+
    geom_abline(slope=line_slope,intercept=line_intercept,linetype=2,size=0.5)+
    geom_point(data=summary,aes(x=pretest,y=result,fill=group),shape=24,size=2)+
    facet_wrap(~group,nrow=1)+
    theme_classic(base_size = 10)+
    xlab(xaxis)+
    ylab(yaxis)+
    scale_fill_manual(values=c('white','black','gray40','gray80'))+
    theme(legend.position='none')+
    theme(strip.background = element_blank(),strip.text.x = element_blank())+
    ggtitle(figure_no)+
    coord_equal()
  return (p1)
}
