# mixed model

# data structure 
# ID, weight, month, trt


data=readxl::read_excel('infants.xlsx')

# check any duplicate 
data=data%>%
  filter(ID!=' ')

# rename columns for easy selection 
data.n=data
names(data.n)[48]='BWSDS'
names(data.n)[50]='blsds'
names(data.n)[4]='b_mother_height'
names(data.n)[5]='b_mother_weight_pre'
names(data.n)[6]='b_mother_weight_curr'

# outier observed in weight, height
data.n[data.n$ID=='31989644',4]=1.6
data.n[data.n$ID=='40336307',137]=1.6
data.n[data.n$ID=='328716212',137]=1.6
data.n[data.n$ID=='37149101',135]=1.74
data.n[data.n$ID=='40336307',46]=3.32

# prepare different dataset for mixed model
# model on original dataset: 
# ID, weight(height, hc, and all relevant sds values) month, trt
# split(df, seq)

dt.str=function(x,data=data.n){
  data.n=data
  dt=data.n%>%
    mutate(trt=(pAHI>5)*1)%>%
    select(ID, trt, matches(x))
  names(dt)=c('ID','trt','0','1','4','12','24')
  dt=tidyr::gather(dt, month, x, -ID,-trt)
  names(dt)[4]=eval(x)
  if (stringr::str_detect(x,'\\$')){
    names(dt)[4]=ifelse(stringr::str_detect(x,'HC'),'HC', 'lSDS')
  }
  dt
}

data.mix=Reduce(function(x,y) full_join(x,y,by=c('ID','trt','month')), list(dt.str('weight_gr'), dt.str('length'), dt.str('HC$'),
                                                  dt.str('WSDS'), dt.str('[^//]lSDS$'), dt.str('hcSDS')))

# model on imptued dataset
data.up=read.csv('imputation.csv')
data.mix.up=Reduce(function(x,y) full_join(x,y,by=c('ID','trt','month')), list(dt.str('WSDS', data=data.up), 
                                                                            dt.str('[^/.]lSDS$', data=data.up), 
                                                                            dt.str('hcSDS', data=data.up)))

data.mix.bl= data.mix%>%
  group_by(ID)%>%
  mutate(bd.weight_gr= (sum(is.na(trt))==0 & sum(is.na(weight_gr))==0) *1)%>%
  mutate(bd.length = (sum(is.na(trt))==0 & sum(is.na(length))==0) *1)%>%
  mutate(bd.HC = (sum(is.na(trt))==0 & sum(is.na(HC))==0) *1)%>%
  mutate(bd.WSDS = (sum(is.na(trt))==0 & sum(is.na(WSDS))==0) *1)%>%
  mutate(bd.lSDS= (sum(is.na(trt))==0 & sum(is.na(lSDS))==0) *1)%>%
  mutate(bd.hcSDS = (sum(is.na(trt))==0 & sum(is.na(hcSDS))==0) *1)
write.csv(data.mix.bl, 'bl_mixdata.csv')
                   
                   
data.mix.up.bl= data.mix.up%>%
  group_by(ID)%>%
  mutate(bd.WSDS = (sum(is.na(trt))==0 & sum(is.na(WSDS))==0) *1)%>%
  mutate(bd.lSDS= (sum(is.na(trt))==0 & sum(is.na(lSDS))==0) *1)%>%
  mutate(bd.hcSDS = (sum(is.na(trt))==0 & sum(is.na(hcSDS))==0) *1)                   
write.csv(data.mix.up.bl, 'bl_mixdata_imput.csv')                   
                   
                   
# ranova for dataset 
# Repeated measures ANOVA: differences in mean scores under three or more different conditions, level (or related group)
# null hypothesis (H0) is that mean is the same at all time points

ranova=function(x, data=data.mix.up){
  data.mix=data
  data.mix$month=factor(data.mix$month, levels= c("0" , "1" , "4", "12", "24"))
  # create balanced dataset
  id.del= data.mix%>%
    filter(is.na(trt) | is.na(data.mix[[x]]))%>%
    select(ID)%>%
    distinct()
  
  # p-value
  d=anti_join(data.mix,id.del, by='ID')
  WSDS.aov=aov(d[[x]]~(month*trt) + Error(ID/(month)), anti_join(data.mix,id.del, by='ID'))
  print(summary(WSDS.aov))
  
  # plot mean-graph
  interaction.plot(d$month, d$trt, d[[x]],type="b", 
                   col=c(3,4), legend=F,lty=c(1,2), lwd=2, pch=c(2,4),
                   ylab="mean of outcome",main=x)
  legend("bottomright", c('below 5','above 5'), bty="n",lty=c(1,2),lwd=2,pch=c(2,4), col=c(3,4), 
         title="pAHI",inset = .02)
}


# import dataset
data.mix.up=read.csv('mixdata2.csv')
names(data.mix.up)

# wsds---------------------
# create balanced dataset
ranova('WSDS')

# lSDS----------------------
ranova('lSDS')

# hcSDS--------------------
ranova('hcSDS')



# import original dataset------------------------------------------------------------------------------
data.mix=read.csv('mixdata.csv')
names(data.mix)

# weight--------------------------
ranova('weight_gr', data.mix)

# WSDS-----------------------------
ranova('WSDS', data.mix)

# length---------------------------
ranova('length', data.mix)

# lSDS-----------------------------
ranova('lSDS', data.mix)

# HC--------------------------------
ranova('HC', data.mix)

# hcSDS------------------------------
ranova('hcSDS', data.mix)


# individual plot------------------------------------                  
comp.plot=function(data=data.mix, x=quote(weight_gr)){
  data=data.mix
  a=range(data[[x]], na.rm=T)[1]
  b=range(data[[x]], na.rm=T)[2]
  
  mn=data%>%
    group_by(trt,month)%>%
    summarize(f=mean(eval(x), na.rm=T))
  names(mn)[3]=paste(x)
  
  plot.below=
    data%>%
    filter(trt==0)%>%
    ggplot()+
    geom_line(aes(x=month, y=eval(x), group=ID, color=ID), show.legend = FALSE)+
    geom_line(data=filter(mn, trt==0), aes(x=month, y=eval(x),group=trt), color='black', size=2)+
    ylab(paste(x))+
    scale_y_continuous(limits=c(a, b))
  
  plot.above=
    data%>%
    filter(trt==1)%>%
    ggplot()+
    geom_line(aes(x=month, y=eval(x), group=ID, color=ID), show.legend = FALSE)+
    geom_line(data=filter(mn, trt==1), aes(x=month, y=eval(x),group=trt), color='black', size=2)+
    ylab(paste(x))+
    scale_y_continuous(limits=c(a, b))
  
  cowplot::plot_grid(plot.below, plot.above, labels=c('below 5', 'above 5'))
}

comp.plot()
comp.plot(x=quote(length))
comp.plot(x=quote(HC))
comp.plot(x=quote(WSDS))
comp.plot(x=quote(lSDS))
comp.plot(x=quote(hcSDS))

# on imputated data set---------------------
comp.plot(data.mix.up, x=quote(WSDS))
comp.plot(data.mix.up, x=quote(lSDS))
comp.plot(data.mix.up, x=quote(hcSDS))


                   


t_test=function(x, data=data.mix.up, m=0){
  data.m=data
  data=data.m%>%filter(month==m)
  data$month=factor(data$month)
  id.del= data%>%
    filter(is.na(trt) | is.na(data[[x]]))%>%
    select(ID)%>%
    distinct()
  d=anti_join(data,id.del, by='ID')
  # differenc in mean
  t.test(d[[x]][d$trt==1], d[[x]][d$trt==0], alternative = "two.sided", var.equal = FALSE)
}




for (i in c(0,1,4,12,24)){
  print(i)
  print(t_test('lSDS',m=i))
}

for (i in c(0,1,4,12,24)){
  print(i)
  print(t_test('hcSDS',m=i))
}

for (i in c(0,1,4,12,24)){
  print(i)
  print(t_test('WSDS', data=data.mix, m=i))
}

# boxplt
                   
                   
                   
                   
                   
 feature=function(x){
  baby.features=
    data%>%
    select(ID,contains(x))
  baby.features$miss.n=apply(baby.weights,1,function(x) sum(is.na(x)))
  knitr::kable(baby.features)
  hist(baby.weights$miss.n, main=paste0('# of missing data in ', x, ' across time'), xlab='n')
  features_growth=baby.features[,-7]
  names(features_growth)=c('ID','0','1','4','12','24')
  features_growth=
    tidyr::gather(features_growth,month,features,-ID)%>%
    arrange(ID)
  # change str to integer
  features_growth$month=strtoi(features_growth$month, base = 0L)
  # change ID to factor
  features_growth$ID=factor(features_growth$ID)
  return(features_growth)
}

                             
                             # visualize growth of this type feature
visualization=function(length_growth){
  ggplot(length_growth, aes(x=month, y=features, group=ID, color=ID))+
    geom_point(show.legend = FALSE)
  
  ggplot(length_growth)+
    geom_line(aes(x=month, y=features, group=ID, color=ID), show.legend = FALSE)+
    geom_line(data=mn(length_growth), aes(x=month, y=features), color='black', size=2)
  
#  p=ggplot(length_growth, aes(x=month, y=features))+
#    geom_point(aes(colour= factor(month)))
#  p+transition_states(
#    month, transition_length = 2, state_length = 1) 
}
                             
# cut-off point
`%not_in%`=purrr::negate(`%in%`)

cut.plot=function(x, value, weight_growth.new, mean.data=mn_weight, y=quote(weight),a=0,b=15){
  cutpoint=data%>%
    filter(eval(x)<value)%>%
    select(ID)
  
  plot.below=
    weight_growth.new%>%
    filter(ID %in% cutpoint$ID)%>%
    ggplot()+
    geom_line(aes(x=month, y=eval(y), group=ID, color=ID), show.legend = FALSE)+
    geom_line(data=mean.data, aes(x=month, y=eval(y)), color='black', size=2)+
    scale_y_continuous(limits=c(a, b))
  
  plot.above=
    weight_growth.new%>%
    filter(ID %not_in% cutpoint$ID)%>%
    ggplot()+
    geom_line(aes(x=month, y=eval(y), group=ID, color=ID), show.legend = FALSE)+
    geom_line(data=mean.data, aes(x=month, y=eval(y)), color='black', size=2)+
    scale_y_continuous(limits=c(a, b))
  
  cowplot::plot_grid(plot.below, plot.above, labels=c('above', 'below'))
}


