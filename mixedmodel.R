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
write.csv(data.mix, 'mixdata.csv')

# model on imptued dataset
data.up=read.csv('imputation.csv')
data.mix.up=Reduce(function(x,y) full_join(x,y,by=c('ID','trt','month')), list(dt.str('WSDS', data=data.up), 
                                                                            dt.str('[^/.]lSDS$', data=data.up), 
                                                                            dt.str('hcSDS', data=data.up)))
write.csv(data.mix.up, 'mixdata2.csv')

# above 5 set trt=1, belw trt=0
proc mixed data=infants;
  class trt month ID;
  model weight=trt month month*trt / ddfm=hr;
  random ID / subject= trt;
  repeated month / subject=ID(trt) type=a(1);
  lsmeans month*trt;
run;
  
  
