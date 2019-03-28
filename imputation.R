# imputation for height, weight

library('dplyr')
library('ggplot2')
library('imputeTS')

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


# imputation, delete observation when all values are missing 
table.imput=function(x){
  baby=data.n%>%
    select(ID,matches(x))
  
  # count na points
  baby$miss.n=apply(baby, 1, function(x) sum(is.na(x)))
  
  # at least two non-NA points
  baby.imput=baby%>%
    filter(miss.n< ncol(baby)-3)
  # smooth growth rate of weight
  baby.intp=baby.imput%>%
    select(-ID, -miss.n)%>%
    t()%>%
    na.interpolation(option='spline')%>%
    t()%>%
    as.data.frame()
  baby.intp=cbind(ID=baby.imput$ID,baby.intp) 
  
  # only one-NA points
  baby.imput=baby%>%
    filter(miss.n == ncol(baby)-3)
  # mean value imputation
  baby.mn=baby.imput%>%
    select(-ID, -miss.n)%>%
    t()%>%
    na.mean()%>%
    t()%>%
    as.data.frame()
  baby.mn=cbind(ID=baby.imput$ID,baby.mn) 
  
  # combine together
  rbind(baby.intp, baby.mn)
}


# imputation on weight
imputation=Reduce(function(x,y) full_join(x,y,by='ID'), 
                  list(table.imput('mother_weight'),table.imput('mother_height'),
                       table.imput('father_weight'),table.imput('father_height'),
                       table.imput('WSDS'),table.imput('[^//]lSDS$'),
                       table.imput('hcSDS')))


# updated dataset
data.up=
  data.n%>%
  select(-matches('mother_weight'), -matches('mother_height'),
         -matches('father_weight'), -matches('father_height'),
         -matches('WSDS'), -matches('[^//]lSDS$'),
         -matches('hcSDS'))%>%
  left_join(imputation, by='ID')

# store imputation
write.csv(data.up, file='./imputation.csv')


