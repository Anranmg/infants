# mixed model

# data structure 
# ID, weight, month, trt
proc import datafile = '/folders/myfolders/SASCrunch/mixdata.csv' out = infants dbms = csv;

proc import datafile = '/folders/myfolders/SASCrunch/mixdata2.csv' out = infants.up dbms = csv;

proc contents data= infants;
run;

# above 5 set trt=1, belw trt=0
proc mixed data=infants;
  class trt month ID;
  model weight_gr=trt month month*trt / ddfm=hr;
  random ID / subject= trt;
  repeated month / subject=ID(trt) type=a(1);
  lsmeans month*trt;
run;
  
  
