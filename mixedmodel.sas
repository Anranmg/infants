* mixed model;

* data structure;
* ID, weight, month, trt;
proc import datafile = '/folders/myfolders/SASCrunch/bl_mixdata.csv' out = infants dbms = csv;

proc import datafile = '/folders/myfolders/SASCrunch/bl_mixdata_imput.csv' out = infants.up dbms = csv;

proc contents data= infants;
run;

* create balance dataset for sas;
data infant.m;
  set infants;
  keep trt month ID weight_gr;
  if bd.weight_gr==1;
run;

* above 5 set trt=1, belw trt=0;
proc mixed data=infant.m;
  class trt month ID;
  model weight_gr=trt month month*trt / ddfm=hr;
  random ID / subject= trt;
  repeated month / subject=ID(trt) type=a(1);
  lsmeans month*trt;
run;
  
  
