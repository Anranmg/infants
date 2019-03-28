# mixed model

# data structure 
# ID, weight, month, trt


# above 5 set trt=1, belw trt=0
proc mixed data=infants;
  class trt month ID;
  model weight=trt month month*trt / ddfm=hr;
  random ID / subject= trt;
  repeated month / subject=ID(trt) type=a(1);
  lsmeans month*trt;
run;
  
  