#calculation of mean average percision (map)

#only considers classes that are the same

#maddala

tp_fp_m<-c(233, 13, 3,
		  785, 8, 4,
		  1051, 7, 9,
		  5, 15)

fp_m<-c( 3, 5, 0,
	   	6, 0, 0,
	    0, 0, 0,
	    2, 5 )

per_m<-(tp_fp_m - fp_m)/tp_fp_m
mean(per_m)

#maddala-logistic regression

tp_fp_ml<-c(233, 6, 0,
		  784, 0, 1,
		  1077, 0, 9,
		  2, 9)

fp_ml<-c( 2, 2, 0,
	   	17, 0, 0,
	    27, 0, 0,
	    0, 1 )

per_ml<-(tp_fp_ml - fp_ml)/tp_fp_ml
mean(per_ml)

#lamberti

tp_fp_l<-c(23+307+41, 12, 8,
		  27+622, 12, 6,
		  904, 8, 10,
		  4, 12)

fp_l<-c( 41, 0, 0,
	   	2, 0, 0,
	    0, 0, 0,
	    0, 0 )

per_l<-(tp_fp_l - fp_l)/tp_fp_l
mean(per_l)

#bootstrap validation
tp_v<-c( 991, 945 )

fp_v<-c( 9, 55 )

per_v<-c(tp_v/(tp_v+fp_v), rep(1, 10))

per_v

mean(per_v)

#
