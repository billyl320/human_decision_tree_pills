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


#lamberti

tp_fp_l<-c(402, 12, 8,
		  618, 12, 6,
		  904, 8, 10, 
		  4, 12)

fp_l<-c( 71, 0, 0,
	   	1, 0, 0,
	    0, 0, 0,
	    0, 0 )

per_l<-(tp_fp_l - fp_l)/tp_fp_l
mean(per_l)



#
