library(xtable) #for table creation for latex
library(ggplot2)#for graphics
library(MASS)#for qda
library(scales)#for scientific notation
library(RColorBrewer) #for base r plot
library(class) #for base r plot
library(plyr)#for obtaining means by factor
library(e1071)#for svm
#library(tree)#for tree based methods
library(nnet)#for multinomial regression
#importing custom functions to calculate classes via COLS
source('cols.r')

#defining proper scientific notation

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

#custom theme
mytheme.scat<-theme(

	plot.title = element_text(size=60, face="bold", hjust = 0.5),
	axis.text.x  = element_text(size=20, face="bold"),
	axis.text.y=element_text(size=20, face="bold"),
	axis.title.x=element_text(size=28, face='bold'),
	axis.title.y=element_text(size=28, face='bold'),
	strip.background=element_rect(fill="gray80"),
	panel.background=element_rect(fill="gray80"),
	axis.ticks= element_blank(),
	axis.text=element_text(colour="black"),
  strip.text = element_text(size=25)

	)

#################
#importing data for encircled image histograms

#round or circle
circ1 <-read.table("round1.txt", sep=",", header=TRUE)
circ2 <-read.table('round2.txt', sep=",", header=TRUE)
circ3 <-read.table("round3.txt", sep=",", header=TRUE)
circ4 <-read.table("round4.txt", sep=",", header=TRUE)
circ5 <-read.table("round5.txt", sep=",", header=TRUE)

circ<-rbind(circ1, circ2, circ3, circ4, circ5)

circ1_shapes <-read.table("round1_SHAPES.txt", sep=",", header=TRUE)
circ2_shapes <-read.table('round2_SHAPES.txt', sep=",", header=TRUE)
circ3_shapes <-read.table("round3_SHAPES.txt", sep=",", header=TRUE)
circ4_shapes <-read.table("round4_SHAPES.txt", sep=",", header=TRUE)
circ5_shapes <-read.table("round5_SHAPES.txt", sep=",", header=TRUE)

circ_shapes<-rbind(circ1_shapes, circ2_shapes, circ3_shapes,
                   circ4_shapes, circ5_shapes)

#capsule
caps <-read.table("caps.txt", sep=",", header=TRUE)

caps_shapes <-read.table("caps_SHAPES.txt", sep=",", header=TRUE)

#diamond
diam <-read.table("diam.txt", sep=",", header=TRUE)

diam_shapes <-read.table("diam_SHAPES.txt", sep=",", header=TRUE)

#hexagon
hex1 <-read.table("hex.txt", sep=",", header=TRUE)
hex2 <-read.table("reg_hex.txt", sep=",", header=TRUE)

hex<-rbind(hex1, hex2)

hex_shapes1 <-read.table("hex_SHAPES.txt", sep=",", header=TRUE)
hex_shapes2 <-read.table("reg_hex_SHAPES.txt", sep=",", header=TRUE)

hex_shapes<-rbind(hex_shapes1, hex_shapes2)


#oval
oval1 <-read.table("oval1.txt", sep=",", header=TRUE)
oval2 <-read.table("oval2.txt", sep=",", header=TRUE)
oval3 <-read.table("oval3.txt", sep=",", header=TRUE)

oval<-rbind(oval1, oval2, oval3)

oval1_SHAPES <-read.table("oval1_SHAPES.txt", sep=",", header=TRUE)
oval2_SHAPES <-read.table("oval2_SHAPES.txt", sep=",", header=TRUE)
oval3_SHAPES <-read.table("oval3_SHAPES.txt", sep=",", header=TRUE)

oval_shapes<-rbind(oval1_SHAPES, oval2_SHAPES, oval3_SHAPES)

#pentagon
pent1 <-read.table("pent.txt", sep=",", header=TRUE)
pent2 <-read.table("reg_pent.txt", sep=",", header=TRUE)

pent<-rbind(pent1, pent2)

pent_shapes1 <-read.table("pent_SHAPES.txt", sep=",", header=TRUE)
pent_shapes2 <-read.table("reg_pent_SHAPES.txt", sep=",", header=TRUE)

pent_shapes<-rbind(pent_shapes1, pent_shapes2)

#rectangle
rect <-read.table("rect.txt", sep=",", header=TRUE)

rect_shapes <-read.table("rect_SHAPES.txt", sep=",", header=TRUE)

#semi-circle
sc <-read.table("sc.txt", sep=",", header=TRUE)

sc_shapes <-read.table("sc_SHAPES.txt", sep=",", header=TRUE)

#square
squ <-read.table("squ.txt", sep=",", header=TRUE)

squ_shapes <-read.table("squ_SHAPES.txt", sep=",", header=TRUE)

#tear
tear <-read.table("tear.txt", sep=",", header=TRUE)

tear_shapes <-read.table("tear_SHAPES.txt", sep=",", header=TRUE)

#trapezoid
trap <-read.table("trap.txt", sep=",", header=TRUE)

trap_shapes <-read.table("trap_SHAPES.txt", sep=",", header=TRUE)

#triangle
tri <-read.table("tri.txt", sep=",", header=TRUE)

tri_shapes <-read.table("tri_SHAPES.txt", sep=",", header=TRUE)


#cleaning data for ggplot2 and analysis
labs<-as.factor(c(rep(1, dim(caps)[1]),
                  rep(2, dim(diam)[1]),
                  rep(3, dim(hex)[1]),
                  rep(4, dim(oval)[1]),
                  rep(5, dim(pent)[1]), rep(6, dim(rect)[1]),
                  rep(7, dim(circ)[1]),
                  rep(8, dim(sc)[1]),
                  rep(9, dim(squ)[1]), rep(10, dim(tear)[1]),
                  rep(11, dim(trap)[1]),rep(12, dim(tri)[1])    ) )

mydata<-rbind(caps,
              diam,
              hex,
              oval,
              pent, rect,
              circ,
              sc, squ,
              tear, trap, tri)

#12 classes
myshape<-rbind(caps_shapes,
              diam_shapes,
              hex_shapes,
              oval_shapes,
              pent_shapes, rect_shapes,
              circ_shapes,
              sc_shapes, squ_shapes,
              tear_shapes, trap_shapes, tri_shapes)

#counts plot
temp<-as.data.frame(cbind(labs, mydata))

labs2<-as.factor(c(rep("Capsule", dim(caps)[1]),
                  rep("Diamond", dim(diam)[1]),
                  rep("Hexagon", dim(hex)[1]),
                  rep("Oval", dim(oval)[1]),
                  rep("Pentagon", dim(pent)[1]),
                  rep("Rectangle", dim(rect)[1]), rep("Round", dim(circ)[1]),
                  rep("Semi-Circle", dim(sc)[1]), rep("Square", dim(squ)[1]),
                  rep("Tear", dim(tear)[1]), rep("Trapezoid", dim(trap)[1]),
                  rep("Triangle", dim(tri)[1])    ) )

#EI plot
scat<-ggplot(data=temp, aes(x = white, y = black, colour = as.factor(labs2)))+
          geom_point(size=2)+
	 	      ggtitle("EI for Pillbox Data")+
		      xlab("White Counts")+
					ylab("Black Counts")+
			 		labs(colour= "Legend")+
					scale_y_continuous(label=scientific_10)+
          scale_x_continuous(label=scientific_10)+
          mytheme.scat+
          scale_color_discrete(breaks=c( "Capsule",
                            "Diamond",
                            "Hexagon",
                            "Oval",
                            "Pentagon",
                            "Rectangle", "Round",
                            "Semi-Circle", "Square",
                            "Tear", "Trapezoid",
                            "Triangle"  ))+
          theme(legend.text=element_text(size=18),
                legend.title=element_text(size=24))

ggsave(filename="plots/Encircled_Image_Histograms_Pillbox.png", plot=scat,
       width=9, height=7)

#Circularity vs. Eccentricity
scat<-ggplot(data=myshape, aes(x = Shape_circ, y = Shape_eccent, colour = as.factor(labs2)))+
          geom_point(size=2)+
	 	      ggtitle("Circularity vs.\nEccentricity")+
		      xlab("Circularity")+
					ylab("Eccentricity")+
			 		labs(colour= "Legend")+
					scale_y_continuous(label=scientific_10)+
          scale_x_continuous(label=scientific_10)+
          mytheme.scat+
          scale_color_discrete(breaks=c( "Capsule",
                            "Diamond",
                            "Hexagon",
                            "Oval",
                            "Pentagon",
                            "Rectangle", "Round",
                            "Semi-Circle", "Square",
                            "Tear", "Trapezoid",
                            "Triangle"    ))+
          theme(legend.text=element_text(size=18),
                legend.title=element_text(size=24))

ggsave(filename="plots/circ_ecc_Pillbox.png", plot=scat,
       width=9, height=7)

#
#Sp values vs. Eccentricity
scat<-ggplot(data=temp, aes(x = white/(white+black), y = Shape_eccent, colour = as.factor(labs2)))+
          geom_point(size=2)+
	 	      ggtitle("SP Values vs.\nEccentricity")+
		      xlab("SP Values")+
					ylab("Eccentricity")+
			 		labs(colour= "Legend")+
					scale_y_continuous(label=scientific_10)+
          scale_x_continuous(label=scientific_10)+
          mytheme.scat+
          scale_color_discrete(breaks=c( "Capsule",
                            "Diamond",
                            "Hexagon",
                            "Oval",
                            "Pentagon",
                            "Rectangle", "Round",
                            "Semi-Circle", "Square",
                            "Tear", "Trapezoid",
                            "Triangle"    ))+
          theme(legend.text=element_text(size=18),
                legend.title=element_text(size=24))

ggsave(filename="plots/sp_ecc_Pillbox.png", plot=scat,
       width=9, height=7)

#E1 vs. E2
scat<-ggplot(data=myshape, aes(x = Shape_e1, y = Shape_e2, colour = as.factor(labs2)))+
          geom_point(size=2)+
	 	      ggtitle("1st and 2nd\nEigenvalues")+
		      xlab("E1")+
					ylab("E2")+
			 		labs(colour= "Legend")+
					scale_y_continuous(label=scientific_10)+
          scale_x_continuous(label=scientific_10)+
          mytheme.scat+
          scale_color_discrete(breaks=c( "Capsule",
                            "Diamond",
                            "Hexagon",
                            "Oval",
                            "Pentagon",
                            "Rectangle", "Round",
                            "Semi-Circle", "Square",
                            "Tear", "Trapezoid",
                            "Triangle"    ))+
          theme(legend.text=element_text(size=18),
                legend.title=element_text(size=24))

ggsave(filename="plots/e1_e2_Pillbox.png", plot=scat,
       width=9, height=7)

#
#Number of Corners vs. Eccentricity
scat<-ggplot(data=myshape, aes(x = Shape_corn, y = Shape_eccent, colour = as.factor(labs2)))+
          geom_point(size=2)+
	 	      ggtitle("Number of Corners\nvs. Eccentricity")+
		      xlab("Number of Corners")+
					ylab("Eccentricity")+
			 		labs(colour= "Legend")+
					scale_y_continuous(label=scientific_10)+
          scale_x_continuous(label=scientific_10)+
          mytheme.scat+
          scale_color_discrete(breaks=c( "Capsule",
                            "Diamond",
                            "Hexagon",
                            "Oval",
                            "Pentagon",
                            "Rectangle", "Round",
                            "Semi-Circle", "Square",
                            "Tear", "Trapezoid",
                            "Triangle"    ))+
          theme(legend.text=element_text(size=18),
                legend.title=element_text(size=24))

ggsave(filename="plots/corn_ecc_Pillbox.png", plot=scat,
       width=9, height=7)

#scatterplot matrix
temp<-as.data.frame(cbind(labs, mydata, myshape))

#plot(temp, col=temp$labs)

#Number of EI White vs. Black Box
scat<-ggplot(data=temp, aes(x = white, y = Black_box, colour = as.factor(labs2)))+
          geom_point(size=2)+
	 	      ggtitle("White Counts\nvs. Black Box")+
		      xlab("White Counts")+
					ylab("Black Box")+
			 		labs(colour= "Legend")+
					scale_y_continuous(label=scientific_10)+
          scale_x_continuous(label=scientific_10)+
          mytheme.scat+
          scale_color_discrete(breaks=c( "Capsule",
                            "Diamond",
                            "Hexagon",
                            "Oval",
                            "Pentagon",
                            "Rectangle", "Round",
                            "Semi-Circle", "Square",
                            "Tear", "Trapezoid",
                            "Triangle"    ))+
          theme(legend.text=element_text(size=18),
                legend.title=element_text(size=24))

ggsave(filename="plots/white_black_box.png", plot=scat,
       width=9, height=7)


#Number of White Box vs. Black Box
scat<-ggplot(data=temp, aes(x = White_box, y = Black_box, colour = as.factor(labs2)))+
          geom_point(size=2)+
	 	      ggtitle("White Box\nvs. Black Box")+
		      xlab("White Box")+
					ylab("Black Box")+
			 		labs(colour= "Legend")+
					scale_y_continuous(label=scientific_10)+
          scale_x_continuous(label=scientific_10)+
          mytheme.scat+
          scale_color_discrete(breaks=c( "Capsule",
                            "Diamond",
                            "Hexagon",
                            "Oval",
                            "Pentagon",
                            "Rectangle", "Round",
                            "Semi-Circle", "Square",
                            "Tear", "Trapezoid",
                            "Triangle"    ))+
          theme(legend.text=element_text(size=18),
                legend.title=element_text(size=24))

ggsave(filename="plots/white_box_black_box.png", plot=scat,
       width=9, height=7)

#plot for capsule, oval, round and Rectangle

#cleaning data for ggplot2 and analysis
labs<-as.factor(c(rep(1, dim(caps)[1]),
                  rep(4, dim(oval)[1]),
                  rep(6, dim(rect)[1]),
                  rep(7, dim(circ)[1])    ) )

mydata<-rbind(caps,
              oval,
              rect,
              circ )

#12 classes
myshape<-rbind(caps_shapes,
              oval_shapes,
              rect_shapes,
              circ_shapes)

#counts plot
temp<-as.data.frame(cbind(labs, mydata))

labs2<-as.factor(c(rep("Capsule", dim(caps)[1]),
                  rep("Oval", dim(oval)[1]),
                  rep("Rectangle", dim(rect)[1]), rep("Round", dim(circ)[1]) ) )


#Circularity vs. Eccentricity
scat<-ggplot(data=myshape, aes(x = Shape_circ, y = Shape_eccent, colour = as.factor(labs2)))+
          geom_point(size=2)+
	 	      ggtitle("Circularity vs.\nEccentricity")+
		      xlab("Circularity")+
					ylab("Eccentricity")+
			 		labs(colour= "Legend")+
					scale_y_continuous(label=scientific_10)+
          scale_x_continuous(label=scientific_10)+
          mytheme.scat+
          scale_color_discrete(breaks=c( "Capsule",
                            "Oval",
                            "Rectangle", "Round"    ))+
          theme(legend.text=element_text(size=18),
                legend.title=element_text(size=24))

ggsave(filename="plots/circ_ecc_larger.png", plot=scat,
       width=9, height=7)



#

#cleaning data for ggplot2 and analysis
labs<-as.factor(c(rep(2, dim(diam)[1]),
                  rep(3, dim(hex)[1]),
                  rep(5, dim(pent)[1]),
                  rep(9, dim(squ)[1]),
                  rep(11, dim(trap)[1])    ) )

mydata<-rbind(diam,
              hex,
              pent,
              squ,
              trap)

#12 classes
myshape<-rbind(diam_shapes,
              hex_shapes,
              pent_shapes,
              squ_shapes,
              trap_shapes)

#counts plot
temp<-as.data.frame(cbind(labs, mydata))

labs2<-as.factor(c(rep("Diamond", dim(diam)[1]),
                  rep("Hexagon", dim(hex)[1]),
                  rep("Pentagon", dim(pent)[1]),
                  rep("Square", dim(squ)[1]),
                  rep("Trapezoid", dim(trap)[1]) ) )


#Number of Corners vs. Eccentricity
scat<-ggplot(data=myshape, aes(x = Shape_corn, y = Shape_eccent, colour = as.factor(labs2)))+
          geom_point(size=2)+
	 	      ggtitle("Number of Corners\nvs. Eccentricity")+
		      xlab("Number of Corners")+
					ylab("Eccentricity")+
			 		labs(colour= "Legend")+
					scale_y_continuous(label=scientific_10)+
          scale_x_continuous(label=scientific_10)+
          mytheme.scat+
          scale_color_discrete(breaks=c("Diamond",
                            "Hexagon",
                            "Pentagon",
                            "Square",
                            "Trapezoid",
                            "Triangle"    ))+
          theme(legend.text=element_text(size=18),
                legend.title=element_text(size=24))

ggsave(filename="plots/corn_ecc_some.png", plot=scat,
       width=9, height=7)


##cleaning data for ggplot2 and analysis for Pentagon and Hexagon
labs<-as.factor(c(rep(3, dim(hex)[1]),
                  rep(5, dim(pent)[1]) ) )

mydata<-rbind(hex,
              pent)

#12 classes
myshape<-rbind(hex_shapes,
              pent_shapes)

#counts plot
temp<-as.data.frame(cbind(labs, mydata))

labs2<-as.factor(c(rep("Hexagon", dim(hex)[1]),
                  rep("Pentagon", dim(pent)[1]) ) )


#Number of Corners vs. Eccentricity
scat<-ggplot(data=mydata, aes(x = white, y = black, colour = as.factor(labs2)))+
          geom_point(size=2)+
	 	      ggtitle("EI for Pent\nand Hex")+
		      xlab("White")+
					ylab("Black")+
			 		labs(colour= "Legend")+
					scale_y_continuous(label=scientific_10)+
          scale_x_continuous(label=scientific_10)+
          mytheme.scat+
          scale_color_discrete(breaks=c("Hexagon",
                            "Pentagon"    ))+
          theme(legend.text=element_text(size=18),
                legend.title=element_text(size=24))

ggsave(filename="plots/ei_hex_pent.png", plot=scat,
       width=9, height=7)

#

#Number of Corners vs. Eccentricity
scat<-ggplot(data=myshape, aes(x = Hu1, y = Hu7, colour = as.factor(labs2)))+
          geom_point(size=2)+
	 	      ggtitle("Hu 1 and 7 for Pent\nand Hex")+
		      xlab("Hu1")+
					ylab("Hu7")+
			 		labs(colour= "Legend")+
					scale_y_continuous(label=scientific_10)+
          scale_x_continuous(label=scientific_10)+
          mytheme.scat+
          scale_color_discrete(breaks=c("Hexagon",
                            "Pentagon"    ))+
          theme(legend.text=element_text(size=18),
                legend.title=element_text(size=24))

ggsave(filename="plots/hu1and7_hex_pent.png", plot=scat,
       width=9, height=7)

#
#Number of Corners vs. Eccentricity
scat<-ggplot(data=myshape, aes(x = Shape_circ, y = Shape_eccent, colour = as.factor(labs2)))+
          geom_point(size=2)+
	 	      ggtitle("Circ and Eccent for Pent\nand Hex")+
		      xlab("Circ")+
					ylab("Eccent")+
			 		labs(colour= "Legend")+
					scale_y_continuous(label=scientific_10)+
          scale_x_continuous(label=scientific_10)+
          mytheme.scat+
          scale_color_discrete(breaks=c("Hexagon",
                            "Pentagon"    ))+
          theme(legend.text=element_text(size=18),
                legend.title=element_text(size=24))

ggsave(filename="plots/circ_eccent_hex_pent.png", plot=scat,
       width=9, height=7)

#
#Number of Corners vs. Eccentricity
scat<-ggplot(data=myshape, aes(x = Shape_e1, y = Shape_e2, colour = as.factor(labs2)))+
          geom_point(size=2)+
	 	      ggtitle("E1 and E2 for Pent\nand Hex")+
		      xlab("E1")+
					ylab("E2")+
			 		labs(colour= "Legend")+
					scale_y_continuous(label=scientific_10)+
          scale_x_continuous(label=scientific_10)+
          mytheme.scat+
          scale_color_discrete(breaks=c("Hexagon",
                            "Pentagon"    ))+
          theme(legend.text=element_text(size=18),
                legend.title=element_text(size=24))

ggsave(filename="plots/e1_e2_hex_pent.png", plot=scat,
       width=9, height=7)


#
#Number of Corners vs. Eccentricity
scat<-ggplot(data=myshape, aes(x = White_box, y = Black_box, colour = as.factor(labs2)))+
          geom_point(size=2)+
	 	      ggtitle("White and Black Box for Pent\nand Hex")+
		      xlab("White Box")+
					ylab("Black Box")+
			 		labs(colour= "Legend")+
					scale_y_continuous(label=scientific_10)+
          scale_x_continuous(label=scientific_10)+
          mytheme.scat+
          scale_color_discrete(breaks=c("Hexagon",
                            "Pentagon"    ))+
          theme(legend.text=element_text(size=18),
                legend.title=element_text(size=24))

ggsave(filename="plots/white_and_black_box_hex_pent.png", plot=scat,
       width=9, height=7)



#
