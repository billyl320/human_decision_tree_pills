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

labs2<-as.factor(c(rep("Capsule", dim(caps)[1]),
                  rep("Diamond", dim(diam)[1]),
                  rep("Hexagon", dim(hex)[1]),
                  rep("Oval", dim(oval)[1]),
                  rep("Pentagon", dim(pent)[1]),
                  rep("Rectangle", dim(rect)[1]), rep("Round", dim(circ)[1]),
                  rep("Semi-Circle", dim(sc)[1]), rep("Square", dim(squ)[1]),
                  rep("Tear", dim(tear)[1]), rep("Trapezoid", dim(trap)[1]),
                  rep("Triangle", dim(tri)[1])    ) )

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


temp<-as.data.frame(cbind(labs, mydata, myshape))

#QDA model

#temp<-as.data.frame(cbind(as.factor(labs_train), mytrain))
#colnames(temp)[1]<-"labs"

keep<-which( ( labs== 1)| ( labs== 4)|
             ( labs== 6)| ( labs== 7) )

labs_qda<-as.numeric(labs)
labs_qda[keep]<-1
labs_qda[-keep]<-0

test<-as.data.frame(cbind(labs_qda, mydata, myshape))

sp<-test$white/(test$white+test$black)

#fix mistake
svns<-which(labs==7)

#boxplot(sp[svns])
#table(round(sp[svns], 2))
#pi/4

look_round<-which(sp[svns]<0.70)

labs[svns[look_round]]=4
labs2[svns[look_round]]="Oval"

keep<-which( ( labs== 1)| ( labs== 4)|
             ( labs== 6)| ( labs== 7) )

labs_qda<-as.numeric(labs)
labs_qda[keep]<-1
labs_qda[-keep]<-0

test<-as.data.frame(cbind(labs_qda, mydata, myshape))

sp<-test$white/(test$white+test$black)


plot(sp, myshape$Shape_eccent, col=as.factor(labs_qda))

#creating model
lda.fit = qda(labs_qda ~ sp + Shape_eccent + sp*Shape_eccent,
              data=test)

#predicting
lda.pred=predict(lda.fit, test)
lda.class = lda.pred$class

#results
table(lda.class, labs_qda)
#overall classification rate for training
mean(lda.class==as.factor(as.numeric(labs_qda)))



#simple model to check if it is even possible to do well with svm
#using all of the data

test<-as.data.frame(cbind(as.factor(labs_qda), sp, myshape))
colnames(test)[1]<-"labs_qda"

svmfit=svm(labs_qda ~ sp + Shape_eccent,
       data=test,
       #kernel='radial',
       kernel="polynomial",
       cost=1, coef0= 2, degree=5)
       #gamma=0.1)

plot(svmfit, test, Shape_eccent ~ sp)
summary(svmfit)

ypred=predict(svmfit ,test)
table(predict=ypred, truth=test$labs_qda)
mean(ypred==as.factor(as.numeric(labs_qda)))

#We are able to get 100% classification.  Now we wish to do it using less data

#step 1
#classifying capsule, oval, round, and rectangle vs. else
#using sp values and eccentricity

set.seed(379737)

#sample from larger observations except round
#keep<-which( ( labs== 1)| ( labs== 4)|
#             ( labs== 6), | ( labs== 7) )

keep_rect<-which( ( labs== 6) )
keep_circ<-which( ( labs== 7) )
keep_cap<-which( ( labs== 1) )
keep_oval<-which( ( labs== 4) )

cap_rand<-sample(keep_cap, 332)
train_01<-cap_rand[1:25]
oval_rand<-sample(keep_oval, 688)
train_4<-oval_rand[1:25]

rect_rand<-sample(keep_rect, 6)
train_6<-rect_rand[1:3]
circ_rand<-sample(keep_circ, 904)
train_7<-circ_rand[1:25]


train_1<-c(train_01, train_4,
           train_6, train_7)

#sample from smaller
keep_diam<-which( ( labs== 2) )
keep_hex<-which( ( labs== 3) )
keep_pent<-which( ( labs== 5) )
keep_sc<-which( ( labs== 8) )
keep_squ<-which( ( labs== 9) )
keep_tear<-which( ( labs== 10) )
keep_trap<-which( ( labs== 11) )
keep_tri<-which( ( labs== 12) )

diam_rand<-sample(keep_diam, 12)
train_2<-diam_rand[1:6]
hex_rand_reg<-sample(keep_hex[1:6], 6)
hex_rand_other<-sample(keep_hex[7:8], 2)
hex_rand<-c(hex_rand_reg[1:3], hex_rand_other[1],
            hex_rand_reg[4:6], hex_rand_other[2])
train_3<-hex_rand[1:4]

pent_rand<-sample(keep_pent, 12)
train_5<-pent_rand[1:6]
sc_rand<-sample(keep_sc, 4)
train_8<-sc_rand[1:2]

squ_rand<-sample(keep_squ, 8)
train_9<-squ_rand[1:4]
tear_rand<-sample(keep_tear, 10)
train_10<-tear_rand[1:5]

trap_rand<-sample(keep_trap, 4)
train_11<-trap_rand[1:2]
tri_rand<-sample(keep_tri, 12)
train_12<-tri_rand[1:6]

train_0<- c(train_2, train_3,
            train_5, train_8,
            train_9, train_10,
            train_11, train_12)

train_vals<-c(train_1, train_0)

keep_qda<-c(keep_rect, keep_circ,
            keep_cap, keep_oval)

labs_qda<-as.numeric(labs)
labs_qda[keep_qda]<-1
labs_qda[-keep_qda]<-0

test<-as.data.frame(cbind(labs_qda[train_vals],
                          sp[train_vals],
                          myshape$Shape_eccent[train_vals]))
colnames(test)[1]<-"labs_svm"
colnames(test)[2]<-"SP"
colnames(test)[3]<-"Eccent"

valid<-as.data.frame(cbind(labs_qda[-train_vals],
                                     sp[-train_vals],
                                     myshape$Shape_eccent[-train_vals]))
colnames(valid)[1]<-"labs_svm"
colnames(valid)[2]<-"SP"
colnames(valid)[3]<-"Eccent"

svmfit=svm(as.factor(labs_svm) ~ SP + Eccent,
       data=test,
       #kernel='radial',
       kernel="polynomial",
       cost=1, coef0= 2, degree=5)
       #gamma=0.1)

#testing
plot(svmfit, test, Eccent~ SP)
summary(svmfit)

ypred=predict(svmfit ,test)
table(predict=ypred, truth=test$labs_svm)
mean(ypred== test$labs_svm )

#validation
plot(svmfit, valid, Eccent ~ SP)

ypred=predict(svmfit ,valid)
table(predict=ypred, truth=valid$labs_svm)
mean(ypred== valid$labs_svm )


########################################################
#classifying  round vs. capsule and oval and rectangle
#using circularity and SP

#step 1.1
#SVM model
########################################################

#training setup
train_vals<-c(train_01, train_4, train_6, train_7)

labs_qda<-as.numeric(labs)
labs_qda[c(circ_rand)]<-1
labs_qda[-c(circ_rand)]<-0

test<-as.data.frame(cbind(labs_qda[train_vals],
                          myshape$Shape_circ[train_vals],
                          sp[train_vals]))
colnames(test)[1]<-"labs_qda"
colnames(test)[2]<-"Circ"
colnames(test)[3]<-"SP"

#validation setup
valid_vals<-c(cap_rand[26:length(cap_rand)], oval_rand[26:length(oval_rand)],
              rect_rand[4:6], circ_rand[25:length(circ_rand)])

valid<-as.data.frame(cbind(labs_qda[valid_vals],
                          myshape$Shape_circ[valid_vals],
                          sp[valid_vals]))
colnames(valid)[1]<-"labs_qda"
colnames(valid)[2]<-"Circ"
colnames(valid)[3]<-"SP"


svmfit=svm(as.factor(labs_qda) ~ Circ + SP,
       data=test,
       #kernel='radial',
       kernel="polynomial",
       cost=1, coef0= 1, degree=10)
       #gamma=0.1)

#testing
plot(svmfit, test, SP~ Circ)
summary(svmfit)

ypred=predict(svmfit ,test)
table(predict=ypred, truth=test$labs_qda)
mean(ypred== test$labs_qda )

#validation
plot(svmfit, valid, SP ~ Circ)

ypred=predict(svmfit ,valid)
table(predict=ypred, truth=valid$labs_qda)
mean(ypred== valid$labs_qda )


########################################################
#classifying  capsule and oval vs. rectangle
#using circularity and SP

#step 1.1
#SVM model
########################################################

#training setup
train_vals<-c(train_01, train_4, train_6)

labs_qda<-as.numeric(labs)
labs_qda[c(rect_rand)]<-1
labs_qda[-c(rect_rand)]<-0

test<-as.data.frame(cbind(labs_qda[train_vals],
                          myshape$Shape_eccent[train_vals],
                          myshape$Shape_circ[train_vals]))
colnames(test)[1]<-"labs_qda"
colnames(test)[2]<-"Eccent"
colnames(test)[3]<-"Circ"

#validation setup
valid_vals<-c(cap_rand[26:length(cap_rand)], oval_rand[26:length(oval_rand)],
              rect_rand[4:6], circ_rand[25:length(circ_rand)])

valid<-as.data.frame(cbind(labs_qda[valid_vals],
                          myshape$Shape_eccent[valid_vals],
                          myshape$Shape_circ[valid_vals]))
colnames(valid)[1]<-"labs_qda"
colnames(valid)[2]<-"Eccent"
colnames(valid)[3]<-"Circ"


svmfit=svm(as.factor(labs_qda) ~ Eccent + Circ,
       data=test,
       #kernel='radial',
       kernel="polynomial",
       cost=1, coef0= 2, degree=5)
       #gamma=0.1)

#testing
plot(svmfit, test, Eccent~ Circ)
summary(svmfit)

ypred=predict(svmfit ,test)
table(predict=ypred, truth=test$labs_qda)
mean(ypred== test$labs_qda )

#validation
plot(svmfit, valid, Eccent ~ Circ)

ypred=predict(svmfit ,valid)
table(predict=ypred, truth=valid$labs_qda)
mean(ypred== valid$labs_qda )


#step 1.1.1
#COLS model for oval and capsule

####################
#COLS
####################

#training setup
train_vals<-c(train_01, train_4)

labs_qda<-as.numeric(labs)
labs_qda[cap_rand]<-1
labs_qda[oval_rand]<-2

train<-as.data.frame(cbind(labs_qda[train_vals],
                          mydata$white[train_vals],
                          mydata$black[train_vals]))
colnames(train)[1]<-"labs_cols"
colnames(train)[2]<-"white"
colnames(train)[3]<-"black"

#validation setup
valid_vals<-c(cap_rand[26:length(cap_rand)],
              oval_rand[26:length(oval_rand)])

valid<-as.data.frame(cbind(labs_qda[valid_vals],
                          mydata$white[valid_vals],
                          mydata$black[valid_vals]))
colnames(valid)[1]<-"labs_cols"
colnames(valid)[2]<-"white"
colnames(valid)[3]<-"black"


beta0<-c(0, 0)
#oval
sps<-mydata[train_01,1]/(mydata[train_01,1]+mydata[train_01,2])
beta_oval<-(1/mean(sps))-1
#caps
sps<-mydata[train_4,1]/(mydata[train_4,1]+mydata[train_4,2])
beta_caps<-(1/mean(sps))-1

#combine into a single vector
beta1s<-c(beta_oval, beta_caps)

#predicting classes for training
cols_class<-pred.cols.class(y=train$black, x=train$white, beta0=beta0, beta1=beta1s)

table(predict=cols_class, truth=train$labs_cols)

mean(cols_class== train$labs_cols)

#predicting classes for validation
cols_class<-pred.cols.class(y=valid$black, x=valid$white, beta0=beta0, beta1=beta1s)

table(predict=cols_class, truth=valid$labs_cols)

mean(cols_class==valid$labs_cols)

#qda

#creating model via testing set
lda.fit = qda(labs_cols ~ white + black,
              data=train)

#predicting
lda.pred=predict(lda.fit, train)
lda.class = lda.pred$class

#results
table(lda.class, train$labs_cols)
#overall classification rate for training
mean(lda.class==train$labs_cols)

#validation

#predicting
lda.pred=predict(lda.fit, valid)
lda.class = lda.pred$class

#results
table(lda.class, valid$labs_cols)
#overall classification rate for training
mean(lda.class==valid$labs_cols)

#svm

svmfit=svm(as.factor(labs_cols) ~ white + black,
       data=train,
       #kernel='radial',
       kernel="polynomial",
       cost=1, coef0= 1, degree=2)
       #gamma=0.1)

#testing
plot(svmfit, train, black~white)
summary(svmfit)

ypred=predict(svmfit ,train)
table(predict=ypred, truth=train$labs_cols)
mean(ypred== train$labs_cols )

#validation
plot(svmfit, valid, black ~ white)

ypred=predict(svmfit ,valid)
table(predict=ypred, truth=valid$labs_cols)
mean(ypred== valid$labs_cols )

#able to acheive 96% on validation for svm model
#beats COLS model (which got 94%).  Probably due to the fact that
#COLS does not account for how much the observations vary.





#step 1.2
#classying Tear and Semi-Circle vs Triangle vs. else (Trap, Square, Hex, Diamond)

#using SP and Eccent

#training setup
train_vals<-c(train_10,
              train_12,
              train_8,
              train_11, train_9, train_3, train_2, train_5)

labs_qda<-as.numeric(labs)
labs_qda[c(tear_rand, sc_rand)]<-1
labs_qda[tri_rand]<-2
labs_qda[c(trap_rand, squ_rand, hex_rand, diam_rand, pent_rand)]<-3

train<-as.data.frame(cbind(labs_qda[train_vals],
                          mydata$white[train_vals]/(mydata$white[train_vals]+mydata$black[train_vals]),
                          myshape$Shape_eccent[train_vals]))
colnames(train)[1]<-"labs_svm"
colnames(train)[2]<-"SP"
colnames(train)[3]<-"Eccent"

#validation setup
valid_vals<-c(tear_rand[6:10],
              tri_rand[7:length(tri_rand)],
              sc_rand[3:4],
              trap_rand[3:4],
              squ_rand[5:8],
              hex_rand[5:8],
              diam_rand[7:length(diam_rand)],
              pent_rand[7:12]
            )

valid<-as.data.frame(cbind(labs_qda[valid_vals],
                          mydata$white[valid_vals]/(mydata$white[valid_vals]+mydata$black[valid_vals]),
                          myshape$Shape_eccent[valid_vals]))
colnames(valid)[1]<-"labs_svm"
colnames(valid)[2]<-"SP"
colnames(valid)[3]<-"Eccent"


svmfit=svm(as.factor(labs_svm) ~ SP + Eccent,
       data=train,
       #kernel='radial',
       kernel="polynomial",
       cost=1, coef0= 1, degree=3)
       #gamma=0.1)

#testing
plot(svmfit, train, Eccent~ SP)
summary(svmfit)

ypred=predict(svmfit ,train)
table(predict=ypred, truth=train$labs_svm)
mean(ypred== train$labs_svm )

#validation
plot(svmfit, valid, Eccent~ SP)

ypred=predict(svmfit ,valid)
table(predict=ypred, truth=valid$labs_svm)
mean(ypred== valid$labs_svm )




#classifying Tear vs. Semi-Circle
#using White box and Black box

#SVM algorithm


#training setup
train_vals<-c(train_10,
              train_8)

labs_qda<-as.numeric(labs)
labs_qda[tear_rand]<-1
labs_qda[sc_rand]<-2

train<-as.data.frame(cbind(labs_qda[train_vals],
                          myshape$White_box[train_vals],
                          myshape$Black_box[train_vals]))
colnames(train)[1]<-"labs_svm"
colnames(train)[2]<-"White_box"
colnames(train)[3]<-"Black_box"

#validation setup
valid_vals<-c(tear_rand[6:10],
              sc_rand[3:4]
            )

valid<-as.data.frame(cbind(labs_qda[valid_vals],
                          myshape$White_box[valid_vals],
                          myshape$Black_box[valid_vals]))
colnames(valid)[1]<-"labs_svm"
colnames(valid)[2]<-"White_box"
colnames(valid)[3]<-"Black_box"


svmfit=svm(as.factor(labs_svm) ~ White_box + Black_box,
       data=train,
       #kernel='radial',
       kernel="polynomial",
       cost=1, coef0= 2, degree=10)
       #gamma=0.1)

#testing
plot(svmfit, train, Black_box~ White_box)
summary(svmfit)

ypred=predict(svmfit ,train)
table(predict=ypred, truth=train$labs_svm)
mean(ypred== train$labs_svm )

#validation
plot(svmfit, valid, Black_box~ White_box)

ypred=predict(svmfit ,valid)
table(predict=ypred, truth=valid$labs_svm)
mean(ypred== valid$labs_svm )


#classifying Trapezoid and Diamond vs. Square and Pentagon and Hexagon
#using SP values and Eccentricity


#training setup
train_vals<-c(train_11, train_9, train_3, train_2, train_5)

labs_qda<-as.numeric(labs)
labs_qda[c(trap_rand, diam_rand)]<-1
labs_qda[c(squ_rand, hex_rand, pent_rand)]<-2

train<-as.data.frame(cbind(labs_qda[train_vals],
                          mydata$white[train_vals]/(mydata$white[train_vals]+mydata$black[train_vals]),
                          myshape$Shape_eccent[train_vals]))
colnames(train)[1]<-"labs_svm"
colnames(train)[2]<-"SP"
colnames(train)[3]<-"Eccent"

#validation setup
valid_vals<-c(trap_rand[3:4],
              squ_rand[5:8],
              hex_rand[5:8],
              diam_rand[7:length(diam_rand)],
              pent_rand[7:12]
            )

valid<-as.data.frame(cbind(labs_qda[valid_vals],
                          mydata$white[valid_vals]/(mydata$white[valid_vals]+mydata$black[valid_vals]),
                          myshape$Shape_eccent[valid_vals]))
colnames(valid)[1]<-"labs_svm"
colnames(valid)[2]<-"SP"
colnames(valid)[3]<-"Eccent"


svmfit=svm(as.factor(labs_svm) ~ Eccent + SP,
       data=train,
       #kernel='radial',
       kernel="polynomial",
       cost=1, coef0= 1, degree=2)
       #gamma=0.1)

#testing
plot(svmfit, train, Eccent~ SP)
summary(svmfit)

ypred=predict(svmfit ,train)
table(predict=ypred, truth=train$labs_svm)
mean(ypred== train$labs_svm )

#validation
plot(svmfit, valid, Eccent~ SP)

ypred=predict(svmfit ,valid)
table(predict=ypred, truth=valid$labs_svm)
mean(ypred== valid$labs_svm )



#classifying Trapezoid vs. Diamond
#using Black_box and White_box
# COLS or SVM


#training setup
train_vals<-c(train_11, train_2)

labs_qda<-as.numeric(labs)
labs_qda[trap_rand]<-1
labs_qda[diam_rand]<-2

train<-as.data.frame(cbind(labs_qda[train_vals],
                          myshape$Black_box[train_vals],
                          myshape$White_box[train_vals]))
colnames(train)[1]<-"labs_svm"
colnames(train)[2]<-"Black_box"
colnames(train)[3]<-"White_box"

#validation setup
valid_vals<-c(trap_rand[3:4],
              diam_rand[7:length(diam_rand)]
            )

valid<-as.data.frame(cbind(labs_qda[valid_vals],
                          myshape$Black_box[valid_vals],
                          myshape$White_box[valid_vals]))
colnames(valid)[1]<-"labs_svm"
colnames(valid)[2]<-"Black_box"
colnames(valid)[3]<-"White_box"


svmfit=svm(as.factor(labs_svm) ~ Black_box + White_box,
       data=train,
       #kernel='radial',
       kernel="polynomial",
       cost=1, coef0= 1, degree=1)
       #gamma=0.1)

#testing
plot(svmfit, train, Black_box~ White_box)
summary(svmfit)

ypred=predict(svmfit ,train)
table(predict=ypred, truth=train$labs_svm)
mean(ypred== train$labs_svm )

#validation
plot(svmfit, valid, Black_box~ White_box)

ypred=predict(svmfit ,valid)
table(predict=ypred, truth=valid$labs_svm)
mean(ypred== valid$labs_svm )



beta0<-c(0, 0)
#diamond
sps<-myshape[c(train_2),6]/(myshape[c(train_2),6]+myshape[c(train_2),7])
beta_diam<-(1/mean(sps))-1
#trapezoid
sps<-myshape[train_11,6]/(myshape[train_11,6]+myshape[train_11,7])
beta_trap<-(1/mean(sps))-1

#combine into a single vector
beta1s<-c(beta_trap, beta_diam)

#predicting classes for training
cols_class<-pred.cols.class(y=train$Black_box, x=train$White_box, beta0=beta0, beta1=beta1s)

table(predict=cols_class, truth=train$labs_svm)

mean(cols_class== train$labs_svm)

#predicting classes for validation
cols_class<-pred.cols.class(y=valid$Black_box, x=valid$White_box, beta0=beta0, beta1=beta1s)

table(predict=cols_class, truth=valid$labs_svm)

mean(cols_class==valid$labs_svm)


#classifying  Square vs. Pentagon vs. Hexagon
#using Black_box and White_box
#svm polynomial model



#training setup
train_vals<-c(train_9, train_3, train_5)

labs_qda<-as.numeric(labs)
labs_qda[c(squ_rand)]<-1
labs_qda[c(hex_rand)]<-2
labs_qda[c(pent_rand)]<-3

train<-as.data.frame(cbind(labs_qda[train_vals],
                          myshape$White_box[train_vals],
                          myshape$Black_box[train_vals]))
colnames(train)[1]<-"labs_svm"
colnames(train)[2]<-"White_box"
colnames(train)[3]<-"Black_box"

#validation setup
valid_vals<-c(squ_rand[5:8],
              hex_rand[5:8],
              pent_rand[7:12]
            )

valid<-as.data.frame(cbind(labs_qda[valid_vals],
                          myshape$White_box[valid_vals],
                          myshape$Black_box[valid_vals]))
colnames(valid)[1]<-"labs_svm"
colnames(valid)[2]<-"White_box"
colnames(valid)[3]<-"Black_box"


svmfit=svm(as.factor(labs_svm) ~ White_box + Black_box,
       data=train,
       #kernel='radial',
       kernel="polynomial",
       cost=1, coef0= 50, degree=2)
       #gamma=0.1)

#testing
plot(svmfit, train, Black_box~ White_box)
summary(svmfit)

ypred=predict(svmfit ,train)
table(predict=ypred, truth=train$labs_svm)
mean(ypred== train$labs_svm )

#validation
plot(svmfit, valid, Black_box~ White_box)

ypred=predict(svmfit ,valid)
table(predict=ypred, truth=valid$labs_svm)
mean(ypred== valid$labs_svm )
