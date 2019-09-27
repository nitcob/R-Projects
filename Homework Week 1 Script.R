#Question 2
# The	files credit_card_data.txt (without	headers)	and	credit_card_data-headers.txt	(with	headers)	
# contain	a	dataset	with	654	data	points,	6	continuous	and	4	binary	predictor	variables.		It	has	
# anonymized	credit	card	applications	with	a	binary	response	variable	(last	column)	indicating	if	the	
# application	was	positive	or	negative.	The	dataset	is	the	?Credit	Approval	Data	Set?	from	the	UCI	Machine	
# Learning	Repository	(https://archive.ics.uci.edu/ml/datasets/Credit+Approval)	without	the	categorial	
# variables and	without	data	points	that	have	missing	values.


# Import data from website
data<- read.table("https://d37djvu3ytnwxt.cloudfront.net/assets/courseware/v1/e39a3df780dacd5503df6a8322d72cd2/asset-v1:GTx+ISYE6501x+2T2017+type@asset+block/credit_card_data-headers.txt", TRUE)

#First some data exploration of the variables
# Plot variables to determine best sets for the model

plot (data)

# Too many variables it is still not clear the relationship.
# Use scatterplot Matrices from the glus Package. This will sort the plots and colour code the plots in relation to the response attribute. 

library(gclus)
data.r <- abs(cor(data)) # get correlations
data.col <- dmat.color(data.r) # get colors

# reorder variables so those with highest correlation
# are closest to the diagonal
data.o <- order.single(data.r) 
cpairs(data, data.o, panel.colors=data.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )

#Now to the the questions. 

# Question 2.1
# Using	the	support	vector	machine	function ksvm contained	in	the	R	package	kernlab,	find	a	
# good	classifier	for	this	data. Show	the	equation	of	your	classifier,	and	how	well	it	classifies	the	
# data	points	in	the	full	data	set.		(Don?t	worry	about	test/validation	data	yet;	we?ll	cover	that topic	soon.)

#Set ksvm model1
library(kernlab)
model1 <- ksvm(as.matrix(data[ ,1:10]), as.factor(data[ ,11]), type = "C-svc", kernel="vanilladot", C=50, scaled = TRUE)
# calculate a1?am
a <- colSums(data[model1@SVindex,1:10] * model1@coef[[1]])
a
# calculate a0
a0 <- sum(a*data[1,1:10])- model1@b
a0
# see what the model1 predicts
pred <- predict(model1,data[,1:10])

#pred
# see what fraction of the model predictions match the actual classification
sum(pred == data[,11]) / nrow(data)

#General Summary
model1

#Explore attributes:
#attributes(model1)

# For example, the support vectors
alpha(model1)
alphaindex(model1)
b(model1)

# Answer Q 2.1
# The best classifier I could find is C = 50 it has 86.4% percent accuracy.


#Question 2.2
# You are welcome, but not required, to try other (nonlinear) kernels as well; 
# we’re not covering them in this course, but they can sometimes be useful 
# and might provide better predictions than vanilladot.

model2 <- ksvm(as.matrix(data[ ,1:10]), as.factor(data[ ,11]), type = "C-svc", kernel="rbfdot", C=50, scaled = TRUE)
# calculate a1?am
a <- colSums(data[model2@SVindex,1:10] * model2@coef[[1]])
a
# calculate a0
a0 <- sum(a*data[1,1:10])- model2@b
a0
# see what the model2 predicts
pred <- predict(model2,data[,1:10])

#pred
# see what fraction of the model predictions match the actual classification
sum(pred == data[,11]) / nrow(data)

#General Summary
model2

#Explore attributes:
#attributes(model2)

# For example, the support vectors
alpha(model2)
alphaindex(model2)
b(model2)

# The best classifier with a Gaussian Kernel is able to predict values with a cost of is C = 50 at 93.6% percent accuracy.

#Question 2.3 
# Using the k-nearest-neighbors classification function kknn contained in the R kknn package, 
# suggest a good value of k, and show how well it classifies that data points in the full data set.
# Don’t forget to scale the data (scale=TRUE in kknn).

library("kknn")

#Import data without headers. (headers seem to be a problem when running the KKNN model)

credit<- read.table("https://d37djvu3ytnwxt.cloudfront.net/assets/courseware/v1/39b78ff5c5c28981f009b54831d81649/asset-v1:GTx+ISYE6501x+2T2017+type@asset+block/credit_card_data.txt")

# Mix the rows of the dataframe in order for the model to be able to predict the R1/V11
#set.seed to be able to replicate results according to some of the comments of the students.

set.seed(9850)

#runif() produces a random number for a uniform distribution. 

gp<- runif(nrow(credit))

credit<- credit[order(gp),]

#Training will have 90% of the data 588 aplicants and 67 for de validation set

credit.learn<- credit[1:588, ]
credit.valid<- credit[588:654, ]

for(i in credit.learn)
  response.learn = credit.learn[-i,11]
attrs.learn = credit.learn[-i,1:10]

for(i in credit.valid)
  response.valid = credit.valid[-i,11]
attrs.valid = credit.valid[-i,1:10]

for(i in credit)
  response = credit[-i,11]
attrs = credit[-i,1:10]

model2.tknn <- train.kknn(response.learn~., data = attrs.learn, ks = 10, scale = TRUE)  

model2.tknn.q2 <- train.kknn(response~., data = attrs, ks = 10, scale = TRUE)  

pred2 <- round(predict(model2.tknn, attrs.valid))

pred2.q2 <- round(predict(model2.tknn.q2, attrs))

#Compare the prediction with the value:

testresults <- sum(pred2 == response.valid)
resultprct <- testresults/ length(pred2)

testresults.q2 <- sum(pred2.q2 == response.learn)
resultprct.q2 <- testresults.q2/ length(pred2.q2)

resultprct
resultprct.q2
# Answer Q3:
# The best classifier I could find is K = 10 it has 88.2% percent accuracy. I choose this classifier
# because it was a recomendation made on Jalayer Academy to pick the sqare root of the total datapoints. 


