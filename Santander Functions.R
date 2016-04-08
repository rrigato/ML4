
install.packages("pROC")
library(pROC)
setwd("C:\\Users\\Randy\\Downloads\\Kaggle Santander")

train = read.csv("train.csv")
test = read.csv("test.csv")




sum(train$TARGET)/nrow(train)

plot(train$ID,train$TARGET,  type = 'p')



#calls the split function to divide the train dataset
bothFrames = split(train, .8)
train2 = bothFrames[[1]]
test2 = bothFrames[[2]]

predict = numeric(nrow(train2))
outcome = train2$TARGET

#Gets the area under the ROC curve
temp =roc(outcome, predict)

temp2 = gbmParse(train2[,-c(no_var)], test2[,-c(no_var)])



n_counter = 1
no_var = numeric()
for (i in 2:(ncol(train)-1))
{
	if (sd(train[,i]) <.01)
	{
		no_var[n_counter] = i
		n_counter = n_counter +1
	}
	
}


#xgboost
library(Ckmeans.1d.dp)
library(xgboost)
library(methods)
library(data.table)
library(magrittr)
library(Matrix)





#stores the ids in a vector and removes id from data frames
train2id = train2[,1]
train2 = train2[,-c(1)]

test3id = test2[,1]
test3 = test2[,-c(1)]

#checks that the number of ids in the vector is equal to the number of rows in 
#the data frames
length(train2id) == nrow(train2)
length(test3id) == nrow(test3)






#saves the outcome variable into a seperate vector
train2_response = train2$TARGET
test3_response = test3$TARGET

#removes outcome vector from the data_frame
test3 = test3[,-c(ncol(test3))]
train2 = train2[,-c(ncol(train2))]



length(train2_response) == nrow(train2)
length(test3_response) == nrow(test3)


train2Matrix = data.matrix(train2)


test3Matrix = data.matrix(test3)





#used to keep only those variables in the importance matrix
#train2Matrix = train2Matrix[,keep]
#test3Matrix = test3Matrix[,keep]






#AUC is equal to the ROC curve
#cross_validation parameters
numberOfClasses = 2
param = list( "objective" = "binary:logistic",
		"eval_metric" = "auc"
		)
cv.nround <- 250
cv.nfold <- 3

#setting up cross_validation
bst.cv = xgb.cv(param=param, data = train2Matrix, label = train2_response, 
                nfold = cv.nfold, nrounds = cv.nround)

#test for optimal nround
bst.cv[which(min(bst.cv$test.logloss.mean) == bst.cv$test.logloss.mean),]

#sets the number of rounds based on the number of rounds determined by cross_validation
nround = which(min(bst.cv$test.auc.mean) == bst.cv$test.auc.mean)
#actual xgboost
bst = xgboost(param=param, data = train2Matrix, label = train2_response,
		gamma = .1, eta = .1, nrounds=nround,
		subsample = .75)







# Get the feature real names
names <- dimnames(train2Matrix)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst); importance_matrix

# Nice graph for importance
xgb.plot.importance(importance_matrix[1:100,])



#the predictions are in a nrow(test3)*3 long vector
#bstPred[1:3] is the probability of 0,1,2 for fault_severity
#for the first observation of test2
#has to be a numeric matrix just like the training set
bstPred = predict(bst, test3Matrix)
is.vector(bstPred)
str(bstPred)


#initialize output frame
xgFrame = data.frame(matrix(nrow= nrow(test2), ncol=3))
xgFrame = rename(xgFrame, c("X1" = "id", "X2" = "predictedProb", "X3" = "Actual")) 

#Puts the ids for the observations into the first column of xgFrame[,1]
xgFrame[,1] = test3id
xgFrame[,3] = test3_response
#test to make sure ids are the same
sum(xgFrame[,1] != test3id)


#probability of y = 1
xgFrame[,2] = bstPred


xgFrame2 = xgFrame



log_loss(xgFrame)






######################################################################################
##gbmParse 
#takes two dataframes with a train dataset and a test dataset. Builds a gbm model on the train
#dataset and then gets predictions for the test dataset
#
#returns an output data frame with  3 variables: the ids of the test set, the predicted probability of
#1 for the test set and the actual for the test set
#
#
#Depends: library(gbm) log_loss
#
#
#
#
#GBM target~. -ID -v22, distribution = "adaboost", n.trees = 20, shrinkage = .1,
#		interaction.depth =2, log_loss= .4907367
#
#200 trees with adaboost = .4732754
#
#20 trees normalized numeric variables = .4922601
#####################################################################################
gbmParse <- function (train2, test2) {

	library(gbm)
	library(plyr)
	
	#runs gbm takes out id and v22 because v22 has too many levels for a category
	bTree = gbm(TARGET~. , distribution = "bernoulli", n.trees = 300, shrinkage = .1,
			interaction.depth =2,  data = train2)

	#runs the gbm model on the test set giving back an output of probabilities
	bTreeP = predict(bTree, newdata=test2, n.trees = 5000, type="response")
	bTreeP = as.data.frame(bTreeP)
	head(bTreeP)



	#initializes and fills the outputFrame that will be tested in the log_loss function 
	#and then returned
	outputFrame = data.frame(matrix(nrow= nrow(test2), ncol=4))
	outputFrame = rename(outputFrame, c("X1" = "ID", "X2" = "PredictedProb",
		 "X3" = "actual", "X4" = "rounded"))
	outputFrame[,1] = test2[,1]
	outputFrame[,2] = bTreeP[,1]
	outputFrame[,3] = test2$TARGET
	outputFrame[,4] = 0

	for (i in 1:nrow(outputFrame))
	{
		if (outputFrame[i,2] >= .03)
		{
			outputFrame[i,4] = 1
		}
	

	}
	#runs log_loss on train set
	print(roc(outputFrame[,3], outputFrame[,4]))
	
	#returns the outputed frame
	return(outputFrame);
}








################################################################
#	Splitting the train dataset into train2 and test2
#
#
#
#################################################################



split <- function(train, percentage)
{
	#edit The percentage of the dataset in the train2 and test2, used to build a model 
	size_of_train = floor(percentage*nrow(train))
	ran_num_test = 1:nrow(train)

	#gets random numbers for train2 using a sample
	ran_num_train = sample(1:nrow(train), size_of_train)

	#numbers not randomly selected for train2 are included in test2
	#this command gets the numbers not in ran_num_train
	ran_num_test = ran_num_test[(!(ran_num_test %in% ran_num_train)) == TRUE]
	train2 = train[ran_num_train,]
	test2 = train[ran_num_test,]

	#makes a list of the two frames
	bothFrames = list(train2,test2)
	
	#returns a list of both frames
	return(bothFrames)
}















