
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

temp2 = gbmParse(train2, test2)












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
	bTree = gbm(TARGET~. , distribution = "bernoulli", n.trees = 30, shrinkage = .1,
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

	if (outputFrame[,2] >=.5)
	{
		outputFrame[,4] = 1
	}
	
	if(outputFrame[,2] < .5 )
	{
		outputFrame[,4] = 0
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















