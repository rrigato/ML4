
install.packages("AUC")
library(AUC)
library(randomForest)
setwd("C:\\Users\\Randy\\Downloads\\Kaggle Santander")

train = read.csv("train.csv")
test = read.csv("test.csv")




sum(train$TARGET)/nrow(train)

plot(train$ID,train$TARGET,  type = 'p')



#calls the split function to divide the train dataset
bothFrames = split(train, .85)
train2 = bothFrames[[1]]
test2 = bothFrames[[2]]



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



train2 = train2[,-c(no_var)]
test2 = test2[,-c(no_var)]
test = test[,-c(no_var)]




##################################################################
#glmnet package attempt
#
#
#
#
#
##################################################################
library(glmnet)


glmCV = cv.glmnet(data.matrix(train2[,(2:ncol(train2)-1)]),
					 train2$TARGET, alpha = 0,
					 type.measure='auc', family = "binomial")


glmOut = predict(glmCV,data.matrix(test2[,(2:ncol(test2)-1)]))

glmOut = as.data.frame(glmOut)

glmMod = glmnet(train2[,(2:ncol(train2)-1)], train2$TARGET,
				family = c("binomial"),standardize = TRUE,
				












################################################################################
#Random Forest attempt
#
#
#
#
###############################################################################
library(plyr)


#remember to do a keep = keep + 1 before running a random forest model 
#with optimized feature selection
#The reason for this is because the train2Matrix doesn't have ids 
#so this creates an off by one error for regular train dataset

#keep = keep +1

ranOut = randomForest( y = as.factor(train2[,ncol(train2)]), 
		x = train2[,keep], ntree = 100 )
ranImportance = as.data.frame(importance(ranOut))
ranImportance[,2] = rownames(ranImportance)
ranImportance = rename(ranImportance, c('V2' = 'VarName'))



ranTop = arrange(ranImportance, MeanDecreaseGini)

ranTop = ranTop[1:250,]
head(ranTop)
plot(ranOut)
keepTop = which(colnames(train2Matrix) %in% ranTop[,2])




ranPred = predict(ranOut, newdata = test2, type = 'prob')

str(ranPred)
ranPred = as.data.frame(ranPred)


outputFrame4 = data.frame(matrix(nrow= nrow(test2), ncol=3))
outputFrame4 = rename(outputFrame4, c("X1" = "id", 
	"X2" = "rounded", "X3" = "actual")) 
outputFrame4[,1] = test2[,1]
outputFrame4[,2] = ranPred[,2]
outputFrame4[,3] = test2[,ncol(test2)]


	opCurve = roc(outputFrame4[,2],as.factor(outputFrame4[,3]))
	auc(opCurve)
	plot(opCurve)


extraTreesParse <- function(train5, test5){

	library(extraTrees)
	library(plyr)
	#x is the variables used to predict the outcome variable y,
	#which has to be a factor for classification
	x = train5[,2:(100)]
	y = as.factor(train5[,ncol(train5)])


	 options( java.parameters = "-Xmx6g" )
	set.seed(27)
	#calls the extraTrees function 
	eT = extraTrees(x,y, mtry = 15, nodesize = 5, numRandomCuts = 5,
		na.action ="zero")

	#returns the probabilities and makes it into a dataframe
	etOut = predict(eT, newdata = test5[,2:(100)])
	etOut = as.data.frame(etOut)


	#initialize output frame
	etFrame = data.frame(matrix(nrow= nrow(test2), ncol=4))
	etFrame = rename(etFrame, c("X1" = "id", "X2" = "Round",
		 "X3" = "actual")) 

	#Puts the ids for the observations into the first column of outputFrame[,1]
	etFrame[,1] = test5$ID

	#puts the predictions for y being 1 into etFrame2
	#and the actual in the 2nd column
	etFrame[,2] = etOut
	etFrame[,3] = test5[,ncol(test5)]

	

	#calls the log loss function to get the actual log_loss from the witheld training data
	roc(etFrame[,2], etFrame[,3])

	return(etFrame);
}















explan = 2:(ncol(train2) -1)

dlFrame = deepL(train2, test2, explan)


#initalize all predictions to zero
dlFrame[,4] = numeric(nrow(dlFrame))

#if the probability is over a specified level, that
#observation is predicted  a one
for (i in 1:nrow(dlFrame))
	{
		if (dlFrame[i,2] >= .5)
		{
			dlFrame[i,4] = 1
		}
	

	}

#proportion of ones
sum(dlFrame[,4])/nrow(dlFrame)

#Reciever operating curve for predictions and actual
roc(dlFrame[,4],dlFrame[,3])











train5 = train2
test5 =test2
explanFeatures = 2:(ncol(train2) - 1)
######################################################################
#Deep Learning in H2o log_loss:.4868497
#
#This function takes three arguements, the train data, the test data,
# and the features to be used for the deep learning model
#
#The y variable is assumed to be in the second column of train
#
#depends: library(h2o) library(plyr) and log_loss
#
#####################################################################

deepL <- function(train5, test5, explanFeatures) 
{
	library(h2o)
	library(plyr)
	
	#initializes a thread by connecting to h2os clusters
	h2o.init(nthreads = -1)


	#turns the numeric outcome variable to a factor
	train5[,2] = as.factor(train5[,ncol(train5)])
	test5[,2] = as.factor(test5[,ncol(train5)])

	#converts the two dataframes into h2o frames
	train5 = as.h2o(train5)
	test5 = as.h2o(test5)

	#builds the deep learning neural nets using only the features in explanFeatures
	#2 is the outcome feature
	trainDL = h2o.deeplearning(x = explanFeatures, y = 2 ,
	hidden = c(5), rho = .99, epochs = 75,
	 training_frame = train5)

	#makes probability predictions on the test5 data using the model built
	predictions <- h2o.predict(trainDL, newdata = test5, type = "probs")

	#turns h2o output into dataframe
	DLPred = as.data.frame(predictions[,3])


	#initializes outputFrame
	outputFrame = data.frame(matrix(nrow= nrow(test5), ncol=3))
	outputFrame = rename(outputFrame, c("X1" = "ID", "X2" = "PredictedProb", "X3" = "actual"))
	
	#adds ids back into outputFrame
	outputFrame[,1] = test5[,1]

	#adds the predicted values from the model
	outputFrame[,2] = DLPred
	
	#adds the actual output to the output frame
	outputFrame[,3] = test5$TARGET

	#put roc here
	
	opCurve = roc(outputFrame[,2],as.factor(outputFrame[,3]))
	auc(opCurve)
	plot(opCurve)


	#returns the probabilities in a data frame
	return(outputFrame);

}















################################################################
#
#
#xgboost
##################################################################


library(Ckmeans.1d.dp)
library(xgboost)
library(methods)
library(data.table)
library(magrittr)
library(Matrix)
library(plyr)




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


#trainM <- sparse.model.matrix(TARGET ~ ., data = train2)

#dtrain <- xgb.DMatrix(data=trainM, label=train2_response)
#watchlist <- list(train2=dtrain)




#AUC is equal to the ROC curve
#cross_validation parameters
numberOfClasses = 2
param = list( "objective" = "binary:logistic",
		"booster" = "gbtree",
		"eval_metric" = "auc",
			"eta" = .1,
		 "subsample" = .75,
		"gamma" = .1
		)
cv.nround <- 250
cv.nfold <- 3

#setting up cross_validation
bst.cv = xgb.cv(param=param, data = train2Matrix, label = train2_response, 
                nfold = cv.nfold, nrounds = cv.nround, missing = 'NAN')

#test for optimal nround
bst.cv[which(max(bst.cv$test.auc.mean) == bst.cv$test.auc.mean),]

#sets the number of rounds based on the number of rounds determined by cross_validation
nround = which(max(bst.cv$test.auc.mean) == bst.cv$test.auc.mean)
#actual xgboost
bst = xgboost(param=param, data =  train2Matrix, label = train2_response,	 
	 nrounds=nround, max_delta_step = 5, missing = 'NAN')







# Get the feature real names
names <- dimnames(train2Matrix)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst); importance_matrix

# Nice graph for importance
#xgb.plot.importance(importance_matrix[1:100,])



#the predictions are in a nrow(test3)*3 long vector
#bstPred[1:3] is the probability of 0,1,2 for fault_severity
#for the first observation of test2
#has to be a numeric matrix just like the training set
bstPred = predict(bst, test3Matrix)
is.vector(bstPred)
str(bstPred)




nv = tc['num_var33']+tc['saldo_medio_var33_ult3']+tc['saldo_medio_var44_hace2']+tc['saldo_medio_var44_hace3']+
tc['saldo_medio_var33_ult1']+tc['saldo_medio_var44_ult1']

preds[nv > 0] = 0
preds[tc['var15'] < 23] = 0
preds[tc['saldo_medio_var5_hace2'] > 160000] = 0
preds[tc['saldo_var33'] > 0] = 0
preds[tc['var38'] > 3988596] = 0
preds[tc['var21'] > 7500] = 0
preds[tc['num_var30'] > 9] = 0
preds[tc['num_var13_0'] > 6] = 0
preds[tc['num_var33_0'] > 0] = 0
preds[tc['imp_ent_var16_ult1'] > 51003] = 0
preds[tc['imp_op_var39_comer_ult3'] > 13184] = 0
preds[tc['saldo_medio_var5_ult3'] > 108251] = 0
preds[tc['num_var37_0'] > 45] = 0
preds[tc['saldo_var5'] > 137615] = 0
preds[tc['saldo_var8'] > 60099] = 0
preds[(tc['var15']+tc['num_var45_hace3']+tc['num_var45_ult3']+tc['var36']) <= 24] = 0





#initialize output frame
xgFrame = data.frame(matrix(nrow= nrow(test2), ncol=3))
xgFrame = rename(xgFrame, c("X1" = "id", "X2" = "predictedProb",
			 "X3" = "Actual")) 

#Puts the ids for the observations into the first column of xgFrame[,1]
xgFrame[,1] = test3id
xgFrame[,3] = test3_response
#test to make sure ids are the same
sum(xgFrame[,1] != test3id)


#probability of y = 1
xgFrame[,2] = bstPred


xgFrame2 = xgFrame




opCurve = roc(xgFrame[,2],as.factor(xgFrame[,3]))
auc(opCurve)
plot(opCurve)



#example program
#auc(roc(churn$predictions,churn$labels))
#plot(roc(churn$predictions,churn$labels))

sum(xgFrame[,2])/nrow(xgFrame)


importance_matrix = as.data.frame(importance_matrix)





######################################################################################
#
#
#feature_selection
####################################################################################


importance_matrix = as.data.frame(importance_matrix)


#gets the names of the variables that matter
top124 = importance_matrix[,1]


#gets all the names in train2Matrix
train2Col = colnames(train2Matrix)

#gets the column numbers of the variables you should keep
keep = which(train2Col %in%  top124)











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















