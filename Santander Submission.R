#Script data
test$TARGET <- -1
test3id = test$ID
test <- sparse.model.matrix(TARGET ~ ., data = test)

preds <- predict(bst, test)
pred <-predict(bst,train2)
AUC<-function(actual,predicted)
{
  library(pROC)
  auc<-auc(as.numeric(actual),as.numeric(predicted))
  auc 
}
AUC(train2_response,pred) ##AUC


# Under 23 year olds are always happy
preds[var15 < 23] = 0
preds[saldo_medio_var5_hace2 > 160000]=0
preds[saldo_var33 > 0]=0
preds[var38 > 3988596]=0
preds[NV>0]=0
preds[V21>7500]=0











#initialize output frame
finalFrame = data.frame(ID = test3id, TARGET=preds)

#test to make sure ids are the same
sum(finalFrame[,1] != test3id)



#validation
nrow(finalFrame) == length(unique(test3id))
sum(finalFrame$id != unique(test3id))
sum(is.na(finalFrame))


sum(finalFrame[,2])


#for train this is about 3.956853%
sum(finalFrame[,2])/nrow(test)


write.csv(finalFrame, "C:\\Users\\Randy\\Downloads\\Kaggle Santander\\Results6.csv",
		row.names = FALSE)












####################
#Script 2
#
#
###################


test3id = test[,1]
test3 = test[,-c(1)]

#checks that the number of ids in the vector is equal to the number of rows in 
#the data frames

length(test3id) == nrow(test3)
 

test3Matrix = data.matrix(test3)


#testKeep = which(colnames(test3Matrix) %in% colnames(train2Matrix))
#test3Matrix = test3Matrix[,testKeep]





#has to be a numeric matrix just like the training set
bstPred = predict(bst, test3Matrix)
is.vector(bstPred)
str(bstPred)





nv = test['num_var33']+test['saldo_medio_var33_ult3']+test['saldo_medio_var44_hace2']+test['saldo_medio_var44_hace3']+
test['saldo_medio_var33_ult1']+test['saldo_medio_var44_ult1']

bstPred[nv > 0] = 0
bstPred[test['var15'] < 23] = 0
bstPred[test['saldo_medio_var5_hace2'] > 160000] = 0
bstPred[test['saldo_var33'] > 0] = 0
bstPred[test['var38'] > 3988596] = 0
bstPred[test['var21'] > 7500] = 0
bstPred[test['num_var30'] > 9] = 0
bstPred[test['num_var13_0'] > 6] = 0
bstPred[test['num_var33_0'] > 0] = 0
bstPred[test['imp_ent_var16_ult1'] > 51003] = 0
bstPred[test['imp_op_var39_comer_ult3'] > 13184] = 0
bstPred[test['saldo_medio_var5_ult3'] > 108251] = 0
bstPred[test['num_var37_0'] > 45] = 0
bstPred[test['saldo_var5'] > 137615] = 0
bstPred[test['saldo_var8'] > 60099] = 0
bstPred[(test['var15']+test['num_var45_hace3']+test['num_var45_ult3']+test['var36']) <= 24] = 0










#initialize output frame
finalFrame = data.frame(matrix(nrow= nrow(test), ncol=2))
finalFrame = rename(finalFrame, c("X1" = "ID", "X2" = "TARGET")) 

#Puts the ids for the observations into the first column of finalFrame[,1]
finalFrame[,1] = test3id

#test to make sure ids are the same
sum(finalFrame[,1] != test3id)


#probability of y = 1
finalFrame[,2] = bstPred



#adjustment for scale
#finalFrame[,2] = finalFrame[,2] * 0.85581612138895227301483621121297

#validation
nrow(finalFrame) == length(unique(test3id))
sum(finalFrame$id != unique(test3id))
sum(is.na(finalFrame))


sum(finalFrame[,2])


#for train this is about 3.956853%
sum(finalFrame[,2])/nrow(test)


write.csv(finalFrame, "C:\\Users\\Randy\\Downloads\\Kaggle Santander\\Results13.csv",
		row.names = FALSE)



















































