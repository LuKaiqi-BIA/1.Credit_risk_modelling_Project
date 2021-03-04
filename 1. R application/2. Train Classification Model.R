library(dplyr)

library(rpart)
library(rpart.plot)
library(caret)
library(e1071)

library(forecast)

# Removing ttl_pym for training model
class.train = train.mlm %>% select(-ttl_pym)
class.test = test.mlm  %>% select(-ttl_pym)


a = nrow(class.train[class.train$loan_stat!= "Default",])
b = nrow(class.train[class.train$loan_stat== "Default",])

modelWeights <- ifelse(class.train$loan_stat== "Default",a/b, 1)


# Classification tree
rpart.tree <- rpart(loan_stat~.,
                    data = class.train,
                    control = rpart.control(cp = 0.0005),
                    method = "class",
                    weights = modelWeights
)
preds = predict(rpart.tree, class.test, type = "class")
prp(rpart.tree,
    type = 3,
    extra = 1,
    under = TRUE,
    split.font = 1,
    varlen = -10,
    cex= 0.6
)


rpart.rules(rpart.tree)

# Generate Confusion Matrix
confusionMatrix(table(preds, class.test$loan_stat), positive = "Default")
