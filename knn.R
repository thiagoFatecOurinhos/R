# Seleciona o dir. de trabalho
setwd("G:\\Meu Drive\\Fatec\\FATEC - Aulas\\2020-2\\PISRC\\Datasets\\Matutino")

# Importa o dataset
dataset <- read.csv("amostra_5perc.csv", 
                    header = TRUE, stringsAsFactors = TRUE)

#install.packages("caret")
#install.packages("e1071")

library(caret)
library(e1071)

index <- sort(sample(nrow(dataset), nrow(dataset)*0.75))

training <- dataset[index,]
testing  <- dataset[-index,]

training[["Label"]] <- factor(training[["Label"]])
testing[["Label"]]  <- factor(testing[["Label"]])

controleTreinamento <- trainControl(method = "repeatedcv",
                                    number = 10, 
                                    repeats = 3)

ids <- train(Label ~.,
             data = training,
             method = "knn",
             trControl = controleTreinamento,
             tuneLength = 10)


deteccao <- predict(ids, newdata = testing)
confusionMatrix(deteccao, testing$Label)

# 1%   - 99.06%
# 5%   - 99.91%
# 10%  - ?
# 25%  - ?
# 50%  - ?
# 100% - ?

