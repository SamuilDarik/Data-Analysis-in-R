#install.packages("class")
#install.packages("ggplot2")
#install.packages("gridExtra")
library(class)
library(ggplot2)
library(gridExtra)

# train dataset is first 25 tuples of each type of iris together
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])

# test dataset is the left part of iris3
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])

# create a factor (s - Setosa, c - Versicolor, v - Virginica)
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))

# use the library function knn with 3 neighbours
result <- knn(train, test, cl, k = 3, prob=TRUE)[1:75]

# transform the test data into the dataframe
df.test <- as.data.frame(test)

# add the result to the dataframe
df.test$result <- result

# let's plot our results
sepal <- ggplot(df.test, aes(x=df.test$`Sepal L.`, y=df.test$`Sepal W.`, color=df.test$result)) +
  geom_point() +
  labs(title="Classification of Iris (Sepal)", x="Length", y="Width", color="Type")
petal <- ggplot(df.test, aes(x=df.test$`Petal L.`, y=df.test$`Petal W.`, color=df.test$result)) +
  geom_point() +
  labs(title="Classification of Iris (Petal)", x="Length", y="Width", color="Type")
grid.arrange(sepal, petal, ncol=2)

# estimation of classification quality

# build a confusion matrix where columns - expert decisions, rows - classificator decisions
confusion_matrix <- matrix(rep(0, 3), nrow = 3, ncol = 3)
colnames(confusion_matrix) <- c("s", "c", "v")
rownames(confusion_matrix) <- c("s", "c", "v")

result <- as.vector(result)
cl <- as.vector(cl)
for (i in 1:75) {
    confusion_matrix[result[i], cl[i]] = confusion_matrix[result[i], cl[i]] + 1
}

confusion_matrix

# we are going to use F score, so we need precision and recall

# count precision and recall for each type
precision_each <- diag(confusion_matrix)/rowSums(confusion_matrix)
recall_each <- diag(confusion_matrix)/colSums(confusion_matrix)

# find final result as a mean
precision <- mean(precision_each)
recall <- mean(recall_each)

# count F score
F_est <- 2 * (precision * recall / (precision + recall))

F_est