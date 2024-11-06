# veriyi gap}ral}m (Importing the dataset)
dataset = read.csv('BBAD.csv')
head(dataset)
dataset = dataset[3:5]
dataset

summary (dataset)
str(dataset)


# Eksik verileri tespit edelim (missing data detection)
anyNA(dataset)
is.na(dataset)
sum(is.na(dataset))
colSums(is.na(dataset))

#ayk}r} gvzlemleri tespiti, silme ya da ortalama ile depi~tirme (outlier detection)
class(dataset$jobcategory)
boxplot(dataset$current_salary)
boxplot(dataset$beginning_salary)

#silme
y=which(dataset$current_salary %in% boxplot.stats(dataset$current_salary)$out)
y
z=which(dataset$beginning_salary %in% boxplot.stats(dataset$beginning_salary)$out)
z

birlikte=union(y,z)
dataset=dataset[-birlikte,] #silme

boxplot(dataset$current_salary)
boxplot(dataset$beginning_salary)

#encoding
dataset$jobcategory = factor(dataset$jobcategory, levels = c(1,2,3))

#Veri Ayr}~t}rma  Epitim ve Test veri seti (Splitting the dataset into the Training set and Test set)
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$jobcategory, SplitRatio = 0.80)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

training_set
test_set

training_set[,2:3]=scale(training_set[,2:3])
test_set[,2:3]=scale(test_set[,2:3])
training_set
test_set


#SVM
install.packages('e1071')
library(e1071)
classifier = svm(formula = jobcategory ~ beginning_salary +  current_salary ,
 
                 data = training_set,
                 type = "C-classification",
                 kernel = "linear")

# Test veri setindeki birimlerin s}n}flar}n}n tahmini (Predicting the Test set results)
y_pred = predict(classifier, newdata = test_set[-1])

# do??ruluk oran?? Performans vlg|m| igin Karma~}kl}k matrisi olu~turulmas} (Making the Confusion Matrix for Performance Measurement)
cm = table(test_set[,1], y_pred)
acc=(cm[1,1]+cm[2,2]+cm[3,3])/sum(cm) #dopruluk katsay}s}n} gvrelim (accuracy)
acc
err=1-acc
err
