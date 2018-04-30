#### Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now
possible to collect a large amount of data about personal activity
relatively inexpensively. These type of devices are part of the
quantified self movement.A group of enthusiasts who take measurements
about themselves regularly to improve their health, to find patterns in
their behavior, or because they are tech geeks. One thing that people
regularly do is quantify how much of a particular activity they do, but
they rarely quantify how well they do it.

#### Overview of the data

Six young health participants were asked to perform one set of 10
repetitions of the Unilateral Dumbbell Biceps Curl in five different
fashions: exactly according to the specification (Class A), throwing the
elbows to the front (Class B), lifting the dumbbell only halfway (Class
C), lowering the dumbbell only halfway (Class D) and throwing the hips
to the front (Class E). Class A corresponds to the specified execution
of the exercise, while the other 4 classes correspond to common
mistakes.

Read more: <http://groupware.les.inf.puc-rio.br/har#ixzz5E6GrxVGS>

##### Goal

In this project, the goal will be to use data from accelerometers on the
belt, forearm, arm, and dumbell of 6 participants. They were asked to
perform barbell lifts correctly and incorrectly in 5 different ways. The
goal of your project is to predict the manner in which they did the
exercise. This is the "classe" variable in the training set. You may use
any of the other variables to predict with.

#### How do we approch this goal

1.  Describing how to choose the best model
2.  Use cross validation,
3.  Expected out of sample error is
4.  Why you made the choices you did
5.  Use the selected prediction model to predict 20 different test cases

#### Load data and exploratory data analysis

    library(AppliedPredictiveModeling)

    ## Warning: package 'AppliedPredictiveModeling' was built under R version
    ## 3.3.3

    library(caret)

    ## Warning: package 'caret' was built under R version 3.3.3

    ## Warning: package 'ggplot2' was built under R version 3.3.3

    library(rpart)
    library(randomForest)

    ## Warning: package 'randomForest' was built under R version 3.3.3

    library(gbm)

    ## Warning: package 'gbm' was built under R version 3.3.3

    ## Warning: package 'survival' was built under R version 3.3.3

    library(e1071)

    ## Warning: package 'e1071' was built under R version 3.3.3

    training<-read.csv("C:/Users/yinjing/Desktop/R/Coursera/pml-training.csv", na.string=c("NA","#DIV/0!", ""))

    testing<-read.csv("C:/Users/yinjing/Desktop/R/Coursera/pml-testing.csv",na.string=c("NA","#DIV/0!", ""))

    dim(training)

    ## [1] 19622   160

    dim(testing)

    ## [1]  20 160

    inTrain = createDataPartition(training$classe,p=0.7, list=FALSE)
    myTraining<-training[inTrain,]
    myTesting<-training[-inTrain,]

**data cleaning**

1.  remove NearZeroVariance variables
2.  remove variable with more than 97% NA's
3.  remove non-predictor variables:"X","user\_name",
    "raw\_timestamp\_part\_1","raw\_timestamp\_part\_2",
    "cvtd\_timestamp", "num\_window"

<!-- -->

    #remove NearZeroVariance variables
    nzv<-nearZeroVar(myTraining)
    myTraining<-myTraining[,-nzv]
    myTesting<-myTesting[,-nzv]

    #remove variable with more than 97% NA's

    remV<-sapply(myTraining, function(x) mean(is.na(x)))


    myTraining<-myTraining[, remV<0.03]
    myTesting<-myTesting[, remV<0.03]

    #remove non-predictor variables
    myTraining<-myTraining[,-c(1:6)]
    myTesting<-myTesting[,-c(1:6)]

### Build model

to choose the best model, we will fit the data with three machine
learning algorithms. we will use Decision Trees, Random Forest and
Generalized Boosted Regression Model with 5-fold cross validation.

**Compare different algorithm**

    set.seed(1234)
    ctr1<-trainControl(method="repeatedcv", number=5,repeats=1,verboseIter = FALSE)

    modFit1<-train(classe~., data=myTraining, method="rf", trControl=ctr1)

    modFit2<-rpart(classe~., method="class", data=myTraining)
    modFit3<-train(classe~., data=myTraining,  method="gbm",trControl=ctr1, verbose=FALSE)

    pred_rf<-predict(modFit1, myTesting)
    pred_rpart<-predict(modFit2,myTesting, type="class")
    pred_gbm<-predict(modFit3, myTesting)

    confusionMatrix(pred_rf,myTesting$classe)$overall[1]

    ##  Accuracy 
    ## 0.9950722

    confusionMatrix(pred_rpart,myTesting$classe)$overall[1]

    ##  Accuracy 
    ## 0.7486831

    confusionMatrix(pred_gbm,myTesting$classe)$overall[1]

    ##  Accuracy 
    ## 0.9604078

as we can see that the Random Forest algorithm has the highest accuracy
99.35. so the our of sample error is 1-the accuracy, which is 0.65%

    varImp(modFit1)

    ## rf variable importance
    ## 
    ##   only 20 most important variables shown (out of 52)
    ## 
    ##                   Overall
    ## roll_belt          100.00
    ## yaw_belt            76.58
    ## magnet_dumbbell_z   70.05
    ## magnet_dumbbell_y   63.53
    ## pitch_belt          62.49
    ## pitch_forearm       60.55
    ## roll_forearm        57.03
    ## magnet_dumbbell_x   51.56
    ## magnet_belt_y       44.49
    ## accel_belt_z        44.12
    ## magnet_belt_z       42.77
    ## accel_dumbbell_y    41.82
    ## roll_dumbbell       40.03
    ## accel_dumbbell_z    36.24
    ## accel_forearm_x     34.10
    ## roll_arm            33.80
    ## gyros_belt_z        30.80
    ## yaw_dumbbell        29.81
    ## gyros_dumbbell_y    28.96
    ## accel_dumbbell_x    28.46

**use the Random Forest prediction model to predict 20 different test
cases**

    pred_new<-predict(modFit1, newdata=testing)
    pred_new

    ##  [1] B A B A A E D B A A B C B A E E A B B B
    ## Levels: A B C D E
