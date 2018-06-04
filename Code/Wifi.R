#### Data ####

## Load packages:
pacman::p_load(readr, caret, dplyr, scatterplot3d, ggplot2, tidyr, doSNOW, parallel, ranger, e1071)
# We don't have NA's in our dataset. 
##Load Data:
setwd("~/Desktop/Ubiqum/R_task6_Wifi")
validationData <- read.csv("validationData.csv", stringsAsFactors = FALSE)
Training.data<-read.csv("trainingData.csv", stringsAsFactors = FALSE ) 

####Exploring Training####
#Changed the categories of the variables:
Training.data$FLOOR<-as.factor(Training.data$FLOOR)
Training.data$BUILDINGID<-as.factor(Training.data$BUILDINGID)
Training.data$SPACEID<-as.factor(Training.data$USERID) # 18 different points.  Internal ID number to identify the Space (office, corridor, classroom) where the capture was taken, 
Training.data$RELATIVEPOSITION<-as.factor(Training.data$RELATIVEPOSITION) # in vs out
Training.data$USERID<-as.factor(Training.data$USERID)
Training.data$PHONEID<-as.factor(Training.data$PHONEID)

#We change the names of some categorical values:
Training.data$RELATIVEPOSITION<-factor(Training.data$RELATIVEPOSITION, levels = c(1,2), labels = c("Inside", "FrontDoor"))
Training.data$BUILDINGID<-factor(Training.data$BUILDINGID, levels = c(0, 1, 2), labels = c("B.1", "B.2", "B.3"))

#The same for Validation Data: 
validationData$FLOOR<-as.factor(validationData$FLOOR)
validationData$BUILDINGID<-as.factor(validationData$BUILDINGID)
validationData$SPACEID<-as.factor(validationData$USERID)
validationData$RELATIVEPOSITION<-as.factor(validationData$RELATIVEPOSITION)
validationData$USERID<-as.factor(validationData$USERID)
validationData$PHONEID<-as.factor(validationData$PHONEID)

#We change the names of some categorical values:
validationData$RELATIVEPOSITION<-factor(validationData$RELATIVEPOSITION, levels = c(1,2), labels = c("Inside", "FrontDoor"))
validationData$BUILDINGID<-factor(validationData$BUILDINGID, levels = c(0, 1, 2), labels = c("B.1", "B.2", "B.3"))


#*PLOTS####
##Training data##
# 3D Plot by building 
dfnew<- Training.data[,c(521:524,526)]
B1<-dfnew%>%filter(BUILDINGID=="B.1")
B2<-dfnew%>%filter(BUILDINGID=="B.2")
B3<-dfnew%>%filter(BUILDINGID=="B.3")

B1PLOT<-scatterplot3d(x=B1$LONGITUDE, y=B1$LATITUDE, z=B1$FLOOR, 
              xlab = "Longitude", ylab = "Latitude", zlab = "Floor", main="Building 1", box=FALSE)

B2PLOT<-scatterplot3d(x=B2$LONGITUDE, y=B2$LATITUDE, z=B2$FLOOR, 
                      xlab = "Longitude", ylab = "Latitude", zlab = "Floor", main="Building 2")

B3PLOT<-scatterplot3d(x=B3$LONGITUDE, y=B3$LATITUDE, z=B3$FLOOR, 
                      xlab = "Longitude", ylab = "Latitude", zlab = "Floor", main="Building 3")

# Flat plot all buildings: 
flatbuildings<-plot(Training.data$LONGITUDE, Training.data$LATITUDE, xlab = "Longitude", ylab = "Latitude", main="Locations - TRAINING DATA")

##Validation data##
dfnew_validation<- validationData[,c(521:524,526)]
B1_validation<-dfnew_validation%>%
  filter(BUILDINGID=="B.1")
B2_validation<-dfnew_validation%>%
  filter(BUILDINGID=="B.2")
B3_validation<-dfnew_validation%>%
  filter(BUILDINGID=="B.3")

B1PLOT_validation<-scatterplot3d(x=B1_validation$LONGITUDE, y=B1_validation$LATITUDE, z=B1_validation$FLOOR, 
                      xlab = "Longitude", ylab = "Latitude", zlab = "Floor", main="Building 1")

B2PLOT_validation<-scatterplot3d(x=B2_validation$LONGITUDE, y=B2_validation$LATITUDE, z=B2_validation$FLOOR, 
                      xlab = "Longitude", ylab = "Latitude", zlab = "Floor", main="Building 2")

B3PLOT_validation<-scatterplot3d(x=B3_validation$LONGITUDE, y=B3_validation$LATITUDE, z=B3_validation$FLOOR, 
                      xlab = "Longitude", ylab = "Latitude", zlab = "Floor", main="Building 3")

flatbuildings_validation<-plot(validationData$LONGITUDE, validationData$LATITUDE, xlab = "Longitude", ylab = "Latitude", main="Locations - VALIDATION DATA")

# We count the number of check's for position: ( View table)
count_B1F1<- B1F1%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())  ## also:   sapply(B1F1, uniqueN)

count_B1F2<- B1F2%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())

count_B1F3<- B1F3%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())

count_B2F1<- B2F1%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n()) # Locations have between 20 to 50 points! 

count_B3F1<- B3F1%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n()) # Locations have between 20 to 50 points! 

##ªªªConclusion 1: The user has recorded de data severeal times on the same 	time. 

##All floors by user
plotForUSer<-function(builNum) {
  builId<- paste("B.", builNum, sep = "" )
  plotTitle<- paste("Building", builNum, ": Users in floors", sep=" ")
  
  print(paste("building Id:", builId))
  print(paste("Plot title:", plotTitle))


Training.data%>%filter(BUILDINGID==builId)%>%
  ggplot()+
  geom_point(aes(x=LONGITUDE, y= LATITUDE, color=USERID)) + 
  facet_grid(. ~ FLOOR) + 
  labs(title=plotTitle) + 
  theme_linedraw(base_size = 11, base_family = "") + 
  theme(plot.title = element_text(hjust = 0.5, face="bold"))
}

plotForUSer("1")
plotForUSer("2")
plotForUSer("3")

##ªªªª Conclusion 2: B1 bad represented by User. B2 & B3 are OK. 


#All floors of building by Relative Position:
plotForRelativePosition <- function(buildingNumber) {
  buildingId<-paste("B.", buildingNumber, sep = "")
  plotTitle<-paste("Building ", buildingNumber, ": Position in floors", sep = "")
  
  print(paste("building Id:", buildingId))
  print(paste("Plot title:", plotTitle))
  
  Training.data%>%filter(BUILDINGID==buildingId)%>%
    ggplot() + 
    geom_point(aes(x=LONGITUDE, y= LATITUDE, color=RELATIVEPOSITION)) + 
    facet_grid(. ~ FLOOR) + 
    scale_color_manual(values = c("blue", "green")) + 
    labs(title=plotTitle) + 
    theme_linedraw(base_size = 11, base_family = "") + 
    theme(plot.title = element_text(hjust = 0.5, face="bold"))
}

plotForRelativePosition("1")
plotForRelativePosition("2")
plotForRelativePosition("3")

## ºººConclusion3: Bad represented in general by position: Lack of representativity inside rooms. 

#All floors of building by PhoneID:
plotForPhone<- function (buildID){
  buildingID<-paste("B.",buildID,sep="")
  plotTitle<- paste("Building", buildID, ": Phone ID in floors")
  
  print(paste("Building ID:", buildingID))
  print(paste( "Plot Title:", plotTitle))

Training.data%>%
  filter(BUILDINGID==buildingID)%>%
  ggplot()+
  geom_point(aes(x=LONGITUDE, y= LATITUDE, color=PHONEID)) + 
  facet_grid(. ~ FLOOR) + 
  labs(title=plotTitle) + 
  theme_linedraw(base_size = 11, base_family = "") + 
  theme(plot.title = element_text(hjust = 0.5, face="bold"))
}

plotForPhone("1")
plotForPhone("2")
plotForPhone("3")

## ººººConclusion 4: B1 again bad representer by Phone ID ( Same User ID)

# Number of locations by users 
plot(Training.data$USERID,
     xlab="USER NUMBER", ylab="frequency",
     main="Number of locations by User",
     col="pink")

# Number of location by PhoneId 
plot(Training.data$PHONEID,
     xlab="PHONE ID NUMBER",
     ylab="frequency",
     main="Number of locations by Phone",
     col="turquoise3")

## Conclusion 5: The extra activity by Phone and User are the two users of B1. They have done more than 20 clicks by position!!! 
  # B1 very bad represented whilest B2 and specially B3 are fine. 

####Exploring Validation Data####

countB1_validation<-B1_validation%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())

countB2_validation<-B2_validation%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())

countB3_validation<-B3_validation%>%
  group_by(LONGITUDE, LATITUDE, RELATIVEPOSITION)%>%
  summarize(count=n())

##ººConclusion: most positions have just 1 click, and they are well represented. 

plotForUSerVal<-function(builNum) {
  builId<- paste("B.", builNum, sep = "" )
  plotTitle<- paste("Building", builNum, ": Users in floors", sep=" ")
  
  print(paste("building Id:", builId))
  print(paste("Plot title:", plotTitle))
  
  
  validationData%>%filter(BUILDINGID==builId)%>%
    ggplot()+
    geom_point(aes(x=LONGITUDE, y= LATITUDE, color=USERID)) + 
    facet_grid(. ~ FLOOR) + 
    labs(title=plotTitle) + 
    theme_linedraw(base_size = 11, base_family = "") + 
    theme(plot.title = element_text(hjust = 0.5, face="bold"))
}

plotForUSerVal("1")
plotForUSerVal("2")
plotForUSerVal("3")
 ## No identity of the Users 

### Phone ID for Validation
plotForPhoneVal<- function (buildID){
  buildingID<-paste("B.",buildID,sep="")
  plotTitle<- paste("Building", buildID, ": Phone ID in floors - Validation")
  
  print(paste("Building ID:", buildingID))
  print(paste( "Plot Title:", plotTitle))
  
  validationData%>%
    filter(BUILDINGID==buildingID)%>%
    ggplot()+
    geom_point(aes(x=LONGITUDE, y= LATITUDE, color=PHONEID)) + 
    facet_grid(. ~ FLOOR) + 
    labs(title=plotTitle) + 
    theme_linedraw(base_size = 11, base_family = "") + 
    theme(plot.title = element_text(hjust = 0.5, face="bold"))
}

plotForPhoneVal("1")
plotForPhoneVal("2")
plotForPhoneVal("3")

## Well Represented by Phones ---> Validation data even there is less points, are better respresentet. 
# We decide to mix both datas for the better representation of Validation, specially in B1. 

####Pre-Processing data for modeling####

# We remove the columns with all 100 for both Training and Validation data: 
waps_only_training<- Training.data[, c(1:520)]
remove_100<- apply(waps_only_training, 2, function(x) length(unique(x))==1)
Training.data<-Training.data[,-c(which(remove_100==TRUE))]


waps_only_validation<-validationData[,c(1:520)]
remove_100_validation<-apply(validationData, 2, function(x) length(unique(x))==1)
validationData<-validationData[,-c(which(remove_100_validation==TRUE))]

# We remove the columns (WAPS) that are not in validation data to the training set: 
in.training <- (colnames(Training.data)%in%colnames(validationData))
Training.data<-Training.data[,-c(which(in.training==FALSE))]

#We remove the columns (WAPS) in validation that are not in training, and then we will get the same num of waps: 
in.validation<-(colnames(validationData)%in%colnames(Training.data))
validationData<-validationData[,-c(which(in.validation==FALSE))]

#Now we join Training and Validation datasets: 
all.data<-rbind(Training.data,validationData)

#We replace the rest of the 100 by  -105 to not confuse the model: 
all.data[all.data==100]<- -105
#We change the other WAP values so the intensity is always positive, so no signal will be = 0 , and max = +104:
all.data[,c(1:312)]<-all.data[,c(1:312)] +105

# Remove the ROWS that are all -105:
waps.only.all<-all.data[,c(1:312)]
delete.zv.rows<-apply(waps.only.all, 1, function(x) length(unique(x))==1)
all.data<-all.data[-c(which(delete.zv.rows==TRUE)),]


# We have to check for the uniques Waps for any building, and find the most powerfull ones: 
building1<- all.data%>%filter(BUILDINGID=="B.1")
building2<- all.data%>%filter(BUILDINGID=="B.2")
building3<- all.data%>%filter(BUILDINGID=="B.3")

waps_only_b1<-building1[,c(1:312)]
deliting_b1cols<-apply(waps_only_b1, 2, function(x) length(unique(x))==1)
waps_only_b1<-waps_only_b1[,-c(which(deliting_b1cols==TRUE))]

waps_only_b2<-building2[,c(1:312)]
deliting_b2cols<-apply(waps_only_b2, 2, function(x) length(unique(x))==1)
waps_only_b2<-waps_only_b2[,-c(which(deliting_b2cols==TRUE))]

waps_only_b3<-building3[,c(1:312)]
deliting_b3cols<-apply(waps_only_b3, 2, function(x) length(unique(x))==1)
waps_only_b3<-waps_only_b3[,-c(which(deliting_b3cols==TRUE))]

# waps in B1 that are in B1: 
match.1in2<-(colnames(waps_only_b1)%in%colnames(waps_only_b2))
waps_only_b1<-waps_only_b1[,-c(which(match.1in2==TRUE))]

match.1in3<-(colnames(waps_only_b1)%in%colnames(waps_only_b3))
waps_only_b1<-waps_only_b1[,-c(which(match.1in3==TRUE))]

match.2in3<-(colnames(waps_only_b2)%in%colnames(waps_only_b3))
waps_only_b2<-waps_only_b2[,-c(which(match.2in3==TRUE))]


#Created a formula for adding the waps.only columns to the model: 
waps.used <- colnames(waps_only_b1)
waps.used <- c(waps.used,colnames(waps_only_b2))
waps.used <- c(waps.used,colnames(waps_only_b3))
f <- paste("BUILDINGID" , "~", paste(waps.used, collapse = " + " ))
f <- as.formula(f)

#### BUILDING PREDICTION ####
####**Knn####
set.seed(998)
inTraining<-createDataPartition(all.data$BUILDINGID,
                                p=.70,
                                list = FALSE)

training<-all.data[inTraining,]
test<-all.data[-inTraining,]

cluster <- makeCluster(detectCores()-1)
registerDoSNOW(cluster)

fitControl <- trainControl(method = "cv",
                           number = 5,
                           allowParallel = TRUE,
                           verboseIter = TRUE)

knn.model.building<-train(f,
                          data=training,
                          method='knn',
                          trControl = fitControl,
                          preProcess = c("zv", "center", "scale"))
print(knn.model.building)

stopCluster(cluster)
registerDoSEQ(cluster)
rm(cluster)

save(knn.model.building, "building_knn_model.rda")
load("building_knn_model")

#With the last model, we make predictions:
prediction.building<-predict(knn.model.building, test)
confusion<-confusionMatrix(test$BUILDINGID,prediction.building)
summary(knn.model.building)
barchart(knn.model.building)
# accuracy: 99.97%

####_____errors #####

test$prediction<-prediction.building  # create column in our test with the predictions of the model 
test<-test%>%mutate(error.model=abs(as.numeric(prediction)-(as.numeric(BUILDINGID))))  # we create a column with 0 if its the same the predic than the obs, and !=0 otherwise. 
which(test$error.model!=0)  # Now we can check the rows :) 

df.error<-test%>%filter(error.model!=0)

test$error.model<-as.character(error.model) # I do it for the ggplot 
ggplot() + 
  geom_point(data=test, aes(x= LONGITUDE, y= LATITUDE, color= error.model)) +
  scale_color_manual(values = c("white", "black", "black")) +
  ggtitle("Errors prediction Building") + 
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

error.1<-df.error[1,]
waps.error.1<-apply(error.1, 1, function(x) x!=0)
prova.error<-error.1[,-c(which(waps.error.1==FALSE))]  # del primer error, solo me quedo con los waps != 0
m<-which(colnames(prova.error)%in%colnames(waps_only_b1)) # Observed
n<-which(colnames(prova.error)%in%colnames(waps_only_b3)) # Predicted
ñ<-which(colnames(prova.error)%in%colnames(waps_only_b2)) # locura, main waps that have signal are from b.2, but also few in B1 and B3 

#WAP013 WAP027 WAP028 WAP035 WAP036 WAP043 WAP044 WAP051 WAP052 WAP142 WAP143 WAP161 WAP162 WAP216
#WAP248 WAP323

error.2<-df.error[2,]
waps.error.2<-apply(error.2,1,function(x) x!=0)
prova.error2<-error.2[,-c(which(waps.error.2==FALSE))]
l<-which(colnames(prova.error2)%in%colnames(waps_only_b1))
o<-which(colnames(prova.error2)%in%colnames(waps_only_b3)) # Predicted # all waps are on b.3 !! 
p<-which(colnames(prova.error2)%in%colnames(waps_only_b2)) # Observed
 
#WAP113 WAP115 WAP116 WAP165 WAP174 WAP175 WAP178 WAP188 WAP282 WAP284 WAP295

# Graph validation set for Phone ID to check the positions: No good results, they are everywhere. 

validationData%>%
  filter(PHONEID=="0")%>%
  ggplot()+geom_point(aes(x=LONGITUDE, y= LATITUDE, color=PHONEID)) + 
  labs(title="PHONE ID 0 : Locations") + 
  theme_linedraw(base_size = 11, base_family = "") + 
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

validationData%>%
  filter(PHONEID=="12")%>%
  ggplot()+geom_point(aes(x=LONGITUDE, y= LATITUDE, color=PHONEID)) + 
  labs(title="PHONE ID 12 : Locations") + 
  theme_linedraw(base_size = 11, base_family = "") + 
  theme(plot.title = element_text(hjust = 0.5, face="bold"))


### Because we find out someting very strange in the WAPS, lets check:
# Which WAPS are unique for every building in Training Data:

B1.Training<-Training.data%>%filter(BUILDINGID=="B.1")
B2.Training<-Training.data%>%filter(BUILDINGID=="B.2")
B3.Training<-Training.data%>%filter(BUILDINGID=="B.3")

Twaps_only_b1<-B1.Training[,-c(313:318)]
deliting_b1cols<-apply(Twaps_only_b1, 2, function(x) length(unique(x))==1)
Twaps_only_b1<-Twaps_only_b1[,-c(which(deliting_b1cols==TRUE))]

Twaps_only_b2<-B2.Training[,-c(313:318)]
deliting_b2cols<-apply(Twaps_only_b2, 2, function(x) length(unique(x))==1)
Twaps_only_b2<-Twaps_only_b2[,-c(which(deliting_b2cols==TRUE))]

Twaps_only_b3<-B3.Training[,-c(313:318)]
deliting_b3cols<-apply(Twaps_only_b3, 2, function(x) length(unique(x))==1)
Twaps_only_b3<-Twaps_only_b3[,-c(which(deliting_b3cols==TRUE))]

# Waps unique for building - Training data: 
match.1in2<-(colnames(Twaps_only_b1)%in%colnames(Twaps_only_b2))
Twaps_only_b1<-Twaps_only_b1[,-c(which(match.1in2==TRUE))]

match.1in3<-(colnames(Twaps_only_b1)%in%colnames(Twaps_only_b3))
Twaps_only_b1<-Twaps_only_b1[,-c(which(match.1in3==TRUE))]

match.2in3<-(colnames(Twaps_only_b2)%in%colnames(Twaps_only_b3))
Twaps_only_b2<-Twaps_only_b2[,-c(which(match.2in3==TRUE))]

# Which WAPS are unique for every building in Validation Data:

B1.Val<-validationData%>%filter(BUILDINGID=="B.1")
B2.Val<-validationData%>%filter(BUILDINGID=="B.2")
B3.Val<-validationData%>%filter(BUILDINGID=="B.3")

Vwaps_only_b1<-B1.Val[,-c(313:318)]
deliting_b1cols<-apply(Vwaps_only_b1, 2, function(x) length(unique(x))==1)
Vwaps_only_b1<-Vwaps_only_b1[,-c(which(deliting_b1cols==TRUE))]

Vwaps_only_b2<-B2.Val[,-c(313:318)]
deliting_b2cols<-apply(Vwaps_only_b2, 2, function(x) length(unique(x))==1)
Vwaps_only_b2<-Vwaps_only_b2[,-c(which(deliting_b2cols==TRUE))]

Vwaps_only_b3<-B3.Val[,-c(313:318)]
deliting_b3cols<-apply(Vwaps_only_b3, 2, function(x) length(unique(x))==1)
Vwaps_only_b3<-Vwaps_only_b3[,-c(which(deliting_b3cols==TRUE))]

# Waps uniques for building - Validation Data: 
match.1in2<-(colnames(Vwaps_only_b1)%in%colnames(Vwaps_only_b2))
Vwaps_only_b1<-Vwaps_only_b1[,-c(which(match.1in2==TRUE))]

match.1in3<-(colnames(Vwaps_only_b1)%in%colnames(Vwaps_only_b3))
Vwaps_only_b1<-Vwaps_only_b1[,-c(which(match.1in3==TRUE))]

match.2in3<-(colnames(Vwaps_only_b2)%in%colnames(Vwaps_only_b3))
Vwaps_only_b2<-Vwaps_only_b2[,-c(which(match.2in3==TRUE))]

# ------------ Compare Results: 

dif.Waps.b1<-(colnames(Twaps_only_b1)%in%colnames(Vwaps_only_b1))
CHANGED.waps.b1<-Twaps_only_b1[,-c(which(dif.Waps.b1==TRUE))]
colnames(CHANGED.waps.b1)
# "WAP019" "WAP020" "WAP053" "WAP057" "WAP100" "WAP443"

dif.Waps.b2<-(colnames(Twaps_only_b2)%in%colnames(Vwaps_only_b2))
CHANGED.waps.b2<-Twaps_only_b2[,-c(which(dif.Waps.b2==TRUE))]
colnames(CHANGED.waps.b2)
#"WAP017" "WAP023" "WAP043" "WAP044" "WAP050" "WAP055" "WAP056" "WAP101" "WAP102" "WAP155" "WAP262"
# "WAP268" "WAP278" "WAP289" "WAP294" "WAP295" "WAP308" "WAP314" "WAP322" "WAP334" "WAP343" "WAP344"
# "WAP345" "WAP350" "WAP364"

dif.Waps.b3<-(colnames(Twaps_only_b3)%in%colnames(Vwaps_only_b3))
CHANGED.waps.b3<-Twaps_only_b3[,-c(which(dif.Waps.b3==TRUE))]
colnames(CHANGED.waps.b3)
# "WAP018" "WAP107" "WAP108" "WAP109" "WAP111" "WAP112" "WAP140" "WAP175" "WAP178" "WAP179" "WAP188"
# "WAP195" "WAP196" "WAP216" "WAP284" "WAP318"


#####**SVM####

set.seed(998)
inTraining<-createDataPartition(all.data$BUILDINGID,
                                p=.70,
                                list = FALSE)
training<-all.data[inTraining,]
test<-all.data[-inTraining,]

fitControl <- trainControl(method = "cv", number = 5,  verboseIter = TRUE)

svm.kernel.building<-train(f,
                          data=training,
                          method='svmRadial',
                          trControl = fitControl,
                          preProcess = c("zv", "center", "scale"))


print(svm.kernel.building)
prediction.building.svm<-predict(svm.kernel.building, test)
confusion<-confusionMatrix(test$BUILDINGID,prediction.building.svm)
# accuracy: 99.95%

#### MODEL FLOOR ####

#which waps have the highest signal:
which(waps_only_b3[,1:130] == 105, arr.ind=TRUE)  # Yes!! so the highest are in B3
which(waps_only_b2[,1:99] == 105, arr.ind=TRUE) # Null 
which(waps_only_b1[,1:83] == 105, arr.ind=TRUE) # Null 


# Let's map the POSITIONS that have good and bad signal: 

GoodSignal<-apply(all.data[,1:312], 1 , function(x) any(x==105))
GoodSignal<-which(GoodSignal==FALSE)
GoodS<-all.data[-c(GoodSignal),]
gs<-ggplot()+
  geom_point(data=GoodS,
             aes(x=LONGITUDE, y=LATITUDE, color=BUILDINGID)) +
  ggtitle("Locations where WAP signal was VERY HIGH")

LowSignal<-apply(all.data[,1:312], 1, function(x) all(x<20))
LowSignal<-which(LowSignal==FALSE)
LowS<-all.data[-c(LowSignal),]
ls<-ggplot()+
  geom_point(data=LowS,
             aes(x=LONGITUDE, y=LATITUDE, color=BUILDINGID)) + 
  ggtitle("Locations where WAP signal was VERY LOW")

#### Floor B.3 ####

#### *** Knn####
building3<-all.data%>%(filter(BUILDINGID=="B.3"))
building3$TIMESTAMP<-NULL
building3$PHONEID<-NULL
building3$LONGITUDE<-NULL
building3$LATITUDE<-NULL
building3$BUILDINGID<-NULL

set.seed(249)
inTraining.fb3<-createDataPartition(building3$FLOOR,
                                p=.70,
                                list = FALSE)

training.fb3<-building3[inTraining.fb3,]
test.fb3<-building3[-inTraining.fb3,]

fitControl <- trainControl(method = "cv", number = 5,  verboseIter = TRUE)

knn.model.fb3<-train(FLOOR ~.,
                          data=training.fb3,
                          method='knn',
                          trControl = fitControl,
                          preProcess = c("zv", "center", "scale"))

prediction.fb3<-predict(knn.model.fb3, test.fb3)
confusion.fb3<-confusionMatrix(test.fb3$FLOOR,prediction.fb3)
summary(knn.model.fb3)
barchart(knn.model.fb3)


#### ***SVM ####
building3$TIMESTAMP<-NULL
building3$PHONEID<-NULL
building3$LONGITUDE<-NULL
building3$LATITUDE<-NULL
building3$BUILDINGID<-NULL


set.seed(998)
inTraining<-createDataPartition(building3$FLOOR,
                                p=.70,
                                list = FALSE)
training<-building3[inTraining,]
test<-building3[-inTraining,]

numCores<-detectCores()-1
cluster<-makeCluster(numCores)

cluster <- makeCluster(detectCores()-1)
registerDoSNOW(cluster)

fitControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE, verboseIter = TRUE)

svm.floor3<-train(FLOOR ~.,
                   data=training,
                   method='svmRadial',
                   trControl = fitControl,
                   preProcess = c("zv", "center", "scale"))


print(svm.floor3)

stopCluster(cluster)
registerDoSEQ(cluster)
rm(cluster)


prediction.floor.svm<-predict(svm.floor3, test)
confusionf3<-confusionMatrix(test$FLOOR,prediction.floor.svm)
# Accuracy= 98,87%  // kappa= 0.098 


save(svm.floor3, file="svm.F3.rda")
load(file="svm.F3.rda")

####______errors####

test$prediction<-prediction.floor.svm
test$prediction<-as.numeric(test$prediction)
test$FLOOR<-as.numeric(test$prediction)
test$error<-as.numeric(test$prediction-test$FLOOR)

df.error<-test%>%filter(error!=0)

# test$error.model<-as.character(error.model) # I do it for the ggplot 
ggplot() + 
  geom_point(data=test, aes(x= LONGITUDE, y= LATITUDE, color= error)) +
  scale_color_manual(values = c("white", "black", "black")) +
  ggtitle("Errors prediction Building") + 
  theme(plot.title = element_text(hjust = 0.5, face="bold"))


#### Floor B.2 ####
#### ****Knn ####
# We have to drop the level 4 !!!!! we just convert the variable to factor again to drop the unused level.... 
building2<-all.data%>%filter(BUILDINGID=="B.2")
building2$FLOOR<-as.numeric(building2$FLOOR)
building2$FLOOR<-as.factor(building2$FLOOR)


building2$TIMESTAMP<-NULL
building2$PHONEID<-NULL
building2$LONGITUDE<-NULL
building2$LATITUDE<-NULL
building2$BUILDINGID<-NULL

set.seed(249)
inTraining.fb2<-createDataPartition(building2$FLOOR,
                                    p=.70,
                                    list = FALSE)

training.fb2<-building2[inTraining.fb2,]
test.fb2<-building2[-inTraining.fb2,]

fitControl <- trainControl(method = "cv", number = 5,  verboseIter = TRUE)

knn.model.fb2<-train(FLOOR ~.,
                     data=training.fb2,
                     method='knn',
                     trControl = fitControl,
                     preProcess = c("zv", "center", "scale"))

prediction.fb2<-predict(knn.model.fb2, test.fb2)
confusion.fb2<-confusionMatrix(test.fb2$FLOOR,prediction.fb2)
summary(knn.model.fb2)
barchart(knn.model.fb2)


#### ***SVM ####
set.seed(248)
inTraining<-createDataPartition(building2$FLOOR,
                                p=.70,
                                list = FALSE)
training.f2<-building2[inTraining,]
test.f2<-building2[-inTraining,]

cluster <- makeCluster(detectCores()-1)
registerDoSNOW(cluster)

fitControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE, verboseIter = TRUE)

svm.floor2<-train(FLOOR ~.,
                  data=training.f2,
                  method='svmRadial',
                  trControl = fitControl,
                  preProcess = c("zv", "center", "scale"))


print(svm.floor2)

stopCluster(cluster)
registerDoSEQ(cluster)
rm(cluster)


prediction.floor2.svm<-predict(svm.floor2, test.f2)
confusion.svmf2<-confusionMatrix(test.f2$FLOOR,prediction.floor2.svm)

# Accuracy = 98,96% // KAPPA= 0.986

####Floor.B1 ####

####***Knn ####
building1<- all.data%>%filter(BUILDINGID=="B.1")
building1$TIMESTAMP<-NULL
building1$PHONEID<-NULL
building1$LONGITUDE<-NULL
building1$LATITUDE<-NULL
building1$BUILDINGID<-NULL

building1$FLOOR<-as.numeric(building1$FLOOR)
building1$FLOOR<-as.factor(building1$FLOOR)


set.seed(249)
inTraining.fb1<-createDataPartition(building1$FLOOR,
                                    p=.70,
                                    list = FALSE)

training.fb1<-building1[inTraining.fb1,]
test.fb1<-building1[-inTraining.fb1,]

fitControl <- trainControl(method = "cv", number = 5,  verboseIter = TRUE)

system.time(knn.model.fb1<-train(FLOOR ~.,
                     data=training.fb1,
                     method='knn',
                     trControl = fitControl,
                     preProcess = c("zv", "center", "scale")))

prediction.fb1<-predict(knn.model.fb1, test.fb1)
confusion.fb1<-confusionMatrix(test.fb1$FLOOR,prediction.fb1)
summary(knn.model.fb1)


####*** SVM####
set.seed(998)
inTraining<-createDataPartition(building1$FLOOR,
                                p=.70,
                                list = FALSE)
training_f1<-building1[inTraining,]
test_f1<-building1[-inTraining,]


cluster <- makeCluster(detectCores()-1)
registerDoSNOW(cluster)

fitControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE, verboseIter = TRUE)

svm.floor1<-train(FLOOR ~.,
                  data=training_f1,
                  method='svmRadial',
                  trControl = fitControl,
                  preProcess = c("zv", "center", "scale"))


print(svm.floor1)

stopCluster(cluster)
registerDoSEQ(cluster)
rm(cluster)


prediction.f1.svm<-predict(svm.floor1, test_f1)
confusion_f1<-confusionMatrix(test_f1$FLOOR,prediction.f1.svm)

#Accuracy= 98,38%  // KAPPA= 0.984


################## LATITUDE ##################

# Convert longitud and latitude values to absolute values:
all.data$LATITUDE<-all.data$LATITUDE-min(all.data$LATITUDE)
all.data$LONGITUDE<-all.data$LONGITUDE-min(all.data$LONGITUDE)

# We check the max latitude and longitude: 
#max(all.data$LATITUDE)
#max(all.data$LONGITUDE)
# PLOT: ggplot()+geom_point(data=all.data, aes(x=LONGITUDE, y=LATITUDE)) + ggtitle("Log In Locations")

#### ** Latitude B.1 ####

build1.lat<- all.data%>%filter(BUILDINGID=="B.1")

build1.lat$TIMESTAMP<-NULL
build1.lat$PHONEID<-NULL
build1.lat$LONGITUDE<-NULL
build1.lat$BUILDINGID<-NULL
build1.lat$FLOOR<-NULL

#### **** Knn ####
set.seed(123)
inTraining.lat1<-createDataPartition(build1.lat$LATITUDE,
                                      p=.70,
                                      list = FALSE)

training.lat1<-build1.lat[inTraining.lat1,]
test.lat1<-build1.lat[-inTraining.lat1,]

fitControl <- trainControl(method = "cv", number = 5,  verboseIter = TRUE)

knn.model.lat1<-train(LATITUDE ~.,
                     data=training.lat1,
                     method='knn',
                     trControl = fitControl,
                     preProcess = c("zv", "medianImpute"))

prediction.lat1<-predict(knn.model.lat1, test.lat1)
error.lat.1<-postResample(prediction.lat1, test.lat1$LATITUDE)
# RMSE = 2.69 

####**** RF ####

set.seed(123)
inTraining.lat1<-createDataPartition(build1.lat$LATITUDE,
                                     p=.70,
                                     list = FALSE)

training.RF.lat1<-build1.lat[inTraining.lat1,]
test.RF.lat1<-build1.lat[-inTraining.lat1,]

numCores<-detectCores()-1
cluster<-makeCluster(numCores)

cluster <- makeCluster(detectCores()-1)
registerDoSNOW(cluster)

fitControl<- trainControl(method = "cv", number = 5,  verboseIter = TRUE)

rf.lat1<- train(LATITUDE ~ ., 
                data = training.RF.lat1,
                method = "ranger",
                trControl = fitControl, 
                preProcess = c("zv", "medianImpute"))

stopCluster(cluster)
registerDoSEQ(cluster)
rm(cluster)

prediction.RF.lat1<-predict(rf.lat1, test.RF.lat1)
error.rf.lat1<-postResample(prediction.RF.lat1, test.RF.lat1$LATITUDE)
# RMSE= 2.57  Rsquared= 0.99


# To save the models for the next time...!! 
save(rf.lat1, "rf.lat1.rda")
load(rf.lat1)


####** Latitude B.2####
build2.lat<- all.data%>%filter(BUILDINGID=="B.2")

build2.lat$TIMESTAMP<-NULL
build2.lat$PHONEID<-NULL
build2.lat$LONGITUDE<-NULL
build2.lat$BUILDINGID<-NULL
build2.lat$FLOOR<-NULL

####****Knn####
set.seed(123)
inTraining.lat2<-createDataPartition(build2.lat$LATITUDE,
                                      p=.70,
                                      list = FALSE)

training.lat2<-build2.lat[inTraining.lat2,]
test.lat2<-build2.lat[-inTraining.lat2,]

fitControl <- trainControl(method = "cv", number = 5,  verboseIter = TRUE)

knn.model.lat2<-train(LATITUDE ~.,
                      data=training.lat2,
                      method='knn',
                      trControl = fitControl,
                      preProcess = c("zv", "medianImpute"))

prediction.lat2<-predict(knn.model.lat2, test.lat2)
error.lat.2<-postResample(prediction.lat2, test.lat2$LATITUDE)
# RMSE=4.96

####**** RF####

set.seed(123)
inTraining.lat2<-createDataPartition(build2.lat$LATITUDE,
                                     p=.70,
                                     list = FALSE)

training.RF.lat2<-build2.lat[inTraining.lat2,]
test.RF.lat2<-build2.lat[-inTraining.lat2,]

numCores<-detectCores()-1
cluster<-makeCluster(numCores)

cluster <- makeCluster(detectCores()-1)
registerDoSNOW(cluster)

fitControl<- trainControl(method = "cv", number = 5,  verboseIter = TRUE)
  
rf.lat2<- train(LATITUDE ~ ., 
                data = training.RF.lat2,
                method = "ranger",
                trControl = fitControl, 
                preProcess = c("zv", "medianImpute"))

stopCluster(cluster)
registerDoSEQ(cluster)
rm(cluster)

prediction.RF.lat2<-predict(rf.lat2, test.RF.lat2)
error.rf.lat2<-postResample(prediction.RF.lat2, test.RF.lat2$LATITUDE)
# RMSE= 3.67
# Rsquared =99%

save(rf.lat2, file ="rf_lat2.rda")
load("rf_lat2.rda")


####______errors####
test.RF.lat2$prediction<-prediction.RF.lat2
test.RF.lat2$error.model<- abs(test.RF.lat2$prediction-test.RF.lat2$LATITUDE)
error10m<-which(test.RF.lat2$error.model > 10)

plot_ly(data= test.RF.lat2, type= "scatter", x= ~ LONGITUDE, y= ~ LATITUDE, name="observations", mode= "markers") %>% add_trace( y= ~ prediction , name="predictions") 


####** Latitude B.3 ####

build3.lat<- all.data%>%filter(BUILDINGID=="B.3")

build3.lat$TIMESTAMP<-NULL
build3.lat$PHONEID<-NULL
build3.lat$LONGITUDE<-NULL
build3.lat$BUILDINGID<-NULL
build3.lat$FLOOR<-NULL

####****Knn####
set.seed(123)
inTraining.lat3<-createDataPartition(build3.lat$LATITUDE,
                                     p=.70,
                                     list = FALSE)

training.lat3<-build3.lat[inTraining.lat3,]
test.lat3<-build3.lat[-inTraining.lat3,]

fitControl <- trainControl(method = "cv", number = 5,  verboseIter = TRUE)

knn.model.lat3<-train(LATITUDE ~.,
                      data=training.lat3,
                      method='knn',
                      trControl = fitControl,
                      preProcess = c("zv", "medianImpute"))

prediction.lat3<-predict(knn.model.lat3, test.lat3)
error.lat.3<-postResample(prediction.lat3, test.lat3$LATITUDE)


#### **** RF ####
set.seed(123)
inTraining.lat3<-createDataPartition(build3.lat$LATITUDE,
                                     p=.70,
                                     list = FALSE)

training.RF.lat3<-build3.lat[inTraining.lat3,]
test.RF.lat3<-build3.lat[-inTraining.lat3,]

numCores<-detectCores()-1
cluster<-makeCluster(numCores)

cluster <- makeCluster(detectCores()-1)
registerDoSNOW(cluster)

fitControl<- trainControl(method = "cv", number = 5,  verboseIter = TRUE)

rf.lat3<- train(LATITUDE ~ ., 
                data = training.RF.lat3,
                method = "ranger",
                trControl = fitControl, 
                preProcess = c("zv", "medianImpute"))

stopCluster(cluster)
registerDoSEQ(cluster)
rm(cluster)

prediction.RF.lat3<-predict(rf.lat3, test.RF.lat3)
error.rf.lat3<-postResample(prediction.RF.lat3, test.RF.lat3$LATITUDE)
# RMSE = 3.40  # Rsquared= 0.98

####################LONGITUDE########################

####** Longitude B.1 ####
build1.long<-all.data%>%filter(BUILDINGID=="B.1")

build1.long$TIMESTAMP<-NULL
build1.long$PHONEID<-NULL
build1.long$LATITUDE<-NULL
build1.long$BUILDINGID<-NULL
build1.long$FLOOR<-NULL

#### **** Knn ####
set.seed(123)
inTraining.lONG.1<-createDataPartition(build1.long$LONGITUDE,
                                       p=.70,
                                       list = FALSE)

training.long1<-build1.long[inTraining.lONG.1,]
test.long1<-build1.long[-inTraining.lONG.1,]

fitControl <- trainControl(method = "cv", number = 5,  verboseIter = TRUE)

knn.model.long1<-train(LONGITUDE ~.,
                       data=training.long1,
                       method='knn',
                       trControl = fitControl,
                       preProcess = c("zv", "medianImpute"))

prediction.long1<-predict(knn.model.long1, test.long1)
error.long.1<-postResample(prediction.long1, test.long1$LONGITUDE)

#####****RF####

set.seed(123)
inTraining<-createDataPartition(build1.long$LONGITUDE,
                                     p=.70,
                                     list = FALSE)

training.RF.long1<-build1.long[inTraining,]
test.RF.long1<-build1.long[-inTraining,]


cluster <- makeCluster(detectCores()-1)
registerDoSNOW(cluster)

fitControl<- trainControl(method = "cv", number = 5,  verboseIter = TRUE)

rf.long1<- train(LONGITUDE ~ ., 
                data = training.RF.long1,
                method = "ranger",
                trControl = fitControl, 
                preProcess = c("zv", "medianImpute"))

stopCluster(cluster)
registerDoSEQ(cluster)
rm(cluster)

prediction.RF.long1<-predict(rf.long1, test.RF.long1)
error.rf.long1<-postResample(prediction.RF.long1, test.RF.long1$LONGITUDE) 
# RMSE= 3,14 # Rsquared= 0.98

#### **Longitude B.2 ####

build2.long<-all.data%>%filter(BUILDINGID=="B.2")
build2.long$TIMESTAMP<-NULL
build2.long$PHONEID<-NULL
build2.long$LATITUDE<-NULL
build2.long$BUILDINGID<-NULL
build2.long$FLOOR<-NULL

#### ****Knn ####
set.seed(123)
inTraining.lONG.2<-createDataPartition(build2.long$LONGITUDE,
                                      p=.70,
                                      list = FALSE)

training.long2<-build2.long[inTraining.lONG.2,]
test.long2<-build2.long[-inTraining.lONG.2,]

fitControl <- trainControl(method = "cv", number = 5,  verboseIter = TRUE)

knn.model.long2<-train(LONGITUDE ~.,
                      data=training.long2,
                      method='knn',
                      trControl = fitControl,
                      preProcess = c("zv", "medianImpute"))

prediction.long2<-predict(knn.model.long2, test.long2)
error.long.2<-postResample(prediction.long2, test.long2$LONGITUDE)

####**** RF ####
set.seed(123)
inTraining<-createDataPartition(build2.long$LONGITUDE,
                                p=.70,
                                list = FALSE)

training.RF.long2<-build2.long[inTraining,]
test.RF.long2<-build2.long[-inTraining,]


cluster <- makeCluster(detectCores()-1)
registerDoSNOW(cluster)

fitControl<- trainControl(method = "cv", number = 5,  verboseIter = TRUE)

rf.long2<- train(LONGITUDE ~ ., 
                 data = training.RF.long2,
                 method = "ranger",
                 trControl = fitControl, 
                 preProcess = c("zv", "medianImpute"))

stopCluster(cluster)
registerDoSEQ(cluster)
rm(cluster)

prediction.RF.long2<-predict(rf.long2, test.RF.long2)
error.rf.long2<-postResample(prediction.RF.long2, test.RF.long2$LONGITUDE) 
# RMSE= 4.10  # Rsquared= 0,99

####**Longitude B.3 ####

build3.long<-all.data%>%filter(BUILDINGID=="B.3")

build3.long$TIMESTAMP<-NULL
build3.long$PHONEID<-NULL
build3.long$LATITUDE<-NULL
build3.long$BUILDINGID<-NULL
build3.long$FLOOR<-NULL

#### **** Knn ####
set.seed(123)
inTraining.lONG.3<-createDataPartition(build3.long$LONGITUDE,
                                       p=.70,
                                       list = FALSE)

training.long3<-build3.long[inTraining.lONG.3,]
test.long3<-build3.long[-inTraining.lONG.3,]

fitControl <- trainControl(method = "cv", number = 5,  verboseIter = TRUE)

knn.model.long3<-train(LONGITUDE ~.,
                       data=training.long3,
                       method='knn',
                       trControl = fitControl,
                       preProcess = c("zv", "medianImpute"))

prediction.long3<-predict(knn.model.long3, test.long3)
error.long.3<-postResample(prediction.long3, test.long3$LONGITUDE)

#### **** RF ####
set.seed(123)
inTraining<-createDataPartition(build3.long$LONGITUDE,
                                p=.70,
                                list = FALSE)

training.RF.long3<-build3.long[inTraining,]
test.RF.long3<-build3.long[-inTraining,]


cluster <- makeCluster(detectCores()-1)
registerDoSNOW(cluster)

fitControl<- trainControl(method = "cv", number = 5,  verboseIter = TRUE)

rf.long3<- train(LONGITUDE ~ ., 
                 data = training.RF.long3,
                 method = "ranger",
                 trControl = fitControl, 
                 preProcess = c("zv", "medianImpute"))

stopCluster(cluster)
registerDoSEQ(cluster)
rm(cluster)

prediction.RF.long3<-predict(rf.long3, test.RF.long3)
error.rf.long3<-postResample(prediction.RF.long3, test.RF.long3$LONGITUDE) 
#RMSE= 5,36 # Rsquared= 0,98

# We create a dataset with the errors:
a<-data.frame(c("RMSE", "Rsquared", "MAE"), c(error.lat.1, error.lat.2, error.lat.3))
colnames(a)[1]<-"METRICS"
colnames(a)[2]<-"ERROR"
a<-a[-c(3,6,9),]
df.RMSE<-a[c(1,3,5),]  
df.Rsquared<-a[c(2,4,6),]
RMSE_latitude<-mean(df.RMSE$ERROR)
#3.651483
RSquared_latitude<-mean(df.Rsquared$ERROR)
#0.987053


b<-data.frame(c("RMSE", "Rsquared", "MAE"), c(error.long.1, error.long.2, error.long.3))
colnames(b)[1]<-"METRICS"
colnames(b)[2]<-"ERROR"
b<-b[-c(3,6,9),]
df.RMSE.long<-b[c(1,3,5),]  
df.Rsquared.long<-b[c(2,4,6),]
RMSE_longitude<-mean(df.RMSE.long$ERROR)
#4.3811
RSquared_longitude<-mean(df.Rsquared.long$ERROR)
#0.983055

distances<-- c("Longitude", "Latitude")
matrix.long<- c()






####ªª More WAPS were changed from B2 and B3, this is way the predictions
#      are worst on those building 
------------------------------------------------
  ------------------------------------------------

--------------------  ##Alexito ---------------------

WantToVisit<-function(country) {
  sentence<-paste("I want to go to", country, sep=" FUCKING ")
  print(sentence)
}
  
WantToVisit("SouthAfrica")

sayHelloTo <- function(friend) {
  greeting <- paste("hola", friend)
  print(greeting)
}
sayHelloTo("your mum")




norm<- function(x){
  y= x - min(x) / max(x) - min(x)
  print (y)
}



for ( i in 1:10)
  if (i %% 2)
   print(i)


