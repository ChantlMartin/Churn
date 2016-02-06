#############################
##Download SPSS Data Files###
#############################
library(foreign);
dataset = read.spss("C:\\Users\\Chantl\\Documents\\Marketing Analytics\\Fall 2015\\Statistical Programming\\Group Project\\Churn\\Cell2Cell_SPSS_Data.sav", to.data.frame=TRUE);

############################################
#Create Calibration and Validation Sample###
############################################

calibration <- dataset[dataset$CALIBRAT == 1, ];
validation <- dataset[dataset$CALIBRAT == 0, ];

########################
##Create CART Model#####
########################
library(rpart);

fit1<-rpart(CHURNDEP ~ EQPDAYS + MONTHS + RETCALL + CHANGEM + MOU + CREDITDE + CHANGER,data = calibration,cp = 10^(-7), minsplit = 2500);

#############################
##Create images of the data##
#############################
png("cellchurn1.png", width = 1200, height = 800);
post(fit, file = "", title. = "Classifying Churn Data Variables", bp = 18);
dev.off();

##############################################
##Create prediction and save outcome in file##
##############################################

validation$CHURNDEP <- predict(object = fit1, newdata = validation);

validation <- validation[order(validation$CHURNDEP, decreasing=TRUE),] ;

write.csv(validation, file="pkdata1.csv");

##################
##Plot ROC Curve##
##################
library(ROCR)

#we are using the second column of probabilities to generate an ROC curve
#first argument is predition data you want to access
#second argument is the actual data
pred1 = prediction(validation$CHURNDEP, validation$CHURN)

#first argument is outcome of prediction function
#second and third argument are x and y axis
#tpr = true positive rate
#fpr = false positive rate
perf1 = performance(pred1, "tpr", "fpr")

#plot ROC curve
plot(perf1, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2, 1.7), main = "Plot of ROC curve for CART Model")


######################################
##Calculate the area under the curve##
######################################

library(pROC);

area.result <- roc(formula = CHURN ~ CHURNDEP, data = validation);

area1 <- area.result$auc[1];

area1

##########################################################################

########################
##Create CART Model#####
########################
library(randomForest);

fit2<-randomForest(CHURNDEP ~ EQPDAYS + MONTHS + RETCALL + CHANGEM + MOU + CREDITDE + CHANGER,data = calibration,cp = 10^(-7), nodesize = 1200, ntree = 500, na.action = na.exclude);

#############################
##Create images of the data##
#############################
#png("cellchurn.png", width = 1200, height = 800);
#post(fit, file = "", title. = "Classifying Churn Data Variables",
#     bp = 18);
#dev.off();

#####################
##Create prediction##
#####################

validation$CHURNDEP <- predict(object = fit2, newdata = validation);

validation <- validation[order(validation$CHURNDEP, decreasing=TRUE),] ;

write.csv(validation, file="pkdata2.csv");

#sum(validation$CHURN);

#sum(validation$CHURN[1:3105]);

##################
##Plot ROC Curve##
##################
library(ROCR)

#we are using the second column of probabilities to generate an ROC curve
#first argument is predition data you want to access
#second argument is the actual data
pred2 = prediction(validation$CHURNDEP, validation$CHURN)

#first argument is outcome of prediction function
#second and third argument are x and y axis
#tpr = true positive rate
#fpr = false positive rate
perf2 = performance(pred2, "tpr", "fpr")

#plot ROC curve
plot(perf2, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2, 1.7), main = "Plot of ROC curve Using Random Forest")


######################################
##Calculate the area under the curve##
######################################

library(pROC);

area.result <- roc(formula = CHURN ~ CHURNDEP, data = validation);

area2 <- area.result$auc[1];

area2


###########################################################################

dataset$MOU[dataset$MOU == 0] <- 0.000001;
dataset$MOU <- log(dataset$MOU);
#dataset$CUSTCARE <- dataset$CUSTCARE^2;

dataset$EQPB <- rep(0,71047);
dataset$MONB <- rep(0,71047);

dataset$EQPB[dataset$EQPDAYS > 301] <- 1;
dataset$MONB[dataset$MONTHS > 10] <- 1;

dataset$EQMON <- dataset$EQPB*dataset$MONB;

dataset$EQP2 <- dataset$EQPDAYS*dataset$EQPDAYS;

calibration <- dataset[dataset$CALIBRAT == 1, ];
validation <- dataset[dataset$CALIBRAT == 0, ];

library(rpart);
library(randomForest);

#fit<-rpart(CHURNDEP ~ REVENUE + MOU + RECCHRGE + DIRECTAS + OVERAGE + ROAM + CHANGEM + CHANGER + DROPVCE + BLCKVCE + UNANSVCE + CUSTCARE + THREEWAY + MOUREC + OUTCALLS + INCALLS + PEAKVCE + OPEAKVCE + DROPBLK + CALLFWDV + CALLWAIT + MONTHS + UNIQSUBS + ACTVSUBS + PHONES + MODELS + EQPDAYS + AGE1 + AGE2 + CHILDREN + CREDITA + CREDITAA + CREDITB + CREDITC + CREDITDE + CREDITGY + CREDITZ + PRIZMRUR + PRIZMUB + PRIZMTWN + REFURB + WEBCAP + TRUCK + RV + OCCPROF + OCCCLER + OCCCRFT + OCCSTUD + OCCHMKR + OCCRET + OCCSELF + OWNRENT + MARRYUN + MARRYYES + MARRYNO + MAILORD + MAILRES + MAILFLAG + TRAVEL + PCOWN + CREDITCD + RETCALLS + RETACCPT + NEWCELLY + NEWCELLN + REFER + INCMISS + INCOME + MCYCLE + CREDITAD + SETPRCM + SETPRC + RETCALL,data = calibration, method="anova", cp=10^(-5), minsplit=1100);

fit3<-glm(CHURN ~ EQPB + MONB + EQMON + MOU + RECCHRGE + OVERAGE + CHANGEM + CHANGER + DROPVCE + BLCKVCE + MOUREC + PEAKVCE + OPEAKVCE + DROPBLK + CALLFWDV + CALLWAIT + MONTHS + MODELS + EQPDAYS + AGE2 + CHILDREN + CREDITAA + CREDITB + CREDITC + CREDITDE + CREDITGY + CREDITZ + PRIZMRUR + PRIZMTWN + REFURB + TRUCK + OCCPROF + OCCCLER + OCCSTUD + OCCHMKR + OCCRET + OWNRENT + MARRYNO + MAILRES + MAILFLAG + PCOWN + RETCALLS + RETACCPT + NEWCELLN + INCOME + CREDITAD + SETPRCM, data = calibration, family="binomial");

#fit<-rpart(CHURNDEP ~ REVENUE + MOU + RECCHRGE + DIRECTAS + OVERAGE + ROAM + CHANGEM + CHANGER + DROPVCE + BLCKVCE + UNANSVCE + CUSTCARE + THREEWAY + MOUREC + OUTCALLS + INCALLS + PEAKVCE + OPEAKVCE + DROPBLK + CALLFWDV + CALLWAIT + MONTHS + UNIQSUBS + ACTVSUBS + PHONES + MODELS + EQPDAYS + AGE1 + AGE2 + RETCALLS + RETACCPT + REFER + INCOME + CREDITAD + SETPRC,data = calibration,cp=10^(-5), minsplit = 400);

#calibration$CHURNDEP <- as.factor(calibration$CHURNDEP);

#calibration$CHURNDEPLOG <- log((calibration$CHURNDEP)/(1-calibration$CHURNDEP));

#best one yet; 
#fit<-rpart(CHURNDEP ~ EQPB + MONB + EQPDAYS + MONTHS + CHANGEM + MOU + RETCALL + OVERAGE + WEBCAP + INCALLS + SETPRC + REFURB + AGE2 + INCOME + MAILORD + BLCKVCE + MARRYUN + ACTVSUBS + MARRYNO + NEWCELLN + PRIZMUB + OCCCRFT, data = calibration,cp=10^(-7), minsplit = 377);

#png("spactree9.png", width = 1200, height = 800);
#post(fit, file = "", title. = "Classifying SPAC Donation Size, 9 splits",     bp = 18);
#dev.off();

validation$CHURNDEP <- predict(object = fit3, newdata = validation);

validation <- validation[order(validation$CHURNDEP, decreasing=TRUE),] ;

write.csv(validation, file="pkdata3.csv");

#sum(validation$CHURN);

#sum(validation$CHURN[1:3105]);

library(ROCR)

#we are using the second column of probabilities to generate an ROC curve
#first argument is predition data you want to access
#second argument is the actual data
pred3 = prediction(validation$CHURNDEP, validation$CHURN)

#first argument is outcome of prediction function
#second and third argument are x and y axis
#tpr = true positive rate
#fpr = false positive rate
perf3 = performance(pred3, "tpr", "fpr")

#plot ROC curve
plot(perf3, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2, 1.7), main = "Plot of ROC curve Using GLM")


library(pROC);

area.result <- roc(formula = CHURN ~ CHURNDEP, data = validation);

area3 <- area.result$auc[1];

area3
###########################################################################

#putting roc plots together

plot(perf1, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2, 1.7), main = "Plot of ROC curve for all models")
plot(perf2, add = TRUE, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2, 1.7))
plot(perf3, add = TRUE, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2, 1.7))

