
data(aSAH)
# Syntax (response, predictor):
auc(aSAH$outcome, aSAH$s100b)
# With a roc object:
rocobj <- roc(aSAH$outcome, aSAH$s100b)
# Full AUC:
auc(rocobj)
#Plot
plot(rocobj)
plot(rocobj,print.thres="best")
result.coords <- coords(rocobj, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))


#Prediction 
result.predicted<- factor(ifelse(aSAH$s100b > result.coords[1], "Poor", "Good"))
xtabs(~  result.predicted + aSAH$outcome)
table( result.predicted,aSAH$outcome)
