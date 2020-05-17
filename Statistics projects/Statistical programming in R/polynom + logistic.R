############### Polynomial regr on studytime, traveltime, goout #########################
load('student.RData')
attach(student)
fit = lm(avgG~poly(studytime-mean(studytime),degree=2,raw=T))
plot(studytime ,avgG)
lines(sort(studytime ),fitted(fit)[order(studytime )])

fit = lm(avgG~poly(traveltime-mean(traveltime),degree=3,raw=T))# 3rd dg makes more sense in the dot plot.
plot(traveltime ,avgG)
lines(sort(traveltime ),fitted(fit)[order(traveltime )])

fit = lm(avgG~poly(goout-mean(goout),degree=2,raw=T))
plot(goout,avgG)
lines(sort(goout),fitted(fit)[order(goout)])

########## To be done... Logistic Regression ##########
# Does an individual take higher education?
student=read.table("student/student-mat.csv",sep=";",header=TRUE)# 395 students
attach(student)
fac = factor(student$higher,level=c('no','yes'),labels = c(0,1))
student$higher = 1
student$higher = fac
# Check class bias
table(student$higher)
higher_yes = student[student$higher==1,]
higher_no = student[student$higher==0,]
set.seed(1)
higher_yes_training_rows = sample(1:nrow(higher_yes),0.9*nrow(higher_yes))
higher_no_training_rows = sample(1:nrow(higher_no),0.9*nrow(higher_no))
# Training Data
training_yes = higher_yes[higher_yes_training_rows,]
training_no = higher_no[higher_no_training_rows,]
training_data = rbind(training_yes,training_no)
# Test Data
test_yes = higher_yes[-higher_yes_training_rows,]
test_no = higher_no[-higher_no_training_rows,]
test_data = rbind(test_yes,test_no)
model = glm(higher~studytime+failures+Medu,family=binomial(link='logit'),data=training_data)
predicted = predict(model,test_data,type='response')
#library(InformationValue)
optCutOff = optimalCutoff(test_data$higher,predicted)# Minimizes the misclassification error
summary(model)
#library(car)
vif(model)
misClassError(test_data$higher,predicted,threshold=optCutOff)# percentage mismatch of predicted vs actuals

# Probabilities for every possible combinations of the three variables (4*5*4=80 cases)
newdata = with(student,expand.grid(Medu=unique(Medu),studytime=unique(studytime),failures=unique(failures)))# All combinations of the three factors' levels
newdata$prob = predict(model,newdata,type='response')

# Trying to make it as smoothe line(shows the confint of probabilities)
ggplot(newdata,aes(Medu,prob,color=factor(studytime),group=studytime))+geom_smooth()
ggplot(newdata,aes(studytime,prob,color=factor(failures),group=failures))+geom_smooth()

# We can plot the mean predicted probabilities in barplot_dodge
mean.probs = newdata %>% group_by(studytime,Medu) %>% summarise(avg=mean(prob))
p=ggplot(mean.probs,aes(Medu,avg,fill=factor(studytime)))
p+geom_col(position='dodge')+ylab('mean of probabilities of taking higher edu')
mean.probs = newdata %>% group_by(studytime,failures) %>% summarise(avg=mean(prob))
p=ggplot(mean.probs,aes(failures,avg,fill=factor(studytime)))
p+geom_col(position='dodge')+ylab('mean of probabilities of taking higher edu')


## Reference URL: http://r-statistics.co/Logistic-Regression-With-R.html
###########################################################################