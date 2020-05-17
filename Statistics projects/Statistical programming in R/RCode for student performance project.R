##############################################################################################################################
##############################################################################################################################
#########################  MATH 267A R Programming Project                                                ####################
#########################  Project Name: Statistical Analysis of High School Student Academic Performance ####################
#########################  Authors: Jung-a Kim, Mengqi Yin                                                ####################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################


##### RData file and libraries
load('student.RData')
library('ggplot2')
library(RColorBrewer)
library(magrittr)# For %>%
library(dplyr)# For group_by
library(plotrix)# For pie3D
library('gridExtra')# Multiple plots in a row
library(InformationValue)# For misclassification error
library(ROCR)# To find True Positive Rate and True Negative Rate for Logistic Regression
library(car) # To find VIF's of the predictors

# Description of the dataset:
# This dataset was used for data mining project at Univeristy of Guimarães, Portugal to predict
# student achievement in secondary education of two Portuguese schools, Gabriel Pereira and Mousinho da Silveira.
# The dataset has 395 students with the attributes including students' first, second, and final Math exam scores,
# demographic, social, and school-related features collected by using school reports and questionnaires.
# The perfect score for each exam is 20.
# The third exam score(G3) has a strong correlation with the first exam(G1) and the second exam(G2)
# with r = 0.80 and r = 0.90 respectively.


########## [Histogram] of overall dispersity in Age,Absences,and Scores ###################################################
graphics.off()
par(mfrow=c(2,3),oma=c(1,1,3,1))
hist(student$age, col = "darkolivegreen3",xlab='Age',main = "Student Age",breaks=seq(15,22,1))
hist(student$absences,col = "bisque3",xlab='Absences',main = "Number of school absences",breaks=seq(0,93,1))
hist(student$avgG,col="coral3",main="Average Score",xlab='Score',breaks=seq(0,20,1))
hist(student$G1,col="darkorchid3",main="First period grade",xlab='Score',breaks=seq(0,20,1))
hist(student$G2,col="darkslategray3",main="Second period grade",xlab='Score',breaks=seq(0,20,1))
hist(student$G3,col="deeppink2",main="Final grade",xlab='Score',breaks=seq(0,20,1))
title('Dispersity',outer=TRUE)

# Interpretation:
# The histograms above show the dispersity of continuous variables age, the number of absences, and grade in the dataset.
# In "Dispersity in Student Age", it is shown that the majority of students who come to school is about 15-18 years old.
# For absences, most of the students miss 0-5 classes.
# The first exam score, second exam score, and final exam score all have approximate symmetric bell-shaped distribution
# with the same median equal to 11. Those whose second grade is zero have also zero score for their final exams.
# There are 38 students who have zero score in their final grades.
which(student$G2==0) %in% which(student$G3==0)# Those whose second grade is zero all have final grade as zero.
length(which(student$G3==0))# The number of zero final scores.

#========= [Boxplot] of Age distribution against sex ==> shows the difference in mean age btwn sex.
p1 = ggplot(student, aes(x=sex,y=age,fill=sex))+xlab('Sex')+ylab('Age')+ggtitle('Age distribution')
means = aggregate(age~sex,data=student,FUN = 'mean')
p1+geom_boxplot()+stat_summary(aes(label=paste('mean=',round(..y..,2))),fun.y='mean',geom='point',col='yellow')+
  geom_text(data=means,aes(label=round(age,2),y=age+0.2),col='white',fontface='bold',size=4)+
  scale_fill_manual('Sex:',labels=c('Female','Male'),values=c('cornflowerblue','coral3'))

#========= [Histogram_dodge] of Age distribution against sex ==> shows the skewness of age distrb.
p2 = ggplot(student,aes(x=age,fill=sex))
p2+geom_histogram(binwidth=.5,position='dodge')+scale_fill_manual('Sex:',labels=c('Female','Male'),values=c('cornflowerblue','coral3'))+
  theme(legend.position = "top")+geom_text(aes(label=..count..),stat='count',position=position_dodge(.5),vjust=-.2)+
  ylim(0,65)+xlab('Age')

max(student$age)
median(student$age)
mean.age = mean(student$age)#16.6962
mean.age.f = mean(student$age[student$sex=='F'])#16.73077
mean.age.m = mean(student$age[student$sex=='M'])#16.65775

# Interpretation:
# The first box plot shows the difference in mean age between sex.
# The second box plot shows the skewness of the age distribution for both sex.
# The minimum age is 15, maximum student age is 22, median age is 17 and mean age is 16.7.
# The mean age is similar between female and male students as the mean age of female students is 16.73 and
# the mean age of male students is 16.66.
# An outlier is a 22-year-old male student.
###################################################################################################################################


########## [Barplot] Female students who want to take higher education pay extra classes #################################
# Computing the count of students who paid extra classes categorized by sex and higher
p3=ggplot(student,aes(x=higher,fill=paid))+facet_wrap(~sex)
p3+geom_bar(position="dodge")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)+
  ggtitle('Students who paid extra classes')+xlab('Want to take higher education')+
  scale_fill_manual(name = 'Paid Extra classes',labels=c('No','Yes'),values=c('deeppink3','dodgerblue3'))+
  theme(legend.position = 'top')

# Interpretation:
# This graph suggests that for female students who want to take higher education, about 53%
# (108 out of 204 female students) take extra paid Math classes.
# Male students who want to take higher education about 42.1%(73 out of 171 male students)
# take extra paid Math classes.

subset = student[student$higher==1,]# Subset those who want to go to college
tb1 = table(subset$sex,subset$paid)# Create a frequency table for each combination of the categories. 
tb1 = cbind(yes=tb1[,2],no=tb1[,1])# Put higher=='yes' to the first column for proportion test.
prop.test(tb1,correct=FALSE)

# Interpretation:
# The null hypothesis that the proportion of taking paid classes
# between male and female students who want to take higher education is the same.
# With p-value = 0.03645 < 0.05, we reject H0 and conclude that
# female students who want to take higher education is more likely to take paid classes than male students.
###################################################################################################################################


########## [Boxplot] Grade after taking paid math classes #########################################################################
p4 = ggplot(student, aes(x=factor(paid),y=totalG,fill=paid))+xlab('Take paid Math classes')+
  ylab('Total grade of the three exams')+ggtitle('Grade after taking paid classes')
p4+geom_boxplot()+stat_summary(aes(label=paste('mean=',round(..y..,2))),size=5,fun.y='mean',geom='text',col='white')+
  theme(legend.position = 'top')

# Interpretation:
# The mean of the total score of students who take paid math classes is 33.11602.
# The mean of the total score of students who do not take paid math classes is 31.12617.
# Given that the total score is 60, the mean scores for the two groups are not much different.
# The median of the score of students who take paid courses is 33
# The median of the score of students who do not take paid courses is 32.

t.test(student$totalG[student$paid=='no'],student$totalG[student$paid=='yes'])

# Interpretation:
# With p-value = 0.06922 and significance level = 0.05, we fail to reject that the mean total score of students
# who take paid math classes is different from the mean total score of students who do not take paid math classes and
# conclude that the mean total scores are the same.
####################################################################################################################################


######### [Barplot] Family relationship + Grade + Pstatus ##########################################################################
# Grouping the dataset by the variables 'Pstatus','famrel', and 'avgL'.
# For each combination, computes the proportions of each letter grade.
percentData1 = student %>% group_by(Pstatus,famrel) %>% count(avgL) %>%
  mutate(ratio=scales::percent(n/sum(n)))
p5 = ggplot(student,aes(factor(famrel),fill=factor(avgL)))
p5+geom_bar(position='fill')+
  geom_text(data=percentData1,aes(y=n,label=ratio),size=5,position=position_fill(vjust=0.5))+
  facet_wrap(~Pstatus,labeller = labeller(Pstatus = c(A = 'Parents Apart',T = 'Parents Together')))+
  xlab('Family Relationship')+ylab('')+scale_y_reverse()+coord_flip()+theme(legend.position='top')+
  scale_fill_brewer('Average Grade:', labels=c(': A',': B',': C',': D',': F'),palette='Accent')+
  theme(axis.text.x = element_blank(),axis.ticks.x=element_blank())

# Interpretation:
# It is noticeable that if the parents are together, the failure ratio is moderate across all the family
# relationship levels.
# Overall, failure ratio is greater when the parents are apart than when the parents are together.
# If the family relationship is excellent, the ratio of A is higher when parents are apart.
# But since there's only 41 out of 395 students whose parents are apart,
# so there is some bias in comparing the ratio between the two groups.
####################################################################################################################################


########## [Barplot] Those in a relationship + studytime + grade ###################################################################
# Grouping the dataset by the variables 'romantic','studytime', and'avgL'
# For each combination, computes the proportions of each letter grade.
percentData2 = student %>% group_by(romantic,studytime) %>% count(avgL) %>%
  mutate(ratio=scales::percent(n/sum(n)))
p6 = ggplot(student,aes(factor(studytime),fill=factor(avgL)))+geom_bar(position='fill')
p6+geom_text(data=percentData2,aes(y=n,label=ratio),size=5,position=position_fill(vjust=0.5))+
  facet_wrap(~romantic,labeller = labeller(romantic=c(no='Single',yes='In a relatioship')))+
  xlab('Study time')+ylab('')+scale_y_reverse()+coord_flip()+  theme(legend.position='top')+
  scale_fill_brewer('Average Grade:', labels=c(': A',': B',': C',': D',': F'),palette='Accent')+
  theme(axis.text.x = element_blank(),axis.ticks.x=element_blank())

#========= [Boxplot] Studytime + average score
p7 = ggplot(student, aes(factor(studytime),avgG,fill=factor(studytime)))
p7+geom_boxplot()+ ggtitle("Weekly study time vs Average Score")+
  stat_summary(fun.y='mean',colour='white',size=2,geom='point')+xlab('Study time')+ylab('Average Score')+
  scale_fill_brewer('Studytime:', labels=c('<2hrs','2~5hrs','5~10hrs','>10hrs'),palette='Set2')

#========= [Boxplot] goout + average score
p8 = ggplot(student, aes(factor(goout),avgG,fill=factor(goout)))
p8+geom_boxplot()+ ggtitle("Going out with friends vs Average Score")+
  scale_fill_brewer('Going out with friends:', labels=c('very low','low','medium','high','very high'),palette='Set2')+
  ylab('Average score')+xlab('Going out with friends')+
  stat_summary(fun.y='mean',colour='red',size=2,geom='point')


# Interpretation:
# It is noticeable that students in a relationship who spend more than 10 hours on studying per week
# don't have 'A' or 'B' as their average grades.
# Those who are in a relationship don't have 'A'.
# 'B' is also rare for those in a relationship who study more than 2 hours per week.
# Those in a relationship who study less than 2 hours per week have grade C or lower.
# Also, when those in a relationship are more likely to fail than those who are single,
# if they spend less than 2 hours on studying per week.
# Those who are single and study more than 10 hours per week, 9% of them get 'A'.
# Failure rate for those who study 2~10 hours per week is similar between those who are
# in a relationship or not in a relationship.

# From the first box plot, we can verify that the mean score between those who study 5~10 hours is
# not much different than those who study more than 10 hours per week.
# From the second box plot, we can see that those who hang out very high has the lowest average score.
# But those who hang out with friends at medium level is about similar to those who rarely hang out.
####################################################################################################################################


########## [Geom_smooth] Simple Linear Regression: Average score + Absences + Paid #################################################
p9 = ggplot(student,aes(absences,avgG,color='paid')) 
p9+geom_smooth(method='lm')+ggtitle('Average score against Absences by Extra paid class')+
  facet_wrap(~paid,labeller = labeller(paid=c(no='Did not pay the extra class',yes = 'Paid the extra class')))+
  scale_color_brewer(palette = 'Spectral')+ylab("Average Score")+xlab('Number of Absences')+
  theme(legend.position = 'none')

# Interpretation:
# If they paid the extra class, the number of absences and the average score has a negative linear relationship.
# If they did not pay the extra class, the absences and the average score have a positive relationship.
# It is noticeable that those two plots have a large variance for the large number of absences.
# There are only 26 students out of 395 students who were absent more than 16 times.
# Thus, the error is large for the number of absences greater than 16.
###################################################################################################################################


########## Logistic Regression ####################################################################################################
# Question: Does an individual want to go to college?(higher)

# First, we separate training data and testing data(unused new data)
# Check class bias
table(student$higher)# no to yes is 20:375 <==> 20 should not be concentrated in one sample.
higher_yes = student[student$higher==1,]# success
higher_no = student[student$higher==0,]# failure

set.seed(1)# Randomly assign training data = 90% of success + 90% of failures
higher_yes_training_rows = sample(1:nrow(higher_yes),0.9*nrow(higher_yes))# 90% of success
higher_no_training_rows = sample(1:nrow(higher_no),0.9*nrow(higher_no))# 90% of failures


# Training Data
training_yes = higher_yes[higher_yes_training_rows,]
training_no = higher_no[higher_no_training_rows,]
training_data = rbind(training_yes,training_no)

# Test Data = everything that is not training data
new_yes = higher_yes[-higher_yes_training_rows,]
new_no = higher_no[-higher_no_training_rows,]
new_data = rbind(new_yes,new_no)

# Binomial Logistic Regression based on training data
model = glm(higher~studytime+failures+Medu,family=binomial(link='logit'),data=training_data)

### Since the class is biased towards 'higher'==1, we need to select optimal threshold to determine success
# To get the optimal cut-off(threshold for success), the plan is to randomly select 10% of the training dataset
# and average the cutoffs

# First, we evenly split the dataset again.
table(training_data$higher)
higher_yes = training_data[training_data$higher==1,]
higher_no = training_data[training_data$higher==0,]

cutoff = numeric()
# optimal cut-off is where the product of True Positive Rate and True Negative Rate is maximum
# (the point where two line graphs meet)

opt.cut = function(tpr.y, tnr.y){
  # Returns the index of the cut-off probability
  
  d = abs(tpr.y-tnr.y)# The distance between the y-coordinates between TPR and TNR
  ind = which(d == min(d))# The distance has to be the minimum to find the point they meet.
  return(ind)# Return the index of the cut-off
}


# To decide the opitmal cut-off,
# We once again randomly select 10% of the training dataset 100 times and save each cut-off.
# Then we averaged the 100 cut-offs and decide it as the optimal cut-off for the new dataset

for(i in 1:100){
set.seed(i)# Randomly assign test data
higher_yes_test_rows = sample(1:nrow(higher_yes),0.1*nrow(higher_yes))
higher_no_test_rows = sample(1:nrow(higher_no),0.1*nrow(higher_no))

# Test Data
test_yes = higher_yes[higher_yes_test_rows,]
test_no = higher_no[higher_no_test_rows,]
test_data = rbind(test_yes,test_no)

probs = predict(model,test_data,type='response')# predicted probabilities needed to compute TPR and TNR
pred = prediction(probs,test_data$higher)# prediction object to draw TPR and TNR line graphs
perf1 = performance(pred,labels=test_data,measure = 'tpr',x.measure = 'cutoff')# True Positive Rate graph
perf2 = performance(pred,labels=test_data,measure = 'tnr',x.measure = 'cutoff')# True Negative Rate graph

tpr.y = unlist(slot(perf1,'y.values'))# True Positive Rate values(in probability)
tnr.y = unlist(slot(perf2,'y.values'))# True Negative Rate values(in probability)
x.values = unlist(slot(perf1,'x.values'))# Unique predicted probabilities
cutoff[i] = x.values[opt.cut(tpr.y,tnr.y)]# Save each cut-off in 100 loops
}
optCutOff = mean(cutoff)# optimal cut-off = average of the 100 cut-offs(0.8903155)

# A plot of 100th "TPR vs TNR graph"
graphics.off()
par(yaxt='n',mar=c(5,4,4,4))
plot(x.values,tpr.y, ylab=NA,col='blue',pch=20,xlab='Cut-off')
par(new=T,xaxt='n',yaxt='n')
plot(perf1,col='blue',ylab=NA,xlab='')
par(new=T,xaxt='n')
plot(x.values,tnr.y, ylab=NA,pch=20,xlab='')
par(new=T,xaxt='n')
plot(perf2,ylab=NA,xlab='')
abline(v=x.values[opt.cut(tpr.y,tnr.y)],lty=2,col='red')
par(yaxt='s')
axis(2,ylim=c(0,1))
axis(4,ylim=c(0,1))
legend(x = 'topright',inset=c(-.1,-.3),xpd=T,legend=c('True Positive Rate','True Negative Rate'),col=c('blue','black'),pch=19:19,cex=.7)
title('TPR vs TNR')

### Does the person #338 from the new dataset want to go to college?
# The person's weekly study hours is 2~5 hours, the person failed no class,
# and the mother has a secondary education.
# Compare the predicted success(yes or no) to the actual variable 'higher' value
(predict(model,student[338,],type='response')>optCutOff) == (student[338,'higher']==1) # True positive(both TRUE)


### The person #154 from the new dataset does not want to go to college.
# This person's weekly study hours is less than 2 hours, the person failed 3 classes,
# and the mother has a secondary education.
(predict(model,student[154,],type='response')>optCutOff) == (student[154,'higher']==1) # True negative(both FALSE)


# Check the accuracy with the unused new data
predicted = predict(model,new_data,type='response')
misClassError(new_data$higher,predicted,threshold=optCutOff)# 2.5% = percentage mismatch of predicted vs actuals


summary(model)
vif(model)# All predictors around 1 which means they're linearly independent.


# Probabilities for every possible combinations of the three variables (4*5*4=80 cases)
# All combinations of the three factors' levels
tb2 = with(student,expand.grid(Medu=unique(Medu),studytime=unique(studytime),failures=unique(failures)))
tb2$prob = predict(model,tb2,type='response')# Add 'prob' column for predicted probabilities

#========= [Barplot_dodge] Mean predicted probabilities of wanting to go to college
# against Mother's education degree according to studytime
mean.probs1 = tb2 %>% group_by(studytime,Medu) %>% summarise(avg.prob=mean(prob))
p10=ggplot(mean.probs1,aes(Medu,avg.prob,fill=factor(studytime)))+theme(legend.position = 'top')+
  geom_col(position='dodge')+ggtitle('Mean predicted probabilities of wanting to go to college')+
  ylab('Average Predicted Probability')+xlab('Mother\'s education')+
  geom_text(position=position_dodge(width=1),size=3.5,aes(y=avg.prob-.05,label=paste(round(avg.prob,2)*100,'%',sep='')))+
  scale_fill_manual(name='Studytime:',labels = c(': <2hrs',': 2-5hrs',': 5-10hrs',': >10hrs'),values=c('gold','darkolivegreen4','purple','deeppink'))

#========= [Geom_smooth] Mean predicted probabilities in smoothe line(shows the confint of probabilities)
p11 = ggplot(tb2,aes(Medu,prob,color=factor(studytime,levels=c(4,3,2,1)),group=studytime))+
  geom_smooth()+theme(legend.position = 'top')+xlab('Mother\'s education')+ggtitle('')+
  ylab('Average Predicted Probability')+labs(color='Studytime:')+
  scale_color_manual(labels = c(": >10hrs", ": 5-10hrs",': 2-5hrs',': <2hrs'),values=c('deeppink','purple','darkolivegreen4','gold'))
  
grid.arrange(p10,p11,nrow=1)# Show two plots in the same frame

#========= [Piechart] The students of each Mother's education is evenly spread.
graphics.off()
slices = c(length(which(student$Medu==0)),length(which(student$Medu==1)),length(which(student$Medu==2)),length(which(student$Medu==3)),length(which(student$Medu==4)))
lbls = c('None ','4th grade ','5th - 9th grade ','Secondary ','Higher education ')
pct = round(slices/sum(slices)*100)
lbls = paste(lbls,'(',pct,'%)',sep='')
pie3D(slices,labels=lbls,col=brewer.pal(length(lbls),'Paired'),labelcex=1,main="Mother's education",explode=0.2,theta=1)

# Interpretation: 
# If the Mother's education is college or higher, a student who studies less than 2 hours a week
# would want to go to college with 90% probaility on average, but the variation is very large
# for those who study less than 2 hours.
# A student who studies more than 10 hours a week would almost 100% want to go to college,
# regardless of the mother's education degree.

#========= [Barplot_dodge] Mean predicted probabilities of wanting to go to college
# against failures according to studytime
mean.probs2 = tb2 %>% group_by(studytime,failures) %>% summarise(avg.prob=mean(prob))
p12=ggplot(mean.probs2,aes(failures,avg.prob,fill=factor(studytime)))+
  geom_col(position='dodge')+ggtitle('Mean predicted probabilities of wanting to go to college')+
  geom_text(position=position_dodge(width=1),size=3.5,aes(y=avg.prob-.05,label=paste(round(avg.prob,3)*100,'%',sep='')))+
  xlab('Failures')+ylab('Average Predicted Probability')+
  scale_fill_manual(name='Studytime:',labels = c(': <2hrs',': 2-5hrs',': 5-10hrs',': >10hrs'),values=c('gold','darkolivegreen4','purple','deeppink'))+
  theme(legend.position = 'top')

#========= [Geom_smooth] Mean predicted probabilities in smooth line
p13=ggplot(tb2,aes(failures,prob,color=factor(studytime,levels=c(4,3,2,1)),group=studytime))+
  geom_smooth()+xlab('Failures')+ggtitle('')+
  labs(color='Studytime:')+ylab('Average Predicted Probability')+
  scale_color_manual(labels = c(": >10hrs", ": 5-10hrs",': 2-5hrs',': <2hrs'),values=c('deeppink','purple','darkolivegreen4','gold'))+
  theme(legend.position = 'top')

grid.arrange(p12,p13,nrow=1)# Show two plots in the same frame

# Interpretation:
# A student who studies less than 2 hours with 0 class failure are more likely to want to
# go to college than those who study 2~5 hours with 2~3 class failures.
# It is noticeable that if a student studies more than 10 hours a week,
# the student is likely to want to go to college regardless of the number of failures.
# If a student studies less than 2 hours a week, the probability of wanting to go to college
# decreases steeply as the number of failures increases.


####################################################################################################################################
# REFERENCES
#
# P. Cortez and A. Silva. Using Data Mining to Predict Secondary School Student Performance. In A. Brito and J. Teixeira Eds., Proceedings of 5th FUture BUsiness TEChnology Conference (FUBUTEC 2008) pp. 5-12, Porto, Portugal, April, 2008, EUROSIS, ISBN 978-9077381-39-7. 
# Available at: https://archive.ics.uci.edu/ml/datasets/student+performance
#
# Logistic Regression Reference URL: http://r-statistics.co/Logistic-Regression-With-R.html
####################################################################################################################################