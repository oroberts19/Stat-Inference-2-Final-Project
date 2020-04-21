library(tidyverse)

# reading in the math class
mat <- read.csv('./data/student-mat.csv')

# reading in the portugese class
port <- read.csv('./data/student-por.csv')


## The following code is to be used if we wish to use both classes, math and portugese

# merge is included in Kaggle with the student identifying columns to merge on
both.class.students <- merge(mat,port,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

# the Kaggle creator specifies that there are 382 students that are in both classes 
## here we check that the correct number of students result from the merge
nrow(both.class.students)

# this is conducted for the 649 observations of students in the dataset 
## these students are in the Portugese course

# here I rename the columns so that they are clear and preesnt a summary of each varaibles
portugese.class.data <- port %>% 
  select(School = school, 
         Gender = sex, 
         Age = age, 
         Address = address, 
         Family.Size = famsize, 
         Parental.Status = Pstatus, 
         Mothers.Edu = Medu, 
         Fathers.Edu = Fedu, 
         Mothers.Job = Mjob, 
         Fathers.job = Fjob, 
         Reason.for.school.choice = reason,
         Students.guardian = guardian,
         Travel.time = traveltime, 
         Study.time = studytime,
         Num.past.class.failures = failures,
         School.support = schoolsup, 
         Family.support = famsup,
         Extra.port.classes = paid,
         Activities = activities, 
         Nusery.School = nursery,
         Wants.higher.ed = higher,
         Home.internet = internet, 
         Romantic.Relationship = romantic,
         Family.rel.qual = famrel, 
         Free.time.categorical = freetime,
         Time.out.categorical = goout,
         Workday.alc = Dalc,
         Weekend.alc = Walc,
         Health.categorical = health, 
         Count.absences = absences, 
         First.term.grade = G1, 
         Second.term.grade = G2, 
         Final.Grade = G3)

# structure
str(portugese.class.data)

# summary 
summary(portugese.class.data)

# # all the plots below are included without cleaning or preprocessing the data, they are just a look at  
# # # the raw data in context of the tasks for the project

# plots for Count of absenses with different categorical variables
absenses.and.grade <- ggplot(data = portugese.class.data, 
                             aes(x = Count.absences, 
                                 y = Final.Grade)) +
  geom_smooth(method = "lm")

absenses.and.grade
# # workday alcohol consumption
work.day.alc <- ggplot(data = portugese.class.data, 
                       aes(x = Count.absences, 
                           y = Final.Grade, 
                           col = as.factor(Workday.alc))) +
                       geom_smooth(method = "lm")
work.day.alc

# # weekend alcohol consumption
weekend.alc <-  ggplot(data = portugese.class.data, 
                       aes(x = Count.absences, 
                           y = Final.Grade, 
                           col = as.factor(Weekend.alc))) +
                       geom_smooth(method = "lm") 

weekend.alc

# # binary nursery school, i.e. did they attend
nursery.school <- ggplot(data = portugese.class.data, 
                         aes(x = Count.absences, 
                             y = Final.Grade, 
                             col = as.factor(Nusery.School))) +
                         geom_smooth(method = "lm")

nursery.school

# Note that the grades are on a 1-20 scale so they follow the "common reporting scale" where 7 or high is a 
# # passing score. also note that there are 4 passing classes
# # # 7-10 are third class 
# # # 11-13 are Lower Second Class 
# # # 14-16 are Upper Second Class 
# # # 17-20 are First Class

# an example of creating a binary response variable and running a simple logistic regression
# # UPPER  are final grades greater than the median and LOWER are less than the median 

binary.data <- portugese.class.data

# the median is 12
med <- median(binary.data$Final.Grade)

binary.data.with.response <- binary.data %>%
  mutate(Eval = as.factor(ifelse(Final.Grade<med, yes = "lower", no="uppser")))

# shows the counts of upper and lower cases 
# # if you change the boundary to pass fail there are only 20 fails 
summary(binary.data.with.response$Eval)

# simple logistic regression with varaible count of absense 

log.reg.data <- binary.data.with.response %>%
  select(Eval, Count.absences)

fit <- glm(Eval~., family = "binomial", data = log.reg.data)
summary(fit)

