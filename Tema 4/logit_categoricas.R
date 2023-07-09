install.packages("ISLR")
library(ISLR)
View(Default)
# Default en este contexto significa "impago"
aj1 <- glm(default ~ student, data = Default, family=binomial)
pred1<-predict(aj1,data.frame(student=c("Yes","No")), 
        type ="response")

pred1

aj2<-glm(default ~ .,data=Default,family=binomial)
exp(coef(aj2)["studentYes"])
predict(aj2,data.frame(student=c("Yes","No"),balance=1500,income=40000),
        type = "response")
