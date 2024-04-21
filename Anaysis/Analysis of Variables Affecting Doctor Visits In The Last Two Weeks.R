visit_pois <- glm(visits ~ ., data = Visits, family = poisson(link="log"))
summary(visit_pois)
pchisq(visit_pois$deviance, visit_pois$df.residual, lower.tail=F)
par(mfrow=c(2,2))
plot(visit_pois)

visit_qpois <- glm(visits ~ ., family = quasipoisson, data = Visits)
summary(visit_qpois)
par(mfrow=c(2,2))
plot(visit_qpois)
pchisq(visit_qpois$deviance, visit_qpois$df.residual, lower.tail=F)

visit_nb <- glm.nb(visits ~ ., data = Visits)
summary(visit_nb)
par(mfrow=c(2,2))
plot(visit_nb)

# Copyng visits dataframe into a new dataframe and coding binary variables
Visits2 <- Visits
Visits2$private <- ifelse(Visits2$private == 'yes', 1, 0)
Visits2$freepoor <- ifelse(Visits2$freepoor == 'yes', 1, 0)
Visits2$freerepat <- ifelse(Visits2$freerepat == 'yes', 1, 0)
Visits2$nchronic <- ifelse(Visits2$nchronic == 'yes', 1, 0)
Visits2$lchronic <- ifelse(Visits2$lchronic == 'yes', 1, 0)
Visits2$gender <- ifelse(Visits2$gender == 'male', 1, 0)

cor_matrix <- cor(Visits2)
pairs(Visits2)
cor_matrix1 <- as.data.frame(cor_matrix)



#Visits data without nchronic after lasso
Visits2$nchronic <- NULL

#new regression without nchronic
visit2_pois <- glm(visits ~ ., data = Visits2, family = poisson(link="log"))
summary(visit2_pois)
pchisq(visit2_pois$deviance, visit2_pois$df.residual, lower.tail=F)
par(mfrow=c(2,2))
plot(visit2_pois)


#quasipoisson regression
visit2_qpois <- glm(visits ~ ., family = quasipoisson(link="log"), data = Visits2)
summary(visit2_qpois)
par(mfrow=c(2,2))
plot(visit_qpois)

#Negative binomial regression
visit_nb2 <- glm.nb(visits ~ ., data = Visits2)
summary(visit_nb2)
par(mfrow=c(2,2))
plot(visit_nb2)
vif(visit3_nb)

Visits3 <- Visits
Visits3$nchronic <- NULL
Visits3$private <- NULL

visit3_pois <- glm(visits ~ ., data = Visits3, family = poisson(link="log"))
summary(visit3_pois)
pchisq(visit3_pois$deviance, visit3_pois$df.residual, lower.tail=F)
par(mfrow=c(2,2))
plot(visit3_pois)

visit3_qpois <- glm(visits ~ ., family = quasipoisson, data = Visits3)
summary(visit3_qpois)
par(mfrow=c(2,2))
plot(visit_qpois)

visit3_nb <- glm.nb(visits ~ ., data = Visits3)
summary(visit3_nb)
par(mfrow=c(2,2))
plot(visit3_nb)



Visits4 <- Visits
Visits4$nchronic <- NULL
Visits4$private <- NULL
Visits4$freerepat <- NULL
visit4_pois <- glm(visits ~ ., data = Visits4, family = poisson(link="log"))
summary(visit4_pois)


visit4_nb <- glm.nb(visits ~ ., data = Visits4)
summary(visit4_nb)


Visits5 <- Visits
Visits5$nchronic <- NULL
Visits5$private <- NULL
Visits5$freerepat <- NULL
Visits5$income <- NULL

visit5_pois <- glm(visits ~ ., data = Visits5, family = poisson(link="log"))
summary(visit5_pois)
par(mfrow=c(2,2))
plot(visit5_pois)


visit5_nb <- glm.nb(visits ~ ., data = Visits5)
summary(visit5_nb)
par(mfrow=c(2,2))
plot(visit5_nb)

Visits6 <- Visits
Visits6$nchronic <- NULL
Visits6$private <- NULL
Visits6$freerepat <- NULL
Visits6$income <- NULL
Visits6$lchronic <- NULL

rm(visit6_pois)
visit6_nb <- glm.nb(visits ~ ., data = Visits6)
summary(visit6_nb)
par(mfrow=c(2,2))
plot(visit6_nb)


visit6_qpois <- glm(visits ~ ., family = quasipoisson, data = Visits6)
summary(visit6_qpois)
par(mfrow=c(2,2))
plot(visit6_qpois)


visit6_nb <- glm(visits ~ ., data = Visits6, family = poisson(link="log"))
summary(visit6_nb)
par(mfrow=c(2,2))
plot(visit6_nb)
Visit8 <- Visits
Visits8$freerepat <- NULL
Visits8$private <- NULL
Visits8$nchronic <- NULL


visit8_pois <- glm(visits ~ ., data = Visits8, family = poisson(link="log"))
summary(visit8_pois)
plot(visit7_pois)

visit7_nb <- glm(visits ~ ., data = Visits7, family = poisson(link="log"))
summary(visit7_nb)
plot(visit7_nb)
pchisq(visit8_pois$deviance, visit8_pois$df.residual, lower.tail=F)

Visit9 <- Visits
Visit9$freerepat <- NULL
Visit9$nchronic <- NULL
visit_pois9 <- glm(visits ~ ., data = Visit9, family = poisson(link="log"))
summary(visit_pois9)
plot(visit_pois9)

visit_nb9 <- glm.nb(visits ~ ., data = Visit9)
summary(visit_nb9)
par(mfrow=c(2,2))
plot(visit_nb9)

# Stepwise regression using AIC
stepwise_model <- stepAIC(visit_nb2, direction="both", trace=TRUE)

visits ~ gender + age + income + illness + reduced + health + 
freepoor + lchronic
visit15_nb <- glm(visits ~ . -privatefreerepat, data = Visits2)
summary(visit15_nb)
plot(visit15_nb)

Visits20 <- Visits
Visits20$nchronic <- NULL
Visits20$private <- NULL
Visits20$freerepat <- NULL
visit_nb20 <- glm.nb(visits ~ ., data = Visits20)
summary(visit_nb20)
plot(visit_nb20)

Visits21 <- Visits
Visits21$nchronic <- NULL
Visits21$private <- NULL
Visits21$freerepat <- NULL
Visits21$income <- NULL
visit_nb21 <- glm.nb(visits ~ ., data = Visits21)
summary(visit_nb21)
plot(visit_nb21)



Visits24$freepoor <- ifelse(Visits24$freepoor == 'yes', 1, 0)
Visits24$gender <- ifelse(Visits24$gender == 'male', 1, 0)

Visits24 <- Visits
Visits24$nchronic <- NULL
Visits24$private <- NULL
Visits24$freerepat <- NULL
Visits24$income <- NULL
Visits24$lchronic <- NULL
cor(Visits24)

Visits22 <- Visits
Visits22$nchronic <- NULL
Visits22$private <- NULL
Visits22$freerepat <- NULL
Visits22$income <- NULL
Visits22$lchronic <- NULL
visit_nb22 <- glm.nb(visits ~ ., data = Visits22)
summary(visit_nb22)
plot(visit_nb22)
visit_pois22 <- glm(visits ~ ., data = Visits22, family = poisson(link="log"))
summary(visit_pois22)
plot(visit_pois22)

visit_poisqp22 <- glm(visits ~ ., data = Visits22, family = quasipoisson(link="log"))
par(mfrow=c(2,2))

lm_model <- lm(visits ~ ., data = Visits22)
vif(lm_model)
summary(lm_model)
library(car)
vif(lm_model) 
vvv <- vif(visit_pois22)
View(vvv)
pchisq(visit_pois22$deviance, visit_pois22$df.residual, lower.tail=F)
gvif(visit_pois22)
exp(-0.200511)

cor(Visits22)




Visits23 <- Visits
Visits23$income <- NULL
Visits23$age <- NULL
Visits23$health <- NULL
Visits23$reduced <- NULL
Visits23$illness <- NULL
visit_pois23 <- glm(visits ~ ., data = Visits23, family = poisson(link="log"))
summary(visit_nb23)
plot(visit_nb23)
vif(visit_pois23)
visit_nb23 <- glm.nb(visits ~ ., data = Visits23)


library(stargazer)
stargazer(visit_pois22, type = "text", out = "my_model_output.txt")

(exp(0.127714)-1)*100
