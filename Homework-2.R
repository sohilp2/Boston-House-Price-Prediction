install.packages("usdm")
install.packages("car")
install.packages("MASS")
install.packages("DAAG")
install.packages("lmtest")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("GGally")
installed.packages("car")

library(usdm) # for testing collinearity 
library(car) # for testing outliers
library(MASS) # for testing studentized residuals
library(DAAG) # for cross validation of model
library(lmtest) # for checking homoskedasticity/heteroskedasticity
library(ggplot2) # Use for visuals
library(ggfortify)
library(GGally)
library(car)
 
attach(Boston)

data1 <- data.frame(Boston)
bst.df <- data1[-c(496:506),]
test.df <- data1[c(496:506),]


# Since rad seems to be an ordinal variable we add levels 
# just to be safe!    
logMedv <- log(bst.df$medv)
colnames(logMedv) <- c("logMedv")
bst.df$logMedv <- logMedv
bst.df$logMedv <- as.numeric(bst.df$logMedv)

str(bst.df)

pairs <- ggpairs(bst.df, columns = c(1, 2, 4, 6, 7, 8,9,11, 15), 
                 lower=list(continuous=wrap("smooth",
                                            colour="turquoise4")),
                 diag=list(continuous=wrap("barDiag",
                                           fill="turquoise4")))  + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray"))
pairs


bst.df$logMedv<-NULL

logMedv <- data.frame(log(bst.df$medv))
test_log <- as.numeric(log(test.df$medv))

# Next we change the name of the column to make it easy
# for us to call in our ggplot2 visual
colnames(logMedv) <- c("logMedv")

logMedvHist <- ggplot(logMedv, aes(logMedv))
logMedvHist + geom_histogram(aes(y=..density.., fill=..count..), 
                             colour = 'white', bins = 25) + 
  geom_density() +
  scale_fill_gradient("Count", low = "black", 
                      high = "turquoise4") +
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray")) + 
  ggtitle("Histogram of log(MEDV)")
#Model 1: All
fit1 <- lm(formula = log(medv)~ crim+zn+chas+rm+age+dis+rad+ptratio, data=bst.df)
mse_test_value <- mean((test_log - predict(fit1,test.df))^2)
mse_test_value 

summary(fit1)
bptest(fit1)
attr(fit1_CV, 'ms')
vif(fit1)
plot(fit1)  

#model 2: Remove collinear
fit2 <- update(fit1,~crim+zn+chas+dis+rm+age+rad+ptratio)
mse_test_value <- mean((test_log - predict(fit2,test.df))^2)
mse_test_value
summary(fit2)
vif(fit2)

#model 3: remove statiscally insignificant zn and rad
fit3 <- update(fit2,~crim+chas+dis+rm+age+ptratio)
mse_test_value <- mean((test_log - predict(fit3,test.df))^2)
mse_test_value
summary(fit3)
vif(fit3)
#outlier test 
outlierTest(fit3, cutoff = Inf, n.max = 15)
plot(fit3)
cooksd= cooks.distance(fit3)
plot(cooksd, pch="*", cex=0.5, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""),cex=0.7, col="red")  # add labels
bst2.df <- bst.df[-c(369,372,366,373,368,417,400,420,370,401,402,490,371,408),]

#model 4
fit4 <- lm(log(medv) ~ crim+chas+rm+age+ptratio+dis, data = bst2.df)
mse_test_value <- mean((test_log - predict(fit4,test.df))^2)
mse_test_value
summary(fit4)
plot(fit4)


#checking for non linearity
residBar <- ggplot(data=fit4, aes(residuals(fit4))) + 
  geom_histogram(aes(y =..density..), 
                 col="black", fill="white") +
  geom_density(col=1) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  ggtitle("Histogram of Residuals for fit4") 
residBar

#model 5 Adding non linear term
fit5 <- lm(log(medv) ~ crim+chas+rm+dis+age+ptratio+ I(crim^2)+ I(rm^2), data = bst2.df)
summary(fit5)
mse_test_value <- mean((test_log - predict(fit5,test.df))^2)
mse_test_value

#model 6 Adding more terms
#fit6 <- lm(log(medv) ~ crim+chas+dis+rm+age+ptratio+ I(crim^2)+I(crim^3)+ I(rm^2)+ I(rm^3), data = bst2.df)
#summary(fit6)

ncvTest(fit5)
crPlots(fit5)
#validation
par(mfrow = c(1,1))
fit5_CV <- suppressWarnings(CVlm(data = bst2.df, 
                                 form.lm = formula(fit5), 
                                 m = 10, 
                                 dots = FALSE, 
                                 seed = 1, 
                                 plotit = c("Observed", "Residual"),
                                 main = "Cross-validation for fit4", 
                                 legend.pos="topleft"))
fit1_CV <- suppressWarnings(CVlm(data = bst.df, 
                                 form.lm = formula(fit1), 
                                 m = 10, 
                                 dots = FALSE, 
                                 seed = 1, 
                                 plotit = c("Observed", "Residual"),
                                 main = "Cross-validation for fit1", 
                                 legend.pos="topleft"))

attr(fit5_CV, 'ms')
attr(fit1_CV, 'ms')
