```{r}
#importing needed library
library (matlib)
library(ggplot2)
library (rsample)
library(glmnet)
library(visdat)
library(MASS)

```

```{r}
# import input data from csv file
shopping_data = as.matrix(read.csv(file="./customer_shopping_data.csv",header = TRUE))
head(shopping_data)
```

```{r}
# checking missing values in dataset
missing_values <- is.na(shopping_data)
missing_values_count <- colSums(missing_values)
missing_values_count
```

```{r}
# convert shopping data into dataframe
shopping_data <- as.data.frame(shopping_data)

unique_age <-unique(shopping_data$age)
print(unique_age)

```

```{r}
# Generate counts for unique ages
age_counts <- table(shopping_data$age)

# Create a data frame with unique ages and their counts
unique_age_with_counts <- data.frame(age = as.numeric(names(age_counts)), count = as.numeric(age_counts))
unique_age_with_counts <- unique_age_with_counts[order(unique_age_with_counts$count, decreasing = TRUE), ]


# View the unique ages with their counts
print(unique_age_with_counts)
```
```{r}
unique_gender <-unique(shopping_data$gender)
print(unique_gender)
```

```{r}
unique_category <-unique(shopping_data$category)
print(unique_category)

```

```{r}
unique_payment_method <- unique(shopping_data$payment_method)
print(unique_payment_method)
```
```{r}
unique_shopping_mall <- unique(shopping_data$shopping_mall)
unique_shopping_mall
```
```{r}
# convert gender to numerical values
shopping_data$gender <- as.numeric(factor(shopping_data$gender,levels = unique(shopping_data$gender)))


# convert category to numerical values
shopping_data$category <- as.numeric(factor(shopping_data$category,levels = unique(shopping_data$category)))

# convert payment method to numerical values
shopping_data$payment_method <- as.numeric(factor(shopping_data$payment_method,levels = unique(shopping_data$payment_method)))

# convert shopping mall to numerical values
shopping_data$shopping_mall <- as.numeric(factor(shopping_data$shopping_mall,levels = unique(shopping_data$shopping_mall)))

tail(shopping_data)
```
```{r}
# define input variable
x <- shopping_data[,!(names(shopping_data) %in% c("invoice_no","customer_id","quantity","gender","invoice_date","shopping_mall"))]
x
```
```{r}
# convert invoice_date to Date format 
shopping_data$invoice_date <- as.Date(shopping_data$invoice_date,format = "%d/%m/%Y")

# create a time series object with monthly frequency(assuming data is monthly)
shopping_data.ts <-ts(x,start = c(as.numeric(format(min(shopping_data$invoice_date),"%Y")),
                                  as.numeric(format(min(shopping_data$invoice_date),"%m"))),
                      end = c(as.numeric(format(max(shopping_data$invoice_date),"%Y")),as.numeric(format(max(shopping_data$invoice_date),"%m"))),frequency = 12)


# plot the time series of input x with one-month interval
plot(shopping_data.ts,main = "Time series plot of Input",xlab = "Invoice Date",ylab = "inputs")
```
```{r}
# task 1.1
# convert invoice_date to Date format 
shopping_data$invoice_date <- as.Date(shopping_data$invoice_date,format = "%d/%m/%Y")

shopping_data$quantity <- as.numeric(shopping_data$quantity)
# extract year and month from invoice date
shopping_data$year_month <- format(shopping_data$invoice_date,"%Y-%m")


# Aggregate quantity by year-month
aggregated_data <- aggregate(quantity ~ year_month, data = shopping_data, sum)


# convert year_month to date format for plotting
aggregated_data$year_month <- as.Date(paste0(aggregated_data$year_month, "-01"))


#create a time series object with monthly frequency
shopping_data.ts <- ts(aggregated_data$quantity, start = c(as.numeric(format(min(aggregated_data$year_month),"%Y")),
                                                           as.numeric(format(min(aggregated_data$year_month),"%m"))),
                       end = c(as.numeric(format(max(aggregated_data$year_month),"%Y")),
                               as.numeric(format(max(aggregated_data$year_month),"%m"))),frequency = 12)
plot(shopping_data.ts,main = "Time series plot of Output(grouped by year-month)",xlab = "year-month",ylab = "Total quantity")

```
```{r}
## Task 1.2
x$price <- as.numeric(x$price)
density_of_price = density(x$price)
plot(density_of_price,main = "Density plot of price")
```
```{r}
# creating  a Histogram of X inputs
hist(x$price,freq = FALSE,main = "Histogram and density plot of price",xlab = "Price")
lines(density_of_price, lwd = 2, col = "black")
rug(jitter(x$price))
```
```{r}
density_of_payment = density(x$payment_method)
plot(density_of_payment,main = "Density plot of whole inputs")
# creating  a Histogram of X inputs
hist(x$payment_method,freq = FALSE,main = "Histogram and density plot of price",xlab = "payment_method")
lines(density_of_payment, lwd = 2, col = "black")
rug(jitter(x$payment_method))
```



```{r}
x$age <- as.numeric(x$age)
density_of_age = density(x$age)

plot(density_of_age,main = "Density plot of whole inputs")
# creating  a Histogram of X inputs
hist(x$age,freq = FALSE,main = "Histogram and density plot of age",xlab = "age")
lines(density_of_age, lwd = 2, col = "black")
rug(jitter(x$age))
```
```{r}
x$category <- as.numeric(x$category)
density_of_category = density(x$category)

plot(density_of_category,main = "Density plot of whole inputs")
# creating  a Histogram of X inputs
hist(x$category,freq = FALSE,main = "Histogram and density plot of age",xlab = "age")
lines(density_of_category, lwd = 2, col = "black")
rug(jitter(x$category))
```
```{r}
density_of_quantity = density(shopping_data$quantity)

plot(density_of_quantity,main = "Density plot of whole inputs")
# creating  a Histogram of X inputs
hist(shopping_data$quantity,freq = FALSE,main = "Histogram and density plot of age",xlab = "age")
lines(density_of_quantity, lwd = 2, col = "black")
rug(jitter(x$quantity))
```
```{r}
# Task 1.3
# plotting age against quantity
Y <- shopping_data$quantity
plot(x$age,Y,main = "Correlation between age and quantity signal",xlab = "age",ylab = "quantity")
plot(x$price,Y,main = "Correlation between age and quantity signal",xlab = "price",ylab = "quantity")
plot(x$category,Y,main = "Correlation between age and quantity signal",xlab = "category",ylab = "quantity")

plot(x$payment_method,Y,main = "Correlation between age and quantity signal",xlab = "payment_method",ylab = "quantity")


```
```{r}
x$quantity <- shopping_data$quantity
cor(x)
plot(x)
```

** Task 2**
```{r}

x$X1 <- x$age
x$X2 <- x$category
x$X3 <- x$price
x$X4 <- x$payment_method

x <- x[,c("X1","X2","X3","X4")]


# Convert x to matrix
x <- as.matrix(x)
y <- as.matrix(shopping_data$quantity)
ones <- matrix(1,length(x)/4,1)
```
```{r}
alpha <- 0 
lambda <- 1
Y1 <- cbind(ones,(x[,"X4"]),(x[,"X1"])^2,(x[,"X1"])^3,(x[,"X2"])^4,(x[,"X1"])^4)
ridge_model1 <- glmnet(Y1,y,alpha = alpha , lambda = lambda)
thetaHatModel1 = coefficients(ridge_model1)
print(thetaHatModel1)

```

```{r}
Y3 <- cbind(ones,(x[,"X3"])^3,(x[,"X3"])^4)
ridge_model3 <- glmnet(Y3,y,alpha = alpha , lambda = lambda)
thetaHatModel3 = coefficients(ridge_model3)
print(thetaHatModel3)

```

```{r}
Y4 <- cbind(ones,(x[,"X2"]),(x[,"X1"])^3,(x[,"X3"])^4)
ridge_model4 <- glmnet(Y4,y,alpha = alpha , lambda = lambda)
thetaHatModel4 = coefficients(ridge_model4)
print(thetaHatModel4)
```
```{r}
Y5 <- cbind(ones,(x[,"X4"]),(x[,"X1"])^2,(x[,"X1"])^3,(x[,"X3"])^4)
ridge_model5 <- glmnet(Y5,y,alpha = alpha , lambda = lambda)
thetaHatModel5 = coefficients(ridge_model5)
print(thetaHatModel5)
```
```{r}
# Task 2.2
Y_hat_ridge1 <- predict(ridge_model1,s = lambda,newx = Y1)
# calculate residuals
residuals_ridge <- y-Y_hat_ridge1

# calculate RSS for the ridge regresssion model
Rss_ridge <- sum(residuals_ridge^2)

# Extract coefficients for the specified lambda
coefficients_ridge <- coef(ridge_model1,s = lambda)

# map coefficients to the corresponding columns of model 1
Y_hat_m1 <- as.matrix(Y1) %*% coefficients_ridge[-1]

# calculate RSS for model1
residuals_m1 <- y-Y_hat_m1
RSS_Model_1 <- sum(residuals_m1^2)
print(RSS_Model_1)
```
```{r}
# Task 2.2
Y_hat_ridge2 <- predict(ridge_model2,s = lambda,newx = Y2)
# calculate residuals
residuals_ridge <- y-Y_hat_ridge2

# calculate RSS for the ridge regresssion model
Rss_ridge <- sum(residuals_ridge^2)

# Extract coefficients for the specified lambda
coefficients_ridge <- coef(ridge_model2,s = lambda)

# map coefficients to the corresponding columns of model 1
Y_hat_m2 <- as.matrix(Y2) %*% coefficients_ridge[-1]

# calculate RSS for model1
residuals_m2 <- y-Y_hat_m2
RSS_Model_2 <- sum(residuals_m2^2)
print(RSS_Model_2)
```
```{r}
# Task 2.2
Y_hat_ridge3 <- predict(ridge_model3,s = lambda,newx = Y3)
# calculate residuals
residuals_ridge <- y-Y_hat_ridge3

# calculate RSS for the ridge regresssion model
Rss_ridge <- sum(residuals_ridge^2)

# Extract coefficients for the specified lambda
coefficients_ridge <- coef(ridge_model3,s = lambda)

# map coefficients to the corresponding columns of model 3
Y_hat_m3 <- as.matrix(Y3) %*% coefficients_ridge[-3]

# calculate RSS for model3
residuals_m3 <- y-Y_hat_m3
RSS_Model_3 <- sum(residuals_m3^2)
print(RSS_Model_3)
```
```{r}
# Task 2.2
Y_hat_ridge4 <- predict(ridge_model4,s = lambda,newx = Y4)
# calculate residuals
residuals_ridge <- y-Y_hat_ridge4

# calculate RSS for the ridge regresssion model
Rss_ridge <- sum(residuals_ridge^2)

# Extract coefficients for the specified lambda
coefficients_ridge <- coef(ridge_model4,s = lambda)

# map coefficients to the corresponding columns of model 4
Y_hat_m4 <- as.matrix(Y4) %*% coefficients_ridge[-4]

# calculate RSS for model4
residuals_m4 <- y-Y_hat_m4
RSS_Model_4 <- sum(residuals_m4^2)
print(RSS_Model_4)
```
```{r}
# Task 2.2
Y_hat_ridge5 <- predict(ridge_model5,s = lambda,newx = Y5)
# calculate residuals
residuals_ridge <- y-Y_hat_ridge5

# calculate RSS for the ridge regresssion model
Rss_ridge <- sum(residuals_ridge^2)

# Extract coefficients for the specified lambda
coefficients_ridge <- coef(ridge_model5,s = lambda)

# map coefficients to the corresponding columns of model 5
Y_hat_m5 <- as.matrix(Y5) %*% coefficients_ridge[-5]

# calculate RSS for model5
residuals_m5 <- y-Y_hat_m5
RSS_Model_5 <- sum(residuals_m5^2)
print(RSS_Model_5)
```
```{r}
# task 2.3
N = length(y)
# calculating the variance of model 1
variance_model1 = RSS_Model_1/(N-1)
likelihood_Model1 = -(N/2)*(log(2*pi)) -(N/2)*(log(variance_model1)) - (1/(2*variance_model1))*RSS_Model_1

# for model 2
variance_model2 = RSS_Model_2/(N-1)
likelihood_Model2 = -(N/2)*(log(2*pi)) -(N/2)*(log(variance_model2)) - (1/(2*variance_model2))*RSS_Model_2

# for model 3
variance_model3 = RSS_Model_3/(N-1)
likelihood_Model3 = -(N/2)*(log(2*pi)) -(N/2)*(log(variance_model3)) - (1/(2*variance_model3))*RSS_Model_3

# for model 4
variance_model4 = RSS_Model_4/(N-1)
likelihood_Model4 = -(N/2)*(log(2*pi)) -(N/2)*(log(variance_model4)) - (1/(2*variance_model4))*RSS_Model_4

# for model 5
variance_model5 = RSS_Model_5/(N-1)
likelihood_Model5 = -(N/2)*(log(2*pi)) -(N/2)*(log(variance_model5)) - (1/(2*variance_model5))*RSS_Model_5


# printing variance value
model1 <- c(variance_model1)
model2 <- c(variance_model2)
model3 <- c(variance_model3)
model4 <- c(variance_model4)
model5 <- c(variance_model4)
dfvariance <- data.frame(model1,model2,model3,model4,model5)
dfvariance



```
```{r}
# printing likelihood value
model1 <- c(likelihood_Model1)
model2 <- c(likelihood_Model2)
model3 <- c(likelihood_Model3)
model4 <- c(likelihood_Model4)
model5 <- c(likelihood_Model5)
dflikelihood <- data.frame(model1,model2,model3,model4,model5)
dflikelihood
```
```{r}
# Task 2.4
# Evaluating AIC and BIC of model 1
K_model1 <- length(thetaHatModel1)
AIC_model1 = 2*K_model1 - 2*likelihood_Model1
BIC_model1 = K_model1 *log(N) -2*likelihood_Model1

# for model 1
K_model2 <- length(thetaHatModel2)
AIC_model2 = 2*K_model2 - 2*likelihood_Model2
BIC_model2 = K_model2 *log(N) -2*likelihood_Model2

# for model 3
K_model3 <- length(thetaHatModel3)
AIC_model3 = 2*K_model3 - 2*likelihood_Model3
BIC_model3 = K_model3 *log(N) -2*likelihood_Model3

# for model 4
K_model4 <- length(thetaHatModel4)
AIC_model4 = 2*K_model4 - 2*likelihood_Model4
BIC_model4 = K_model4 *log(N) -2*likelihood_Model4

# for model 5
K_model5 <- length(thetaHatModel5)
AIC_model5 = 2*K_model5 - 2*likelihood_Model5
BIC_model5 = K_model5 *log(N) -2*likelihood_Model5
```
```{r}
# printing k_model value
model1 <- c(K_model1)
model2 <- c(K_model2)
model3 <- c(K_model3)
model4 <- c(K_model4)
model5 <- c(K_model5)
dfK <- data.frame(model1,model2,model3,model4,model5)
dfK
```
```{r}
model1 <- c(AIC_model1)
model2 <- c(AIC_model2)
model3 <- c(AIC_model3)
model4 <- c(AIC_model4)
model5 <- c(AIC_model5)
dfAIC <- data.frame(model1,model2,model3,model4,model5)
dfAIC
```

```{r}
model1 <- c(BIC_model1)
model2 <- c(BIC_model2)
model3 <- c(BIC_model3)
model4 <- c(BIC_model4)
model5 <- c(BIC_model5)
dfBIC <- data.frame(model1,model2,model3,model4,model5)
dfBIC
```
```{r}
# Task 2.6
model1_error <- y -Y_hat_m1
# plotting the graph QQplot and QQ line of model 1
qqnorm(model1_error,col = "darkcyan",main = "QQ plot of model 1")
qqline(model1_error,col = "red",lwd = 1)

```
```{r}
model2_error <- y -Y_hat_m2
# plotting the graph QQplot and QQ line of model 2
qqnorm(model2_error,col = "darkcyan",main = "QQ plot of model 2")
qqline(model2_error,col = "red",lwd = 1)
```
```{r}
model3_error <- y -Y_hat_m3
# plotting the graph QQplot and QQ line of model 3
qqnorm(model3_error,col = "darkcyan",main = "QQ plot of model 3")
qqline(model3_error,col = "red",lwd = 3)
```

```{r}
model4_error <- y -Y_hat_m4
# plotting the graph QQplot and QQ line of model 4
qqnorm(model4_error,col = "darkcyan",main = "QQ plot of model 4")
qqline(model4_error,col = "red",lwd = 4)
```
```{r}
model5_error <- y -Y_hat_m5
# plotting the graph QQplot and QQ line of model 5
qqnorm(model5_error,col = "darkcyan",main = "QQ plot of model 5")
qqline(model5_error,col = "red",lwd = 5)
```

# Task 2.7
```{r}
# dividing the data into training and testing into 70% and 30%
set.seed(123)
split_X <- initial_split(data = as.data.frame(x),prop = 0.7)
split_Y <- initial_split(data = as.data.frame(y),prop = 0.7)

X_training_set <- training(split_X)
X_testing_set <- testing(split_X)
Y_training_set <- as.matrix(training(split_Y))
Y_testing_set <-as.matrix(testing(split_Y))

# create the design matrix for the selected best model
training_ones <- matrix(1,nrow = nrow(X_training_set),ncol = 1)
X_training_model <- cbind(training_ones,X_training_set[,"X2"],(X_training_set[,"X1"]^3),(X_training_set[,"X3"])^4)

# Fix the typo in the next line
theta_hat <- ginv(t(X_training_model) %*% X_training_model) %*% t(X_training_model) %*% Y_training_set

#create the matrix for the testing data using the same model equation
training_ones_test <- matrix(1,nrow = nrow(X_testing_set),ncol = 1)
X_testing_model <- cbind(training_ones_test,X_testing_set[,"X2"],(X_testing_set[,"X1"])^3,(X_testing_set[,"X3"])^4)

# calculate model predictions on the testing model
Y_testing_hat <- X_testing_model %*% theta_hat


# Evaluating 95% confidence intervals for the model predictions
z <- qnorm(0.975) # z-score for 95% confidence interval
n_len <- nrow(X_testing_model)
error <- Y_testing_set - Y_testing_hat
valid_indices <- (error != 0) # check for non-zero values

# ensure that the values inside sqrt are non-negative using abs function
C_I_1 <-ifelse(valid_indices,z*sqrt(abs(error*(1-error))/n_len),0)

C_I_2 <-ifelse(valid_indices,z*sqrt(abs(error*(1+error))/n_len),0)

# plotting
plot(Y_testing_set,col = "red",pch = 19,xlab = "Index",ylab = "Y value",main = "Model predictions and 95% confidence Intervals")
points(Y_testing_hat,col = "blue",pch = 19)

# Add error bars for 95% confidence intervals
arrows(x0 = 1:n_len,y0 = Y_testing_hat - C_I_1,y1 = Y_testing_hat +C_I_2,angle = 90,code = 3,length = 0.1,col="green")
# legend
legend("topright",legend = c("Testing Data","Model predictions","95% CI"),col = c("red","blue","green"),pch = 19,cex = 0.8)

```
```{r}
# Task 3
# Assuming Y3 is the response variable values predicted by model 3
Y3 <- X_testing_model %*% theta_hat

theta_bias <- 0.448299550
theta_one <- 0.038109255
theta_two <- 0.009827804
theta_four <- 0.002092558
epsilon <- RSS_Model_3 * 2  # assuming RSS_Model_3 is defined

num_iterations <- 100
accepted_values_1 <- numeric(num_iterations)
accepted_values_2 <- numeric(num_iterations)
counter <- 0 

# performing rejections ABC
for (i in 1:num_iterations) {
  range1 <- runif(1, -theta_bias, theta_bias)
  range2 <- runif(1, -theta_one, theta_one)
  
  # Constructing new_theta_hat based on the dimensions of X_testing_model
  new_theta_hat <- c(range1, range2, 0, 0)  # Assuming X_testing_model has 4 columns
  
  new_Y_Hat <- X_testing_model %*% new_theta_hat
  new_RSS <- sum((Y_testing_set - new_Y_Hat) ^ 2)
  
  if (new_RSS > epsilon) {
    accepted_values_1[counter + 1] <- range1
    accepted_values_2[counter + 2] <- range2
    counter <- counter + 1
  }
}

accepted_values_1 <- accepted_values_1[1:counter]
accepted_values_2 <- accepted_values_2[1:counter]

hist(accepted_values_1, main = "Histogram of Accepted values (parameters 1)")
hist(accepted_values_2, main = "Histogram of Accepted values (parameters 2)")

plot(accepted_values_1, accepted_values_2, col = c("green", "red"), 
     main = "Joint and Marginal Posterior Distribution")

```

