#HW4 Lab 3
#Valery Delgado
#Econometrics and Statistics 
#John Robison and Kseniia Huseinova
#September 23, 2022

#For this data I was trying to analyze for each borough based on their household income the amount of units in strucutre owned along with owncost of maintaining different properties.

load("/Users/valerydelgado/Downloads/acs2017_ny_data.RData")
dat_NYC <- subset(acs2017_ny, (acs2017_ny$in_NYC == 1)&(acs2017_ny$AGE > 21) & (acs2017_ny$AGE < 35))
attach(dat_NYC)
borough_f <- factor((in_Bronx + 2*in_Manhattan + 3*in_StatenI + 4*in_Brooklyn + 5*in_Queens), levels=c(1,2,3,4,5),labels = c("Bronx","Manhattan","Staten Island","Brooklyn","Queens"))

norm_varb <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
}

is.na(HHINCOME) <- which(HHINCOME == 9999999) 
norm_HHINCOME <- norm_varb(HHINCOME)
norm_OWNCOST <- norm_varb(OWNCOST)
norm_UNITSSTR <- norm_varb(UNITSSTR)

summary(norm_HHINCOME)
summary(norm_UNITSSTR)
summary(norm_OWNCOST)

prop.table(summary(norm_HHINCOME))
prop.table(summary(norm_UNITSSTR))
prop.table(summary(norm_OWNCOST))

xtabs(~UNITSSTR+borough_f)
#In terms of Manhattan there are 166 units that are not in structure while the Staten Island has 36 which are 10 units in structure.Therefore, this could mean that in the other boroughs it could be newer developments in apartments and condos to increase the real estate of the area.


owner_ship <-   HHINCOME+(OWNCOST*UNITSSTR)
norm_owner_ship <- norm_varb(owner_ship)
data_use_prelim<- cbind(norm_HHINCOME,norm_UNITSSTR, norm_OWNCOST)
data_use_prelim <- data.frame(data_use_prelim)
good_obs_data_use <- complete.cases(data_use_prelim,borough_f)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(borough_f,good_obs_data_use)

set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]

View(cl_data)
summary(cl_data)
prop.table(summary(cl_data))
summary(train_data)
require(class)
for (indx in seq(1, 15, by= 2)) {
  pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_borough == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}

cl_data_n <- as.numeric(cl_data)

model_ols1 <- lm(cl_data_n ~ train_data$norm_HHINCOME + train_data$norm_UNITSSTR + train_data$norm_OWNCOST)

y_hat <- fitted.values(model_ols1)

mean(y_hat[cl_data_n == 1])
mean(y_hat[cl_data_n == 2])
mean(y_hat[cl_data_n == 3])
mean(y_hat[cl_data_n == 4])
mean(y_hat[cl_data_n == 5])


cl_data_n1 <- as.numeric(cl_data_n == 1)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$norm_HHINCOME + train_data$norm_UNITSSTR + train_data$norm_OWNCOST)
y_hat_v1 <- fitted.values(model_ols_v1)
mean(y_hat_v1[cl_data_n1 == 1])
mean(y_hat_v1[cl_data_n1 == 0])