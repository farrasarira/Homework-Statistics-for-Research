library(readxl)
library(tidyverse)
library(moments)

# import dataset
my_data <- read_excel("database.xlsx")

# get body height and typing speed data
dataset_A <- as.numeric(my_data$body_height)
dataset_B <- as.numeric(my_data$typing_speed)

# Desriptive Statistics of dataset_A
mean_A <- mean(dataset_A)
print(mean_A)
median_A <- median(dataset_A)
print(median_A)
min_A <- min(dataset_A)
print(min_A)
max_A <- max(dataset_A)
print(max_A)
var_A <- var(dataset_A)
print(sqrt(var_A))
skew_A <- skewness(dataset_A)
print(skew_A)

print("-------------------------------------------")

# Desriptive Statistics of dataset_B
mean_B <- mean(dataset_B)
print(mean_B)
median_B <- median(dataset_B)
print(median_B)
min_B <- min(dataset_B)
print(min_B)
max_B <- max(dataset_B)
print(max_B)
var_B <- var(dataset_B)
print(sqrt(var_B))
skew_B <- skewness(dataset_B)
print(skew_B)

# Visualization of the distribution of the data
# hist(dataset_A, xlab='body height(cm)')
hist(dataset_B, xlab='typing speed(WPM)',breaks=10)

# boxplot(dataset_A, ylab='body height(cm)')
# boxplot(dataset_B, ylab='typing speed(WPM)')

# print("-------------------------------------------")

# calculate quartiles, quantiles
print(" Quartile : ")
print( quantile(dataset_A, c(0.25,0.5,0.75)) )
print( quantile(dataset_B, c(0.25,0.5,0.75)) )
print(" Quantile : ")
print( quantile(dataset_A, c(0.1,0.2,0.8,0.9)) )
print( quantile(dataset_B, c(0.1,0.2,0.8,0.9)) )

print("-------------------------------------------")

# Comparison of body height
# average of indonesian body height : 158 cm https://en.tempo.co/read/1699087/indonesians-ranked-first-in-the-worlds-shortest-people
# average of global population body height : 168.5 cm https://www.dryukselyurttas.com/post/average-height-men-and-women#:~:text=On%20a%20global%20scale%2C%20the,4%20inches%20(162%20cm).
tresult_indo <- t.test(dataset_A, mu=158, alternative="two.sided")
print(tresult_indo)
print("+++++++++++++++++++++++++++++++++++++++++++")
tresult_global <- t.test(dataset_A, mu=168.5, alternative="two.sided")
print(tresult_global)

# comparison of typing speed
# mean typing speed of all the people who took the test : 40.426
print("*******************************************")
tresult_global <- t.test(dataset_B, mu=40.426, alternative="two.sided")
print(tresult_global)


print("-------------------------------------------")

# correlation between typing speed and body height
print("correlation of typing speed and body height:")
print(cor(dataset_B,dataset_A))
plot(dataset_B,dataset_A, xlab='typing speed(WPM)', ylab='body height(cm)')

# test significant different between the typing speed of requler and non-reguler class
my_data$class_type=='Regular'
regu <- my_data[my_data$class_type=='Regular',]
nonregu =  my_data[my_data$class_type !='Regular',]
dataset_B_regu = as.numeric(regu$typing_speed) 
print (dataset_B_regu)
print("################################")
dataset_B_nonregu = as.numeric(nonregu$typing_speed)
print(dataset_B_nonregu)

t2result = t.test(dataset_B_regu, dataset_B_nonregu, conf.level=0.95)
print("^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
print(t2result)








