Sys.setenv(LANGUAGE="en")
install.packages("jsonlite")
library(jsonlite)
install.packages("randomForest")
library(randomForest)

restaurant <- stream_in(file("yelp_academic_dataset_business.json"))
restaurant$isBuffet <- grepl("buffet", restaurant$categories)

restaurant$cuisine <- "NA"
restaurant$cuisine[grepl("Mexican|mexican|mexico|spanish|Spanish", restaurant$categories)] <- "Mexican"

restaurant$price <- 0
restaurant$price[!is.null(data$attributes[["RestaurantsPriceRange1"]])] <- 1
restaurant$price[!is.null(data$attributes[["RestaurantsPriceRange2"]])] <- 2
restaurant$price[!is.null(data$attributes[["RestaurantsPriceRange3"]])] <- 3

# model 3: random forest for review count
# features: buffet flag, cuisine, city, price, stars, check-ins, review span
# testset : MAE
train3 <- restaurant[restaurant$city != "MAE",]
train3sample = train3[sample(nrow(train3), 200),]
test3 <- restaurant[restaurant$city == "MAE",]
set.seed(30)
rf.restaurant=randomForest(review_count~cuisine+city+price+stars+isBuffet,data=train3,mtry=6,importance=TRUE)
print(rf.restaurant)

predict3 = predict(rf.restaurant, newdata = test3)
plot(test3, predict3)

# model 4: logistic regression for open status
#
#
glm.restaurant = glm(Open~., data=restaurant, family=binomial)
predict4 = predict(glm.restuarant, test4)


# model 5: k-means clustering restaurant profiles
#
#
set.seed(4)
kmeans.restaurant = kmeans()






