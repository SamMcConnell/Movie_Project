setwd("~/Documents/STAT306/Project Data")
#read data from unsorted csv file
mydata = read.csv("movie_metadata 2.csv")
mydata
# read data from csv file sorted by year
newdata = read.csv("moviedata_sortedby_year.csv")
newdata
# create subset of first 10 movies for year 2016
ten.data <- newdata[c(1:10),]
ten.data
# create subset for first 100 movies by year
onehundered.data <- newdata[c(1:100),]
# create subset for 1043 movies from the start of 2012 onwards
from2012.data <- newdata[c(1:1043),]
# install.packages("leaps")
# library(leaps)
r.ten.data <- lm(gross~ duration + budget + imdb_score , data=ten.data)
summary(r.ten.data)
r.onehundered.data <- lm(gross~ duration + budget + imdb_score , data=onehundered.data)
summary(r.onehundered.data)

r.from2012 <- lm(gross~ duration + budget + imdb_score , data=from2012.data)
summary(r.from2012)

y = -6.181e+08 + (-9.877e+05*183) + (2.652e+00*2.50e+08) + (6.314e+07*6.9)


# r.short1 <- regsubsets(gross~., data=ten.data, method="exhaustive")
# summary(r.short1)
# r.short1


# r1 <- lm(gross~num_critic_for_reviews + duration + director_facebook_likes + actor_3_facebook_likes +
#                     actor_1_facebook_likes + gross + num_voted_users + cast_total_facebook_likes + 
#                     facenumber_in_poster + num_user_for_reviews + budget + actor_2_facebook_likes + 
#                     imdb_score + aspect_ratio + movie_facebook_likes, data=mydata)
# summary(r1)
# r1 <- regsubsets(gross~., data=mydata, method="exhaustive")
# r1 <- summary(s1)
# r1

