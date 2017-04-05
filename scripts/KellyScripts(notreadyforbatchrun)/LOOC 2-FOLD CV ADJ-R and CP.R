data = read.csv("moviedata_sortedby_year.csv", colClasses=c("genres"="character"))
# add adjgross to data
for (i in 1:Len) {
  if ( data$title_year[i] >= 1915 | data$title_year[i] <= 1925) {
    data$adjgross[i] <- data$gross[i] * 40
  } else if (data$title_year[i] > 1925 | data$title_year[i] <= 1930) {
    data$adjgross[i] <- data$gross[i] * 35
  } else if (data$title_year[i] > 1930 | data$title_year[i] <= 1935) {
    data$adjgross[i] <- data$gross[i] * 30
  } else if (data$title_year[i] > 1935 | data$title_year[i] <= 1940) {
    data$adjgross[i] <- data$gross[i] * 25
  } else if (data$title_year[i] > 1940 | data$title_year[i] <= 1945) {
    data$adjgross[i] <- data$gross[i] * 21.6
  } else if (data$title_year[i] > 1945 | data$title_year[i] <= 1950) {
    data$adjgross[i] <- data$gross[i] * 14.5
  } else if (data$title_year[i] > 1950 | data$title_year[i] <= 1955) {
    data$adjgross[i] <- data$gross[i] * 11.5
  } else if (data$title_year[i] > 1955 | data$title_year[i] <= 1960) {
    data$adjgross[i] <- data$gross[i] * 10
  } else if (data$title_year[i] > 1960 | data$title_year[i] <= 1965) {
    data$adjgross[i] <- data$gross[i] * 7.5
  } else if (data$title_year[i] > 1965 | data$title_year[i] <= 1970) {
    data$adjgross[i] <- data$gross[i] * 5
  } else if (data$title_year[i] > 1970 | data$title_year[i] <= 1975) {
    data$adjgross[i] <- data$gross[i] * 4
  } else if (data$title_year[i] > 1975 | data$title_year[i] <= 1980) {
    data$adjgross[i] <- data$gross[i] * 3
  } else if (data$title_year[i] > 1980 | data$title_year[i] <= 1985) {
    data$adjgross[i] <- data$gross[i] * 2.5
  } else if (data$title_year[i] > 1985 | data$title_year[i] <= 1990) {
    data$adjgross[i] <- data$gross[i] * 2
  } else if (data$title_year[i] > 1990 | data$title_year[i] <= 1995) {
    data$adjgross[i] <- data$gross[i] * 1.8
  } else if (data$title_year[i] > 1995 | data$title_year[i] <= 2000) {
    data$adjgross[i] <- data$gross[i] * 1.5
  } else if (data$title_year[i] > 2000 | data$title_year[i] <= 2005) {
    data$adjgross[i] <- data$gross[i] * 1.3
  } else if (data$title_year[i] > 2005 | data$title_year[i] <= 2010) {
    data$adjgross[i] <- data$gross[i] * 1.1
  } else {
    data$adjgross[i] <- data$gross[i]
  }
}

directors <- subset(data, table(data$director_name)[data$director_name] >= 10)
  
directors$lngross <- log(directors$gross)
directors$lnadjgross <- log(directors$adjgross)
directors$lndirector_facebook_likes <- log(directors$director_facebook_likes)
directors$lnactor_1_facebook_likes <- log(directors$actor_1_facebook_likes)
directors$lnnum_voted_users <- log(directors$num_voted_users)
directors$lncast_total_facebook_likes <- log(directors$cast_total_facebook_likes)
directors$lnbudget <- log(directors$budget)
directors$lnnum_users_for_review <- log(directors$num_user_for_reviews)
directors$lnmovie_facebook_likes <- log(directors$movie_facebook_likes)

# cleanup bad data
directors$lnbudget[is.infinite(directors$lnbudget)] <- NA 
directors$lnadjgross[is.infinite(directors$adjgross)] <- NA
directors$lnmovie_facebook_likes[is.infinite(directors$lnmovie_facebook_likes)] <- NA 
directors$lngross[is.infinite(directors$lngross)] <- NA 
directors$lnnum_voted_users[is.infinite(directors$lnnum_voted_users)] <- NA 
directors$lncast_total_facebook_likes[is.infinite(directors$lncast_total_facebook_likes)] <- NA 
directors$lndirector_facebook_likes[is.infinite(directors$lndirector_facebook_likes)] <- NA 
directors$lncast_total_facebook_likes[is.infinite(directors$lncast_total_facebook_likes)] <- NA 
directors$lnactor_1_facebook_likes[is.infinite(directors$lnactor_1_facebook_likes)] <- NA 

directors$lnbudget[is.nan(directors$lnbudget)] <- NA 
directors$lnadjgross[is.nan(directors$adjgross)] <- NA
directors$lnmovie_facebook_likes[is.nan(directors$lnmovie_facebook_likes)] <- NA 
directors$lngross[is.nan(directors$lngross)] <- NA 
directors$lnnum_voted_users[is.nan(directors$lnnum_voted_users)] <- NA 
directors$lncast_total_facebook_likes[is.nan(directors$lncast_total_facebook_likes)] <- NA 
directors$lndirector_facebook_likes[is.nan(directors$lndirector_facebook_likes)] <- NA 
directors$lncast_total_facebook_likes[is.nan(directors$lncast_total_facebook_likes)] <- NA 
directors$lnactor_1_facebook_likes[is.nan(directors$lnactor_1_facebook_likes)] <- NA 

# create regression model
directors <- na.omit(directors)

fit <- lm(lnadjgross ~ lncast_total_facebook_likes + lnactor_1_facebook_likes + lndirector_facebook_likes + lnnum_voted_users + lnnum_users_for_review + lnbudget + imdb_score + content_rating + duration + director_name, data = directors)

# install leaps package and run backward/forward selection
install.packages("leaps")
library(leaps)

# remove categorical variables as not compatible with variable selection algorithm
fitbackward <- regsubsets(lnadjgross ~ lncast_total_facebook_likes + lnactor_1_facebook_likes + lndirector_facebook_likes + lnnum_voted_users + lnnum_users_for_review + lnbudget + imdb_score + duration, data = directors, method = "backward")
fitforward <- regsubsets(lnadjgross ~ lncast_total_facebook_likes + lnactor_1_facebook_likes + lndirector_facebook_likes + lnnum_voted_users + lnnum_users_for_review + lnbudget + imdb_score + duration, data = directors, method = "backward")

# this process can be automated below to get the appropriate statistics for comparison
fitbackward$summary <- summary(fitbackward) # to find cp and adjr2
fitforward$summary <- summary(fitforward) # to find cp and adjr2
maxadjr2fitbackward <- which.max(fitbackward$summary$adjr2) #model with max adjr2 value
minfitforwardcp <- which.min(fitforward$summary$cp) #model with min cp value

# calculate leave-one-out CV error
ls.cvrmse <- function(ls.out)
  # Compute the leave-one-out cross-validated root mean squared error of prediction.
  # Handles missing values.
  # ls.out is a fitted regression model from lsfit or lm.
  # (c) Copyright William J. Welch 1997
{
  res.cv <- ls.out$residuals / (1.0 - ls.diag(ls.out)$hat)
  # Identify NA's and remove them.
  is.na.res <- is.na(res.cv)
  res.cv <- res.cv[!is.na.res]
  cvrmse <- sqrt(sum(res.cv^2) / length(res.cv))
  return(cvrmse)
}

# Compare the full model and best model found by regsubsets, categoricals removed
fit <- lm(lnadjgross ~ lncast_total_facebook_likes + lnactor_1_facebook_likes + lndirector_facebook_likes + lnnum_voted_users + lnnum_users_for_review + lnbudget + imdb_score + content_rating + duration + director_name, data = directors)
#full model

# remove director_facebook_likes and duration due to variable selection process, categoricals removed
fit2 <- lm(lnadjgross ~ lnactor_1_facebook_likes + lnnum_voted_users + lnnum_users_for_review + lnbudget + imdb_score + content_rating + director_name, data = directors)# removed num_users_for_review, but usually you use your best model against your full model# find best model using forward, backward or exhaustive methods
summary(fit2)

# Compare the full model and best model found by regsubsets, categoricals removed
fitA <- lm(lnadjgross ~ lncast_total_facebook_likes + lnactor_1_facebook_likes + lndirector_facebook_likes + lnnum_voted_users + lnnum_users_for_review + lnbudget + imdb_score + duration, data = directors)
#full model

# remove director_facebook_likes and duration due to variable selection process, categoricals removed
fit2A <- lm(lnadjgross ~ lnactor_1_facebook_likes + lnnum_voted_users + lnnum_users_for_review + lnbudget + imdb_score, data = directors)# removed num_users_for_review, but usually you use your best model against your full model# find best model using forward, backward or exhaustive methods
summary(fit2)

# Calculate the leave-one-out CV RMSE for the full model, categoricals removed
fitA.cvrmse <- ls.cvrmse(fitA)

# Calculate the leave-one-out CV RMSE for the best model via regsubsets, categoricals removed
fit2A.cvrmse <- ls.cvrmse(fit2A)

print(c(fitA.cvrmse, fit2A.cvrmse))
# The best model has smaller cvrmse
# fit2 has a smaller cvrmse which is surprising as it has a smaller adj-R2.  But maybe it's due to the NaNs produced

# Two-fold CV, using the Lab8 Technique
n <- nrow(directors)
set.seed(1)
id.subset1 <- sort(sample(1:n, round(n/2), replace = FALSE))

# randomly select holdout data
directors.subset1 <- directors[id.subset1,]
directors.subset2 <- directors[-id.subset1,]
fit.subset1 <- lm(lnadjgross ~ lncast_total_facebook_likes + lnactor_1_facebook_likes + lndirector_facebook_likes + lnnum_voted_users + lnnum_users_for_review + lnbudget + imdb_score + content_rating + duration + director_name, data = directors.subset1)

# calculate a prediction based on the holdout set, using the full model
fit.pred1 <- predict(fit.subset1, data = directors.subset2)
# calculate the error
fit.err1 <- sqrt(sum((directors.subset2$lnadjgross - fit.pred1)^2)/length(fit.pred1))

# calculate a prediction based on the 2nd fit model
fit2.subset1 <- lm(lnadjgross ~ lnactor_1_facebook_likes + lnnum_voted_users + lnnum_users_for_review + lnbudget + imdb_score + content_rating + director_name, data = directors.subset1)
fit2.pred1 <- predict(fit2.subset1, directors.subset2)
fit2.err1 <- sqrt(sum((directors.subset2$lnadjgross - fit2.pred1)^2)/length(fit2.pred1))
fit2.err1

# now the error in the second model is worse than the first based on the prediction
# one other option is to create another holdout data set and test again
