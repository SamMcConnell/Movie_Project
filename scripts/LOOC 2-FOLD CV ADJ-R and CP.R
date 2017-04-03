data = read.csv("moviedata_sortedby_year.csv", colClasses=c("genres"="character"))
directors <- subset(data, table(data$director_name)[data$director_name] >= 10)
directors <- na.omit(directors)
  
directors$lngross <- log(directors$gross)
directors$lndirector_facebook_likes <- log(directors$director_facebook_likes)
directors$lnactor_1_facebook_likes <- log(directors$actor_1_facebook_likes)
directors$lnnum_voted_users <- log(directors$num_voted_users)
directors$lncast_total_facebook_likes <- log(directors$cast_total_facebook_likes)
directors$lnbudget <- log(directors$budget)
directors$lnnum_users_for_review <- log(directors$num_user_for_reviews)
directors$lnmovie_facebook_likes <- log(directors$movie_facebook_likes)

# cleanup bad data
directors$lnbudget[is.infinite(directors$lnbudget)] <- NA 
directors$lnmovie_facebook_likes[is.infinite(directors$lnmovie_facebook_likes)] <- NA 
directors$lngross[is.infinite(directors$lngross)] <- NA 
directors$lnnum_voted_users[is.infinite(directors$lnnum_voted_users)] <- NA 
directors$lncast_total_facebook_likes[is.infinite(directors$lncast_total_facebook_likes)] <- NA 
directors$lndirector_facebook_likes[is.infinite(directors$lndirector_facebook_likes)] <- NA 
directors$lncast_total_facebook_likes[is.infinite(directors$lncast_total_facebook_likes)] <- NA 
directors$lnactor_1_facebook_likes[is.infinite(directors$lnactor_1_facebook_likes)] <- NA 

directors$lnbudget[is.nan(directors$lnbudget)] <- NA 
directors$lnmovie_facebook_likes[is.nan(directors$lnmovie_facebook_likes)] <- NA 
directors$lngross[is.nan(directors$lngross)] <- NA 
directors$lnnum_voted_users[is.nan(directors$lnnum_voted_users)] <- NA 
directors$lncast_total_facebook_likes[is.nan(directors$lncast_total_facebook_likes)] <- NA 
directors$lndirector_facebook_likes[is.nan(directors$lndirector_facebook_likes)] <- NA 
directors$lncast_total_facebook_likes[is.nan(directors$lncast_total_facebook_likes)] <- NA 
directors$lnactor_1_facebook_likes[is.nan(directors$lnactor_1_facebook_likes)] <- NA 

# create regression model
directors <- na.omit(directors)
fit <- lm(lngross ~ actor_1_name + lncast_total_facebook_likes + lnactor_1_facebook_likes + lndirector_facebook_likes + lnnum_voted_users + lnnum_users_for_review + lnbudget + imdb_score + content_rating + director_name + Action + Adventure + Animation + Biography + Comedy + Crime + Drama + Family + Fantasy + History + Horror + Music + Musical + Mystery + Romance + Thriller + War, data = directors)

# install leaps package and run backward/forward selection
install.packages("leaps")
library(leaps)

# remove categorical variables as not compatible with variable selection algorithm
fitbackward <- regsubsets(lngross ~ lncast_total_facebook_likes + lnactor_1_facebook_likes + lndirector_facebook_likes + lnnum_voted_users + lnnum_users_for_review + lnbudget + imdb_score + duration, data = directors, method = "backward")
fitforward <- regsubsets(lngross ~ lncast_total_facebook_likes + lnactor_1_facebook_likes + lndirector_facebook_likes + lnnum_voted_users + lnnum_users_for_review + lnbudget + imdb_score + duration, data = directors, method = "backward")

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
fit <- lm(lngross ~ lncast_total_facebook_likes + lnactor_1_facebook_likes + lndirector_facebook_likes + lnnum_voted_users + lnnum_users_for_review + lnbudget + imdb_score + duration, data = directors)
#full model

# remove director_facebook_likes and duration due to variable selection process, categoricals removed
fit2 <- lm(lngross ~ lncast_total_facebook_likes + lnactor_1_facebook_likes + lnnum_voted_users + lnnum_users_for_review + lnbudget + imdb_score, data = directors)# removed num_users_for_review, but usually you use your best model against your full model
# find best model using forward, backward or exhaustive methods
summary(fit2)

# Calculate the leave-one-out CV RMSE for the full model, categoricals removed
fit.cvrmse <- ls.cvrmse(fit)

# Calculate the leave-one-out CV RMSE for the best model via regsubsets, categoricals removed
fit2.cvrmse <- ls.cvrmse(fit2)

print(c(fit.cvrmse, fit2.cvrmse))
# The best model has smaller cvrmse
# fit2 has a smaller cvrmse which is surprising as it has a smaller adj-R2.  But maybe it's due to the NaNs produced

# Two-fold CV, using the Lab8 Technique
n <- nrow(directors)
set.seed(1)
id.subset1 <- sort(sample(1:n, round(n/2), replace = FALSE))

# randomly select holdout data
directors.subset1 <- directors[id.subset1,]
directors.subset2 <- directors[-id.subset1,]
fit.subset1 <- lm(lngross ~ lnnum_voted_users + lnnum_users_for_review + lnbudget + imdb_score + content_rating + director_name, data = directors.subset1)

# calculate a prediction based on the holdout set, using the full model
fit.pred1 <- predict(fit.subset1, directors.subset2)
# calculate the error
fit.err1 <- sqrt(sum((directors.subset2$lngross - fit.pred1)^2)/length(fit.pred1))

# calculate a prediction based on the 2nd fit model
fit2.subset1 <- lm(lngross ~ lnnum_voted_users + lnbudget + imdb_score + content_rating + director_name, data = directors.subset1)
fit2.pred1 <- predict(fit2.subset1, directors.subset2)
fit2.err1 <- sqrt(sum((directors.subset2$lngross - fit2.pred1)^2)/length(fit2.pred1))
fit2.err1

# now the error in the second model is worse than the first based on the prediction
# one other option is to create another holdout data set and test again
