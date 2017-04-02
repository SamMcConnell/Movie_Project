dat2 <- read.csv("movies.csv")
dat2 <- na.omit(dat2)
directors$lngross <- log(directors$gross)
directors$lndirector_facebook_likes <- log(directors$director_facebook_likes)
directors$lnactor_1_facebook_likes <- log(directors$actor_1_facebook_likes)
directors$lnnum_voted_users <- log(directors$num_voted_users)
directors$lncast_total_facebook_likes <- log(directors$cast_total_facebook_likes)
directors$lnbudget <- log(directors$budget)
directors$lnnum_users_for_review <- log(directors$num_user_for_reviews)
directors$lnmovie_facebook_likes <- log(directors$movie_facebook_likes)
fit <- lm(lngross ~ lnnum_voted_users + lnnum_users_for_review + lnbudget + imdb_score + content_rating + director_name, data = directors)
fitbackward <- regsubsets(lngross ~ lnnum_voted_users  + lnbudget + lnnum_users_for_review + lnbudget + imdb_score + content_rating + director_name, data = directors, method = "backward")
fitforward <- regsubsets(lngross ~ lnnum_voted_users  + lnbudget + lnnum_users_for_review + lnbudget + imdb_score + content_rating + director_name, data = directors, method = "forward")

# this process can be automated below to get the appropriate statistics for comparison
fitbackward$summary <- summary(fitbackward) # to find cp and adjr2
fitforward$summary <- summary(fitforward) # to find cp and adjr2
which.max(fitbackward$summary$adjr2) #model with max adjr2 value
which.min(fitforward$summary$cp) #model with min adjr2 value

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

# Compare the full model and best model found by regsubsets
fit #full model

fit2 <- fit2 <- lm(lngross ~ lnnum_voted_users  + lnbudget + imdb_score + content_rating + director_name, data = directors)
# removed num_users_for_review, but usually you use your best model against your full model
# find best model using forward, backward or exhaustive methods
summary(fit2)

# Calculate the leave-one-out CV RMSE for the full model
fit.cvrmse <- ls.cvrmse(fit)

# Calculate the leave-one-out CV RMSE for the best model via regsubsets
fit2.cvrmse <- ls.cvrmse(fit2)

print(c(fit.cvrmse, fit2.cvrmse))
# The best model has smaller cvrmse
# fit2 has a smaller cvrmse which is surprising as it has a smaller adj-R2.  But maybe it's due to the NaNs produced

# Two-fold CV, using the Lab8 Technique
n <- nrow(dat)
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
