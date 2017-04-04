#univariate summary statistics
summary(directors)

# sample corr matrix, excluding non-numeric variables
cor(directors[sapply(directors, is.numeric)])

# Number of directors that have made more than 3 movies
director_table <- table(directors$director_name)
director_table[which(director_table >= 3)]

# Count of movies with a certain rating
content_table <- table(data$content_rating)
content_table

# plots, residuals vs numerical explanatories
par(mfrow=c(2,2))
plot(directors$lnadjgross, fit$summary$residuals)
plot(directors$lnbudget, fit$summary$residuals)
plot(directors$lnactor_1_facebook_likes, fit$summary$residuals)
plot(directors$num_critic_for_reviews, fit$summary$residuals)

plot(directors$lnactor_1_facebook_likes, fit$summary$residuals)
plot(directors$duration, fit$summary$residuals)
plot(directors$imdb_score, fit$summary$residuals)
plot(directors$lndirector_facebook_likes, fit$summary$residuals)

plot(directors$lnmovie_facebook_likes, fit$summary$residuals)
