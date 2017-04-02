dat$lngross <- log(dat$gross)
dat$lnnum_critic_reviews <- log(dat$num_critic_for_reviews)
dat$lnnum_voted_users <- log(dat$num_voted_users)

# make plots
par(mfrow=c(2,2))

# num_voted_users vs gross
dat$lnnum_voted_users <- log(dat$num_voted_users)
plot(dat$num_critic_for_reviews, log(dat$gross))
plot(log(dat$num_critic_for_reviews), log(dat$gross))

# num_critic_reviews vs gross
plot(dat$num_critic_for_reviews, dat$gross)
plot(dat$num_critic_for_reviews, log(dat$gross))
plot(log(dat$num_critic_for_reviews), log(dat$gross))

# duration vs gross
par(mfrow=c(1,2))
plot(dat$duration, log(dat$gross))
plot(log(dat$duration), log(dat$gross))

# budget vs gross
par(mfrow=c(2,2))
plot(dat$budget, dat$gross)
plot(dat$budget, log(dat$gross))
plot(log(dat$budget), log(dat$gross))

# imdb_score vs gross
par(mfrow=c(1,2))
plot(dat$imdb_score, dat$gross)
plot(dat$imdb_score, log(dat$gross))

# Content Rating vs lngross
par(mfrow=c(1,1))
boxplot(dat$lngross ~ dat$content_rating, main="Content Rating", xlab = "Content Rating", ylab = "ln(Gross Revenue, USD)")

