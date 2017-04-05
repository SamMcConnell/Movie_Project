
par(mfrow=c(4,4))
fit <- lm(lngross ~ actor_1_name + lncast_total_facebook_likes + lnactor_1_facebook_likes + lndirector_facebook_likes + lnnum_voted_users + lnnum_users_for_review + lnbudget + imdb_score + content_rating + director_name + Action + Adventure + Animation + Biography + Comedy + Crime + Drama + Family + Fantasy + History + Horror + Music + Musical + Mystery + Romance + Thriller + War, data = directors)

plot(directors$lncast_total_facebook_likes, fit$residuals, xlab = "ln(Facebook Likes)", ylab = "Residuals")
plot(directors$lnactor_1_facebook_likes, fit$residuals, xlab = "ln(Actor Facebook Likes)", ylab = "Residuals")
plot(directors$lnnum_voted_users, fit$residuals, xlab = "ln(Number Voted Users)", ylab = "Residuals")
plot(directors$lnnum_users_for_review, fit$residuals, xlab = "ln(Num User Reviews)", ylab = "Residuals")
