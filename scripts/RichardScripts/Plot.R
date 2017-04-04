movie_actor<-na.omit(dat)
#only picks movies data with main actor who shows up in more than 20 movies
ac1<-subset(movie_actor, table(actor_1_name)[actor_1_name] >= 20)
ac1<-na.omit(ac1)

#take log for big data
ac1$logadjgross<-log(ac1$adjgross)
ac1$logactor_1_facebook_likes<-log(ac1$actor_1_facebook_likes)
ac1$logcast_total_facebook_likes<-log(ac1$cast_total_facebook_likes)
ac1$lognum_voted_users<-log(ac1$num_voted_users)
ac1$logbudget<-log(ac1$budget)
ac1$lognum_user_for_reviews<-log(ac1$num_user_for_reviews)
ac1$logdirector_facebook_likes<-log(ac1$director_facebook_likes)

# fit full model with actor_name as catagory variables
fit <- lm(logadjgross ~logactor_1_facebook_likes+logcast_total_facebook_likes
          +lognum_voted_users+num_critic_for_reviews+logbudget+lognum_user_for_reviews+imdb_score+actor_1_name,data=ac1)
s1<-summary(fit)

par(mfrow=c(2,4))
plot(ac1$logbudget,s1$residuals,xlab="logbudget",ylab="residual")
plot(ac1$imdb_score,s1$residuals,xlab="imdb_score",ylab="residual")
plot(ac1$num_critic_for_reviews,s1$residuals,xlab="num_critic_for_reviews",ylab="residual")
plot(ac1$lognum_voted_users,s1$residuals,xlab="lognum_voted_users",ylab="residual")
plot(ac1$lognum_user_for_reviews,s1$residuals,xlab="lognum_user_for_reviews",ylab="residual")
plot(ac1$logactor_1_facebook_likes,s1$residuals,xlab="logactor_1_facebook_likes",ylab="residual")
plot(ac1$logcast_total_facebook_likes,s1$residuals,xlab="logcast_total_facebook_likes",ylab="residual")

#box plot
par(mfrow=c(1,1))
boxplot(ac1$logadjgross ~ ac1$actor_1_name, main="actor_1_name", xlab = "actor_1_name", ylab = "ln(Gross Revenue, USD)")

# with movie type, but doesn't make much sense
fit2 <- lm(logadjgross ~num_critic_for_reviews+logcast_total_facebook_likes + logactor_1_facebook_likes 
           + lognum_voted_users + lognum_user_for_reviews + logbudget + 
             imdb_score + content_rating + Action + Adventure + Animation + 
             Biography + Comedy + Crime + Drama + Family + Fantasy + History + Horror + Music +
             Musical + Mystery + Romance + Thriller + War, data = ac1)

s2<-summary(fit2)