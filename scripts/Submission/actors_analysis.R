#install.packages("leaps")
library(leaps)
#forward and backward selection for full model without catagory variable 
fitbackward <- regsubsets(logadjgross ~logactor_1_facebook_likes+logcast_total_facebook_likes+duration
                          +lognum_voted_users+num_critic_for_reviews+logbudget+lognum_user_for_reviews+imdb_score, data = ac1, method = "backward")
fitforward <- regsubsets(logadjgross ~logactor_1_facebook_likes+logcast_total_facebook_likes+duration
                         +lognum_voted_users+num_critic_for_reviews+logbudget+lognum_user_for_reviews+imdb_score, data = ac1, method = "forward")

fitbackward$summary <- summary(fitbackward)
fitforward$summary <- summary(fitforward)

#check adj and cp to choose the best model, adj suggests that full model is the best, but cp suggests model with four variable is better
#fit both models suggested by adj and cp value
which.max(fitbackward$summary$adjr2)
which.min(fitbackward$summary$cp)
which.max(fitforward$summary$adjr2)
which.min(fitforward$summary$cp)

fitbest<-lm(logadjgross ~duration +lognum_voted_users+num_critic_for_reviews+logbudget+lognum_user_for_reviews+imdb_score, data = ac1)
fitfull <- lm(logadjgross ~logactor_1_facebook_likes+logcast_total_facebook_likes+duration
             +lognum_voted_users+num_critic_for_reviews+logbudget+lognum_user_for_reviews+imdb_score, data = ac1)
#summary
summaryfull<-summary(fitfull)
summarybest<-summary(fitbest)


ls.cvrmse <- function(ls.out)
{
  res.cv <- ls.out$residuals / (1.0 - ls.diag(ls.out)$hat)
  # Identify NA's and remove them.
  is.na.res <- is.na(res.cv)
  res.cv <- res.cv[!is.na.res]
  cvrmse <- sqrt(sum(res.cv^2) / length(res.cv))
  return(cvrmse)
}
cat("\nCross-validation root mean square error using ls.cvrmse function\n")
cvrmsebest<-ls.cvrmse(fitbest); cvrmsefull<-ls.cvrmse(fitfull)
cat(cvrmsebest,cvrmsefull,"\n") 


#For each of the 2 models, we do 3 random holdout prediction to get the 
#Cross-validation root mean square error with holdout,here we add 
#back the catagory variables (actor_1_name)

# for the full model 
n <- nrow(ac1)
set.seed(1)
id.subset1 <- sort(sample(1:n, round(n/2), replace = FALSE))

# randomly select holdout data
ac1.subset1 <- ac1[id.subset1,]
ac1.subset2 <- ac1[-id.subset1,]
#here we add back the category variables
fit.subseta <- lm(logadjgross ~logactor_1_facebook_likes+logcast_total_facebook_likes+duration
                  +lognum_voted_users+num_critic_for_reviews+logbudget+lognum_user_for_reviews+imdb_score+actor_1_name, data = ac1.subset1)

# calculate a prediction based on the holdout set, using the full model
fit.preda <- predict(fit.subseta, ac1.subset2)
# calculate the error
fit.erra <- sqrt(sum((ac1.subset2$logadjgross - fit.preda)^2)/length(fit.preda))
fit.erra

#repeat for 3 times
id.subset3 <- sort(sample(1:n, round(n/2), replace = FALSE))
ac1.subset3 <- ac1[id.subset3,]
ac1.subset4 <- ac1[-id.subset3,]
fit.subsetb <- lm(logadjgross ~logactor_1_facebook_likes+logcast_total_facebook_likes+duration
                  +lognum_voted_users+num_critic_for_reviews+logbudget+lognum_user_for_reviews+imdb_score+actor_1_name, data = ac1.subset3)

fit.predb <- predict(fit.subsetb, ac1.subset4)
# calculate the error
fit.errb <- sqrt(sum((ac1.subset4$logadjgross - fit.predb)^2)/length(fit.predb))
fit.errb

#third time
id.subset5 <- sort(sample(1:n, round(n/2), replace = FALSE))
ac1.subset5 <- ac1[id.subset5,]
ac1.subset6 <- ac1[-id.subset5,]
fit.subsetc <- lm(logadjgross ~logactor_1_facebook_likes+logcast_total_facebook_likes+duration
                  +lognum_voted_users+num_critic_for_reviews+logbudget+lognum_user_for_reviews+imdb_score+actor_1_name, data = ac1.subset5)

fit.predc <- predict(fit.subsetc, ac1.subset6)
fit.errc <- sqrt(sum((ac1.subset6$logadjgross - fit.predc)^2)/length(fit.predc))
fit.errc

# do this again for 6 variable model and the category variables
id.subset1 <- sort(sample(1:n, round(n/2), replace = FALSE))

# randomly select holdout data
ac1.subset1 <- ac1[id.subset1,]
ac1.subset2 <- ac1[-id.subset1,]
#here we add back the category variables
fit.subseta <- lm(logadjgross ~duration +lognum_voted_users+num_critic_for_reviews+logbudget+lognum_user_for_reviews+imdb_score+actor_1_name, data = ac1.subset1)

# calculate a prediction based on the holdout set, using the full model
fit.preda <- predict(fit.subseta, ac1.subset2)
# calculate the error
fit.errd <- sqrt(sum((ac1.subset2$logadjgross - fit.preda)^2)/length(fit.preda))
fit.errd

#repeat for 3 times
id.subset3 <- sort(sample(1:n, round(n/2), replace = FALSE))
ac1.subset3 <- ac1[id.subset3,]
ac1.subset4 <- ac1[-id.subset3,]
fit.subsetb <- lm(logadjgross ~duration +lognum_voted_users+num_critic_for_reviews+logbudget+lognum_user_for_reviews+imdb_score+actor_1_name, data = ac1.subset3)

fit.predb <- predict(fit.subsetb, ac1.subset4)
# calculate the error
fit.erre <- sqrt(sum((ac1.subset4$logadjgross - fit.predb)^2)/length(fit.predb))
fit.erre

#third time
id.subset5 <- sort(sample(1:n, round(n/2), replace = FALSE))
ac1.subset5 <- ac1[id.subset5,]
ac1.subset6 <- ac1[-id.subset5,]
fit.subsetc <- lm(logadjgross ~duration +lognum_voted_users+num_critic_for_reviews+logbudget+lognum_user_for_reviews+imdb_score+actor_1_name, data = ac1.subset5)

fit.predc <- predict(fit.subsetc, ac1.subset6)
fit.errf <- sqrt(sum((ac1.subset6$logadjgross - fit.predc)^2)/length(fit.predc))
fit.errf



#get the residual SD and adjusted R2 
SD1<-summaryfull$sigma
SD2<-summarybest$sigma
adj1<-summaryfull$adj.r.squared
adj2<-summarybest$adj.r.squared



# build table 
trial <- matrix(c(adj1,SD1,cvrmsefull,fit.erra,fit.errb,fit.errc,adj2,SD2,cvrmsebest,fit.errd,fit.erre,fit.errf), ncol=2)
colnames(trial) <- c("full model", "fit by cp")
rownames(trial) <- c("adjusted R2","residual SD","rmsepred(leave-one-out)","rmsepred(train/holdout) 1",
                     "rmsepred(train/holdout) 2","rmsepred(train/holdout) 3")
trial.table <- as.table(trial)
trial.table

# Model with 4 explanatory variables is better based on leave-one-out

fitbest2<-lm(logadjgross ~duration +lognum_voted_users+num_critic_for_reviews+logbudget+lognum_user_for_reviews+imdb_score+actor_1_name+content_rating, data = ac1)
summary(fitbest2)
ac2<-ac1[,c(3,4,26,48,49,50,51,52,53)]
summary(ac2)
