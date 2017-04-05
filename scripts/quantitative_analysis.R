#setwd("./Movie_Project")
#install.packages("leaps")
#library(leaps)

dat = read.csv("data/moviedata_sortedby_year_split_genre.csv", stringsAsFactors = FALSE)

# omit bad data with na.omit
options(na.action=na.omit)
options(max.print=99999)


# Adjust for inflation
year <- dat$title_year
Len  <- length(year)

for (i in 1:Len) {
  if ( dat$title_year[i] >= 1915 & dat$title_year[i] <= 1925) {
    dat$adjgross[i] <- dat$gross[i] * 40
  } else if (dat$title_year[i] > 1925 & dat$title_year[i] <= 1930) {
    dat$adjgross[i] <- dat$gross[i] * 35
  } else if (dat$title_year[i] > 1930 & dat$title_year[i] <= 1935) {
    dat$adjgross[i] <- dat$gross[i] * 30
  } else if (dat$title_year[i] > 1935 & dat$title_year[i] <= 1940) {
    dat$adjgross[i] <- dat$gross[i] * 25
  } else if (dat$title_year[i] > 1940 & dat$title_year[i] <= 1945) {
    dat$adjgross[i] <- dat$gross[i] * 21.6
  } else if (dat$title_year[i] > 1945 & dat$title_year[i] <= 1950) {
    dat$adjgross[i] <- dat$gross[i] * 14.5
  } else if (dat$title_year[i] > 1950 & dat$title_year[i] <= 1955) {
    dat$adjgross[i] <- dat$gross[i] * 11.5
  } else if (dat$title_year[i] > 1955 & dat$title_year[i] <= 1960) {
    dat$adjgross[i] <- dat$gross[i] * 10
  } else if (dat$title_year[i] > 1960 & dat$title_year[i] <= 1965) {
    dat$adjgross[i] <- dat$gross[i] * 7.5
  } else if (dat$title_year[i] > 1965 & dat$title_year[i] <= 1970) {
    dat$adjgross[i] <- dat$gross[i] * 5
  } else if (dat$title_year[i] > 1970 & dat$title_year[i] <= 1975) {
    dat$adjgross[i] <- dat$gross[i] * 4
  } else if (dat$title_year[i] > 1975 & dat$title_year[i] <= 1980) {
    dat$adjgross[i] <- dat$gross[i] * 3
  } else if (dat$title_year[i] > 1980 & dat$title_year[i] <= 1985) {
    dat$adjgross[i] <- dat$gross[i] * 2.5
  } else if (dat$title_year[i] > 1985 & dat$title_year[i] <= 1990) {
    dat$adjgross[i] <- dat$gross[i] * 2
  } else if (dat$title_year[i] > 1990 & dat$title_year[i] <= 1995) {
    dat$adjgross[i] <- dat$gross[i] * 1.8
  } else if (dat$title_year[i] > 1995 & dat$title_year[i] <= 2000) {
    dat$adjgross[i] <- dat$gross[i] * 1.5
  } else if (dat$title_year[i] > 2000 & dat$title_year[i] <= 2005) {
    dat$adjgross[i] <- dat$gross[i] * 1.3
  } else if (dat$title_year[i] > 2005 & dat$title_year[i] <= 2010) {
    dat$adjgross[i] <- dat$gross[i] * 1.1
  } else {
    dat$adjgross[i] <- dat$gross[i]
  }
}

# Use these lines to filter out by country and language
dat<-dat[(dat$country=="USA"| dat$country=="UK" | dat$country=="Canada" | 
            dat$country=="New Zealand" | dat$country=="Australia" | dat$country=="Ireland" |
            (dat$country=="Germany" & dat$language=="English") | (dat$country=="France" & dat$language=="English") |
            (dat$country=="Italy" & dat$language=="English") | (dat$country=="Spain" & dat$language=="English")),]


# get rid of categorical variables and gross for this analysis
drops <- c("gross", "color", "director_name", "actor_2_name", "genres", "primary_genre", "actor_1_name", "movie_title", "actor_3_name", "plot_keywords", "movie_imdb_link", "language", "country", "content_rating", "facenumber_in_poster", "title_year", "aspect_ratio")
dat <- dat[ , !(names(dat) %in% drops)]

# creating regression
dat$adjgross <- log(dat$adjgross/10000000)                          # scale adjusted gross by 10 million dollars and log
dat$num_critic_for_reviews <- log(dat$num_critic_for_reviews)       # log number of critic reviews
dat$director_facebook_likes <- log(dat$director_facebook_likes)     # log nunber of director likes
dat$actor_3_facebook_likes <- log(dat$actor_3_facebook_likes)       # log number of 3rd top billing actor likes
dat$actor_1_facebook_likes <- log(dat$actor_1_facebook_likes)       # log number of 1st top billing actor likes
dat$num_voted_users <- log(dat$num_voted_users)                     # log number of user votes
dat$cast_total_facebook_likes <- log(dat$cast_total_facebook_likes) # log number of total cast likes
dat$num_user_for_reviews <- log(dat$num_user_for_reviews)           # log number of user reviews
dat$budget <- log(dat$budget)                                       # log budget
dat$actor_2_facebook_likes <- log(dat$actor_2_facebook_likes)       # log number of 2nd top billing actor likes
dat$movie_facebook_likes <- log(dat$movie_facebook_likes)           # log number of facebook likes

dat <- do.call(data.frame,lapply(dat, function(x) replace(x, is.infinite(x),NA)))
dat <- na.omit(dat)

reg_quant <- lm(adjgross ~., data = dat, na.action = na.omit)
s_quant <- summary(reg_quant)

# check which model works best with forward selection  (best fit is 8|10 variables)
s.fwd <- regsubsets(adjgross ~., data=dat, method="forward", nvmax = 20)
ss.fwd <- summary(s.fwd)
which.min(ss.fwd$cp)     # 8 variables for best model
which.max(ss.fwd$adjr2)  # 10 variables for best model

# check which model works best with backward selection   (best fit is 8|10 variables)
s.bwd <- regsubsets(adjgross ~., data=dat, method="backward", nvmax = 20)
ss.bwd <- summary(s.bwd)
which.min(ss.bwd$cp)     # 8 variables for best model
which.max(ss.bwd$adjr2)  # 10 variables for best model

# the 12 explanatory variables to make the best model as identified by forward/backward selection
keep <- c("adjgross", "num_critic_for_reviews", "director_facebook_likes", "actor_1_facebook_likes", "num_voted_users", "cast_total_facebook_likes", "num_user_for_reviews", "budget", "imdb_score")

# generate a new regression for the best model
datBest <- dat[keep]
regBest <- lm(adjgross ~., data = datBest)
sBest <- summary(regBest)

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

n <- nrow(datBest)
set.seed(1)
# here select half of the data randomly. 
index.subset1 <- sort(sample(1:n, round(n/2), replace = FALSE))

# Initially subset1 is the training set and subset2 is the hold-out set 
quant.subset1 <- dat[index.subset1,]
quant.subset2 <- dat[-index.subset1,] 

quantFull.subset1 <- lm(adjgross~., data = quant.subset1)

# Make predictions at the hold-out data set.
quantFull.pred1 <- predict(quantFull.subset1, quant.subset2)
quantFull.err1 <- sqrt(sum((quant.subset2$adjgross - quantFull.pred1)^2)/length(quantFull.pred1))

# Compare to the selected model.
quantBest.subset1 <- lm(adjgross~., data = datBest[index.subset1,])
quantBest.pred1 <- predict(quantBest.subset1, datBest[-index.subset1,])
quantBest.err1 <- sqrt(sum((datBest[-index.subset1,]$adjgross - quantBest.pred1)^2)/length(quantBest.pred1))


# ~~~~~~~~~~~~~~~~~~~~~ SUMMARY OF ANALYSIS ~~~~~~~~~~~~~~~~~~~~~ #

sBest                 # Summary statistics of the best model

sBest$adj.r.squared   # adjusted r squared of the best model
s_quant$adj.r.squared # adjusted r squared of the full model

sBest$sigma           # residual SD of the best model
s_quant$sigma         # residual SD of the full model

ls.cvrmse(regBest)    # leave-one-out cross validation of the best model
ls.cvrmse(reg_quant)  # leave-one-out cross validation of the full model

quantBest.err1        # rmse prediction of the training/holdout sets of the best model
quantFull.err1        # rmse prediction of the training/holdout sets of the full model




















