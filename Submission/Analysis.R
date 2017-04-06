#setwd("./Movie_Project")
install.packages("leaps")
library(leaps)

dat = read.csv("moviedata_sortedby_year.csv", stringsAsFactors = FALSE)

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


# ~~~~~~~~~~~~~~~~~~~~~ DIRECTOR ANALYSIS ~~~~~~~~~~~~~~~~~~~~~ #

data = read.csv("moviedata_sortedby_year.csv", colClasses=c("genres"="character"))
# add adjgross to data
for (i in 1:Len) {
    if ( data$title_year[i] >= 1915 & data$title_year[i] <= 1925) {
        data$adjgross[i] <- data$gross[i] * 40
    } else if (data$title_year[i] > 1925 & data$title_year[i] <= 1930) {
        data$adjgross[i] <- data$gross[i] * 35
    } else if (data$title_year[i] > 1930 & data$title_year[i] <= 1935) {
        data$adjgross[i] <- data$gross[i] * 30
    } else if (data$title_year[i] > 1935 & data$title_year[i] <= 1940) {
        data$adjgross[i] <- data$gross[i] * 25
    } else if (data$title_year[i] > 1940 & data$title_year[i] <= 1945) {
        data$adjgross[i] <- data$gross[i] * 21.6
    } else if (data$title_year[i] > 1945 & data$title_year[i] <= 1950) {
        data$adjgross[i] <- data$gross[i] * 14.5
    } else if (data$title_year[i] > 1950 & data$title_year[i] <= 1955) {
        data$adjgross[i] <- data$gross[i] * 11.5
    } else if (data$title_year[i] > 1955 & data$title_year[i] <= 1960) {
        data$adjgross[i] <- data$gross[i] * 10
    } else if (data$title_year[i] > 1960 & data$title_year[i] <= 1965) {
        data$adjgross[i] <- data$gross[i] * 7.5
    } else if (data$title_year[i] > 1965 & data$title_year[i] <= 1970) {
        data$adjgross[i] <- data$gross[i] * 5
    } else if (data$title_year[i] > 1970 & data$title_year[i] <= 1975) {
        data$adjgross[i] <- data$gross[i] * 4
    } else if (data$title_year[i] > 1975 & data$title_year[i] <= 1980) {
        data$adjgross[i] <- data$gross[i] * 3
    } else if (data$title_year[i] > 1980 & data$title_year[i] <= 1985) {
        data$adjgross[i] <- data$gross[i] * 2.5
    } else if (data$title_year[i] > 1985 & data$title_year[i] <= 1990) {
        data$adjgross[i] <- data$gross[i] * 2
    } else if (data$title_year[i] > 1990 & data$title_year[i] <= 1995) {
        data$adjgross[i] <- data$gross[i] * 1.8
    } else if (data$title_year[i] > 1995 & data$title_year[i] <= 2000) {
        data$adjgross[i] <- data$gross[i] * 1.5
    } else if (data$title_year[i] > 2000 & data$title_year[i] <= 2005) {
        data$adjgross[i] <- data$gross[i] * 1.3
    } else if (data$title_year[i] > 2005 & data$title_year[i] <= 2010) {
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

# ~~~~~~~~~~~~~~~~~~~~~ SUMMARY OF ANALYSIS ~~~~~~~~~~~~~~~~~~~~~ #

fullModel <- summary(fit)
secondBest <- summary(fit2)

print(c(fitA.cvrmse, fit2A.cvrmse))

# calculate the error
fit.err1
fit2.err1

# ~~~~~~~~~~~~~~~~~~~~~ ACTOR'S ANALYSIS ~~~~~~~~~~~~~~~~~~~~~ #

#first clean up the data
library(readr)
dat <- read_csv("moviedata_sortedby_year.csv")
data<-dat[(dat$country=="USA"| dat$country=="UK" | dat$country=="Canada" | 
             dat$country=="New Zealand" | dat$country=="Australia" | dat$country=="Ireland" |
             (dat$country=="Germany" & dat$language=="English") | (dat$country=="France" & dat$language=="English") |
             (dat$country=="Italy" & dat$language=="English") | (dat$country=="Spain" & dat$language=="English")),]
#setwd(".")    # <--- trying to avoid direct path as we all have different setups
#install.packages('plyr')
#install.packages("stringr", repos='http://cran.us.r-project.org')
library(stringr)
library(plyr)
#read data from csv file sorted by year, with primary genres selected

options(na.action=na.omit)
options(max.print=99999)
#produces frewuency table output for content rating as seen on page 110:
count(data,"content_rating")

#create a vector that includes genre in the form "Action|Comedy|etc." for use in a function used to count frequency of genres 

genrevector <- data$genres 
countAction = 0; countAdventure = 0; countAnimation = 0; countBiography = 0; countComedy = 0;
countCrime = 0; countDocumentary = 0; countDrama = 0; countFamily = 0; 
countFantasy = 0; countFilmNoir = 0; countGameShow = 0; countWar=0;
countHistory = 0; countHorror = 0; countMusic = 0; countMusical = 0; countMystery = 0;
countRomance = 0; countSciFi = 0; countThriller = 0; countWestern = 0; countSport = 0; countNews = 0; countRealityTV = 0;

#code to count frequency of genres
for (i in genrevector ) {
  if (grepl(i, "Action")) 
    countAction = countAction + 1;
  if (grepl(i, "Adventure")) 
    countAdventure = countAdventure + 1;
  if (grepl(i, "Animation")) 
    countAnimation = countAnimation + 1;
  if (grepl(i, "Biography")) 
    countBiography = countBiography + 1;
  if (grepl(i, "Comedy")) 
    countComedy = countComedy + 1;
  if (grepl(i, "Crime")) 
    countCrime = countCrime + 1;
  if (grepl(i, "Documentary")) 
    countDocumentary = countDocumentary + 1;
  if (grepl(i, "Drama")) 
    countDrama = countDrama + 1;
  if (grepl(i, "Family")) 
    countFamily = countFamily + 1;
  if (grepl(i, "Fantasy")) 
    countFantasy = countFantasy + 1;
  if (grepl(i, "Film-Noir")) 
    countFilmNoir = countFilmNoir + 1;
  if (grepl(i, "Game-Show")) 
    countGameShow = countGameShow + 1;
  if (grepl(i, "History")) 
    countHistory = countHistory + 1;
  if (grepl(i, "Horror")) 
    countHorror = countHorror + 1;
  if (grepl(i, "Music")) 
    countMusic = countMusic + 1;
  if (grepl(i, "Musical")) 
    countMusical = countMusical + 1;
  if (grepl(i, "Mystery")) 
    countMystery = countMystery + 1;
  if (grepl(i, "Romance")) 
    countRomance = countRomance + 1;
  if (grepl(i, "Sci-Fi")) 
    countSciFi = countSciFi + 1;
  if (grepl(i, "Thriller")) 
    countThriller = countThriller + 1;
  if (grepl(i, "Western")) 
    countWestern = countWestern + 1;
  if (grepl(i, "War")) 
    countWar = countWar + 1;
  if (grepl(i, "Sport")) 
    countSport = countSport + 1;
  if (grepl(i, "News")) 
    countNews = countNews + 1;
  if (grepl(i, "Reality-TV")) 
    countRealityTV = countRealityTV + 1;
}
countAction; countAdventure; countAnimation; countBiography; countComedy ;
countCrime ; countDocumentary ; countDrama ; countFamily ; 
countFantasy ; countFilmNoir ; countGameShow ;
countHistory ; countHorror ; countMusic ; countMusical ; countMystery ;
countRomance ; countSciFi ; countThriller ; countWestern ; countWar; countSport; countNews; countRealityTV;

# Create binary vector for each genre
# Make vectors for:
#   - Action, Adventure, Animation, Biography, Comedy, Crime, Drama, Family, Fantasy, History, Horror, Music, Musical, Mystery, Romance, Sci-fi, Thriller, War

# Given a genre and a the genres of a movie, check if a movie contains it, if it does return 1, 0 otherwise
hasGenre <- function(genre, movieGenres) {
  regExp <- paste("(^|\\|)", genre, "(\\||$)", sep = "")
  if(grepl(regExp, movieGenres)){
    return(1)
  } else {
    return(0)
  }
}

# Given a genre and a vector of the genres of various movies, produce a vector of TRUE/FALSE for movies that contain/don't contain the given genre
moviesHaveGenre <- function(genre_, genreVector) {
  return(sapply(genreVector, hasGenre, genre = genre_, USE.NAMES = FALSE))
}

commonGenres <- c("Action", "Adventure", "Animation", "Biography", "Comedy", "Crime", "Drama", "Family", "Fantasy", "History", "Horror", "Music", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War")

# Given a genre, create vector of movies that have or do not have the genre (1|0), and cbind to the dataframe

for (genre in commonGenres) {
  gVector <- moviesHaveGenre(genre, data$genres)
  data[genre] <- gVector
}


dat<-data.frame(data)
dat<-na.omit(dat)

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

# data cleaned 

movie_actor<-na.omit(dat)
#only picks movies data with main actor who shows up in more than 20 movies
ac1<-subset(movie_actor, table(actor_1_name)[actor_1_name] >= 25)
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

# ~~~~~~~~~~~~~~~~~~~~~ SUMMARY OF ANALYSIS ~~~~~~~~~~~~~~~~~~~~~ #

summaryfull #summary(fitfull)
summarybest #summary(fitbest)
SD1 #summaryfull$sigma
SD2 #summarybest$sigma
adj1 #summaryfull$adj.r.squared
adj2 #summarybest$adj.r.squared

cvrmsebest

# calculate the error, training holdout
fit.erra
fit.errb

fit.errc
fit.errd

fit.erre
fit.errf






