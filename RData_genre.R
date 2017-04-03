setwd(".")    # <--- trying to avoid direct path as we all have different setups

# summary statistics

#install.packages('plyr')
#install.packages("stringr", repos='http://cran.us.r-project.org')
library(stringr)
library(plyr)
#read data from csv file sorted by year, with primary genres selected
data = read.csv("data/moviedata_sortedby_year_split_genre.csv", colClasses=c("genres"="character"))
data

options(na.action=na.omit)
options(max.print=99999)
#produces frequency table output for content rating as seen on page 110:
count(data,"content_rating")
table(data$primary_genre)
count(data$primary_genre)

#create a vector that includes genre in the form "Action|Comedy|etc." for use in a function used to count frequency of genres 

genrevector <- data$genres 
countAction = 0; countAdventure = 0; countAnimation = 0; countBiography = 0; countComedy = 0;
countCrime = 0; countDocumentary = 0; countDrama = 0; countFamily = 0; 
countFantasy = 0; countFilmNoir = 0; countGameShow = 0;
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

#substet data frame for num_critic_reviews, duration, gross, num_voted_users, content_rating, budget, imdb_score
dataSubSetByColumns <- data[,c(3,4,9,14,23,24,27)]
# print out summary
summary(dataSubSetByColumns)

#substet data frame for num_critic_reviews, duration, gross, num_voted_users, budget, imdb_score
dataSubSetByNumeric <- data[,c(3,4,9,14,24,27)]
pairs(dataSubSetByNumeric)
#produces correlation matrix for all numerical data items that contain all items
print(cor(dataSubSetByNumeric, use="complete.obs"))

#summary statistics for total data set, may want to get frequency tables in a nicer form
summary(data)
#check the number of factors in a vector
# factor(obj)
# count(obj) counts numers of specific items
# table(obj) presents data in a vector in table form

# create subset of first 10 movies for year 2016
tenMovies <- data[c(1:10),]
#summary statistics for small set
summary(tenMovies)
#subset 10 row with specific columns selected
tenMoviesSubSet <- tenMovies[,c(2,3,4,9,11,12,14,23,24,27)]
tenMoviesSubSetOnlyNumeric <- tenMovies[,c(3,4,9,14,24,27)]
tenMoviesSubSetOnlyNumeric
summary(tenMoviesSubSetOnlyNumeric)
print(summary(tenMoviesSubSetOnlyNumeric))

# create subset for first 100 movies by year
oneHundredMovies <- data[c(1:100),]
# create subset for 1043 movies from the start of 2012 onwards
from2012 <- data[c(1:1043),]
