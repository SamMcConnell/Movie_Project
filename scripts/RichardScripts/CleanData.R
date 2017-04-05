#first clean up the data
library(readr)
dat <- read_csv("~/R/movie_metadata.csv/moviedata_sortedby_year.csv")
data<-dat[(dat$country=="USA"| dat$country=="UK" | dat$country=="Canada" | 
             dat$country=="New Zealand" | dat$country=="Australia" | dat$country=="Ireland" |
             (dat$country=="Germany" & dat$language=="English") | (dat$country=="France" & dat$language=="English") |
             (dat$country=="Italy" & dat$language=="English") | (dat$country=="Spain" & dat$language=="English")),]
setwd(".")    # <--- trying to avoid direct path as we all have different setups
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
  if ( dat$title_year[i] >= 1915 | dat$title_year[i] <= 1925) {
    dat$adjgross[i] <- dat$gross[i] * 40
  } else if (dat$title_year[i] > 1925 | dat$title_year[i] <= 1930) {
    dat$adjgross[i] <- dat$gross[i] * 35
  } else if (dat$title_year[i] > 1930 | dat$title_year[i] <= 1935) {
    dat$adjgross[i] <- dat$gross[i] * 30
  } else if (dat$title_year[i] > 1935 | dat$title_year[i] <= 1940) {
    dat$adjgross[i] <- dat$gross[i] * 25
  } else if (dat$title_year[i] > 1940 | dat$title_year[i] <= 1945) {
    dat$adjgross[i] <- dat$gross[i] * 21.6
  } else if (dat$title_year[i] > 1945 | dat$title_year[i] <= 1950) {
    dat$adjgross[i] <- dat$gross[i] * 14.5
  } else if (dat$title_year[i] > 1950 | dat$title_year[i] <= 1955) {
    dat$adjgross[i] <- dat$gross[i] * 11.5
  } else if (dat$title_year[i] > 1955 | dat$title_year[i] <= 1960) {
    dat$adjgross[i] <- dat$gross[i] * 10
  } else if (dat$title_year[i] > 1960 | dat$title_year[i] <= 1965) {
    dat$adjgross[i] <- dat$gross[i] * 7.5
  } else if (dat$title_year[i] > 1965 | dat$title_year[i] <= 1970) {
    dat$adjgross[i] <- dat$gross[i] * 5
  } else if (dat$title_year[i] > 1970 | dat$title_year[i] <= 1975) {
    dat$adjgross[i] <- dat$gross[i] * 4
  } else if (dat$title_year[i] > 1975 | dat$title_year[i] <= 1980) {
    dat$adjgross[i] <- dat$gross[i] * 3
  } else if (dat$title_year[i] > 1980 | dat$title_year[i] <= 1985) {
    dat$adjgross[i] <- dat$gross[i] * 2.5
  } else if (dat$title_year[i] > 1985 | dat$title_year[i] <= 1990) {
    dat$adjgross[i] <- dat$gross[i] * 2
  } else if (dat$title_year[i] > 1990 | dat$title_year[i] <= 1995) {
    dat$adjgross[i] <- dat$gross[i] * 1.8
  } else if (dat$title_year[i] > 1995 | dat$title_year[i] <= 2000) {
    dat$adjgross[i] <- dat$gross[i] * 1.5
  } else if (dat$title_year[i] > 2000 | dat$title_year[i] <= 2005) {
    dat$adjgross[i] <- dat$gross[i] * 1.3
  } else if (dat$title_year[i] > 2005 | dat$title_year[i] <= 2010) {
    dat$adjgross[i] <- dat$gross[i] * 1.1
  } else {
    dat$adjgross[i] <- dat$gross[i]
  }
} 

# data cleaned 