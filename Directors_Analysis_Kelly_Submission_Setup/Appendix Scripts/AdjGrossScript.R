year <- data$title_year
Len  <- length(year)

for (i in 1:Len) {
  if ( data$title_year[i] >= 1915 | data$title_year[i] <= 1925) {
    data$adjgross[i] <- data$gross[i] * 40
  } else if (data$title_year[i] > 1925 | data$title_year[i] <= 1930) {
    data$adjgross[i] <- data$gross[i] * 35
  } else if (data$title_year[i] > 1930 | data$title_year[i] <= 1935) {
    data$adjgross[i] <- data$gross[i] * 30
  } else if (data$title_year[i] > 1935 | data$title_year[i] <= 1940) {
    data$adjgross[i] <- data$gross[i] * 25
  } else if (data$title_year[i] > 1940 | data$title_year[i] <= 1945) {
    data$adjgross[i] <- data$gross[i] * 21.6
  } else if (data$title_year[i] > 1945 | data$title_year[i] <= 1950) {
    data$adjgross[i] <- data$gross[i] * 14.5
  } else if (data$title_year[i] > 1950 | data$title_year[i] <= 1955) {
    data$adjgross[i] <- data$gross[i] * 11.5
  } else if (data$title_year[i] > 1955 | data$title_year[i] <= 1960) {
    data$adjgross[i] <- data$gross[i] * 10
  } else if (data$title_year[i] > 1960 | data$title_year[i] <= 1965) {
    data$adjgross[i] <- data$gross[i] * 7.5
  } else if (data$title_year[i] > 1965 | data$title_year[i] <= 1970) {
    data$adjgross[i] <- data$gross[i] * 5
  } else if (data$title_year[i] > 1970 | data$title_year[i] <= 1975) {
    data$adjgross[i] <- data$gross[i] * 4
  } else if (data$title_year[i] > 1975 | data$title_year[i] <= 1980) {
    data$adjgross[i] <- data$gross[i] * 3
  } else if (data$title_year[i] > 1980 | data$title_year[i] <= 1985) {
    data$adjgross[i] <- data$gross[i] * 2.5
  } else if (data$title_year[i] > 1985 | data$title_year[i] <= 1990) {
    data$adjgross[i] <- data$gross[i] * 2
  } else if (data$title_year[i] > 1990 | data$title_year[i] <= 1995) {
    data$adjgross[i] <- data$gross[i] * 1.8
  } else if (data$title_year[i] > 1995 | data$title_year[i] <= 2000) {
    data$adjgross[i] <- data$gross[i] * 1.5
  } else if (data$title_year[i] > 2000 | data$title_year[i] <= 2005) {
    data$adjgross[i] <- data$gross[i] * 1.3
  } else if (data$title_year[i] > 2005 | data$title_year[i] <= 2010) {
    data$adjgross[i] <- data$gross[i] * 1.1
  } else {
    data$adjgross[i] <- data$gross[i]
  }
}