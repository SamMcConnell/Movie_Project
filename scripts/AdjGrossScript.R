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