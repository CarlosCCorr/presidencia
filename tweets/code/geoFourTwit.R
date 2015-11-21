## Código utilizado para realizar consultas a  Twitter y a foursquare.
## Librerías utilizadas.
suppressPackageStartupMessages(library(twitteR))
suppressPackageStartupMessages(library(RJSONIO))
suppressPackageStartupMessages(library(RCurl))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(streamR))
## ---------------------------------------
## ---------------------------------------
## Coordenadas de interés.
## Lado superior derecho:
upper.right <- c(19.543909,-99.018358)
## Lado inferior izquierdo:
lower.left  <- c(19.480806,-99.124617)
## ---------------------------------------
## ---------------------------------------
## --------------FOURSQUARE---------------
## ---------------------------------------
## ---------------------------------------
## OAuth para foursquare
options(RCurlOptions =
            list(cainfo =
                     system.file("CurlSSL",
                                 "cacert.pem",
                                 package = "RCurl")))
## Datos de cuenta
clientid     <- "AOYBQE5QJBG0CEH4F0KDGPBZLE2KRJ22FD2Q3NI4XVYMU22S"
clientsecret <- "TAI4KVCWYFRZNIIIOY1FKN4RHZOZCAETU2RIZTBV0GL5V424"
## Búsqueda
lat    <- c(upper.right[1],lower.left[1])
long   <- c(upper.right[2],lower.left[2])
radius <- 1000
limit  <- 50
query  <- ""
query  <- paste0("https://api.foursquare.com/v2/venues/explore?client_id=",
                 clientid,
                 "&client_secret=",
                 clientsecret,
                 "&ll=",
                 lat[1],
                 ",",
                 long[1],
                 "&query=",query,"&v=20130815&radius=",radius,"&limit=",limit)
result   <- getURL(query)
data     <- fromJSON(result)
response <- data$response
places   <- response$groups[[1]]$items
## Volver los resultados un data.frame
test <- ldply( places,
              function(t){t <- unlist(t$venue)
                          t <- t[ names(t) %in%
                                     c("name",
                                       "location.lat",
                                       "location.lng",
                                       "location.country",
                                       "categories.name",
                                       "stats.checkinsCount",
                                       "stats.usersCount",
                                       "hereNow.summary"
                                       )]
                          t
                      }
              )
## ---------------------------------------
## ---------------------------------------
## ---------------TWITTER-----------------
## ---------------------------------------
## ---------------------------------------
## OAuth para Twitter.
access_token        <-
    "235347211-WPBkwLTonKCus2LbIvZm7WJkoUXhwkiKRQn4ONQH"
access_token_secret <-
    "81vTk2pu1Cc424Wx7GcD5MrCNq1zY50K7Ese2D954yy2S"
consumer_key        <-
    "jvIzU7EpCOVHSpKZwIihQweFC"
consumer_secret     <-
    "RtObIJuzJX7fwhHVafLGu3yHsdSacaornwAfddnCAuYWD9O8uh"
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_token_secret)

data.tweet <- searchTwitter("JaimeRdzNL",
                           n = 15000)
##                           geocode='25.657394, -100.295553, 100mi')

## Convertir a data.frame
data.frame.tweet   <- twListToDF(data.tweet)

## Escribir resultados
write.csv(data.frame.tweet,
          './data/queryTweet.csv',
          row.names = FALSE)

############################################################
## Twitter Jalisco
############################################################
library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
my_oauth <- OAuthFactory$new(consumerKey=consumer_key,
consumerSecret=consumer_secret, requestURL=requestURL,
accessURL=accessURL, authURL=authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

filterStream(file="tweets_jal.json",
             locations=c( -105.847962, 18.385231,  -103.105679, 21.950384),
             track="Patricia",oauth=my_oauth)

