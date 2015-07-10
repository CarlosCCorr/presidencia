                                        # En este código se tienen las funciones que llevan a cabo el proces de extracción y procesamiento de datos de la web.
###########################
                                        # Librerías utilizadas
suppressPackageStartupMessages(library(XML))
suppressPackageStartupMessages(library(RCurl))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(rjson))
suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(wordcloud))
suppressPackageStartupMessages(library(shinyBS))
suppressPackageStartupMessages(library(ggmap))
suppressPackageStartupMessages(library(googleVis))
suppressPackageStartupMessages(library(twitteR))
suppressPackageStartupMessages(library(Rfacebook))
suppressPackageStartupMessages(library(ProgGUIinR))
                                        #--------------------------------------------------
options(httr_oauth_cache=T)
###########################
## Función que limpia palabras de paro de un párrafo.
cleanText <- function(text){
    text <- str_trim(text) %>%
        removeWords(stopwords("spanish"))
    tolower(text)
}

## Función que obtiene las ligas y párrafos de una página web especificada
minLinksText <- function(webPage){
    result <- list()
    ## Obtener la página// encoding utf-8
    page <-
        getURL(webPage, .encoding = 'UTF-8')
    ## Pasar a R
    tree <-
        htmlParse(page)
    ## Obtener todas las ligas
    links <-
        xpathApply(tree, path = "//*/a",
                   fun = xmlGetAttr,
                   name = "href") %>%
            unlist()
    links <- links[str_length(links) > 5]
    ## Obtener párrafos
    paragraphs <-
        xpathApply(tree, path= "//*/p",
                   fun = xmlValue) %>%
            unlist()
    paragraphs <- paragraphs[str_length(paragraphs)>20]
    ## Limpiar párrafos
    paragraphs <- aaply(paragraphs,1,function(t)t<-cleanText(t))
    ## Unificar resultados en una sola lista
    result[[1]] <- links
    result[[2]] <- paragraphs
    result
}
## Función que extrae recursivamente las ligas y párrafos de una página especificada
minLinksRec <- function(webPage, connectLink, connectPara){
    res   <- minLinksText(webPage)
    links <- res[[1]][str_detect(res[[1]],"http")]
    writeLines(links, con = connectLink, sep = "\n")
    writeLines(res[[2]], con = connectPara, sep = "\n")
    for(i in links){
        res <- try(minLinksRec(i, connectLink, connectPara))
        if(inherits(res, "try-error")){
            print(paste("error en lectura",i,sep=" "))
        }        
    }
}
## Función que extrae entidades de un párrafo
entities <- function(text){
    names <- str_match_all(text,"( *[[:upper:]]]+[[:alpha:]]{3,} *[[:upper:]]+[[:alpha:]]{3,} *[[:upper:]]+[[:alpha:]]{3,} *| *[[:upper:]]+[[:alpha:]]{3,} *[[:upper:]]+[[:alpha:]]{3,} *| *[[:upper:]]+[[:alpha:]]{3,} *)")
    names <- names[[1]][str_length(names[[1]][,1])>=3,1]
    names
}

## Función que busca un caracter y regresa el párrafo que lo contiene
locateWord <- function(textList, word){
    textList <- llply(textList, function(t)t <- tolower(t))
    laply(textList,function(t)t <- str_detect(t,tolower(word)))
}
## Función que genera arreglo con las N palabras más frecuentes asociadas a cada búsqueda
asocWords <- function(textList, word, N=10){
    words <- laply(textList[locateWord(textList,word)],function(t)t<-str_split(t," "))
    words <- unlist(words)
    words <- tolower(words[words != word])
    words <- str_replace_all(words,"[[:punct:]]","")
    words <- plyr::count(words[str_length(words)>3])
    words <- words[order(-words[,2]),]
    words[1:N,]
}
## Función que genera histograma con palabras más frecuentes asociadas a la búsqueda
histWord <- function(data, word, N=10){
    angle <- 0
    if(N > 15){
        angle <- 90
    }
    ggplot(data, aes(x = x, y = freq)) + geom_bar(stat = "identity", fill="#DD2C00") + xlab(paste("Palabras asociadas a",word,sep = " ")) + ylab("Frequencia") + theme(axis.text = element_text(color = "#9E9E9E"),
                                                      axis.text.x= element_text(angle = angle),
                                                      axis.title = element_text(color = "#DD2C00"),
                                                      axis.title.x = element_text(vjust = .2),
                                                      panel.background = element_blank(),
                                                      panel.grid = element_blank(),
                                                      panel.border = element_blank())
}

## Función que genera una nube de palabras con las palabras asociadas a la búsqueda
## Función que genera un mapa con las coordenadas especificadas
                                        # mapTweet <- function(coords, zoom = 13){
                                        #    ggmap(get_map(location = c(mean(coords$lon),mean(coords$lat)), zoom = zoom,maptype = "roadmap")) +
                                        #        geom_point(data = coords, aes(x = lon, y = lat), color = "#DD2C00", size = 3, alpha = .7 ) + xlab("lon") + ylab("lat")+
                                        #    theme(axis.text = element_text(color = "#9E9E9E"),
                                        #          axis.title = element_text(color = "#DD2C00"))
                                        #}
## Extracción de tweets
access_token        <- "235347211-WPBkwLTonKCus2LbIvZm7WJkoUXhwkiKRQn4ONQH"
access_token_secret <- "81vTk2pu1Cc424Wx7GcD5MrCNq1zY50K7Ese2D954yy2S"
consumer_key        <- "jvIzU7EpCOVHSpKZwIihQweFC"
consumer_secret     <- "RtObIJuzJX7fwhHVafLGu3yHsdSacaornwAfddnCAuYWD9O8uh"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_token_secret)
## función para procesar tweets
getCleanTwitt <- function(entidad,N=10,n){
    tweet     <- searchTwitter(entidad,n)
    tweetDf   <- twListToDF(tweet)
    tweetMssg <- tweetDf$text
    ## Procesamiento
    words <- laply(tweetMssg,function(t)t<-str_split(t," "))
    words <- unlist(words)
    words <- str_replace_all(words,"[[:punct:]]","")
    words[str_detect(words,"http")] <- ""
    words <- plyr::count(words[str_length(words)>3])
    words <- words[order(-words[,2]),]
    words[1:N,]
}
## función para obtener porcentaje de retweets
getRetweet <- function(entidad, n){
    tweet   <- searchTwitter(entidad, n)
    tweetDf <- twListToDF(tweet)
    df      <- count(tweetDf$retweeted)
    df$freq <- df$freq/nrow(tweetDf)
    ggplot(data = df, aes(x = x, y = freq)) +
        geom_bar(stat = "identity", fill ="#1565C0")  +
    theme(
        plot.background = element_blank(),
        panel.background = element_blank()
        )
}
