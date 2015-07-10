source("./functions.R")
## Define server logic required to summarize and view the 
## selected dataset
shinyServer(function(input, output) {
                ## Tweets Retweeteados
                output$tweetRet <- renderPlot({
                    getRetweet(input$speak,input$tweets)    
                })
                ## Nube de palabras
                output$cloud <- renderPlot({
                    words   <- getCleanTwitt(input$speak, input$tweets, input$tweets)
                    wordcloud(words$x,
                              freq=words$freq,
                              min.freq  = 2,
                              colors = c("#1565C0","#9E9E9E"),
                              random.color = TRUE)
                }, height = 500, width=800)
            })

