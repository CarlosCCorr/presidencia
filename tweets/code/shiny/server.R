source("./functions.R")
## Define server logic required to summarize and view the 
## selected dataset
shinyServer(function(input, output) {
                ## Actividad
                output$tweettime <- renderPlot({
                    if(str_length(input$speak) > 0){
                        getTimeSeries(input$speak,input$tweets)
                    }
                })
                ## Tweets Retweeteados
                output$tweetRet <- renderPlot({
                    if(str_length(input$speak) > 0){
                        getRetweet(input$speak,input$tweets)
                    }
                })
                ## Nube de palabras
                output$cloud <- renderPlot({
                    if(str_length(input$speak) > 0){
                        words   <- getCleanTwitt(input$speak, input$tweets, input$tweets)
                        wordcloud(words$x,
                                  freq=words$freq,
                                  min.freq  = 2,
                                  colors = c("#1565C0","#9E9E9E"),
                                  random.color = TRUE)
                    }
                }, height = 500, width=800)
            })

