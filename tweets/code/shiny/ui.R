shinyUI(fluidPage(theme = "theme.css",
                  titlePanel("Speakers"),
                  sidebarLayout(
                      sidebarPanel(
                          textInput("speak", "Escribe el nombre de  un speaker:"),
                          sliderInput("tweets", "Número de tweets a analizar:", min = 10,
                                      max = 100, value = 50, step = 10),
                                helpText("Nota: Puede que algunas búsquedas no desprendan resultados, esto se debe a que no se han encontrado tweets con los nombres seleccionados. Las búsquedas son en tiempo real."),
                          submitButton("Analizar")
                          ),
                      mainPanel(
                          tabsetPanel(
                              tabPanel(h4("Palabras Asociadas"),
                                       plotOutput("cloud")
                                       ),
                              tabPanel(h4("Palabras Asociadas"),
                                       plotOutput("tweetRet")
                                       )
                              )
                          
                          )
                      )
                  ))
