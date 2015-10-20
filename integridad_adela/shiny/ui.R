shinyUI(fluidPage(
  theme = "theme.css",
  titlePanel("Valida datos"),

  sidebarLayout(
      sidebarPanel(
          tabsetPanel(
              tabPanel(
                  h4("Controles"),
                  br(),
                  textInput("date",
                            "Ingrese una fecha:",
                            value = ""),
                  textInput("entity",
                            "Ingrese una Entidad:",
                            value = ""),
                  br(),
                  br(),
                  submitButton("Corregir"),
                  br(),
                  h3("Ayuda"),
                  helpText("Nota: Ingresar una fecha y una Entidad Federativa para poner a prueba el algoritmo de corrección. Estas deben ser no vacías.")
              ),
              tabPanel(
                  h4("Validación"),
                  br(),
                  checkboxGroupInput("prop", 
                                     label = h3("Resultados erroneos"), 
                                     choices = list("Fecha" = 1,
                                                    "Nombre entidad" = 2,
                                                    "Clave entidad" = 3),
                                     selected = 1),
                  br(),
                  hr(),
                  br(),
                  h3("Resultados correctos"),
                  textInput("date_cor",
                            "Fecha:",
                            value = ""),
                  textInput("ent_cor",
                            "Clave entidad:",
                            value = ""),
                  submitButton("Registrar")
              )
          )
          
                   ),
      mainPanel(
          h4("Candidato fecha 1: "),
          textOutput("fecha1"),
          br(),
          h4("Candidato fecha 2: "),
          textOutput("fecha2"),
          br(),
          hr(),
          h4("Nombre entidad: "),
          textOutput("entidad"),
          br(),
          h4("Clave INEGI: "),
          textOutput("clave")
      )
  )
))
