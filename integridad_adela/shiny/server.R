source("functions.R", local = FALSE)

shinyServer(function(input, output) {
    output$fecha1 <- renderText({
          if(str_length(input$date)>0)
          date_pre_proc(input$date)$date1
      })
      output$fecha2 <- renderText({
          if(str_length(input$date)>0)
          date_pre_proc(input$date)$date2
      })
      output$entidad <- renderText({
          if(str_length(input$entity)>0)
          transform.entity(input$entity)$Entidad
     })
      output$clave <- renderText({
          if(str_length(input$entity)>0)
          transform.entity(input$entity)$clave
      })
    output$registro <- renderText({
        if(str_length(input$ent_cor)>0){
        entities <- add.entity(input$ent_cor,
                              input$entity,
                              entities
                              )
            "Cambios realizados."
        }
    })
})
