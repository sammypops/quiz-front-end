#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
## app.R ##
library(shinydashboard)
library(mongolite)
library(tidyverse)

source("ui.R", local = T)

server <- function(input, output, session) {
  session$allowReconnect("force")
  
  update_ans = observe({
    
    dbconn = mongo(db = "quiz", collection = "answers")
    # Create a table of data to store the name against
    df = data.frame(from = input$from, from_mult = input$from_mult, 
                    to = input$to, to_mult = input$to_mult,
                    time = Sys.time(), token=session$options$appToken)
    
    dbconn$insert(df)
    
    
  })
  
  update_name = observe({
    
    dbconn = mongo(db = "quiz", collection = "names")
    # Create a table of data to store the name against
    df = data.frame(name = input$name, time = Sys.time(), token=session$options$appToken)
    
    dbconn$insert(df)
    
    
  })
  
  # 
  calc_number = function(n,m){
    
    mult = switch(m,
           `1` = 1,
           "k" = 10^3,
           "m" = 10^6,
           "b" = 10^9)
    
    return(n*mult)
    
  }
  
  feedback = reactive({
    
    f = input$from
    t = input$to
    
    if(any(is.na(c(f,t)))){
      return("Answer not accepted yet...")
    }
    
    f = calc_number(input$from,input$from_mult)
    t = calc_number(input$to,input$to_mult)
    
    # check for answer being same
    if(f==t){
      return("Answer not accepted yet...")
      
    } else if(t<f){
      # Check for answer being wrong way round
      h=f
      f=t
      t=h
      rm(h)
      
    }
    
    f = prettyNum(f, nsmall = 0, big.mark = ",", scientific = F  )
    t = prettyNum(t, nsmall = 0, big.mark = ",", scientific = F  )
    
    
    return(paste(f,"to",t))
    
    
  })
  
  output$feedback = renderText(feedback())
  
  output$name_feedback = renderText({
    
    name = input$name
    
    dbconn = mongo(db = "quiz", collection = "names")
    
    name_history = dbconn$find(query = paste0('{ "token":"', session$options$appToken,'"}'),
                               fields = '{}')
    
    current_name = name_history %>% 
      arrange(desc(time)) %>% 
      slice(1) %>% 
      pull(name)
    
    current_name
    
    
  })
  
}

shinyApp(ui, server)