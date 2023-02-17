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
library(dplyr)
library(stringr)
library(cookies)



source("ui.R", local = T)

name_char_lim = 30

server <- function(input, output, session) {
  
  
  setBookmarkExclude(c("from", "from_mult", "name", 
                       "sidebarCollapsed", "sidebarItemExpanded", 
                       "submit", "to", "to_mult"))
  
  observeEvent(input$player, ignoreInit = F, {
    # print("player")
    # updateTextInput(inputId = "player", value = input$player)
# http://192.168.1.195:8787/p/613fd588/?_inputs_&player=%22kjsdnflijdanflka%22
    shinyjs::disable("player")

  })
  
  session$allowReconnect("force")

  get_player = function(){
    
    player = get_cookie(cookie_name = "player", missing = "")
    
    if(player == ""){
      
      set_cookie(
        cookie_name = "player",
        cookie_value = input$player,
        expiration = 1
      )
      
      
      return(get_cookie(cookie_name = "player", missing = ""))
      
    } else {
      
      return(player)
      
    }
    
  }
  
  update_ans = observe({
    
    dbconn = mongo(db = "quiz", collection = "question_number")
    
    qn = dbconn$find(query = '{}',
                     fields = '{}',
                     sort = '{"timestamp":-1}',
                     limit = 1) 
    
    if(all(dim(qn) == c(0,0))){
      
      # no question number found. don't show anything
      Q = NULL
      
    } else {
      
      Q = qn$question_number
      
    }
    
    dbconn = mongo(db = "quiz", collection = "answers")
    # Create a table of data to store the name against
    df = data.frame(Q=Q,
                    from = input$from, from_mult = input$from_mult, 
                    to = input$to, to_mult = input$to_mult,
                    time = Sys.time(), token=get_player())
    
    dbconn$insert(df)
    
    
  })
  
  get_name = reactive({
    
    name = input$name
    
    name_remove_extra_spaces = str_remove(name, "\\s+$")
    
    name_string_length_limit = if_else(nchar(name_remove_extra_spaces)>name_char_lim,
                                       substr(name_remove_extra_spaces,1,name_char_lim),
                                       name_remove_extra_spaces)
    
    return(name_string_length_limit)
    
  })
  
  update_name = observe({
    
    name = get_name()
    
    if(name != ""){
      
      dbconn = mongo(db = "quiz", collection = "names")
      # Create a table of data to store the name against
      
      
      
      df = data.frame(name = name, 
                      time = Sys.time(), 
                      token=get_player())
      
      dbconn$insert(df)
      
    }
    
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
    # Sys.sleep(0.1)
    this.name = get_name()
    
    this.player = get_player()
    
    dbconn = mongo(db = "quiz", collection = "names")
    # if the box is blank then check to see if the session had a name
    # if so then use that name if not then trivial case of new player no name yet
    if(this.name == ""){
      
      players_history = dbconn$find(query = paste0('{ "token":"', this.player,'"}'),
                                         fields = '{}') 
      
      if(all(dim(players_history) == c(0,0))){
        
        # no history found return no name and wait for edits
        return("")
        
      } 
      
      players_current_name = players_history %>% 
        arrange(desc(time)) %>% 
        slice(1) %>% 
        pull(name)
      
      
      
      return(players_current_name)
      
      
      
    } else {
      
      # if the box isn't blank then we need to check if the name is already in use and 
      # if so deny the change and notify
      
      name_history = dbconn$find(query = '{}',#paste0('{ "token":"', session$options$appToken,'"}'),
                                 fields = '{}') 
      
      current_name = name_history %>% 
        group_by(token) %>% 
        arrange(desc(time)) %>% 
        slice(1) %>% 
        group_by(name) %>% 
        arrange(time) %>% 
        slice(1) %>% 
        ungroup() %>% 
        filter(name == this.name)
      
      if(length(current_name$name)<1){
        current_name$name
      } else
        if(current_name$token == this.player){
          # case when the name chosen matches the session token
          # this is the first player with this name and gets to keep it
          current_name$name
        } else {
          
          # case when another user already has this name and we need to inform this
          # user that their chosen name is not accepted
          
          res = dbconn$remove(query = paste0('{ "token":"', this.player,'", "name":"', this.name,'"}'))
          
          player_history = dbconn$find(query = paste0('{ "token":"', this.player,'"}'),
                                       fields = '{}')
          
          if(all(dim(player_history) == c(0,0))){
            
            # player has no history and immeidately chose a name in use
            
            return("Sorry... This name is taken. Please choose another")
            
          }
          
          last_good_name = player_history %>% 
            arrange(desc(time)) %>% 
            slice(1) %>% 
            pull(name)
          
          paste0("Sorry... This name is taken. '", last_good_name, "' is your current name.")
          
        }
      
    }
    
    
    
    
     
    
  })
  
  hit_submit = observeEvent(input$submit,{
    
    dbconn = mongo(db = "quiz", collection = "submit")
    
    df = data.frame(time = Sys.time(), token=get_player())
    
    dbconn$insert(df)
    
  })
  
  output$question_number = renderText({
    
    invalidateLater(2000, session)
    
    dbconn = mongo(db = "quiz", collection = "question_number")
    
    qn = dbconn$find(query = '{}',
                                  fields = '{}',
                                  sort = '{"timestamp":-1}',
                                  limit = 1) 
    
    if(all(dim(qn) == c(0,0))){
      
      # no question number found. don't show anything
      return("")
      
    } else {
      
      return(paste("Question: ",qn$question_number))
      
    }
  })
  
}












enableBookmarking("url")
shinyApp(ui, server, enableBookmarking = "url", options = list(
  launch.browser = TRUE
))