####--SERVER------------------------------------------------------------------------------------------------

shinyServer(function(input, output, session, options = options(warn = -1)) {
  
  ####--UI BLOCK----------------------------------------------------------------------------------------------
  default_tab = "model"
  
  output$app_version <- renderUI({
    fluidRow(
      column(12, offset = 1, 
             br(),
      )
    )
  })
  
  output$ui_sidebar <- renderUI({
    sidebarMenu(id = "tab",
                
                menuItem("Model", 
                         tabName = "model", 
                         icon = icon("cube")
                ),
                
                menuItem("Info", 
                         tabName = "info", 
                         icon = icon("question-circle")
                ),
                
                uiOutput("app_version")
    )
  })
  
  output$ui_body <- renderUI({
    updateTabsetPanel(session, "tab", selected = "model")
    tabItems(
      tabItem_model,
      tabItem_info
    )
  })
  
  ####--SERVER BLOCK-----------------------------------------------------------------------------------------
  
  ## Constants
  
  ## ReactiveValues
  v <- reactiveValues(
    solu_slider_time = NULL
  )
  
  # Server modules 
  source('server/s_data.R', local = TRUE)
  source('server/s_model.R', local = TRUE)
  source('server/s_info.R', local = TRUE)
  
})  
