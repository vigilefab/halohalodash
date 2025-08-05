
bootstrapPage(
  theme  = bs_theme(version = 5, bootswatch = "minty"), 
  div(class="container-fluid",
      
      # application title ----
      h2("The Halo-Halo Network Dashboard"),
      div(class="row",
          
          # controls panel --------------------------------------
          div(class="col-sm-4",
              div(class="well",
                 # Select country ----
                 selectInput(inputId = "selected_cntry",
                   label = "Select country:",
                   choices = c("Worldwide", "Deutschland", "Österreich", "Schweiz", "Sonstiges"),
                   selected = "Worldwide"), 
      
                 # Date range ----
                 dateRangeInput(inputId = 'date', label ='Date range:', 
                                min = min(as.Date(maillist$Timestamp)), max = max(as.Date(maillist$Timestamp)),
                                start = min(as.Date(maillist$Timestamp)), end = max(as.Date(maillist$Timestamp))),                       
               
                 # Bezug vs Motivation ----
                 radioGroupButtons(inputId = "word", label = "Select details:",
                                   choices = list('Motivation to join' = 'motivation', 'Connection to PH' = 'bezug'),
                                   selected = list('Motivation' = 'motivation'), status = 'info', size = 'xs',
                                   justified = TRUE)
                 ),
        ),
        div(class="col-sm-8", 
            # row with two stats, a time series of new members and map
            div(class = "row", 
                # column containing two stats stacked on top of time series of new members 
                div(class = "col-sm-6", 
                    
                    # stats
                    div(class = "row", 
                        
                        # stats: members
                        div(class = "col",
                            value_box(
                              title = "Total members",
                              value = textOutput("members"),
                              showcase = bsicons::bs_icon("people-fill", size = "5rem"),
                              #theme = "orange"
                              ),
                            ),
                        
                        # stats: roots
                        div(class = "col",
                            value_box(
                              title = "Members with Filipino roots",
                              value = textOutput("roots"),
                              showcase = bsicons::bs_icon("percent", size = "5rem"),
                              #theme = "orange"
                            ),
                        ),
                        ),
                    
                    # time series
                    div(class = "row", 
                        highchartOutput(outputId = "hc_newmem", height = 300)
                        ),
                    ), 
                
                # column containing map
                div(class = "col-sm-6",
                    leafletOutput("scatterMap")
                    )
                    
                ),
            
            # row with growth rate, donut and word cloud
            div(class = "row", 
                
                # column with growth rate
                div(class = "col",
                    highchartOutput(outputId = "hc_cumts")
                    ),
                
                # column with donut
                div(class = "col",
                    highchartOutput(outputId = "hc_donut")
                    ),
                
                # column with word cloud
                div(class = "col",
                    
                    # panel with motivation
                     wordcloud2Output(outputId = "wordcloud")
                                     ),
                    
                ),
            )
        )
      )
  )
