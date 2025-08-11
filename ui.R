
bootstrapPage(
  theme  = bs_theme(version = 5),
  div(class="container-fluid",
      # application title ----
      div(class="row",
          # controls panel --------------------------------------
          div(class="col-lg-3",
              div(class="well", style = "margin: 10px; z-index: 1;",
                  h1(HTML('<img src= "Halo-Halo Logo_ohne Schrift_transparent.png" width ="100px"><p>The <font color = "#0073c2">Halo</font>-<font color = "#fcba03">Halo</font> <font color = "#D45855">Dash</font>'), align = "center"),
                  
                  p("How many second and third-generation Filipinos are there in Europe? 
                    The answer to this question cannot be found in any official statistics. 
                    But thanks to the the recently-founded and constantly growing 
                    network of young Filipinos in Germany, Austria and Switzerland, 
                    we are coming to realize that there are a lot of us, and we are 
                    everywhere."),
                  HTML("Explore the <a href = 'https://halo-halo.de/' 
                    target = '_blank'>Halo-Halo Network</a>'s recent growth and progress 
                    towards the nth generation metric.<p>"),
                  p("This dashboard uses data from the Halo-Halo mailing list. The Network
                    considers itself inclusive, meaning anyone who identifies themselves as a
                    Halo-Halo is a Halo-Halo, even when they haven't signed up. :) However, 
                    we can only keep track of the growth of our network from
                    the ones who sign up. What this dashboard calls a member is therefore a person
                    who has signed up for the mailing list."),
                 style="position:sticky;",
                 # Select country ----
                 selectInput(inputId = "selected_cntry",
                   label = "Select country:",
                   choices = c("Worldwide", "Deutschland", "Österreich", "Schweiz", "Rest of the world" = "Sonstiges"),
                   selected = "Worldwide") 
      
                 # # Date range ----
                 # dateRangeInput(inputId = 'date', label ='Date range:', 
                 #                min = min(as.Date(maillist$Timestamp)), max = max(as.Date(maillist$Timestamp)),
                 #                start = min(as.Date(maillist$Timestamp)), end = max(as.Date(maillist$Timestamp)))                       

                 )
        ),
        div(class="col-lg-9", 
            # row with two stats, a time series of new members and map
            layout_column_wrap( 
              # column containing two stats stacked on top of time series of new members 
              card(style = "border:none;background-color:none;",
                max_height = 700,
                full_screen = FALSE,
                layout_column_wrap( 
                          # stats: members ----
                  value_box(
                    showcase = bsicons::bs_icon("people-fill", size = "5rem"),
                    title = "",
                    value = textOutput("members"),
                    h5("Members"),
                    p(paste0("as of ", format(max(maillist$Timestamp), "%B %Y"))),
                    #theme = value_box_theme(fg = "#D45855ff")
                    class = "custom-metric"
                       ),
                  # stats: new mems in last yr ----
                  value_box(
                    showcase = bsicons::bs_icon("person-fill-add", size = "5rem"),
                    title = "",
                    value = textOutput("latest_newmems"),
                    h5("New Members"),
                    p(paste("in", max(year(maillist$Timestamp)))),
                    class = "custom-metric"
                  )
                ),
                          
                      
                # stats (row 2)
                layout_column_wrap( 
                  
                  # stats: countries ----
                  value_box(
                    title = "",
                    value = textOutput("cntry_count"),
                    showcase = bsicons::bs_icon("globe-americas", size = "5rem"),
                    height = "100%", 
                    h5("Countries"),
                    class = "custom-metric"
                  ),
                        # stats: roots ----
                              value_box(
                                title = "",
                                value = textOutput("roots"),
                                showcase = bsicons::bs_icon("diagram-3-fill", size = "5rem"),
                                height = "100%",
                                h6("Have Filipino Roots"),
                                class = "custom-metric"
                                #theme = "orange"
                                )
                        )
                ),
                
                # leaflet: map ----
                    card(class = "custom-card",
                      max_height = 700,
                      full_screen = FALSE,
                      card_header("Member distribution", style="font-weight:bold; font-size:18pt; color:#D45855;"),
                      p("Each dot is colored and sized by the number of years since a member signed up. 
                        Coordinates are slightly jittered to protect personal data."),
                      leafletOutput("scatterMap")
                    ),
                ),
            
            # row with growth rate, donut
               layout_column_wrap( 
                 # chart: growth rate ----
                    card(class = "custom-card",
                      max_height = 700,
                      full_screen = FALSE,
                      card_header("Cumulative growth and new members", style="font-weight:bold; font-size:18pt; color:rgb(252, 186, 3);"),
                      p("The line graph (top) shows the cumulative member growth. The bar chart (bottom) shows the number of monthly new sign-ups."),
                      highchartOutput(outputId = "hc_cumts", height = 250),
                      highchartOutput(outputId = "hc_newmem", height = 200)
                      ),
                    #), 
                
                # chart: donut ----
                    card(class = "custom-card",
                      max_height = 700,
                      full_screen = FALSE,
                      card_header("Member proportions by area", style="font-weight:bold; font-size:18pt; color:#0073c2;"),
                      p("Each piece of the donut represents a country or a region."),
                      highchartOutput(outputId = "hc_donut")
                    ),
                    
                    ),
            # chart: word cloud ----
              card(class = "custom-card",
                max_height = 700,
                full_screen = FALSE,
                card_header("Details", style="font-weight:bold; font-size:18pt; color:#D45855;"),
                p('Answers to the questions "Why did you join?" or "What is your connection to the Philippines?"'),
                radioGroupButtons(inputId = "word", 
                                          choices = list('Motivation to join' = 'motivation', 'Connection to PH' = 'bezug'),
                                          selected = list('Motivation' = 'motivation'), status = 'info', size = 'xs',
                                          justified = TRUE),
                wordcloud2Output(outputId = "wordcloud", width = "100%")
                    )
            )
        )
      )
)
