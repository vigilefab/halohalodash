
function(input, output, session) {
  
  # Declare a Theme
  custom_theme <- bs_theme(version = 5, bootswatch = "minty")
  custom_theme <- bs_theme_update(custom_theme, primary = "#D45855", secondary = "#0073C2", 
                                  success = "#EFC000", warning = "#56cc9d")

    # Load our custom theme from the custom.scss file
  custom_theme <- bs_add_rules(custom_theme, sass::sass_file("custom.scss"))
  
    # set theme
  session$setCurrentTheme(custom_theme)

  # data: country subset ----
  cntry_subset <- reactive({
    req(input$selected_cntry)
    if (input$selected_cntry == "Worldwide") {
      maillist %>% 
#        filter(as.Date(Timestamp) >= input$date[[1]], as.Date(Timestamp) <= input$date[[2]]) %>%
        mutate(area = Land2)
    } else {
        filter(maillist, Land2 %in% input$selected_cntry) %>%
#        filter(as.Date(Timestamp) >= input$date[[1]], as.Date(Timestamp) <= input$date[[2]]) %>%
        mutate(area = region)
      }
  })

  
  
  
  # data: monthly by country ----

  monthly_byctry <- reactive({
    req(input$selected_cntry)
    if (input$selected_cntry == "Worldwide") {
      monthly %>% 
      # filter(ymd(paste(month, "1")) >= input$date[[1]], ymd(paste(month, "1")) <= input$date[[2]]) %>%
        arrange(month)
    } else {
      monthly %>% 
        # filter(ymd(paste(month, "1")) >= input$date[[1]], ymd(paste(month, "1")) <= input$date[[2]]) %>% 
        filter(Land2 %in% input$selected_cntry) %>%
        arrange(month)
      }
  })
  
  
  
  # data: bezug ----
  bezug_subset <- reactive({
    req(input$selected_cntry)
    if (input$selected_cntry == "Worldwide") {
      bezug %>% group_by(word) %>% 
        summarise(wordcount = sum(wordcount)) %>% arrange(desc(wordcount))
    }  else  {
      filter(bezug, Land2 %in% input$selected_cntry) %>% group_by(word) %>% 
        summarise(wordcount = sum(wordcount)) %>% arrange(desc(wordcount))
    }
  }) 
  
  
  
  # data: motivation ----
  motivation_subset <- reactive({
    req(input$selected_cntry)
    if (input$selected_cntry == "Worldwide") {
      motivation %>% group_by(word) %>% 
        summarise(wordcount = sum(wordcount)) %>% arrange(desc(wordcount))
    }  else  {
      filter(motivation, Land2 %in% input$selected_cntry) %>% group_by(word) %>% 
        summarise(wordcount = sum(wordcount)) %>% arrange(desc(wordcount))
    }
  }) 
  
  
  
  
  # data: zoom settings ----
  zoom_setting <- reactive({
    req(input$selected_cntry)
    if (input$selected_cntry == "Worldwide") {
      c(51.1642, 
        10.4541, 
        4)
    } else {
      c(cntry_subset()$center_lat[1], 
        cntry_subset()$center_lon[1], 
        cntry_subset()$center_zoom[1])
    }
  })
  
  
  # data: radius ----
  rad <- reactive({
    (subset(cntry_subset(), Postleitzahl != "")$tenure)/365
    })
  pal <- colorNumeric(palette = c("#F4BC4Eff", "#326EB6ff"), domain = sort(unique(maillist$tenure/365)))

  
  
  # stat: members ----
  output$members <- renderText({
    nrow(cntry_subset())
  })
  
  # stat: roots ----
  output$roots <- renderText({
    t <- cntry_subset() %>% filter(`Hast du philippinische Wurzeln?` != "") %>% ungroup %>%
      select(`Hast du philippinische Wurzeln?`) %>% table
    percent(t["Ja"]/sum(t))
  })
  
  # stat: # countries ----
  output$cntry_count <- renderText({
    cntry_subset()$Land %>% unique %>% length
  })
  
  
  # stat: new mems in latest year ----
  output$latest_yr <- renderText({
    max(year(maillist$Timestamp))
  })
  
  output$latest_newmems <- renderText({
    cntry_subset() %>% filter(year(Timestamp) == max(year(maillist$Timestamp))) %>% nrow
  })
  
  
  
  
  # leaflet ----
    output$scatterMap <- renderLeaflet({
      leaflet(
        subset(cntry_subset(), Postleitzahl != "")) %>%
        setView(zoom_setting()[2], zoom_setting()[1], zoom = zoom_setting()[3]) %>%
        #setView(10.4541, 51.1642, zoom = 5)  %>%
        addProviderTiles("CartoDB.Positron",
                         options = providerTileOptions(minZoom = 1, maxZoom = 13)) %>%
        addCircleMarkers(lng = ~jitter_lon, lat = ~jitter_lat,
          label = ~ `label`,
          radius = ~ rad()*3,
          stroke = TRUE, weight = 0.5, color = "black", opacity = 1,
          fillColor = ~pal(rad()),
          fillOpacity = .7
        ) %>%
        addLegend("topright", pal = pal, values = ~sort(unique(maillist$tenure/365)),
                  title = "Membership</br>length</br>in years",
                  opacity = 1
        )
    })
    

    
    # chart: cumulative time series ----
    output$hc_cumts <- renderHighchart({
    cumulative <- monthly_byctry() %>%
      group_by(month) %>%
      summarise(membr = sum(membr)) %>%  # must complete missing months
      arrange(month) %>%
      mutate(cumsum = cumsum(membr))

      hchart(cumulative, "area",
             hcaes(x = month, y = cumsum), name = "Number of members") %>%
        hc_plotOptions(
          area = list(fillOpacity = 0.5) #, fillColor = "#F4BC4Eff", lineColor = "#F4BC4Eff")
        )  %>%
        hc_yAxis(title = list(enabled = FALSE)) %>%
        hc_xAxis(title = list(enabled = FALSE),
                 labels = list(rotation = 90)) 
    })
    
    
    
    # chart: bar by country ----
    output$hc_newmem <- renderHighchart({
      hchart(monthly_byctry(), 'column',
           hcaes(x = month, y = membr, group = Land2),
           stacking = "normal"
           ) %>%
        hc_plotOptions(
          series = list(borderWidth = 0, groupPadding = 0.07, pointPadding= 0)
          )  %>%
        hc_colors(c("#D45855", "#0073c2", "#fcba03", "#656364")) %>%
        #hc_title(text = "New members per month", align = "left")%>%
        hc_yAxis(title = list(enabled = FALSE)) %>%
        hc_xAxis(title = list(enabled = FALSE),
                 labels = list(enabled = FALSE)) 
    })
    
    
    
    # chart: donut ----
    output$hc_donut <- renderHighchart({
      df <- cntry_subset() %>%
        group_by(area) %>%
        filter(Postleitzahl != "") %>%
        summarise(membr = dplyr::n())
      
      hchart(df,
      "pie", hcaes(x = area, y = membr), 
      center = c(50, 50), 
      innerSize="50%",
      name = "Number of members"
      ) %>%
        
             
        hc_plotOptions(
          innersize="50%", 
          startAngle=180, 
          center=list('50%', '75%'),
          size='110%'
          )  #%>%
        #hc_title(text = "Member distribution by area", align = "left") 
    })
    
    
    
    
    # chart: wordcloud ----

    output$wordcloud <- renderWordcloud2 ({
    pal <- rep(c("#D45855", "#0073C2", "#EFC000", "#656364"), 8)
    df <- if (input$word == "bezug") bezug_subset() else motivation_subset()
    df <- df %>% arrange(desc(wordcount)) %>% head(50)  %>% 
      mutate(wordcount = rescale(wordcount, to = c(5,60)))
    
    my_wordcloud = wordcloud2a(
      df,
      color = rep_len(pal,nrow(df)),
      size = .5)  
    })
  

    # themes ----
    #bs_themer() 
    
 #waitress$hide()
    
}
