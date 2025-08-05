
function(input, output, session) {
 

  # data: country subset ----
  cntry_subset <- reactive({
    req(input$selected_cntry)
    if (input$selected_cntry == "Worldwide") {
      maillist %>% 
        filter(as.Date(Timestamp) >= input$date[[1]], as.Date(Timestamp) <= input$date[[2]]) %>%
        mutate(area = Land2)
    } else {
        filter(maillist, Land2 %in% input$selected_cntry) %>%
        filter(as.Date(Timestamp) >= input$date[[1]], as.Date(Timestamp) <= input$date[[2]]) %>%
        mutate(area = region)
      }
  })

  
  
  
  # data: monthly by country ----

  monthly_byctry <- reactive({
    req(input$selected_cntry)
    if (input$selected_cntry == "Worldwide") {
      monthly %>% 
        filter(ymd(paste(month, "1")) >= input$date[[1]], ymd(paste(month, "1")) <= input$date[[2]]) %>%
        arrange(month)
    } else {
      monthly %>% 
        filter(ymd(paste(month, "1")) >= input$date[[1]], ymd(paste(month, "1")) <= input$date[[2]]) %>% 
        filter(Land2 %in% input$selected_cntry) %>%
        arrange(month)
      }
  })

  
  
  
  # data: zoom settings ----
  zoom_setting <- reactive({
    req(input$selected_cntry)
    if (input$selected_cntry == "Worldwide") {
      c(51.1642, 
        10.4541, 
        2)
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

  
  
  # stat: members ----
  output$members <- renderText({
    nrow(cntry_subset())
  })
  
  # stat: roots ----
  output$roots <- renderText({
    t <- cntry_subset()$`Hast du philippinische Wurzeln?` %>% na.omit %>% table
    percent(t["Ja"]/sum(t))
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
    
    # output$bar <- renderPlot({
    #   byctry <- maillist %>% 
    #     group_by(Land2) %>% 
    #     summarise(members = n())
    #   
    #   ggplot(data = byctry, aes(x = Land2, y = members)) +
    #     geom_bar(stat = "identity") +
    #     theme_minimal()
    # })
    # 
    # output$newmems <- renderPlotly ({
    #   cumulative <- cntry_subset() %>%
    #     mutate(month = format(Timestamp, "%Y-%m")) %>%
    #     group_by(month) %>%
    #     summarise(membr = n()) %>%  # must complete missing months
    #     arrange(month) %>%
    #     mutate(cumsum = cumsum(membr))
    # 
    #  plot_ly(cumulative, type = "bar", color = I("#D45855ff")) %>%
    #  #  plot_ly(cumulative, type = "bar", color = ~Land2) %>%
    #     add_trace(x = ~month, y = ~membr, name = 'New members') %>%
    #     layout(showlegend = F) %>%
    #     layout(title = "",
    #            yaxis = list(title = 'New members'),
    #            xaxis = list(title = ''))
    # })
    # 
    # output$cumts <- renderPlotly ({
    #   cumulative <- cntry_subset() %>%
    #     mutate(month = format(Timestamp, "%Y-%m")) %>%
    #     filter(!is.na(month)) %>% 
    #     group_by(month) %>%
    #     summarise(membr = n()) %>%  # must complete missing months
    #     arrange(month) %>%
    #     mutate(cumsum = cumsum(membr))
    #   
    #   plot_ly(cumulative, type = 'scatter', mode = 'lines', color = I("#F4BC4Eff"), fill = "tozeroy") %>%
    #   add_trace(x = ~month, y = ~cumsum, name = 'Members') %>%
    #   layout(showlegend = F) %>%
    #   layout(title = "",
    #          yaxis = list(title = 'Cumulative number of members'),
    #          xaxis = list(title = ''))
    # })
    
    
    # chart: cumulative time series ----
    output$hc_cumts <- renderHighchart({
    cumulative <- monthly_byctry() %>%
      group_by(month) %>%
      summarise(membr = sum(membr)) %>%  # must complete missing months
      arrange(month) %>%
      mutate(cumsum = cumsum(membr))

      hchart(cumulative, "line",
             hcaes(x = month, y = cumsum), color = "#F4BC4Eff", name = "Number of members") %>%
        hc_title(text = "Cumulative network growth", align = "left") %>%
        hc_yAxis(title = list(enabled = FALSE)) %>%
        hc_xAxis(title = list(enabled = FALSE)) 
    })
    
    
    
    # chart: bar by country ----
    output$hc_newmem <- renderHighchart({
      hchart(monthly_byctry(), 'column',
           hcaes(x = month, y = membr, group = Land2),
           stacking = "normal"
           ) %>%
        hc_colors(c("#D45855ff", "#0073C2FF", "#EFC000FF", "#656364")) %>%
        hc_title(text = "New members per month", align = "left")%>%
        hc_yAxis(title = list(enabled = FALSE)) %>%
        hc_xAxis(title = list(enabled = FALSE)) 
    })
    
    
    
    # chart: donut ----
    output$hc_donut <- renderHighchart({
      df <- cntry_subset() %>%
      group_by(area) %>%
      summarise(membr = dplyr::n())
      
      hchart(df,
      "pie", hcaes(x = area, y = membr), 
      center = c(50, 50), 
      innerSize="50%",
      name = "Number of members"
      ) %>%
        
             
        hc_plotOptions(
          innersize="50%", 
          startAngle=90, 
          endAngle=90,
          center=list('50%', '75%'),
          size='110%'
          )  %>%
        hc_title(text = "Member distribution by area", align = "left") 
    })
    
    
    
    
    # chart: wordcloud ----

    output$wordcloud <- renderWordcloud2 ({
    pal <- rep(c("#D45855ff", "#0073C2FF", "#EFC000FF", "#656364"), 8)
    df <- if (input$word == "bezug") bezug_subset() else motivation_subset()
    df <- head(df, 50)
    
    my_wordcloud = wordcloud2a(
      df,
      color = rep_len(pal,
                      nrow(df)))  
    })
  
    # output$gg_bezug <- renderPlot ({
    #   pal <- rep(c("#D45855ff", "#0073C2FF", "#EFC000FF", "#656364"), 8)
    #   df <- head(bezug_subset(), 30)
    #   p <- ggplot(df, 
    #               aes(label = word, size = wordcount, 
    #                   color = factor(sample.int(10, nrow(df), replace = TRUE)))) +
    #     geom_text_wordcloud() +
    #     scale_size_area(max_size = 20)  +
    #     scale_color_manual(values = head(pal,nrow(df))) +
    #     labs(title = "What connects you to the Philippines?") +
    #     theme_classic() +
    #     theme(plot.title = element_text(size = 18))
    #   p
    #   #ggiraph(ggobj = p, width_svg = 7, height_svg = 7)
    # })
    # 
    # 
    # 
    # 
    # output$gg_motiv <- renderPlot ({
    #   pal <- rep(c("#D45855ff", "#0073C2FF", "#EFC000FF", "#656364"), 8)
    #   df <- head(motivation_subset(), 20)
    #   p <- ggplot(df, 
    #               aes(label = word, size = wordcount, 
    #                   color = factor(sample.int(10, nrow(df), replace = TRUE)))) +
    #     geom_text_wordcloud() +
    #     scale_size_area(max_size = 20)  +
    #     scale_color_manual(values = head(pal,nrow(df))) +
    #     labs(title = "Motivation for joining the network") +
    #     theme_classic() +
    #     theme(plot.title = element_text(size = 18))
    #   p
    #   #ggiraph(ggobj = p, width_svg = 7, height_svg = 7)
    # })
    # 
    
    # real time theming
    #bs_themer() 
    
    
}
