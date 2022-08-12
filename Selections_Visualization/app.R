library(tidyverse)
library(stringr)
library(ggcorrplot)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(ggrepel)
library(kableExtra)
# install.packages("elevatr")
library(elevatr)

# install.packages("htmlTable")
library(htmlTable)

# library(LaCroixColoR)
# install.packages("LaCroixColoR")
library(corrr)
library(sf)
library(leaflet)
# options(shiny.sanitize.errors = TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Selections"),
  dashboardSidebar(
    sidebarMenu(
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "search ..."),
      menuItem("Summary statistics", tabName = "summary"),
      menuItem("Correlation Plots", tabName = "corrr"),
      menuItem("Heatmaps", tabName = "heatmaps"),
      menuItem("Images", tabName = "images"),
      menuItem("traits and Selection Index", tabName = "traits" ),
      menuItem("Phenotypic Values", tabName = "phenotypic"),
      menuItem("traits Performances", tabName = "performance"),
      menuItem("Interactive Map", tabName = "maps")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "summary",
              h2("Check Mean Data"),
              fluidRow(
                column(3, fileInput("analysed", label= "Enter your analyzed file")),
                column(3, uiOutput("select_checks")),
                # column(3, verbatimTextOutput("p2")),
                column(3, fileInput('uploadfile', label = "Select images for this trial", 
                                    multiple=TRUE, accept = c(".jpg",".png",".jpeg")))
                
              ),
              fluidRow(
                box(column(12, DT::dataTableOutput("check_mean")), 
                    collapsible = TRUE, status = "success", title = "Check mean"), 
                box(column(12, DT::dataTableOutput("check_mean_diff")), 
                    collapsible = TRUE, status = "success", title = "Check mean difference")
              )
      ),
      
      tabItem(tabName = "corrr",
              h2("Correlation Plot"),
              fluidRow(
                box(width = 3, title = "Preferred Choices for Visualization", status = "info", collapsible = TRUE,
                    selectInput(inputId = "correlation",label = "Select type of correlation", 
                                      choices = c("Bar correlation wrt SINDEX", "Normal correlation plot"), selected = "Normal correlation plot" ),
                    selectInput(inputId = "shapes",label = "Shapes", 
                                      choices = c("square", "circle","full"), selected = "Circle"),
                    selectInput(inputId = "type",label = "Type", 
                                      choices = c("lower", "upper","full"), selected = "full" ),
                    selectInput(inputId = "label",label = "Label observations", 
                                      choices = c(TRUE, FALSE), selected = TRUE ),
                    selectInput(inputId = "siglvl",label = "Significance level", 
                                      choices = c("Yes", "No"), selected = "Yes" ),
                    selectInput(inputId = "order",label = "Order", 
                                      choices = c(TRUE, FALSE), selected = TRUE)
                ),
                box(width = 9, height = "100%", title = "Correlation Plot", status = "info", collapsible = TRUE,
                    plotlyOutput("correlation_plot", width = "100%", height = "100%"))
              )
      ),
      tabItem(tabName = "heatmaps", 
              h2("Heatmaps")
      ),
      tabItem(tabName = "images",
              h2("Images")
      ),
      tabItem(tabName = "traits",
              h2("traits and Selection Index")
      ),
      tabItem(tabName = "phenotypic",
              h2("Phenotypic Data")
      ),
      tabItem(tabName = "performance",
              h2("Trait Performance"),
              fluidRow(
                box(width = 3, title = "Preferred choices for visualization", status = "info", collapsible = TRUE,
                    uiOutput("yaxis"),
                    uiOutput("xaxis"),
                    uiOutput("color"),
                    uiOutput("size"),
                    uiOutput("islabel"),
                    uiOutput("range")
                ),
                box(width = 9, title = "Genotypes Performance Accross Difference Traits", status = "info", collapsible = TRUE,
                    plotlyOutput("performance_plot", width = "100%", height = "700px")
                )
              )
      ),
      tabItem(tabName = "maps",
              h2("Interactive Map"),
              box(width = 12, 
              fluidRow(
                column(3, fileInput("geo_data", "Enter your data with coordinates")),
                column(3, uiOutput("trait_map")),
                column(3, uiOutput("checks_select")),
                column(3, uiOutput("acc_select"))
              )),
              fluidRow(
                div(style = "padding-left: 20px",
                    uiOutput("switch")
                  ),
                box(
                  width = 12, height = "100%", title = "Location of trials on Map", status = "warning", collapsible = TRUE,
                  plotlyOutput("loc_map", width = "100%", height = "500px")
                ),
                box(
                  width = 12, height = "100%", title = "GEO WORLD", status = "success", collapsible = TRUE,
                  leafletOutput("geo_world", width = "100%", height = "700px")
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  
  df <- reactive({
    req(input$analysed)
    file <- input$analysed
    ext <- tools::file_ext(file$datapath)
    validate(need(ext == "csv", "Please your data must be a csv file."))
    read.csv(file = file$datapath)
  })
  
  df2 <- reactive({
    # req(input$checks)
    req(df())
    ayt20_sindex <- df()
    names <- c("accession_name", "genotype", "clones", "clone", "genotypes", "accession")
    is.ava <- names %in% colnames(ayt20_sindex)
    truth.test <- match(TRUE, is.ava)
    pos.truth.test <- names[truth.test]
    name.avai.pos <- match(pos.truth.test, colnames(ayt20_sindex))
    colnames(ayt20_sindex)[name.avai.pos] <- "accession_name"
    ayt20_sindex <- ayt20_sindex %>%
      janitor::clean_names() %>% 
      select(accession_name:sindex) %>%
      mutate_if(is.numeric, round,2)
    return(ayt20_sindex)
  })
  
  output$select_checks <- renderUI({
    ayt20_sindex <- df2()
    list_of_accession <- ayt20_sindex$accession_name
    pickerInput(
      inputId = "checks",
      label = "Please select checks",
      choices = c(ayt20_sindex$accession_name),
      selected = NULL,
      multiple = TRUE,
      options = list(style = "btn-primary",`action-box` = TRUE, size = 5),
      choicesOpt = list(
        subtext = paste("SI", 
                        ayt20_sindex$sindex,
                        sep = ": ")),
      width = NULL,
      inline = FALSE
    )
    # selectInput("checks", "Please select checks", multiple = TRUE, choices = c(ayt20_sindex$accession_name))
    
  })
  
  
  
  # output$p2 <- renderText(class(input$checks))

  
  output$check_mean <- DT::renderDataTable({
    req(df2())
    # calculate for checks average 
    ayt20_sindex <- df2()
    # req(input$checks)
    checks <- c(input$checks)
    
    checks_mean <- ayt20_sindex %>%
      filter(accession_name %in% checks) %>%
      add_row(accession_name = "check_mean", summarise(., across(where(is.numeric), mean))) %>%
      filter(accession_name == "check_mean")
    ayt20_sindex <- bind_rows(ayt20_sindex,checks_mean)
    
    DT::datatable(ayt20_sindex, options = list(pageLength = 5, scrollX = TRUE, scrollY = TRUE))
  })
  
  output$check_mean_diff <- DT::renderDataTable({
    req(df2())
    
    ayt20_sindex <- df2()
    # req(input$checks)
    checks <- c(input$checks)
    
    
    checks_mean <- ayt20_sindex %>%
      filter(accession_name %in% checks) %>%
      add_row(accession_name = "check_mean", summarise(., across(where(is.numeric), mean))) %>%
      filter(accession_name == "check_mean")
    ayt20_sindex <- bind_rows(ayt20_sindex,checks_mean)
    # calculate for checks average 
    ayt20_sindex_checkdiff <- ayt20_sindex %>% 
      select(-sindex) %>% 
      mutate(across(where(is.numeric), .fns = ~((./.[accession_name == "check_mean"]-1)*100))) %>% 
      mutate(sindex=ayt20_sindex$sindex) %>% 
      mutate(rank = factor(row_number()))
    
    DT::datatable(ayt20_sindex_checkdiff, options = list(pageLength = 5, scrollX = TRUE, scrollY = TRUE))
  })
  
  df_checkmean <- reactive({
    ayt20_sindex <- df2()
    # req(input$checks)
    checks <- c(input$checks)
    ayt20_sindex <- ayt20_sindex %>%
      janitor::clean_names() %>% 
      select(accession_name:sindex) %>%
      mutate_if(is.numeric, round,2)
    
    checks_mean <- ayt20_sindex %>%
      filter(accession_name %in% checks) %>%
      add_row(accession_name = "check_mean", summarise(., across(where(is.numeric), mean))) %>%
      filter(accession_name == "check_mean")
    ayt20_sindex <- bind_rows(ayt20_sindex,checks_mean)
    return(ayt20_sindex)
  })
  
  output$correlation_plot <- renderPlotly({
    req(input$analysed)
    
    ayt20_sindex <- df_checkmean()
    if(input$correlation == "Normal correlation plot"){
      corr <- round(cor(ayt20_sindex[,-1], use = "pairwise.complete.obs"), 1)
      p.mat <- cor_pmat((ayt20_sindex[,-1]), use = "pairwise.complete.obs")
      x <- ""
      
      val <- input$siglvl
      if(val == "Yes"){
        x <- p.mat
      } else {
        x <- NULL
      }
      
      corr.plot <- ggcorrplot(corr, method = input$shapes, type = input$type, 
                              ggtheme = ggplot2::theme_minimal, title = "",
                              show.legend = TRUE, legend.title = "Correlation", show.diag = FALSE,
                              colors = c("blue", "white", "red"), outline.color = "white",
                              hc.order = input$order, hc.method = "complete", lab = input$label,
                              lab_col = "black", lab_size = 4, p.mat = x, sig.level = 0.05,
                              insig = c("pch", "blank"), pch = 4, pch.col = "black",
                              pch.cex = 5, tl.cex = 10, tl.col = "black", tl.srt = 45,
                              digits = 2)
      ggplotly(corr.plot)
      
    } else {
      corr2.plot <- column_to_rownames(ayt20_sindex, var =  "accession_name") %>%
       corrr::correlate() %>%
        corrr::focus(sindex) %>% 
        dplyr::mutate(term = reorder(term, sindex)) %>%
        ggplot2::ggplot(ggplot2::aes(term, sindex)) +
        # color each bar based on the direction of the correlation
        ggplot2::geom_col(ggplot2::aes(fill = sindex >= 0)) + 
        ggplot2::coord_flip()
      ggplotly(corr2.plot)
    }
    
  })
  
  output$yaxis <- renderUI({
    req(df2())
    checks <- input$checks
    ayt20_sindex <- df2()
    list_of_traits <- colnames(ayt20_sindex %>% select_if(is.numeric))
    selectInput("yaxis", "Select trait for y axis", multiple = FALSE, choices = list_of_traits[-1], selected = "dm")
  })
  
  output$xaxis <- renderUI({
    req(df2())
    checks <- input$checks
    ayt20_sindex <- df2()
    list_of_traits <- colnames(ayt20_sindex %>% select_if(is.numeric))
    selectInput("xaxis", "Select trait for x axis", multiple = FALSE, choices = list_of_traits[-1], selected = "fyld")
  })
  
  output$color <- renderUI({
    req(df2())
    checks <- input$checks
    ayt20_sindex <- df3()
    list_of_traits <- colnames(ayt20_sindex %>% select(everything()))
    selectInput("color", "Select trait for color", multiple = FALSE, choices = c(list_of_traits[-1]), selected = "sindex")
  })
  
  output$size <- renderUI({
    req(df2())
    checks <- input$checks
    ayt20_sindex <- df2()
    list_of_traits <- colnames(ayt20_sindex %>% select_if(is.numeric))
    selectInput("size", "Select trait by size", multiple = FALSE, choices = list_of_traits[-1], selected = "dm")
  })
  
  output$islabel <- renderUI({
    req(df2())
    checks <- input$checks
    ayt20_sindex <- df2()
    list_of_traits <- colnames(ayt20_sindex %>% select_if(is.numeric))
    selectInput("islabel", "Label Points", multiple = FALSE, choices = c(TRUE, FALSE))
  })
  
  output$range <- renderUI({
    req(df2())
    checks <- input$checks
    ayt20_sindex <- df2()
    list_of_traits <- colnames(ayt20_sindex %>% select_if(is.numeric))
    selectInput("range", "Filter sindex in %", multiple = FALSE, choices = c(80, 70, 60, 50))
  })
  
  df3 <- reactive({
    req(input$checks)
    ayt20_sindex <- df2()
    checks <- c(input$checks)
    ayt20_sindex <- ayt20_sindex %>% mutate(category = if_else(accession_name %in% checks, "check","selection"))
    return(ayt20_sindex)
  })
  
  output$performance_plot <- renderPlotly({
    req(input$checks)
    ayt20_sindex <- df3()
    # checks <- input$checks
    # ayt20_sindex <- ayt20_sindex %>% mutate(category = if_else(accession_name %in% checks, "check","selection"))
    sub_val <- subset(ayt20_sindex, sindex > input$range)
    
    peformance_plot <- ayt20_sindex %>% ggplot(aes_string(x = input$xaxis, y = input$yaxis, label = "accession_name")) +
      geom_point(aes_string(color = input$color, size = input$size)) +
      geom_smooth(se = FALSE, method = lm, fullrange = FALSE) +
      geom_text(data = sub_val, vjust = 0, nudge_y = .2) +
      # scale_color_gradient(low = "red", high = "blue") + 
      # geom_text(hjust = 0, nudge_x = 0.05, size = 3) + 
      theme_light()
    ggplotly(peformance_plot)
    
    
    # ayt20_sindex %>% ggplot(aes(x = dm, y = fyld, label = accession_name)) +
    #   geom_point(aes(colour = categor_y, size = sindex)) +
    #   geom_smooth(se = FALSE, method = lm, fullrange = FALSE) +
    #   geom_text(data = subset(ayt20_sindex, sindex > 80),vjust = 0, nudge_y = 0.08)+
    #   theme_minimal()
  })
  
  ################################################## Maps ##################################################################
  
  geo_data <- reactive({
    req(input$geo_data)
    file <- input$geo_data
    ext <- tools::file_ext(file$datapath)
    validate(need(ext == "csv", "Please your data must be a csv file."))
    read.csv(file = file$datapath)
  })
  
  
  wrang <- function(datageh){
    dat <- janitor::clean_names(datageh)
    
    piv <- dat %>% pivot_longer(cols = -c(trait, accession, combined, category), names_to = "location", values_to = "values")
    locs <- data.frame(location = c("Ibadan", "Mokwa", "Ago-owu", "Onne", "Otobi", "Ubiaja"), lat = c(7.3775, 9.2928, 7.2519, 4.7238, 7.1079, 6.6493),
                       long = c(3.9470,5.0547,4.3258,7.1516,8.0897,6.3918))
    piv2 <- piv %>%  mutate(
      location = case_when(
        stringr::str_detect(location, pattern = regex("ag"))        ~ "Ago-owu",
        stringr::str_detect(location, pattern = regex("ib"))        ~ "Ibadan",
        stringr::str_detect(location, pattern = regex("mk"))        ~ "Mokwa",
        stringr::str_detect(location, pattern = regex("on"))        ~ "Onne",
        stringr::str_detect(location, pattern = regex("ot"))        ~ "Otobi",
        stringr::str_detect(location, pattern = regex("ub"))        ~ "Ubiaja",
        stringr::str_detect(location, pattern = regex("ik"))        ~ "Ikenne"
      )
    ) %>% 
      mutate(
        long = case_when(
          location == "Ago-owu"      ~ locs %>% filter(location=="Ago-owu") %>% select(long) %>% as.numeric(),
          location == "Ibadan"      ~ locs %>% filter(location=="Ibadan") %>% select(long) %>% as.numeric(),
          location == "Mokwa"      ~ locs %>% filter(location=="Mokwa") %>% select(long) %>% as.numeric(),
          location == "Onne"      ~ locs %>% filter(location=="Onne") %>% select(long) %>% as.numeric(),
          location == "Otobi"      ~ locs %>% filter(location=="Otobi") %>% select(long) %>% as.numeric(),
          location == "Ubiaja"      ~ locs %>% filter(location=="Ubiaja") %>% select(long) %>% as.numeric()
        )
      ) %>% 
      mutate(
        lat = case_when(
          location == "Ago-owu"      ~ locs %>% filter(location=="Ago-owu") %>% select(lat) %>% as.numeric(),
          location == "Ibadan"      ~ locs %>% filter(location=="Ibadan") %>% select(lat) %>% as.numeric(),
          location == "Mokwa"      ~ locs %>% filter(location=="Mokwa") %>% select(lat) %>% as.numeric(),
          location == "Onne"      ~ locs %>% filter(location=="Onne") %>% select(lat) %>% as.numeric(),
          location == "Otobi"      ~ locs %>% filter(location=="Otobi") %>% select(lat) %>% as.numeric(),
          location == "Ubiaja"      ~ locs %>% filter(location=="Ubiaja") %>% select(lat) %>% as.numeric()
        )
      )
    return(piv2)
  }
  
 
  wrangle_data <- reactive({
    dat <- geo_data()
    checks <- input$checks_select
    # locs <- data.frame(location = c("Ibadan", "Mokwa", "Ago-owu", "Onne", "Otobi", "Ubiaja"), lat = c(7.3775, 9.2928, 7.2519, 4.7238, 7.1079, 6.6493),
    #                    long = c(3.9470,5.0547,4.3258,7.1516,8.0897,6.3918))
    dat <- dat %>% janitor::clean_names() %>% mutate(category = if_else(accession %in% checks, "checks", "selection"))
    return(wrang(dat))
  })
  
  
  
  
  output$trait_map <- renderUI({
    pickerInput(
      inputId = "trait",
      label = "Please select a trait",
      choices = c("FYLD", "DYLD", "PLANT_HEIGHT", "DM", "SPROUT","MCMDS"),
      selected = "DYLD",
      multiple = FALSE,
      options = list(style = "btn-primary",`action-box` = TRUE, size = 5),
      # choicesOpt = list(
      #   subtext = paste("SI", 
      #                   ayt20_sindex$sindex,
      #                   sep = ": ")),
      width = NULL,
      inline = FALSE
    )
  })
  
  output$acc_select <- renderUI({
    df <- geo_data() %>% janitor::clean_names() %>% arrange(desc(combined))
    list_of_accession <- df$accession
    pickerInput(
      inputId = "acc_select",
      label = "Select accession to visualize",
      choices = c(list_of_accession),
      selected = list_of_accession[[1]],
      multiple = TRUE,
      options = list(style = "btn-primary", `action-box` = TRUE, `live-search` = TRUE, size = 5),
      choicesOpt = list(subtext = paste("dyld", df$combined, sep = ":")),
      width = NULL,
      inline = FALSE
    )
  })
  
  output$checks_select <- renderUI({
    df <- geo_data() %>% janitor::clean_names() %>% arrange(desc(combined))
    list_of_accession <- df$accession
    pickerInput(
      inputId = "checks_select",
      label = "Select checks",
      choices = c(list_of_accession),
      selected = list_of_accession[[1]],
      multiple = TRUE,
      options = list(style = "btn-primary", `action-box` = TRUE, `live-search` = TRUE, size = 5),
      choicesOpt = list(subtext = paste("dyld", df$combined, sep = ":")),
      width = NULL,
      inline = FALSE
    )
  })
  
  output$switch <- renderUI({
    req(wrangle_data())
    # materialSwitch(
    #   inputId = "switch",
    #   label = "Success", 
    #   value = TRUE,
    #   status = "success"
    # )
    
    switchInput(
      inputId = "switch",
      label = "Check Difference", 
      labelWidth = "150px"
    )
  })
  
  calc_checkmean <- function(dat){
    checks_mean <- dat %>% 
      filter(accession %in% checks) %>%
      add_row(accession = "check_mean", summarise(., across(where(is.numeric), mean))) %>%
      filter(accession == "check_mean") %>% 
      mutate(trait = replace_na("DYLD"))
    
    # checks_mean %>% View()
    check_mean_data <- bind_rows(dat,checks_mean)
    # check_mean_data %>% View()
    
    piv2_difference <- check_mean_data %>% 
      mutate(across(where(is.numeric), .fns = ~((./.[accession == "check_mean"]-1)*100)))
    
    
    piv2_difference <- piv2_difference %>% 
      mutate(category = if_else(accession %in% checks, "checks", "selection"))
    piv2_difference <- wrang(piv2_difference)
    
    return(piv2_difference)
  }
  
  lev1 <- st_read("NGA_population_v1_2_admin/NGA_population_v1_2_admin_level2_boundaries.shp")
  output$loc_map <- renderPlotly({
    req(input$acc_select)
    # req(input$switch)
    piv2 <- wrangle_data() %>% arrange(desc(combined))
    # piv2 %>% View()
    
    # frac <- if_else(input$range == "10%", 0.1, if_else(input$range == "20%", 0.2, 
    #                                                    if_else(input$range == "50%", 0.5, 
    #                                                            if_else(input$range == "100%", 1.0, 0.1))))
    
    # filtered_piv2 <- piv2 %>% arrange(desc(combined))
    # # View(filtered_piv2)
    
    selected <- input$acc_select
    
    piv2 <- piv2 %>% filter(accession %in% selected)
    # piv2 %>% View()
    
    if(input$switch == FALSE){
      tf <- ggplot() + 
        # geom_sf(data = lev1, show.legend = TRUE) + 
        geom_sf(data = lev1, colour = "white", fill = "grey", size = .1) + 
        geom_text(data = piv2, aes(x = long, y = lat, label = location), nudge_x = .2, nudge_y = .3, check_overlap = FALSE) +
        geom_point(data = piv2, mapping = aes(x = long, y = lat, size = values, fill = values, color = category,
                                              text = paste0("<b> trait: ",trait,"</b> \n",
                                                            "<b> accession: ", accession, "</b> \n",
                                                            "<b> DYLD: ",values,"</b>")))+
        # scale_color_viridis_c() +
        scale_fill_gradient2(low = "red", midpoint = 6.357, mid = "yellow", high = "green") +
        scale_color_discrete(low = "blue", mid = "white", high = "darkblue") +
        # coord_sf(xlim = c(2, 6), ylim = c(6, 10), expand = FALSE) +
        # geom_sf_text(data = lev1, aes(label = statename)) +
        # theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
        #                                       size = 0.5), panel.background = element_rect(fill = "aliceblue")) +
        facet_wrap(~fct_inorder(accession), ncol = 2) +
        theme_gray()
      return(plotly::ggplotly(tf, tooltip = "text"))
      
    } else {
      checks <- input$checks_select
      dat <- geo_data() %>% janitor::clean_names() %>% arrange(desc(combined))
      # dat %>% View()

      if(length(checks) >= 0){
        piv2_difference <- calc_checkmean(dat)
        piv2_difference <- piv2_difference %>% filter(accession %in% selected)
        
        tf <- ggplot() + 
          # geom_sf(data = lev1, show.legend = TRUE) + 
          
          geom_sf(data = lev1, colour = "white", fill = "grey", size = .1) + 
          geom_text(data = piv2_difference, aes(x = long, y = lat, label = location), 
                    nudge_x = .2, nudge_y = .3, check_overlap = FALSE) +
          geom_point(data = piv2_difference, mapping = aes(x = long, y = lat, size = values, fill = values, color = category,
                                                text = paste0("<b> trait: ",trait,"</b> \n",
                                                              "<b> accession: ", accession, "</b> \n",
                                                              "<b> DYLD: ",values,"</b>")))+
          # scale_color_viridis_c() +
          scale_fill_gradient2(low = "red", midpoint = 0, mid = "yellow", high = "green") +
          scale_colour_discrete() +
          # coord_sf(xlim = c(2, 6), ylim = c(6, 10), expand = FALSE) +
          # geom_sf_text(data = lev1, aes(label = statename)) +
          # theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
          #                                       size = 0.5), panel.background = element_rect(fill = "aliceblue")) +
          facet_wrap(~fct_inorder(accession), ncol = 2) +
          theme_gray()
        return(plotly::ggplotly(tf, tooltip = "text"))
      }
      
      
    }
  })
  
  output$geo_world <- renderLeaflet({
    # lev1 <- st_read("NGA_population_v1_2_admin/NGA_population_v1_2_admin_level2_boundaries.shp")
    
    req(input$acc_select)
    # req(input$switch)
    piv2 <- wrangle_data() %>% arrange(desc(combined))
    # piv2 %>% View()
    
    # frac <- if_else(input$range == "10%", 0.1, if_else(input$range == "20%", 0.2, 
    #                                                    if_else(input$range == "50%", 0.5, 
    #                                                            if_else(input$range == "100%", 1.0, 0.1))))
    # lev1 <- st_read("NGA_population_v1_2_admin/NGA_population_v1_2_admin_level2_boundaries.shp")
    # filtered_piv2 <- piv2 %>% arrange(desc(combined))
    # # View(filtered_piv2)
    
    selected <- input$acc_select
    
    piv2 <- piv2 %>% filter(accession %in% selected[length(selected)])
    print(selected)
    # piv2 %>% View()
    
    if(input$switch == FALSE){
      lev1 %>% leaflet() %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
        addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
        addLayersControl(baseGroups = c("Toner Lite", "World Imagery")) %>% 
        # addPolygons(color = "#cecece", weight = .8, smoothFactor = 0.5, 
        #             fillOpacity = .1,
        #             layerId = ~statename,
        #             highlightOptions = highlightOptions(color = "red", weight = 1, bringToFront = TRUE)) %>% 
        # addMarkers(label = ayt20_sindex$accession_name, 
        #            clusterOptions = markerClusterOptions(),
        #            popup = ifelse(ayt20_sindex$accession_name == "IITA-TMS-IBA000070",
        #                           "IITA-TMS-IBA000070 is the one", # Value if True
        #                           "Not the one")) %>% 
        addCircleMarkers(data = piv2, lng = ~long, lat = ~lat, 
                         radius = ~ifelse(values > 6, 15, 7), 
                         color = ~if_else(values > 6,"green","red"),
                         fill = ~if_else(values > 6,"green","red"),
                         stroke = FALSE,
                         popup = ~paste("<table> 
                                            <tr> 
                                              <th> Variable </th> 
                                              <th> Value </th> 
                                            </tr>
                                            <tr>
                                              <td> Accession </td>
                                              <td>",accession ,"</td>
                                            </tr>
                                            <tr>
                                              <td> Dry Yield </td>
                                              <td>",values,"</td>
                                            </tr>
                                            <tr>
                                              <td> Location </td>
                                              <td>",location ,"</td>
                                            </tr>
                                            <tr>
                                              <td> Category </td>
                                              <td>",category ,"</td>
                                            </tr>
                                          </table>")) %>%
        setView(lng = 9.0820, lat = 8.6753, zoom = 6) %>% 
        addMiniMap(
          toggleDisplay = TRUE,
          tiles = providers$Stamen.TonerLite
        )

    } else {
      checks <- input$checks_select
      dat <- geo_data() %>% janitor::clean_names() %>% arrange(desc(combined))
      # dat %>% View()
      
      if(length(checks) >= 0){
        
        piv2_difference <- calc_checkmean(dat)
        piv2_difference <- piv2_difference %>% filter(accession %in% selected)
        
        lev1 %>% leaflet() %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
          addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
          addLayersControl(baseGroups = c("Toner Lite", "World Imagery", "Topography")) %>% 
          # addPolygons(color = "#cecece", weight = .8, smoothFactor = 0.5, 
          #             fillOpacity = .1,
          #             layerId = ~statename,
          #             highlightOptions = highlightOptions(color = "red", weight = 1, bringToFront = TRUE)) %>% 
          # addMarkers(label = ayt20_sindex$accession_name, 
          #            clusterOptions = markerClusterOptions(),
          #            popup = ifelse(ayt20_sindex$accession_name == "IITA-TMS-IBA000070",
          #                           "IITA-TMS-IBA000070 is the one", # Value if True
          #                           "Not the one")) %>% 
          addCircleMarkers(data = piv2_difference, lng = ~long, lat = ~lat, 
                           radius = ~ifelse(values > 0, 15, 7), 
                           color = ~if_else(values > 0,"green","red"),
                           fill = ~if_else(values > 0,"green","red"),
                           stroke = FALSE,
                           popup = ~paste("<table style = ","border","> 
                                            <tr> 
                                              <th> Variable </th> 
                                              <th> Value </th> 
                                            </tr>
                                            <tr>
                                              <td> Accession </td>
                                              <td>",accession ,"</td>
                                            </tr>
                                            <tr>
                                              <td> Summary </td>
                                              <td>",if_else(values >= 0, 
                                                paste(round(values, 2),"% greater check-mean"), 
                                                paste0(round(values, 2),"% lesser than check mean")) ,
                                              "</td>
                                            </tr>
                                            <tr>
                                              <td> Location </td>
                                              <td>",location ,"</td>
                                            </tr>
                                            <tr>
                                              <td> Category </td>
                                              <td>",category ,"</td>
                                            </tr>
                                          </table>")) %>%
          setView(lng = 9.0820, lat = 8.6753, zoom = 6) %>% 
          addMiniMap(
            toggleDisplay = TRUE,
            tiles = providers$Stamen.TonerLite
          )
      }
    }
    
  })
}

shinyApp(ui, server)