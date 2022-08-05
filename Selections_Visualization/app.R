library(tidyverse)
library(stringr)
library(ggcorrplot)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(ggrepel)
# library(LaCroixColoR)
# install.packages("LaCroixColoR")
library(corrr)
options(shiny.sanitize.errors = TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Selections"),
  dashboardSidebar(
    sidebarMenu(
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "search ..."),
      menuItem("Summary statistics", tabName = "summary"),
      menuItem("Correlation Plots", tabName = "corrr"),
      menuItem("Heatmaps", tabName = "heatmaps"),
      menuItem("Images", tabName = "images"),
      menuItem("Traits and Selection Index", tabName = "traits" ),
      menuItem("Phenotypic Values", tabName = "phenotypic"),
      menuItem("Traits Performances", tabName = "performance"),
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
                    plotlyOutput("correlation_plot", width = "100%", height = "700px"))
              )
      ),
      tabItem(tabName = "heatmaps", 
              h2("Heatmaps")
      ),
      tabItem(tabName = "images",
              h2("Images")
      ),
      tabItem(tabName = "traits",
              h2("Traits and Selection Index")
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
              fluuidRow(
                column(3, uiOutput("accession_map")),
                column(3, uiOutput("trait_map")),
                column(3, uiOutput("check_map")),
                column(3, uiOutput("size_map"))
              ),
              box(
                width = 12, title = "Location of trials on Map", status = "warning", collapsible = TRUE,
                plotlyOutput("loc_map", width = "100%", height = "700px")
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
  
  
  
}

shinyApp(ui, server)