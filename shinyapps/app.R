library(shiny)
library(shinythemes)
library(markdown)
library(ggplot2)
library(tidyverse)
library(shinydashboard)
library(DT)
library(plotly)

options(shiny.maxRequestSize = 30*10240^2)

#html tags in shiny
#https://shiny.rstudio.com/articles/tag-glossary.html

#switch na_rule to NA to exclude NA


source("./functions/top_column_counts.R")
source("./functions/percent.R")
#df should_be from df_column_counts() function
source("./functions/log_10_indicator_for_graphs.R")
#this was origianlly used to check to see if a column has text with length > length_val, not currently needed. 
col_length_check <- function(df, length_val){
  which(sapply(df, function(x) nchar(x) >length_val) %>% 
          apply(2, function(x) max(x))==1)
}

#Also used in Javascript functionality below to trim characters displayed in data table
length_val  <- 15

ui <- fluidPage(
            theme = shinytheme("cosmo"),
            tags$head(tags$style(
              type="text/css",
              "#rotate_select img {max-width: 100%; width: auto; height: 100%}",
              #this will hide errors, e.g. a temporary error that pops up, Warning: Error in : No expression to parse
              #turn this off to view errors while building
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }"
            )),
            includeCSS("styles.css"),
            includeCSS("styles2.css"),

                
    # Application title
    navbarPage("FAAST Data Explorer!",
# Data Preview Panel               
        tabPanel("Data Preview",
            sidebarLayout(
                sidebarPanel(
                    fileInput("file1", "Choose CSV File",
                              accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")
                    ),
                    tags$hr(),
                    checkboxInput("header", "Header", TRUE),
                    tags$hr(),
                    fluidRow(
                      selectInput("xcol", "Select X Axis", choices = NULL, selected = NULL,  multiple = F ),
                      conditionalPanel(
                        condition = 'output.panelStatus',
                        sliderInput("plot_xy_bins", label = "", min = 1, max = 50, value = 20)
                        ),
                        plotlyOutput("plot_xy")
                    )
                      
                    # checkboxInput("random", "Show Random Data", FALSE)
                , width = 4),
                
                #mainpanel on page
                mainPanel(
                  fluidRow(tags$h1(class = "test_header", "Overview")),  
                  #verbatimTextOutput("summary"),
                  fluidRow(
                    column(width = 5,
                      tags$h3("Dataset Info"),
                      tableOutput('data_info')
                    ),
                    column(width = 7,
                      tags$h3("Identifier Columns"),
                      tableOutput('id_columns')
                    )
                  ),
                  column(width = 12, 
                   DT::dataTableOutput("table.output"),
                   style = "overflow-x: scroll;"
                    )
                )
            )
        ),
# Summary Panel
        
        tabPanel("Summary",
                 tabsetPanel(type = "tabs",
                             tabPanel("Summary",
                                      verbatimTextOutput("summary")
                                      ),
                             tabPanel("Histogram"),
                             tabPanel("Common Values"),
                             tabPanel("Extreme Values")
                 )
        ),

# Graphs Panel        
        tabPanel("Graphs",
                 mainPanel(
                     #selectInput("variables", "Select Columns", choices = NULL)
                     #tableOutput("plotdata_select")
                     # selectInput("xcol", "Select X Axis", choices = "var_choices", selected = "Survived",  multiple = F ),
                     # selectInput("y_axis_type", "Toggle Y Scale (Auto-suggestion Provided)", choices = c("Linear","Logarithmic"), selected = "Linear"),
                     # plotOutput("plot_xy"),
                 )
        )
    )
)

server <- function(input, output, session) {
    
    # Use UI input to bring csv data onto server
    mydata <- reactive({
        req(input$file1)
        #using the data gathered from fileInput on UI Side
        inFile <- input$file1
        
        #display null if nothing exists
        #if (is.null(inFile))
        #     return(NULL)
        
        req(inFile)
        tbl <- read.csv(inFile$datapath, header=input$header, stringsAsFactors = F)
        
        req(tbl)
        if (is.null(tbl)==FALSE) {
          updateSelectInput(session,  inputId = 'xcol', label = 'Select X Axis',
                            selected  = NULL,
                            choices = names(tbl))
        }
        
        

    
        return(tbl)
    })
    
    #Check for histogram or barplot
    num_or_char_cols <- reactive({
      
      req(mydata())
      #I.e if the selected input has less than 10 unique fields or is a character field
      
      
      
      unique_count <- apply(mydata(),2, function(x) (length(unique(x))))
      
      return(unique(c(names(mydata())[unique_count < 10], names(mydata())[sapply(mydata(),class) %in% "character"])))
      
    })
    
    
    #show hide bin slider
    # output$panelStatus <- reactive({
    #   
    #   req(num_or_char_cols())
    #   return(!input$xcol %in% num_or_char_cols())
    # })
    # outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)
    #       
    #       
    # 
    #     observe({
    #         
    #       #for creating auto y_axis_type
    #         log_10_df <- mydata() %>% 
    #             df_column_counts() %>% 
    #             log_10_ind() %>% 
    #             mutate(log_10_label = ifelse(log10_ind==1,"Logarithmic","Linear"))
    #         
    #         
    #        # continuous_column_selected <- 
    #         
    #         updateSelectInput(session,  inputId = 'y_axis_type',
    #                           label = 'Y Scale',
    #                           choices = c("Linear", "Logarithmic"),
    #                           selected = log_10_df$log_10_label[log_10_df$col_name==input$xcol])
    #         
    #     })
        

    #run the mydata dataset into a renderable table for UI
    output$table.output <- renderDataTable({
      
      req(mydata())
      targets_test <- which(sapply(mydata(),class) %in% "character")
        
      datatable(
        mydata(),
        options = list(
          columnDefs = list(list(
            targets= targets_test,
            render = JS(paste0(
              #This trims the text only on the display table. Actual data is retained. length_val was set above
              #but can be changed manually
              "function(data, type, row, meta) {",
              "return type === 'display' && data.length >", length_val,"?",
              "'<span title=\"' + data + '\">' + data.substr(0,", length_val,") + '...</span>' : data;",
              "}"))
          )),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
            #Text trim, and tool tip?
          
          language = list(search = 'Filter:'),
          pageLength = 5,
          lengthMenu = c(5,10,15,20),
          autoWidth = TRUE
        ),
        callback = JS('table.page(3).draw(false);')
      )
    })
    
    
    #Generate a table of Dataset info
    output$data_info <- renderTable({
        
        info_number_cols <- ncol(mydata())
        info_number_rows <- nrow(mydata())
        info_number_na <- percent(sum(is.na(mydata()))/(info_number_cols*info_number_rows))
        info_number_blanks <- percent(sum(grepl("^\\s*$", mydata()[!is.na(mydata())]))/(info_number_cols*info_number_rows))
        
        as.data.frame(
            cbind(
            c("Number of variables", "Number of observations", "Total NA (%)", "Total Blank (%)"), 
            c(info_number_cols, info_number_rows, info_number_na, info_number_blanks)
            )
            
        )
        
    }, colnames = FALSE)
    
    

    
    output$id_columns <- renderTable({
        
        `Column Name` <- names(which(apply(mydata(),2, function(x) (length(unique(x))/length(x)))>=.99))
        `Percent Unique` <- percent(apply(mydata(),2, function(x) (length(unique(x))/length(x)))[which(apply(mydata(),2, function(x) (length(unique(x))/length(x)))>=.99)])
        
        as.data.frame(cbind(`Column Name`,`Percent Unique`), row.names = F)
        
        
    })

     # Generate a summary of the dataset
    output$summary <- renderPrint({
        
        if (is.null(mydata()))
            return(NULL)
        
        #return the  bsic r summary table
        summary(mydata())
        
    })
    
    # Generate a list from the dataset
     output$plot_xy <- renderPlotly({
         
         req(mydata())
         log_10_df <- mydata() %>% 
             df_column_counts() %>% 
             log_10_ind()
         
         #Check for histogram or barplot
         unique_count <- apply(mydata(),2, function(x) (length(unique(x))))
         
         #I.e if the selected input has less than 10 unique fields or is a character field
         if(xlab(input$xcol) %in% num_or_char_cols()){
         
           plot_xy <- mydata() %>%
              #Running a summary function on each column this basically makes a table, by column
               df_column_counts() %>% 
               filter(col_name == input$xcol) %>% 
               mutate(label_name = case_when(is.na(label_name) ~ "missing data", 
                                             grepl("^\\s*$",label_name) ~ "missing data",
                                             TRUE ~ label_name)) %>%
               #add missing or other indicator
               mutate(missing_ind = ifelse(label_name == "missing data", 1, 0),
                      other_ind = ifelse(label_name == "other", 1, 0)) %>% 
                #arranging by count, moving other and missing to the end
               arrange(missing_ind, other_ind, label_name) %>% 
               #locking the order by using factors
               mutate(label_name = factor(label_name, levels = label_name)) %>% 
               ggplot(aes(x=label_name, y = count, 
                          #controls the ill of the bars into 3 groups, spacing in label intentional for ordering purposes
                          fill = case_when(label_name == "missing data" ~ "missing data",
                                           label_name == "other" ~ " other",
                                           TRUE ~ "  Common Values"),
                          text = case_when(label_name == "other"~ "All other data points grouped",
                                           label_name == "missing data" ~ "Blank or missing data",
                                           TRUE ~ paste(col_name, label_name)))
               ) + 
               geom_bar(stat = "identity") +
                #limits x axis labels to 12 characters
               scale_x_discrete(label = function(x) stringr::str_trunc(x, 12)) +
                #removes the x axis title
               labs(x = "") + 
               #xlab(input$xcol) 
               #manually fill the factor colors
               scale_fill_manual(name = "", values = c("#636363", "#bdbdbd",  "#f0f0f0")) +
                #usign a preset theme
               theme_bw() + 
                #angling labels to 45
               theme(axis.text.x = element_text(angle = 30))
               #theme(legend.position = "bottom" , legend.box = "horizontal")

             #using auto scaling for y axis from y_axis_type
             
              # eval(parse(text = ifelse(input$y_axis_type =="Logarithmic",
              #                           "scale_y_log10()", 
              #                           "scale_y_continuous()")))
         }
         
         else{
           # plot_xy <- mydata() %>% 
           #   filter(!is.na(input$xcol)) %>% 
           #     #use aes_string in shiny
           #     ggplot(aes_string(x=input$xcol)) +
           #     geom_histogram(bins= input$plot_xy_bins) +
           #     theme_bw()
           
           plot_xy <- ggplot_build(
             mydata() %>% 
               #remove na data
               filter(!is.na(input$xcol)) %>% 
               #I could not find a tooltip setting for plotly to add bin ranges on histograms. Instead,
               #I make a histogram and then grab the data out of that histogram, and then make a barchart
               ggplot(aes_string(x=input$xcol)) +
               geom_histogram(bins= input$plot_xy_bins)
             #this is the data frame where all the data for a histogram is made
           )$data[[1]] %>% 
             ggplot(aes(x=factor(x), y = count, text = paste0("range: ",round(xmin, 1), " - ", round(xmax,1)))) + 
             #set width = 1 to appear like histogram
             geom_bar(stat="identity", width = 1) + 
             #setse the main label to blank
             labs(x = "") +
             theme_bw() +
             #sets the xbars to blank
             theme(axis.text.x = element_blank(), 
                   #sets the tick marks to blank for each bar
                   axis.ticks =element_blank()) 
             
        
         }
         
         return(
           ggplotly(plot_xy, tooltip = c("text", "count")) %>% 
             layout(legend = list(orientation = "h", x = 0, y = -0.2))
           )
             #labs(x = label_name)
     }
     )
    
    
}

shinyApp(ui, server)
