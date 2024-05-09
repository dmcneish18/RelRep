
##make sure the latest version of dynamic is installed
#devtools::install_github("melissagwolf/dynamic")
#devtools::install_github("daattali/shinycssloaders")

###packages
library(shiny)
library(dynamic)
library(mirt)
library(ggplot2)
library(dplyr)
library(tidyr)
library(Bayesrel)
library(stringr)
library(shinycssloaders)

######################## Shiny Code

## Code for Interface
ui <- fluidPage(

    # Application title
    titlePanel(h1(HTML("Reliability Representativeness"), align="center", style="background-color: #1C4E80; color: white; padding-top:10px;padding-bottom:10px;")
               ),

    # sidebar for data upload and option selection
      sidebarLayout(
        
        sidebarPanel(
          
          #make labels font color white
          tags$style('label {color:white;}'),
          
          #Slider value font
          tags$style('.irs-grid-text{color:white;background:none; text-shadow:none;}'),
          tags$style('.irs-min {color:white;}'),
          tags$style('.irs-max {color:white;}'),
          
          #make help text font color white;
          tags$style('.help-block {color:white;}'),
          
          #change progress bar to black
          tags$head(tags$style(".progress-bar{background-color:#000000;}")),
          
          #create object that is horizontal line
          tags$head(tags$style(HTML("hr {border-top: 1px solid color=white;}"))), 
          
            #instructions heading
            h3(HTML("<b>Instructions</b>"), align="center", style="color: white; margin-top:0px;"),
            
            #instructions;
            helpText("1. Upload a Dataset in .csv format"),
            helpText("2. The first 5 rows will appear to verify the data loaded correctly."),
            helpText("3. If data look accurate, select your coefficient, missing data indicator, and scale items"), 
            helpText("4. Click Submit one time to begin calculation"),
            helpText("5. Results will appear to the right when calculations are complete"),
            
            #add horizontal line to separate instructions from options
            hr(),
          
            #label for file upload
            h5(HTML("<b>File Upload</b>"), style="color: white; margin-top:25px; margin-bottom:10px;"),
          
            #box to upload data
            fileInput("upload", NULL, accept = c(".csv")),
          
            #reduce space between upload box and ratio buttons
            div(style = "margin-top:-15px"),
          
            #box to input missing data indicator
            textInput("missing", "Missing Data Indicator", "NA"),
          div(style = "margin-bottom:-15px"),
          # Helptext underneath box to select items
          helpText(HTML("<i>Leave at default if no missing data</i>")),
          
          #box to select scale items
          #adapts once  data are uploaded
          selectizeInput(
            "vec1"
            , "Select Scale Items"
            , choices = "Upload Data to Populate"
            , multiple = TRUE),
          #reduce space between upload box and ratio buttons
          div(style = "margin-bottom:-15px"),
          # Helptext underneath box to select items
          helpText(HTML("<i>Press backspace to remove variables</i>")),
          
            #Coefficient options
            #inline puts them horizontal instead of vertical
            radioButtons("rel", label="Reliability Coefficient", choiceNames=c("Alpha","Omega"), choiceValues=c("alpha","omega"), inline=TRUE),
          
            #interval Options
            radioButtons("interval", label="Interval Type", choiceNames=c("Credible Interval (default)","Fixed Width", "Raw Bounds"), choiceValues=c("CI","width","raw"), inline=TRUE),
            helpText(HTML("<i>Use Fixed Width for a predefined level of precision (e.g., within .05 of alpha) </i>")),
            helpText(HTML("<i>Use Raw Bounds for predefined reliability values (e.g., between .80 and .90)</i>")),
          
            uiOutput("Width"),
            uiOutput("RawLow"),
            uiOutput("RawHigh"),
           
            #activation button to begin calculations
            #center "submit" within column
            div(style="display:inline-block; width:100%; text-align: center;", actionButton("go", "Submit")),
            
            #make background blue
            style="background-color: #1C4E80;"
        ),
  
   #print first 5 rows of data after it is uploaded
   mainPanel(tableOutput("head"),
   
     #create panels for output
     tabsetPanel(id="Tabs",
       
       tabPanel(title="Welcome",
                h4("Welcome to Relability Representativeness!"),
                p(HTML("<br/>")),
                h4(HTML("<ul><li>Commonly reported reliability coefficients like alpha or omega
                    imply equal precision across a distribution of composite scores.")),
                h4(HTML("<ul><li>However, reliability is often <i>conditional</i> such that it can be different for different scores.")),
                h4(HTML("<ul><li><b> This application is a tool to assess how well a reliability coefficient generalizes across an entire score distribution </b>"))
                ),
       
       #tab for reliability plot
       #div code formats the location of the download button to be below the plot, centered with the x-axis label
       tabPanel(title = "Plot",
                  plotOutput("Plot", width="5.4in", height="6in"),
                  div(style="display:inline-block; width:400px; text-align:center; margin-top:15px",uiOutput("RelPlot")),
                  div(style="width:380px; text-align:center;margin-top:5px" ,uiOutput("SVGPlot"))
         ),
          
       #tab for the table of statistics/indices for reliability representativeness
       tabPanel( title = "Statistics",
                   tableOutput("Stat")
            ),
         
       #tab for table of conditional reliabilities
         tabPanel( title = "Table",
                   div(style="display:inline-block; width:350px; text-align:center; margin-bottom:15px; margin-top:15px;" ,uiOutput("RelTable")),
                   tableOutput("Table")),
        )
     )
   )
)

#R code to execute in the background
server <- function(input, output,session) {
  
  #create object for data once it is uploaded  
  data <- reactive({
        req(input$upload)
        
        ext <- tools::file_ext(input$upload$name)
        switch(ext,
               csv = vroom::vroom(input$upload$datapath, delim = ","),
               validate("Invalid file; Please upload a .csv file")
        )
    })
  
    #Once data are uploaded, update the dropdown menu with the columns names of the uploaded data
    observeEvent(data(), updateSelectizeInput(session,"vec1", choices=names(data())))
    
    #If prespecified interval width, then show slider with width options
    output$Width<-renderUI({
      req(input$interval=="width")
              sliderInput(inputId="W", 
                       label="Enter width (from coefficient to bound):",
                      min=0, max=.20, step=.005, value=.05, ticks=FALSE)
    })
    
    #if raw interval width, show slider bar for interval lower bound
    output$RawLow<-renderUI({
      req(input$interval=="raw")
      sliderInput(inputId="rawLow", 
                    label="Enter Interval Lower Bound:",
                    min=0, max=1, step=.01, value=.50, ticks=FALSE)
    })
    
    #if raw interval wdith, show slider bar for interval upper bound
    output$RawHigh<-renderUI({
      req(input$interval=="raw")
      sliderInput(inputId="rawHigh", 
                  label="Enter Interval Upper Bound",
                  min=0, max=1, step=.01, value=.50, ticks=FALSE)
    })
  
  #print the first five rows of the uploaded data
    output$head <- renderTable({
        req(input$upload)
        head(data(), 5)
    })

    #once the submit button is clicked (go =1), run the conditional reliabliity function using the selected options
    observeEvent(input$go,{
     
      #show spinner while calculating
      shinycssloaders::showPageSpinner(caption = "Calculating, results will appear in tabs when calculations are complete")
      
      #Reliability Representativeness function
      obj<-dynamic::RelRep(data(), items=input$vec1,rel=input$rel, missing=input$missing, method=input$interval, width=input$W, raw.low=input$rawLow, raw.high=input$rawHigh)
      
      #Hide Spinner when calculation is complete
      shinycssloaders::hidePageSpinner()
      
      #open results tab once calculations are complete
      updateTabsetPanel(session, inputId = "Tabs", selected = 'Plot')

    #save reliability plot
    output$Plot <-renderPlot(obj$plot)
    
    #save the conditional reliability table
    output$Table<-renderTable({obj$table})
    
    #create a table of RR statistics and indices
    output$Stat <-renderTable({
        #blank out the column names
        colnames(obj$stat) <- c(" ", "  ")
        obj$stat
    })
    
    ## Create Download Button for Reliabilty Table after the Submit Button is clicked
    output$RelTable<-renderUI(downloadButton("downloadTable", "Download Reliability Table as CSV"))
    ## define what gets downloaded
    output$downloadTable <- downloadHandler(
      filename = function() {
        # create default file name
        paste0("Reliability_Table", ".csv")
      },
      content = function(file) {
        # Write conditional reliability table to a csv to be downloaded
        write.csv(obj$table, file)
      })
    
    ## PNG: Create Download Button for Reliability Plot after the Submit Button is clicked
    output$RelPlot<-renderUI(downloadButton("downloadPlot", "Download as High Resolution PNG"))
    output$downloadPlot <- downloadHandler(
      filename = function() {
        # create default file name
        paste0("Reliability_Plot", ".png")
      },
      content = function(file) {
        # create a hi-res png file of the plot to be downloaded
        ggsave(file, 
               obj$plot,
               height=6.47,
               width=6,
               dpi=1200,
               units="in",
               device="png")
      })
    
    ## SVG: Create Download Button for Reliability Plot after the Submit Button is clicked
    output$SVGPlot<-renderUI(downloadButton("downloadPlotSVG", "Download as EPS Vector Image"))
    output$downloadPlotSVG <- downloadHandler(
      filename = function() {
        # create default file name
        paste0("Reliability_Plot", ".eps")
      },
      content = function(file) {
        # create a a vector image EPS file of the plot to be downloaded
        ggsave(file, 
               obj$plot,
               height=6.47,
               width=6,
               dpi=1200,
               units="in",
               device="eps")
      })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
