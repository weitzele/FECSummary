library(shiny)
library(rsconnect)
library(readr)
library(dplyr)
library(stringr)

# Define UI ----
ui <- fluidPage(
  titlePanel("Summarizing FEC Data from Individual Donors"),
  
  br(),
  
  sidebarLayout(
    sidebarPanel(
      h4(strong("Instructions")),
      br(),
      h5(strong("Step 1: Download FEC Data")),
      p("Visit fec.gov and search for the individual you are researching. When you have used the drop down menus and text boxes on the left to filter your results, click the Export button in the top right of your screen. This will download a .csv file to your computer. Open this file and clean it up to make sure that all rows are the correct person, if you haven't ensured this already. But do not change the column names!"),
      h5(strong("Step 2: Upload .csv Here")),
      p("After you have cleaned up the .csv file, upload it here in the box to the right. As soon as you upload the file, this web app will summarize the data. This will only work if the uploaded file is a .csv downloaded from the FEC database."),
      h5(strong("Step 3: Set the Date Range")),
      p("Use the slider to the right to set the date range from which you want results."),
      h5(strong("Step 4: Set the Min and Max Dollar Amounts")),
      p("Use the boxes to the right to set the minimum single donation dollar amount, the maximum single donation dollar amount, the minimum cumulative donation dollar amount, and the maximum cumulative donation dollar amount."),
      h5(strong("Step 5: Copy Output to Database")),
      p("Once the output is generated, copy the relevant information."),
      br(),
      p(em("Warning: This web app is intended to make research using the FEC database easier, but this is not a replacement for ensuring data quality yourself. Compare the output results of the text and table versions and be on the lookout for discrepancies. This app will not work perfectly all the time, and may not accurately sort through each recipient or provide an accurate summary of all donations. It is meant to provide a general impression, not accurate or detailed specifics. If any serious issues are encountered, or if you would like to contribute to the further development of this Shiny app, email elicweitzel1@gmail.com."))
    ),
    
    mainPanel(
      fluidRow(
        column(5,
          #create input for .csv file upload
          fileInput("file", h4("Upload File"), 
                    accept = c(".csv")),
          #create min and max single donation input values
          numericInput("min1", h4("Min. Single Donation"), value = 1),
          numericInput("max1", h4("Max. Single Donation"), value = 1e10)
        ),
        column(5, offset = 1,
          #create input for date range
          sliderInput("daterange", h4("Set Date Range"),
                      min = 2000, max = 2021, 
                      value = c(2018, 2020), sep = ""),
          #create min and max cumulative donation input values
          numericInput("min2", h4("Min. Cumulative Donation"), 
                       value = 1),
          numericInput("max2", h4("Max. Cumulative Donation"), 
                       value = 1e10)
        )
      ),
      br(),
      #create text output
      h4("Text Summary of Results"),
      textOutput("results.text"),
      br(),
      #create table output
      h4("Table Summary of Results"),
      hr(),
      tableOutput("results.table")
    ),
  )
)

# Define server logic ----
server <- function(input, output, session) {
  infile <- reactive({
    req(input$file) #won't run unless file is uploaded
    infile <- input$file #assign input .csv file
    infile <- read_csv(infile$datapath) #read .csv
  })
  
  results.table <- reactive({
    req(input$file) #won't run unless file is uploaded
    
    #create list of earmarked values
    
    #if committee name includes " LIST", save the value from receipt_type_full, because this is where the FEC database saves earmarks for Emily's List
    earmk <- with(infile(),
                  ifelse(grepl(" LIST", committee_name) & 
                           !is.na(receipt_type_full),
                         paste(receipt_type_full),
                         paste(memo_text)))
    
    #if the earmk value contains " LIST", delete the earmark value because these are redundant in relation to committee_name for Emily's List entries
    earmk <- ifelse(grepl(" LIST", earmk),
                    paste(NA),
                    paste(earmk))
    
    #if the earmk value is "CONTRIBUTION", delete the value since it's not providing any information not available from committee_name
    earmk <- ifelse(earmk == "CONTRIBUTION", 
                    paste(NA), 
                    paste(earmk))
    
    #if the earmk value contains "SEE BELOW", delete the value because it's not providing any information not available from committee_name
    earmk <- ifelse(grepl("SEE BELOW", earmk),
                         paste(NA),
                         paste(earmk))
    
    #if the earmk value contains "CONDUIT", delete the value because it's not providing any information not available from committee_name
    earmk <- ifelse(grepl("CONDUIT", earmk),
                    paste(NA),
                    paste(earmk))
    
    #save the committee_name values as an object called recip
    recip <- with(infile(), committee_name)
    
    #if the earmarked values in earmk contain "EAR" or "CONTRIBUTIONS", save them, because they contain more detail than the committee_name. But if they don't, save the committee_name instead
    recip <- ifelse(grepl("EAR", earmk) | grepl("CONTRIBUTIONS", earmk),
                    paste(earmk),
                    paste(recip))
    
    #remove extraneous characters from the recip object
    recip <- recip %>% str_remove_all("EARMARKED FOR ") %>% 
                       str_remove_all("EARMARK FOR ") %>% 
                       str_remove_all("EARMARK TO: ") %>% 
                       str_remove_all(" CONTRIBUTIONS")
    
    #make the recip object stop yelling
    recip <- recip %>% tolower() %>% str_to_title()
    
    #replace some common words with appropriately capitalized versions
    recip <- recip %>% str_replace("Us", "US") %>%
                       str_replace("U.s.", "US") %>%
                       str_replace("Dccc", "DCCC") %>%
                       str_replace("Actblue", "ActBlue") %>%
                       str_replace("Pac", "PAC") %>%
                       str_replace("Dscc", "DSCC")
    
    #delete parenthetical codes associated with committees
    recip <- str_split(recip, pattern = " \\(", n = 2, simplify = T)[,1]
    
    #make a df containing the key information: year, recipient, and amount
    results.table <- data.frame("Year" = with(infile(), 
                                              report_year), 
                                "Recipient" = recip,
                                "Amount" = with(infile(), 
                                                contribution_receipt_amount))
    
    #group this df by year and recipient to aggregate donations
    results.table <- results.table %>% group_by(Year, 
                                                Recipient,
                                                .add = TRUE)
    
    #summarize this df to get the total amount per year, the number of donations per year, and the maximum single donation per year
    results.table <- results.table %>% summarise(
      "Total" = sum(Amount), 
      "Separate_Donations" = n(), 
      "Max_Donation" = max(Amount))
    
    #sort this table from largest contribution to smallest, and also from most recent contributions to oldest
    results.table <- results.table[order(-results.table$Total),]
    results.table <- results.table[order(-results.table$Year),]
    
    #subset the table according to the date ranges specified in the input
    results.table <- subset(results.table, 
                            Year >= input$daterange[1] & 
                            Year <= input$daterange[2])
    
    #make the years factors so they're reported nicely
    results.table$Year <- as.factor(results.table$Year)
    
    #subset the table according to the min and max dollar amounts in the input
    results.table <- subset(results.table,
                            Max_Donation >= input$min1 &
                            Max_Donation <= input$max1)
    
    #subset the table according to the min and max dollar amounts in the input
    results.table <- subset(results.table,
                            Total >= input$min2 &
                            Total <= input$max2)
    
    #return the final table
    results.table
  })
  
  results.text <- reactive({
    req(input$file) #won't run unless file is uploaded
    
    #if there are more than one donation to an entity per year, add "cumul." to the total amount to signify that this is a cumulative total from multiple contributions
    cumul <- with(results.table(), 
                  ifelse(Separate_Donations > 1,
                         paste(Total, "cumul."),
                         paste(Total)))
    
    #paste the year, recipient, and total together into a continuous character string, with each entry separated by a semi-colon
    results.text <- with(results.table(),
                         paste(Year, 
                               Recipient, 
                               cumul,
                               sep = " ",
                               collapse = "; "))
    
    #return the final character string
    results.text
  })
  
  #assign the resulting table and character string to the output
  output$results.table <- renderTable({results.table()})
  output$results.text <- renderText({results.text()})
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)