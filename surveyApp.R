library(shiny)
library(dplyr)

# which fields get saved for use later in the app
fieldsAll <- c("name","gender", "employment",
               "householdIncome", "relationshipStatus", "educationLevel", "age", "socialMediaUsage", 
               "friendsFamilyUseUsedStores", "opinionSecondHandStores", "everShoppedSecondHand",
               "reasonForSecondHandStore", "itemPurchaseSecondHand", "usedItemsLifespan",
               "usedItemsDangerous", "usedItemsPoorTaste", "usedItemsWorseHealth",
               "usedItemsFlaws", "usedItemsNotCool", "usedItemsDegrade", "usedItemsLowClass",
               "usedItemsNotTrendy", "usedItemsUnwantedHealth", "usedItemsLowQuality",
               "usedItemsLessPrestigious", "whatDoWithClothing", "howPerMonthFurniture",
               "howPerMonthElectronics", "howPerMonthClothing", "howPerMonthAppliances")

# which fields are mandatory for the survey participant to fill-out
fieldsMandatory <- c("name","gender", "employment",
                     "householdIncome", "relationshipStatus", "educationLevel", "age", "socialMediaUsage", 
                     "friendsFamilyUseUsedStores", "opinionSecondHandStores", "everShoppedSecondHand",
                     "reasonForSecondHandStore", "itemPurchaseSecondHand", "usedItemsLifespan",
                     "usedItemsDangerous", "usedItemsPoorTaste", "usedItemsWorseHealth",
                     "usedItemsFlaws", "usedItemsNotCool", "usedItemsDegrade", "usedItemsLowClass",
                     "usedItemsNotTrendy", "usedItemsUnwantedHealth", "usedItemsLowQuality",
                     "usedItemsLessPrestigious", "whatDoWithClothing", "howPerMonthFurniture",
                     "howPerMonthElectronics", "howPerMonthClothing", "howPerMonthAppliances")

# add an asterisk to an input label for the mandatory fields
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# get current time of the system
epochTime <- function() {
  return(as.integer(Sys.time()))
}

humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}


# saves the results of the survey to a file and records the time that the survey was completed
saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))
  
  #writes the survey to a csv file
  write.csv(x = data, file = file.path(responsesDirectory, fileName),
            row.names = FALSE, quote = TRUE)
}

#loads the responses of the user from the csv file into a dataframe
loadData <- function() {
  files <- list.files(file.path(responsesDirectory), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  
  #data <- dplyr::rbind_all(data)
  data <- do.call(bind_rows, data)
  data
}

#the directory where the responses get stores
responsesDirectory <- file.path("responses")

#CSS used throughout the app
appCSS <-
  ".mandatory_star { color: red; }
.shiny-input-container { margin-top: 25px; }
#submit_msg { margin-left: 15px; }
#error { color: red; }
body { background: #fcfcfc; }
#header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
"

#usernames that have access to the app for downloading the responses
adminUsers <- c("admin", "prof")


#this is where the actual shiny app starts.
shinyApp(
  #the ui portion of the app begins here
  ui <- fluidPage(
    title = "Thrift Store Opinion Survey",
    div(id = "header",
        h1("Thrift Store Opinion Survey")
    ),
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    
    #centers the respondent page
    fluidRow(align = "center",
      column(6,
             div(
               id = "form",
               #this is where the questions for questionaire starts. The selections are made here.
               textInput("name", labelMandatory("Name:"), ""),
               selectInput("gender", labelMandatory("Gender:"),
                           c("",  "Male", "Female", "Other")),
               selectInput("employment", labelMandatory("Employment Status:"),
                           c("",  "Employed full time", "Employed part time", "Unemployed", "Retired", "Prefer not to say")),
               selectInput("householdIncome", labelMandatory("Household Income:"),
                           c("",  "Less than $25,000", "$25,000-$50,000", "$50,000-$100,000", "$100,000-$200,000", "More than $200,000", "Prefer not to say")),
               selectInput("relationshipStatus", labelMandatory("Relationship Status:"),
                           c("",  "Single", "In a relationship", "Engaged", "Married", "Divorced", "Prefer not to say")),
               selectInput("educationLevel", labelMandatory("Level of Education:"),
                           c("",  "Some High School", "High School", "Some College", "Bachelor's Degree", "Master's Degree", "PhD or higher", "Trade School", "Prefer not to say")),
               selectInput("age", labelMandatory("Age:"),
                           c("",  "18-24", "25-30", "30-49", "50-64", "65+", "Prefer not to say")),
               selectInput("socialMediaUsage", labelMandatory("Do you follow secondhand stores on social media or internet?"),
               c("",  "Yes", "No")),
             selectInput("friendsFamilyUseUsedStores", labelMandatory("Do you have friends or family who shop at secondhand stores?"),
             c("",  "Yes", "No","I don't know")),
      selectInput("howPerMonthAppliances", labelMandatory("How many times per month do you shop for household appliances?"),
      c("",  "0", "1","2", "3+")),
    selectInput("howPerMonthClothing", labelMandatory("How many times per month do you shop for clothing?"),
    c("",  "0", "1","2", "3+")),
  selectInput("howPerMonthElectronics", labelMandatory("How many times per month do you shop for electronics?"),
  c("",  "0", "1","2", "3+")),
selectInput("howPerMonthFurniture", labelMandatory("How many times per month do you shop for furniture?"),
c("",  "0", "1","2", "3+")),
selectInput("whatDoWithClothing", labelMandatory("What do you do with clothing you don’t wear anymore?"),
c("",  "Throw them away","Sell them", "Donate them", "Other")),
selectInput("usedItemsLessPrestigious", labelMandatory("Used items are less prestigious than new items"),
c("",  "Strongly disagree", "disagree","neutral", "agree", "strongly agree")),
selectInput("usedItemsLowQuality", labelMandatory("Used items are lower quality than new items"),
c("",  "Strongly disagree", "disagree","neutral", "agree", "strongly agree")),
selectInput("usedItemsUnwantedHealth", labelMandatory("Using used items can lead to unwanted health effects"),
c("",  "Strongly disagree", "disagree","neutral", "agree", "strongly agree")),
selectInput("usedItemsNotTrendy", labelMandatory("Used items are not trendy"),
c("",  "Strongly disagree", "disagree","neutral", "agree", "strongly agree")),
selectInput("usedItemsLowClass", labelMandatory("People who usually use used items are from lower social classes"),
c("",  "Strongly disagree", "disagree","neutral", "agree", "strongly agree")),
selectInput("usedItemsDegrade", labelMandatory("Used items will degrade quicker than new items"),
c("",  "Strongly disagree", "disagree","neutral", "agree", "strongly agree")),
selectInput("usedItemsNotCool", labelMandatory("Used items are not cool"),
c("",  "Strongly disagree", "disagree","neutral", "agree", "strongly agree")),
selectInput("usedItemsFlaws", labelMandatory("Used items have more flaws than new items"),
c("",  "Strongly disagree", "disagree","neutral", "agree", "strongly agree")),
selectInput("usedItemsWorseHealth", labelMandatory("Using used items is worse for your health than using new items"),
c("",  "Strongly disagree", "disagree","neutral", "agree", "strongly agree")),
selectInput("usedItemsPoorTaste", labelMandatory("Using a used item shows poor taste"),
c("",  "Strongly disagree", "disagree","neutral", "agree", "strongly agree")),
selectInput("usedItemsDangerous", labelMandatory("Using a used item is dangerous"),
c("",  "Strongly disagree", "disagree","neutral", "agree", "strongly agree")),
selectInput("usedItemsLifespan", labelMandatory("Used items have a shorter lifespan than new items"),
c("",  "Strongly disagree", "disagree","neutral", "agree", "strongly agree")),
selectInput("itemPurchaseSecondHand", labelMandatory("Which of the following would you be more interested in purchasing at a secondhand store?"),
c("",  "Household appliances", "Clothing","Electronics", "Furniture", "Other")),
selectInput("reasonForSecondHandStore", labelMandatory("Why do you go to secondhand stores?"),
c("",  "Economical", "Ethical","Fashion", "I buy secondhand items online", "I don't buy secondhand items")),
selectInput("everShoppedSecondHand", labelMandatory("Have you ever shopped at a secondhand store?"),
c("",  "Yes", "No")),
selectInput("opinionSecondHandStores", labelMandatory("What is your opinion about secondhand stores?"),
c("",  "Good", "Neutral","Bad", "Don't have opinion")),
      
               #submit button for submitting the responses from the survey to a csv file
               actionButton("submit", "Submit", class = "btn-primary"),
               
               #message that will only appear if an error occurs
               shinyjs::hidden(
                 span(id = "submit_msg", "Submitting..."),
                 div(id = "error",
                     div(br(), tags$b("Error: "), span(id = "error_msg"))
                 )
               )
             ),
             
             #message that happens if the response was successfully recorded
             shinyjs::hidden(
               div(
                 id = "thankYouMessage",
                 h3("Thanks, your response was submitted successfully!"),
                 actionLink("submit_another", "Submit another response!")
               )
             ),
column(6,
       #admin panel for getting access to downloading the responses
       uiOutput("adminPanelContainer")
)
      ),
    )
  ),

  #server portion of the app starts here
  server = function(input, output, session) {
    
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
    
    #gets all of the responses from the survey and adds a timestamp for when the survey was completed
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- c(data, timestamp = epochTime())
      data <- t(data)
      data
    })    
    
    #submission button for starting the process of recording the responses to a csv file
    observeEvent(input$submit, {
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      #shows the message for if an error occured or a thank you message if the submission of the survey was done correctly
      tryCatch({
        saveData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankYouMessage")
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
        shinyjs::enable("submit")
        shinyjs::hide("submit_msg")
      })
    })
    
    #submit another survey response button
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankYouMessage")
    })
    
    #renders a the admin panel and the ui for downloading the csv file that holds the responses to the survey
    output$adminPanelContainer <- renderUI({
      if (!isAdmin()) return()
      
      div(
        id = "adminPanel",
        downloadButton("downloadButton", "Download responses"), br(), br()
      )
    })
    
    #this determines if the user is an admin or not, and it will allow a response table to be displayed if the viewer is an admin
    isAdmin <- reactive({
      is.null(session$user) || session$user %in% adminUsers
    })    
    output$responsesTable <- DT::renderDataTable({
      data <- loadData()
      data$timestamp <- as.POSIXct(data$timestamp, origin="1970-01-01")
      DT::datatable(
        data,
        rownames = FALSE,
        options = list(searching = FALSE, lengthChange = FALSE)
      )
    })
    
    #allows the users to download the csv file of the responses
    output$downloadButton <- downloadHandler(
      filename = function() { 
        sprintf("%s.csv", humanTime())
      },
      content = function(file) {
        write.csv(loadData(), file, row.names = FALSE)
      }
    )    
  }
)