# Load R packages
library(shiny)
library(shinythemes)
#install.packages("data.table")
library(data.table)


#################################################################### 
###################### Define UI ###################################
#################################################################### 

# Read in the RF model
model <- readRDS("model_rf2.rds")

ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "NYC Property Sales ",
                 tabPanel(title = "Home",
                          imageOutput("home_img"),
                          br(),
                          hr(),
                          h4(strong("About")),
                          p(style="text-align: justify; font-size = 40px",
                            "Welcome to our web app for predicting the sale price of properties in New York City! 
                            Our app is designed to provide you with accurate estimates for the sale price of properties 
                            based on a variety of factors such as location, property size, and features."),
                          p(style="text-align: justify; font-size = 25px",
                           "Using our app is easy: simply enter the required information about the property you're 
                            interested in, and we'll provide you with an estimate of the sale price. Our app uses 
                            advanced machine learning algorithms and data analysis techniques to provide you with 
                            the most accurate predictions possible."),
                          p(style="text-align: justify; font-size = 25px",
                            "Whether you're a real estate investor, a property buyer or seller, or 
                            simply curious about the current market prices in New York City, our app
                            can provide you with valuable insights into the real estate market. 
                            Our goal is to help you make informed decisions when it comes to buying
                            or selling properties, and we're committed to providing you with the most 
                            accurate and up-to-date information possible."),
                          
                          tags$blockquote("Thank you for choosing our app for your real estate needs. 
                                          We hope you find it useful and informative!"),
                          hr()
                 ),
                  tabPanel("Price Prediction",
                           sidebarPanel(tags$label(h3('Input Parameters')),
                                        numericInput("RESIDENTIAL.UNITS", 
                                                     label = "RESIDENTIAL UNITS", 
                                                     value = 5),
                                        numericInput("COMMERCIAL.UNITS", 
                                                     label = "COMMERCIAL UNITS", 
                                                     value = 5),
                                        numericInput("TOTAL.UNITS", 
                                                     label = "TOTAL UNITS", 
                                                     value = 10),
                                        numericInput("LAND.SQUARE.FEET", 
                                                     label = "LAND SQUARE FEET", 
                                                     value = 3000),
                                        numericInput("GROSS.SQUARE.FEET", 
                                                     label = "GROSS SQUARE FEET", 
                                                     value = 5000),
                                        numericInput("Property_Age", 
                                                     label = "Property Age", 
                                                     value = 100),
                                        selectInput("BOROUGH", label = "BOROUGH", 
                                                    choices = list("Manhattan" = "BOROUGH_X1","Bronx" = "BOROUGH_X2", "Brooklyn" = "BOROUGH_X3", "Queens" = "BOROUGH_X4","Staten Island" = "BOROUGH_X5"), 
                                                    selected = "Manhattan" ),
                                        selectInput("TAX.CLASS.AT.TIME.OF.SALE", label = "TAX CLASS AT TIME OF SALE", 
                                                    choices = list("Class 1" = "TAX.CLASS.AT.TIME.OF.SALE_X1","Class 2" = "TAX.CLASS.AT.TIME.OF.SALE_X2", "Class 4" = "TAX.CLASS.AT.TIME.OF.SALE_X4"), 
                                                    selected = "Class 1"),
                                        selectInput("Neighborhood_Category", label = "Neighborhood Category", 
                                                    choices = list("Central Brooklyn" = "Neighborhood_Category_central_brooklyn","Central Queens" = "Neighborhood_Category_central_queens" , 
                                                                   "Eastern Brooklyn" = "Neighborhood_Category_eastern_brooklyn", "Lower Manhattan" ="Neighborhood_Category_Lower.Manhattan"     ,
                                                                   "Mid Islands Staten" = "Neighborhood_Category_mid_islands_staten" , "Midtown Manhattan" = "Neighborhood_Category_Midtown.Manhattan" ,
                                                                   "North Shore Staten" = "Neighborhood_Category_north_shore_staten","Northeast Bronx" = "Neighborhood_Category_northeast_bronx" ,
                                                                   "Northeastern Queens" = "Neighborhood_Category_northeastern_queens", "Northern Brooklyn" = "Neighborhood_Category_northern_brooklyn" ,
                                                                   "Northwest Bronx" =   "Neighborhood_Category_northwest_bronx", "Northwest Brooklyn" = "Neighborhood_Category_northwest_brooklyn"  ,
                                                                   "Northwestern Queens"=  "Neighborhood_Category_northwestern_queens","South Shore Staten" =  "Neighborhood_Category_south_shore_staten",
                                                                   "Southeast Bronx" = "Neighborhood_Category_southeast_bronx","Southeastern Queens" = "Neighborhood_Category_southeastern_queens", 
                                                                   "Southern Brooklyn" = "Neighborhood_Category_southern_brooklyn"  , "Southwes Bronx" = "Neighborhood_Category_southwest_bronx" ,
                                                                   "Southwest Brooklyn" = "Neighborhood_Category_southwest_brooklyn","Southwestern Queens" ="Neighborhood_Category_southwestern_queens", 
                                                                   "The Rockaways" = "Neighborhood_Category_the_rockaways" , "Upper Manhattan" ="Neighborhood_Category_Upper.Manhattan" ,"West Side" ="Neighborhood_Category_West.Side" ), 
                                                    selected = "Central Brooklyn"),
                                        selectInput("Building_class_cat", label = "Building Class Catrgory", 
                                                    choices = list("Class A" = "Building_class_cat2_class_A","Class B" = "Building_class_cat2_class_B", "Class C" = "Building_class_cat2_class_C", "Class D" ="Building_class_cat2_class_D","Class R" = "Building_class_cat2_class_R", "Class S" = "Building_class_cat2_class_S", "Other" = "Building_class_cat2_class_other"), 
                                                    selected = "Class A"),
                                        actionButton("submitbutton", "Submit", 
                                                     class = "btn btn-primary")), # sidebarPanel 
                           
                           mainPanel(
                             tags$label(h3('Predicted Sales Price in Dollers')), # Status/Output Text Box
                             verbatimTextOutput('contents'),
                             tags$blockquote( tableOutput('tabledata') ),# Prediction results table
                             imageOutput("output_image")
                             ) 
                            # mainPanel
                           
                  )
                  
                ) # navbarPage
) # fluidPage


#################################################################### 
###################### Define server ###############################
#################################################################### 

server <- function(input, output) {
  
  
  # Define a function to calculate missing_value variable

  
  # Define a function to predict sale price based on user inputs
  datasetInput <- reactive({
      # Create a new data frame with user input and missing_value column
      df<- data.frame(
        
        Name = c("RESIDENTIAL.UNITS",
                 "COMMERCIAL.UNITS",
                 "TOTAL.UNITS",
                 "LAND.SQUARE.FEET",
                 "GROSS.SQUARE.FEET",
                 "Property_Age",
                 "missing_value",
                 "BOROUGH_X1",
                 "BOROUGH_X2",
                 "BOROUGH_X3",
                 "BOROUGH_X4",
                 "BOROUGH_X5",
                 "TAX.CLASS.AT.TIME.OF.SALE_X1",
                 "TAX.CLASS.AT.TIME.OF.SALE_X2",
                 "TAX.CLASS.AT.TIME.OF.SALE_X4",
                 "Neighborhood_Category_central_brooklyn",
                 "Neighborhood_Category_central_queens",
                 "Neighborhood_Category_eastern_brooklyn",
                 "Neighborhood_Category_Lower.Manhattan",
                 "Neighborhood_Category_mid_islands_staten",
                 "Neighborhood_Category_Midtown.Manhattan",
                 "Neighborhood_Category_north_shore_staten",
                 "Neighborhood_Category_northeast_bronx",
                 "Neighborhood_Category_northeastern_queens",
                 "Neighborhood_Category_northern_brooklyn",
                 "Neighborhood_Category_northwest_bronx",
                 "Neighborhood_Category_northwest_brooklyn",
                 "Neighborhood_Category_northwestern_queens",
                 "Neighborhood_Category_south_shore_staten",
                 "Neighborhood_Category_southeast_bronx",
                 "Neighborhood_Category_southeastern_queens",
                 "Neighborhood_Category_southern_brooklyn",
                 "Neighborhood_Category_southwest_bronx",
                 "Neighborhood_Category_southwest_brooklyn",
                 "Neighborhood_Category_southwestern_queens",
                 "Neighborhood_Category_the_rockaways",
                 "Neighborhood_Category_Upper.Manhattan",
                 "Neighborhood_Category_West.Side",
                 "Building_class_cat2_class_A",
                 "Building_class_cat2_class_B",
                 "Building_class_cat2_class_C",
                 "Building_class_cat2_class_D",
                 "Building_class_cat2_class_other",
                 "Building_class_cat2_class_R",
                 "Building_class_cat2_class_S"     ),
        value=c(RESIDENTIAL.UNITS = input$RESIDENTIAL.UNITS,
        COMMERCIAL.UNITS = input$COMMERCIAL.UNITS,
        TOTAL.UNITS = input$TOTAL.UNITS,
        LAND.SQUARE.FEET = input$LAND.SQUARE.FEET ,
        GROSS.SQUARE.FEET = input$GROSS.SQUARE.FEET,
        Property_Age = input$Property_Age,
        missing_value =ifelse(((input$LAND.SQUARE.FEET ) +(input$GROSS.SQUARE.FEET))==0,1,0),
        BOROUGH_X1 = ifelse(input$BOROUGH == "BOROUGH_X1", 1, 0),
        BOROUGH_X2 = ifelse(input$BOROUGH == "BOROUGH_X2", 1, 0),
        BOROUGH_X3 = ifelse(input$BOROUGH == "BOROUGH_X3", 1, 0),
        BOROUGH_X4 = ifelse(input$BOROUGH == "BOROUGH_X4", 1, 0),
        BOROUGH_X5 = ifelse(input$BOROUGH == "BOROUGH_X5", 1, 0),
        TAX.CLASS.AT.TIME.OF.SALE_X1 = ifelse(input$TAX.CLASS.AT.TIME.OF.SALE== "TAX.CLASS.AT.TIME.OF.SALE_X1", 1, 0),
        TAX.CLASS.AT.TIME.OF.SALE_X2 = ifelse(input$TAX.CLASS.AT.TIME.OF.SALE== "TAX.CLASS.AT.TIME.OF.SALE_X2", 1, 0),
        TAX.CLASS.AT.TIME.OF.SALE_X4 = ifelse(input$TAX.CLASS.AT.TIME.OF.SALE=="TAX.CLASS.AT.TIME.OF.SALE_X4", 1, 0),
        Neighborhood_Category_central_brooklyn = ifelse(input$Neighborhood_Category == "Neighborhood_Category_central_brooklyn", 1, 0),
        Neighborhood_Category_central_queens = ifelse(input$Neighborhood_Category== "Neighborhood_Category_central_queens", 1, 0),
        Neighborhood_Category_eastern_brooklyn = ifelse(input$Neighborhood_Category == "Neighborhood_Category_eastern_brooklyn ", 1, 0),
        Neighborhood_Category_Lower.Manhattan = ifelse(input$Neighborhood_Category== " Neighborhood_Category_Lower.Manhattan", 1, 0),
        Neighborhood_Category_mid_islands_staten  = ifelse(input$Neighborhood_Category== " Neighborhood_Category_mid_islands_staten ", 1, 0),
        Neighborhood_Category_Midtown.Manhattan = ifelse(input$Neighborhood_Category== "Neighborhood_Category_Midtown.Manhattan ", 1, 0),
        Neighborhood_Category_north_shore_staten  = ifelse(input$Neighborhood_Category== "Neighborhood_Category_north_shore_staten ", 1, 0),
        Neighborhood_Category_northeast_bronx  = ifelse(input$Neighborhood_Category== " Neighborhood_Category_northeast_bronx", 1, 0),
        Neighborhood_Category_northeastern_queens  = ifelse(input$Neighborhood_Category== "Neighborhood_Category_northeastern_queens", 1, 0),
        Neighborhood_Category_northern_brooklyn= ifelse(input$Neighborhood_Category== "Neighborhood_Category_northern_brooklyn", 1, 0),
        Neighborhood_Category_northwest_bronx= ifelse(input$Neighborhood_Category== "Neighborhood_Category_northwest_bronx", 1, 0),
        Neighborhood_Category_northwest_brooklyn = ifelse(input$Neighborhood_Category== "Neighborhood_Category_northwest_brooklyn ", 1, 0),
        Neighborhood_Category_northwestern_queens = ifelse(input$Neighborhood_Category== "Neighborhood_Category_northwestern_queens", 1, 0),        
        Neighborhood_Category_south_shore_staten = ifelse(input$Neighborhood_Category== "Neighborhood_Category_south_shore_staten", 1, 0),
        Neighborhood_Category_southeast_bronx = ifelse(input$Neighborhood_Category== "Neighborhood_Category_southeast_bronx", 1, 0),
        Neighborhood_Category_southeastern_queens = ifelse(input$Neighborhood_Category== "Neighborhood_Category_southeastern_queens", 1, 0),
        Neighborhood_Category_southern_brooklyn  = ifelse(input$Neighborhood_Category== "Neighborhood_Category_southern_brooklyn ", 1, 0),
        Neighborhood_Category_southwest_bronx = ifelse(input$Neighborhood_Category== "Neighborhood_Category_southwest_bronx", 1, 0),
        Neighborhood_Category_southwest_brooklyn = ifelse(input$Neighborhood_Category== "Neighborhood_Category_southwest_brooklyn", 1, 0),
        Neighborhood_Category_southwestern_queens= ifelse(input$Neighborhood_Category== "Neighborhood_Category_southwestern_queens", 1, 0),
        Neighborhood_Category_the_rockaways= ifelse(input$Neighborhood_Category== "Neighborhood_Category_the_rockaways", 1, 0),
        Neighborhood_Category_Upper.Manhattan = ifelse(input$Neighborhood_Category== "Neighborhood_Category_Upper.Manhattan", 1, 0),
        Neighborhood_Category_West.Side= ifelse(input$Neighborhood_Category== "Neighborhood_Category_West.Side", 1, 0),
        Building_class_cat2_class_A= ifelse(input$Building_class_cat == "Building_class_cat2_class_A", 1, 0),
        Building_class_cat2_class_B= ifelse(input$Building_class_cat == "Building_class_cat2_class_B", 1, 0),
        Building_class_cat2_class_C= ifelse(input$Building_class_cat == "Building_class_cat2_class_C", 1, 0),
        Building_class_cat2_class_D= ifelse(input$Building_class_cat == "Building_class_cat2_class_D", 1, 0),
        Building_class_cat2_class_other= ifelse(input$Building_class_cat == "Building_class_cat2_class_other", 1, 0),
        Building_class_cat2_class_R= ifelse(input$Building_class_cat == "Building_class_cat2_class_R", 1, 0),
        Building_class_cat2_class_S= ifelse(input$Building_class_cat == "Building_class_cat2_class_S", 1, 0))
        )
      
      
      df <- rbind(df)
      input <- transpose(df)
      write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
      
      test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
      
      log_Output <- data.frame(Predictedion =predict(model,test))
      Output=exp(log_Output)
      print(Output)
      
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
  output$home_img <- renderImage({
    
    list(src = "pic.jpg",
         width = "100%",
         height = 330)
    
  }, deleteFile = F)
  
  output$output_image <- renderImage({
    list(src = "pic2.jpg",
         width = "100%",
         height = 300) 
  }, deleteFile = FALSE)
  
  
  
}



####################################
# Create the shiny app             
####################################
shinyApp(ui = ui, server = server)



