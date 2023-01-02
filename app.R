### Reference
# Formula
# Body mass index, kg/m2 = weight, kg / (height, m)2
# Body surface area (the Mosteller formula), m2 = [ Height, cm x Weight, kg  / 3600 ]1/2

library(shiny)
library(readxl)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Hematological Drug Assistant"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          textInput("weight",
                    "Weight(KG)",
                    "50"),
          textInput("height",
                    "Height(CM)",
                    "170"),
          selectInput("regimens",
                      "Regimens Selected",
                      selected = "R-CHOP",
                      choices = c("R-CHOP", "regimen2"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
          tabPanel("Recommendation", 
                   tableOutput("table1"),
                   tableOutput("table2")),
           tabPanel("Reference",
                    tableOutput("table3")),
          tabPanel("About me",
                   textOutput("about"),
                   plotOutput("plt"),)
        )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  bmi <- reactive({
    w <- input$weight
    h <- input$height
    w /((h / 100)^2)
  })
  
  bsa <- reactive({
    # Body surface area (the Mosteller formula), m2 = [ Height, cm x Weight, kg  / 3600 ]1/2
    w <- input$weight
    h <- input$height
    (h * w / 3600)^0.5
  })
  
  regimen <- reactive({
    read_excel(paste0(getwd(), "/data/", input$regimens, ".xlsx"),
               col_types = c("text", "text", "numeric", 
                              "numeric", "numeric", "text"))
  })
  
  calculate_regimen <- reactive({
    df <- regimen()
    bsa <- bsa()
    for (i in nrow(df)) {
      if (df[i,2] == "monoclone"){
        print("no more operation")
      }else if (is.na(df[i,3]) == TRUE) {
        df[i,4] = df[i,4] * bsa
        df[i,5] = df[i,5] * bsa
      } else {
        df[i,3] = df[i,3] * bsa
      }
    }
  })
  
  output$table1 <- renderTable({
    data.frame(Weight = input$weight,
               Height = input$height,
               BMI = bmi(),
               BSA = bsa())
  })
  
  output$table2 <- renderTable({
    calculate_regimen()
  })
  
  output$table3 <- renderTable({
    regimen()
  })
  
  output$about <- renderText({
    df <- regimen()
    print(paste("This application is under developed by fallingstar, The calculation of Body Mass
          Index(BMI) and Body Surface Area(BSA) is refered to the Mosteller formula, in which 
          m2 = [ Height, cm x Weight, kg  / 3600 ]1/2. In addition, the refered drug does and regimens are taken according to 
          血液科临床处方手册",df[1,2]))
  }
  )
  
  output$plt <- renderPlot({
    img <- jpeg::readJPEG(paste0(getwd(),"/image/logo.jpg"))
    plot(as.raster(img))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
