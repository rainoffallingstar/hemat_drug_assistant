### Reference
# Formula
# Body mass index, kg/m2 = weight, kg / (height, m)2
# Body surface area (the Mosteller formula), m2 = [ Height, cm x Weight, kg  / 3600 ]1/2

library(shiny)
library(shinyWidgets)
library(readxl)
library(fs)
library(dplyr)

# Define UI for application that draws a histogram
disease_list <- list()
disease_names<- list.dirs(paste0(getwd(), "/data/"),
                                 full.names = FALSE, recursive = FALSE)

for (i in 1:length(disease_names)){
  filelist <- list.files(paste0(getwd(), "/data/",disease_names[i],"/"), 
                                  pattern = ".xlsx", recursive = FALSE)
  for (a in 1:length(filelist) ){
    filelist[a] <- substring(filelist[a],1,nchar(filelist[a])-5)
  }
  disease_list[[i]] <- filelist
}
names(disease_list) <-disease_names

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
          pickerInput("regimens",
                      "Regimens Selected",
                      selected = "R-CHOP",
                      choices = disease_list
          ),
          prettyRadioButtons(
            inputId = "gender",
            label = "Choose Gender:", 
            choices = c("Common", "Female", "Male"),
            inline = TRUE, 
            status = "danger",
            fill = TRUE
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
          tabPanel("Recommendation", 
                   tableOutput("table1"),
                   tableOutput("table2")),
           #tabPanel("Reference",
            #        tableOutput("table3")),
          tabPanel("About me",
                   textOutput("about"),
                   textOutput("formula"),
                   textOutput("references"),
                   plotOutput("plt")
                  )
        )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  bmi <- reactive({
    w <- as.numeric(input$weight)
    h <- as.numeric(input$height)
    w /((h / 100)^2)
  })
  
  bsa <- reactive({
    # Body surface area (the Mosteller formula), m2 = [ Height, cm x Weight, kg  / 3600 ]1/2
    w <- as.numeric(input$weight)
    h <- as.numeric(input$height)
    if (input$gender == "Common"){
      (h * w / 3600)^0.5
    } else if (input$gender == "Female"){
    #0.00586×身高（cm）+0.0126×体重（kg）-0.0461
      0.00586 * h + 0.0126 * w - 0.0461
    } else{
    #0.00607×身高（cm）+0.0127×体重（kg）- 0.0698
      0.00607 * h + 0.0127 * w - 0.0698
    }
  })
  
  regimen <- reactive({
    n=1
    drug_path = paste0(getwd(), "/data/",disease_names[n],"/",input$regimens, ".xlsx")
    while (file.exists(drug_path) == FALSE && n <= length(disease_names)-1) {
      n = n+1
      drug_path = paste0(getwd(), "/data/",disease_names[n],"/",input$regimens, ".xlsx")
    }
    read_excel(drug_path,
               col_types = c("text", "text", "numeric", 
                              "numeric", "numeric", "numeric", "text"))
  })
  
  calculate_regimen <- reactive({
    df2 <- as.data.frame(regimen())
    bsa <- as.numeric(bsa())
    for (i in 1:nrow(df2)) {
      if (df2[i,2] == "monoclone"){
        df2[i,4] = df2[i,4]+ df2[i,3]
      }else if (df2[i,2] == "weight"){
        df2[i,4] = df2[i,3] * as.numeric(input$weight)
      }else if (df2[i,3] == 0) {
        df2[i,5] = df2[i,5] * bsa
        df2[i,6] = df2[i,6] * bsa
      } else {
        df2[i,4] = df2[i,3] * bsa
      }
    }
    df <- df2 
  })
  
  output$table1 <- renderTable({
    data.frame(Weight = input$weight,
               Height = input$height,
               BMI = bmi(),
               BSA = bsa())
  })
  
  output$table2 <- renderTable({
    as.data.frame(calculate_regimen()) %>% 
      select(-"类型")
  })
  
  #output$table3 <- renderTable({
   # regimen()
  #})
  
  output$about <- renderText({
   
    print(paste("This application is under developed by Yanhua Zheng,Dr.Qinchuan Yu and Prof.Xiaoxue Wang from the department of hematology,CMU1H, and Dr.Linfeng He from Institute for Empirical Social Science Research,XJTU. "))
  }
  )
  
  output$formula <- renderText({
    print("The calculation of Body Mass
          Index(BMI) is based on the famous formula BMI = weight,kg/(height,cm/100)^2, and Body Surface Area(BSA) is generally refered to the Mosteller formula, in which 
          m2 = [ Height, cm x Weight, kg  / 3600 ]^1/2. However, we also provide some alternations for the situation of different genders refered to the Songshan Zhao's formula, in which 
          the BSA for Female = 0.00586× height,cm + 0.0126× weight,kg -0.0461 and BSA for male = 0.00607× height,cm +0.0127 × weight,kg -0.0698.")
  })
  
  output$references <- renderText({
    print(" In addition, the citated drug does and regimens are taken from an chinese clinical handbook named 
          [血液科临床处方手册]")
  })
  
  output$plt <- renderPlot({
    img <- jpeg::readJPEG(paste0(getwd(),"/image/logo.jpg"))
    plot(as.raster(img))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
