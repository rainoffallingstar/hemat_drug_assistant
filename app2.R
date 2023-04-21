### Reference
# Formula
# Body mass index, kg/m2 = weight, kg / (height, m)2
# Body surface area (the Mosteller formula), m2 = [ Height, cm x Weight, kg  / 3600 ]1/2

library(shiny)
library(shinyWidgets)
library("shinythemes")
library(readxl)
library(fs)
library(dplyr)
library(stringr)

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

# define  functions to process the strings in the excel files
# make all drug names display in the same way
update_druglist_title <- function(x,y) {
  raw_data <- x
  for (i in 1:nrow(raw_data)){
    raw_data[i,y] <- raw_data[i,y] %>% 
      str_to_lower() %>% 
      str_to_sentence()
  }
  return(raw_data)
}
database <- read_excel("data/standerd_drug_names.xlsx") %>% 
  update_druglist_title(1) %>% 
  update_druglist_title(2)
                          
  
get_druglist_title <- function(x,y,z) {
  raw_data <- x
  for (i in 1:nrow(raw_data)){
    if (str_detect(raw_data[i,y],"[\\p{Han}]")){
      id <- grep(raw_data[i,y],database$药名)
    }else{
      pro <- raw_data[i,y] %>% 
        str_to_lower() %>% 
        str_to_sentence()
      id <- grep(pro,database$Drugs)
    }
    
    if (is.na(id[1])){
      raw_data[i,y] <- raw_data[i,y]
    }else {
      raw_data[i,y] <- database[id[1],z] 
    }
    
  }
  return(raw_data)
}

# Define a function to switch all text on the interface between Chinese and English
switch_language <- function(lang) {
  if (lang == "Chinese") {
    updateTextInput(session, "weight", "体重(KG)", "50")
    updateTextInput(session, "height", "身高(CM)", "170")
    updatePickerInput(session, "regimens", "选择方案", selected = "R-CHOP", choices = disease_list)
    updatePrettyRadioButtons(session, "gender", "选择性别:", choices = c("通用", "女性", "男性"), inline = TRUE, status = "danger", fill = TRUE)
    updateSwitchInput(session, "Id078", onLabel = "En", offLabel = "中")
    # Add a switch button to change the tabpanel titles between Chinese and English
    observeEvent(input$Id078, {
      if (input$Id078 == TRUE) {
        updateTabsetPanel(session, "tabs",
                          tabPanel("患者信息"),
                          tabPanel("药物选择"),
                          tabPanel("方案推荐"),
                          tabPanel("药物剂量"),
                          tabPanel("药物副作用"),
                          tabPanel("药物相互作用"),
                          tabPanel("药物说明"))
      } else {
        updateTabsetPanel(session, "tabs",
                          tabPanel("Patient Info"),
                          tabPanel("Drug Selection"),
                          tabPanel("Regimen Recommendation"),
                          tabPanel("Drug Dosage"),
                          tabPanel("Drug Side Effects"),
                          tabPanel("Drug Interactions"),
                          tabPanel("Drug Instructions"))
      }
    })
  } else {
    updateTextInput(session, "weight", "Weight(KG)", "50")
    updateTextInput(session, "height", "Height(CM)", "170")
    updatePickerInput(session, "regimens", "Select Regimen", selected = "R-CHOP", choices = disease_list)
    updatePrettyRadioButtons(session, "gender", "Select Gender:", choices = c("All", "Female", "Male"), inline = TRUE, status = "danger", fill = TRUE)
    updateSwitchInput(session, "Id078", onLabel = "中", offLabel = "En")
    # Add a switch button to change the tabpanel titles between Chinese and English
    observeEvent(input$Id078, {
      if (input$Id078 == TRUE) {
        updateTabsetPanel(session, "tabs",
                          tabPanel("患者信息"),
                          tabPanel("药物选择"),
                          tabPanel("方案推荐"),
                          tabPanel("药物剂量"),
                          tabPanel("药物副作用"),
                          tabPanel("药物相互作用"),
                          tabPanel("药物说明"))
      } else {
        updateTabsetPanel(session, "tabs",
                          tabPanel("Patient Info"),
                          tabPanel("Drug Selection"),
                          tabPanel("Regimen Recommendation"),
                          tabPanel("Drug Dosage"),
                          tabPanel("Drug Side Effects"),
                          tabPanel("Drug Interactions"),
                          tabPanel("Drug Instructions"))
      }
    })
  }
}

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("journal"),
                titlePanel("Hematology Drug Assistant"),
                sidebarLayout(
                  sidebarPanel(
                    textInput("weight", "Weight(KG)", "50"),
                    textInput("height", "Height(CM)", "170"),
                    pickerInput("regimens", "Select Regimen", selected = "R-CHOP", choices = disease_list),
                    prettyRadioButtons("gender", "Select Gender:", choices = c("All", "Female", "Male"), inline = TRUE, status = "danger", fill = TRUE),
                    switchInput("Id078", "中/En", onLabel = "中", offLabel = "En")
                  ),
                  mainPanel(
                    tabsetPanel(id = "tabs",
                                tabPanel("Patient Info"),
                                tabPanel("Drug Selection"),
                                tabPanel("Regimen Recommendation"),
                                tabPanel("Drug Dosage"),
                                tabPanel("Drug Side Effects"),
                                tabPanel("Drug Interactions"),
                                tabPanel("Drug Instructions")
                    )
                  )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observe({
    switch_language(input$Id078)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

