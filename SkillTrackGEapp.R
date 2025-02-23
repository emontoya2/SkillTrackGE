library(shiny)
library(dplyr)
library(shinythemes)
# Basic apppearance ---will update.

df <- read.csv("SkillTrackGE_data_example.csv")


ui <- fluidPage(
  titlePanel("Completion Rates of General Education Areas A1, A2, A3, or B4"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("geArea", 
                  "Select General Education Area:", 
                  choices = c("A1", "A2", "A3", "B4")),
      selectInput("conditionVar1", 
                  "Condition on First Variable:", 
                  choices = c("None", "School", "Major", "Term admitted",   "Class Standing")),
      uiOutput("conditionalChoices1"), # Dynamically rendered based on conditionVar1 selection
      selectInput("conditionVar2", 
                  "Condition on Second Variable:", 
                  choices = c("None", "School", "Major", "Term admitted",   "Class Standing")),
      uiOutput("conditionalChoices2") # Dynamically rendered based on conditionVar2 selection
    ),
    
    
    mainPanel(
      textOutput("unconditionalProp"),
      textOutput("conditionalProp")
    )
  )
)


server <- function(input, output, session) {
  
  # Unconditional prop calc  
  output$unconditionalProp <- renderText({
    geArea <- input$geArea
    prop <- mean(df[[geArea]] == "N", na.rm = TRUE)
    paste("Unconditional proportion not completed for", geArea, ":", sprintf("%.2f%%", prop * 100))
  })
  
  # Reactivity for the first conditional dropdown
  output$conditionalChoices1 <- renderUI({
    req(input$conditionVar1)
    if(input$conditionVar1 != "None") {
      # If the first variable is selected, show the choices related to it
      if(input$conditionVar1 %in% c("Term admitted", "Class Standing")) {
        checkboxGroupInput("conditionVal1", 
                           paste("Select", input$conditionVar1, ":"), 
                           choices = levels(df[[input$conditionVar1]]))
      } else {
        selectInput("conditionVal1", 
                    paste("Select", input$conditionVar1, ":"), 
                    choices = unique(df[[input$conditionVar1]]),
                    multiple = FALSE)
      }
    }
  })
  
  # Reactivity for the second conditional dropdown
  output$conditionalChoices2 <- renderUI({
    req(input$conditionVar2)
    if(input$conditionVar2 != "None" && input$conditionVar2 != input$conditionVar1) {
      # If the second variable is selected, and it's different from the first, show the choices related to it
      if(input$conditionVar2 %in% c("Term admitted", "Class Standing")) {
        checkboxGroupInput("conditionVal2", 
                           paste("Select", input$conditionVar2, ":"), 
                           choices = levels(df[[input$conditionVar2]]))
      } else {
        selectInput("conditionVal2", 
                    paste("Select", input$conditionVar2, ":"), 
                    choices = unique(df[[input$conditionVar2]]),
                    multiple = FALSE)
      }
    }
  })
  
  # Conditional prop  calcs
  output$conditionalProp <- renderText({
    req(input$geArea)
    geArea <- input$geArea
    subset_df <- df
    
    # Apply 1st condition if set
    if(input$conditionVar1 != "None") {
      conditionVar1 <- input$conditionVar1
      conditionVal1 <- input$conditionVal1
      subset_df <- subset_df[subset_df[[conditionVar1]] %in% conditionVal1, ]
    }
    
    # Apply 2nd condition if set and it's different from the first
    if(input$conditionVar2 != "None" && input$conditionVar2 != input$conditionVar1) {
      conditionVar2 <- input$conditionVar2
      conditionVal2 <- input$conditionVal2
      subset_df <- subset_df[subset_df[[conditionVar2]] %in% conditionVal2, ]
    }
    
    # Calculate the proportion
    prop <- mean(subset_df[[geArea]] == "N", na.rm = TRUE)
    
    # Create the condition strings for display
    conditionStr1 <- if(input$conditionVar1 != "None") paste(conditionVar1, "=", paste(conditionVal1, collapse = ", ")) else ""
    conditionStr2 <- if(input$conditionVar2 != "None" && input$conditionVar2 != input$conditionVar1) paste(conditionVar2, "=", paste(conditionVal2, collapse = ", ")) else ""
    
    # Combine the condition strings and output the result
    conditions <- if (nchar(conditionStr1) > 0 && nchar(conditionStr2) > 0) {
      paste(conditionStr1, "and", conditionStr2)
    } else {
      paste(conditionStr1, conditionStr2)
    }
    paste("Conditional proportion not completed for", geArea, ":", sprintf("%.2f%%", prop * 100), "given", conditions)
  })
  
}

# Run the application 
# shinyApp(ui = ui, server = server)

# Run the application 
shinyApp(ui = ui, server = server)