library(shiny)
library(tidyverse)

dig <- read.csv("DIG.csv")
dig_df <- dig %>% 
  select("patient_id" = "ID",
         "treatment" = "TRTMT",
         "age" = "AGE",
         "gender" = "SEX",
         "bmi" = "BMI",
         "k_level" = "KLEVEL",
         "creatinine" = "CREAT",
         "dia_bp" = "DIABP",
         "sys_bp" = "SYSBP",
         "hypertension" = "HYPERTEN",
         "cvd" = "CVD",
         "whf" = "WHF",
         "digoxin" = "DIG",
         "hosp_hist" = "HOSP",
         "hosp_days" = "HOSPDAYS",
         "death" = "DEATH",
         "death_day" = "DEATHDAY"
  )
dig_df <- dig_df %>%
  mutate(treatment = recode(treatment, '0' = "Placebo", '1' = "Treatment"),
         gender = recode(gender, '1' = "Male", '2' = "Female"),
         hypertension = recode(hypertension, '1' = "Yes", '0' = "No"),
         cvd = recode(cvd, '1' = "Yes", '0' = "No"),
         whf = recode(whf, '1' = "Yes", '0' = "No"),
         digoxin = recode(digoxin, '1' = "Yes", '0' = "No"),
         hosp_hist = recode(hosp_hist, '1' = "Yes", '0' = "No"),
         death = recode(death, '0' = "Alive", '1' = "Death"))


ui <- fluidPage(
  titlePanel("DIG Trial Dashboard: Safety and Efficacy of Digoxin in Heart Failure"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "treatment", label = "Treatment / Placebo", choices = c("Treatment", "Placebo"), multiple = TRUE),
      radioButtons(inputId = "gender", label = "Select Gender", choices = c("Male", "Female")),
      sliderInput(inputId = "age", label = "Select the Age Range:", min = 18, max = 100,value = c(20, 60)),
      
      
    ),
    mainPanel(
      plotOutput("plot1")
    )
  )
)

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    dig_df %>%
    filter(treatment == input$treatment) %>%
      filter(gender == input$gender) %>%
      filter(age >= input$age[1], age <= input$age[2]) %>%
      ggplot(aes(x = treatment, y = age, fill = treatment)) +
      geom_violin(alpha = 0.3) +
      geom_boxplot(width = 0.2, outlier.size = 0) +
      theme_minimal() +
      labs(title = "Age Distribution by Treatment Group",
           x = "Treatment Group",
           y = "Age")
  })
}

shinyApp(ui, server)
