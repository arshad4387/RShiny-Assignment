library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinycssloaders)
library(DT)


# ----------------------- Load data -----------------------
dig <- read.csv('/Users/arshad/Downloads/DIG-1.csv')

# Keep relevant variables + NEW ONES
dig_df <- dig %>% 
  select(
    "Patient ID" = ID,
    "Treatment" = TRTMT,
    "Age" = AGE,
    "Gender" = SEX,
    ef = EJF_PER,
    chestx = CHESTX,
    bmi = BMI,
    k_level = KLEVEL,
    creatinine = CREAT,
    chf_duration = CHFDUR,
    heart_rate = HEARTRTE,
    nyha = FUNCTCLS,
    hypertension = HYPERTEN,
    cvd = CVD,
    whf = WHF,
    digoxin = DIGUSE,
    hosp_hist = HOSP,
    hosp_days = HOSPDAYS,
    n_hosp = NHOSP,
    death = DEATH
  )

# ----------------------- Recode -----------------------
dig_df <- dig_df %>%
  mutate(
    Treatment = ifelse(Treatment == 1, "Treatment", "Placebo"),
    Gender = ifelse(Gender == 1, "Male", "Female"),
    nyha = factor(nyha, levels = c(1,2,3,4)),
    hypertension = ifelse(hypertension == 1, "Yes", "No"),
    cvd = ifelse(cvd == 1, "Yes", "No"),
    whf = ifelse(whf == 1, "Yes", "No"),
    digoxin = ifelse(digoxin == 1, "Yes", "No"),
    hosp_hist = ifelse(hosp_hist == 1, "Yes", "No"),
    death = ifelse(death == 1, "Death", "Alive")
  )


# ==========================================================
#                        UI
# ==========================================================
ui <- navbarPage("DIG Trial Insights",
                 # --------------------------------------------------------
                 # PAGE 1: OVERVIEW
                 # --------------------------------------------------------
                 tabPanel("Trial Overview",
                          fluidPage(
                            h2("Trial Summary"),
                            
                            # Filters
                            fluidRow(
                              column(4, selectInput("overview_treatment", "Select Treatment:",
                                                    choices = c("All", "Treatment", "Placebo"))),
                              column(4, selectInput("overview_gender", "Select Gender:",
                                                    choices = c("All", "Male", "Female"))),
                              column(4, sliderInput("overview_age", "Select Age Range:",
                                                    min = min(dig_df$"Age", na.rm = TRUE),
                                                    max = max(dig_df$"Age", na.rm = TRUE),
                                                    value = c(min(dig_df$"Age", na.rm = TRUE),
                                                              max(dig_df$"Age", na.rm = TRUE))))
                            ),
                            
                            # Summary boxes
                            fluidRow(
                              valueBoxOutput("vTotal"),
                              valueBoxOutput("vTreatment"),
                              valueBoxOutput("vPlacebo")
                            ),
                            fluidRow(
                              valueBoxOutput("vAge"),
                              valueBoxOutput("vHosp"),
                              valueBoxOutput("vDeaths")
                            ),  
                            
                            # Plots
                            fluidRow(
                              column(4, plotOutput("treatmentPie")),
                              column(4, plotOutput("ageHist")),
                              column(4, plotOutput("genderBar"))
                            )
                          )
                 ),
                 # --------------------------------------------------------
                 # PAGE 2: HOSPITALISATION
                 # --------------------------------------------------------
                 tabPanel("Hospitalisations",
                          fluidPage(
                            h2("Hospitalisation Overview"),
                            
                            fluidRow(
                              column(4, numericInput("minDays", "Minimum Hospitalisation Days:",
                                                     value = 0, min = 0)
                              ),
                              column(4,
                                     selectInput("hospHistory", "Hospital History:", choices = c("All", "Yes", "No"))
                              ),
                              column(4, sliderInput("numHospFilter", "Number of Hospitalisations:",
                                                    min = min(dig_df$n_hosp, na.rm = TRUE),
                                                    max = max(dig_df$n_hosp, na.rm = TRUE),
                                                    value = c(min(dig_df$n_hosp, na.rm = TRUE),
                                                              max(dig_df$n_hosp, na.rm = TRUE)))
                              )
                            ),                            
                            fluidRow(
                              column(4, selectInput("overview_death", "Death Status:",
                                                    choices = c("All", "Alive", "Death"))),
                              column(4, actionButton("applyHospFilter","Apply Filters"))
                            ),
                            tags$p("Please select the filters and click Apply Filters", 
                                   style = "font-weight: bold; color:red; margin-top: 10px;"),
                            br(),
                            fluidRow(
                              column(12, withSpinner(DTOutput("hospTable")))
                            )
                          )
                 ),
                 
                 # --------------------------------------------------------
                 # PAGE 3: CLINICAL CHARACTERISTICS (NEW!)
                 # --------------------------------------------------------
                 tabPanel("Clinical Characteristics",
                          fluidPage(
                            h2("Clinical Profile of Patients"),
                            
                            fluidRow(
                              valueBoxOutput("cEF"),
                              valueBoxOutput("cHR"),
                              valueBoxOutput("cNYHA"),
                              valueBoxOutput("cCHFdur")
                            ),
                            
                            fluidRow(
                              column(6, plotOutput("efHist")),
                              column(6, plotOutput("hrHist"))
                            ),
                            
                            fluidRow(
                              column(6, plotOutput("nyhaBar")),
                              column(6, plotOutput("chestxHist"))
                            )
                          )
                 )
)

# ==========================================================
#                        SERVER
# ==========================================================
server <- function(input, output) {
  
  # ------------ Overview Filter ------------
  filtered_overview <- reactive({
    df <- dig_df
    if(input$overview_treatment != "All")
      df <- df %>% filter(Treatment == input$overview_treatment)
    if(input$overview_gender != "All")
      df <- df %>% filter(Gender == input$overview_gender)
    df %>% filter(Age >= input$overview_age[1],
                  Age <= input$overview_age[2])
  })
  
  # ------------ Overview Value Boxes ------------
  output$vTotal <- renderValueBox({
    valueBox(nrow(filtered_overview()), "Total Patients", color = "blue")
  })
  
  output$vTreatment <- renderValueBox({
    valueBox(sum(filtered_overview()$"Treatment" == "Treatment"),
             "On Digoxin", color = "green")
  })
  
  output$vPlacebo <- renderValueBox({
    valueBox(sum(filtered_overview()$"Treatment" == "Placebo"),
             "On Placebo", color = "yellow")
  })
  
  output$vAge <- renderValueBox({
    valueBox(round(mean(filtered_overview()$"Age", na.rm=TRUE),1),
             "Mean Age", color="purple")
  })
  
  output$vHosp <- renderValueBox({
    valueBox(sum(filtered_overview()$hosp_hist == "Yes"),
             "Hospitalizations", color="maroon")
  })
  
  output$vDeaths <- renderValueBox({
    valueBox(sum(filtered_overview()$death == "Death"),
             "Total Deaths", color="black")
  })
  
  
  # ------------ Overview Plots ------------
  output$treatmentPie <- renderPlot({
    ggplot(filtered_overview(), aes(x="", fill=Treatment)) +
      geom_bar(width=1) +
      coord_polar("y") +
      theme_void() +
      labs(title="Treatment Allocation", fill = "Group")
  })
  
  output$ageHist <- renderPlot({
    ggplot(filtered_overview(), aes(Age)) +
      geom_histogram(color="black", fill="skyblue", bins=20) +
      theme_minimal() +
      labs(title="Age Distribution", x="Age", y="Number of patients")
  })
  
  output$genderBar <- renderPlot({
    ggplot(filtered_overview(), aes(Gender, fill=Gender)) +
      geom_bar() +
      theme_minimal() +
      labs(title="Gender Proportion", fill = "Gender", x = "Gender", y="Number of patients")
  })
  
  # ------------ Page 2: Patient Data ------------
  hosp_filtered <- eventReactive(input$applyHospFilter, {
    df <- dig_df %>%
      filter(hosp_days >= input$minDays) %>%
      filter(n_hosp >= input$numHospFilter[1],
             n_hosp <= input$numHospFilter[2])
    
    if (input$hospHistory != "All") {
      df <- df %>% filter(hosp_hist == input$hospHistory)
    }
    
    if (input$overview_death != "All") {
      df <- df %>% filter(death == input$overview_death)
    }
    
    df
  }, ignoreNULL = FALSE)
  
  
  output$hospTable <- renderDT({
    hosp_filtered() %>%
      select(
        Treatment,
        Age,
        Gender,
        "Number of Hospitalisations" = n_hosp,
        "Days to First/Last Hospitalisation" = hosp_days,
        "Hospital History" = hosp_hist,
        "Death Status" = death
      ) %>%
      arrange(desc(`Number of Hospitalisations`))
  }, options = list(pageLength=10, scrollX = TRUE))
  
  # ======================================================
  #      PAGE 3: NEW Clinical Characteristics
  # ======================================================
  
  # Value boxes
  output$cEF <- renderValueBox({
    valueBox(round(mean(dig_df$ef, na.rm=T),1), "Mean EF", color="red")
  })
  
  output$cHR <- renderValueBox({
    valueBox(round(mean(dig_df$heart_rate, na.rm=T),1),
             "Mean Heart Rate", color="purple")
  })
  
  output$cNYHA <- renderValueBox({
    valueBox(round(mean(dig_df$nyha >= 3, na.rm=T) * 100,1),
             "% NYHA III–IV", color="navy")
  })
  
  output$cCHFdur <- renderValueBox({
    valueBox(round(mean(dig_df$chf_duration, na.rm=T),1),
             "Mean CHF Duration (months)", color="olive")
  })
  
  # Plots
  output$efHist <- renderPlot({
    ggplot(dig_df, aes(ef)) +
      geom_histogram(fill="darkred", color="black", bins=20) +
      theme_minimal() +
      labs(title="Ejection Fraction Distribution", x="EF (%)", y="Count")
  })
  
  output$hrHist <- renderPlot({
    ggplot(dig_df, aes(heart_rate)) +
      geom_histogram(fill="orange", color="black", bins=20) +
      theme_minimal() +
      labs(title="Heart Rate Distribution", x="Heart Rate", y="Count")
  })
  
  output$nyhaBar <- renderPlot({
    ggplot(dig_df, aes(nyha, fill=nyha)) +
      geom_bar() +
      theme_minimal() +
      labs(title="NYHA Functional Class", x="NYHA Class", y="Count")
  })
  
  output$chestxHist <- renderPlot({
    ggplot(dig_df, aes(chestx)) +
      geom_histogram(fill="steelblue", color="black", bins=20) +
      theme_minimal() +
      labs(title="Chest X-Ray CTR Distribution", x="CTR", y="Count")
  })
  
}

shinyApp(ui, server)
