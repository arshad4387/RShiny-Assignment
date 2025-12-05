library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(plotly)
library(rsconnect)

# ----------------------- Load data -----------------------
dig <- read.csv('DIG.csv')

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
    diabetes = DIABETES,
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

# Calculating trial outcomes
dig_df <- dig_df %>%
  mutate(
    # Cardiovascular mortality (using CVD diagnosis + death)
    cv_mortality = ifelse(death == "Death" & cvd == "Yes", 1, 0),
    
    # HF-related event (using WHF variable)
    hf_event = ifelse(whf == "Yes", 1, 0),
    
    # Non-cardiovascular hospitalisation 
    noncv_hosp = ifelse(hosp_hist == "Yes" & cvd == "No", 1, 0)
  )


# ==========================================================
#                        UI
# ==========================================================
ui <- navbarPage("DIG Trial Insights",
                 
                 tabPanel("Trial Outcomes & Overview",
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
                            h3("Results", style = "font-weight: bold; color:teal"),
                            fluidRow(
                              valueBoxOutput("cvMortalityBox"),
                              valueBoxOutput("hfEventBox"),
                              valueBoxOutput("noncvHospBox")
                            ),
                            h3("Overview", style = "font-weight: bold; color:teal"),
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
                            
                            fluidRow(
                              column(4, plotOutput("treatmentPie")),
                              column(4, plotOutput("ageHist")),
                              column(4, plotOutput("genderBar"))
                            )
                          )
                 ),
                 
                 # --------------------------------------------------------
                 # PAGE 2
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
                 # PAGE 3
                 # --------------------------------------------------------
                 tabPanel("Clinical Characteristics",
                          fluidPage(
                            h2("Clinical Profile of Patients"),
                            
                            fluidRow(
                              valueBoxOutput("cEF"),
                              valueBoxOutput("cHR"),
                              valueBoxOutput("cNYHA")
                            ),
                            
                            fluidRow(
                              valueBoxOutput("cCHFdur"),
                              valueBoxOutput("cHTN"),
                              valueBoxOutput("cDM")
                            ),
                            
                            fluidRow(
                              valueBoxOutput("cDigUse"),
                              valueBoxOutput("cCreat"),
                              valueBoxOutput("cBMI")
                            ),
                            
                            br(),
                            
                            fluidRow(
                              column(6, plotOutput("efHist")),
                              column(6, plotOutput("hrHist"))
                            ),
                            
                            fluidRow(
                              column(6, plotOutput("nyhaBar")),
                              column(6, plotOutput("chestxHist"))
                            )
                          )
                 ),
                 
                 # --------------------------------------------------------
                 # PAGE 4: Cohort
                 # --------------------------------------------------------
                 tabPanel("Cohort / Baseline",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("ageRange", "Age range (years):",
                                          min = floor(min(dig$AGE, na.rm = TRUE)),
                                          max = ceiling(max(dig$AGE, na.rm = TRUE)),
                                          value = c(min(dig$AGE, na.rm=TRUE), max(dig$AGE, na.rm=TRUE))),
                              
                              checkboxGroupInput("treatmentChoice", "Treatment arm:",
                                                 choices = levels(as.factor(dig$TRTMT)),
                                                 selected = levels(as.factor(dig$TRTMT))),
                              
                              selectInput("sexChoice", "Sex:", choices = c("All","Male","Female")),
                              width = 3
                            ),
                            
                            mainPanel(
                              fluidRow(
                                column(6, withSpinner(plotlyOutput("cohortAgeHist"))),
                                column(6, withSpinner(plotlyOutput("bmiBox")))
                              ),
                              fluidRow(
                                column(6, withSpinner(plotlyOutput("efByTreatment"))),
                                column(6, withSpinner(plotlyOutput("nyhaBar2")))
                              )
                            )
                          )
                 ),
                 
                 # --------------------------------------------------------
                 # PAGE 5: Exploration
                 # --------------------------------------------------------
                 tabPanel("Exploration",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("xvar","X variable:",
                                          choices = c("AGE","BMI","EJF_PER","CREAT","KLEVEL"),
                                          selected = "AGE"),
                              
                              selectInput("yvar","Y variable:",
                                          choices = c("NHOSP","HOSPDAYS","DEATHDAY"),
                                          selected = "NHOSP"),
                              
                              selectInput("colorBy","Color by:",
                                          choices = c("TRTMT","SEX","FUNCTCLS"),
                                          selected = "TRTMT"),
                              
                              checkboxInput("addSmooth","Add smoothing (geom_smooth)", value = TRUE),
                              width = 3
                            ),
                            
                            mainPanel(
                              withSpinner(plotlyOutput("scatterPlot")),
                              br(),
                              withSpinner(DTOutput("scatterSubset"))
                            )
                          )
                 ),
                 
                 # --------------------------------------------------------
                 # PAGE 6: Download
                 # --------------------------------------------------------
                 tabPanel("Download & Repro",
                          fluidPage(
                            h4("Download filtered data"),
                            downloadButton("downloadData", "Download CSV (current filters)"),
                            br(), br(),
                            h4("Session Info"),
                            verbatimTextOutput("sessionInfo")
                          )
                 )
)

# ==========================================================
#                        SERVER
# ==========================================================
server <- function(input, output) {
  
  # ---------------- Overview Filters ----------------
  filtered_overview <- reactive({
    df <- dig_df
    if(input$overview_treatment != "All")
      df <- df %>% filter(Treatment == input$overview_treatment)
    
    if(input$overview_gender != "All")
      df <- df %>% filter(Gender == input$overview_gender)
    
    df %>% filter(Age >= input$overview_age[1],
                  Age <= input$overview_age[2])
  })
  
  # Value Boxes
  output$vTotal <- renderValueBox({
    valueBox(nrow(filtered_overview()), "Total Patients", color = "blue")
  })
  
  output$vTreatment <- renderValueBox({
    valueBox(sum(filtered_overview()$Treatment == "Treatment"),
             "On Digoxin", color = "green")
  })
  
  output$vPlacebo <- renderValueBox({
    valueBox(sum(filtered_overview()$Treatment == "Placebo"),
             "On Placebo", color = "yellow")
  })
  
  output$vAge <- renderValueBox({
    valueBox(round(mean(filtered_overview()$Age, na.rm=TRUE),1),
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
  
  # ---- CARD 1: Cardiovascular Mortality ----
  output$cvMortalityBox <- shinydashboard::renderValueBox({
    count <- sum(filtered_overview()$cv_mortality, na.rm = TRUE)
    
    shinydashboard::valueBox(
      subtitle = "Cardiovascular Deaths",
      value = count,
      color = "red"
    )
  })
  
  # ---- CARD 2: HF-related Events ----
  output$hfEventBox <- shinydashboard::renderValueBox({
    count <- sum(filtered_overview()$hf_event, na.rm = TRUE)
    
    shinydashboard::valueBox(
      subtitle = "Worsening Heart Failure",
      value = count,
      color = "purple"
    )
  })
  
  # ---- CARD 3: Non-CV Hospitalisations ----
  output$noncvHospBox <- shinydashboard::renderValueBox({
    count <- sum(filtered_overview()$noncv_hosp, na.rm = TRUE)
    
    shinydashboard::valueBox(
      subtitle = "Non-Cardio Vascular Hospitalisations",
      value = count,
      color = "teal"
    )
  })
  
  
  # ---------------- Overview plots ----------------
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
      labs(title="Age Distribution")
  })
  
  output$genderBar <- renderPlot({
    ggplot(filtered_overview(), aes(Gender, fill=Gender)) +
      geom_bar() +
      theme_minimal() +
      labs(title="Gender Proportion")
  })
  
  # ---------------- Page 2: Hospital Filters ----------------
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
  #      PAGE 3: Clinical Characteristics
  # ======================================================
  
  output$cEF <- renderValueBox({
    valueBox(round(mean(dig_df$ef, na.rm=T),1), "Mean EF", color="red")
  })
  
  output$cHR <- renderValueBox({
    valueBox(round(mean(dig_df$heart_rate, na.rm=T),1),
             "Mean Heart Rate", color="purple")
  })
  
  output$cNYHA <- renderValueBox({
    nyha_num <- as.numeric(as.character(dig_df$nyha))
    perc <- mean(nyha_num >= 3, na.rm = TRUE) * 100
    valueBox(round(perc,1), "% NYHA III–IV", color="navy")
  })
  
  output$cCHFdur <- renderValueBox({
    valueBox(round(mean(dig_df$chf_duration, na.rm=T),1),
             "Mean CHF Duration (months)", color="olive")
  })
  
  output$cHTN <- renderValueBox({
    perc <- mean(dig_df$hypertension == "Yes", na.rm = TRUE) * 100
    valueBox(round(perc,1), "% with Hypertension", color = "red")
  })
  
  output$cDM <- renderValueBox({
    perc <- mean(dig_df$diabetes == 1, na.rm = TRUE) * 100
    valueBox(round(perc,1), "% with Diabetes", color = "orange")
  })
  
  output$cDigUse <- renderValueBox({
    perc <- mean(dig_df$digoxin == "Yes", na.rm = TRUE) * 100
    valueBox(round(perc,1), "% Using Digoxin (past week)", color = "green")
  })
  
  output$cCreat <- renderValueBox({
    med <- median(dig_df$creatinine, na.rm = TRUE)
    valueBox(round(med,2), "Median Creatinine (mg/dL)", color = "purple")
  })
  
  output$cBMI <- renderValueBox({
    valueBox(round(mean(dig_df$bmi, na.rm = TRUE),1),
             "Mean BMI", color="blue")
  })
  
  output$efHist <- renderPlot({
    ggplot(dig_df, aes(ef)) +
      geom_histogram(fill = "darkred", color = "black", bins = 20) +
      theme_minimal() +
      labs(title = "Ejection Fraction Distribution",x = "Ejection Fraction (%)",
           y = "Number of Patients")
  })
  
  output$hrHist <- renderPlot({
    ggplot(dig_df, aes(heart_rate)) +
      geom_histogram(fill = "orange", color = "black", bins = 20) +
      theme_minimal() +
      labs(title = "Heart Rate Distribution",x = "Heart Rate (beats per minute)",
        y = "Number of Patients")
  })
  
  output$nyhaBar <- renderPlot({
    ggplot(dig_df, aes(nyha, fill = nyha)) +
      geom_bar() +
      theme_minimal() +
      labs(title = "NYHA Class Distribution",x = "NYHA Class",
        y = "Number of Patients")
  })
  
  output$chestxHist <- renderPlot({
    ggplot(dig_df, aes(chestx)) +
      geom_histogram(fill = "steelblue", color = "black", bins = 20) +
      theme_minimal() +
      labs(title = "Chest X-Ray Score Distribution",x = "Chest X-Ray Score",
        y = "Number of Patients")
  })
  
  # ======================================================
  #    PAGE 4 : Cohort / Baseline Characteristics
  # ======================================================
  filtered_base <- reactive({
    df <- dig
    
    df <- df %>% filter(AGE >= input$ageRange[1],
                        AGE <= input$ageRange[2])
    
    df <- df %>% filter(TRTMT %in% input$treatmentChoice)
    
    if (input$sexChoice != "All")
      df <- df %>% filter(SEX == input$sexChoice)
    
    df
  })
  
  output$cohortAgeHist <- renderPlotly({
    p <- ggplot(filtered_base(), aes(x=AGE, fill=TRTMT)) + 
      geom_histogram(position="stack", bins=30) +
      labs(title="Age distribution by arm") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$bmiBox <- renderPlotly({
    p <- ggplot(filtered_base(), aes(x = factor(TRTMT, labels = c("Placebo", "Treatment")), y = BMI)) +
      geom_boxplot() +
      labs(title = "BMI by arm", x = "Treatment Arm") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$efByTreatment <- renderPlotly({
    p <- ggplot(filtered_base(), aes(x=TRTMT, y=EJF_PER)) +
      geom_jitter(width=0.2, alpha=0.6) +
      stat_summary(fun=mean, geom="point", shape=23, size=3, fill="white") +
      labs(title="Ejection fraction by arm")
    ggplotly(p)
  })
  
  output$nyhaBar2 <- renderPlotly({
    tb <- filtered_base() %>% count(FUNCTCLS, TRTMT)
    p <- ggplot(tb, aes(x=FUNCTCLS, y=n, fill=TRTMT)) +
      geom_bar(stat="identity", position="dodge") +
      labs(title="NYHA Class by arm")
    ggplotly(p)
  })
  
  
  # ---------------- Scatter Plot ----------------
  output$scatterPlot <- renderPlotly({
    df <- dig %>% filter(!is.na(.data[[input$xvar]]),
                         !is.na(.data[[input$yvar]]))
    
    p <- ggplot(df, aes_string(x=input$xvar, y=input$yvar, color=input$colorBy)) +
      geom_point(alpha=0.6) +
      theme_minimal() +
      labs(title = paste(input$yvar, "vs", input$xvar))
    
    if(input$addSmooth)
      p <- p + geom_smooth(method="loess", se=FALSE)
    
    ggplotly(p)
  })
  
  output$scatterSubset <- renderDT({
    
    df <- dig %>% 
      select(
        "Patient ID" = ID,
        "Treatment" = TRTMT,
        "Age" = AGE,
        "Gender" = SEX,
        all_of(input$xvar),
        all_of(input$yvar),
        all_of(input$colorBy)
      )
    
    # Rename the dynamic columns too
    names(df)[names(df) == input$xvar] <- input$xvar |> 
      stringr::str_replace_all("_", " ") |>
      stringr::str_to_title()
    
    names(df)[names(df) == input$yvar] <- input$yvar |>
      stringr::str_replace_all("_", " ") |>
      stringr::str_to_title()
    
    names(df)[names(df) == input$colorBy] <- input$colorBy |>
      stringr::str_replace_all("_", " ") |>
      stringr::str_to_title()
    
    datatable(df, options = list(pageLength = 8, scrollX = TRUE))
  })
  
  output$downloadData <- downloadHandler(
    filename=function() paste0("DIG_filtered_", Sys.Date(), ".csv"),
    content=function(file) readr::write_csv(filtered_base(), file)
  )
  
  output$sessionInfo <- renderPrint({ sessionInfo() })
}

shinyApp(ui, server)