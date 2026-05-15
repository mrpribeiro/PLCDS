library(shiny)
library(ggplot2)
library(scales)

ui <- fluidPage(
        titlePanel("Heart Failure Risk (Logistic Model)"),
        
        tabsetPanel(
                # 1) Study description / user benefit
                tabPanel(
                        "Study",
                        tags$div(
                                style = "max-width: 1000px; margin-top: 10px;",
                                tags$h3("Abstract"),
                                tags$p(
                                        "This study examines the association between habitual dietary sodium and protein intake and the self-reported prevalence of heart failure (HF) in the US adult population, adjusting for socioeconomic determinants and metabolic comorbidities. Data from four combined cycles of the National Health and Nutrition Examination Survey (NHANES 2011–2018) were analysed using complex survey-weighted logistic regression. The sample includes adults aged 18 and older with complete covariate data. Results are reported as weighted Odds Ratios (OR) with 95% confidence intervals, representative of the non-institutionalised US adult population."
                                ),
                                
                                tags$hr(),
                                
                                tags$h3("What you can do in this app"),
                                tags$ul(
                                        tags$li("Enter your diet and clinical profile (e.g., sodium, protein, blood pressure, BMI)."),
                                        tags$li("Explore “what-if” scenarios (e.g., lowering sodium) and see how the model prediction changes."),
                                        tags$li("Compare your sodium and protein intake to a US reference population using percentiles."),
                                        tags$li("Use the curves to understand how changing one variable at a time affects the model estimate (holding others constant).")
                                ),
                                
                                tags$p(
                                        style = "color:#8a6d3b;",
                                        tags$b("Important: "),
                                        "This is for informational purposes only and does not provide medical advice, diagnosis, or treatment."
                                )
                        )
                ),
                
                # 2) Variable selection + plots (your current app main UI)
                tabPanel(
                        "App",
                        sidebarLayout(
                                sidebarPanel(
                                        tags$h4("1) Diet (main inputs)"),
                                        tags$p(style="color:#666;",
                                               "Enter your typical daily intake. If unsure, use a reasonable estimate."),
                                        
                                        sliderInput("sodium_mg", "Sodium (mg/day):",
                                                    min = 0, max = 7000, value = 2300, step = 50),
                                        tags$p(style="font-size:12px; color:#666; margin-top:-8px;",
                                               tags$b("What it means: "),
                                               "Total dietary sodium (often related to salt intake)."),
                                        
                                        sliderInput("protein_g", "Protein (g/day):",
                                                    min = 0, max = 250, value = 70, step = 1),
                                        tags$p(style="font-size:12px; color:#666; margin-top:-8px;",
                                               tags$b("What it means: "),
                                               "Total daily protein intake."),
                                        
                                        selectInput("protein_origin", "Protein source:",
                                                    choices = c("Mixed", "Animal", "Plant")),
                                        tags$p(style="font-size:12px; color:#666; margin-top:-8px;",
                                               tags$b("What it means: "),
                                               "Whether your protein intake is mainly from mixed, animal, or plant sources."),
                                        
                                        hr(),
                                        
                                        tags$h4("2) Personal & clinical profile"),
                                        tags$p(style="color:#666;",
                                               "These variables help the model tailor the estimate to a profile similar to yours."),
                                        
                                        numericInput("age", "Age (years):", value = 50, min = 18, max = 99, step = 1),
                                        tags$p(style="font-size:12px; color:#666; margin-top:-8px;",
                                               "Age is commonly associated with cardiovascular risk in population models."),
                                        
                                        numericInput("bmi", "BMI:", value = 27, min = 10, max = 60, step = 0.1),
                                        tags$p(style="font-size:12px; color:#666; margin-top:-8px;",
                                               tags$b("BMI: "),
                                               "Body Mass Index (kg/m²)."),
                                        
                                        sliderInput("pir", "Poverty Index Ratio (PIR):",
                                                    min = 0, max = 5, value = 2.0, step = 0.1),
                                        tags$p(style="font-size:12px; color:#666; margin-top:-8px;",
                                               tags$b("What it means: "),
                                               "A socioeconomic indicator used in many population health surveys. ",
                                               "It can capture contextual differences associated with health outcomes."),
                                        
                                        sliderInput("sbp", "Mean systolic blood pressure (mmHg):",
                                                    min = 80, max = 220, value = 120, step = 1),
                                        tags$p(style="font-size:12px; color:#666; margin-top:-8px;",
                                               tags$b("What it means: "),
                                               "Systolic BP is the “top number”. If you have multiple readings, use an approximate average."),
                                        
                                        selectInput("gender", "Gender:", choices = c("Male", "Female")),
                                        tags$p(style="font-size:12px; color:#666; margin-top:-8px;",
                                               "Demographic factor used by the model."),
                                        
                                        selectInput("education", "Education:", choices = c(
                                                "< 9th Grade",
                                                "9th to 11th Grade",
                                                "High School Graduate",
                                                "Some College",
                                                "College Graduate or +"
                                        )),
                                        tags$p(style="font-size:12px; color:#666; margin-top:-8px;",
                                               "Included as a socioeconomic/contextual variable (as defined in the reference dataset)."),
                                        
                                        selectInput("diabetes", "Diabetes:", choices = c("No", "Pre-diabetes", "Yes")),
                                        tags$p(style="font-size:12px; color:#666; margin-top:-8px;",
                                               "Select the category that best matches your current status."),
                                        
                                        selectInput("med_use", "Medication use:", choices = c("No", "Yes")),
                                        tags$p(style="font-size:12px; color:#666; margin-top:-8px;",
                                               "Indicates whether you use medication (as defined/encoded in the model)."),
                                        
                                        hr(),
                                        helpText(tags$b("Auto-update: "),
                                                 "The estimate and plots are recalculated whenever you change any input.")
                                ),
                                
                                mainPanel(
                                        fluidRow(
                                                column(
                                                        width = 4,
                                                        wellPanel(
                                                                h4("Model estimate"),
                                                                textOutput("prob_txt"),
                                                                p(style="color:#666;",
                                                                  tags$b("How to interpret: "),
                                                                  "This is the model’s predicted value for the current inputs. ",
                                                                  "It is best used to compare scenarios and understand direction of change.")
                                                        )
                                                ),
                                                column(
                                                        width = 4,
                                                        wellPanel(
                                                                h4("Sodium percentile (US reference)"),
                                                                textOutput("pct_sodium_txt"),
                                                                p(style="color:#666;",
                                                                  tags$b("How to interpret: "),
                                                                  "Higher percentiles indicate higher sodium intake relative to the reference population.")
                                                        )
                                                ),
                                                column(
                                                        width = 4,
                                                        wellPanel(
                                                                h4("Protein percentile (US reference)"),
                                                                textOutput("pct_prot_txt"),
                                                                p(style="color:#666;",
                                                                  tags$b("How to interpret: "),
                                                                  "Higher percentiles indicate higher protein intake relative to the reference population.")
                                                        )
                                                )
                                        ),
                                        
                                        tabsetPanel(
                                                tabPanel(
                                                        "Curve vs Sodium",
                                                        tags$p(style="color:#666;",
                                                               "This plot shows how the model estimate changes when only sodium varies, ",
                                                               "holding all other inputs constant. The dashed line marks your current value."),
                                                        plotOutput("plot_sodium", height = 350)
                                                ),
                                                tabPanel(
                                                        "Curve vs Protein",
                                                        tags$p(style="color:#666;",
                                                               "This plot shows how the model estimate changes when only protein varies, ",
                                                               "holding all other inputs constant. The dashed line marks your current value."),
                                                        plotOutput("plot_protein", height = 350)
                                                ),
                                                tabPanel(
                                                        "Percentiles (bars)",
                                                        tags$p(style="color:#666;",
                                                               "Bars show your position (0–100) relative to the reference population. ",
                                                               "This is not “risk”; it’s an intake comparison."),
                                                        plotOutput("plot_percentiles", height = 250)
                                                )
                                        )
                                )
                        )
                ),
                
                # 3) Descriptive stats + data source + repo link
                tabPanel(
                        "Methods & data",
                        tags$div(
                                style = "max-width: 1000px; margin-top: 10px;",
                                tags$h3("Data Import (NHANES Interface)"),
                                tags$p(
                                        "The following block mirrors the original mapping and extraction from the CDC database for cycles G to J (2011-2018). Combining four survey cycles increases the statistical power of the analysis."
                                ),
                                
                                tags$p(
                                        tags$b("Methodological Note — Complete Case Analysis: "),
                                        "Missing data (NAs) in clinical and socioeconomic covariates (e.g., income ratio, BMI, blood pressure) cause listwise deletion during logistic regression. If NAs enter the survey design object, the model silently discards incomplete records, distorting sample weights. A Complete Case Analysis (CCA) filter is therefore applied explicitly, ensuring the statistical model operates on fully observed data only. The number of participants excluded by CCA is reported transparently in Section 2.1."
                                ),
                                
                                tags$hr(),
                                
                                tags$h3("Descriptive statistics"),
                                uiOutput("tab1"),
                                
                                tags$hr(),
                                
                                tags$h3("Source code / repository"),
                                tags$p(
                                        "GitHub repository: ",
                                        tags$a(
                                                href = "https://github.com/mrpribeiro/PLCDS",
                                                target = "_blank",
                                                "https://github.com/mrpribeiro/PLCDS"
                                        )
                                )
                        )
                )
        )
)

bundle <- readRDS("shiny_bundle.rds")

server <- function(input, output, session) {
        
        newdata <- reactive({
                data.frame(
                        Sodium_mg  = input$sodium_mg,
                        Protein_g = input$protein_g,
                        Protein_Origin  = as.factor(input$protein_origin),
                        Age             = input$age,
                        Gender          = as.factor(input$gender),
                        Poverty_Index_Ratio = input$pir,
                        Education       = as.factor(input$education),
                        BMI             = input$bmi,
                        Diabetes        = as.factor(input$diabetes),
                        Mean_Systolic   = input$sbp,
                        Medication_Use  = as.factor(input$med_use)
                )
        })
        
        prob_ic <- reactive({
                # For the binomial GLM: type="response" returns the probability
                p <- predict(bundle$final_model, newdata = newdata(), type = "response")
                as.numeric(p)
        })
        
        output$prob_txt <- renderText({
                percent(prob_ic(), accuracy = 0.1)  # ex: "12.3%"
        })
        
        # Percentiles
        pct_sodium <- reactive({
                ecdf(bundle$ref_sodium_mg)(input$sodium_mg) * 100
        })
        
        pct_prot <- reactive({
                ecdf(bundle$ref_protein_g)(input$protein_g) * 100
        })
        
        output$pct_sodium_txt <- renderText({
                paste0(round(pct_sodium(), 1), "th percentile")
        })
        
        output$pct_prot_txt <- renderText({
                paste0(round(pct_prot(), 1), "th percentile")
        })
        
        # 3) Visualizations
        output$plot_sodium <- renderPlot({
                grid <- data.frame(
                        Sodium_mg  = seq(0, 7000, by = 50)
                )
                
                grid$odds <- exp(coef(bundle$final_model)[2]*grid$Sodium_mg)
                
                ggplot(grid, aes(x = Sodium_mg, y = odds)) +
                        geom_line(linewidth = 1) +
                        geom_vline(xintercept = input$sodium_mg, linetype = 2) +
                        labs(x = "Sodium (mg/day)", y = "Odds Ratio",
                             title = "Odds Ratio vs Sodium (holding all other factors constant)") +
                        theme_minimal()
        })
        
        output$plot_protein <- renderPlot({
                grid2 <- data.frame(
                        Protein_g = seq(0, 250, by = 1)
                )
                
                grid2$odds <- exp(coef(bundle$final_model)[3]*grid2$Protein_g)
                
                ggplot(grid2, aes(x = Protein_g, y = odds)) +
                        geom_line(linewidth = 1) +
                        geom_vline(xintercept = input$protein_g, linetype = 2) +
                        labs(x = "Protein (g/day)", y = "Odds Ratio",
                             title = "Odds Ratio vs Protein (holding all other factors constant)") +
                        theme_minimal()
        })
        output$plot_percentiles <- renderPlot({
                df <- data.frame(
                        variable = c("Sodium (mg/day)", "Protein (g/day)"),
                        percentile = c(pct_sodium(), pct_prot())
                )
                
                ggplot(df, aes(x = percentile, y = variable)) +
                        geom_col(width = 0.6) +
                        scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
                        labs(x = "Percentile (0–100)", y = NULL, title = "Position relative to the reference population (USA)") +
                        theme_minimal()
        })
        
        output$tab1 <- renderUI({
                HTML(bundle$tab1_html)
        })
}

shinyApp(ui, server)