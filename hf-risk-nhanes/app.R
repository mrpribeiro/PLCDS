library(shiny)
library(ggplot2)
library(scales)

ui <- fluidPage(
        titlePanel("Risk of Heart Failure (Logistic Model)"),
        
        sidebarLayout(
                sidebarPanel(
                        sliderInput("sodium_mg", "Sodium (mg/day):",
                                    min = 0, max = 7000, value = 2300, step = 50),
                        
                        sliderInput("protein_g", "Protein (g/day):",
                                    min = 0, max = 250, value = 70, step = 1),
                        
                        hr(),
                        
                        numericInput("age", "Age (years):", value = 50, min = 18, max = 99, step = 1),
                        numericInput("bmi", "BMI:", value = 27, min = 10, max = 60, step = 0.1),
                        
                        sliderInput("pir", "Poverty Index Ratio:",
                                    min = 0, max = 5, value = 2.0, step = 0.1),
                        
                        sliderInput("sbp", "Mean systolic BP:",
                                    min = 80, max = 220, value = 120, step = 1),
                        
                        selectInput("gender", "Gender:", choices = c("Male","Female")),
                        selectInput("education", "Education:", choices = c("< 9th Grade","9th to 11th Grade","High School Graduate","Some College","College Graduate or +")),
                        selectInput("diabetes", "Diabetes:", choices = c("No","Pre-diabetes","Yes")),
                        selectInput("med_use", "Medication use:", choices = c("No","Yes")),
                        selectInput("protein_origin", "Protein origin:", choices = c("Mixed","Animal","Plant")),
                        
                        hr(),
                        helpText("The probability is automatically recalculated whenever any input is changed.")
                ),
                
                mainPanel(
                        fluidRow(
                                column(
                                        width = 4,
                                        wellPanel(
                                                h4("Estimated probability"),
                                                textOutput("prob_txt"),
                                                p(style="color:#666;", "Interpretation: value predicted by the logistic model.")
                                        )
                                ),
                                column(
                                        width = 4,
                                        wellPanel(
                                                h4("Sodium percentile (EUA)"),
                                                textOutput("pct_sodium_txt")
                                        )
                                ),
                                column(
                                        width = 4,
                                        wellPanel(
                                                h4("Protein percentile (EUA)"),
                                                textOutput("pct_prot_txt")
                                        )
                                )
                        ),
                        
                        tabsetPanel(
                                tabPanel("Curve vs Sodium", plotOutput("plot_sodium", height = 350)),
                                tabPanel("Curve vs Protein", plotOutput("plot_protein", height = 350)),
                                tabPanel("Percentiles (bars)", plotOutput("plot_percentiles", height = 250)),
                                tabPanel("Descriptive stats", uiOutput("tab1"))
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
                paste0(round(pct_sodium(), 1), "º percentile")
        })
        
        output$pct_prot_txt <- renderText({
                paste0(round(pct_prot(), 1), "º percentile")
        })
        
        # 3) Visualizations
        output$plot_sodium <- renderPlot({
                        grid <- data.frame(
                                Sodium_mg  = seq(0, 7000, by = 50),
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
                        
                        grid$prob <- as.numeric(predict(bundle$final_model, newdata = grid, type = "response"))
                        
                        ggplot(grid, aes(x = Sodium_mg, y = prob)) +
                                geom_line(linewidth = 1) +
                                geom_vline(xintercept = input$sodium_mg, linetype = 2) +
                                scale_y_continuous(labels = percent_format(accuracy = 0.05)) +
                                labs(x = "Sodium (mg/day)", y = "Predicted probability",
                                     title = "Predicted probability vs Sodium (holding all other factors constant)") +
                                theme_minimal()
                })
                
                output$plot_protein <- renderPlot({
                        grid <- data.frame(
                                Sodium_mg  = input$sodium_mg,
                                Protein_g = seq(0, 250, by = 1),
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
                        
                        grid$prob <- as.numeric(predict(bundle$final_model, newdata = grid, type = "response"))
                        
                        ggplot(grid, aes(x = Protein_g, y = prob)) +
                                geom_line(linewidth = 1) +
                                geom_vline(xintercept = input$protein_g, linetype = 2) +
                                scale_y_continuous(labels = percent_format(accuracy = 0.05)) +
                                labs(x = "Protein (g/day)", y = "Predicted probability",
                                     title = "Predicted probability vs Protein (holding all other factors constant)") +
                                theme_minimal()
                })
                output$plot_percentiles <- renderPlot({
                        df <- data.frame(
                                variavel = c("Sodium (mg/day)", "Protein (g/day)"),
                                percentile = c(pct_sodium(), pct_prot())
                        )
                        
                        ggplot(df, aes(x = percentile, y = variavel)) +
                                geom_col(width = 0.6) +
                                scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
                                labs(x = "Percentile (0–100)", y = NULL, title = "Position relative to the reference population (EUA)") +
                                theme_minimal()
                })
                
                output$tab1 <- renderUI({
                        HTML(bundle$tab1_html)
                })
}

shinyApp(ui, server)