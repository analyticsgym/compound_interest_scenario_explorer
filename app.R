library(shiny)
library(tidyverse)
library(scales)
library(plotly)
library(reactable)

### TODO: add warning messages for invalid inputs
### TODO: restrict inputs to positive values only

ui <- fluidPage(
      titlePanel("Compound Interest Scenario Explorer"),
      p("Author: Brian Moore (@analyticsgym)"),
      HTML("<p><a href='https://github.com/analyticsgym/compound_interest_scenario_explorer/blob/main/app.R'>Code on Github</a></p>"),
      br(),
      fluidRow(
        column(width = 8,
               numericInput("yrs", "Years to invest", 
                            min = 1, max = 100, value = 30))
      ),
      fluidRow(
        hr()
      ),
      fluidRow(
        column(width = 4,
               # h3("Scenario 1"),
               numericInput("gs1", "Scenario 1: Starting Investment", 
                            value = 0)),
        
        column(width = 4,
               # h3("Scenario 2"),
               numericInput("gs2", "Scenario 2: Starting Investment", 
                            value = 5000)),
        
        column(width = 4,
               # h3("Scenario 3"),
               numericInput("gs3", "Scenario 3: Starting Investment", 
                            value = 11000))
      ),
      fluidRow(
        column(width = 4,
               numericInput("c1", "Scenario 1: Annual Contribution (year end)", 
                            value = 3000, min=0)),
        
        column(width = 4,
               numericInput("c2", "Scenario 2: Annual Contribution (year end)", 
                            value = 500, min=0)),
        
        column(width = 4,
               numericInput("c3", "Scenario 3: Annual Contribution (year end)", 
                            value = 0, min=0))
      ),
      fluidRow(
        column(width = 4,
               sliderInput("ir1", "Scenario 1: Annual Interest Rate", 
                           min = 1, max = 30, post  = " %", value = 1)),
        
        column(width = 4,
               sliderInput("ir2", "Scenario 2: Annual Interest Rate", 
                           min = 1, max = 30, post  = " %", value = 8)),
        
        column(width = 4, 
               sliderInput("ir3", "Scenario 3: Annual Interest Rate", 
                           min = 1, max = 30, post  = " %", value = 8))
      ),
      fluidRow(
        column(width = 4, 
               htmlOutput("s1_summary")),
        column(width = 4, 
               htmlOutput("s2_summary")),
        column(width = 4, 
               htmlOutput("s3_summary"))
      ),
      fluidRow(
        hr()
      ),
      fluidRow(
        align="center",
        plotlyOutput("plot1")
      ),
      fluidRow(
        hr()
      ),
      fluidRow(
        align="center",
        plotlyOutput("plot2")
      ),
      fluidRow(
        hr()
      ),
      fluidRow(
        align="center",
        h4("Future Investment Value by Year [Table View]"),
        reactableOutput("num_table")
      )
)

server <- function(input, output, server) {
  
  ### function to calculate compound interest
  compound_calc <- function(getting_started_amount, 
                            contribution, interest_rate, year) {
    P <- as.numeric(getting_started_amount)
    A <- as.numeric(contribution)
    i <- as.numeric(interest_rate)/100
    n <- as.numeric(year)
    a_over_i <- ifelse(is.nan(A/i), 0, A/i)
    (P + a_over_i) * ((1 + i)^n) - (a_over_i) 
  }
  
  ### bullet point text summary based on user inputs
  scenario_text_summary <- function(getting_started_amount, 
                                    contribution, 
                                    interest_rate,
                                    years,
                                    scenario) {
    P <- as.numeric(getting_started_amount)
    A <- as.numeric(contribution)
    years_var <- as.numeric(years)
    fv <- compound_calc(getting_started_amount, contribution, 
                        interest_rate, years)
    
    
    gs_amount <- label_dollar(accuracy = 1)(P)
    ac_account <- label_dollar(accuracy = 1)(A)
    interest_rate <- percent_format()(as.numeric(interest_rate)/100)
    total_contribution <- label_dollar(accuracy = 1)(P + (years_var * A))
    interest_earned <- label_dollar(accuracy = 1)(fv - (P + (years_var * A)))
    fv_dollars <- label_dollar(accuracy = 1)(fv)
    
    row_1 <- paste0("<b>Scenario ", scenario, "</b>")
    bullet_1 <- paste0("\U2022 starting investment: ", gs_amount)
    bullet_2 <- paste0("\U2022 annual contribution: ", ac_account)
    bullet_3 <- paste0("\U2022 interest rate: ", interest_rate)
    bullet_4 <- paste0("\U2022 total contribution: ", total_contribution)
    bullet_5 <- paste0("\U2022 total interest earned: ", interest_earned)
    bullet_6 <- paste0("\U2022 future investment value: ", fv_dollars)
    
    paste(row_1, 
          bullet_1, bullet_2, bullet_3, 
          bullet_4, bullet_5, bullet_6,
          "<br/>",
          sep="<br/>")
  }
  
  ### function to generate a df for scenario outcome
  scenario_summary_df <- function(getting_started_amount, 
                                    contribution, 
                                    interest_rate,
                                    scenario) {
    
    P <- as.numeric(getting_started_amount)
    A <- as.numeric(contribution)
    years_var <- as.numeric(input$yrs)
    fv <- compound_calc(getting_started_amount, contribution, 
                        interest_rate, input$yrs)
    
    total_contribution <- (P + (years_var * A))
    interest_earned <- fv - total_contribution
    scenario_var <- paste0("Scenario ", scenario)
    
    tibble(scenario = scenario_var,
           total_contribution = total_contribution,
           total_intererest_earned = interest_earned,
           future_investment_value = fv)
  }
  
  data <- reactive({
    tibble(year_number = 0:as.numeric(input$yrs),
           s1 = compound_calc(input$gs1, input$c1, input$ir1, year_number),
           s2 = compound_calc(input$gs2, input$c2, input$ir2, year_number),
           s3 = compound_calc(input$gs3, input$c3, input$ir3, year_number))
  })
  
  scenario_result_data <- reactive({
    df1 <- scenario_summary_df(input$gs1, input$c1, input$ir1, 1)
    df2 <- scenario_summary_df(input$gs2, input$c2, input$ir2, 2)
    df3 <- scenario_summary_df(input$gs3, input$c3, input$ir3, 3)
    
    bind_rows(list(df1, df2, df3))
  })
  
  output$plot1 <- renderPlotly({
    p1 <- scenario_result_data() %>%
      gather(key="metric", value="amount", -scenario) %>%
      mutate(scenario = factor(scenario, levels = c("Scenario 1",
                                                    "Scenario 2",
                                                    "Scenario 3")),
             metric = factor(metric, levels = c("total_contribution",
                                                "total_intererest_earned",
                                                "future_investment_value")),
             amount_formatted = dollar(round(amount,0))) %>%
      ggplot(aes(x=metric,
                 y=amount,
                 fill=scenario,
                 text = paste0(
                   scenario,
                   "\n", metric,
                   "\n", amount_formatted))
             ) +
      geom_col(alpha=0.7) +
      geom_text(aes(label=amount_formatted), color="black") +
      facet_grid(. ~ scenario) +
      theme(legend.position = "none",
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      labs(title="Outcome summary comparison\n",
           x="",
           y="Amount (USD)") +
      scale_y_continuous(labels = scales::dollar_format())
    ggplotly(p1, tooltip = "text")
  })
  
  output$plot2 <- renderPlotly({
    p2 <- data() %>%
      gather(key="scenario", value="value", -year_number) %>%
      ggplot(aes(x=year_number,
                 y=value,
                 color=scenario,
                 group=scenario,
                 text = paste0(
                   "Scenario ", str_replace(scenario, "s", ""),
                   "\nYear number: ", year_number,
                   "\n", dollar(round(value,0))))
             ) +
      geom_point() +
      geom_line() +
      theme(legend.position = "none") +
      labs(title="Future Investment Value by Year",
           x="Years from Starting Investment",
           y="Future Investment Value\n\n\n") +
      scale_y_continuous(labels = scales::dollar_format())
    ggplotly(p2, tooltip = "text") %>% layout(hovermode = "x") 
  })
  
  output$s1_summary <- renderText({
    scenario_text_summary(input$gs1, input$c1, input$ir1, input$yrs, 1)
  })

  output$s2_summary <- renderText({
    scenario_text_summary(input$gs2, input$c2, input$ir2, input$yrs, 2)
  })
  
  output$s3_summary <- renderText({
    scenario_text_summary(input$gs3, input$c3, input$ir3, input$yrs, 3)
  })

  output$num_table <- renderReactable(
    reactable(data(),
          defaultPageSize = 10,
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(10, 20, 30),
          defaultSorted = list(year_number = "desc"),
          showSortIcon = FALSE,
          columns = list(
            year_number = colDef(name = "Years from Starting Investment"),
            s1 = colDef(name = "Scenario 1",
                        format = colFormat(prefix = "$", 
                                           separators = TRUE, digits = 0)),
            s2 = colDef(name = "Scenario 2",
                        format = colFormat(prefix = "$", 
                                           separators = TRUE, digits = 0)),
            s3 = colDef(name = "Scenario 3",
                        format = colFormat(prefix = "$", 
                                           separators = TRUE, digits = 0))
          )
      )
   )
}

shinyApp(ui = ui, server = server)
