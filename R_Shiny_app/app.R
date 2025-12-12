#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

ui <- fluidPage(
  
  titlePanel("Run log of Wage Regressions"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose predictor (X) variables:"),
      
      # Multi-select input for choosing X variables
      selectInput(
        inputId = "xvars",
        label = "Select X variables",
        choices = names(together),     
        multiple = TRUE,
        selected = c("avg_creative_weight") 
      ),
      
      actionButton("run", "Run Regression")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 verbatimTextOutput("model_summary")
        ),
        tabPanel("Predicted vs Actual",
                 plotOutput("pred_plot")
        ),
        tabPanel("Residual Plot",
                 plotOutput("resid_plot")
        ),
        tabPanel("Distributions",
                 selectInput("dist_var", "Select variable:", choices = names(df)),
                 plotOutput("dist_plot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$run, {
    
    # ensure at least one x-variable
    req(input$xvars)
    
    # Build formula dynamically
    f <- as.formula(
      paste0("log(HOURWAGE_) ~ ", paste(input$xvars, collapse = " + "))
    )
    
    # Run regression
    model <- lm(f, data = together)
    
    # Print summary
    output$modelOut <- renderPrint({
      summary(model)
      
      output$pred_plot <- renderPlot({
        mod <- model_fit()
        preds <- predict(mod)
        actual <- df$HOURWAGE_
        
        plot(actual, preds,
             pch = 20,
             xlab = "Actual Hourly Wage",
             ylab = "Predicted Hourly Wage")
        abline(0, 1, lwd = 2)
      })
      output$resid_plot <- renderPlot({
        mod <- model_fit()
        plot(residuals(mod),
             ylab = "Residuals",
             xlab = "Index",
             pch = 20)
        abline(h = 0, lwd = 2)
      })
      
      
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
