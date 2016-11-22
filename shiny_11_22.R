library(shiny)
library(ggplot2)
library(dplyr)

gss = read.csv("https://stat.duke.edu/~mc301/data/gss2010.csv")
is_num = sapply(gss, is.numeric)
gss = gss[,is_num]


app = shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput("var", "Pick a variable to visualize:", choices = names(gss), selected = "hrsrelax"),
        selectInput("var2", "Pick a 2nd variable to visualize:", choices = c("None"="none",names(gss)), selected = "None"),
        conditionalPanel(
          "input.var2 == 'none'",
          selectInput("plot_type","Choose plot type:", choices = c("Histogram"="hist", "Boxplot"="box")),
          conditionalPanel(
            "input.plot_type == 'hist'",
            sliderInput("n_bins","Number of bins:",min=1,max=50,value=30),
            HTML("You are plotting a histogram.")
          )
        ),
        conditionalPanel(
          "input.var2 != 'none'",
          selectInput("plot_type_2var","Choose plot type:", choices = c("Scatter plot"="scatter")),
          numericInput("scatter_alpha",label = "Point alpha:", min = 0, max=1, value = 0.9),
          checkboxInput("scatter_jitter", label = "Jitter points:", value = FALSE),
          selectInput("scatter_trend", label = "Fit model:", choices = c("None"="none", "lm"="lm", "Loess"="loess"))
        )
      ),
      mainPanel(
        h4("Distribution:"),
        plotOutput("main_plot"),
        hr(),
        h4("Summary Statistics:"),
        tableOutput("summ_stat")
      )
    )
  ),
  server = function(input, output, session)
  {
    observe({
      cur_col = gss[[input$var]] %>% na.omit()
      
      if (typeof(cur_col) == "integer")
      {
        new_max = min(max(cur_col) - min(cur_col), 50)
      } else if (typeof(cur_col) == "double") {
        new_max = cur_col %>% unique() %>% length() %>% min(50)
      }
      updateSliderInput(session, "n_bins", max = new_max)
    })
    
    output$main_plot = renderPlot({
      if (input$var2 == "none")
      {
        if (input$plot_type == "hist")
          ggplot(gss, aes_string(x=input$var)) + geom_histogram(bins=input$n_bins)
        else if (input$plot_type == "box")
          ggplot(gss, aes_string(y=input$var, x=1)) + geom_boxplot()
      } else {
        if (input$plot_type_2var == "scatter")
        {
          g = ggplot(gss, aes_string(x=input$var, y=input$var2)) 
          
          if (input$scatter_jitter == FALSE)
            g = g + geom_point(alpha=input$scatter_alpha)
          else
            g = g + geom_jitter(alpha=input$scatter_alpha)
          
          if (input$scatter_trend == "lm")
            g = g + geom_smooth(method="lm")
          else if (input$scatter_trend == "loess")
            g = g + geom_smooth()
        }
        
        print(g)
      }
    })
    
    output$summ_stat = renderTable({
      data = gss[[input$var]] %>% na.omit()
      
      data.frame(
        Q1 = quantile(data, probs=0.25),
        Median = median(data),
        Q3 = quantile(data, probs=0.75),
        Mean = mean(data),
        SD = sd(data)
      )
    })
  }
)


runApp(app)

