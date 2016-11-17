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
        selectInput("var","Pick a vatiable to visualize", choices = names(gss), selected = "hrsrelax"),
        selectInput("plot_type","Choose plot type:", choices = c("Histogram"="hist", "Boxplot"="box")),
        conditionalPanel(
          "input.plot_type == 'hist'",
          sliderInput("n_bins","Number of bins:",min=1,max=50,value=30),
          HTML("You are plotting a histogram.")
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
  server = function(input, output)
  {
    output$main_plot = renderPlot({
      if (input$plot_type == "hist")
        ggplot(gss, aes_string(x=input$var)) + geom_histogram(bins=input$n_bins)
      else
        ggplot(gss, aes_string(y=input$var, x=1)) + geom_boxplot()
    })
    
    output$summ_stat = renderTable({
      data = gss[,input$var] %>% na.omit()
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