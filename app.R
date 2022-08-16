library(shiny)
library(tidyverse)
library(kableExtra)
library(ggplot2)
library(forcats)
library(RColorBrewer)
library(treemap)
library(d3treeR)

# Load data

university <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-10-16/recent-grads.csv")



# Define UI
ui <- fluidPage(

  titlePanel("Rank of major in USA"),

  h3("1. What number of bins do you stop seeing bimodality in the waiting time?"),
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        sliderInput("bins",
                    "Number of bins:",
                    min = 1,
                    max = 50,
                    value = 30)
      ),

      mainPanel(
        plotOutput("distPlot")
      )
    )
  ),

  h3("2. How do the different geoms change the view of the data?"),
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        radioButtons("geom",
                     "Geom choice:",
                     choices = c("geom_point",
                                 "geom_density_2d",
                                 "geom_density_2d_filled",
                                 "geom_bin_2d",
                                 "geom_hex"))
      ),

      mainPanel(
        plotOutput("plot")
      )
    )
  ),

  h3("3. Is a mixture of two normal distribution good fit on eruption time?"),
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        sliderInput("bins2",
                    "Adjust the number of bins (if needed):",
                    min = 1,
                    max = 50,
                    value = 30),
        "Enter your guess for the:",
        numericInput("p", "Mixing probability:",
                     value = 0.35, min = 0, max = 1),
        numericInput("mean1", "Mean of the first group:",
                     value = 2.02),
        numericInput("mean2", "Mean of the second group:",
                     value = 4.27),
        numericInput("sd1", "Standard deviation of the first group:",
                     value = 0.24, min = 0),
        numericInput("sd2", "Standard deviation of the second group:",
                     value = 0.44, min = 0)
      ),

      mainPanel(
        plotOutput("mixDistFit")
      )
    )
  ),

  h3("Rank of median income"),
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        # Select type of trend to plot
        selectInput(inputId = "income", label = strong("Faculty:"),
                    choices = unique(university$Major_category))
      ),
      # Create a spot for the treechart
      mainPanel(
        plotOutput("incomePlot")
      )
    )
  ),



  h3("Number of students in major"),
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        # Select type of trend to plot
        selectInput(inputId = "university", label = strong("Major:"),
                    choices = unique(university$Major),
                    selected = "Engineering")
      ),
        # Create a spot for the barplot
        mainPanel(
          plotOutput("studentPlot")
        )
      )
    ),

  h3("Employment rate"),
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        # Select type of trend to plot
        selectInput(inputId = "university3", label = strong("Major:"),
                    choices = unique(university$Major),
                    selected = "Engineering")
      ),
      # Create a spot for the barplot
      mainPanel(
        plotOutput("employratePlot")
      )
    )
  ),

  h3("Employment"),
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        # Select type of trend to plot
        selectInput(inputId = "university4", label = strong("Major:"),
                    choices = unique(university$Major),
                    selected = "Engineering")
      ),
      # Create a spot for the barplot
      mainPanel(
        plotOutput("employPlot")
      )
    )
  ),

  fluidRow(
    column(10,
           div(class = "about",
               uiOutput('about'))
    )
  ),
  includeCSS("styles.css")
)

server <- function(input, output) {

  output$distPlot <- renderPlot({
    ggplot(faithful, aes(waiting)) +
      geom_histogram(bins = input$bins, color = "white") +
      theme_bw(base_size = 14) +
      labs(x = "Waiting time", y = "Count")
  })

  output$plot <- renderPlot({
    ggplot(faithful, aes(waiting, eruptions)) +
      get(input$geom)() +
      theme_bw(base_size = 14) +
      labs(x = "Waiting time", y = "Eruption time")
  })

  output$mixDistFit <- renderPlot({
    df <- data.frame(x = seq(min(faithful$eruptions), max(faithful$eruptions), length = 1000)) %>%
      mutate(density = input$p * dnorm(x, input$mean1, input$sd1) +
               (1 - input$p) * dnorm(x, input$mean2, input$sd2))

    ggplot(faithful, aes(eruptions)) +
      geom_histogram(aes(y = stat(density)), bins = input$bins2, color = "white") +
      geom_line(data = df, aes(x = x, y = density), color = "red", size = 2) +
      theme_bw(base_size = 14) +
      labs(x = "Eruption time", y = "Density")
  })


  #treechart for income
  output$incomePlot <- renderPlot({
    # Render a treechart
    charac_income <- input$income

    faculty <- university %>%
      filter(Major_category == charac_income)

    treemap1 <- treemap(faculty,
            index = "Major",
            vSize = "Median",
            type = "index")

    print(treemap1)
  })


  #bar chart for student
  output$studentPlot <- renderPlot({
    # Render a barplot

    student <- university %>%
      pivot_longer(cols = "Total":"Women",
                   names_to = "sex",
                   values_to = "number_student") %>%
      select(Major, Major_category, sex, number_student)
    charac2 <- input$university
    student_bar <- student %>%
      group_by(Major_category) %>%
      filter(Major == charac2) %>%
      mutate(sex = fct_relevel(sex,
                               "Total", "Men", "Women")) %>%
      ggplot(aes(x = sex, y = number_student, fill = sex)) +
      geom_bar(stat='identity') +
      xlab("Sex") +
      ylab("Number of students")

    print(student_bar)
    })


  #pie chart for employment rate
  output$employratePlot <- renderPlot({
    # Render a piechart

    employment <- university %>%
      mutate("Employment rate" = 100 - Unemployment_rate) %>%
      rename("Unemployment rate" = Unemployment_rate)%>%
      pivot_longer(cols = c("Employment rate","Unemployment rate"),
                   names_to = "employment",
                   values_to = "percentage") %>%
      select(Major, Major_category, employment, percentage)

    charac3 <- input$university3

    pie_employ <- employment %>%
      group_by(Major_category) %>%
      filter(Major == charac3)

    pie_employ$data.label <- sprintf("%s (%.2f%%)", pie_employ$employment, pie_employ$percentage)

    pie_chart <- pie(pie_employ$percentage, labels = pie_employ$data.label, col= c("pink","blue"))

    print(pie_chart)
  })

  #bar chart for employ
  output$employPlot <- renderPlot({
    # Render a barplot

    employ <- university %>%
      pivot_longer(cols = College_jobs : Low_wage_jobs,
                   names_to = "job",
                   values_to = "number_jobs") %>%
      select(Major, Major_category, job, number_jobs)

    charac4 <- input$university4

    employ_bar <- employ %>%
      group_by(Major_category) %>%
      filter(Major == charac4) %>%
      mutate(job = fct_relevel(job,
                               "College_jobs", "Non_college_jobs", "Low_wage_jobs")) %>%
      ggplot(aes(x = job, y = number_jobs, fill = job)) +
      geom_bar(stat='identity') +
      xlab("Job") +
      ylab("Number of jobs")



    print(employ_bar)
  })

  output$about <- renderUI({
    knitr::knit("about.Rmd", quiet = TRUE) %>%
      markdown::markdownToHTML(fragment.only = TRUE) %>%
      HTML()
  })
}

shinyApp(ui = ui, server = server)
