library(shiny)
library(tidyverse)
library(kableExtra)
library(ggplot2)
library(forcats)
library(RColorBrewer)
library(treemap)
library(d3treeR)
library(plotly)
library(DT)

# Load data

university <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-10-16/recent-grads.csv")



# Define UI
ui <- fluidPage(

  titlePanel("Rank of major in USA"),

# section 1.1
h3("Rank of major by median income"),
fluidRow(
  sidebarLayout(
    sidebarPanel(
      # table
      conditionalPanel(
        'input.dataset === "diamonds"',
        checkboxGroupInput("show_vars", "Columns in diamonds to show:",
                           names(diamonds), selected = names(diamonds))

      )
    ),
    # Create a table
    mainPanel(
      id = 'dataset',
      tabPanel("diamonds", DT::dataTableOutput("mytable1")),
    )
  )
),



# section 1.2
  h3("Rank of median income"),
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        # Select type of trend to plot
        selectInput(inputId = "income", label = strong("Major category:"),
                    choices = unique(university$Major_category))
      ),
      # Create a spot for the treechart
      mainPanel(
        plotlyOutput("incomePlot")
      )
    )
  ),


#section 2
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
          plotlyOutput("studentPlot")
        )
      )
    ),

#Section 3
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

#Section4
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
        plotlyOutput("employPlot")
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

 #create the table

  output$mytable1 <- DT::renderDataTable({
    table_all <- university %>%
      select(Rank, Major, Major_category, P25th, Median, P75th) %>%
      rename("Major category" = Major_category,
             "Income P25" = P25th,
             "Median income" = Median,
             "Income P75" = P75th)
    DT::datatable(table_all, rownames = FALSE, options = list(lengthMenu = c(10, 25, 50), pageLength = 10))
  })


  #treechart for income
  output$incomePlot <- renderPlotly({
    # Render a treechart
    charac_income <- input$income

    faculty <- university %>%
      filter(Major_category == charac_income)

    plot_ly(faculty,
            labels = ~ paste0(Major, "<br><br>", Median),
            parents = NA,
            values = ~ Median,
            type = 'treemap',
            hovertemplate = "Major: %{label}<br>Median income: %{value}<extra></extra>",
            textposition = "middle center")
  })


  #bar chart for student
  output$studentPlot <- renderPlotly({
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
                               "Total", "Men", "Women"))
      ggplotly(ggplot(data = student_bar, aes(x = sex, y = number_student, fill = sex)) +
      geom_bar(stat='identity') +
      xlab("Sex") +
      ylab("Number of students"))


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
  output$employPlot <- renderPlotly({
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
                               "College_jobs", "Non_college_jobs", "Low_wage_jobs"))

      ggplotly(ggplot(data = employ_bar, aes(x = job, y = number_jobs, fill = job)) +
      geom_bar(stat='identity') +
      xlab("Job") +
      ylab("Number of jobs"))


  })

  output$about <- renderUI({
    knitr::knit("about.Rmd", quiet = TRUE) %>%
      markdown::markdownToHTML(fragment.only = TRUE) %>%
      HTML()
  })
}

shinyApp(ui = ui, server = server)
