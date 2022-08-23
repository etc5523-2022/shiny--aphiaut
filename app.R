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
library(shinythemes)
library(ggthemes)


# Load data

university <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-10-16/recent-grads.csv")



# Define UI
ui <- navbarPage(
  "GraduateCollegeMajor",
  theme = shinytheme("flatly"),
  tabPanel(
    "HOME",
    # title
    titlePanel(div(
      windowTitle = "GraduatUSA",
      img(src = "https://cdn.pixabay.com/photo/2015/10/18/19/04/graduation-995042_960_720.jpg", width = "100%", class = "bg", height="350px"),
    )),

    tags$br(),

    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Overview",

        sidebarLayout(
          sidebarPanel(

            h3("Data Overview"),
            tags$br(),

            sliderInput(
              "incomeRange",
              label = "Mediean Income Range",
              min = 20000,
              max = 120000,
              value = c(20000, 120000)
            ),

            sliderInput(
              "employRange",
              label = "Employment Rate Range",
              round = -2, step = 0.01,
              min = 90,
              max = 100,
              value = c(90, 100)
            ),


            actionButton("actionDT", "Filter", class = "btn btn-warning"),
          ),
          mainPanel(
            h3("Rank of Major by Median Income"),
            tags$br(),
            dataTableOutput("myTable"),
            tags$br(),
            tags$br(),
          )
        ),
        fluidRow(
          column(3),
          column(6,
                 shiny::HTML("<br><br><center> <h1>Rank of Median Income</h1> </center><br>")
          ),
          column(3),
          sidebarPanel(
            # Select type of trend to plot
            selectInput(inputId = "income", label = strong("Major category:"),
                        choices = unique(university$Major_category))
          )
        ),
        # Create a spot for the treechart
        plotlyOutput("incomePlot"),
        tags$hr(),
      ),


      tabPanel(
        "Visual Comparison",
        fluidRow(
          column(3),
          column(6,
                 shiny::HTML("<br><br><center> <h1>Number of Students in Major</h1> </center><br>")
          ),
          column(3),
          sidebarPanel(
            # Select type of trend to plot
            selectInput(inputId = "university", label = strong("Major:"),
                        choices = unique(university$Major),
                        selected = "Engineering")
          )
        ),
        # Create a spot for the barplot
        br(),
        plotlyOutput("studentPlot"),
        br(),

        fluidRow(
          column(3),
          column(6,
                 shiny::HTML("<br><br><center> <h1>Employment Rate</h1> </center><br>")
          ),
          column(3),
          sidebarPanel(
            # Select type of trend to plot
            selectInput(inputId = "university3", label = strong("Major:"),
                        choices = unique(university$Major),
                        selected = "Engineering")
          ),
          # Create a spot for the pie chart
          mainPanel(
            plotOutput("employratePlot")
          )
        ),

        fluidRow(
          column(3),
          column(6,
                 shiny::HTML("<br><br><center> <h1>Employment</h1> </center><br>")
          ),
          column(3),
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
        br(),

        fluidRow(

                 shiny::HTML("<br><br><center> <h1>Median Income Comparison</h1> </center><br>"),

          br(),

          align = "center",
          plotlyOutput(outputId = "boxplotPlot", width = "80%"),

        ),



        # bar plot
        fluidRow(
          column(3),
          column(6,
                 shiny::HTML("<br><br><center> <h1>Percentage of Full-time Employment</h1> </center><br>")
          ),
          column(3),
          sidebarLayout(
            sidebarPanel(
              # Select type of trend to plot
              selectInput(inputId = "university6", label = strong("Major Category:"),
                          choices = unique(university$Major_category),
                          selected = "Engineering")
            ),
            # Create a spot for the barplot
            mainPanel(
              plotlyOutput("percentfulltimePlot")
            )
          )
        ),


        # dot plot
        fluidRow(
          column(3),
          column(6,
                 shiny::HTML("<br><br><center> <h1>Full-time Employment Rate vs Median Income</h1> </center><br>")
          ),
          column(3),
          sidebarLayout(
            sidebarPanel(
              # Select type of trend to plot
              selectInput(inputId = "university5", label = strong("Major Category:"),
                          choices = unique(university$Major_category),
                          selected = "Engineering")
            ),
            # Create a spot for the dotplot
            mainPanel(
              plotlyOutput("employvsincomePlot")
            )
          )
        ),
        tags$br(),
        tags$br(),
        tags$hr(),
      )
    )
  ),


      tabPanel("BACKGROUND",
               fluidPage(fluidRow(
                           column(3),
                           column(6,
                                  shiny::HTML("<br><br><center> <h1>Are you hesitated about selecting a college major?</h1> </center><br>"),
                                  shiny::HTML("<p><h5>The average income of the new graduates with a full-time job is $36,000 per year in the USA,
                                  however, some graduate career pathway gets $110,000 such as Petroleum Engineering which is five times the lowest median job of a college major, $22,000.
                                  Although the engineering pathway can get the highest salary, it doesn't popular as psychology.
                                  If you are interested why would it be like that, you can read more in
                       <a href='https://fivethirtyeight.com/features/the-economic-guide-to-picking-a-college-major/'>
                       The Economic Guide To Picking A College Major</a>.
                       </h5></p>")

                           ),
                           column(3),
                           includeCSS("styles.css")
                         ),

                         fluidRow(

                           style = "height:50px;")
               )
                         ),

      tabPanel("ABOUT",
               fluidPage(
                 tags$hr(),
                         about_page <- tabPanel(title = "ABOUT",
                                                fluidRow(
                                                  column(10,
                                                         div(class = "about",
                                                             uiOutput('about'))
                                                  )
                                                ),
                                                includeCSS("styles.css")
                         )
               )
      ),
  includeCSS("styles.css")
)


server <- function(input, output) {

 #create the table

  filtered_DT <- reactive({
    input$actionDT
    isolate({
      minIncome <- input$incomeRange[1]
      maxIncome <- input$incomeRange[2]
      minEmploy <- input$employRange[1]
      maxEmploy <- input$employRange[2]
    })
    university %>%
      mutate("Employment_rate" = 100 - round(Unemployment_rate,2)) %>%
    filter(Median > minIncome,
           Median < maxIncome) %>%
      filter(Employment_rate > minEmploy,
             Employment_rate < maxEmploy) %>%
      select(Rank, Major, Major_category, P25th, Median, P75th, Employment_rate)

})
  # Render table
  output$myTable <- DT::renderDataTable({
    filtered_DT() %>%
      datatable(
        .,
        rownames = FALSE,
        class = "table",
        option = list(pageLength = 10, scrollX = T),
        colnames = c("Rank", "Major", "Major category", "P25th", "Median", "P75th", "Employment rate"

        )
      )

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
            textposition = "middle center",
            fontcolor.labels="white",
            fontsize.labels=48)
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
      ggtitle("The number of student in major")+
      scale_fill_manual(values=c("#DABFDE", "#BFD6F5", "#FDE3FC")) +
      scale_y_continuous(labels = scales::comma) +
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
      rename("Degree required jobs" = College_jobs, "Non-degree required jobs" = Non_college_jobs) %>%
      pivot_longer(cols = "Degree required jobs" : "Non-degree required jobs",
                   names_to = "Job",
                   values_to = "number_jobs") %>%
      select(Major, Major_category, Job, number_jobs)

    charac4 <- input$university4

    employ_bar <- employ %>%
      group_by(Major_category) %>%
      filter(Major == charac4) %>%
      mutate(Job = fct_relevel(Job,
                               "Degree required jobs", "Non-degree required jobs"))

      ggplotly(ggplot(data = employ_bar, aes(x = Job, y = number_jobs, fill = Job)) +
      geom_bar(stat='identity') +
      ggtitle("The number of jobs that require the major degree")+
      scale_fill_manual(values=c("#F3C0A1", "#C7E3A4")) +
      scale_y_continuous(labels = scales::comma)+
      xlab("Job") +
      ylab("Number of jobs"))

  })

  #boxplot chart
  output$boxplotPlot <- renderPlotly({
  boxplot <- university %>%
    group_by(Major_category) %>%
    ggplot(aes(x = Major_category, y = Median, fill = Major_category))+
    geom_boxplot(color = "black",
                 size = 1,
                 width = 0.3) +
    theme_hc() +
    scale_y_continuous(labels = scales::comma)+
    theme(
      legend.title = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )+
    ylab("US dollar")+
    ggtitle("Median Income Comparison")
  boxplot <- ggplotly(boxplot + coord_flip(), tooltip = ("Median"))
  hide_legend(boxplot)
  })



  #bar chart for percent full
  output$percentfulltimePlot <- renderPlotly({
    # Render a barplot
    charac6 <- input$university6

    percentage <- university %>%
      mutate(percent = (Full_time/(Full_time+Part_time)*100)) %>%
      group_by(Major_category) %>%
      filter(Major_category == charac6)

    ggplotly(ggplot(data = percentage, aes(x = Major, y = percent)) +
               geom_segment( aes(x=Major, xend=Major, y=0, yend=percent), color="skyblue") +
               geom_point( color="#F4EEB1", size=4, alpha=0.6) +
               ggtitle("Percentage of Full-time Employment") +
               xlab("")+
               ylab("Percentage")+
               ylim(0,100)+
               theme_light() +
               coord_flip() +
               theme(
                 panel.grid.major.y = element_blank(),
                 panel.border = element_blank(),
                 axis.ticks.y = element_blank()
               ))

  })



  #scatterplot chart
  output$employvsincomePlot <- renderPlotly({
    # Render a barplot


    charac5 <- input$university5

    fulltime <- university %>%
      mutate(fulltime_rate = (Full_time/(Full_time + Part_time))*100) %>%
      group_by(Major_category)%>%
      filter(Major_category == charac5)




    ggplotly(ggplot(data = fulltime, aes(x = Median, y = fulltime_rate)) +
               geom_point(aes(colour = Major), size = 4) +
               geom_point(colour = "grey90", size = 1.5) +
               ylab("Full-Time Rate") +
               xlab("US dollar")+
               ggtitle("Fulltime Employment Rate vs Median Income") +
               theme(legend.position="bottom",
                     legend.text = element_text(size = 10))+
               scale_x_continuous(labels = scales::comma))

  })




  output$about <- renderUI({
    knitr::knit("about.Rmd", quiet = TRUE) %>%
      markdown::markdownToHTML(fragment.only = TRUE) %>%
      HTML()
  })
}

shinyApp(ui = ui, server = server)
