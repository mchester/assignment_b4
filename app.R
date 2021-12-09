library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(tidyverse)
library(datateachr)

## cancer_sample app.R ##

ui <- dashboardPage(
  #change dashboard header style
  dashboardHeader(title = shinyDashboardLogo(
    theme = "flat_red",
    boldText = "Cancer Sample",
    mainText = "App",
    badgeText = "v1.0"
  )),
  #create dashboard sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Histograms", tabName = "histogram", icon = icon("bar-chart-o")),
      menuItem("QQ Plots", tabName = "qqplot", icon = icon("bar-chart-o")),
      menuItem("Boxplots", tabName = "boxplots", icon = icon("bar-chart-o")),
      menuItem("Scatterplot", tabName = "scatterplots", icon = icon("bar-chart-o"))
    )
  ),
  
dashboardBody(
  #change theme
  shinyDashboardThemes(
    theme = "flat_red"
  )
  
  ### ui tabs ###
  ,tabItems(
    #create dashboard
    tabItem(tabName = "dashboard",
            fluidPage(
              titlePanel(h1(strong("Cancer Sample App"))),
              titlePanel(h2(strong("Why use this app?"))),
              titlePanel(h4("Use this app to quickly visualize variables and relationships in the 
                            datateachr cancer sample dataframe.")),
              titlePanel(h2(strong("Where did this data come from?"))),
              titlePanel(h4("This data came from a sample of quantitative 
                            features that were calculated from images of 
                            nuclei present in fine needle aspiration biopsies of 
                            breast masses from patients at the University of Wisconsin Hospital. It can be downloaded
                            using the datateachr package in R Studio.")),
              titlePanel(h2(strong("What can I do with this app?"))),
              titlePanel(h4("You can create histograms and qqplots to determine normality or lognormality, as well
              as boxplots and scatterplots to visualize and summarize associations.")))
    ),
    
    #create histogram inputs and plots
    tabItem(tabName = "histogram",
            fluidRow(box(title = "Create Histogram",
                         selectInput("variable_hist","Select Variable",
                                     choices = names(cancer_sample %>%
                                                       select(-c(ID, diagnosis))))),
                     box(plotOutput("histogram"))),
            fluidRow(box(title = "Create Histogram (Log Transformed)",
                         selectInput("variable_hist_log", "Select Variable",
                                     choices = names(cancer_sample %>%
                                                       select(-c(ID, diagnosis)) %>%
                                                       mutate(across(where(is.numeric), log))))),
                     box(plotOutput("histogram_log")))
    ),
    
    #create qq inputs and plots
    tabItem(tabName = "qqplot",
            fluidRow(box(title = "Create QQ Plot",
                         selectInput("variable_qqplot","Select Variable",
                                     choices = names(cancer_sample %>%
                                                       select(-c(ID, diagnosis))))),
                     box(plotOutput("qqplot"))),
            fluidRow(box(title = "Create QQ Plot (Log Transformed)",
                         selectInput("variable_qqplot_log", "Select Variable",
                                     choices = names(cancer_sample %>%
                                                       select(-c(ID, diagnosis)) %>%
                                                       mutate(across(where(is.numeric), log))))),
                     box(plotOutput("qqplot_log")))
    ),
    
    #create boxplot inputs and plot
    tabItem(tabName = "boxplots",
            fluidRow(box(title = "Create Boxplot",
                         selectInput("variable_boxplot","Select Variable",
                                     choices = names(cancer_sample %>%
                                                       select(-c(ID, diagnosis)))))),
            fluidRow(box(plotOutput("boxplot")),
                     box(title = "Analysis of Variance (ANOVA)",
                         verbatimTextOutput("summary_boxplot")))
    ),
    
    #create scatterplot inputs and plot
    tabItem(tabName = "scatterplots",
            fluidRow(box(title = "Create Scatterplot: Step 1",
                         selectInput("variable_x","Select X Variable",
                                     choices = names(cancer_sample %>%
                                                       select(-c(ID, diagnosis))))),
                     box(title = "Create Scatterplot: Step 2",
                         selectInput("variable_y","Select Y Variable",
                                     choices = names(cancer_sample %>%
                                                       select(-c(ID, diagnosis)))))),
            fluidRow(box(radioButtons("rb", "Choose Outcome:",
                                      c("All" = "all",
                                        "Benign"= "benign",
                                        "Malignant" = "malignant")))),
            fluidRow(box(plotOutput("scatterplot")),
                     (box(title = "Summary",
                          verbatimTextOutput("summary_scatter")))
    )
  )
)
)
)

server <- function(input, output) {
  
  output$histogram <- renderPlot({
    ggplot(cancer_sample, aes_string(x = input$variable_hist)) +
      geom_histogram(aes(y = ..density..)) +
      geom_density(colour = "red")
  })
  
  output$histogram_log <- renderPlot({
    ggplot(cancer_sample %>%
             select(-c(ID, diagnosis)) %>%
             mutate(across(where(is.numeric), log)), 
                    aes_string(x = input$variable_hist_log)) +
      geom_histogram(aes(y = ..density..)) +
      geom_density(colour = "red")
  })
  
  output$qqplot <- renderPlot({
    ggplot(cancer_sample, aes_string(sample = input$variable_qqplot)) +
      stat_qq() +
      stat_qq_line(colour = "red")
  })
  
  output$qqplot_log <- renderPlot({
    ggplot(cancer_sample %>%
             select(-c(ID, diagnosis)) %>%
             mutate(across(where(is.numeric), log)),
           aes_string(sample = input$variable_qqplot_log)) +
      stat_qq() +
      stat_qq_line(colour = "red")
  })
  
  output$boxplot <- renderPlot({
    ggplot(cancer_sample, aes_string(y = input$variable_boxplot)) +
      geom_boxplot(aes(diagnosis, get(input$variable_boxplot))) + 
      scale_x_discrete(limits = c("B", "M"),
                       labels = c("benign", "malignant"))
  })
  
  output$summary_boxplot <- renderPrint({
    model <- aov(get(input$variable_boxplot) ~ cancer_sample$diagnosis, data = cancer_sample)
    print(summary(model))
  })
  
  output$scatterplot <- renderPlot({
    rb <- switch(input$rb,
                 all = cancer_sample,
                 benign = cancer_sample %>%
                   filter(diagnosis == "B"),
                 malignant = cancer_sample %>%
                   filter(diagnosis == "M"))
    
    ggplot(rb, aes_string((input$variable_x), (input$variable_y))) +
      geom_point(aes(colour = diagnosis), alpha = 0.3) + 
      scale_colour_discrete(limits = c("B", "M"),
                       labels = c("benign", "malignant"))
  })

  output$summary_scatter <- renderPrint({
    regression <- lm(get(input$variable_y) ~ get(input$variable_x), data = cancer_sample)
    print(summary(regression))
  })
}

shinyApp(ui, server)