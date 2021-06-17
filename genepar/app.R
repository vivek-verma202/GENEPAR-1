library(shiny)
library(shinydashboard)
library(DT)
library(summarytools)
library(plotly)
library(RColorBrewer)
library(cowplot)
library(tidyverse)
df <- readRDS("GENEPAR1.RDS")

# Define UI ----

ui <- tagList(dashboardPage(title="GENEPAR-1", 
                    skin = "black",
                    dashboardHeader(title = 
                                      tags$a(href='https://www.humanpaingenetics.ca/',
                                             alt='GENEPAR-1',
                                             tags$img(src='logo.png',height=50,width=150,
                                                      align = "centre")),titleWidth = 250),
                    dashboardSidebar(
                      width = 250,
                      sidebarMenu(id = "MenuTabs",
                                  menuItem("Home", tabName = "Home", icon = icon("home")),
                                  menuItem("Univariate Analysis (Continuous)", tabName = "uniCon", icon = icon("chart-area")),
                                  menuItem("Univariate Analysis", tabName = "Univariate", icon = icon("bar-chart")),
                                  menuItem("Bi Variate Scatter Plot", tabName = "ScatterPlot", icon = icon("dot-circle-o")),
                                  menuItem("Bi Variate Box Plot", tabName = "BoxPlot", icon = icon("sliders")),
                                  menuItem("Bi Variate Group Bar Plot", tabName = "GBarPlot", icon = icon("bar-chart")),
                                  menuItem("Bi Variate Group Histogram", tabName = "GHistPlot", icon = icon("area-chart")),
                                  menuItem("Multi Variate Scatter Plot", tabName = "MultiScatterPlot", icon = icon("dot-circle-o")),
                                  menuItem("Multi Variate Box Plot", tabName = "MultiBoxPlot", icon = icon("sliders")),
                                  menuItem("EDA", tabName = "EDA", icon = icon("info-circle"))
                      )),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "Home",
                                fluidPage(
                                  fixedRow(
                                    column(10,
                                           align = "left",
                                           HTML("<style>
                                                .content {
                                                  background-color: white;
                                                }
                                                .st-table {
                                                  width:100%
                                                }
                                                </style>"),
                                           h1(strong("Interactive Exploratory Data Analysis of GENEPAR-1")),
                                           HTML("<i>"),
                                           h3(strong("GENEPAR-1 participants are  a subset of Quebec Pain Registry (QPR) patients with lower back pain.")),
                                           HTML("</i>"),
                                           HTML("<div style='height: 5px;'>"),
                                           HTML("</div>"),
                                           htmlOutput('tbl'),
                                           HTML("<div style='height: 15px;'>"),
                                           HTML("</div>"),
                                           h2(strong("Entire GENEPAR-1 dataset:")),
                                           DTOutput("tbl1"),
                                           HTML("<div style='display: block;padding-bottom:50px'>"),
                                           HTML("</div>"))))),
                        tabItem(tabName = "uniCon",
                                fluidRow(
                                  box(selectInput(
                                    inputId = "contVar",
                                    label = "Select a continuous variable:",
                                    choices = names(df %>% select(where(is.double)))),
                                    box(width = 12, title = "Histogram",
                                        status = "primary",
                                        plotlyOutput("hist"))
                                    
                                  ),
                                  box(
                                    selectInput(
                                      inputId = "catVar",
                                      label = "Select a categorical variable:",
                                      choices = names(df %>% select(where(is.factor)))),
                                    box(width = 12, title = "Barplot",
                                        status = "primary",
                                        plotlyOutput("bar"))
                                  )
                                ))))
),
tags$footer(HTML(
  paste(icon("globe"),"<a href='https://www.humanpaingenetics.ca/' target='blank' style='padding-right:10px; padding-left:3px'>Human Pain Genetics Lab</a>"),
  paste(icon("github"),"<a href='https://github.com/vivek-verma202/GENEPAR-1' target='blank' style='padding-right:10px; padding-left:3px'>Source code and license information</a>"),
  paste(icon("envelope"),"<a href='mailto:vivek.verma@mail.mcgill.ca' style='padding-right:10px; padding-left:3px'>vivek.verma@mail.mcgill.ca</a>"),
  paste(icon("copyright"),"<span style='padding-right:10px; padding-left:3px'>Vivek Verma</span>"),
  paste(icon("balance-scale"),"<span style='padding-right:10px; padding-left:3px'>GNU General Public License v3.0</span>")), 
  align = "center", style = "
              position:fixed;
              bottom:0;
              width:100%;
              height:50px;   /* Height of the footer */
              color: white;
              padding: 10px;
              background-color: #222d32;
              z-index: 1000;"))

# Define server logic ----
server <- function(input, output) {
  output$tbl   <- renderUI({
    SumProfile <- print(dfSummary(readRDS("GENEPAR1.RDS")),
                        omit.headings = T, method = 'render',
                        bootstrap.css = F)
    SumProfile})
  output$tbl1  <- renderDT({df})
  x <- reactive({
    df %>% select(input$contVar) %>% drop_na(input$contVar)
  })
  #bw <- reactive({2*IQR(x())/length(x())^(1/3)})
  output$hist  <- renderPlotly({
    df1 <- df %>% select(input$contVar) %>% 
      drop_na() %>% rename("xvar"=input$contVar)
    bw <- 2*IQR(df1$xvar)/length(df1$xvar)^(1/3)
    ggplotly(ggplot(df1, aes(x = xvar)) + 
               geom_histogram(aes_string(fill = "..count..",
                                         color = "..count.."),
                              binwidth = bw) +
               scale_fill_gradient(low="blue",high="pink") +
               scale_color_gradient(low="blue",high="pink") +
               xlab(input$contVar) + theme_bw() +
               theme(legend.position = "none"))
  })
  
  output$bar  <- renderPlotly({
    df1 <- df %>% select(input$catVar) %>% 
      drop_na() %>% rename("xvar"=input$catVar) %>%
      group_by(xvar) %>% 
      summarise(count = n())
    ggplotly(ggplot(df1, aes(reorder(xvar,(-count)), y = count, fill = xvar)) +
               geom_bar(stat = "identity") + 
               scale_fill_brewer(palette = "Set1") + 
               xlab(input$catVar) + theme_bw() + 
               theme(legend.position = "none")
    )
  })
  
}

# Run the app ----
shinyApp(ui, server)