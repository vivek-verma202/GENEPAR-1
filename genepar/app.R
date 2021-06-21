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
                                    HTML("<div class='header-logo'>
                                          <a href='https://www.humanpaingenetics.ca/'>
                                          <img src='logo.png' height='50px'/>
                                          </a>
                                          <h2><strong>Interactive Exploratory Data Analysis of GENEPAR-1</strong></h1>
                                          </div>"),titleWidth = "100%"),
                    dashboardSidebar(
                      width = 250,
                      sidebarMenu(id = "MenuTabs",
                                  menuItem("Home", tabName = "Home", icon = icon("home")),
                                  menuItem("Univariate Analyses", tabName = "uniCon", icon = icon("chart-area")),
                                  menuItem("Bivariate Analyses", tabName = "biVar", icon = icon("bar-chart")),
                                  menuItem("Multivariate Analyses", tabName = "multiVar", icon = icon("sliders")),
                                  menuItem("Filters", tabName = "fil", icon = icon("filter")),
                                  menuItem("Documents", tabName = "docu", icon = icon("file-download"))
                                  
                      )),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "Home",
                                fluidPage(
                                  fixedRow(
                                    column(10,
                                           align = "left",
                                           HTML("<style>
                                                .header-logo {
                                                  color: #B0361B;
                                                  position: absolute;
                                                  top: -7px;
                                                  width: 100%;
                                                }
                                                .header-logo a {
                                                    float: left;
                                                    margin-top: 5px;
                                                  }
                                                .content {
                                                  background-color: white;
                                                }
                                                .st-table {
                                                  width:100%
                                                }
                                                .footer {
                                                  width: 80%;
                                                  float: right;
                                                }
                                                </style>"),
                                           HTML("<i>"),
                                           h3(strong("GENEPAR-1 participants are  a subset of Quebec Pain Registry (QPR) patients with lower back pain.")),
                                           HTML("</i>"),
                                           HTML("<div style='height: 5px;'>"),
                                           HTML("</div>"),
                                           h2(strong("GENEPAR-1 dataset: Variable description")),
                                           dataTableOutput('tbl'),
                                           HTML("<div style='height: 15px;'>"),
                                           HTML("</div>"),
                                           h2(strong("GENEPAR-1 dataset: Summary of all variables")),
                                           htmlOutput("tbl1"),
                                           HTML("<div style='display: block;padding-bottom:50px'>"),
                                           HTML("</div>"))))),
                        tabItem(tabName = "uniCon",
                                fluidRow(
                                  box(selectInput(
                                    inputId = "contVar",
                                    label = "Select a continuous variable:",
                                    choices = names(df %>% select(where(is.double)))),
                                    verbatimTextOutput('tbl2a'),
                                    box(width = 9, title = "Histogram",
                                        status = "primary",
                                        plotlyOutput("hist")),
                                    box(width = 3, title = "Q-Q Plot",
                                        status = "primary",
                                        plotlyOutput("qqp")),
                                    box(width = 12, title = "Box and whiskers plot",
                                        status = "primary",
                                        plotlyOutput("bwp")),
                                  ),
                                  box(
                                    selectInput(
                                      inputId = "catVar",
                                      label = "Select a categorical variable:",
                                      choices = names(df %>% select(where(is.factor)))),
                                    verbatimTextOutput('tbl2b'),
                                    box(width = 12, title = "Barplot",
                                        status = "primary",
                                        plotlyOutput("bar")),
                                    box(width = 12, title = "Piechart",
                                        status = "primary",
                                        plotlyOutput("pie"))
                                  )
                                )),
                        tabItem(tabName = "biVar",
                                fluidRow(box())),
                        tabItem(tabName = "multiVar",
                                fluidRow(box(width = 3,
                                             selectInput(
                                               inputId = "x",
                                               label = "Select X variable:",
                                               choices = names(df %>% select(where(is.double)))),
                                             selectInput(
                                               inputId = "y",
                                               label = "Select Y variable:",
                                               choices = names(df %>% select(where(is.double)))),
                                             selectInput(
                                               inputId = "z",
                                               label = "Select Z variable:",
                                               choices = names(df %>% select(where(is.double))))),
                                         box(width = 9, title = "3D Surface Plot",
                                             status = "primary",
                                             plotlyOutput("p3d")))),
                        tabItem(tabName = "fil",
                                fluidRow(box())),
                        tabItem(tabName = "docu",
                                fluidRow(box(width = 12,
                                             h2(strong("GENEPAR-1 - Supporting files")),
                                             h3(strong("1. Entire GENEPAR-1 dataset")),
                                             )))
                        ))
),
tags$footer(HTML("<div class='footer'>",
  paste(icon("globe"),"<a href='https://www.humanpaingenetics.ca/' target='blank' style='padding-right:10px; padding-left:3px'>Human Pain Genetics Lab</a>"),
  paste(icon("github"),"<a href='https://github.com/vivek-verma202/GENEPAR-1' target='blank' style='padding-right:10px; padding-left:3px'>Source code and license information</a>"),
  paste(icon("envelope"),"<a href='mailto:vivek.verma@mail.mcgill.ca' style='padding-right:10px; padding-left:3px'>vivek.verma@mail.mcgill.ca</a>"),
  paste(icon("copyright"),"<span style='padding-right:10px; padding-left:3px'>Vivek Verma</span>"),
  paste(icon("balance-scale"),"<span style='padding-right:10px; padding-left:3px'>GNU General Public License v3.0</span></div>")), 
  align = "center", style = "
              bottom:0;
              width:100%;
              height:50px;   /* Height of the footer */
              color: white;
              padding: 10px;
              background-color: #222d32;"))

# Define server logic ----
server <- function(input, output) {
  
  output$tbl <- renderDataTable(read.delim("DICTIONARY.txt"))
  
  output$tbl1  <- renderUI({
    SumProfile <- print(dfSummary(readRDS("GENEPAR1.RDS")[,-c(1)]),
                        omit.headings = T, method = 'render',
                        bootstrap.css = F)
    SumProfile})
  
  output$hist  <- renderPlotly({
    df1 <- df %>% select(input$contVar) %>% 
      drop_na() %>% rename("xvar"=input$contVar)
    bw <- 3*IQR(df1$xvar)/length(df1$xvar)^(1/3)
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
               scale_fill_brewer(palette = "Set3") + 
               xlab(input$catVar) + theme_bw() + 
               theme(legend.position = "none")
    )
  }) 
  
  output$pie  <- renderPlotly({
    df1 <- df %>% select(input$catVar) %>% 
      rename("xvar"=input$catVar) %>%
      group_by(xvar) %>% 
      summarise(count = n())
    fig <- plot_ly(df1, labels = ~xvar, values = ~count, type = 'pie',
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   hoverinfo = 'text',
                   showlegend = F) %>% layout(title = input$catVar,
                          xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
                          yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
    
    fig
  }) 
  
  output$bwp <- renderPlotly({
    df1 <- df %>% select(c("ID",input$contVar)) %>% 
      drop_na() %>% rename("xvar"=input$contVar)
      ggplotly(
        ggplot(data = df1,aes(x = 1,y = xvar)) +
          geom_boxplot(colour = "black",lwd=1.5,fill = "cyan") + 
          geom_jitter(aes(text = ID),width = 0.2, alpha = 0.4) +
          ylab(input$contVar) +
          coord_flip() +
          theme_classic() + 
          theme(axis.line = element_line(colour = 'black', size = 2),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(colour = "black", size = 2),
                axis.text.x = element_text(color = "black", size = 20),
                axis.text.y = element_blank(),
                axis.title.x = element_text(color = "black", size = 0),
                axis.title.y = element_blank(),
                legend.position = "none")
      )
  }) 
  
  output$tbl2a   <- renderPrint({
    df %>% select(input$contVar) %>% summary() %>% 
      print(method = 'render',bootstrap.css = F)
    }) 
  output$qqp   <- renderPlotly({
    df1 <- df %>% select(c("ID",input$contVar)) %>% 
      drop_na() %>% rename("xvar"=input$contVar)
    ggplotly(
      ggplot(data = df1,aes(sample = xvar)) +
        geom_qq() + geom_qq_line() +
        labs(title = "Q-Q Plot") +
        theme_classic()
      )
  }) 

  output$tbl2b   <- renderPrint({
    df %>% select(input$catVar) %>% summary() %>% 
      print(method = 'render',bootstrap.css = F)
  }) 
  output$p3d  <- renderPlotly({
    plot_ly(z = ~volcano, type = "scatter3d")
  })
}

# Run the app ----
shinyApp(ui, server)