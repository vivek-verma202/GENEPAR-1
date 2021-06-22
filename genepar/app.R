library(shiny)
library(shinydashboard)
library(DT)
library(summarytools)
library(plotly)
library(RColorBrewer)
library(cowplot)
library(tidyverse)
library(heatmaply)
library(ggcorrplot)
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
                                                .content-wrapper {
                                                  min-height: 620px !important;
                                                }
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
                                                pre {
                                                  height: 150px;
                                                }
                                                .download-links li {
                                                  font-size: 20px;
                                                  font-weight: bold;
                                                  width: 40%;
                                                  padding-bottom: 4px;
                                                }
                                                .download-links i {
                                                  padding-left: 20px;
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
                                    choices = sort(names(df %>% select(where(is.double))))),
                                    verbatimTextOutput('tbl2a'),
                                    box(width = 9, title = "Histogram",
                                        status = "primary",
                                        plotlyOutput("hist")),
                                    box(width = 3, title = "QQ Plot",
                                        status = "primary",
                                        plotlyOutput("qqp")),
                                    box(width = 12, title = "Box and whiskers plot",
                                        status = "primary",
                                        plotlyOutput("bwp"))
                                  ),
                                  box(
                                    selectInput(
                                      inputId = "catVar",
                                      label = "Select a categorical variable:",
                                      choices = sort(names(df %>% select(where(is.factor))))),
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
                                fluidRow(tabBox(
                                  width = "12",
                                  tabPanel(HTML("<strong>Continuous or Ordinal Variables</strong>"),
                                    fluidRow(
                                      box(width = 12,
                                          title = "Kendall's Tau Correlation Matrix",
                                          plotlyOutput("cp", height = 800)),
                                      box(width = 3,
                                          selectInput(
                                          inputId = "x",
                                          label = "Select X variable:",
                                          choices = sort(names(df %>% select(where(is.numeric) | where(is.ordered))))),
                                          selectInput(
                                            inputId = "y",
                                            label = "Select Y variable:",
                                            choices = sort(names(df %>% select(where(is.numeric) | where(is.ordered))))),
                                          selectInput(
                                            inputId = "z",
                                            label = "Color by:",
                                            choices = sort(names(df %>% select(where(is.factor)))))),
                                      box(width = 9,title = "Scatterplot",
                                          plotlyOutput("sp")))),
                                  tabPanel(HTML("<strong>Categorical Variables</strong>"),
                                           fluidRow(
                                             box(
                                               selectInput(
                                               inputId = "x1",
                                               label = "Select X variable:",
                                               choices = sort(names(df %>% select(where(is.factor))))
                                               )),
                                             box(
                                               selectInput(
                                                 inputId = "y1",
                                                 label = "Select Y variable:",
                                                 choices = sort(names(df %>% select(where(is.factor))))
                                               )),
                                             box(title = "Stacked bar plot",
                                               width = 12,
                                               plotlyOutput("br1")
                                             ),
                                             box(title = "Grouped bar plot",
                                               width = 12,
                                               plotlyOutput("br2")
                                             ),
                                             box(title = "Segmented bar plot",
                                               width = 12,
                                               plotlyOutput("br3")
                                             )
                                             )),
                                  tabPanel(HTML("<strong>Categorical vs. Continuous Variables</strong>"),
                                           fluidRow(
                                             box(
                                               selectInput(
                                                 inputId = "x2",
                                                 label = "Select a categorical variable:",
                                                 choices = sort(names(df %>% select(where(is.factor))))
                                               )),
                                             box(
                                               selectInput(
                                                 inputId = "y2",
                                                 label = "Select a continuous variable:",
                                                 choices = sort(names(df %>% select(where(is.numeric))))
                                               )),
                                             box(title = "Grouped kernel density plot",
                                                 width = 12,
                                                 plotlyOutput("bx1")
                                             ),
                                             box(title = "Box and whiskers plot",
                                                 width = 12,
                                                 plotlyOutput("bx2")
                                             ),
                                             box(title = "XYZ plot",
                                                 width = 12,
                                                 plotlyOutput("bx3")
                                             )
                                           ))
                                ))),
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
                                             HTML('<img src="test.gif" style="width:60%" />')))),
                        tabItem(tabName = "fil",
                                fluidRow(
                                  HTML('<img src="filter.gif" style="padding-left:30%" />')
                                )),
                        tabItem(tabName = "docu",
                                fluidPage(
                                  fixedRow(
                                    column(10,
                                           align = "left",
                                          
                                       HTML("<h3><strong>Download Links</strong></h3>"),
                                       HTML("<ul class='download-links'>"),
                                       HTML("<li>",
                                       paste("<a href='LBP_extraction.xlsx' target='blank'>Entire Genepar-1 dataset", icon("database", class='icon-download')), "</a></li>"),
                                       HTML("<li>",
                                            paste("<a href='DICTIONARY.txt' target='blank'>Dictionary", icon("book-open", class='icon-download')), "</a></li>"),
                                       HTML("<li>",
                                            paste("<a href='GENEPAR-1.txt' target='blank'>GENEPAR-1", icon("file-alt", class='icon-download')), "</a></li>"),
                                       HTML("<li>",
                                            paste("<a href='List_variables_GENEPAR_final.xlsx' target='blank'>List variables GENEPAR final", icon("list-ul", class='icon-download')), "</a></li>"),
                                       HTML("<li>",
                                            paste("<a href='QPR_code_book_2014.pdf' target='blank'>QPR code book(2014)", icon("book", class='icon-download')), "</a></li>"),
                                       HTML("<li>",
                                            paste("<a href='QUESTION_EN.docx' target='blank'>Questionnaire(EN)", icon("question-circle", class='icon-download')), "</a></li>"),
                                       HTML("</ul>")
                                       
                                       )
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
      rename("xvar"=input$contVar)
      ggplotly(
        ggplot(data = df1,aes(x = 1,y = xvar)) +
          geom_boxplot(colour = "black",lwd=1.5,fill = "cyan") + 
          geom_jitter(aes(text = ID),width = 0.2, alpha = 0.4) +
          ylab(input$contVar) +
          coord_flip() +
          theme_classic() + 
          theme(axis.line = element_line(colour = 'black', size = 1),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(colour = "black", size = 1),
                axis.text.x = element_text(color = "black", size = 15),
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
        geom_qq(alpha = 0.2, size = 0.5) + geom_qq_line() +
        theme_minimal() +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank()), height = 150, width = 150
      )
  }) 

  output$tbl2b   <- renderPrint({
    df %>% select(input$catVar) %>% summary() %>% 
      print(method = 'render',bootstrap.css = F)
  }) 
  output$p3d  <- renderPlotly({
    plot_ly(z = ~volcano, type = "scatter3d")
  })
  
  output$cp <- renderPlotly({
    df1 <- df %>% select(where(is.numeric) | where(is.ordered)) %>% sapply(as.numeric)
    heatmaply_cor(
      cor(df1, use = "pairwise.complete.obs", method = "kendall"),
      k_col = 2, k_row = 2)
  })
  
  output$sp <- renderPlotly({
    df1 <- df %>% select(c("ID",input$x,input$y,input$z)) %>% 
      rename("xvar"=input$x,"yvar"=input$y,"zvar"=input$z)
    df1[,c("xvar","yvar")] <- sapply(df1[,c("xvar","yvar")],as.numeric)
    ggplotly(
      ggplot(df1, aes(x=xvar,y=yvar)) +
        geom_jitter(aes(text=ID,color = zvar),width = 0.2) +
        geom_smooth(aes(color = zvar)) +
        labs(x = input$x, y = input$y, color = input$z) +
        theme_classic() +
        scale_fill_brewer(palette = "Set3") +
        scale_color_brewer(palette = "Set3"), height = 430
    )
  })
  output$br1 <- renderPlotly({
    df1 <- df %>% select(c(input$x1,input$y1)) %>% 
      rename("xvar"=input$x1,"yvar"=input$y1)
    ggplotly(
      ggplot(df1, aes(x=xvar,fill=yvar)) +
        geom_bar(position = "stack",color = "black", lwd = 0.5) +
        labs(x = input$x1, y = "ratio") +
        scale_fill_brewer(palette = "Set3") +
        theme_classic()
    )
  })
  output$br2 <- renderPlotly({
    df1 <- df %>% select(c(input$x1,input$y1)) %>% 
      rename("xvar"=input$x1,"yvar"=input$y1)
    ggplotly(
      ggplot(df1, aes(x=xvar,fill=yvar)) +
        geom_bar(position = "dodge",color = "black", lwd = 0.5) +
        labs(x = input$x1) +
        scale_fill_brewer(palette = "Set3") +
        theme_classic()
    )
  })
  output$br3 <- renderPlotly({
    df1 <- df %>% select(c(input$x1,input$y1)) %>% 
      rename("xvar"=input$x1,"yvar"=input$y1)
    ggplotly(
      ggplot(df1, aes(x=xvar,fill=yvar)) +
        geom_bar(position = "fill",color = "black", lwd = 0.5) +
        labs(x = input$x1, y = "ratio") +
        scale_fill_brewer(palette = "Set3") +
        theme_classic()
    )
  })
  output$bx1 <- renderPlotly({
    df1 <- df %>% select(c(input$x2,input$y2)) %>% 
      rename("xvar"=input$x2,"yvar"=input$y2)
    ggplotly(
      ggplot(df1, aes(x=yvar,fill=xvar)) +
        geom_density(alpha = 0.4) +
        labs(x = input$y2) +
        scale_fill_brewer(palette = "Set3") +
        theme_classic()
    )
  })
  output$bx2 <- renderPlotly({
    df1 <- df %>% select(c("ID",input$x2,input$y2)) %>% 
      rename("xvar"=input$x2,"yvar"=input$y2)
    ggplotly(
      ggplot(data = df1,aes(x = xvar,y = yvar)) +
        geom_boxplot(aes(fill = xvar),colour = "black",lwd=1) + 
        geom_jitter(aes(text = ID),width = 0.2, alpha = 0.4) +
        labs(x = input$x2, y = input$y2) +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
        scale_fill_brewer(palette = "Set3") +
        theme_classic() + 
        theme(axis.line = element_line(colour = 'black', size = 1),
              axis.ticks = element_line(colour = "black", size = 1),
              axis.text = element_text(color = "black", size = 15),
              axis.title = element_text(color = "black", size = 10),
              legend.position = "right")
    )
    })
}

# Run the app ----
shinyApp(ui, server)