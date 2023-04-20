library(shiny)
library(shinydashboard)
library(DT)



dashboardPage(
  
  dashboardHeader(title="Exploring Twitter Sentiment Analysis with R & Shiny Dashboard", titleWidth = 550, 
                  tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/loepiinpiin/" ,icon("linkedin"), "My Profile", target="_blank")),
                  tags$li(class="dropdown",tags$a(href="https://github.com/Pp1420/dataset", icon("github"), "Source Code", target="_blank"))
                  ),
  
  
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Dataset", tabName = "data", icon = icon("database")),
                menuItem("Visualization", tabName = "viz", icon=icon("chart-line")),
                menuItem("Generate Report", tabName = "sectors", icon = icon("download"),
                         radioButtons('format', 'Document format', c('PDF', 'Word'),inline = FALSE, selected = 1),
                         downloadButton("report", "Download Report", class = "butt"),
                         tags$head(tags$style(".butt{color: blue !important;}"))),
                menuItem("Prediction Model", tabName = "model", icon=icon("check-square")),
                menuItem("Author", tabName = "author", icon=icon("user-circle"))
                
    )
  ),
  
  dashboardBody(
    
    tabItems(
      ## First tab item
      tabItem(tabName = "data", 
              tabBox(id="t1", width = 12, 
                     tabPanel("About", icon=icon("address-card"),
                              fluidRow(
                                column(width = 7, 
                                       tags$img(src = "sentiment_analysis.jpg", width = 550 , height = 400,
                                                style = "margin: 20px auto 0px auto; display: block;"),
                                       tags$br(), 
                                       tags$a("Source: KDnuggets", href = "https://www.altexsoft.com/blog/business/sentiment-analysis-types-tools-and-use-cases/", target = "_blank"), align = "center"),
                                column(width = 4, tags$br() ,
                                       tags$p("The dataset comprises 50,002 observations of a single variable named Tweet. The aim of this task is to identify potential cases of depression in the tweets using sentiment analysis. Consequently, the variables Date, Username, and URL were excluded from the dataset as they do not contribute to the objective of detecting depression in the tweets. By focusing solely on the text of the tweet, the sentiment analysis algorithm can better identify patterns and indicators of depression in the language used by the Twitter users."),
                                       style = "font-size: 18px; text-align: justify;margin-top: 45px; margin-bottom: 40px;"
                                )
                              )
                     ),
                     
                     tabPanel("Raw Data", 
                             div(style="height:670px; overflow-y:scroll", 
                                  DTOutput("dataT")), 
                              icon = icon("table")
                     ),
                     
                     tabPanel("Processed Data", 
                              div(style="height:670px; overflow-y:scroll", 
                                  DTOutput("Processed_dataT")), 
                              icon = icon("uncharted")
                     ),
                     
                    tabPanel("Text Stats",
                        fluidRow(
                          column(width = 12,
                            verbatimTextOutput("text_summary")
                          )
                        ),
                        fluidRow(
                          column(width = 12,
                            plotOutput("Text_stats_chart", height = "450px", width = "100%")
                          )
                        ),
                        HTML("<div style='text-align:center;'><p style='font-size:18px;font-weight:bold;margin-top:20px;'><p style='font-size:18px;'>The presented chart provides insights into the text cleaning process applied to the tweets dataset. It illustrates that the process involved several steps, such as converting all tweets to lowercase and eliminating numbers.</p></div>"),
                        icon = icon("chart-pie")
                      ),
                    
                    
                    tabPanel("Lexicons", icon=icon("book"),
                             fluidRow(
                               column(width = 5,
                                      tags$h4(strong("Process of Lexicon-Based Sentiment Analysis")),
                                      tags$br(),
                                      tags$img(src = "process.jpg", width = 350 , height = 480,
                                               style = "margin: 0px auto 0px auto; display: block;"),
                                      tags$br(), 
                                      tags$a("Source: Azeema Sadia", href = "https://ieec.neduet.edu.pk/2018/Papers_2018/15.pdf", target = "_blank"), align = "center"),
                               column(width = 7,
                                      plotOutput("wordslexicon", height = "600px", width = "100%")
                               )
                             )
                    ),
                    
                    
                    
                       

                     
              )
              
      ),
      
      
      # Second Tab Item
      tabItem(tabName = "viz", 
              tabBox(id="t2",  width=15,  
                            tabPanel(
                              title = "Main Dashboard",
                              icon=icon("dashboard"),
                              value = "page1",
                                column(width = 5.1,
                                       valueBox(num_rows, icon = icon("twitter"), color = "aqua", subtitle = "Number of Tweets Analyzed")
                                ),
                                column(width = 5.1, 
                                       valueBoxOutput("pos_perc_box")
                                ),
                              
                              column(width = 5.1, 
                                     valueBoxOutput("accuracybox")
                              )
                              ),
                                box(
                                  title = "Word Cloud",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  sliderInput(
                                    "max_words",
                                    label = "Adjust Maximum Words:", min = 50,max = 250, value = 150,step=10
                                  ),
                                  plotOutput("plot", height = "350px"),
                                  width = 4
                                ),
                                
                                column(width = 8,
                                       plotOutput("density_plot", height = "520px")
                                ),
                                
                                box(
                                  title = "Most Frequent Words by Lexicons"
                                  ,status = "primary"
                                  ,solidHeader = TRUE 
                                  ,collapsible = TRUE 
                                  ,radioButtons("plot_choice", "Select a Lexicon : ",
                                               choices = c("Bing", "NRC", "Syuzhet"),
                                               selected = "Bing")
                                  ,uiOutput("plot20", height = "300px")
                                  ,width = 12
                                ),
                                
                             #   fluidRow(
                              #    column(width = 12,
                               #          plotOutput("plotsyus", height = "520px")
                                #  ),
                              #  ),
                     
                     fluidRow(
                       box(
                         title = "Polarity of Text",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 12,
                         padding = 3.6, # Adjust padding here
                         sidebarLayout(
                           sidebarPanel(
                             textInput("userinput", "Enter text to analyze:"),
                             div(style = "text-align:center;",
                             actionButton("analyze_btn", "Analyze"),
                             actionButton("refresh_btn", "Refresh")
                             )
                             
                           ),
                           mainPanel(
                             verbatimTextOutput("sentiment_output")
                             
                           )
                         )
                       )
                     ),
                     
                     # Create a button
                    # actionButton("generate_report", "Generate Report"),
                     
                     # Add download link for the generated report
                    # downloadButton("download_report", "Download Report")
                     
                     
                     
                     
                                
                                
                                
                              )
                                
                                  
      ),
      
      ## Third tab item
      tabItem(tabName = "model", 
              tabBox(id="t1", width = 12, 
                     tabPanel("Summary", icon = icon("bar-chart"),
                              div(style="height:650px; overflow-y:scroll", 
                                  plotOutput("model_acc_plot"))
                              ),
                     
                     
                     
                     tabPanel("Naive Bayes",
                              div(style="height:650px; overflow-y:scroll",
                              plotOutput("NaiveHeatmap"))
                     ),
                     
                     tabPanel("Support Vector Machine",
                              div(style="height:650px; overflow-y:scroll",
                                  plotOutput("SVMHeatmap"))
                              
                     ),
                     
                     tabPanel("XGBoost",
                              div(style="height:650px; overflow-y:scroll",
                                  plotOutput("XGHeatmap"))
                             
                     )
                     
                     
              )
              
      ),
      
      ## Fourth tab item
      tabItem(tabName = "author", 
              tabBox(id="t1", width = 12, 
                     tabPanel("Profile", icon=icon("opera"),
                             fluidRow(
                                 column(
                                   width = 4, 
                                   tags$br(),
                                   tags$h3("About Me", style = "text-align: center; margin-top: 12px;"),
                                   tags$br(),
                                   tags$img(src = "profilepic.jpg", width = 230 , height = 230,
                                            style = "display: block; margin: -4px auto 10px auto; border-radius: 50%; box-shadow: 0px 0px 10px 0px rgba(0,0,0,0.5);"),
                                   tags$br(),
                                   tags$a("", target = "_blank", style = "display: block; text-align: center; margin-top: 10px; font-weight: bold; text-decoration: none; color: #333;"),
                                   align = "center"
                                 ),
                                 

                                      
                               column(
                                 width = 7, 
                                 style = "padding: 35px; background-color: #f2f2f2; border-radius: 10px; text-align: center; margin-top: 25px; margin-bottom: 25px",
                                 tags$span(
                                   style = "font-size: 25px; font-weight: bold; margin-top: 30px;",
                                   "Contact Information"
                                 ),
                                 tags$hr(
                                   style = "border-color: #ccc; margin: 28px 0;"
                                 ),
                                 tags$div(
                                   style = "font-size: 20px; text-align: left;",
                                   tags$a("Name", style = "color: blue; padding-right: 44px;"),
                                   tags$a("Loe Piin Piin", style = "color: blue;"),
                                   tags$br(),
                                   tags$a("Age", style = "color: blue; padding-right: 62px;"),
                                   tags$a("23 years old", style = "color: blue;"),
                                   tags$br(),
                                   tags$a("Contact", style = "color: blue; padding-right: 24px;"),
                                   tags$a("(+60)16-777 1420", style = "color: blue;"),
                                   tags$br(),
                                   tags$a("Email", href = "mailto:i20019694@student.newinti.edu.my", style = "color: blue; padding-right: 44px;"),
                                   tags$a("i20019694@student.newinti.edu.my", style = "color: blue;"),
                                   tags$br(),
                                   tags$a("LinkedIn", href = "https://www.linkedin.com/in/loepiinpiin/", style = "color: blue; padding-right: 20px;"),
                                   tags$a("https://www.linkedin.com/in/loepiinpiin/", style = "color: blue;")
                                 )
                                 
                                 
                               )
                               
                             )
                     )
                     
                     
              )
              
      )
      
      
      
      
      
     
                       
                )
                
        ),
                                
                                
                              )

                                
              
              
          
        
    
  