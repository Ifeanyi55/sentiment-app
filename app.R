
    library(shiny)
    library(shinyalert)  
    library(shinycssloaders)  
    library(shinythemes)
    library(shinyWidgets)
    library(shinyjs)  
    library(SentimentAnalysis)
    library(SnowballC)
    library(textclean)
    library(wordcloud2)
    library(reactable)
    library(ggplot2)
    library(plotly)
    library(rsconnect)
    library(curl)  
    library(tm)
    library(purrr)
    
    options(spinner.color="#3498db",
            spinner.color.background="#ffffff",
            spinner.size=1)  
    
    in.p.name<-paste0("p",1:5)
    in.t.name<-paste0("t",1:5)
    in.r.name<-paste0("r",1:5)
    in.s.name<-paste0("s",1:5)
    
    ui<-list(useShinyjs(),navbarPage(strong("Mavis Analytic"),theme=shinytheme("cerulean"),
                                     windowTitle="Mavis Analytic",fluid=TRUE,inverse=FALSE,
                                     tabPanel(strong("Opinion Miner"),icon=icon("table"),useShinyalert(),
                                              setBackgroundColor("white"),setBackgroundImage(src="white.jpg"),        
                                              sidebarLayout(
                                                  sidebarPanel(width=3,
                                                               img(src="logo.jpg",height=130,width=150),
                                                               h4(strong("Enter your texts in these fields")),
                                                               actionButton("clear",strong("Clear Fields"),icon=icon("broom")),br(),br(),
                                                               textAreaInput("text","Text Field 1",value="It is a beautiful day"),
                                                               textAreaInput("texts","Text Field 2",value="I am happy to be here"),
                                                               textAreaInput("word","Text Field 3",value="Let's have some fun"),
                                                               textAreaInput("words","Text Field 4",value="It has been a bad outing"),
                                                               textAreaInput("wordy","Text Field 5",value="I dislike clowns"),
                                                               actionButton("run",strong("Run Analysis"),icon=icon("caret-right")),br(),hr(),h5(strong("The number of words entered into each text field:")),reactableOutput("count",width="40%")),
                                                          
                                                  mainPanel(h4("The Opinion Miner is a tool for conducting sentiment analysis. It is useful for analyzing and scoring the kinds of sentiments expressed in texts, such as in literature, social media posts, or product reviews."),hr(),h3(strong("Table of Sentiment Scores")),withSpinner(reactableOutput("table"),type=1),downloadButton("download",strong("Download Table")),br(),br(),
                                                            fluidRow(column(4,selectInput("choice","Select Sentiment Score to Plot",choices=c("QDAP","LoughranM","HarvardIV"))),column(4,selectInput("color","Select Color",choices=c("Blue","Red","Green","Yellow","Purple")))),hr(),
                                                            fluidRow(column(6,withSpinner(plotOutput("plot"),type=1)),column(6,withSpinner(plotOutput("graph"),type=1))),
                                                            fluidRow(column(6,downloadButton("plot.down",strong("Download Graph")))), br(),hr(),
                                                            fluidRow(column(6,withSpinner(wordcloud2Output("cloud"),type=1)),column(6,withSpinner(reactableOutput("cloudable"),type=1))),
                                                            fluidRow(column(6,sliderInput("cloudslide",strong("Adjust text size"),min=0.1,max=0.6,value=0.2,step=0.05)),column(6,sliderInput("tabslide",strong("Most frequently occuring words"),min=5,max=20,value=10,step=1))),
                                                            fluidRow(column(6,radioButtons("shapes",strong("Change word cloud shape"),choices=c("Circle","Cardoid","Diamond","Triangle","Pentagon","Star"),selected="Circle")),column(6,downloadButton("freqdown",strong("Download table"))))))),
                                     
                                     tabPanel(strong("Profitability Ratios Calculator"),icon=icon("chart-bar"),
                                              sidebarLayout(
                                                  sidebarPanel(width=2,h4(strong("Profit figures:")),
                                                               actionButton("delete",strong("Clear Fields"),icon=icon("broom")),br(),br(),  
                                                               textInput(in.p.name[1],"Profit 1",value="100000",width=150),
                                                               textInput(in.p.name[2],"Profit 2",value="150000",width=150),
                                                               textInput(in.p.name[3],"Profit 3",value="200000",width=150),
                                                               textInput(in.p.name[4],"Profit 4",value="250000",width=150),
                                                               textInput(in.p.name[5],"Profit 5",value="300000",width=150),
                                                               hr(),
                                                               h4(strong("Turnover figures:")),
                                                               actionButton("remove",strong("Clear Fields"),icon=icon("broom")),br(),br(),
                                                               textInput(in.t.name[1],"Turnover 1",value="350000",width=150),
                                                               textInput(in.t.name[2],"Turnover 2",value="300000",width=150),
                                                               textInput(in.t.name[3],"Turnover 3",value="420000",width=150),
                                                               textInput(in.t.name[4],"Turnover 4",value="600000",width=150),
                                                               textInput(in.t.name[5],"Turnover 5",value="550000",width=150),
                                                               actionButton("go",strong("Calculate Ratio"),icon=icon("caret-right"))),
                                                               
                                                                 
                                                  mainPanel(h4("The Profitability Ratios Calculator makes a comparative analysis of a company's profitability ratios over time."),hr(),
                                                            h3(strong("Profitability Ratios")),h4("(Profit/Turnover): Calculates how much profit margin (%) a company makes on its sales"),
                                                            fluidRow(column(12,withSpinner(plotlyOutput("bar",width=750,height=500),type=1))),
                                                            fluidRow(column(12,downloadButton("drop",strong("Download Bar Plot")))),br(),hr(),
                                                            fluidRow(column(12,withSpinner(plotOutput("pie",width=750,height=500),type=1))),
                                                            fluidRow(column(12,downloadButton("fall",strong("Download Pie Chart"))))
                                                            )
                                                  
                                              )),
                                     tabPanel(strong("Efficiency Ratios Calculator"),icon = icon("chart-bar"),
                                       sidebarLayout(
                                         sidebarPanel(width=2,
                                           h4(strong("Receivables figures:")),
                                           actionButton("vanish",strong("Clear Fields"),icon=icon("broom")),br(),br(),
                                           textInput(in.r.name[1],"Receivables 1",value="150000",width=150),
                                           textInput(in.r.name[2],"Receivables 2",value="100000",width=150),
                                           textInput(in.r.name[3],"Receivables 3",value="50000",width=150),
                                           textInput(in.r.name[4],"Receivables 4",value="80000",width=150),
                                           textInput(in.r.name[5],"Receivables 5",value="170000",width=150),
                                           hr(),
                                           h4(strong("Turnover figures:")),
                                           actionButton("disappear",strong("Clear Fields"),icon=icon("broom")),br(),br(),
                                           textInput(in.s.name[1],"Turnover 1",value="350000",width=150),
                                           textInput(in.s.name[2],"Turnover 2",value="300000",width=150),
                                           textInput(in.s.name[3],"Turnover 3",value="420000",width=150),
                                           textInput(in.s.name[4],"Turnover 4",value="600000",width=150),
                                           textInput(in.s.name[5],"Turnover 5",value="550000",width=150),
                                           actionButton("away",strong("Calculate Ratio"),icon=icon("caret-right"))),
                                           
                                           mainPanel(
                                           h3(strong("Efficiency Ratios Calculator")),h4("(Receivables/Turnover): Calculates the average length of time (in days) it takes for a company to recover its debts"),
                                           fluidRow(column(6,withSpinner(plotOutput("sphere"),type=1)),column(6,withSpinner(plotlyOutput("bin"),type=1))),
                                           fluidRow(column(6,downloadButton("down",strong("Download Pie Chart"))),column(6,downloadButton("plummet",strong("Download Bar Plot"))))
                                         )
                                       )
                                     ),
                                     tabPanel(strong("About Mavis Analytic"),icon=icon("info"),mainPanel(h4(div(style="color:#5499c7","Mavis Analytic is designed as a multi-functional web application, with a variety of features that make it a modern business tool.
                                                                                                                    It currently incorporates tools for conducting sentiment analysis and financial performance evaluation. The goal of the designers of Mavis Analytic, Mavis Naissance Consulting,
                                                                                                                    is to keep expanding the scope of the application, by creating more functions that will make it a very versatile software for a wide range of business uses. ")),img(src="New.jpg",height=150,width=150))),
                                     tabPanel(strong("Send Us Your Feedback"),icon=icon("envelope"),
                                              HTML("<iframe src=https://docs.google.com/forms/d/e/1FAIpQLSex-eKi33LdmZE7L3PwjWm4tFbFoEIw_L_KUvsSi6kgdXgJPA/viewform?embedded=true width=1400 height=1000 frameborder=0 marginheight=0 marginwidth=0>Loading.</iframe>")),
                                     navbarMenu(strong("More"),
                                                tabPanel(strong("Graphs and Charts"),icon=icon("chart-bar"),sidebarLayout(sidebarPanel(width=3, selectInput("select","Select Variable to Plot",choice=c(
                                                    "Indegree","Outdegree","Degree","Normalized",
                                                    "Domain","Proximity","Modularity","Clusters"
                                                ),selected="Modularity"),sliderInput("slide","Change Pie Chart Angle",min=45,max=180,animate=TRUE,value=45,step=1)),
                                                
                                                mainPanel(fluidRow(column(6,plotOutput("draw")),column(6,plotOutput("art"))),br(),
                                                          fluidRow(column(6,plotOutput("paint")),column(6,plotOutput("part")))))),
                                                
                                                tabPanel(strong("Tables"),icon=icon("table"),
                                                         mainPanel(h3("Network data pulled from Twitter interactions on different subjects "),actionButton("detail",strong("Table Info")),downloadButton("table.down",strong("Download Table")),reactableOutput("metrics.table"),hr(),
                                                                   actionButton("more",strong("Table Info")),downloadButton("flat",strong("Download Table")), reactableOutput("freedom.table"))),
                                                tabPanel(strong("Library"),icon=icon("file-pdf"),
                                                         fluidRow(column(6,tags$iframe(style="height:400px;width:100%;scrolling=yes",src="analysis.pdf")),column(6,tags$iframe(style="height:400px;width:100%;scrolling=yes",src="Audits.pdf"))),br(),
                                                         fluidRow(column(6,tags$iframe(style="height:400px;width:100%;scrolling=yes",src="pricing.pdf")),column(6,tags$iframe(style="height:400px;width:100%;scrolling=yes",src="tech.pdf")))))
                                     
    ))
    server<-function(input,output,session){
        
        observeEvent(input$clear,{
            updateTextAreaInput(session,"text",value="",placeholder="Enter new text")
            updateTextAreaInput(session,"texts",value="",placeholder="Enter new text")
            updateTextAreaInput(session,"word",value="",placeholder="Enter new text")
            updateTextAreaInput(session,"words",value="",placeholder="Enter new text")
            updateTextAreaInput(session,"wordy",value="",placeholder="Enter new text")
            
        })
        
        observeEvent(input$run,{
            shinyalert(title="Running Analysis",text="Please be patient while the app cleans the texts and runs the analysis",
                       closeOnEsc=TRUE,confirmButtonText="OK",timer=10000,animation=TRUE,closeOnClickOutside=TRUE)
        })
        
        observeEvent(input$detail,{
            shinyalert(title="Disease on Twitter",text="This table contains the metrics underlying the network of Twitter interactions about disease, using the #disease hashtag",
                       closeOnEsc=TRUE,closeOnClickOutside=TRUE,animation=TRUE,confirmButtonText="OK",timer=10000)
        })
        
        observeEvent(input$more,{
            shinyalert(title="Freedom on Twitter",text="This table contains the statistical data of the Twitter conversation about freedom, using the #freedom hashtag",
                       closeOnEsc=TRUE,closeOnClickOutside=TRUE,animation=TRUE,confirmButtonText="OK",timer=10000)
        })
        observeEvent(input$delete,{
          walk(
          .x = in.p.name,
          .f = ~updateTextInput(session,.x,value="",placeholder="0"))
            
        })
        observeEvent(input$remove,{
          walk(
          .x = in.t.name,
          .f = ~updateTextInput(session,.x,value="",placeholder="0"))
            
        })
        observeEvent(input$vanish,{
          walk(
            .x = in.r.name,
            .f = ~updateTextInput(session,.x,value="",placeholder="0"))
          
        })
        observeEvent(input$disappear,{
          walk(
            .x = in.s.name,
            .f = ~updateTextInput(session,.x,value="",placeholder="0"))
              
        })
        
        doc<-reactive({c(if(nchar(input$text)>0){input$text},
                         if(nchar(input$texts)>0){input$texts},
                         if(nchar(input$word)>0){input$word},
                         if(nchar(input$words)>0){input$words},
                         if(nchar(input$wordy)>0){input$wordy}
        )
        })
        
        
        profit<-reactive({
            map_dbl(
              in.p.name,
              ~{
                num<-as.numeric(input[[.]])
                ifelse(num > 0,num,NA_real_)
              }
            )
        })
        
        turnover<-reactive({
            map_dbl(
              in.t.name,
              ~{
                num<-as.numeric(input[[.]])
                ifelse(num > 0,num,NA_real_)
              }
            )
            
        })
        
        receivables<-reactive({
            map_dbl(
              in.r.name,
              ~{
                num<-as.numeric(input[[.]])
                ifelse(num > 0,num, NA_real_)
              }
            )
        })
        
        sales<-reactive({
          map_dbl(
            in.s.name,
            ~{
              num<-as.numeric(input[[.]])
              ifelse(num > 0,num,NA_real_)
            }
          )
        })
        
        
        output$table.down<-downloadHandler(
            filename=function(){
                paste("Metrics",".csv",sep="")
            },
            content=function(file){
                write.csv(disease,file)
            }
        )
        
        output$flat<-downloadHandler(
            filename=function(){
                paste("Freedom",".csv",sep="")
            },
            content=function(file){
                write.csv(freedom,file)
            }
        )
        
        disease<-read.csv("metrics.csv",header=TRUE,sep=",")
        
        disease.df<-data.frame(disease)
        
        pie.data<-reactive({head(disease$Degree,n=8)})
        
        bar.data<-reactive({head(freedom$Outdegree,n=6)})
        
        freedom<-read.csv("Freedom.csv",header=TRUE,sep=",")
        
        freedom.df<-data.frame(freedom)
        
        data.X<-reactive({switch(input$select,
                                 "Indegree"=disease$Indegree,
                                 "Outdegree"=disease$Outdegree,
                                 "Degree"=disease$Degree,
                                 "Normalized"=disease$Normalized,
                                 "Domain"=disease$Domain,
                                 "Proximity"=disease$Proximity,
                                 "Modularity"=disease$Modularity,
                                 "Clusters"=disease$Clusters
        )})
        
        data.Y<-reactive({switch(input$choice,
                                 "Indegree"=disease$Indegree,
                                 "Outdegree"=disease$Outdegree,
                                 "Degree"=disease$Degree,
                                 "Normalized"=disease$Normalized,
                                 "Domain"=disease$Domain,
                                 "Proximity"=disease$Proximity,
                                 "Modularity"=disease$Modularity,
                                 "Clusters"=disease$Clusters
        )})
        
        
        output$metrics.table<-renderReactable({
            
            reactable(disease.df,
                      searchable=TRUE,
                      outlined=TRUE,
                      highlight=TRUE,
                      bordered=TRUE,
                      striped=TRUE,
                      compact=TRUE,
                      defaultColDef=colDef(
                          align="center",
                          headerStyle=list(background="#5dade2")
                      ))
        })
        
        output$freedom.table<-renderReactable({
            
            reactable(freedom.df,
                      compact=TRUE,
                      striped=TRUE,
                      searchable=TRUE,
                      bordered=TRUE,
                      highlight=TRUE,
                      defaultColDef=colDef(
                          align="center",
                          headerStyle=list(background="#5dade2")
                      ))
        })
        #This is the function for calculating profitability ratio
        profitability<-function(profit,turnover){
            profitability=round((profit/turnover)*100,1)
            return(profitability)
        }
        profitability.ratio<-reactive({profitability(profit(),turnover())})
        
        #This is the function for calculating efficiency ratio
        efficiency<-function(receivables,sales){
            efficiency<-round((receivables/sales)*365,1)
            return(efficiency)
        }
        
        efficiency.reactive<-reactive({
            efficiency(receivables(),sales())
        })
        
        Analyze<-reactive({
            round(analyzeSentiment(
                replace_symbol(
                    replace_number(
                        replace_ordinal(
                            doc())))),1)})
        
        QDAP<-reactive({Analyze()$SentimentQDAP})
        LoughranM<-reactive({Analyze()$SentimentLM})
        HarvardIV<-reactive({Analyze()$SentimentGI})
        
        word.count<-reactive({countWords(doc(),removeStopwords=FALSE)})
        
        tables<-reactive({
            data.frame(QDAP(),LoughranM(),HarvardIV())
        })
        
        data<-reactive({switch(input$choice,
                               "QDAP"=tables()$QDAP,
                               "LoughranM"=tables()$LoughranM,
                               "HarvardIV"=tables()$HarvardIV)})
        
        
        output$download<-downloadHandler(
            filename=function(){
                paste("table",".csv",sep="")
            },
            content=function(file){
                write.csv(tables(),file)
            }
        )
        
        output$drop<-downloadHandler(
            filename=function(){
                paste("Barplot",".jpg",sep="")
            },
            content=function(file){
                jpeg(file)
               print(ggplot(profitability.df())+aes(x=Years,y=Profitability,fill=Years)+
                       geom_bar(stat="identity")+scale_fill_brewer(palette = "Set1")+labs(x="Years",y="Margins (%)",fill="Years",title="Profitability Ratios Over Five Years")+
                       theme(axis.title = element_text(face="bold",size=12),
                             plot.title = element_text(face="bold",size=15)))
                dev.off()
            }
        )
        
        output$fall<-downloadHandler(
            filename=function(){
                paste("Piechart",".jpg",sep="")
            },
            
            content=function(file){
                jpeg(file)
                print(ggplot(profitability.df())+aes(x=2,y=Profitability,fill=Years)+geom_bar(stat = "identity",width=1)+
                        coord_polar("y",start = 0)+scale_fill_brewer(palette = "Set1")+
                        labs(x=NULL,y=NULL,fill="Years",title = "Profitability Ratios Over Five Years")+
                        theme(axis.line = element_blank(),
                              axis.text = element_blank(),
                              legend.position = "bottom",
                              axis.ticks = element_blank(),
                              plot.title = element_text(face="bold",size=15))+geom_text(aes(label=Profitability),position = position_stack(vjust=0.5))+
                        xlim(.2,2.5))
                dev.off()
            }
        )
        
        output$down<-downloadHandler(
            filename=function(){
                paste("Piechart",".jpg",sep="")
            },
            
            content=function(file){
                jpeg(file)
                print(ggplot(efficiency.df())+aes(x=2,y=Efficiency,fill=Years)+geom_bar(stat = "identity",width=1)+
                        coord_polar("y",start = 0)+scale_fill_manual(values = c("#5dade2","#85c1e9","#3498db","#2e86c1","#2874a6"))+
                        labs(x=NULL,y=NULL,fill="Years",title = "Receivables Turnover Ratios")+
                        theme(axis.line = element_blank(),
                              axis.text = element_blank(),
                              legend.position = "bottom",
                              axis.ticks = element_blank(),
                              plot.title = element_text(face="bold",size=15))+geom_text(aes(label=Efficiency),position = position_stack(vjust=0.5))+
                        xlim(.2,2.5))
                dev.off()
            }
        ) 
        
        output$plummet<-downloadHandler(
            filename=function(){
                paste("Barplot",".jpg",sep="")
            },
            content=function(file){
                jpeg(file)
                print(ggplot(efficiency.df())+aes(x=Years,y=Efficiency,fill=Years)+labs(x="Years",y="Days")+geom_bar(stat = "identity")+
                        scale_fill_manual(values = c("#5dade2","#85c1e9","#3498db","#2e86c1","#2874a6"))+labs(title = "Receivables Turnover Ratios")+
                        theme(axis.title = element_text(face="bold",size=12),
                              plot.title = element_text(face="bold",size=15)))
                dev.off()
            }
        )
        
        output$plot.down<-downloadHandler(
            filename=function(){
                paste("Graph",".jpg",sep="")
            },
            content=function(file){
                jpeg(file)
                color<-switch(input$color,
                              "Blue"="#5dade2",
                              "Red"="#e74c3c",
                              "Green"="#1abc9c",
                              "Yellow"="#f7dc6f",
                              "Purple"="#a569bd")
                barplot(data(),col=color,border="white",xlab="Texts",ylab="Sentiment Scores",main="Bar Plot of Sentiment Scores")
                dev.off()
            }
        )
        
        output$table<-renderReactable({
            input$run
            isolate(reactable(tables(),searchable=TRUE,bordered=TRUE,defaultColDef=colDef(
                align="center",
                headerStyle=list(background="#5dade2"),
                style=function(value){
                    if(value>0){color<-"#27ae60"}
                    else if(value<0){color<-"#e74c3c"}
                    else{color<-"#5dade2"}
                    list(color=color,fontWeight="bold")
                }),
                highlight=TRUE,outlined=TRUE,striped=TRUE,filterable=FALSE,compact=TRUE,onClick="expand")
            )
        })
        
        output$count<-renderReactable({
            input$run
            isolate(reactable(data.frame(word.count()),bordered=TRUE,striped=TRUE,compact=TRUE,
                              defaultColDef=colDef(
                                  align="center",
                                  headerStyle=list(background="#85c1e9"))
            ))
        })
        
        output$plot<-renderPlot({
            
            color<-switch(input$color,
                          "Blue"="#5dade2",
                          "Red"="#e74c3c",
                          "Green"="#1abc9c",
                          "Yellow"="#f7dc6f",
                          "Purple"="#a569bd")
            input$run 
            isolate(barplot(data(),col=color,border="white",xlab="Texts",ylab="Sentiment Scores",main="Bar Plot of Sentiment Scores"))
            
        })
        output$graph<-renderPlot({
            
            input$run
            isolate(plotSentiment(data(),xlab="Texts",ylab="Sentiment Scores"))
        })
        
        Years1<-reactive({
          years.in.p<-map2_chr(
            .x = in.p.name,
            .y = seq_along(in.p.name),
            ~ifelse(as.numeric(input[[.x]])>0,paste0("Year",.y),NA_character_)
          ) %>% na.omit()
          years.in.t<-map2_chr(
            .x = in.t.name,
            .y = seq_along(in.t.name),
            ~ifelse(as.numeric(input[[.x]])>0,paste0("Year",.y),NA_character_)
          ) %>% na.omit()
          commonyears<-intersect(years.in.p,years.in.t)
          return(commonyears)
        })
        
        profitability.df<-reactive({
          Profitability<-req(profitability.ratio()) %>% na.omit()
          Years<-req(Years1())
          
          data.frame(Profitability,Years)
          })
        
        output$bar<-renderPlotly({
            
            input$go
            isolate(ggplot(profitability.df())+aes(x=Years,y=Profitability,fill=Years)+
                      geom_bar(stat="identity")+scale_fill_brewer(palette = "Set1")+labs(x="Years",y="Margins (%)",fill="Years",title="Profitability Ratios Over Five Years")+
                      theme(axis.title = element_text(face="bold",size=12),
                            plot.title = element_text(face="bold",size=15)))
                      
            
            
        })
        
        output$pie<-renderPlot({
            
            input$go
            isolate(ggplot(profitability.df())+aes(x=2,y=Profitability,fill=Years)+geom_bar(stat = "identity",width=1)+
                      coord_polar("y",start = 0)+scale_fill_brewer(palette = "Set1")+
                      labs(x=NULL,y=NULL,fill="Years",title = "Profitability Ratios Over Five Years")+
                      theme(axis.line = element_blank(),
                            axis.text = element_blank(),
                            legend.position = "bottom",
                            axis.ticks = element_blank(),
                            plot.title = element_text(face="bold",size=15,hjust = 0.5))+geom_text(aes(label=paste(Profitability,"%",sep="")),position = position_stack(vjust=0.5))+
                      xlim(.2,2.5)
                            
                    )
            
        })
        
        Years2<-reactive({
          years.in.r<-map2_chr(
            .x = in.r.name,
            .y = seq_along(in.r.name),
            ~ifelse(as.numeric(input[[.x]])>0,paste0("Year",.y),NA_character_)
          ) %>% na.omit()
          years.in.s<-map2_chr(
            .x = in.s.name,
            .y = seq_along(in.s.name),
            ~ifelse(as.numeric(input[[.x]])>0,paste0("Year",.y),NA_character_)
          ) %>% na.omit()
          commonyears<-intersect(years.in.r,years.in.s)
          return(commonyears)
        })
        
        efficiency.df<-reactive({
          Efficiency<-req(efficiency.reactive()) %>% na.omit()
          Years<-req(Years2())
          
          data.frame(Efficiency,Years)
        })
        output$sphere<-renderPlot({
            
            input$away
            isolate(
              ggplot(efficiency.df())+aes(x=2,y=Efficiency,fill=Years)+geom_bar(stat = "identity",width=1)+
                coord_polar("y",start = 0)+scale_fill_manual(values = c("#5dade2","#85c1e9","#3498db","#2e86c1","#2874a6"))+
                labs(x=NULL,y=NULL,fill="Years",title = "Receivables Turnover Ratios")+
                theme(axis.line = element_blank(),
                      axis.text = element_blank(),
                      legend.position = "bottom",
                      axis.ticks = element_blank(),
                      plot.title = element_text(face="bold",size=15))+geom_text(aes(label=paste(Efficiency,"days",sep="")),position = position_stack(vjust=0.5))+
                xlim(.2,2.5)
                )
        })
        
        output$bin<-renderPlotly({
            input$away
            isolate(
                ggplot(efficiency.df())+aes(x=Years,y=Efficiency,fill=Years)+labs(x="Years",y="Days")+geom_bar(stat = "identity")+
                  scale_fill_manual(values = c("#5dade2","#85c1e9","#3498db","#2e86c1","#2874a6"))+labs(title = "Receivables Turnover Ratios")+
                  theme(axis.title = element_text(face="bold",size=12),
                        plot.title = element_text(face="bold",size=15))
                )
        })
        
        output$draw<-renderPlot({
            
            hist(data.X(),border="white",col="#3498db",main="Variable Plots",xlab="Metrics")
        })
        
        output$art<-renderPlot({
            
            plot(data.X(),disease$Modularity,col="#e74c3c",xlab="Metrics",ylab="Value",pch=20,
                 cex=2,type="h",main="Variable Plots")
        })
        
        output$paint<-renderPlot({
            
            pie(pie.data(),radius=1.1,clockwise=TRUE,init.angle=input$slide,
                border="white",col=c("#5dade2","#ec7063","#48c9b0","#a569bd","#f4d03f"),main="Pie Chart of Degrees")
        })
        
        output$part<-renderPlot({
            
            barplot(bar.data(),border="white",main="Bar Plot of Outdegrees",
                    xlab="Outdegrees",ylab="Values",col=c("#5dade2","#ec7063","#48c9b0","#a569bd","#f4d03f"))
        })
        
            
        
        
            
                corp<-reactive({Corpus(VectorSource(doc()))})
                corpA<-reactive({tm_map(corp(),removeNumbers)})
                corpB<-reactive({tm_map(corpA(),removePunctuation)})
                corpC<-reactive({tm_map(corpB(),stripWhitespace)})
                corpD<-reactive({tm_map(corpC(),tolower)})
                corpE<-reactive({tm_map(corpD(),removeWords,stopwords("english"))})
                tms<-reactive({TermDocumentMatrix(corpE())})
                a<-reactive({as.matrix(tms())})
                v<-reactive({sort(rowSums(a()),decreasing=TRUE)})
                d<-reactive({data.frame(Words=names(v()),Frequency=as.numeric(v()))})
                
                
                
           cloudShape<-reactive({switch(input$shapes,
                              "Circle"="circle",
                              "Cardoid"="cardoid",
                              "Diamond"="diamond",
                              "Triangle"="triangle",
                              "Pentagon"="pentagon",
                              "Star"="star")})
           

            
        
        freq.table<-reactive({data.frame(head(d(),n=input$tabslide))})
        
        text.cloud<-reactive({word.cloud(doc())})
        
        
      output$cloud<-renderWordcloud2({
          
          wordcloud2(d(),size=input$cloudslide,shape=cloudShape())
      })
      
      
      
     output$cloudable<-renderReactable({
       
       
      reactable(freq.table(),striped=TRUE,bordered=TRUE,
                 highlight=TRUE,defaultColDef=colDef(
                   headerStyle=list(background="#5dade2"),
                   align="center"
                 ))
     })
     
     output$freqdown<-downloadHandler(
       
       filename=function(){
         paste("WordFrequency",".csv",sep="")
       },
       content=function(file){
         write.csv(freq.table(),file)
       }
     )
    }


shinyApp(ui=ui,server=server)  