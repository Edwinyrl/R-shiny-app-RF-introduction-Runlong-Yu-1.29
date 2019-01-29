

library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(caret)
library(randomForest)
library(party)
################################################################################

################################################################################################
################################################################################################
#Rshiny

#Servershiny
server <- shinyServer(function(input, output, session) {
#From xiaowen  
  dsnames <- c()
  data_set <- reactive({
    req(input$file1)
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL) }
    
    if (input$fileType_Input == "1") {
      data_set<-read.csv(inFile$datapath,
                         header = TRUE,
                         stringsAsFactors = TRUE)
    } else {
      data_set<-read_excel(inFile$datapath)
    }
  })
  
  
  observe({
    req(input$file1)
    dsnames <- names(data_set())
    # cb_options <- list()
    # cb_options[dsnames] <- dsnames
    updateCheckboxGroupInput(session, "inCheckboxGroup",
                             label = "Select explanatory variables",
                             choices = dsnames,  
                             selected = "")
    updateSelectInput(session, "selectInput",
                      label = "Select a response variable",
                      choices = dsnames,  
                      selected = "")
  })
  
  
  output$contents<- renderDataTable({
    #Add req function to check whether the input is truthy, which refrains error message when not selecting variables
    req(input$inCheckboxGroup)
    req(input$selectInput)
    selected_data <- data_set()[,c(input$selectInput,input$inCheckboxGroup)]
    datatable(data = selected_data, 
              options = list(pageLength = 10), 
              rownames = FALSE)
  })
  
  
  output$choose_columns <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()
  })

  
  
 

  
  
 
  output$rf<-renderPlot({
    req(input$inCheckboxGroup)
    req(input$selectInput)
    set.seed(17)
    forest=randomForest(x =data_set()[,c(input$inCheckboxGroup)], y =as.factor(data_set()[,c(input$selectInput)]), ntree =input$ntree,nodesize =input$node,mtry=input$mtry) 

    plot(forest)
    output$rfoutcome <- renderPrint({
      print(forest, digits = 3, signif.stars = FALSE)
    })
  })
  output$error1<-renderText({
    
    print('The green curve represents the error in predicting the passenger to be alive.')
  })
  output$error2<-renderText({
    
    print('The red curve represents the error in predicting the passenger to be dead.')
  })
  output$error3<-renderText({
    
    print('The black curve represents the out of bag error.')
  })
  output$error4<-renderText({
    
    print('More details of the model are listed below:')
  })
  
  
  output$treediagram<-renderPlot({
    
    req(input$inCheckboxGroup)
    req(input$selectInput)
    train_index=sample(nrow(data_set()),0.7*nrow(data_set()),FALSE)
    f=as.formula(paste(input$selectInput,"~",paste(input$inCheckboxGroup,collapse="+")))
    dt<-ctree(f,data=data_set()[train_index,],controls=ctree_control(maxdepth=input$max_depth,mincriterion=input$criterion,minbucket =input$node2,mtry=0))
    plot(dt,type='simple')
    prediction=unlist(predict(dt,data_set()[-train_index,input$inCheckboxGroup]))>0.5
    outcome=table(prediction,data_set()[-train_index,input$selectInput])
    output$tab<-renderPrint({print(outcome)})
    a=outcome[1,1]
    b=outcome[1,2]
    c=outcome[2,1]
    d=outcome[2,2]
    
    output$metric<-renderPrint({cat(paste(' Definition of some metrics:',"\n","\n" ,'Sensitivity:true positive/(true positive+false positive)',"\n",'Specificity:true negative/(true negative+false negative)',"\n",'Precision:true positive/(true positive+false negative)'))})
    output$accuracy<-renderText({print((a+d)/(a+b+c+d))})
    output$sen<-renderText({print((a)/(a+c))})
    output$spe<-renderText({print((d)/(b+d))})
    output$pre<-renderText({print((a)/(a+b))})
    
  })
  
  
  output$ens_text1<-renderText({print('- Consider a situation where the jury is making a decision via majority voting')})
  output$ens_text2<-renderText({print('- N is the total number of jurors')})
  output$ens_text3<-renderText({print('- p is the possibility that each single juror make the correct choice(for simplification, assume constant over all jurors)')})
  output$ens_text4<-renderText({print('- μ is the chance that the whole jury makes a correct decision')})
  output$ens_text5<-renderText({print('- In this setting, if p>0.5(juror tends to make correct decision), adding more juror into the jury should let μ approach 1.')})
  output$ens_text6<-renderText({print('- It’s an analogy to combining sensible algorithms to improve predictive performance.')})
  
}  )

#Body
body <- dashboardBody(

#Modified based on work of Xiaowen
  tabItems(
    tabItem(tabName = "data",
            # header
            fluidRow(box(width=6,h4("Import your dataset(Please wrangle your dataset first)"), 
                         #file Input
                         fileInput("file1","choose excel file"),
                         radioButtons("fileType_Input",
                                      label = h4("Choose File type"),
                                      choices = list(".csv/txt" = 1, ".xlsx" = 2),
                                      selected = 1,
                                      inline = TRUE),
                         selectInput("selectInput",
                                     "Checkbox group input for response variable:",
                                     c("label 1" = "option1",
                                       "label 2" = "option2")),
                         checkboxGroupInput("inCheckboxGroup",
                                            "Checkbox group input for explanatory variable:",
                                            c("label 1" = "option1",
                                              "label 2" = "option2")),
                         uiOutput("choose_columns")),
                     box(width=6,tabsetPanel(type="tabs",
                                     tabPanel("data",dataTableOutput("contents") )))),
            fluidRow(box(width=12,tabsetPanel(type="tabs",
                                              tabPanel("Decision Tree",h2("Explore decision tree!"),
                                                       fluidRow(box(width=7,
                                                         numericInput(inputId = 'max_depth', 
                                                                      label = 'Maximum number of levels in the tree', 
                                                                      min =1, max =6, 
                                                                      value =3,step=1),
                                                         numericInput(inputId = 'node2', 
                                                                      label = 'Minimum number of samples in node', 
                                                                      min =0, max =300, 
                                                                      value =10,step=10),
                                                         sliderInput(inputId = 'criterion', 
                                                                     label = 'criterion(1 - p-value) for implementing a split.', 
                                                                     min =0, max =1, 
                                                                     value =0.95))
                                                       ),
                                                       fluidRow(
                                                         
                                                         column(width=6,
                                                                box(width=NULL,plotOutput(outputId = 'treediagram')),verbatimTextOutput('metric')),
                                                         column(width=6,
                                                                box(width=NULL,verbatimTextOutput('tab'),
                                                                    infoBox(width=NULL,title='Accuracy',value=textOutput('accuracy')),
                                                                    infoBox(width=NULL,title='Sensitivity/Recall',value=textOutput('sen')),
                                                                    infoBox(width=NULL,title='Specificity',value=textOutput('spe')),
                                                                    infoBox(width=NULL,title='Precision',value=textOutput('pre')))
                                                                
                                                         ))),
                                              tabPanel("Random Forest",h2("Welcome to the forest!"), 
                                                        fluidRow(
                                                          box(width=7,sliderInput(inputId = 'ntree', 
                                                                          label = 'number of trees', 
                                                                          min =1, max =1000, 
                                                                          value =100),
                                                              sliderInput(inputId = 'mtry', 
                                                                          label = 'Number of variables considered in choosing each split', 
                                                                          min =1, max =6, 
                                                                          value =3),
                                                              sliderInput(inputId = 'node', 
                                                                          label = 'Minimum number of samples in node', 
                                                                          min =1, max =100, 
                                                                          value =10))
                                                        )                                  ,
                                                                               plotOutput(outputId = 'rf'),
                                                       fluidRow(box(width=12,textOutput(outputId ='error1' ),textOutput(outputId ='error2' ),
                                                                    textOutput(outputId ='error3' ),textOutput(outputId ='error4' )))
                                                                               ,
                                                                               verbatimTextOutput(outputId ='rfoutcome')
                                              ))))
            ),

                   
    tabItem(tabName = "concept",solidHeader = TRUE,
            h2("Key Knowledge"),
            fluidRow(
              tabBox(
                title = "Concepts to mention",width=12,
                id = "concept", height = "420px",
                tabPanel("Ensemble", box(width=12,"This is an idea shared by both statistics(statistical 
                                         ensemble) and machine learning (ensemble learning). In ML, 
                                         it means 'use multiple learning algorithms to obtain better predictive
                                         performance than could be obtained from any of the constituent 
                                         learning algorithms alone'(Wikipedia). Concretely, for random forest, it combines
                                         decision tree classifiers to get better result. To give you a sense of what it means,
                                         look at the example below."),
                         box(title='Example of ensemble: Condorcet’s Jury Theorem',width=12,solidHeader = TRUE,
                             status='success',fluidRow(column(width=4,tags$img(src='Ensemble.png', 
                                                                               height='140px',width='300px')),
                                                       column(width=8,textOutput('ens_text1'),
                                                              textOutput('ens_text2'),
                                                              textOutput('ens_text3'),
                                                              textOutput('ens_text4'),
                                                              textOutput('ens_text5'),
                                                              textOutput('ens_text6')))
                         ))
                ,
                tabPanel("Decision Tree", box(width=12,"We only discuss about ‘classification tree’ here, 
                                              which is about using a tree-like predictive model to go from observations 
                                              about an item to conclusions about item’s target value(wikipedia). To 
                                              begin with, it is good to think of “20 Questions” game. The strategy 
                                              of guesser is to ask the questions that narrow down the most of the options.
                                              The same intuition applies to ‘decision tree’. Searching for splits in decision 
                                              tree is analogous to proposing the questions, but we have certain criterions 
                                              for the search. Here we have a more concrete example using ‘information gain’
                                              as the criterion(of course there are other metrics to choose from). This 
                                              is based on the concept of information entropy, which is the measure of 
                                              degree of chaos in the system. ‘Information gain’ captures the change 
                                              in entropy after splitting."), 
                         fluidRow(column(width=6,tags$img(src='DT1.png',height='220px',width='520px')),column(width=6,tags$img(src='DT2.png',height='220px',width='480px')))),
                
                tabPanel("Bootstrapping",box(width=12, "In statistics, bootstrapping is any test or metric that relies on random sampling 
with replacement. This allows estimation of the sampling distribution of almost any statistic(wikipedia). Suppose that we 
are drawing balls of different colours from a bag once at a time. The selected ball is returned to the bag so that each selection 
has equal probability of drawing certain colour. More generally, for a sample X of size N, we draw N elements from it with 
replacement and form a new sample(bootstrap sample) of the same size. We can repeat this process and compute statistics of 
original distribution using a large number of bootstrap samples."),fluidRow(column(width=6,tags$img(src='Boot.png',height='280px',width='520px')))),
                
                tabPanel("Bagging", box(width=12,"Let’s combine what we have just learnt together. Suppose that we have generated 
                                        m bootstrap samples: X1,X2,…,Xm. We would like to train a decision tree classifier on each 
                                        bootstrap sample and average all the individual classifiers to build our final classifier. 
                                        This is called Bagging. However, though bagging is usually used with decision trees, it 
                                        works with other methods as well. This technique improves stability and accuracy of algorithms
                                        —— it can be proved that mean squared error is reduced by factor of m. Plus, bagging reduces
                                        variance and prevents overfitting. More concretely, it decreases the error generated when 
                                        training on different datasets. Intuitively, errors of models trained on different data are
                                        cancelled out with each other."), 
                         fluidRow(column(width=6,tags$img(src='bagging.png',height='280px',width='520px'))))
              )
            )
            ),
    tabItem(tabName = "sketch",solidHeader = TRUE,
            h2("Simple sketch of Random Forest"),
            fluidRow(
              infoBox(width=12,title='Random Forest',value='Consider building a random forest consisting of N trees(for k=1,2,..,N)',icon=icon('bars'),fill=TRUE,color="purple")),
            fluidRow(
              infoBox(width=6,title='Step One',value='Generate bootstrap sample Xk',icon=icon('list-ol'),color='teal'),
              infoBox(width=6,title='Step Two',value='Build a decision tree based on that sample',icon=icon('list-ol'),color='teal')
            ),  
              
              fluidRow(infoBox(width=12,title='Step Three',value='From all the features(e.g. T variables) prepared to train the (entire) random forest model, randomly select M of them to feed into this decision tree',icon=icon('list-ol'),color='teal')),
              fluidRow(infoBox(width=12,title='Step Four',value='Select the first splitting, searching over all randomly chosen features, choosing the one maximizing information gain (e.g. entropy/gini method, more details to be added)',icon=icon('list-ol'),color='teal')),
            fluidRow(infoBox(width=12,title='Step Five',value='Repeat this process until the sample is exhausted under the restriction given(e.g. maximum level of tree)',icon=icon('list-ol'),color='teal')),
            fluidRow(infoBox(width=12,title='Step Six',value='Get N classifiers from N decision trees, and then average them to get the final classifier',icon=icon('list-ol'),color='teal'))
            )
            
    
))



#Side bar
sidebar<-dashboardSidebar(
  sidebarMenu(width=70,
              menuItem(text='Concepts',tabName='concept',icon=icon('lightbulb')),
              menuItem(text='Sketch of Random Forest',tabName='sketch',icon=icon('dashboard')),
              menuItem(text='Playground', tabName='data',icon=icon('table'))
              
                       
              
              
  )
)




ui <- dashboardPage(header = dashboardHeader(),
                    sidebar = sidebar,
                    body = body
)
shinyApp(ui, server)

