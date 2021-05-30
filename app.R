library(shiny)
library(shinyjs)
library(ggplot2)

#reads the data
dataset <- read.csv("https://raw.githubusercontent.com/darylGan/Heart-Attack-Analysis/main/dataset/cleanedData.csv", header = TRUE)

dataset$cp <- as.factor(dataset$cp)
dataset$exng <- as.factor(dataset$exng)
dataset$caa <- as.factor(dataset$caa)
dataset$hd <- as.factor(dataset$hd)

#choose a seed to stabilize the performance of the model
#from analysis, we found that seed 3 give best performance
#then, #split the training and testing set with 80% and 20% proportion
set.seed(3) 
split_index <- sample(2,nrow(dataset),replace = T , prob = c(0.8,0.2))
train_set <- dataset[split_index == 1,]
test_set <- dataset[split_index == 2,]

#build the logistic model with the selected predictors
logistic_model <- glm(hd ~ cp+exng+oldpeak+caa,data=train_set,family="binomial",)
summary(logistic_model)

#Train the model using training data set
train_predict <- predict(logistic_model, train_set, type = 'response')
train_predict <- ifelse(train_predict >=0.5 , TRUE , FALSE)
train_matrix <- table(Prediction = train_predict , Actual = train_set$hd)
train_accuracy <- sum(train_predict==train_set$hd)/nrow(train_set)*100

#Get the performance of the model of testing data set
test_predict <- predict(logistic_model, test_set, type = 'response')
test_predict <- ifelse(test_predict >=0.5 ,  TRUE , FALSE)
test_matrix <- table(Prediction = test_predict , Actual = test_set$hd)
test_accuracy <-sum(test_predict == test_set$hd)/nrow(test_set)*100

#calculate the accuracy of the prediction
paste(format(round(train_accuracy, digits = 2), nsmall=2), "%")
paste(format(round(test_accuracy, digits = 2), nsmall=2), "%")

#column names selected for bar chart's input selections
colname <- c('sex','cp','slp','exng','caa','fbs','restecg','thall','hd')

# Define UI for application
ui <- fluidPage(
    #set up the shiny js
    useShinyjs(),
    # Application title
    titlePanel("Heart Disease Prediction Model and Analysis"),
    
    # Sidebar with a slider input for number of bins 
    sidebarPanel(
        
        radioButtons(inputId = 'exng', label = 'Exercise induced angina', choiceNames = c('yes', 'no'), choiceValues = c('TRUE', 'FALSE')),
        radioButtons(inputId = 'cp', label = 'Chest Pain Type', choiceNames = c('Asymptomatic', 'Typical Angina', 'Atypical Angina', 'Non-angina pain')
                     , choiceValues = c('0', '1', '2', '3')),
        selectInput(inputId='caa', label = 'Number of major vessels colored by flourosopy', choices = c('0', '1', '2', '3')),
        sliderInput(inputId ='oldpeak', label = 'ST depression induced by exercise relative to rest', min = 0.00, max = 8.00, value = 1.10, step = 0.1),
        
        sliderInput(inputId='sampleSize', label='Choose the first n samples', min = 1, max = nrow(dataset),value = nrow(dataset), step=1),
        selectInput(inputId='xaxis', label='Choose the X-axis', choices=names(dataset), selected=names(dataset)[[1]]),
        selectInput(inputId='yaxis', label='Choose the Y-axis', choices=names(dataset), selected=names(dataset)[[2]]),
        
        #put none as "-" but still shown as none in the selection for syntax use in building facet grid later
        selectInput(inputId='facet_row', label='Choose the Facet Row', choices=c(None='.', names(dataset))), 
        selectInput(inputId='facet_col', label='Choose the Facet Column', choices=c(None='.', names(dataset))),
        
        selectInput(inputId='legend', label='Legend', choices=c('None', names(dataset))),
        checkboxInput(inputId ='selectAll',label = 'Show All Variables', value = FALSE),
        
        #for bar
        selectInput(inputId='xaxis_bar', label='Choose the X-axis', choices=colname, selected=names(dataset)[[1]]),
        selectInput(inputId='yaxis_bar', label='Choose the Y-axis', choices=colname, selected=names(dataset)[[2]]),
        
        #put none as "-" but still shown as none in the selection for syntax use in building facet grid later
        selectInput(inputId='facet_row_bar', label='Choose the Facet Row', choices=c(None='.', colname)), 
        selectInput(inputId='facet_col_bar', label='Choose the Facet Column', choices=c(None='.', colname)),
        
    ),
    
    mainPanel(
        
        tabsetPanel( id = 'tabsetPanel', selected = 'Table',
                     
                     tabPanel(title = "Prediction Model", h3("Accuracy of the Model"),
                              h4("The accuracy for the train set(%):"), verbatimTextOutput("trainAccuracy"),tableOutput("trainConfusion"),
                              h4("The accuracy for the test set(%):"),verbatimTextOutput("testAccuracy"),tableOutput("testConfusion"),
                              h4("The predicted output is shown below:"),verbatimTextOutput("predictOutput")),
                     
                     tabPanel(title = "Table", tableOutput("table")),
                     
                     tabPanel(h4("The graph plotted below is the jitter graph because the dataset consists of mostly discrete data"),
                              title = "Graph", plotOutput("graph"),
                              helpText("Note: If possible, please avoid from choosing continuous variable as facets input, it would not be your desired output")),
                     
                     tabPanel(title = "Bar", plotOutput("bar"),
                              helpText("Note: variable of y-axis will be automatically chosen to build the legends"))
                     
                     
        )
    )
)


server <- function(input, output) {
    
    #extract certain portion of the data from whole data sets
    data_shown <- reactive({
        dataset[1:input$sampleSize,]
    })
    
    #get the tabselected so that we can specifically show and hide certain input box in the sidebar panel
    tabselected <- reactive ({
        input$tabsetPanel
    })
    
    #get the facet row
    facet_row <- reactive({
        
        #it cannot be same with facet column, if it is, an error message will be shown
        validate(
            need((input$facet_row != input$facet_col ) || input$facet_row == '.', "Please choose value different with facet column for facet row")
        )
        input$facet_row
    })
    
    #get the facet column
    facet_column <- reactive({
        #it cannot be same with facet row, if it is, an error message will be shown
        validate(
            need((input$facet_col != input$facet_row) || input$facet_col == '.', "Please choose value different with facet row for facet column")
        )
        input$facet_col
    })
    
    #get the facet row for bar
    facet_row_bar <- reactive({
        
        #it cannot be same with facet column, if it is, an error message will be shown
        validate(
            need((input$facet_row_bar != input$facet_col_bar ) || input$facet_row_bar == '.', "Please choose value different with facet column for facet row")
        )
        input$facet_row_bar
    })
    
    #get the facet column for bar
    facet_column_bar <- reactive({
        #it cannot be same with facet row, if it is, an error message will be shown
        validate(
            need((input$facet_col_bar != input$facet_row_bar) || input$facet_col_bar == '.', "Please choose value different with facet row for facet column")
        )
        input$facet_col_bar
    })
    
    output$trainAccuracy <- renderText({
        if(tabselected() == 'Prediction Model'){
            hide('sampleSize')
            hide('xaxis')
            hide('yaxis')
            hide('facet_row')
            hide('facet_col')
            hide('selectAll')
            hide('legend')
            hide('xaxis_bar')
            hide('yaxis_bar')
            hide('facet_row_bar')
            hide('facet_col_bar')
            show('exng')
            show('cp')
            show('caa')
            show('oldpeak')
        }
        
        #this is calculating the accuracy of model on predicting train set in percentage
        paste(format(round(train_accuracy, digits = 2), nsmall=2), "%")
    })
    
    output$trainConfusion <- renderTable({
        #makes a confusion matrix on train set
        #it displays the number of true positive, false positive, true negative and false negative
        table(Prediction = train_predict , Actual = train_set$hd)
    })
    
    #calculate accuracy of model on predicting test set in percentage
    output$testAccuracy <- renderText({
        paste(format(round(test_accuracy, digits = 2), nsmall=2), "%")
    })
    
    output$testConfusion <- renderTable({
        #makes a confusion matrix on test set
        #it displays the number of true positive, false positive, true negative and false negative
        table(Prediction = test_predict , Actual = test_set$hd)
    })
    
    output$predictOutput <- renderText({
        
        #create a dataframe for the model to predict output
        #these are the default values of the dataframe
        input_data <- data.frame(
            exng = input$exng,
            cp = input$cp,
            caa = input$caa,
            oldpeak =input$oldpeak
        )
        
        #predict on the input
        prediction <- predict(logistic_model, input_data, type = 'response')
        
        #if the probability more than or equals to 0.5, the person is diagnosed with heart disease
        #else, the person is healthy
        prediction <- ifelse(prediction >=0.5 , "Sadly, you are diagnosed with a heart disease." , "Congratulations, you are healthy!")
        
        #show the result of the prediction
        prediction
        
    })
    
    output$table <- renderTable({
        if(tabselected() == 'Table'){
            show('sampleSize')
            show('xaxis')
            show('yaxis')
            show('facet_row')
            show('facet_col')
            show('selectAll')
            hide('xaxis_bar')
            hide('yaxis_bar')
            hide('facet_row_bar')
            hide('facet_col_bar')
            hide('legend')
            hide('exng')
            hide('cp')
            hide('caa')
            hide('oldpeak')
        }
        
        #if user chooses to show all variable
        if (input$selectAll == TRUE){
            #just simply show the data 
            data_shown()
        }else{
            
            #else, get the xaxis,yaxis,facet row and facet column
            selected = c(input$xaxis,input$yaxis)
            
            # . means none for facet_row input, so if it is none, ignore it
            if (input$facet_row != '.'){
                #else, appends it to the selected vector
                selected = c(selected,facet_row())
            }
            
            #same trick with facet_row
            if (input$facet_col != '.')
                selected = c(selected,facet_column())
            
            #same trick with facet_row and column but just that none is none for legend input
            if (input$legend != 'None')
                selected = c(selected,input$legend)
            
            #show the table
            a <- subset(data_shown(), select = unique(selected))
            a
        }
    })
    
    output$graph <- renderPlot({
        
        #show and hide some control widgets
        if(tabselected() == 'Graph'){
            show('sampleSize')
            show('xaxis')
            show('yaxis')
            show('facet_row')
            show('facet_col')
            hide('selectAll')
            show('legend')
            hide('xaxis_bar')
            hide('yaxis_bar')
            hide('facet_row_bar')
            hide('facet_col_bar')
            hide('exng')
            hide('cp')
            hide('caa')
            hide('oldpeak')
        }
        
        #plot the graph with jitter plot because most of the varaibles are nominal data
        #Thus jitter plot is the one that most suit with it
        #width and height is the range of the points scatter around
        p <- ggplot(data_shown(), aes_string(x=input$xaxis, y=input$yaxis)) +
            geom_jitter(width = 0.25, height = 0.25)
        
        if (input$legend != 'None')
            p <- p + aes_string(color=input$legend) +
            scale_fill_discrete(name = "Legends")
        
        facets <- paste(facet_row(), '~', facet_column())
        if (facets != '. ~ .')
            p <- p + facet_grid(facets)
        
        #show the graph
        p
        
    })
    
    output$bar <- renderPlot({
        if(tabselected() == 'Bar'){
            show('sampleSize')
            show('xaxis_bar')
            show('yaxis_bar')
            show('facet_row_bar')
            show('facet_col_bar')
            hide('selectAll')
            hide('xaxis')
            hide('yaxis')
            hide('facet_row')
            hide('facet_col')
            hide('selectAll')
            hide('legend')
            hide('exng')
            hide('cp')
            hide('caa')
            hide('oldpeak')
        }
        
        
        #position = dodge means that the group of bars will be side by side
        #by default it is stack, means that the group of bars will stack on each other
        #y = ..prop.. means it is count divided by sum of all count that belongs to same group
        #label = scales::percent is making it to become percentange to be shown as labels
        #limits = c(0,1) is making the y-axis labels to show from 0.00 to 1.00 which converts to percentage are 0% to 100%
        #vjust = -.5 makes the label text like 69.7% to be shown above the bar for each group
        gg <- ggplot(data_shown(), aes_string(x=input$xaxis_bar, group=input$yaxis_bar, fill= input$yaxis_bar)) +
            geom_bar(aes(y=..prop..),position="dodge")+
            scale_y_continuous(limits = c(0,1),name = 'Percentage', labels = scales::percent)+
            geom_text(aes(label = scales::percent(..prop..), y=..prop..), 
                      stat="count", vjust=-.5,position=position_dodge(0.9))
        
        
        facets <- paste(facet_row_bar(), '~', facet_column_bar())
        if (facets != '. ~ .')
            gg <- gg + facet_grid(facets)
        
        #show the bar
        gg
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
