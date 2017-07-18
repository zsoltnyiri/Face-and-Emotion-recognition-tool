# progress <- shiny::Progress$new(session, min = 1, max=15)
# progress$set(message = 'Making plots', value = 1)
# progress$set(value = 15)
# on.exit(progress$close())


library(shiny)
library(httr)
library(XML)
library(stringr)
library(ggplot2)
library(Kmisc)
library(jpeg)
library(Rmisc)
library(grid)
library(highcharter)
library(rCharts)
library(plotly)

options(scipen = 999)
options(shiny.trace = F)
options(shiny.reactlog = F)

ui = shinyUI(fluidPage(
  
  titlePanel("Face and Emotion recognition tool"),
  
  sidebarLayout(
    # sidebar elements
    sidebarPanel(
      textInput("url", "Paste an Image URL", value = 'http://thelala.com/wp-content/uploads/2016/03/rtr4yevj_0.jpg', placeholder ='http://thelala.com/wp-content/uploads/2016/03/rtr4yevj_0.jpg'),
      print("or"),
      br(),
      br(),
      fileInput(inputId = 'files', 
                label = 'Select an Image',
                multiple = F,
                accept=c('image/png', 'image/jpeg'))
    ),
    # main panel elements
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 fluidRow(
                   br(),
                   column(12, align = "center", h3(textOutput("age")))),
                 br(),
                 fluidRow(
                   column(6, plotOutput("pic")),
                  # column(6, plotOutput("hist")),
                   column(6, showOutput('hist2', 'highcharts')),
                   column(6, plotlyOutput("plotly"))
                 )),  
        tabPanel("Raw Data", tableOutput("data"))
      )
    )
  )
))

server = function(input, output, clientData, session) {

  progress <- shiny::Progress$new(session, min = 0, max = 4)
  progress$set(message = 'Making plots', value = 0)
  
  # Saves the uploaded item onto a selected place  
  # observeEvent(input$files, {
  #   inFile <- input$files
  #   if (is.null(inFile))
  #     return()
  #   file.copy(inFile$datapath, file.path('C:\\Users\\zsolt\\Documents\\', inFile$name) )
  # })
  
  emotion_data = reactive({
    url = input$url
    mybody = list(url = url)
    faceEMO = POST(
      url = emotion_url,
      content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = emotion_key)),
      body = mybody,
      encode = 'json'
    )
    # Request results
    emotion_results = httr::content(faceEMO)[[1]]
    # Transformation
    df_emotion_results = as.data.frame(as.matrix(emotion_results$scores))
    df_emotion_results$V1 = as.numeric(df_emotion_results$V1)*100
    colnames(df_emotion_results)[1] = "Level"
    df_emotion_results$Emotion = rownames(df_emotion_results)  
    df_emotion_results
  })
  
  face_data = reactive({
    url = input$url
    mybody = list(url = url)
    faceResponse = POST(
      url = face_url, 
      content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = face_key)),
      body = mybody,
      encode = 'json'
    )
    # Request results
    face_results = httr::content(faceResponse)[[1]]
    face_results
  })
  
  output$pic = renderPlot({
    progress$set(value = 1)
    url = input$url
    # Download the selected picture from the given url and save it
    download.file(url, 'host_pic.jpg', mode = 'wb')
    pic = readJPEG('host_pic.jpg', native = T)
    raster = rasterGrob(pic, interpolate  = T)
    progress$set(value = 2)
    # Plot the picture
    qplot(geom ='blank') +
      annotation_custom(raster, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      geom_point()
    # Remove the downloaded picture
    # file.remove('host_pic.jpg')
  })
  
  # output$hist = renderPlot({
  #   data = emotion_data()
  #   # Plot the predicted data
  #   ggplot(data = data, aes(x = Emotion, y = Level, fill = Emotion)) +
  #     geom_bar(stat = 'identity') + 
  #     ylab('%') +
  #     theme(plot.title = element_text(hjust = 0.5)) +
  #     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  # })
  
   output$hist2 = renderChart2({
    data = emotion_data()
    h1 <-  rCharts:::Highcharts$new()
    h1$chart(type = "column", backgroundColor = 'transparent')
    # Emotion
    h1$series(data = data$Level) %>%
      h1$addParams(width = "auto", dom= 'hist2')
      h1$xAxis(categories= data$Emotion,
               labels=list(enabled = T))
      h1$legend(enabled = T)
      h1$exporting(enabled = T, buttons=list(contextButton=list(menuItems=list(list(text= 'Export to PNG',
                                                                                    onclick= paste0("#!function () {
                                                                                                    this.exportChart({filename:'", 'hist2',"'})}!#")),
                                                                               list(text='Export to PDF',
                                                                                    onclick= paste0("#!function () {
                                                                                                    this.exportChart({type: 'application/pdf',filename:'", 'hist2',"'})}!#"))))))
      h1$title(text = paste0("Emotions"),style=list(fontSize=12))
      h1$navigation(buttonOptions=list(height=12,width=12,symbolSize=8,symbolX=6,symbolY=6,symbolStrokeWidth=1.1,theme=list(fill= 'transparent',states=list(hover=list(fill= 'transparent'),select=list(fill= 'transparent',stroke= 'transparent'),hover=list(fill= 'transparent', stroke= 'transparent')))))
      h1$plotOptions(bar = list(groupPadding= 0.1,pointPadding= 0.1,grouping= F))
      h1$tooltip(shared=T)
      return(h1)
  })
  
  output$plotly = renderPlotly({
    data = emotion_data()
    axis = list(title = (""))
    progress$set(value = 3)
    plot_ly(data, type = "bar", x = ~Emotion, y = ~Level, color = ~Emotion) %>% layout(xaxis = axis, yaxis = axis)
  })
  
  output$data = renderTable({
    data = emotion_data()
    data
  })
  
  output$age = renderText({
    data = face_data()
    progress$set(value = 4)
    on.exit(progress$close())
    # Extract the predicted age & concatenate with a string to serve as a dinamic plot title
    paste('Predicted age: ', data$faceAttributes[[1]])
  })
  
}

# Authentication
emotion_key = 'e84596b3f9b64377904e2e896dde8f42'
face_key = 'a79c66ae113f44a6857fbf10ba0bede5'

face_url = "https://api.projectoxford.ai/face/v1.0/detect?returnFaceId=true&returnFaceLandmarks=true&returnFaceAttributes=age"
emotion_url = 'https://api.projectoxford.ai/emotion/v1.0/recognize'

shinyApp(ui = ui, server = server)