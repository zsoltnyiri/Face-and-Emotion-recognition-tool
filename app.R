library(shiny)
library(jpeg)
library(httr)
library(XML)
library(stringr)
library(ggplot2)
library(Kmisc)
library(jpeg)
library(Rmisc)
library(grid)
library(highcharter)
library(plotly)

options(scipen = 999)
options(shiny.trace = F)
options(shiny.reactlog = F)

ui = shinyUI(fluidPage(
  
  titlePanel("Face and Emotion recognition tool"),
  
  sidebarLayout(
    # sidebar elements
    sidebarPanel(
      textInput("url", "Paste an Image URL", value = 'https://www.biography.com/.image/t_share/MTU1MDE2MzU4OTIzMzQ3MDYy/gigi-hadid-appears-backstage-at-the-2015-muchmusic-video-awards-at-muchmusic-hq-on-june-21-2015-in-toronto-canada-photo-by-george-pimentel_wireimage-square.jpg', placeholder ='https://www.biography.com/.image/t_share/MTU1MDE2MzU4OTIzMzQ3MDYy/gigi-hadid-appears-backstage-at-the-2015-muchmusic-video-awards-at-muchmusic-hq-on-june-21-2015-in-toronto-canada-photo-by-george-pimentel_wireimage-square.jpg'),
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
                   column(6, plotlyOutput("hist"))
                 )), 
        tabPanel("Raw Data", tableOutput("data"))
      )
    )
  )
))

server = function(input, output, clientData, session) {
  
   # #Saves the uploaded item onto a selected place
   #  observeEvent(input$files, {
   #    inFile <- input$files
   #    if (is.null(inFile))
   #      return()
   #    file.copy(inFile$datapath, file.path(getwd()), inFile$name)
   #  })
  
  face_data = reactive({
    url = input$url
    mybody = list(url = url)
    faceResponse = POST(
      url = face_url, 
      content_type('application/json'), 
      add_headers(.headers = c('Ocp-Apim-Subscription-Key' = face_key)),
      body = mybody,
      encode = 'json'
    )
    face_data = httr::content(faceResponse)[[1]]
    emotions = as.data.frame(as.matrix(face_data$faceAttributes$emotion))
    age = as.data.frame(as.matrix(face_data$faceAttributes$age))
    colnames(age) = 'age'
    age = t(age)
    face_df = rbind(emotions, age)
    face_df
  })
  
  output$pic = renderPlot({
    url = input$url
    # Download the selected picture from the given url and save it
    download.file(url, 'host_pic.jpg', mode = 'wb')
    pic = readJPEG('host_pic.jpg', native = T)
    raster = rasterGrob(pic, interpolate  = T)
    # Plot the picture
    qplot(geom ='blank') +
      annotation_custom(raster, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      geom_point()
    #Remove the downloaded picture
    #file.remove('host_pic.jpg')
  })

  output$hist = renderPlotly({
    data = face_data()
    data = data[-nrow(data),]
    data = as.data.frame(as.matrix(data))
    data$V1 = as.numeric(data$V1)*100
    colnames(data)[1] = "Level"
    data$Emotion = rownames(data)
    # Plot the predicted data
    plot_ly(
      data = data,
      x = ~Emotion,
      y = ~Level,
      color = ~Emotion,
      type = "bar")
  })
  
  output$data = renderTable({
    data = face_data()
    data = data[-nrow(data),]
    data = as.data.frame(as.matrix(data))
    data$V1 = as.numeric(data$V1)*100
    colnames(data)[1] = "Level"
    data$Emotion = rownames(data)
    data
  })
  
  output$age = renderText({
    data = face_data()
    data = as.numeric(data[9,][1])
    # Extract the predicted age & concatenate with a string to serve as a dinamic plot title
    paste('Predicted age: ', data)
  })
  
}

# Authentication
face_key = 'your key here'

face_url = 'https://api.projectoxford.ai//face/v1.0/detect?returnFaceAttributes=age,emotion'

shinyApp(ui = ui, server = server)
