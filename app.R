library(shiny)
library(shinyjs)
library(shinyauthr)
library(shinydashboard)
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
library(sodium)

options(scipen = 999)
options(shiny.trace = F)
options(shiny.reactlog = F)

user_base <- readRDS("user_base.rds")

ui = shinyUI(dashboardPage(
  
  dashboardHeader(title = "Face and Emotion recognition tool",
                  tags$li(class = "dropdown", style = "padding: 8px;",
                          shinyauthr::logoutUI("logout")) ,
                  tags$li(class = "dropdown", 
                          tags$a(icon("github"), 
                                 href = "https://github.com/zsoltnyiri/Face-and-Emotion-recognition-in-r-with-the-use-of-Microsoft-s-Face-and-Emotion-API-",
                                 title = "See the code on github"))
  ),
  
  dashboardSidebar(collapsed = TRUE,
    # sidebar elements
      textInput("url", "Paste an Image URL", value = 'https://www.biography.com/.image/t_share/MTU1MDE2MzU4OTIzMzQ3MDYy/gigi-hadid-appears-backstage-at-the-2015-muchmusic-video-awards-at-muchmusic-hq-on-june-21-2015-in-toronto-canada-photo-by-george-pimentel_wireimage-square.jpg', placeholder ='https://www.biography.com/.image/t_share/MTU1MDE2MzU4OTIzMzQ3MDYy/gigi-hadid-appears-backstage-at-the-2015-muchmusic-video-awards-at-muchmusic-hq-on-june-21-2015-in-toronto-canada-photo-by-george-pimentel_wireimage-square.jpg'),
      fileInput(inputId = 'files', 
                label = 'Select an Image',
                multiple = F,
                accept=c('image/png', 'image/jpeg'))
    ),
    # main panel elements
    dashboardBody(
      shinyjs::useShinyjs(),
      tags$head(tags$style(".table{margin: 0 auto;}"),
                tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                            type="text/javascript"),
                includeScript("returnClick.js")
      ),
      shinyauthr::loginUI("login"),
      uiOutput("fullUI"),
      # tabsetPanel(
      #   tabPanel("Plot",
      #            fluidRow(
      #              br(),
      #              column(12, align = "center", h3(textOutput("age")))),
      #            br(),
      #            fluidRow(
      #              column(6, plotOutput("pic")),
      #              column(6, plotlyOutput("hist"))
      #            )), 
      #   tabPanel("Raw Data", tableOutput("data"))
      HTML('<div data-iframe-height></div>')
    )
  )
)

server = function(input, output, clientData, session) {
  
   # #Saves the uploaded item onto a selected place
   #  observeEvent(input$files, {
   #    inFile <- input$files
   #    if (is.null(inFile))
   #      return()
   #    file.copy(inFile$datapath, file.path(getwd()), inFile$name)
   #  })
  
  # call login module supplying data frame, user and password cols
  # and reactive trigger
  credentials <- callModule(shinyauthr::login, 
                            id = "login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password,
                            sodium_hashed = TRUE,
                            log_out = reactive(logout_init()))

  # call the logout module with reactive trigger to hide/show
  logout_init <- callModule(shinyauthr::logout, 
                            id = "logout", 
                            active = reactive(credentials()$user_auth))
  
  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  
  face_data = reactive({
    req(credentials()$user_auth)
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
    req(credentials()$user_auth)
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
    req(credentials()$user_auth)
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
    req(credentials()$user_auth)
    data = face_data()
    data = data[-nrow(data),]
    data = as.data.frame(as.matrix(data))
    data$V1 = as.numeric(data$V1)*100
    colnames(data)[1] = "Level"
    data$Emotion = rownames(data)
    data
  })
  
  output$age = renderText({
    req(credentials()$user_auth)
    data = face_data()
    data = as.numeric(data[9,][1])
    # Extract the predicted age & concatenate with a string to serve as a dinamic plot title
    paste('Predicted age: ', data)
  })
  
  output$fullUI = renderUI({
    req(credentials()$user_auth)
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
      tabPanel("Raw Data", tableOutput("data")))
  })
  
}

# Authentication
face_key = readRDS("face_key.rds")

face_url = 'https://api.projectoxford.ai//face/v1.0/detect?returnFaceAttributes=age,emotion'

shinyApp(ui = ui, server = server)
