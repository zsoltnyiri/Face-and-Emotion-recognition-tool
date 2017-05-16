### Install required packages if needed

# install.packages('httr')
# install.packages('XML')
# install.packages('stringr')
# install.packages('ggplot2')
# install.packages('Kmisc')
# install.packages('jpeg')
# install.packages('Rmisc')

##### PREDICT EMOTION

# Provide your own FACE and EMOTION API keys after registering for a free account at 
# https://azure.microsoft.com/en-us/services/cognitive-services/face/
predict_emotion = function(url = readline(prompt = "Please define the image source (url): "), 
                           emotion_key = '...', 
                           face_key = '...')
{
  # Load relevant packages
  lapply(c('httr', 'XML', 'stringr', 'ggplot2', 'Kmisc', 'jpeg', 'Rmisc', 'grid'), require, character.only = T)
  # Define image
  mybody = list(url = url)
  
  ##### Emotion API
  # Define Microsoft API URL to request data
  emotion_url = 'https://api.projectoxford.ai/emotion/v1.0/recognize'
  # Request data from Microsoft
  faceEMO = POST(
    url = emotion_url,
    content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = emotion_key)),
    body = mybody,
    encode = 'json'
  )
  # Request results
  emotion_results = httr::content(faceEMO)[[1]]
  # Transform and prepare the results
  options(scipen = 999) 
  df_emotion_results = as.data.frame(as.matrix(emotion_results$scores))
  df_emotion_results$V1 = as.numeric(df_emotion_results$V1)*100
  colnames(df_emotion_results)[1] = "Level"
  df_emotion_results$Emotion = rownames(df_emotion_results)
  
  ##### Face API
  # Define Microsoft API URL to request data
  face_url = "https://api.projectoxford.ai/face/v1.0/detect?returnFaceId=true&returnFaceLandmarks=true&returnFaceAttributes=age"
  # Request data from Microsoft
  faceResponse = POST(
    url = face_url, 
    content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = face_key)),
    body = mybody,
    encode = 'json'
  )
  # Request results
  face_results = httr::content(faceResponse)[[1]]
  # Extract the predicted age & concatenate with a string to serve as a dinamic plot title
  age = paste('Predicted age:', '', face_results$faceAttributes[[1]])
  
  ##### Plots
  ### Plot emotion barchart with the age variable as title and the picture itself, delete the picture afterwards
  # Download the selected picture from the given url and save it
  download.file(url, 'host_pic.jpg', mode = 'wb')
  pic = readJPEG('host_pic.jpg', native = T)
  raster = rasterGrob(pic, interpolate  = T)
  # Plot the picture
  pic_plot = qplot(geom='blank') +
    annotation_custom(raster, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    geom_point()
  # Plot the predicted data
  plot = ggplot(data = df_emotion_results, aes(x = Emotion, y = Level)) +
    geom_bar(stat = 'identity') + 
    ggtitle(age) +
    ylab('%') +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  # Show both plots in one figure
  multiplot = multiplot(pic_plot, plot, cols = 2)
  # Remove the downloaded picture
  file.remove('host_pic.jpg')
  
  # Return the numerical emotion data and the plots
  return(list(df_emotion_results, multiplot))
}

predict_emotion()
