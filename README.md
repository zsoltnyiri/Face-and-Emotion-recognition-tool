## Face and emotion recognition app

An [__R Shiny__](https://shiny.rstudio.com/) application hosted on shiny.io utilizing Microsoft's free Face API. For authentication it uses [__shinyauthR__](https://github.com/PaulC91/shinyauthr) and [__sodium__](https://github.com/jeroen/sodium) to hash to login credentials. Plotting is done in [__Plotly__](https://plotly.com/). The application is __fully responsive__, so the viewing experience should be the same on desktop or mobile.

![Authentication](/cred_sample.JPG)

Just paste a url into the box, or upload any pictures to predict the age and emotions on the face present. 

![Sample usage](/sample.JPG)
*Sample picture is provided by https://www.thispersondoesnotexist.com/, therefore it doesn't exist, so no privacy rights have been violated.*

__Limitations__: currently it can only handle a single input (aka. 1 face) per picture, although it can be easily expanded later, should there be a need.

It can be accessed [here.](https://zsoltnyiri.shinyapps.io/Face_app/)
