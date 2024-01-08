library(shiny)
library(shinyFiles)
library(this.path)

appdir <<- this.dir()

runApp('app.R', host = '0.0.0.0', port = 8000)
