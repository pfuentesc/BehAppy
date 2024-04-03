library(shiny)
library(shinyFiles)


# Define UI for app
ui <- fluidPage(
  
  titlePanel("BehAppy"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Extracción de datos conductuales"),
      p("Esta aplicación ejecuta los scripts para extraer datos conductuales
        de diferentes tareas de RM."),
      p("El resultado de ejecutar esta herramienta será un fichero en formato texto
        o csv con los datos de behavioral de los sujetos que tuvieras en tu documento
        de sujetos para la tarea seleccionada. Este fichero puede ser leído por R, Excel,
        SPSS o JAMOVI."),
      p("Si el proceso falla, por favor, comprueba que los códigos del documento de 
        sujetos coincidan con los nombres de archivo de los logfiles/outputs de la tarea ya
        que este suele ser el motivo más habitual de errores."),
      br(),
      br(),
      br(),
      h3("Cosas rápidas"),
      br(),
      h4("Convertir TAP a CI (FSIQ)"),
      textInput("tap", "Introduce las puntuaciones de TAP separadas por comas")
      
      
    ),
    
    
    mainPanel(
      
      h4("Instrucciones"),
      p("Selecciona la tarea de la que quieres extraer los datos y sube un fichero de 
        texto en el que indiques los códigos de RM de los sujetos a analizar 
        (en el formato 000000_AAAAMMDD_ABC), con un sujeto en cada línea."),
      p("A continuación selecciona el directorio donde están guardados los logfiles 
        (ficheros de output de cada tarea) y el directorio donde quieres guardar
        el fichero de resultados. Finalmente debes indicar un nombre para el fichero
        de resultados."),
      
      br(),
      br(),
      
      h4("Elige la tarea a analizar"),
      # Input: Select the task to analyze ----
      # This will determine which Rscript is run
      selectInput("task", "Select task to analyze",
                  c("Select..." = "", 
                    "CMET" = "cmet", "NBACK" = "nback",
                    "NBACK (from 1.5T scanner in SJD)" = "nback_old", 
                    "RLT" = "rlt", "Rimas" = "rimas")),
      
      # Input: Select a file ----
      br(),
      h4("Fichero de sujetos"),
      fileInput("subjects", "Select subjects file",
                accept = ".txt"),
      
      # Input: Select logfile directory ---
      h4("Directorio de ficheros behavioral"),
      # actionButton("logdir", "Select logfile directory"),
      # textOutput("out_logdir"),
      textInput("logdir", "Select logfile directory"),
      
      # Input: Select the output directory ----
      h4("Directorio para resultados"),
      # actionButton("outputdir", "Select output directory"),
      # textOutput("out_outputdir"),
      textInput("outputdir", "Select output directory"),
      
      # Input: output file name
      br(),
      h4("Ponle un nombre a tu fichero de resultados"),
      textInput("outputname", ""),
      
      # Button for execution
      br(),
      br(),
      actionButton("runScript", "Run"),
      br(),
      verbatimTextOutput("resultado"),
      
      
    )
  )
  
)


# Define server logic
server <- function(input, output) {
  
  
  # observeEvent(input$logdir, {
  #   logdir <<- choose.dir()
  #   output$out_logdir <- renderText(logdir)
  # })
  # 
  # observeEvent(input$outputdir, {
  #   outputdir <<- choose.dir()
  #   output$out_outputdir <- renderText(outputdir)
  # })
  
  
  # Run script
  observeEvent(input$runScript, {
    
    
    # Go to app directory (for some reason this was changed after running the script once)
    setwd(appdir)
    
    
    subjects <<- read.table(input$subjects$datapath)$V1
    outfile <<- input$outputname
    logdir <<- gsub("\\", "/", input$logdir, fixed=TRUE)
    outputdir <<- gsub("\\", "/", input$outputdir, fixed=TRUE)
    
    
    # Load all scripts
    source("behavioral_CMET.R")
    source("behavioral_NBACK.R")
    source("behavioral_NBACK_old_data.R")
    source("behavioral_RLT.R")
    source("behavioral_RIMAS.R")
    
    
    
    # Load and execute script
    tryCatch({
      
      if(input$task=="cmet") {
        
        behavioral_CMET(outputdir, logdir, subjects, outfile)
        
        resultado <<- "Script ejecutado con éxito"
        
      } else if(input$task=="nback") {
        
        behavioral_NBACK(outputdir, logdir, subjects, outfile)
        
        resultado <<- "Script ejecutado con éxito"
        
      } else if(input$task=="nback_old") {
        
        behavioral_NBACK_old_data(outputdir, logdir, subjects, outfile)
        
        resultado <<- "Script ejecutado con éxito"
        
      } else if(input$task=="rlt") {
        
        behavioral_RLT(outputdir, logdir, subjects, outfile)
        
        resultado <<- "Script ejecutado con éxito"
        
      } else if(input$task=="rimas") {
        
        behavioral_RIMAS(outputdir, logdir, subjects, outfile)
        
        resultado <<- "Script ejecutado con éxito"
        
      } else {
        
        resultado <<- "Script no encontrado"
        
      }
      
      
    }, error = function(e) {
      
      resultado <<- paste("Error al ejecutar el script: ", e$message)
      
    })
    
    
    output$resultado <- renderText({
      
      resultado
      
    })
    
  })
  
  
}



shinyApp(ui = ui, server = server)
