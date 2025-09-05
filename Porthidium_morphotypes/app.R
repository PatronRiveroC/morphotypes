library(shiny)

ui <- fluidPage(
  titlePanel("Porthidium Morphotypes"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("species", "Species:",
                  choices = c("P_arcosae", "P_dunni", "P_lansbergii", 
                              "P_nasutum", "P_ophryomegas", 
                              "P_porrasi", "P_volcanicum", "P_yucatanicum")), 
      checkboxGroupInput("morphs", "Morfotypes:",
                         choices = NULL)
    ),
    
    mainPanel(
      uiOutput("images_ui")
    )
  )
)

server <- function(input, output, session) {
  
  # Cuando cambia la especie, listamos morfos desde el sistema de archivos
  observeEvent(input$species, {
    local_base <- "www/images"              # sistema de archivos real
    species_path <- file.path(local_base, input$species)
    
    if (dir.exists(species_path)) {
      morphs <- list.dirs(species_path, full.names = FALSE, recursive = FALSE)
      morphs <- morphs[order(as.numeric(gsub("\\D", "", morphs)))] # orden correcto
    } else {
      morphs <- character(0)
    }
    
    updateCheckboxGroupInput(session, "morphs",
                             choices = morphs,
                             selected = morphs[1])
  })
  
  # Renderizamos las imágenes para mostrar en el navegador
  output$images_ui <- renderUI({
    req(input$species, input$morphs)
    
    web_base <- "images"    # recursos web (no incluir 'www/')
    views <- c("dorsal", "ventral", "head_dorsal", "lateral")
    
    all_morphs <- lapply(input$morphs, function(m) {
      imgs <- lapply(views, function(v) {
        path <- file.path(web_base, input$species, m, paste0(v, ".jpg"))
        btn_id <- paste0("zoom_", m, "_", v)
        
        tags$div(
          class = "col-sm-6 col-md-4 col-lg-3",
          actionLink(
            inputId = btn_id,
            label = tags$img(
              src = path, width = "100%", height = "auto",
              style = "margin:10px; border:1px solid #ccc; cursor: zoom-in;"
            )
          )
        )
      })
      
      tags$div(
        tags$h3(m),
        fluidRow(imgs),
        tags$hr()
      )
    })
    
    do.call(tagList, all_morphs)
  })
  
  # Eventos de zoom en modal
  observe({
    req(input$species, input$morphs)
    views <- c("dorsal", "ventral", "head_dorsal", "lateral")
    
    lapply(input$morphs, function(m) {
      lapply(views, function(v) {
        btn_id <- paste0("zoom_", m, "_", v)
        observeEvent(input[[btn_id]], {
          showModal(modalDialog(
            title = paste("Zoom:", m, "-", v),
            tags$img(
              src = file.path("images", input$species, m, paste0(v, ".jpg")),
              width = "100%", height = "auto",
              style = "border:2px solid #444;"
            ),
            easyClose = TRUE,
            size = "l"
          ))
        }, ignoreInit = TRUE)
      })
    })
  })
}

shinyApp(ui = ui, server = server)
