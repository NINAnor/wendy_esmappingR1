#' training UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_instructions_ui <- function(id){
  ns <- NS(id)
  tagList(
    mainPanel(
      # "The following section shows you how the mapping of nature values will be done. In total you will try to map your use of four different nature values.",
      # br(),
      # actionButton(ns("proc1"), "proceed"),
      uiOutput(ns("task_1")),
      uiOutput(ns("task_2")),
      uiOutput(ns("task_3")),
      uiOutput(ns("task_4")),
      uiOutput(ns("task_5"))


    )#/main panel

  )
}

#' training Server Functions
#'
#' @noRd
mod_instructions_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    rv1<-reactiveValues(
      u = reactive({})
    )
    
    output$task_1<-renderUI({
      tagList(
        br(),
        "The following section shows you how the mapping of nature values will be done. In total you will try to map your use of four different nature values.",
        br(),
        actionButton(ns("proc1"), "proceed")
      )
    })

    #descr 0
    observeEvent(input$proc1,{
      output$task_2<-renderUI({
        tagList(
          h4("1. Description of nature value"),
          br(),
          "In the mapping window, we first show you a title, an image and a description of the nature value we ask you to map. Nature values are complex, thus it is important that you first read this description carefully and that you reflect on where, when or in which form you already used this nature value.",
          br(),
          h4("2. Importance of the value"),
          br(),
          "Subsequently you are asked to indicate how important this nature value is for you personally and in general for the society. You can ask yourself How much you and others might use or benefit from this nature value. Please make sure that you refer your rating only to the study area indicated in the introduction of the study.",
          br(),
          actionButton(ns("proc2"),"proceed")
        )
      })

      removeUI(selector = paste0("#",ns("task_1")))

    })

    # description 1
    observeEvent(input$proc2,{
      output$task_3<-renderUI({
        tagList(
          h4("3. Are you able to map areas where you use nature values?"),
          br(),
          "You are now going to be asked if you feel comfortable to indicate on a map within the study area where you think you personally can use the described nature value.",
          br(),
          "If you can’t map, no problem, you are now just being asked if you would trust a map of this nature value that has been developed by experts. And the next nature value will be presented.",
          br(),
          actionButton(ns("proc3"),"What if you can map...")
        )
      })

      removeUI(selector = paste0("#",ns("task_2")))

    })

    # description 2
    observeEvent(input$proc3,{
      output$task_4<-renderUI({
        tagList(
          h4("4. Interactive mapping of nature values"),
          br(),
          "If you can map the nature value, an interactive map with orange borders indicating the study area will appear. You can pan and zoom the map using your mouse.",
          br(),
          "You then need to create one or several rectangles that show areas where you personally use the nature values significantly. Please be as precise as possible while drawing these rectangles.",
          "As shown in the animation you can create a rectangle by selecting the rectangle tool. Click in the map where you want to have a corner, press and hold the left mouse and drag the rectangle in the direction you want to place it. The rectangle must meet the following criteria:",
          br(),
          fluidRow(strong("  - not too small areas (edge length longer than 300m)")),
          fluidRow(strong("  - not overlapping with the orange study area border")),
          "You can draw as many polygons as you like. If you want, you can modify or remove rectangles as shown in the animation. Press “save areas” to submit your answers.",
          br(),
          fluidRow(HTML('<iframe src="https://drive.google.com/file/d/1AqgXf3V7HHmtfAUA59efiMFFBxB7iX_F/preview" width="640" height="480" allow="autoplay"></iframe>')),
          br(),
          fluidRow(actionButton(ns("proc4"),"proceed"))

        )
      })
      removeUI(selector = paste0("#",ns("task_3")))

    })

    # description 3
    observeEvent(input$proc4,{
      output$task_5<-renderUI({
        tagList(
          h3("5. Importance of your indicated areas"),
          br(),
          "As soon as you have saved the areas, they will appear on the map with a red number. Below you find for each area a slider with the same number.",
          "Now you can set the slider value for each polygon. 5 indicates that the area is very important for you to use the nature value. 1= that it is a less important area to use the nature value but still relevant for you.",
          "Finally, you can write some words or short sentences why you selected these areas.",
          "Press submit",
          br(),
          fluidRow(HTML('<iframe src="https://drive.google.com/file/d/1nOrJjxGdwgAlTMF82sRRq5fs-ZUYH_Pg/preview" width="640" height="480" allow="autoplay"></iframe>')),
          br(),
          fluidRow(actionButton(ns("proc5"),"proceed"))
        )
      })
      removeUI(selector = paste0("#",ns("task_4")))

      })

    # description 4
    observeEvent(input$proc5,{
      output$task_5<-renderUI({
        tagList(
          h3("6. Your map"),
          br(),
          "The rectangles and ratings are stored in our database and a map that shows you the probability that you use and benefit from the nature value over the whole study area is calculated. This might take up to 1min, so please wait until you can skip to the next mapping task.",
          br(),
          actionButton(ns('sub3'), 'Go to first mapping task', class='btn-primary')
        )
      })


    })

    observeEvent(input$sub3,{
      rv1$u <-reactive({1})
    })

    # play back the value of the confirm button to be used in the main app
    cond <- reactive({rv1$u()})

    return(cond)
  })
}

## To be copied in the UI
# mod_training_ui("training_1")

## To be copied in the server
# mod_training_server("training_1")
