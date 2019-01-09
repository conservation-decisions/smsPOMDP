#' @export
run_application = function(){
  app <- shiny::shinyApp(
    ui <- shiny::fluidPage(
      shiny::titlePanel("POMDP solver"),
      shiny::sidebarLayout(
        shiny::sidebarPanel("Species' parameters"
                     , shiny::numericInput('p0', 'Local probability of persistance (if survey or nothing)', min = 0, max = 1, value = 0.9)
                     , shiny::numericInput('pm', 'Local probability of persistance if managed', min = 0, max = 1, value = 0.94184)
                     , shiny::numericInput('d0', 'Local probability of detection (if managed or nothing)', min = 0, max = 1, value = 0.01)
                     , shiny::numericInput('d', 'Local probability of detection if surveyed', min = 0, max = 1, value = 0.78193)
                     , shiny::numericInput('V', 'Estimated economic value of the species ($/yr)', value = 175.133)
                     , shiny::numericInput('Cm', 'Estimated cost of managing ($/yr)', value = 18.784)
                     , shiny::numericInput('Cs', 'Estimated cost of surveying ($/yr)', min = 0, max = 1, value = 10.840)
                     , shiny::numericInput('b', 'Initial belief state (extant)', value = 1)
                     , shiny::numericInput('Tmax', 'Duration of simulation', value = 10)
                     , shiny::selectInput('a0', 'Previous action', choices = c('Manage','Survey','Nothing'), selected = 'Manage')
                     , shiny::actionButton('sim', 'View simulation')
                     , shiny::actionButton('graph', 'View graphical solution')
        )
        , shiny::mainPanel("Simulation or main solution"
                    , shiny::plotOutput('plot', height = '1000px')
        )
      )

    ),


    server <- function(input, output, session){
      #Inputs
      p0 = shiny::reactive(input$p0)
      pm = shiny::reactive(input$pm)
      d0 = shiny::reactive(input$d0)
      d = shiny::reactive(input$d)
      V = shiny::reactive(input$V)
      Cm = shiny::reactive(input$Cm)
      Cs = shiny::reactive(input$Cs)
      b = shiny::reactive(input$b)
      state_prior = shiny::reactive({c(b(), 1-b())})
      Tmax = shiny::reactive(input$Tmax)
      a0 = shiny::reactive(input$a0)

      shiny::observeEvent(input$sim, {
        output$plot = shiny::renderPlot(TigerTest::sim(p0(), pm(), d0(), d(), V(), Cm(), Cs(), state_prior(), Tmax(), a0(), discount = 0.95, size = 2))
      })

      shiny::observeEvent(input$graph, {
        output$plot = shiny::renderPlot({TigerTest::graph(p0(), pm(), d0(), d(), V(), Cm(), Cs(), discount = 0.95, size = 2)})
      })
    }
  )
  shiny::runApp(app)
}
