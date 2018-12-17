#' @export
run_application = function(){
  library(shiny)
  app <- shiny::shinyApp(
    ui <- fluidPage(
      titlePanel("POMDP solver"),
      sidebarLayout(
        sidebarPanel("Species' parameters"
                     , numericInput('p0', 'Local probability of persistance (if survey or nothing)', min = 0, max = 1, value = 0.9)
                     , numericInput('pm', 'Local probability of persistance if managed', min = 0, max = 1, value = 0.94184)
                     , numericInput('d0', 'Local probability of detection (if managed or nothing)', min = 0, max = 1, value = 0.01)
                     , numericInput('d', 'Local probability of detection if surveyed', min = 0, max = 1, value = 0.78193)
                     , numericInput('V', 'Estimated economic value of the species ($/yr)', value = 175.133)
                     , numericInput('Cm', 'Estimated cost of managing ($/yr)', value = 18.784)
                     , numericInput('Cs', 'Estimated cost of surveying ($/yr)', min = 0, max = 1, value = 10.840)
                     , numericInput('b', 'Initial belief state (extant)', value = 1)
                     , numericInput('Tmax', 'Duration of simulation', value = 10)
                     , selectInput('a0', 'Previous action', choices = c('Manage','Survey','Nothing'), selected = 'Manage')
                     , actionButton('sim', 'View simulation')
                     , actionButton('graph', 'View graphical solution')
        )
        , mainPanel("Simulation or main solution"
                    , plotOutput('sim_plot', height = '1000px')
        )
      )

    ),


    server <- function(input, output, session){
      #Inputs
      p0 = reactive(input$p0)
      pm = reactive(input$pm)
      d0 = reactive(input$d0)
      d = reactive(input$d)
      V = reactive(input$V)
      Cm = reactive(input$Cm)
      Cs = reactive(input$Cs)
      b = reactive(input$b)
      state_prior = reactive({c(b(), 1-b())})
      Tmax = reactive(input$Tmax)
      a0 = reactive(input$a0)

      observeEvent(input$sim, {
        R = rew(p0(), pm(), d0(), d(), V(), Cm(), Cs())
        T = tr(p0(), pm(), d0(), d(), V(), Cm(), Cs())
        O = obs(p0(), pm(), d0(), d(), V(), Cm(), Cs())
        S = sim(T, O, R,0.95, state_prior(), Tmax(), a0())
        output$sim_plot = renderPlot(S)
      })

      observeEvent(input$graph, {
        R = rew(p0(), pm(), d0(), d(), V(), Cm(), Cs())
        T = tr(p0(), pm(), d0(), d(), V(), Cm(), Cs())
        O = obs(p0(), pm(), d0(), d(), V(), Cm(), Cs())
        output$sim_plot = renderPlot({graph(T, O, R)})
      })
    }
  )
  shiny::runApp(app)
}
