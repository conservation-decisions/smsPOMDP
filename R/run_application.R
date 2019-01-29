#' @export
run_application = function(){
  app <- shiny::shinyApp(
    ui <- shiny::fluidPage(
      shiny::titlePanel("POMDP solver: When to stop managing or surveying cryptic threatened species ?"),
      shiny::sidebarLayout(
        shiny::sidebarPanel("POMDP parameters"
                            , shiny::numericInput('p0', 'Local probability of persistence (if survey or nothing)', min = 0, max = 1, value = 0.9)
                            , shiny::numericInput('pm', 'Local probability of persistence (if managed)', min = 0, max = 1, value = 0.94184)
                            , shiny::numericInput('d0', 'Local probability of detection (if managed or nothing)', min = 0, max = 1, value = 0.01)
                            , shiny::numericInput('d', 'Local probability of detection (if surveyed)', min = 0, max = 1, value = 0.78193)
                            , shiny::numericInput('V', 'Estimated economic value of the species ($/yr)', value = 175.133)
                            , shiny::numericInput('Cm', 'Estimated cost of managing ($/yr)', value = 18.784)
                            , shiny::numericInput('Cs', 'Estimated cost of surveying ($/yr)', min = 0, max = 1, value = 10.840)
                            , shiny::numericInput('disc', 'Discount factor', value = 0.95, max = 1, min = 0)
                            , shiny::helpText('Parameters only for the simulation')
                            , shiny::numericInput('Tmax', 'Duration of simulation', value = 10)
                            , shiny::numericInput('b', 'Initial belief state (extant)', value = 1)
                            , shiny::actionButton('sim', 'View simulation')
                            , shiny::actionButton('graph', 'View graphical solution')
                            , shiny::actionButton('past', 'Set past management stream')
        )
        , shiny::mainPanel(""
                           , shiny::uiOutput('main')

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
      disc = shiny::reactive(input$disc)
      b = shiny::reactive(input$b)
      state_prior = shiny::reactive({c(b(), 1-b())})
      Tmax = shiny::reactive(input$Tmax)

      #different scenarios depending on the users choice
      #launch a simulation
      shiny::observeEvent(input$sim, {
        output$plot = shiny::renderPlot({TigerPOMDP::sim(p0(), pm(), d0(), d(), V(), Cm(), Cs(), state_prior(), Tmax(), disc(), size = 2)})
        output$main = renderUI({
          shiny::plotOutput('plot', height = '1000px')
        })
      })

      #see decision graph
      shiny::observeEvent(input$graph, {
        output$plot = shiny::renderPlot({TigerPOMDP::graph(p0(), pm(), d0(), d(), V(), Cm(), Cs(), c(1,0), disc(), size = 2)})
        output$main = renderUI({
          shiny::plotOutput('plot', height = '1000px')
        })
      })

      #################################################################################
      #give a set of past actions and observations and see the best following strategy#
      #################################################################################

      #correponding panel
      shiny::observeEvent(input$past, {
        output$main = renderUI({
          shiny::tagList(
            shiny::column(width = 2
                          , shiny::numericInput('length_past', "Number of years for past management", value = 1, min = 1)
                          , shiny::actionButton('submit_length_past', 'Submit')
                          , shiny::uiOutput('past_control')
            )
            , shiny::column(width = 9
                            , shiny::plotOutput('past_plot')
                            , shiny::actionButton('next_policy', 'Next policy')
                            , shiny::plotOutput('next_policy_plot')
            )


          )
        })
      })

      #set past management stream
      observeEvent(input$submit_length_past,{
        output$past_control = shiny::renderUI({
          shiny::tagList(
            shiny::numericInput(inputId = 'past_init_b', 'Initial belief state (extant)', value = 1, min = 0, max = 1)
            , shiny::h5(paste0('Year ', 1))
            , shiny::selectInput(inputId = paste0("past_action_", 1)
                                 ,label = 'Action'
                                 ,choices = c('Manage', 'Survey', 'Nothing')
                                 , selected = 'Manage')
            , shiny::selectInput(inputId = paste0("past_obs_", 1)
                                 ,label = 'Observation'
                                 ,choices = c('Seen', 'Not_seen')
                                 , selected = 'Seen')
            , shiny::actionButton(inputId = paste0("submit_couple_", 1)
                                  , label = 'Submit couple')
            , conditionalPanel('input.length_past > 1',
                               lapply(c(2:input$length_past),
                                      function(i){
                                        shiny::conditionalPanel(paste0('input.submit_couple_', i-1)
                                                                , shiny::h5(paste0('Year ', i))
                                                                , shiny::selectInput(inputId = paste0("past_action_", i)
                                                                                     ,label = 'Action'
                                                                                     ,choices = c('Manage', 'Survey', 'Nothing')
                                                                                     , selected = 'Manage')
                                                                , shiny::selectInput(inputId = paste0("past_obs_", i)
                                                                                     ,label = 'Observation'
                                                                                     ,choices = c('Seen', 'Not_seen')
                                                                                     , selected = 'Seen')
                                                                , shiny::actionButton(inputId = paste0("submit_couple_", i)
                                                                                      , label = 'Submit couple')
                                        )

                                      })
            )
          )
        })
      })

      p_a = shiny::reactive(TigerPOMDP::past_actions(input)) #past actions
      p_o = shiny::reactive(TigerPOMDP::past_obs(input)) #past observations
      init_belief = shiny::reactive({c(input$past_init_b, 1-input$past_init_b)}) #initial belief state
      current_belief = shiny::reactive(TigerPOMDP::compute_belief(p0(), pm(), d0(), d(), V(), Cm(), Cs(),init_belief(), p_a(), p_o(), disc()))
      #
      observeEvent(input$submit_couple_1, {
        output$past_plot = shiny::renderPlot(TigerPOMDP::plot_stream(p0(), pm(), d0(), d(), V(), Cm(), Cs(),init_belief(), p_a(), p_o(), disc()))
      })
      observeEvent(input$next_policy, {
        output$next_policy_plot = shiny::renderPlot({TigerPOMDP::graph(p0(), pm(), d0(), d(), V(), Cm(), Cs(), current_belief(), disc())})
      })


    }
  )
  shiny::runApp(app)
}
