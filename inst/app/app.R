library(shiny)
ui <- shiny::fluidPage(
  shiny::titlePanel("POMDP solver: When to stop managing or surveying cryptic threatened species ?"),
  shiny::sidebarLayout(
    shiny::sidebarPanel("POMDP parameters"
                        , shiny::numericInput('p0', 'Local probability of persistence (if survey or stop)', min = 0, max = 1, value = 0.9)
                        , shiny::numericInput('pm', 'Local probability of persistence (if manage)', min = 0, max = 1, value = 0.94184)
                        , shiny::numericInput('d0', 'Local probability of detection (if stop)', min = 0, max = 1, value = 0.01)
                        , shiny::numericInput('dm', 'Local probability of detection (if manage)', min = 0, max = 1, value = 0.01)
                        , shiny::numericInput('ds', 'Local probability of detection (if survey)', min = 0, max = 1, value = 0.78193)
                        , shiny::numericInput('V', 'Estimated economic value of the species ($/yr)', value = 175.133)
                        , shiny::numericInput('Cm', 'Estimated cost of managing ($/yr)', value = 18.784)
                        , shiny::numericInput('Cs', 'Estimated cost of surveying ($/yr)', min = 0, max = 1, value = 10.840)
                        , shiny::numericInput('disc', 'Discount factor', value = 0.95, max = 1, min = 0)
                        , shiny::actionButton('sim', 'View simulation')
                        , shiny::actionButton('graph', 'View graphical solution')
                        , shiny::actionButton('past', 'Set history of management and observations')
                        , shiny::conditionalPanel('input.sim'
                                                  , shiny::helpText('Parameters only for the simulation')
                                                  , shiny::numericInput('Tmax', 'Duration of simulation', value = 10, min = 1)
                                                  , shiny::numericInput('b', 'Initial belief state (extant)', value = 1, min = 0, max = 1)
                        )
    )
    , shiny::mainPanel(""
                       , shiny::uiOutput('main')
                       
    )
  )
  
)


server <- function(input, output, session){
  #Inputs
  p0 <- shiny::reactive({
    shiny::validate( shiny::need(input$p0 >=0 & input$p0 <=1 , "Please select local probability of persistence (if survey or stop) between 0 and 1") )
    input$p0
  })
  pm <- shiny::reactive({
    shiny::validate( shiny::need(input$pm >=0 & input$pm<=1 , "Please select local probability of persistence (if manage) between 0 and 1") )
    input$pm
  })
  d0 <- shiny::reactive({
    shiny::validate( shiny::need(input$d0 >=0 & input$d0 <=1 , "Please select local probability of detection (if stop) between 0 and 1") )
    input$d0
  })
  dm <- shiny::reactive({
    shiny::validate( shiny::need(input$dm >=0 & input$dm <=1 , "Please select local probability of detection (if manage) between 0 and 1") )
    input$dm
  })
  ds <- shiny::reactive({
    shiny::validate( shiny::need(input$ds >=0 & input$ds <=1 , "Please select local probability of detection (if survey) between 0 and 1") )
    input$ds
  })
  V <- shiny::reactive({
    shiny::validate( shiny::need(input$V >=0 , "Please select estimated economic value of the species ($/yr) positive") )
    input$V
  })
  Cm <- shiny::reactive({
    shiny::validate( shiny::need(input$Cm >=0, "Please select estimated cost of managing ($/yr) positive") )
    input$Cm
  })
  Cs <- shiny::reactive({
    shiny::validate( shiny::need(input$Cs >=0, "Please select estimated cost of survey ($/yr) positive") )
    input$Cs
  })
  disc <- shiny::reactive({
    shiny::validate( shiny::need(input$disc >=0 & input$disc <=1 , "Please select a discount factor between 0 and 1") )
    input$disc
  })
  b <- shiny::reactive({
    shiny::validate( shiny::need(input$b >=0 & input$b <=1 , "Please select initial belief state (extant) between 0 and 1") )
    input$b
  })
  state_prior <- shiny::reactive({c(b(), 1-b())})
  Tmax <- shiny::reactive({
    shiny::validate( shiny::need(input$Tmax >=0, "Please select a positive duration of simulation") )
    input$Tmax
  })
  
  #different scenarios depending on the users choice
  #launch a simulation
  shiny::observeEvent(input$sim, {
    output$plot <- shiny::renderPlot({smsPOMDP::sim(p0(), pm(), d0(), dm(), ds(), V(), Cm(), Cs(), state_prior(), Tmax(), disc(), size = 2)})
    output$main <- shiny::renderUI({
      shiny::plotOutput('plot', height = '1000px')
    })
  })
  
  #see decision graph
  shiny::observeEvent(input$graph, {
    output$plot <- shiny::renderPlot({smsPOMDP::graph(p0(), pm(), d0(), dm(), ds(), V(), Cm(), Cs(), c(1,0), disc(), size = 2)})
    output$main <- shiny::renderUI({
      shiny::plotOutput('plot', height = '1000px')
    })
  })
  
  #################################################################################
  #give a set of past actions and observations and see the best following strategy#
  #################################################################################
  
  #correponding panel
  shiny::observeEvent(input$past, {
    output$main <- shiny::renderUI({
      shiny::tagList(
        shiny::column(width = 2
                      , shiny::numericInput('length_past', "Number of years for past management", value = 1, min = 1)
                      , shiny::actionButton('submit_length_past', 'Submit')
                      , shiny::uiOutput('past_control')
        )
        , shiny::column(width = 9
                        , shiny::plotOutput('past_plot')
                        , shiny::conditionalPanel('input.submit_couple_1'
                                                  , shiny::actionButton('next_policy', 'Next policy')
                                                  , shiny::plotOutput('next_policy_plot')                    
                        )
                        
        )
        
        
      )
    })
  })
  
  #set past management stream
  shiny::observeEvent(input$submit_length_past,{
    output$past_control <- shiny::renderUI({
      shiny::tagList(
        shiny::numericInput(inputId = 'past_init_b', 'Initial belief state (extant)', value = 1, min = 0, max = 1)
        , shiny::h5(paste0('Year ', 1))
        , shiny::selectInput(inputId = paste0("past_action_", 1)
                             ,label = 'Action'
                             ,choices = c('Manage', 'Survey', 'Stop')
                             , selected = 'Manage')
        , shiny::selectInput(inputId = paste0("past_obs_", 1)
                             ,label = 'Observation'
                             ,choices = c('Seen', 'Not_seen')
                             , selected = 'Seen')
        , shiny::actionButton(inputId = paste0("submit_couple_", 1)
                              , label = 'Submit')
        , shiny::conditionalPanel('input.length_past > 1',
                                  lapply(c(2:input$length_past),
                                         function(i){
                                           shiny::conditionalPanel(paste0('input.submit_couple_', i-1)
                                                                   , shiny::h5(paste0('Year ', i))
                                                                   , shiny::selectInput(inputId = paste0("past_action_", i)
                                                                                        ,label = 'Action'
                                                                                        ,choices = c('Manage', 'Survey', 'Stop')
                                                                                        , selected = 'Manage')
                                                                   , shiny::selectInput(inputId = paste0("past_obs_", i)
                                                                                        ,label = 'Observation'
                                                                                        ,choices = c('Seen', 'Not_seen')
                                                                                        , selected = 'Seen')
                                                                   , shiny::actionButton(inputId = paste0("submit_couple_", i)
                                                                                         , label = 'Submit')
                                           )
                                           
                                         })
        )
      )
    })
  })
  
  p_a <- shiny::reactive(smsPOMDP::past_actions(input)) #past actions
  p_o <- shiny::reactive(smsPOMDP::past_obs(input)) #past observations
  init_belief <- shiny::reactive({
    shiny::validate( shiny::need(input$past_init_b >=0 & input$past_init_b <=1 , "Please select initial belief state (extant) between 0 and 1") )
    c(input$past_init_b, 1-input$past_init_b)
  }) #initial belief state
  current_belief <- shiny::reactive(smsPOMDP::compute_belief(p0(), pm(), d0(), dm(), ds(), V(), Cm(), Cs(),init_belief(), p_a(), p_o(), disc()))
  #
  shiny::observeEvent(input$submit_couple_1, {
    output$past_plot <- shiny::renderPlot(smsPOMDP::plot_stream(p0(), pm(), d0(), dm(), ds(), V(), Cm(), Cs(),init_belief(), p_a(), p_o(), disc(), size = 2))
  })
  shiny::observeEvent(input$next_policy, {
    output$next_policy_plot <- shiny::renderPlot({smsPOMDP::graph(p0(), pm(), d0(), dm(), ds(), V(), Cm(), Cs(), current_belief(), disc())})
  })
  
  
}

shiny::shinyApp(ui, server)
