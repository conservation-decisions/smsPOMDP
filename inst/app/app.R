library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(sarsop)
library(smsPOMDP)
library(plotly)
library("bsplus")

## MODALS OF PARAMETERS#####

modal_p0 <-
  bsplus::bs_modal(
    id = "modal_p0",
    title = "Local probability of persitance : P(extant/extant, survey or stop)",
    body = shiny::includeMarkdown(system.file("markdown", "modal_p0.md", package = "smsPOMDP")),
    size = "medium"
  )

modal_pm <-
  bsplus::bs_modal(
    id = "modal_pm",
    title = "Local probability of persitance if manage : P(extant/extant, manage)",
    body = shiny::includeMarkdown(system.file("markdown", "modal_pm.md", package = "smsPOMDP")),
    size = "medium"
  )

modal_d0 <-
  bsplus::bs_modal(
    id = "modal_d0",
    title = "Local probability of detection : P(present/extant, stop)",
    body = shiny::includeMarkdown(system.file("markdown", "modal_d0.md", package = "smsPOMDP")),
    size = "medium"
  )

modal_dm <-
  bsplus::bs_modal(
    id = "modal_dm",
    title = "Local probability of detection : P(present/extant, manage)",
    body = shiny::includeMarkdown(system.file("markdown", "modal_dm.md", package = "smsPOMDP")),
    size = "medium"
  )

modal_ds <-
  bsplus::bs_modal(
    id = "modal_ds",
    title = "Local probability of detection if survey : P(present/extant, survey)",
    body = shiny::includeMarkdown(system.file("markdown", "modal_ds.md", package = "smsPOMDP")),
    size = "medium"
  )

modal_V <-
  bsplus::bs_modal(
    id = "modal_V",
    title = "Estimated economic value of the species ($/yr)",
    body = shiny::includeMarkdown(system.file("markdown", "modal_V.md", package = "smsPOMDP")),
    size = "medium"
  )

modal_Cm <-
  bsplus::bs_modal(
    id = "modal_Cm",
    title = "Estimated cost of managing ($/yr)",
    body = shiny::includeMarkdown(system.file("markdown", "modal_Cm.md", package = "smsPOMDP")),
    size = "medium"
  )

modal_Cs <-
  bsplus::bs_modal(
    id = "modal_Cs",
    title = "Estimated cost of survey ($/yr)",
    body = shiny::includeMarkdown(system.file("markdown", "modal_Cs.md", package = "smsPOMDP")),
    size = "medium"
  )

modal_initial_belief <-
  bsplus::bs_modal(
    id = "modal_initial_belief",
    title = "Initial belief state",
    body = shiny::includeMarkdown(system.file("markdown", "modal_initial_belief.md", package = "smsPOMDP")),
    size = "medium"
  )

modal_Tmanage <-
  bsplus::bs_modal(
    id = "modal_Tmanage",
    title = "Duration of past data (time steps)",
    body = shiny::includeMarkdown(system.file("markdown", "modal_Tmanage.md", package = "smsPOMDP")),
    size = "medium"
  )

modal_Tsim <-
  bsplus::bs_modal(
    id = "modal_Tsim",
    title = "Duration of simulation (time steps)",
    body = shiny::includeMarkdown(system.file("markdown", "modal_Tsim.md", package = "smsPOMDP")),
    size = "medium"
  )

modal_case_study <-
  bsplus::bs_modal(
    id = "modal_case_study",
    title = "Case of study",
    body = shiny::includeMarkdown(system.file("markdown", "modal_case_study.md", package = "smsPOMDP")),
    size = "medium"
  )
## UI ####
ui <- shinydashboard::dashboardPage(
  title = "POMDP solver: When to stop managing or surveying cryptic threatened species ?",
  
  # HEADER #############################
  shinydashboard::dashboardHeader(
    title = "smsPOMDP", 
    tags$li(
      a(
        strong("ABOUT smsPOMDP"),
        height = 40,
        href = "https://github.com/conservation-decisions/smsPOMDP",
        title = "",
        target = "_blank"
      ),
      class = "dropdown"
    )
    
  ),
  
  # SIDEBAR #####################
  shinydashboard::dashboardSidebar(disable = TRUE),
  
  # BODY ############################## 
  shinydashboard::dashboardBody(
    tags$head(tags$style(shiny::HTML(".shiny-output-error-validation 
                              {color: red; font-size: large; font-weight: bold;}"))),
    
    shiny::fluidRow(
      #POMDP PARAMETERS ####
      tags$div(class = "another-box", id = "primariy2",
               shinydashboard::box(       
                 title = "POMDP parameters", width = 3, solidHeader = TRUE, status = "primary",
                 # Probabilities ####
                 shiny::h3("Probabilities"),
                 shiny::numericInput('p0', 'Local probability of persistence (if survey or stop)',
                                     min = 0, max = 1, value = 0.9) %>%
                   bsplus::shinyInput_label_embed(
                     bsplus::shiny_iconlink() %>%
                       bsplus::bs_attach_modal(id_modal = "modal_p0")
                   ),
                 shiny::numericInput('pm', 'Local probability of persistence (if manage)', 
                                     min = 0, max = 1, value = 0.94184) %>%
                   bsplus::shinyInput_label_embed(
                     bsplus::shiny_iconlink() %>%
                       bsplus::bs_attach_modal(id_modal = "modal_pm")
                   ),
                 shiny::numericInput('d0', 'Local probability of detection (if stop)',
                                     min = 0, max = 1, value = 0.01) %>%
                   bsplus::shinyInput_label_embed(
                     bsplus::shiny_iconlink() %>%
                       bsplus::bs_attach_modal(id_modal = "modal_d0")
                   ),
                 shiny::numericInput('dm', 'Local probability of detection (if manage)', 
                                     min = 0, max = 1, value = 0.01) %>%
                   bsplus::shinyInput_label_embed(
                     bsplus::shiny_iconlink() %>%
                       bsplus::bs_attach_modal(id_modal = "modal_dm")
                   ),
                 shiny::numericInput('ds', 'Local probability of detection (if survey)', 
                                     min = 0, max = 1, value = 0.78193) %>%
                   bsplus::shinyInput_label_embed(
                     bsplus::shiny_iconlink() %>%
                       bsplus::bs_attach_modal(id_modal = "modal_ds")
                   ),
                 # Costs ####
                 shiny::h3("Costs"),
                 shiny::numericInput('V', 'Estimated economic value of the species ($/yr)',
                                     value = 175.133) %>%
                   bsplus::shinyInput_label_embed(
                     bsplus::shiny_iconlink() %>%
                       bsplus::bs_attach_modal(id_modal = "modal_V")
                   ),
                 shiny::numericInput('Cm', 'Estimated cost of managing ($/yr)',
                                     value = 18.784) %>%
                   bsplus::shinyInput_label_embed(
                     bsplus::shiny_iconlink() %>%
                       bsplus::bs_attach_modal(id_modal = "modal_Cm")
                   ),
                 shiny::numericInput('Cs', 'Estimated cost of surveying ($/yr)',
                                     min = 0, value = 10.840) %>%
                   bsplus::shinyInput_label_embed(
                     bsplus::shiny_iconlink() %>%
                       bsplus::bs_attach_modal(id_modal = "modal_Cs")
                   ),
                 # Case studies ####
                 shiny::h3("Case studies"),
                 shiny::selectInput("case_study", "Select case study", 
                                    choices = c("Sumatran tiger", 
                                                "Expensive management",
                                                "Detection in management")) %>%
                   bsplus::shinyInput_label_embed(
                     bsplus::shiny_iconlink() %>%
                       bsplus::bs_attach_modal(id_modal = "modal_case_study")
                   ),
                 shiny::actionButton("reload", "Reload parameters")
                 
               ),
               tags$style(shiny::HTML("
                        #primariy2 .box.box-solid.box-primary>.box-header {
                        color:#fff;
                        background:#666666
                        }

                        .box.box-solid.box-primary {
                        border-bottom-color:#666666;
                        border-left-color:#666666;
                        border-right-color:#666666;
                        border-top-color:#666666;
                        }

                        "))
      ),
      
      # Plot parameters ####
      shinydashboard::box(width = 3,
                          shiny::numericInput("initial_belief", "Initial belief state", 
                                              value = 1, min = 0, max = 1) %>%
                            bsplus::shinyInput_label_embed(
                              bsplus::shiny_iconlink() %>%
                                bsplus::bs_attach_modal(id_modal = "modal_initial_belief")
                            )
      ),
      
      shinydashboard::box(width = 3,
                          shiny::numericInput('Tmanage', "Duration of past data (time steps)", 
                                              value = 5, min = 0) %>%
                            bsplus::shinyInput_label_embed(
                              bsplus::shiny_iconlink() %>%
                                bsplus::bs_attach_modal(id_modal = "modal_Tmanage")
                            )
      ),
      
      shinydashboard::box(width = 3,
                          shiny::numericInput('Tsim', "Duration of simulation (time steps)", 
                                              value = 10, min = 0, max = 20) %>%
                            bsplus::shinyInput_label_embed(
                              bsplus::shiny_iconlink() %>%
                                bsplus::bs_attach_modal(id_modal = "modal_Tsim")
                            )
      ),
      # plots####
      shinydashboard::box(width = 9, 
                          "Choose actions performed in the past",
                          plotly::plotlyOutput("plot_actions", height = "350px")),
      shinydashboard::box(width = 9, 
                          "Choose observations following the actions",
                          plotly::plotlyOutput("plot_observations", height = "300px")),
      shinydashboard::box(width = 9, 
                          "Explore discounted expected rewards over time",
                          plotly::plotlyOutput("plot_reward", height = "300px"))
      # add modals ####
      , modal_p0
      , modal_pm
      , modal_d0
      , modal_dm
      , modal_ds
      , modal_V
      , modal_Cm
      , modal_Cs
      , modal_initial_belief
      , modal_Tmanage
      , modal_Tsim
      , modal_case_study
      # activate tooltips, popovers, and MathJax ####
      , bsplus::use_bs_tooltip()
      , bsplus::use_bs_popover()
      , shiny::withMathJax()
      
    )
    
  )
)

# SERVER ###############################
server <- function(input, output, session){
  
  #Inputs #####
  p0 <- shiny::reactive({
    shiny::validate(shiny::need(input$p0 >=0 & input$p0 <=1 , "Please select local probability of persistence (if survey or stop) between 0 and 1") )
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
  Tmanage <- shiny::reactive({
    shiny::validate( shiny::need(input$Tmanage >=0, "Please select horizon of past management positive") )
    input$Tmanage
  })
  init_belief <- shiny::reactive({
    shiny::validate( shiny::need(input$initial_belief >=0 & input$initial_belief <=1 , "Please select initial belief state (extant) between 0 and 1") )
    c(input$initial_belief, 1-input$initial_belief)
  }) #initial belief state
  Tsim <- shiny::reactive({
    shiny::validate( shiny::need(input$Tsim >=0, "Please select a positive duration of simulation") )
    input$Tsim
  })
  
  Tplot <- shiny::reactive({
    shiny::validate( shiny::need(input$Tsim >=0, "Please select a positive duration of simulation") )
    max(10, input$Tsim)
  })
  
  # Treat inputs #####
  input_past <- shiny::reactiveValues(
    data_actions=c(),
    actions = c(),
    data_observations = c(),
    observations = c(),
    belief_extant = isolate({matrix(init_belief(), ncol = 2)}),
    rewards = c()
  )
  
  data_action_reactive <- shiny::reactive({
    return(input_past$data_actions)
  })
  data_observation_reactive <- shiny::reactive({
    return(input_past$data_observations)
  })
  actions_past <- shiny::reactive({input_past$actions})
  observations_past <- shiny::reactive({input_past$observations})
  # change Tmanage ####
  shiny::observeEvent(Tmanage(), {
    if (input$Tmanage == 0){
      input_past$data_actions <- c()
      input_past$actions <- c()
      input_past$data_observations <- c()
      input_past$observations <- c()
      input_past$belief_extant <- matrix(init_belief(), ncol = 2)
      return()
    } 
    
    if (is.null(input_past$data_actions)) {
      actions <- c("Stop","Survey","Manage")
      time_steps <- seq_len(Tmanage())
      tab <- expand.grid(actions, time_steps)
      tab$color <- "Off"
      names(tab) <- c("action", "step", "color")
      input_past$data_actions <- tab
    } else {
      data <- input_past$data_actions
      Tmax <- max(data$step)
      diff <- Tmanage()-Tmax
      if (diff > 0){
        actions <- c("Stop","Survey","Manage")
        time_steps <- seq(Tmax+1,Tmanage())
        tab <- expand.grid(actions, time_steps)
        tab$color <- "Off"
        names(tab) <- c("action", "step", "color")
        input_past$data_actions <- rbind(data, tab)
      } else {
        input_past$data_actions <- data[which(data$step <= Tmanage()),]
      }
      
    }
    
    if (is.null(input_past$data_observations)) {
      obs <- c("Not seen","Seen")
      time_steps <- seq_len(Tmanage())
      tab2 <- expand.grid(obs, time_steps)
      tab2$color <- "Off"
      names(tab2) <- c("obs", "step", "color")
      input_past$data_observations <- tab2
    } else {
      data <- input_past$data_observations
      Tmax <- max(data$step)
      diff <- Tmanage()-Tmax
      if (diff > 0){
        obs <- c("Not seen","Seen")
        time_steps <- seq(Tmax+1,Tmanage())
        tab <- expand.grid(obs, time_steps)
        tab$color <- "Off"
        names(tab) <- c("obs", "step", "color")
        input_past$data_observations <- rbind(data, tab)
      } else {
        input_past$data_observations <- data[which(data$step <= Tmanage()),]
      }
      
    }
    if ( (Tmanage()-length(input_past$actions)) < 0){
      input_past$actions <- input_past$actions[seq_len(Tmanage())]
    }
    if ( (Tmanage()-length(input_past$observations)) < 0){
      input_past$observations <- input_past$observations[seq_len(Tmanage())]
    }
  })
  
  # click on action plot ####
  shiny::observeEvent(plotly::event_data("plotly_click", source = "A"),{
    d <- plotly::event_data("plotly_click", source = "A")
    if (is.null(d)){
      return()
    }
    isolate({
      x <- d$x + Tmanage() +1
      y <- d$y
      tab <-input_past$data_actions
      tab[which(tab$step == x),]$color <- "Off"
      tab[which((tab$step == x)&(tab$action == y)),]$color <- "On"
      input_past$data_actions <- tab
      
      input_past$actions[x] <-d$y
    })
  })
  
  # click on observation plot ####
  shiny::observeEvent(plotly::event_data("plotly_click", source = "O"),{
    d <- plotly::event_data("plotly_click", source = "O")
    if (is.null(d)){
      return()
    }
    isolate({
      x <- d$x + Tmanage() +1
      y <- d$y
      tab <-input_past$data_observations 
      tab[which(tab$step == x),]$color <- "Off"
      tab[which((tab$step == x)&(tab$obs == y)),]$color <- "On"
      input_past$data_observations <- tab
      input_past$observations[x] <-d$y
    })
    
  })
  
  # beliefs ####
  #reactive list of beliefs in the past, matrix 
  beliefs <- shiny::reactive({
    if (length(observations_past()) != length(actions_past())
        |any(is.na(observations_past()))
        |any(is.na(actions_past())) ){
      return(input_past$belief_extant)
    } else {
      input_past$belief_extant <- smsPOMDP::compute_belief_list(p0(), pm(), d0(), dm(), ds(), 
                                                                V(), Cm(), Cs(),init_belief(), 
                                                                actions_past(), observations_past())
      return(input_past$belief_extant)
    }
  })
  
  #reactive vector of current belief
  current_belief <- shiny::reactive({
    b <- beliefs()
    return(b[nrow(b),])
    # input_past$belief_extant[nrow(input_past$belief_extant), ]
  })
  
  # rewards ####
  #reactive vector of rewards
  rewards <- shiny::reactive({
    if (length(observations_past()) != length(actions_past())
        |any(is.na(observations_past()))
        |any(is.na(actions_past())) ){
      return(input_past$rewards)
    } else {
      input_past$rewards <- smsPOMDP::reward_belief(p0(), pm(), d0(), dm(), ds(), 
                                                    V(), Cm(), Cs(),beliefs(), actions_past())
      return(input_past$rewards)
    }
  })
  #reactive datasets of simulations ####
  data_sim <- shiny::reactive({smsPOMDP::simulations_tab(p0(), pm(), d0(), dm(), ds(), V(), Cm(), Cs(), current_belief(), Tsim())})
  data_sim_ref <- shiny::reactive({smsPOMDP::simulations_tab(p0(), pm(), d0(), dm(), ds(), V(), Cm(), Cs(), init_belief(), Tmanage()+Tsim())})
  
  # Plots ####
  #optimal solution plot ####
  optimal_solution <- shiny::reactive({
    log_dir <- tempdir()
    infile <- paste0(log_dir, "/optimal_sol.png")
    png(infile, width = 1280, height = 720, units = "px")
    smsPOMDP::graph(p0(), pm(), d0(), dm(),
                    ds(), V(), Cm(), Cs(), current_belief(),
                    size = 2)
    
    dev.off()
    png::readPNG(infile)
  })
  #actions plot ####
  output$plot_actions <- plotly::renderPlotly({
    xaxis <- list(
      title = "Time steps",
      autotick = FALSE,
      ticks = "outside",
      dtick = 1,
      range = c(-Tmanage()-1, Tplot()+2)
    )
    yaxis <- list(type = "category", 
                  categoryorder ="array",
                  categoryarray = c("Stop", "Survey", "Manage"),
                  title = "Actions")
    if (Tmanage() >0){
      if (length(unique(data_action_reactive()$color))==1){colors_palette = c("grey")} else {colors_palette = c("grey","red")}
      
      if ((length(observations_past()) == length(actions_past())) 
          & (length(observations_past()) == Tmanage())){
        
        plotly::plot_ly(
          height = 350, source = "A") %>% 
          plotly::add_trace(x=~step-Tmanage()-1, y=~action, hoverinfo="text",text=~action, 
                            data = data_action_reactive(), type = 'scatter',
                            mode = 'markers',
                            marker = list(size = 20, opacity = 0.8) ,
                            color = ~color,
                            colors = colors_palette,
                            showlegend = FALSE) %>%
          plotly::layout( xaxis = xaxis,yaxis=yaxis, showlegend = TRUE
                          , images = list(
                            source = raster2uri(as.raster(optimal_solution())),
                            x = (Tmanage()+1)/(Tmanage()+Tplot()), y = 0.5,
                            sizex = (Tplot()-1)/(Tmanage()+Tplot()), sizey = 0.9,
                            xref = "paper", yref = "paper",
                            xanchor = "left", yanchor = "middle"
                          )
                          , legend = list(orientation = 'h', y = 1.1)
                          , margin = list(l=100, r=100)
          )
      } else {
        plotly::plot_ly(
          height = 350, source = "A") %>% 
          plotly::add_trace(x=~step-Tmanage()-1, y=~action, hoverinfo="text",text=~action, 
                            data = data_action_reactive(), type = 'scatter',
                            mode = 'markers',
                            marker = list(size = 20, opacity = 0.8) ,
                            color = ~color,
                            colors = colors_palette,
                            showlegend = FALSE) %>%
          plotly::layout( xaxis = xaxis,yaxis=yaxis, showlegend = FALSE
                          , margin = list(l=100, r=100)
          )
      }
    } else if (Tmanage()==0){
      xaxis <- list(
        title = "Time steps",
        autotick = FALSE,
        ticks = "outside",
        dtick = 1,
        range = c(-Tmanage()-1, Tplot() + 2)
      )
      yaxis <- list(type = "category", categoryorder ="array",
                    categoryarray = c("Stop", "Survey", "Manage"),
                    visible = FALSE)
      plotly::plot_ly(
        height = 350, source = "A") %>% 
        plotly::layout( xaxis = xaxis,yaxis=yaxis, showlegend = TRUE
                        , images = list(
                          source = raster2uri(as.raster(optimal_solution())),
                          x = (Tmanage()+1)/(Tmanage()+Tplot()), y = 0.5,
                          sizex = (Tplot()-1)/(Tmanage()+Tplot()), sizey = 0.9,
                          xref = "paper", yref = "paper",
                          xanchor = "left", yanchor = "middle"
                        )
                        , legend = list(orientation = 'h', y = 1.1)
                        , margin = list(l=100, r=100)
        )
    }
  })
  #observations plot ####
  output$plot_observations<- plotly::renderPlotly({
    xaxis <- list(
      title = "Time steps",
      autotick = FALSE,
      ticks = "outside",
      dtick = 1,
      range = c(-Tmanage()-1, Tplot() + 2)
    )
    yaxis <- list(
      type = "category",
      categoryorder ="array",
      categoryarray = c("Not seen", "Seen"),
      title = "Observations"
    )
    ay <- list(
      overlaying = "y",
      side = "right",
      title = "b(extant)",
      range = c(-0.3, 1.3)
    )
    
    if (Tmanage()>0){
      b <- beliefs()
      extant <-  c(b[,1])
      xtime <- seq(1,length(extant))-Tmanage()-1
      
      if (length(unique(data_observation_reactive()$color))==1){colors_palette = c("grey")} else {colors_palette = c("grey","red")}
      
      if ((length(observations_past()) == length(actions_past())) 
          & (length(observations_past()) == Tmanage())){
        plotly::plot_ly(
          height = 300, source = "O") %>%
          plotly::add_trace(x=~step-Tmanage()-1, y=~obs, hoverinfo="text",text=~obs, 
                            data = data_observation_reactive(), type = 'scatter',
                            mode = 'markers',
                            marker = list(size = 20, opacity = 0.8) ,
                            color= ~color,
                            colors = colors_palette,
                            showlegend = FALSE) %>%
          
          #simulations from present
          plotly::add_trace(x=seq(0, Tsim()), y=~mean_belief, name = "User's optimal future trajectory b(extant)",
                            data=data_sim(), yaxis = "y2", type = 'scatter',
                            mode = 'lines+markers', visible = TRUE,
                            line = list(color = "green"),
                            marker =list(color = "green"),
                            colors = "green") %>%
          plotly::add_ribbons(x=seq(0, Tsim()), ymin=~low_belief, ymax=~up_belief,
                              name="User's optimal future trajectory b(extant) 95%", 
                              line=list(color="green", opacity=0.4, width=0),
                              data = data_sim(), yaxis = "y2", visible = TRUE,
                              fillcolor  ="rgba(0,255,0,0.2)", showlegend = FALSE) %>%
          #current belief
          plotly::add_trace(x=xtime, y=extant,  name = "User's current b(extant)",
                            visible = TRUE,  yaxis = "y2",
                            type = 'scatter', mode = 'lines+markers', 
                            showlegend = TRUE, line = list(color = "red"),
                            marker =list(color = "red"), colors = "red") %>%
          #simulations from reference point
          plotly::add_trace(x=seq(-Tmanage(), Tsim()), y=~mean_belief, name = "Optimal trajectory b(extant)",
                            data=data_sim_ref(), yaxis = "y2", 
                            type = 'scatter', mode = 'lines+markers',
                            visible = TRUE, line = list(color = "blue"),
                            marker =list(color = "blue"), colors="blue") %>%
          plotly::add_ribbons(x=seq(-Tmanage(), Tsim()), ymin=~low_belief, ymax=~up_belief,
                              name="Optimal trajectory b(extant) 95%", line=list(color="blue",
                                                                                 opacity=0.4, width=0),
                              data = data_sim_ref(), yaxis = "y2",
                              visible = TRUE, showlegend = FALSE, fillcolor ="rgba(0,0,255,0.2)") %>%
          plotly::layout( xaxis = xaxis, yaxis=yaxis,
                          yaxis2 = ay,  legend = list(orientation = 'h', 
                                                      # y = -0.3, 
                                                      x=0)
                          , margin = list(l=100, r=100)
          )
        
      } else {
        plotly::plot_ly(
          height = 300, source = "O") %>%
          plotly::add_trace(x=~step-Tmanage()-1, y=~obs, hoverinfo="text",text=~obs, 
                            data = data_observation_reactive(), type = 'scatter',
                            mode = 'markers',
                            marker = list(size = 20, opacity = 0.8) ,
                            color= ~color,
                            colors = colors_palette,
                            showlegend = FALSE) %>%
          #current belief
          plotly::add_trace(x=xtime, y=extant,  name = "User's current b(extant)",
                            visible = TRUE,  yaxis = "y2",
                            type = 'scatter', mode = 'lines+markers', 
                            showlegend = TRUE, line = list(color = "red"),
                            marker =list(color = "red"), colors = "red"
          ) %>%
          #simulations from reference point
          plotly::add_trace(x=seq(-Tmanage(), Tsim()), y=~mean_belief, name = "Optimal trajectory b(extant)",
                            data=data_sim_ref(), yaxis = "y2", 
                            type = 'scatter', mode = 'lines+markers',
                            visible = TRUE, line = list(color = "blue"),
                            marker =list(color = "blue"), colors = "blue") %>%
          plotly::add_ribbons(x=seq(-Tmanage(), Tsim()), ymin=~low_belief, ymax=~up_belief,
                              name="Optimal trajectory b(extant) 95%", line=list(color="blue",
                                                                                 opacity=0.4, width=0),
                              data = data_sim_ref(), yaxis = "y2", fillcolor = "rgba(0,0,255,0.2)",
                              visible = TRUE, showlegend = FALSE) %>%
          plotly::layout( xaxis = xaxis, yaxis=yaxis,
                          yaxis2 = ay,  legend = list(orientation = 'h',
                                                      # y = -0.3,
                                                      x=0)
                          , margin = list(l=100, r=100)
          )
      }
      
    } else {
      plotly::plot_ly(
        height = 300, source = "O") %>%
        #simulations from reference point
        plotly::add_trace(x=seq(-Tmanage(), Tsim()), y=~mean_belief, name = "Optimal trajectory b(extant)",
                          data=data_sim_ref(), yaxis = "y2", 
                          type = 'scatter', mode = 'lines+markers',
                          visible = TRUE, line = list(color = "blue"),
                          marker =list(color = "blue"), colors="blue") %>%
        plotly::add_ribbons(x=seq(-Tmanage(), Tsim()), ymin=~low_belief, ymax=~up_belief,
                            name="Optimal trajectory b(extant) 95%", line=list(color="blue",
                                                                               opacity=0.4, width=0),
                            data = data_sim_ref(), yaxis = "y2",
                            visible = TRUE, showlegend = FALSE, fillcolor ="rgba(0,0,255,0.2)") %>%
        plotly::layout( xaxis = xaxis, yaxis=yaxis,
                        yaxis2 = ay,  legend = list(orientation = 'h',
                                                    # y = -0.3, 
                                                    x=0)
                        , margin = list(l=100, r=100)
        )
    }
    
  })
  
  # rewards plot ####
  output$plot_reward <- plotly::renderPlotly({
    if (Tmanage()>0){
      if (length(rewards())>=1){
        xtime <- seq(1,length(rewards()))-Tmanage()-1
        
        xaxis <- list(
          autotick = FALSE,
          title = "Time steps",
          ticks = "outside",
          dtick = 1,
          range = c(-Tmanage()-1, Tplot() + 2)
        )
        yaxis <-list(
          title = "Expected reward"
        )
        if ((length(observations_past()) == length(actions_past())) 
            & (length(observations_past()) == Tmanage())){
          plotly::plot_ly(height = 300, source = "R") %>%
            #simulations from present
            plotly::add_lines(x=seq(0, Tsim()), y=~mean_reward,
                              data=data_sim(), name = "User's optimal future expected reward",
                              visible = TRUE, line = list(color = "green"),
                              marker =list(color = "green"), colors="green") %>%
            plotly::add_ribbons(x=seq(0, Tsim()), ymin=~low_reward, ymax=~up_reward,
                                name="Reward 95%", data = data_sim(),
                                line=list(color="green", opacity=0.4, width=0),
                                visible = TRUE, showlegend = FALSE, 
                                fillcolor ="rgba(0,255,0,0.2)") %>%
            #current instant reward
            plotly::add_trace(x=c(xtime, 0), y=c(rewards(), data_sim()$mean_reward[1]),  
                              name = "User's current expected reward",
                              visible = TRUE, type = 'scatter',
                              mode = 'lines+markers',
                              showlegend = TRUE, line = list(color = "red"),
                              marker =list(color = "red"), colors = "red") %>%
            #simulations from reference point
            plotly::add_lines(x=seq(-Tmanage(), Tsim()), y=~mean_reward,
                              data=data_sim_ref(), name = "Optimal expected reward",
                              line = list(color = "blue"),
                              marker =list(color = "blue"), colors="blue") %>%
            plotly::add_ribbons(x=seq(-Tmanage(), Tsim()), ymin=~low_reward, ymax=~up_reward,
                                name="Ref reward 95%", data = data_sim_ref(), 
                                line=list(color="blue", opacity=0.4, width=0),
                                visible = TRUE, showlegend = FALSE, 
                                fillcolor ="rgba(0,0,255,0.2)") %>%
            plotly::layout( xaxis = xaxis, yaxis=yaxis,
                            showlegend = TRUE, legend = list(orientation = 'h'
                                                             #, y = -0.4
                            )
                            , margin = list(l=100, r=100)
            )
          
        } else {
          plotly::plot_ly(height = 300, source = "R") %>%
            #current instant reward
            plotly::add_trace(x=xtime, y=rewards(),  name = "User's current expected reward",
                              visible = TRUE, type = 'scatter',
                              mode = 'lines+markers',
                              showlegend = TRUE, line = list(color = "red"),
                              marker =list(color = "red"), colors = "red") %>%
            #simulations from reference point
            plotly::add_lines(x=seq(-Tmanage(), Tsim()), y=~mean_reward,
                              data=data_sim_ref(), name = "Optimal expected reward",
                              line = list(color = "blue"),
                              marker =list(color = "blue"), colors="blue") %>%
            plotly::add_ribbons(x=seq(-Tmanage(), Tsim()), ymin=~low_reward, ymax=~up_reward,
                                name="Ref reward 95%", data = data_sim_ref(), 
                                line=list(color="blue", opacity=0.4, width=0),
                                visible = TRUE, showlegend = FALSE, 
                                fillcolor ="rgba(0,0,255,0.2)") %>%
            plotly::layout( xaxis = xaxis, yaxis=yaxis,
                            showlegend = TRUE, legend = list(orientation = 'h'
                                                             #, y = -0.4
                            )
                            , margin = list(l=100, r=100))
        }
      } else {
        xaxis <- list(
          autotick = FALSE,
          title = "Time steps",
          ticks = "outside",
          dtick = 1,
          range = c(-Tmanage()-1, Tplot() + 2)
        )
        yaxis <-list(
          title = "Expected reward"
        )
        plotly::plot_ly(height = 300, source = "R") %>%
          #simulations from reference point
          plotly::add_lines(x=seq(-Tmanage(), Tsim()), y=~mean_reward,
                            data=data_sim_ref(), name = "Optimal expected reward",
                            line = list(color = "blue"),
                            marker =list(color = "blue"), colors="blue") %>%
          plotly::add_ribbons(x=seq(-Tmanage(), Tsim()), ymin=~low_reward, ymax=~up_reward,
                              name="Ref reward 95%", data = data_sim_ref(), 
                              line=list(color="blue", opacity=0.4, width=0),
                              visible = TRUE, showlegend = FALSE, 
                              fillcolor ="rgba(0,0,255,0.2)") %>%
          plotly::layout( xaxis = xaxis, yaxis=yaxis,
                          showlegend = TRUE, legend = list(orientation = 'h'
                                                           #, y = -0.4
                          )
                          , margin = list(l=100, r=100)
          )
        
      }
    } else {
      xaxis <- list(
        autotick = FALSE,
        title = "Time steps",
        ticks = "outside",
        dtick = 1,
        range = c(-Tmanage()-1, Tplot() + 2)
      )
      yaxis <-list(
        title = "Expected reward"
      )
      plotly::plot_ly(height = 300, source = "R") %>%
        #simulations from reference point
        plotly::add_lines(x=seq(-Tmanage(), Tsim()), y=~mean_reward,
                          data=data_sim_ref(), name = "Optimal expected reward",
                          line = list(color = "blue"),
                          marker =list(color = "blue"), colors="blue") %>%
        plotly::add_ribbons(x=seq(-Tmanage(), Tsim()), ymin=~low_reward, ymax=~up_reward,
                            name="Ref reward 95%", data = data_sim_ref(), 
                            line=list(color="blue", opacity=0.4, width=0),
                            visible = TRUE, showlegend = FALSE, 
                            fillcolor ="rgba(0,0,255,0.2)") %>%
        plotly::layout( xaxis = xaxis, yaxis=yaxis,
                        showlegend = TRUE, legend = list(orientation = 'h'
                                                         #, y = -0.4
                        )
                        , margin = list(l=100, r=100)
        )
      
    }
  })
  
  # reload values depending on the case of study ################
  shiny::observeEvent(input$reload, {
    if (input$case_study == "Sumatran tiger"){
      shiny::updateNumericInput(session, 'p0', value = 0.9)
      shiny::updateNumericInput(session, 'pm', value = 0.94184)
      
      shiny::updateNumericInput(session, 'd0', value = 0.01)
      shiny::updateNumericInput(session, 'dm', value = 0.01)
      shiny::updateNumericInput(session, 'ds', value = 0.78193)
      
      shiny::updateNumericInput(session, 'V', value = 175.133)
      shiny::updateNumericInput(session, 'Cm', value = 18.784)
      shiny::updateNumericInput(session, 'Cs', value = 10.840)
      
    } else if (input$case_study == "Expensive management"){
      shiny::updateNumericInput(session, 'p0', value = 0.9)
      shiny::updateNumericInput(session, 'pm', value = 0.94184)
      
      shiny::updateNumericInput(session, 'd0', value = 0.01)
      shiny::updateNumericInput(session, 'dm', value = 0.01)
      shiny::updateNumericInput(session, 'ds', value = 0.78193)
      
      shiny::updateNumericInput(session, 'V', value = 200)
      shiny::updateNumericInput(session, 'Cm', value = 50)
      shiny::updateNumericInput(session, 'Cs', value = 25)
      
    } else if (input$case_study == "Detection in management"){
      shiny::updateNumericInput(session, 'p0', value = 0.9)
      shiny::updateNumericInput(session, 'pm', value = 0.94184)
      
      shiny::updateNumericInput(session, 'd0', value = 0.01)
      shiny::updateNumericInput(session, 'dm', value = 0.5)
      shiny::updateNumericInput(session, 'ds', value = 0.78193)
      
      shiny::updateNumericInput(session, 'V', value = 175.133)
      shiny::updateNumericInput(session, 'Cm', value = 18.784)
      shiny::updateNumericInput(session, 'Cs', value = 10.840)
      
    }
  })
  
}

shiny::shinyApp(ui, server)