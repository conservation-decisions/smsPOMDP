library(magrittr)
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

modal_gif <-
  bsplus::bs_modal(
    id = "modal_gif",
    title = "Help to select actions and observations",
    body = shiny::includeMarkdown(system.file("markdown", "modal_gif.md", package = "smsPOMDP")),
    size = "medium"
  )
