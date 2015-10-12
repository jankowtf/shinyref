
# Defaults ----------------------------------------------------------------

.DEFAULTS <- list(
  namespaces = list(
    values = c("Enbw", "EnBWLongterm", "Npower")
  )
)

CTRL <- AppController$new(
  # app_class = structure(list(), class = c("Enbw.Variants", "list"))
  app_class = structure(list(), class = c("EnbwLongterm.Variants", "list"))
)
GLOBALS <- CTRL$createGlobals()

patterns <- list(
  gsub_project = "RSG\\d\\d_.*_original_parallel_"
)
