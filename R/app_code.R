#' @export
createListIndex <- function(x, sep = c("[['", "']][['", "']]")) {
  paste0(sep[1], paste(x, collapse = sep[2]), sep[3])
}

#' @export
mapDirectoryStructureToList <- function(
  dir,
  include_files = TRUE,
  drop_root = FALSE
) {
  if (length(dir) > 1) {
    stop("mapDirectoryStructureToList: only allowed for atomic directory input")
  }
  dirs <- list.dirs(dir, full.names = TRUE, recursive = TRUE)
  files <- list.files(dir, full.names = TRUE, recursive = TRUE)
  files_dirs <- unique(dirname(files))
#   if (drop_root) {
#     pattern <- paste0(dir, "/?")
#     dirs <- gsub(pattern, "", dirs)
# #     files <- gsub(pattern, "", files)
# #     files_dirs <- gsub(pattern, "", files_dirs)
#   }
  dirs <- dirs[dirs != ""]
  files <- files[files != ""]
  files_dirs <- files_dirs[files_dirs != ""]

  splitted <- strsplit(dirs, "/")
  #   level_1=splitted[[2]]
  #   level_2=2
  out <- list()
  for(level_1 in splitted) {
    for(level_2 in 1:length(level_1)) {
      idx <- createListIndex(level_1[1:level_2])
      path <- createListIndex(level_1[1:level_2], sep = c("", "/", ""))
      expr_get <- sprintf("%s%s", "out", idx)
      expr_set <- if (path %in% files_dirs && include_files) {
        sprintf("%s%s <- as.list(list.files(path))", "out", idx)
      } else {
        sprintf("%s%s <- list()", "out", idx)
      }
      tmp <- eval(parse(text = expr_get))
      if (is.null(tmp)) {
        eval(parse(text = expr_set))
      }
    }
  }
  if (drop_root) {
    out <- out[[1]]
  }
  out
}
