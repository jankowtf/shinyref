
# Example 1 ---------------------------------------------------------------

mapDirectory("R")
mapDirectory("R", include_root = FALSE)
mapDirectory("R", include_files = FALSE)


# Example 2 ---------------------------------------------------------------

tmp <- tempdir()
paths <- file.path(tmp, c("a/b/c", "b"))
sapply(paths, dir.create, recursive = TRUE, showWarnings = FALSE)
file.create(file.path(tmp, c("a/a_1.txt", "a/a_2.txt")))
file.create(file.path(tmp, c("a/b/c/c_1.txt", "a/b/c/c_2.txt")))

mapDirectory(dir = file.path(tmp, "a"))
mapDirectory(dir = file.path(tmp, "a"), include_root = FALSE)
mapDirectory(dir = file.path(tmp, "a"), include_files = FALSE)

mapDirectory(dir = tmp)


# Example 3 ---------------------------------------------------------------

mapDirectory(dir = tmp, relative = TRUE)
mapDirectory(dir = tmp, include_root = FALSE, relative = TRUE)
