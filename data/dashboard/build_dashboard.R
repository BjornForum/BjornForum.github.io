# Builds the Shinylive code block for the dashboard page (data/dashboard.qmd).
#
# Quarto runs this script automatically before every render or preview of the
# website (see `pre-render` in _quarto.yml). It combines data/dashboard/app.R
# with the four datasets in data/ and writes data/dashboard/_shinylive_app.md,
# which dashboard.qmd includes. Because the page itself contains no R code,
# Quarto never reuses an old copy: changes to app.R or to the data files always
# show up on the next render.

data_dir  <- "data"
app_file  <- file.path(data_dir, "dashboard", "app.R")
out_file  <- file.path(data_dir, "dashboard", "_shinylive_app.md")
datasets  <- c("TCS_NOR.RDS", "Ministers_NOR.RDS", "POLADV_NOR.RDS", "Governments_NOR.RDS")

app <- readLines(app_file, encoding = "UTF-8", warn = FALSE)
embedded <- vapply(datasets, function(f) {
  paste0("## file: ", f, "\n## type: binary\n", xfun::base64_encode(file.path(data_dir, f)))
}, character(1))

block <- paste(c("```{shinylive-r}",
                 "#| standalone: true",
                 "#| components: [viewer]",
                 "#| viewerHeight: 1000",
                 "## file: app.R",
                 app,
                 embedded,
                 "```"), collapse = "\n")
block <- enc2utf8(block)

# Only write when something changed, so a running preview is not triggered needlessly
old <- if (file.exists(out_file)) paste(readLines(out_file, encoding = "UTF-8", warn = FALSE), collapse = "\n") else ""
if (!identical(block, old)) {
  con <- file(out_file, open = "wb")
  writeBin(charToRaw(paste0(block, "\n")), con)
  close(con)
  message("Dashboard: updated ", out_file)
}
