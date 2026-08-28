## #####################################################################
## BUSN 3000H  —  Table Toolkit
## Presentation-quality statistical tables on a single gt foundation.
##
## Functions
##   freq.table()  one-way frequency table
##   c.table()     two-/three-way contingency table (PROC FREQ style)
##   q.table()     univariate descriptive statistics (econ / percentile / full)
##   png.print()   save any of the above tables, or the current plot, to a PNG
##
## Common export flow:
##   t1 <- freq.table(...) ;  png.print(t1)   # table -> t1.png
##   barplot(...)          ;  png.print()     # plot  -> plot.png
##
## Dependencies: gt, dplyr   (webshot2 required for gtsave() PNG export)
##   install.packages(c("gt", "dplyr", "webshot2"))
## #####################################################################

## Load required packages quietly, so a fresh source() prints nothing:
## no "masked from 'package:stats'" notes, no "built under R version"
## warning. (A genuinely missing package still errors, as it should.)
suppressWarnings(suppressPackageStartupMessages({
  library(gt)
  library(dplyr)
}))


## =====================================================================
## freq.table()  —  one-way frequency table, gt foundation
##
##   x         a vector / data-frame column to tabulate (e.g. d1$REGION)
##   main      optional title; NULL (default) prints no title block, like plot()
##   varname   heading for the first (stub) column. Defaults to the variable
##             name derived from x (d1$REGION -> "REGION"). Override when x has
##             been reassigned (e.g. x <- d1$REGION), which would otherwise
##             read as "x".
##
## Columns: Frequency, Percent, Cumulative Frequency, Cumulative Percent
##
## Same flow as c.table() / q.table():
##   gtsave(freq.table(d1$REGION, main = "Frequency of Region"),
##          "region.png", zoom = 2)
##
## Requires: gt, dplyr
## =====================================================================

freq.table <- function(x, main = NULL, varname = NULL) {

  # default: capture a clean variable name the way plot() does
  if (is.null(varname)) {
    varname <- deparse(substitute(x))
    varname <- sub(".*\\$", "", varname)     # d1$Region  ->  Region
  }

  counts <- table(x)

  tab <- tibble(
    level                   = names(counts),
    Frequency               = as.integer(counts),
    Percent                 = 100 * Frequency / sum(Frequency),
    `Cumulative Frequency`  = cumsum(Frequency),
    `Cumulative Percent`    = cumsum(Percent)
  )

  g <- tab %>%
    gt(rowname_col = "level") %>%
    tab_stubhead(label = varname) %>%
    fmt_number(columns = c(Frequency, `Cumulative Frequency`), decimals = 0) %>%
    fmt_number(columns = c(Percent,   `Cumulative Percent`),   decimals = 2) %>%
    cols_align(align   = "right",
               columns = c(Frequency, Percent,
                           `Cumulative Frequency`, `Cumulative Percent`))

  if (!is.null(main)) g <- tab_header(g, title = main)

  g
}


## =====================================================================
## c.table()  —  two- and three-way contingency table, gt foundation
##              (SAS PROC FREQ style: stacked cells with marginal totals;
##               a 3-way table renders as stratified 2-way panels, one per
##               level of the strata variable)
##
##   row, col     the two categorical variables to cross
##                (e.g. d1$REGION, d1$COMPET)
##   strata       optional categorical variable; when supplied, one 2-way panel
##                is produced per level (the SAS A*B*C layout). NULL (default)
##                gives a single 2-way table.
##   main         optional title; NULL (default) prints no title block
##   row.pct      TRUE adds each cell's row percentage         (default FALSE)
##   col.pct      TRUE adds each cell's column percentage      (default FALSE)
##   total.pct    TRUE adds each cell's percent of grand total (default FALSE)
##   rowname      heading for the row variable;    defaults to the derived name
##   colname      heading for the column variable; defaults to the derived name
##   stratname    label used for the strata panels; defaults to the derived name
##
## Cell contents, in SAS order: Frequency, then Total / Row / Col Pct for
## whichever toggles are on. Margins carry Frequency + Total Pct only. A legend
## of the active cell contents is printed as a source note.
##
## Note: row and col must be categorical. A continuous variable produces one
## row/column per distinct value; bin or factor() it first.
##
## Same flow as freq.table() / q.table():
##   gtsave(c.table(d1$REGION, d1$COMPET, main = "Region by Competition"),
##          "region_compet.png", zoom = 2)
##
## Requires: gt
## =====================================================================

c.table <- function(row, col, strata = NULL,
                    main = NULL,
                    row.pct = FALSE, col.pct = FALSE, total.pct = FALSE,
                    rowname = NULL, colname = NULL, stratname = NULL) {

  rname <- if (!is.null(rowname))   rowname   else sub(".*\\$", "", deparse(substitute(row)))
  cname <- if (!is.null(colname))   colname   else sub(".*\\$", "", deparse(substitute(col)))
  sname <- if (!is.null(stratname)) stratname else sub(".*\\$", "", deparse(substitute(strata)))

  # one stacked cell string, in SAS order, honoring the toggles
  fmt_cell <- function(freq, tp = NA, rp = NA, cp = NA) {
    parts <- format(round(freq), big.mark = ",", trim = TRUE)
    if (total.pct && !is.na(tp)) parts <- c(parts, sprintf("%.2f", tp))
    if (row.pct   && !is.na(rp)) parts <- c(parts, sprintf("%.2f", rp))
    if (col.pct   && !is.na(cp)) parts <- c(parts, sprintf("%.2f", cp))
    paste(parts, collapse = "<br>")
  }

  # build one 2-way panel (row x col) with margins
  panel_df <- function(r, c) {
    tab <- table(r, c)
    rl <- rownames(tab); cl <- colnames(tab)
    R <- length(rl);     C <- length(cl)
    rowtot <- rowSums(tab); coltot <- colSums(tab); grand <- sum(tab)

    disp <- matrix("", R + 1, C + 1)
    for (i in seq_len(R)) for (j in seq_len(C)) {
      disp[i, j] <- fmt_cell(tab[i, j],
                             tp = tab[i, j] / grand     * 100,
                             rp = tab[i, j] / rowtot[i] * 100,
                             cp = tab[i, j] / coltot[j] * 100)
    }
    for (i in seq_len(R)) disp[i, C + 1] <- fmt_cell(rowtot[i], tp = rowtot[i] / grand * 100)
    for (j in seq_len(C)) disp[R + 1, j] <- fmt_cell(coltot[j], tp = coltot[j] / grand * 100)
    disp[R + 1, C + 1] <- fmt_cell(grand, tp = 100)

    out <- data.frame(.rowlab = c(rl, "Total"), check.names = FALSE, stringsAsFactors = FALSE)
    for (j in seq_len(C)) out[[cl[j]]] <- disp[, j]
    out[["Total"]] <- disp[, C + 1]
    list(df = out, col_ids = c(cl, "Total"))
  }

  # 2-way, or stratified 3-way
  if (is.null(strata)) {
    p <- panel_df(row, col); dat <- p$df; col_ids <- p$col_ids; grouped <- FALSE
  } else {
    lv <- levels(as.factor(strata)); pieces <- list(); col_ids <- NULL
    for (l in lv) {
      idx <- which(as.character(strata) == l)
      p <- panel_df(row[idx], col[idx]); d <- p$df
      d$.strata <- paste0(sname, " = ", l)
      pieces[[length(pieces) + 1]] <- d; col_ids <- p$col_ids
    }
    dat <- do.call(rbind, pieces); grouped <- TRUE
  }

  g <- gt(dat,
          rowname_col   = ".rowlab",
          groupname_col = if (grouped) ".strata" else NULL) |>
    fmt_markdown(columns = col_ids) |>
    tab_spanner(label = cname, columns = setdiff(col_ids, "Total")) |>
    tab_stubhead(label = rname) |>
    cols_align(align = "center", columns = col_ids) |>
    tab_style(style = cell_borders(sides = "left", color = "#BBBBBB", weight = px(1)),
              locations = cells_body(columns = "Total"))

  leg <- "Frequency"
  if (total.pct) leg <- paste0(leg, " \u00b7 Total Pct")
  if (row.pct)   leg <- paste0(leg, " \u00b7 Row Pct")
  if (col.pct)   leg <- paste0(leg, " \u00b7 Col Pct")
  g <- tab_source_note(g, source_note = paste0("Cell contents: ", leg))

  if (!is.null(main)) g <- tab_header(g, title = main)
  g
}


## =====================================================================
## q.table()  —  univariate descriptive statistics, gt foundation
## v2   (rebuilt on gt to match freq.table() and c.table())
##
##   type = "econ"        (default)  N, Mean, Std Dev, Min, Max
##   type = "percentile"             N, Mean, Std Dev, 10th ... 90th
##   type = "full"                   N, Mean, Std Dev, Min, Q1, Median, Q3, Max
##
## Same flow as freq.table() / c.table():
##   gtsave(q.table(d1, ...), "summary.png", zoom = 2)
##
## Requires: gt   (plus webshot2 for PNG export, as freq.table already needs)
## =====================================================================

library(gt)

q.table <- function(data,
                    vars    = NULL,
                    labels  = NULL,
                    type    = "econ",
                    headers = "long",
                    digits  = 2,
                    main    = NULL) {

  ## ---- input validation ---------------------------------------------
  if (!is.data.frame(data))
    stop("data must be a data frame.")

  type    <- match.arg(type,    c("econ", "percentile", "full"))
  headers <- match.arg(headers, c("long", "short"))

  if (!is.null(labels) && is.null(vars))
    stop("The labels argument is only available when the user specifies a list of variables using the vars argument.")

  if (is.null(vars)) {
    vars <- names(data)[vapply(data, is.numeric, logical(1))]
    if (length(vars) == 0L)
      stop("No numeric variables found in data.")
  }

  absent <- setdiff(vars, names(data))
  if (length(absent) > 0L)
    stop(sprintf("Variable(s) not found in data: %s",
                 paste(absent, collapse = ", ")))

  not_num <- vars[!vapply(data[vars], is.numeric, logical(1))]
  if (length(not_num) > 0L)
    stop(sprintf("q.table() requires numeric variables. Non-numeric: %s",
                 paste(not_num, collapse = ", ")))

  if (!is.null(labels)) {
    if (length(labels) != length(vars))
      stop(sprintf("labels has %d entr%s but vars has %d.",
                   length(labels),
                   if (length(labels) == 1L) "y" else "ies",
                   length(vars)))
    row_names <- labels
  } else {
    row_names <- vars
  }

  ## ---- statistics (quantile type = 7, R's native default) -----------
  qtype <- 7
  stat_row <- function(x) {
    x <- x[!is.na(x)]
    n <- length(x); m <- mean(x); s <- stats::sd(x)
    switch(type,
      econ = c(N = n, Mean = m, SD = s, Min = min(x), Max = max(x)),
      percentile = {
        q <- stats::quantile(x, c(.10, .25, .50, .75, .90),
                             type = qtype, names = FALSE)
        c(N = n, Mean = m, SD = s,
          p10 = q[1], p25 = q[2], p50 = q[3], p75 = q[4], p90 = q[5])
      },
      full = {
        q <- stats::quantile(x, c(.25, .50, .75), type = qtype, names = FALSE)
        c(N = n, Mean = m, SD = s,
          Min = min(x), Q1 = q[1], Med = q[2], Q3 = q[3], Max = max(x))
      })
  }

  ncols <- switch(type, econ = 5L, percentile = 8L, full = 8L)
  mat   <- t(vapply(data[vars], stat_row, numeric(ncols)))

  df <- data.frame(Variable = row_names, mat,
                   check.names = FALSE, stringsAsFactors = FALSE,
                   row.names = NULL)

  ## ---- display labels for the stat columns (long vs short) ----------
  lab <- switch(type,
    econ = if (headers == "long")
        c(N = "N", Mean = "Mean", SD = "Std Dev",
          Min = "Minimum", Max = "Maximum")
      else
        c(N = "N", Mean = "Mean", SD = "SD", Min = "Min", Max = "Max"),

    percentile = if (headers == "long")
        c(N = "N", Mean = "Mean", SD = "Std Dev",
          p10 = "10th", p25 = "25th", p50 = "50th", p75 = "75th", p90 = "90th")
      else
        c(N = "N", Mean = "Mean", SD = "SD",
          p10 = "p10", p25 = "p25", p50 = "p50", p75 = "p75", p90 = "p90"),

    full = if (headers == "long")
        c(N = "N", Mean = "Mean", SD = "Std Dev",
          Min = "Minimum", Q1 = "First Quartile", Med = "Median",
          Q3 = "Third Quartile", Max = "Maximum")
      else
        c(N = "N", Mean = "Mean", SD = "SD",
          Min = "Min", Q1 = "Q1", Med = "Median", Q3 = "Q3", Max = "Max")
  )

  num_cols  <- setdiff(names(df), "Variable")   # N + all stat cols
  stat_cols <- setdiff(num_cols, "N")           # everything formatted to `digits`

  ## ---- build the gt table -------------------------------------------
  g <- gt(df, rowname_col = "Variable")
  g <- tab_stubhead(g, label = "Variable")
  g <- do.call(cols_label, c(list(g), as.list(lab)))

  g <- fmt_number(g, columns = stat_cols, decimals = digits)   # thousands seps on by default
  g <- fmt_number(g, columns = "N",       decimals = 0)

  ## "Percentile" spanner over the five percentile columns (long headers only)
  if (type == "percentile" && headers == "long")
    g <- tab_spanner(g, label = "Percentile",
                     columns = c("p10", "p25", "p50", "p75", "p90"))

  g <- cols_align(g, align = "right", columns = num_cols)

  if (!is.null(main)) g <- tab_header(g, title = main)

  g
}


## =====================================================================
## png.print()  —  save a toolkit table, or the latest base-R plot, to PNG
##
## Two ways to call it, reflecting a real difference in R:
##
##   TABLES are objects, so you pass the object:
##     t1 <- freq.table(...) ;  png.print(t1)      # writes t1.png
##     c1 <- c.table(...)    ;  png.print(c1)      # writes c1.png
##     q1 <- q.table(...)    ;  png.print(q1)      # writes q1.png
##
##   BASE-R PLOTS are NOT objects (barplot / hist / boxplot / plot draw to
##   the screen and return nothing useful), so you pass NOTHING --
##   png.print() snapshots whatever plot is currently in the Plots pane:
##     barplot(table(d1$q05_class_standing), main = "Class Standing")
##     png.print()                                 # writes plot.png
##
##   You may still pass a name just to set the filename; for a plot its
##   VALUE is ignored and only its name is used:
##     p1 <- barplot(...) ;  png.print(p1)         # writes p1.png
##
## Arguments:
##   x       a gt table to save; OR (for plots) nothing, a name used only for
##           the filename, or a filename string in quotes
##   file    output filename; default is the object's name + ".png", or
##           "table.png" / "plot.png" when no usable name is available. A
##           missing ".png" extension is added automatically.
##   zoom    tables only: gtsave() resolution multiplier (default 2)
##   width, height, res
##           plots only: pixel size and resolution of the PNG
##
## Filenames:
##   png.print(t1)                 -> t1.png        (object name)
##   png.print(t1, "freq.png")     -> freq.png
##   png.print()                   -> plot.png      (default; overwrites!)
##   png.print("age_hist.png")     -> age_hist.png
##   png.print("age_hist")         -> age_hist.png  (extension added)
##   png.print(p1)                 -> p1.png        (name; value ignored)
##
## NOTE: repeated png.print() calls all write plot.png and overwrite each
## other -- give each plot its own name if you want to keep them all.
##
## Call png.print() for a plot immediately after drawing it -- it captures
## the CURRENT plot, so anything drawn in between would be saved instead.
##
## Requires: gt   (plus webshot2 for PNG export of tables)
## =====================================================================

png.print <- function(x, file = NULL, zoom = 2,
                      width = 1200, height = 800, res = 150) {

  supplied <- !missing(x)
  xexpr    <- if (supplied) substitute(x) else NULL

  # a bare string as the first argument IS the filename (handy for plots)
  if (supplied && is.character(x) && length(x) == 1L) {
    if (is.null(file)) file <- x
    supplied <- FALSE
  }

  is_table <- supplied && inherits(x, "gt_tbl")

  if (is.null(file)) {                                   # derive from object name
    nm <- if (supplied) deparse(xexpr) else NULL
    if (!is.null(nm) && !grepl("^[A-Za-z.][A-Za-z0-9._]*$", nm)) nm <- NULL
    file <- if (!is.null(nm)) paste0(nm, ".png")
            else if (is_table) "table.png" else "plot.png"
  }
  if (!grepl("\\.[A-Za-z0-9]+$", file)) file <- paste0(file, ".png")  # ensure extension

  if (is_table) {
    gtsave(x, file, zoom = zoom)                                    # table object
  } else {
    dev.copy(png, file, width = width, height = height, res = res)  # current plot
    dev.off()
  }
  invisible(if (is_table) x else NULL)
}
