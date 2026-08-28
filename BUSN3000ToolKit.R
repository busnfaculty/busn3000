## #####################################################################
## BUSN 3000H  —  Table Toolkit   (pure base R, ZERO packages)
##
## Tables are drawn with base graphics (text/segments) straight onto R's
## png() device -- the SAME machinery bar charts use. Nothing to install:
## no gt, no flextable, no dplyr, no webshot2, no Chrome.
##
## Functions
##   freq.table()  one-way frequency table
##   c.table()     two-/three-way contingency table (PROC FREQ style)
##   q.table()     univariate descriptive statistics (econ / percentile / full)
##   png.print()   save any of the above tables, or the current plot, to a PNG
##
## Export flow:
##   t1 <- freq.table(...) ;  png.print(t1)   # table -> t1.png
##   barplot(...)          ;  png.print()     # plot  -> plot.png
## #####################################################################


## =====================================================================
## Rendering engine (internal) — draws a table spec with base graphics
## =====================================================================

.btable_style <- list(
  title_cex = 1.30, head_cex = 1.00, body_cex = 1.00, foot_cex = 0.85,
  cell_pad_x = 0.12, row_pad_y = 0.07, line_mult = 1.35, margin = 0.16,
  rule_thick = 2.0, rule_mid = 1.1, hair_col = "gray80", hair_lwd = 0.8
)

## place one text string in column j at the column's alignment
.btext <- function(cx, j, align, y, label, cex, pad) {
  if (align == "left")
    text(cx[j] + pad,     y, label, adj = c(0, 0.5),   cex = cex)
  else if (align == "right")
    text(cx[j + 1] - pad, y, label, adj = c(1, 0.5),   cex = cex)
  else
    text((cx[j] + cx[j + 1]) / 2, y, label, adj = c(0.5, 0.5), cex = cex)
}

## compute column widths and row heights (inches); needs an active device
.btable_layout <- function(spec) {
  st   <- .btable_style
  ncol <- length(spec$colkeys)
  lh   <- strheight("Ag", units = "inches", cex = st$body_cex) * st$line_mult

  ## base column widths from header + body cells
  colw <- numeric(ncol)
  for (j in seq_len(ncol)) {
    w <- strwidth(spec$header[j], units = "inches", cex = st$head_cex)
    for (r in spec$body) {
      if (identical(r$type, "data")) {
        lines <- strsplit(r$cells[j], "\n", fixed = TRUE)[[1]]
        if (!length(lines)) lines <- ""
        w <- max(w, max(strwidth(lines, units = "inches", cex = st$body_cex)))
      }
    }
    colw[j] <- w + 2 * st$cell_pad_x
  }

  ## widen a spanner's columns if its label is wider than they are
  for (sp in spec$spanners) {
    need <- strwidth(sp$label, units = "inches", cex = st$head_cex) + 2 * st$cell_pad_x
    have <- sum(colw[sp$cols])
    if (need > have) colw[sp$cols] <- colw[sp$cols] + (need - have) / length(sp$cols)
  }

  total_w <- sum(colw)

  ## make sure title / footnote / group labels fit
  extras <- 0
  if (!is.null(spec$title))    extras <- max(extras, strwidth(spec$title,    units = "inches", cex = st$title_cex))
  if (!is.null(spec$footnote)) extras <- max(extras, strwidth(spec$footnote, units = "inches", cex = st$foot_cex))
  for (r in spec$body)
    if (identical(r$type, "group"))
      extras <- max(extras, strwidth(r$label, units = "inches", cex = st$body_cex))
  extras <- extras + 2 * st$cell_pad_x
  if (total_w < extras) { colw <- colw * (extras / total_w); total_w <- extras }

  ## heights
  has_span <- length(spec$spanners) > 0
  span_h   <- if (has_span) lh + 2 * st$row_pad_y else 0
  head_h   <- lh + 2 * st$row_pad_y
  title_h  <- if (!is.null(spec$title))    strheight(spec$title,    units = "inches", cex = st$title_cex) + 2 * st$row_pad_y else 0
  foot_h   <- if (!is.null(spec$footnote)) strheight(spec$footnote, units = "inches", cex = st$foot_cex)  + 2 * st$row_pad_y else 0

  row_h <- numeric(length(spec$body))
  for (i in seq_along(spec$body)) {
    r <- spec$body[[i]]
    if (identical(r$type, "group")) {
      row_h[i] <- lh + 2 * st$row_pad_y
    } else {
      maxlines <- 1
      for (j in seq_len(ncol))
        maxlines <- max(maxlines, length(strsplit(r$cells[j], "\n", fixed = TRUE)[[1]]))
      row_h[i] <- maxlines * lh + 2 * st$row_pad_y
    }
  }

  body_h  <- sum(row_h)
  total_h <- title_h + span_h + head_h + body_h + foot_h
  list(colw = colw, total_w = total_w, lh = lh, span_h = span_h, head_h = head_h,
       title_h = title_h, foot_h = foot_h, row_h = row_h, total_h = total_h,
       has_span = has_span)
}

## draw the table on the current device, shrinking uniformly to fit if needed
.draw_btable <- function(spec) {
  st <- .btable_style
  op <- par(mar = c(0, 0, 0, 0), xpd = NA); on.exit(par(op))
  plot.new()
  din <- par("din")
  plot.window(xlim = c(0, din[1]), ylim = c(0, din[2]), xaxs = "i", yaxs = "i")

  L    <- .btable_layout(spec)             # natural sizes (inches) at reference cex
  ncol <- length(spec$colkeys)

  ## scale so the whole table fits the device; never enlarge past natural size.
  ## On a PNG sized to the table (png.print) this is 1; on a small Plots pane it shrinks.
  needW <- L$total_w + 2 * st$margin
  needH <- L$total_h + 2 * st$margin
  s <- min(1, din[1] / needW, din[2] / needH)

  colw    <- L$colw    * s; lh      <- L$lh      * s
  span_h  <- L$span_h  * s; head_h  <- L$head_h  * s
  title_h <- L$title_h * s; foot_h  <- L$foot_h  * s
  row_h   <- L$row_h   * s
  total_w <- L$total_w * s; total_h <- L$total_h * s
  padx <- st$cell_pad_x * s
  tcex <- st$title_cex * s; hcex <- st$head_cex * s
  bcex <- st$body_cex  * s; fcex <- st$foot_cex * s

  x_left <- (din[1] - total_w) / 2
  cx     <- x_left + c(0, cumsum(colw))
  x0 <- cx[1]; x1 <- cx[length(cx)]
  y  <- (din[2] + total_h) / 2

  if (!is.null(spec$title)) {
    text((x0 + x1) / 2, y - title_h / 2, spec$title, cex = tcex, adj = c(0.5, 0.5))
    y <- y - title_h
  }
  segments(x0, y, x1, y, lwd = st$rule_thick)
  header_top <- y

  span_cols <- integer(0)
  for (sp in spec$spanners) span_cols <- c(span_cols, sp$cols)

  if (span_h > 0) {
    yb <- y - span_h
    for (sp in spec$spanners) {
      idx <- sp$cols
      text((cx[min(idx)] + cx[max(idx) + 1]) / 2, y - span_h / 2, sp$label, cex = hcex, adj = c(0.5, 0.5))
      segments(cx[min(idx)] + padx, yb, cx[max(idx) + 1] - padx, yb, lwd = st$rule_mid)
    }
    y <- yb
  }

  full_top <- header_top; full_bot <- y - head_h
  for (j in seq_len(ncol)) {
    yc <- if (j %in% span_cols) y - head_h / 2 else (full_top + full_bot) / 2
    .btext(cx, j, spec$align[j], yc, spec$header[j], hcex, padx)
  }
  y <- y - head_h
  segments(x0, y, x1, y, lwd = st$rule_thick)
  body_top <- y

  for (i in seq_along(spec$body)) {
    r  <- spec$body[[i]]; rh <- row_h[i]; ym <- y - rh / 2
    if (identical(r$type, "group")) {
      text(cx[1] + padx, ym, r$label, adj = c(0, 0.5), cex = bcex, font = 3)
    } else {
      for (j in seq_len(ncol)) {
        lines <- strsplit(r$cells[j], "\n", fixed = TRUE)[[1]]
        if (!length(lines)) lines <- ""
        n <- length(lines)
        for (k in seq_len(n)) {
          yk <- ym + ((n - 1) / 2 - (k - 1)) * lh
          .btext(cx, j, spec$align[j], yk, lines[k], bcex, padx)
        }
      }
    }
    if (i < length(spec$body))
      segments(x0, y - rh, x1, y - rh, col = st$hair_col, lwd = st$hair_lwd)
    y <- y - rh
  }

  segments(x0, y, x1, y, lwd = st$rule_thick)
  if (isTRUE(spec$first_col_divider) && ncol >= 2)
    segments(cx[2], body_top, cx[2], y, col = st$hair_col, lwd = st$hair_lwd)

  if (!is.null(spec$footnote))
    text(cx[1], y - foot_h / 2, spec$footnote, adj = c(0, 0.5), cex = fcex, col = "gray30")

  invisible(NULL)
}

## natural size (inches) of a table, measured on a scratch device
.btable_size <- function(spec, res) {
  tf <- tempfile(fileext = ".png")
  png(tf, width = 30, height = 30, units = "in", res = res)
  plot.new()
  L <- .btable_layout(spec)
  dev.off(); unlink(tf)
  c(w = L$total_w + 2 * .btable_style$margin,
    h = L$total_h + 2 * .btable_style$margin)
}

## print method: draw the table to the screen (Plots pane)
print.btable <- function(x, ...) { .draw_btable(x); invisible(x) }


## =====================================================================
## freq.table()  —  one-way frequency table
##
##   x         a vector / data-frame column (e.g. d1$q05_class_standing)
##   main      optional title; NULL (default) = no title
##   varname   heading for the first column; defaults to the variable name
##             from x (d1$REGION -> "REGION"). Override when x was reassigned.
##
## Columns: Frequency, Percent, Cumulative Frequency, Cumulative Percent
## =====================================================================

freq.table <- function(x, main = NULL, varname = NULL) {

  if (is.null(varname)) varname <- sub(".*\\$", "", deparse(substitute(x)))

  counts    <- table(x)
  Frequency <- as.integer(counts)
  Percent   <- 100 * Frequency / sum(Frequency)
  cf <- cumsum(Frequency); cp <- cumsum(Percent)
  lev <- names(counts)

  body <- lapply(seq_along(lev), function(i) list(type = "data", cells = c(
    lev[i],
    formatC(Frequency[i], format = "d", big.mark = ","),
    formatC(Percent[i],   format = "f", digits = 2),
    formatC(cf[i],        format = "d", big.mark = ","),
    formatC(cp[i],        format = "f", digits = 2))))

  structure(list(
    colkeys = c("level", "Frequency", "Percent", "CumFreq", "CumPct"),
    header  = c(varname, "Frequency", "Percent", "Cumulative Frequency", "Cumulative Percent"),
    align   = c("left", "right", "right", "right", "right"),
    spanners = list(), body = body,
    title = main, footnote = NULL, first_col_divider = TRUE),
    class = "btable")
}


## =====================================================================
## c.table()  —  two- and three-way contingency table (PROC FREQ style)
##
##   row, col     the two categorical variables to cross
##   strata       optional third variable; one panel per level (3-way).
##                NULL (default) = a single 2-way table.
##   main         optional title
##   row.pct / col.pct / total.pct   add each percentage to the cells (FALSE)
##   rowname / colname / stratname   heading overrides (default: derived names)
##
## Cell order: Frequency, then Total / Row / Col Pct for the toggles that are
## on. Margins carry Frequency + Total Pct only. Active contents shown in a note.
## =====================================================================

c.table <- function(row, col, strata = NULL, main = NULL,
                    row.pct = FALSE, col.pct = FALSE, total.pct = FALSE,
                    rowname = NULL, colname = NULL, stratname = NULL) {

  rname <- if (!is.null(rowname))   rowname   else sub(".*\\$", "", deparse(substitute(row)))
  cname <- if (!is.null(colname))   colname   else sub(".*\\$", "", deparse(substitute(col)))
  sname <- if (!is.null(stratname)) stratname else sub(".*\\$", "", deparse(substitute(strata)))

  fmt_cell <- function(freq, tp = NA, rp = NA, cp = NA) {
    parts <- formatC(round(freq), format = "d", big.mark = ",")
    if (total.pct && is.finite(tp)) parts <- c(parts, sprintf("%.2f", tp))
    if (row.pct   && is.finite(rp)) parts <- c(parts, sprintf("%.2f", rp))
    if (col.pct   && is.finite(cp)) parts <- c(parts, sprintf("%.2f", cp))
    paste(parts, collapse = "\n")
  }

  panel <- function(r, c) {
    tab <- table(r, c); rl <- rownames(tab); cl <- colnames(tab)
    R <- length(rl); C <- length(cl)
    rowtot <- rowSums(tab); coltot <- colSums(tab); grand <- sum(tab)
    pct <- function(n, d) if (d == 0) 0 else n / d * 100    # 0.00 for empty levels
    rows <- list()
    for (i in seq_len(R)) {
      cells <- character(C + 2); cells[1] <- rl[i]
      for (j in seq_len(C))
        cells[j + 1] <- fmt_cell(tab[i, j], tp = pct(tab[i, j], grand),
                                 rp = pct(tab[i, j], rowtot[i]), cp = pct(tab[i, j], coltot[j]))
      cells[C + 2] <- fmt_cell(rowtot[i], tp = pct(rowtot[i], grand))
      rows[[i]] <- cells
    }
    tr <- character(C + 2); tr[1] <- "Total"
    for (j in seq_len(C)) tr[j + 1] <- fmt_cell(coltot[j], tp = pct(coltot[j], grand))
    tr[C + 2] <- fmt_cell(grand, tp = if (grand == 0) 0 else 100)
    rows[[R + 1]] <- tr
    list(cl = cl, rows = rows)
  }

  if (is.null(strata)) {
    p <- panel(row, col); cl <- p$cl
    body <- lapply(p$rows, function(cells) list(type = "data", cells = cells))
  } else {
    lv <- levels(as.factor(strata)); body <- list(); cl <- NULL
    for (l in lv) {
      idx <- which(as.character(strata) == l)
      p <- panel(row[idx], col[idx]); cl <- p$cl
      body[[length(body) + 1]] <- list(type = "group", label = paste0(sname, " = ", l))
      for (cells in p$rows) body[[length(body) + 1]] <- list(type = "data", cells = cells)
    }
  }

  colkeys <- c(".rowlab", cl, "Total")
  header  <- c(rname, cl, "Total")
  align   <- c("left", rep("center", length(cl)), "center")
  cat_idx <- seq_along(cl) + 1

  leg <- "Frequency"
  if (total.pct) leg <- paste0(leg, " | Total Pct")
  if (row.pct)   leg <- paste0(leg, " | Row Pct")
  if (col.pct)   leg <- paste0(leg, " | Col Pct")

  structure(list(
    colkeys = colkeys, header = header, align = align,
    spanners = list(list(label = cname, cols = cat_idx)),
    body = body, title = main,
    footnote = paste0("Cell contents: ", leg), first_col_divider = TRUE),
    class = "btable")
}


## =====================================================================
## q.table()  —  univariate descriptive statistics
##
##   type = "econ"        (default)  N, Mean, Std Dev, Min, Max
##   type = "percentile"             N, Mean, Std Dev, 10th ... 90th
##   type = "full"                   N, Mean, Std Dev, Min, Q1, Median, Q3, Max
##
##   data      a data frame
##   vars      numeric columns to summarize; NULL (default) = all numeric
##   labels    display names for vars (same length); requires vars
##   headers   "long" (default) spelled-out, or "short" (p10, Q1, ...)
##   digits    decimals for the statistics (N always integer); default 2
##   main      optional title
## =====================================================================

q.table <- function(data, vars = NULL, labels = NULL, type = "econ",
                    headers = "long", digits = 2, main = NULL) {

  if (!is.data.frame(data)) stop("data must be a data frame.")
  type    <- match.arg(type,    c("econ", "percentile", "full"))
  headers <- match.arg(headers, c("long", "short"))

  if (!is.null(labels) && is.null(vars))
    stop("The labels argument is only available when the user specifies a list of variables using the vars argument.")

  if (is.null(vars)) {
    vars <- names(data)[vapply(data, is.numeric, logical(1))]
    if (length(vars) == 0L) stop("No numeric variables found in data.")
  }
  absent <- setdiff(vars, names(data))
  if (length(absent) > 0L) stop(sprintf("Variable(s) not found in data: %s", paste(absent, collapse = ", ")))
  not_num <- vars[!vapply(data[vars], is.numeric, logical(1))]
  if (length(not_num) > 0L) stop(sprintf("q.table() requires numeric variables. Non-numeric: %s", paste(not_num, collapse = ", ")))

  if (!is.null(labels)) {
    if (length(labels) != length(vars))
      stop(sprintf("labels has %d entr%s but vars has %d.", length(labels),
                   if (length(labels) == 1L) "y" else "ies", length(vars)))
    row_names <- labels
  } else row_names <- vars

  stat_row <- function(x) {
    x <- x[!is.na(x)]; n <- length(x); m <- mean(x); s <- stats::sd(x)
    switch(type,
      econ = c(N = n, Mean = m, SD = s, Min = min(x), Max = max(x)),
      percentile = { q <- stats::quantile(x, c(.10, .25, .50, .75, .90), type = 7, names = FALSE)
        c(N = n, Mean = m, SD = s, p10 = q[1], p25 = q[2], p50 = q[3], p75 = q[4], p90 = q[5]) },
      full = { q <- stats::quantile(x, c(.25, .50, .75), type = 7, names = FALSE)
        c(N = n, Mean = m, SD = s, Min = min(x), Q1 = q[1], Med = q[2], Q3 = q[3], Max = max(x)) })
  }

  ncols <- switch(type, econ = 5L, percentile = 8L, full = 8L)
  mat   <- t(vapply(data[vars], stat_row, numeric(ncols)))
  df    <- data.frame(Variable = row_names, mat, check.names = FALSE,
                      stringsAsFactors = FALSE, row.names = NULL)

  lab <- switch(type,
    econ = if (headers == "long")
        c(N = "N", Mean = "Mean", SD = "Std Dev", Min = "Minimum", Max = "Maximum")
      else c(N = "N", Mean = "Mean", SD = "SD", Min = "Min", Max = "Max"),
    percentile = if (headers == "long")
        c(N = "N", Mean = "Mean", SD = "Std Dev", p10 = "10th", p25 = "25th", p50 = "50th", p75 = "75th", p90 = "90th")
      else c(N = "N", Mean = "Mean", SD = "SD", p10 = "p10", p25 = "p25", p50 = "p50", p75 = "p75", p90 = "p90"),
    full = if (headers == "long")
        c(N = "N", Mean = "Mean", SD = "Std Dev", Min = "Minimum", Q1 = "First Quartile", Med = "Median", Q3 = "Third Quartile", Max = "Maximum")
      else c(N = "N", Mean = "Mean", SD = "SD", Min = "Min", Q1 = "Q1", Med = "Median", Q3 = "Q3", Max = "Max"))

  num_cols <- setdiff(names(df), "Variable")
  colkeys  <- c("Variable", num_cols)
  header   <- c("Variable", unname(lab[num_cols]))
  align    <- c("left", rep("right", length(num_cols)))

  spanners <- list()
  if (type == "percentile" && headers == "long")
    spanners <- list(list(label = "Percentile",
                          cols = which(colkeys %in% c("p10", "p25", "p50", "p75", "p90"))))

  body <- lapply(seq_len(nrow(df)), function(i) {
    cells <- character(length(colkeys)); cells[1] <- df$Variable[i]
    for (j in seq_along(num_cols)) {
      key <- num_cols[j]; val <- df[[key]][i]
      cells[j + 1] <- if (key == "N") formatC(val, format = "d", big.mark = ",")
                      else            formatC(val, format = "f", digits = digits, big.mark = ",")
    }
    list(type = "data", cells = cells)
  })

  structure(list(colkeys = colkeys, header = header, align = align,
                 spanners = spanners, body = body, title = main,
                 footnote = NULL, first_col_divider = TRUE),
            class = "btable")
}


## =====================================================================
## png.print()  —  save a toolkit table, or the latest base-R plot, to PNG
##
##   TABLES are objects  -> pass the object:  t1 <- freq.table(...); png.print(t1)
##   BASE-R PLOTS aren't  -> pass nothing:     barplot(...);         png.print()
##
## Filenames:
##   png.print(t1)               -> t1.png       (object name)
##   png.print(t1, "freq.png")   -> freq.png
##   png.print()                 -> plot.png     (default; overwrites!)
##   png.print("age_hist.png")   -> age_hist.png
##   png.print("age_hist")       -> age_hist.png (extension added)
##
##   res / width / height   resolution and (plots only) pixel size
## =====================================================================

png.print <- function(x, file = NULL, res = 200, width = 1200, height = 800) {

  supplied <- !missing(x)
  xexpr    <- if (supplied) substitute(x) else NULL

  if (supplied && is.character(x) && length(x) == 1L) {   # bare string = filename
    if (is.null(file)) file <- x
    supplied <- FALSE
  }

  is_table <- supplied && inherits(x, "btable")

  if (is.null(file)) {
    nm <- if (supplied) deparse(xexpr) else NULL
    if (!is.null(nm) && !grepl("^[A-Za-z.][A-Za-z0-9._]*$", nm)) nm <- NULL
    file <- if (!is.null(nm)) paste0(nm, ".png")
            else if (is_table) "table.png" else "plot.png"
  }
  if (!grepl("\\.[A-Za-z0-9]+$", file)) file <- paste0(file, ".png")

  if (is_table) {
    sz <- .btable_size(x, res)
    png(file, width = sz["w"], height = sz["h"], units = "in", res = res, bg = "white")
    .draw_btable(x)
    dev.off()
  } else {
    dev.copy(png, file, width = width, height = height, res = res)  # current plot
    dev.off()
  }
  invisible(if (is_table) x else NULL)
}
