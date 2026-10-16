## #####################################################################
## BUSN 3000H  —  Table Toolkit   (pure base R, ZERO packages)
## VERSION V3.4
##
## Tables are drawn with base graphics (text/segments) straight onto R's
## png() device -- the SAME machinery bar charts use. Nothing to install:
## no gt, no flextable, no dplyr, no webshot2, no Chrome.
##
## Functions
##   freq.table()  one-way frequency table
##   multi.table() one-way frequency table for a select-all-that-apply question
##   con.table() two-/three-way contingency table (PROC FREQ style)
##   q.table()     univariate descriptive statistics (econ / percentile / full)
##   bar.chart()   bar chart for one or two categorical variables (grouped)
##   png.print()   save any of the above tables, or the current plot, to a PNG
##
## v2: adds multi.table(); bar.chart() gains a mar argument (default unchanged)
## v3: multi.table() takes the survey column as-is and splits the checked
##     boxes itself; its n argument is removed (n is counted from the data)
## v3.1: select-all answers are separated by "; " in the survey files, and
##     every function follows one rule for them (see "Select-all questions"
##     below): freq.table(), multi.table() and bar.chart() count each option;
##     con.table() stops with a message. Also: con.table() strata panels all
##     show every category, and bar.chart()'s x-axis label no longer overlaps
##     long wrapped category labels.
## v3.2: in every table, the columns after the first share one width; long
##     headings wrap, and those headings are centered.
## v3.3: a blank answer ("" or spaces) is a skipped question in every function.
##     freq.table(), multi.table(), con.table() and bar.chart() gain
##     other = NULL: other = 5 puts categories with 5 or fewer respondents into
##     one "Other" category (see "Other categories" below). freq.table(),
##     multi.table(), con.table(), q.table() and bar.chart() gain note = NULL:
##     your own note under the table or chart. png.print() saves charts at
##     8 x 5 inches by default, whatever the size of the Plots pane.
## v3.4: categories are listed in one order everywhere (see "Category order,
##     numbering and wrapping" below): numbers in number order, a factor in its
##     level order, text answers from most to least frequent, "Other" after
##     them. freq.table(), multi.table(), con.table() and bar.chart() gain
##     order = and last =. A select-all question, or one with an answer longer
##     than 30 characters, is numbered: "1. answer" in the table and bars
##     1, 2, 3, ... in the chart, so the table is the chart's key. Table labels
##     longer than 30 characters wrap. freq.table() and multi.table() gain
##     base = "all": percents of every respondent, with a "No Answer" row.
##     bar.chart() sets its own margins (mar = NULL): a tight top and a bottom
##     that fits the labels. Table images have a thinner white border.
##     A bar.chart() note lines up with the y-axis and runs to the end of the
##     x-axis; png.print() draws
##     a bar.chart() again at the PNG's size so the note fits the saved chart.
##     Fixes: a bar.chart() note that doesn't fit no longer breaks the charts
##     after it; a table saved at a new resolution no longer squashes its rows.
##
## Export flow:
##   t1 <- freq.table(...) ;  png.print(t1)   # table -> t1.png
##   barplot(...)          ;  png.print()     # plot  -> plot.png
## #####################################################################


## #####################################################################
## CONFIG  —  edit these to restyle every chart in the toolkit
## #####################################################################

BUSN.fill     <- "#003566"   # default bar fill  color
BUSN.border   <- "#001D3D"   # default bar border color
BUSN.cex.main <- 0.95        # title font size                         (charts AND tables)
BUSN.cex.axis <- 0.75        # axis / header / body / legend font size (charts AND tables)

## #####################################################################


## =====================================================================
## Rendering engine (internal) — draws a table spec with base graphics
## =====================================================================

## Table title / header / body sizes come from the same CONFIG knobs as the
## charts:  title -> BUSN.cex.main ,  headers & body & footnote -> BUSN.cex.axis
.btable_style <- list(
  title_cex = BUSN.cex.main, head_cex = BUSN.cex.axis,
  body_cex  = BUSN.cex.axis, foot_cex = BUSN.cex.axis,
  cell_pad_x = 0.12, row_pad_y = 0.07, line_mult = 1.35, margin = 0.05,
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

## wrap a heading at spaces so each line fits width_in (inches)
.wrap_head <- function(h, width_in, cex) {
  words <- strsplit(h, " ", fixed = TRUE)[[1]]
  words <- words[nzchar(words)]
  if (!length(words)) return(h)
  lines <- character(0); cur <- ""
  for (w in words) {
    cand <- if (nzchar(cur)) paste(cur, w) else w
    if (nzchar(cur) && strwidth(cand, units = "inches", cex = cex) > width_in) {
      lines <- c(lines, cur); cur <- w
    } else cur <- cand
  }
  paste(c(lines, cur), collapse = "\n")
}

## compute column widths and row heights (inches); needs an active device
##
## Column widths: the first column (row labels) is as wide as its contents.
## All other columns share ONE width: wide enough for the widest cell and the
## widest single word of any heading. Headings longer than that wrap onto
## more lines, and the header row grows to fit.
.btable_layout <- function(spec) {
  st   <- .btable_style
  ncol <- length(spec$colkeys)
  lh   <- strheight("Ag", units = "inches", cex = st$body_cex) * st$line_mult
  pad  <- st$cell_pad_x

  ## widest body cell in column j
  body_w <- function(j) {
    w <- 0
    for (r in spec$body) {
      if (identical(r$type, "data")) {
        lines <- strsplit(r$cells[j], "\n", fixed = TRUE)[[1]]
        if (!length(lines)) lines <- ""
        w <- max(w, max(strwidth(lines, units = "inches", cex = st$body_cex)))
      }
    }
    w
  }
  ## widest single word of a heading
  word_w <- function(h) {
    words <- strsplit(h, " ", fixed = TRUE)[[1]]
    words <- words[nzchar(words)]
    if (!length(words)) return(0)
    max(strwidth(words, units = "inches", cex = st$head_cex))
  }

  colw    <- numeric(ncol)
  colw[1] <- max(strwidth(spec$header[1], units = "inches", cex = st$head_cex), body_w(1)) + 2 * pad
  dcols   <- seq_len(ncol)[-1]                         # the data columns
  if (length(dcols)) {
    W <- max(vapply(dcols, function(j) max(body_w(j), word_w(spec$header[j])), numeric(1))) + 2 * pad
    ## a spanner label must fit across the columns it spans
    for (sp in spec$spanners) {
      need <- strwidth(sp$label, units = "inches", cex = st$head_cex) + 2 * pad
      W <- max(W, need / length(sp$cols))
    }
    colw[dcols] <- W
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

  ## wrap the data-column headings to the final column width
  header <- spec$header
  for (j in seq_len(ncol)[-1])
    header[j] <- .wrap_head(header[j], colw[j] - 2 * pad, st$head_cex)
  head_lines <- max(vapply(header, function(h) length(strsplit(h, "\n", fixed = TRUE)[[1]]),
                           integer(1)), 1L)

  ## the user's note = ... goes last, wrapped to the table width (so a long
  ## note never widens the table); "\n" in the note starts a new line
  footnote <- spec$footnote
  if (!is.null(spec$note)) {
    paras <- strsplit(paste(spec$note, collapse = "\n"), "\n", fixed = TRUE)[[1]]
    wrapped <- vapply(paras, function(p) .wrap_head(p, total_w - 2 * pad, st$foot_cex),
                      character(1))
    footnote <- .join_notes(footnote, paste(wrapped, collapse = "\n"))
  }

  ## heights
  has_span <- length(spec$spanners) > 0
  span_h   <- if (has_span) lh + 2 * st$row_pad_y else 0
  head_h   <- head_lines * lh + 2 * st$row_pad_y
  title_h  <- if (!is.null(spec$title))    strheight(spec$title,    units = "inches", cex = st$title_cex) + 2 * st$row_pad_y else 0
  foot_h   <- if (!is.null(footnote)) strheight(footnote, units = "inches", cex = st$foot_cex)  + 2 * st$row_pad_y else 0

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
       has_span = has_span, header = header, footnote = footnote)
}

## draw the table on the current device, shrinking uniformly to fit if needed
.draw_btable <- function(spec) {
  st <- .btable_style
  op <- par(mar = c(0, 0, 0, 0), xpd = NA); on.exit(par(op))
  plot.new()
  .fresh_metrics()                          # measure fonts on THIS device
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

  ## headings: first column keeps its alignment; data-column headings are
  ## centered; wrapped headings are drawn line by line, centered vertically
  full_top <- header_top; full_bot <- y - head_h
  for (j in seq_len(ncol)) {
    yc <- if (j %in% span_cols) y - head_h / 2 else (full_top + full_bot) / 2
    hl <- strsplit(L$header[j], "\n", fixed = TRUE)[[1]]
    if (!length(hl)) hl <- ""
    nh <- length(hl)
    ha <- if (j == 1) spec$align[1] else "center"
    for (k in seq_len(nh))
      .btext(cx, j, ha, yc + ((nh - 1) / 2 - (k - 1)) * lh, hl[k], hcex, padx)
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

  if (!is.null(L$footnote))
    text(cx[1], y - foot_h / 2, L$footnote, adj = c(0, 0.5), cex = fcex, col = "gray30")

  invisible(NULL)
}

## natural size (inches) of a table, measured on a scratch device
.btable_size <- function(spec, res) {
  tf <- tempfile(fileext = ".png")
  png(tf, width = 30, height = 30, units = "in", res = res)
  plot.new()
  .fresh_metrics()
  L <- .btable_layout(spec)
  dev.off(); unlink(tf)
  c(w = L$total_w + 2 * .btable_style$margin,
    h = L$total_h + 2 * .btable_style$margin)
}

## print method: draw the table to the screen (Plots pane)
print.btable <- function(x, ...) { .draw_btable(x); invisible(x) }


## =====================================================================
## Select-all questions (internal) — one rule shared by every function
##
## In the survey files, a select-all-that-apply answer lists the boxes the
## respondent checked separated by "; "
##   (e.g. "HOPE or Zell Miller Scholarship; Student loans").
## A column with ";" in any cell is a select-all column. Any other column is
## single-choice, so a comma inside an answer ("Yes, a sorority") is never split.
##
##   freq.table(), multi.table(), bar.chart()  count each option once per
##                                             respondent who checked it
##   con.table()                               stops with a message
## =====================================================================

.multi_sep <- ";"

## TRUE when x is a select-all column (";" in any cell)
.is_multi <- function(x) {
  v <- as.character(x)
  any(!is.na(v) & grepl(.multi_sep, v, fixed = TRUE))
}

## each respondent's answer split into the options checked (trimmed, blanks
## dropped, each option once); NA or blank -> character(0)
.split_multi <- function(x) {
  lapply(as.character(x), function(s) {
    if (is.na(s)) return(character(0))
    p <- trimws(strsplit(s, .multi_sep, fixed = TRUE)[[1]])
    unique(p[p != ""])
  })
}

## every option anyone checked (for checking order = and last =)
.multi_answers <- function(x) unique(unlist(.split_multi(x)))

## option counts (respondents who checked each) and n (respondents who answered).
## With other = k, options checked by k or fewer respondents become "Other",
## and "Other" counts each respondent once (not once per small option).
## The counts come back in display order (see "Category order" below).
.multi_counts <- function(x, other = NULL, order = NULL, last = NULL) {
  parts <- .split_multi(x)
  picks <- unlist(parts)
  if (is.null(picks)) picks <- character(0)
  kind <- if (is.numeric(x)) "num" else "text"; lv <- NULL
  if (is.factor(x)) {                                     # keep a factor's level order
    flv <- unique(trimws(levels(x))); flv <- flv[flv != ""]
    if (all(picks %in% flv)) { picks <- factor(picks, levels = flv); kind <- "factor"; lv <- flv }
  }
  counts <- table(picks)
  small  <- character(0)
  if (!is.null(other)) {
    .check_other(other)
    small <- .small_cats(counts, other)
    nm    <- names(counts)
    if (length(small) || "Other" %in% nm) {
      parts  <- lapply(parts, function(p) { p[p %in% small] <- "Other"; unique(p) })
      keep   <- setdiff(nm, c(small, "Other"))
      picks  <- unlist(parts)
      if (is.null(picks)) picks <- character(0)
      counts <- table(factor(picks, levels = c(keep, "Other")))
    }
  }
  counts <- counts[.order_levels(counts, kind, lv, order, last)]
  list(counts = counts, n = sum(lengths(parts) > 0L), parts = parts, small = small)
}

## the Option | Frequency | Percent of Respondents table (freq.table and multi.table)
.multi_btable <- function(x, main, varname, other = NULL, note = NULL,
                          order = NULL, last = NULL, base = "answered") {
  .check_note(note)
  base <- .check_base(base)
  .check_order(order, last, .multi_answers(x))
  mc    <- .multi_counts(x, other, .as_names(order), .as_names(last))
  n_ans <- mc$n
  if (n_ans == 0L) stop("Every value of x is blank or NA: no one answered this question.",
                        call. = FALSE)
  n  <- if (base == "all") length(x) else n_ans         # base = "all": every respondent
  na <- n - n_ans                                       # respondents with no answer

  Frequency <- as.integer(mc$counts)
  Percent   <- 100 * Frequency / n
  cats      <- names(mc$counts)
  labs      <- .cat_labels(cats, .numbered(cats, .is_multi(x)))

  body <- lapply(seq_along(cats), function(i) list(type = "data", cells = c(
    labs[i],
    formatC(Frequency[i], format = "d", big.mark = ","),
    formatC(Percent[i],   format = "f", digits = 2))))
  if (base == "all" && na > 0L)                         # not numbered: it has no bar
    body[[length(body) + 1L]] <- list(type = "data", cells = c(
      "No Answer",
      formatC(na, format = "d", big.mark = ","),
      formatC(100 * na / n, format = "f", digits = 2)))

  structure(list(
    colkeys = c("level", "Frequency", "PctResp"),
    header  = c(varname, "Frequency", "Percent of Respondents"),
    align   = c("left", "right", "right"),
    spanners = list(), body = body,
    title = main,
    footnote = .join_notes(
      paste0("n = ", formatC(n, format = "d", big.mark = ","), " respondents",
             if (base == "all" && na > 0L)
               paste0(", including ", formatC(na, format = "d", big.mark = ","),
                      " with no answer") else "",
             if (.is_multi(x)) "; more than one option could be selected" else ""),
      .other_note(mc$small, other)),
    note = note, first_col_divider = TRUE),
    class = "btable")
}

## stop when a select-all column is passed where it can't be used
.stop_if_multi <- function(x, name, use) {
  if (!is.null(x) && .is_multi(x))
    stop(sprintf("%s is a select-all question; %s", name, use), call. = FALSE)
}


## =====================================================================
## Blank answers and Other categories (internal) — shared by every function
##
## A blank answer ("" or only spaces) is a skipped question: it is treated
## exactly like NA, so it never shows up as a category and is not counted.
##
## other = NULL (default): nothing changes.
## other = 5: categories with 5 or fewer respondents go into one "Other"
##   category, after the others. A category already named "Other" absorbs them.
##   Nothing is lumped unless at least two categories qualify (one small
##   category would only be renamed). A footnote says what "Other" holds.
## =====================================================================

## blanks -> NA (factor: blank levels dropped, so those values become NA)
.blank_na <- function(x) {
  if (is.null(x)) return(x)
  if (is.factor(x)) {
    lv  <- levels(x)
    bad <- trimws(lv) == ""
    if (any(bad)) x <- factor(x, levels = lv[!bad])
    return(x)
  }
  if (is.character(x)) x[!is.na(x) & trimws(x) == ""] <- NA
  x
}

.check_other <- function(other) {
  if (length(other) != 1L || !is.numeric(other) || is.na(other) || other < 0)
    stop("other must be one number, 0 or more (e.g. other = 5).", call. = FALSE)
}

## the categories to lump: frequency <= other, at least two of them
.small_cats <- function(counts, other) {
  small <- names(counts)[as.vector(counts) <= other & names(counts) != "Other"]
  if (length(small) >= 2L) small else character(0)
}

## recode a single-choice variable: small categories -> "Other" (last).
## counts = the frequencies that decide what is small (default table(x)).
## Returns list(x = recoded variable, small = categories lumped).
.apply_other <- function(x, other, counts = NULL) {
  if (is.null(other)) return(list(x = x, small = character(0)))
  .check_other(other)
  if (is.null(counts)) counts <- table(x)
  small <- .small_cats(counts, other)
  lv    <- names(counts)                          # display order of the categories
  if (!length(small) && !("Other" %in% lv)) return(list(x = x, small = small))
  v <- as.character(x)
  v[!is.na(v) & v %in% small] <- "Other"
  list(x = factor(v, levels = c(setdiff(lv, c(small, "Other")), "Other")), small = small)
}

## footnote line describing "Other" (NULL when nothing was lumped)
.other_note <- function(small, other) {
  if (!length(small)) return(NULL)
  paste0("Other: categories with ", format(other), " or fewer respondents")
}

## combine footnote lines (skipping NULLs) into one footnote, or NULL
.join_notes <- function(...) {
  notes <- unlist(list(...))
  if (!length(notes)) NULL else paste(notes, collapse = "\n")
}

## note = must be text (or NULL)
.check_note <- function(note) {
  if (!is.null(note) && (!is.character(note) || !length(note)))
    stop('note must be text in quotes, e.g. note = "Note: ..."', call. = FALSE)
  note
}

## a chart note split into lines that fit width_in inches ("\n" = new line),
## measured with the device's own font. bar.chart() passes the width of the
## plot area, so the note runs from the y-axis to the end of the x-axis;
## png.print() draws the chart again at the PNG's size.
.chart_note <- function(note, cex, width_in) {
  paras <- strsplit(paste(note, collapse = "\n"), "\n", fixed = TRUE)[[1]]
  unlist(lapply(paras, function(p) {
    if (!nzchar(trimws(p))) return("")
    w <- tryCatch(.wrap_head(p, width_in, cex), error = function(e) NULL)
    if (is.null(w))                # no font measurements: estimate from the letter size
      w <- paste(strwrap(p, max(10, floor(width_in / (0.55 * par("cin")[2] * cex)))),
                 collapse = "\n")
    strsplit(w, "\n", fixed = TRUE)[[1]]
  }))
}

## R keeps the size of the letter "M" it measured on the last device. A new
## PNG at another resolution can reuse that stale size, which squashes table
## rows. Measuring once at an unusual text size forces a fresh measurement.
.fresh_metrics <- function() invisible(strheight("M", units = "inches", cex = 1.2345))


## =====================================================================
## Category order, numbering and wrapping (internal) — v3.4, shared by
## freq.table(), multi.table(), con.table() and bar.chart(), so a table and
## its chart always list the categories in the same order.
##
## Order:
##   numbers (e.g. a 1-5 rating)  number order
##   a factor                     its level order (set with factor(levels = ...))
##   text answers                 most to least frequent; ties alphabetical
##   "Other" comes after the regular categories.
##   order = c("A", "B")  puts those answers first, in that order.
##   last  = c("Y", "Z")  puts those answers at the end, in that order.
##   Each must match an answer exactly; anything else stops with a message
##   listing the answers. (An answer combined into "Other" is skipped.)
## Numbering: a select-all question, or one with any answer longer than 30
##   characters, is numbered: the table shows "1. answer" and the bar chart
##   labels its bars 1, 2, 3, ... so the table is the chart's key.
## Wrapping: in a table, an answer longer than 30 characters wraps onto more
##   lines (no line over 30 characters, not counting the number in front).
## =====================================================================

.max_label <- 30                       # longer answers are numbered and wrapped

.cat_kind <- function(x) if (is.factor(x)) "factor" else if (is.numeric(x)) "num" else "text"

## the answers in a single-choice variable (for checking order = and last =)
.answers <- function(x) {
  if (is.factor(x)) return(levels(x))
  v <- x[!is.na(x)]
  if (is.numeric(v)) as.character(sort(unique(v))) else unique(as.character(v))
}

.as_names <- function(v) if (is.null(v)) NULL else as.character(v)

## order = and last = must be answers in the data, and not both at once
.check_order <- function(order, last, answers) {
  args <- list(order = order, last = last)
  for (a in names(args)) {
    v <- args[[a]]
    if (is.null(v)) next
    if (!(is.character(v) || is.numeric(v)) || !length(v) || anyNA(v))
      stop(sprintf('%s must be answers in quotes, e.g. %s = c("Yes", "No")', a, a),
           call. = FALSE)
    bad <- setdiff(as.character(v), answers)
    if (length(bad))
      stop(sprintf('%s = "%s": that is not one of the answers. The answers are: %s',
                   a, bad[1], paste0('"', answers, '"', collapse = ", ")), call. = FALSE)
  }
  both <- intersect(as.character(order), as.character(last))
  if (length(both))
    stop(sprintf('"%s" is in both order and last; list it in only one.', both[1]),
         call. = FALSE)
  invisible(NULL)
}

.check_base <- function(base) {
  if (!(is.character(base) && length(base) == 1L && base %in% c("answered", "all")))
    stop('base must be "answered" (the default) or "all".', call. = FALSE)
  base
}

## the display order of a set of category counts (names = categories).
## kind comes from the variable BEFORE other = ("num", "factor" or "text").
.order_levels <- function(counts, kind, lv = NULL, order = NULL, last = NULL) {
  nm <- names(counts)
  if (!length(nm)) return(character(0))
  reg <- nm[nm != "Other"]
  base <- if (kind == "num") {
    reg[base::order(suppressWarnings(as.numeric(reg)))]
  } else if (kind == "factor") {
    if ("Other" %in% nm && "Other" %in% lv) c(lv[lv %in% nm], setdiff(reg, lv))
    else c(lv[lv %in% reg], setdiff(reg, lv))
  } else {
    reg[base::order(-as.vector(counts[reg]), reg, method = "radix")]
  }
  if ("Other" %in% nm && !("Other" %in% base)) base <- c(base, "Other")
  order <- order[order %in% nm]
  last  <- last[last %in% nm]
  c(order, setdiff(base, c(order, last)), last)
}

## numbered: a select-all question, or any answer longer than 30 characters
.numbered <- function(cats, multi) multi || any(nchar(cats) > .max_label)

## wrap a label at spaces so no line is longer than width characters
.wrap_chars <- function(s, width = .max_label) {
  if (is.na(s) || nchar(s) <= width) return(s)
  words <- strsplit(s, " ", fixed = TRUE)[[1]]
  words <- words[nzchar(words)]
  lines <- character(0); cur <- ""
  for (w in words) {
    cand <- if (nzchar(cur)) paste(cur, w) else w
    if (nzchar(cur) && nchar(cand) > width) { lines <- c(lines, cur); cur <- w } else cur <- cand
  }
  paste(c(lines, cur), collapse = "\n")
}

## the first-column labels of a table: each answer wrapped if longer than 30
## characters, then numbered ("1. answer") when the question is numbered
.cat_labels <- function(cats, num) {
  labs <- vapply(cats, .wrap_chars, character(1), USE.NAMES = FALSE)
  if (num) paste0(seq_along(cats), ". ", labs) else labs
}


## =====================================================================
## freq.table()  —  one-way frequency table
##
##   x         a vector / data-frame column (e.g. d1$q05_class_standing)
##   main      optional title; NULL (default) = no title
##   varname   heading for the first column; defaults to the variable name
##             from x (d1$REGION -> "REGION"). Override when x was reassigned.
##   other     NULL (default) = off. other = 5 puts categories with 5 or fewer
##             respondents into one "Other" row, after the others.
##   note      NULL (default) = none. Text printed as the last line under the
##             table, e.g. note = "Note: ...". Long notes wrap; "\n" starts a new line.
##   order     NULL (default) = none. Answers to list first, in this order,
##             e.g. order = c("Monday", "Tuesday").
##   last      NULL (default) = none. Answers to list at the end, in this order,
##             e.g. last = "Prefer not to answer".
##   base      "answered" (default): percents of the respondents who answered.
##             "all": percents of every respondent (every row of the data), with
##             a "No Answer" row for the rest.
##
## Columns: Frequency, Percent, Cumulative Frequency, Cumulative Percent
## Row order: numbers in number order, a factor in its level order, text from
## most to least frequent (see "Category order" above). Blank answers are
## skipped (not counted). A select-all column gets the multi.table() layout
## instead (Frequency and Percent of Respondents; no cumulative columns, since
## percents can sum to more than 100).
## =====================================================================

freq.table <- function(x, main = NULL, varname = NULL, other = NULL, note = NULL,
                       order = NULL, last = NULL, base = "answered") {

  if (is.null(varname)) varname <- sub(".*\\$", "", deparse(substitute(x)))
  .check_note(note)
  base <- .check_base(base)
  x <- .blank_na(x)
  if (.is_multi(x)) return(.multi_btable(x, main, varname, other, note, order, last, base))

  kind <- .cat_kind(x)
  lv   <- if (is.factor(x)) levels(x) else NULL
  .check_order(order, last, .answers(x))
  ao     <- .apply_other(x, other)
  counts <- table(ao$x)
  counts <- counts[.order_levels(counts, kind, lv, .as_names(order), .as_names(last))]
  cats   <- names(counts)

  Frequency <- as.integer(counts)
  n_ans <- sum(Frequency)
  n     <- if (base == "all") length(x) else n_ans      # base = "all": every respondent
  na    <- n - n_ans                                    # respondents with no answer
  Percent <- 100 * Frequency / n
  cf <- cumsum(Frequency); cp <- 100 * cf / n
  labs <- .cat_labels(cats, .numbered(cats, FALSE))

  body <- lapply(seq_along(cats), function(i) list(type = "data", cells = c(
    labs[i],
    formatC(Frequency[i], format = "d", big.mark = ","),
    formatC(Percent[i],   format = "f", digits = 2),
    formatC(cf[i],        format = "d", big.mark = ","),
    formatC(cp[i],        format = "f", digits = 2))))
  if (base == "all" && na > 0L)                         # not numbered: it has no bar
    body[[length(body) + 1L]] <- list(type = "data", cells = c(
      "No Answer",
      formatC(na, format = "d", big.mark = ","),
      formatC(100 * na / n, format = "f", digits = 2),
      formatC(n, format = "d", big.mark = ","),
      formatC(100, format = "f", digits = 2)))

  structure(list(
    colkeys = c("level", "Frequency", "Percent", "CumFreq", "CumPct"),
    header  = c(varname, "Frequency", "Percent", "Cumulative Frequency", "Cumulative Percent"),
    align   = c("left", "right", "right", "right", "right"),
    spanners = list(), body = body,
    title = main,
    footnote = .join_notes(
      if (base == "all")
        paste0("n = ", formatC(n, format = "d", big.mark = ","), " respondents",
               if (na > 0L) paste0(", including ", formatC(na, format = "d", big.mark = ","),
                                   " with no answer") else ""),
      .other_note(ao$small, other)),
    note = note, first_col_divider = TRUE),
    class = "btable")
}


## =====================================================================
## multi.table()  —  frequency table for a select-all-that-apply question
##
##   x         the survey column as-is (e.g. d1$aid): one cell per respondent,
##             with the boxes they checked separated by "; "
##             (e.g. "HOPE or Zell Miller Scholarship; Student loans")
##   main      optional title; NULL (default) = no title
##   varname   heading for the first column; defaults to the variable name
##             from x (d1$aid -> "aid"). Override when x was reassigned.
##   other     NULL (default) = off. other = 5 puts options checked by 5 or
##             fewer respondents into one "Other" row, after the others; "Other"
##             counts each respondent once, however many small options they checked.
##   note      NULL (default) = none. Text printed as the last line under the
##             table, e.g. note = "Note: ...". Long notes wrap; "\n" starts a new line.
##   order     NULL (default) = none. Options to list first, in this order,
##             e.g. order = c("Monday", "Tuesday", "Wednesday").
##   last      NULL (default) = none. Options to list at the end, in this order,
##             e.g. last = "I have not used any of these".
##   base      "answered" (default): percents of the respondents who answered.
##             "all": percents of every respondent (every row of the data), with
##             a "No Answer" row for the rest (e.g. a question only some were asked).
##
## Blank and NA cells are respondents who skipped the question; n = the
## number of respondents who answered. Each cell is split at ";" and each
## option is counted once per respondent. On a single-choice column each cell
## is one option, so the percents sum to 100.
##
## Columns: Frequency (respondents who checked the option) and Percent of
## Respondents (Frequency / n). Percents can sum to more than 100.
## Rows run from the most to the least checked option (see "Category order"
## above). On a select-all question the options are numbered ("1. option"),
## matching the numbered bars of bar.chart().
## =====================================================================

multi.table <- function(x, main = NULL, varname = NULL, other = NULL, note = NULL,
                        order = NULL, last = NULL, base = "answered") {

  if (is.null(varname)) varname <- sub(".*\\$", "", deparse(substitute(x)))
  .multi_btable(x, main, varname, other, note, order, last, base)
}

## =====================================================================
## con.table()  —  two- and three-way contingency table (PROC FREQ style)
##
##   row, col     the two categorical variables to cross
##   strata       optional third variable; one panel per level (3-way).
##                NULL (default) = a single 2-way table.
##   main         optional title
##   freq         show the cell frequency (default TRUE). freq = FALSE prints
##                only the percentages you turn on (margins keep the count).
##   row.pct / col.pct / total.pct   add each percentage to the cells (FALSE)
##   rowname / colname / stratname   heading overrides (default: derived names)
##   other        NULL (default) = off. other = 5 puts row categories whose row
##                total is 5 or less into one "Other" row, and column categories
##                whose column total is 5 or less into one "Other" column (each
##                after the others). The strata variable is never combined.
##   note         NULL (default) = none. Text printed as the last line under the
##                table, e.g. note = "Note: ...". Long notes wrap; "\n" starts a new line.
##   order / last NULL (default) = none. Answers to list first / at the end, in
##                that order. Each applies to whichever variable (row or col)
##                has that answer.
##
## Row and column order: numbers in number order, a factor in its level order,
## text from most to least frequent by the row or column total (see "Category
## order" above). Row labels longer than 30 characters are numbered and wrap.
## Cell order: Frequency, then Total / Row / Col Pct for the toggles that are
## on. Margins always carry the count + Total Pct. Active contents shown in a note.
## Every strata panel shows every row and column category (0 where a
## category is absent), so the panels line up under one header.
## Respondents with a blank or NA in row, col or strata are left out.
## A select-all column stops with a message: use multi.table() or bar.chart().
## =====================================================================

con.table <- function(row, col, strata = NULL, main = NULL, freq = TRUE,
                    row.pct = FALSE, col.pct = FALSE, total.pct = FALSE,
                    rowname = NULL, colname = NULL, stratname = NULL, other = NULL,
                    note = NULL, order = NULL, last = NULL) {

  if (!freq && !row.pct && !col.pct && !total.pct)
    stop("With freq = FALSE, turn on at least one of total.pct, row.pct, col.pct.")
  .check_note(note)

  rname <- if (!is.null(rowname))   rowname   else sub(".*\\$", "", deparse(substitute(row)))
  cname <- if (!is.null(colname))   colname   else sub(".*\\$", "", deparse(substitute(col)))
  sname <- if (!is.null(stratname)) stratname else sub(".*\\$", "", deparse(substitute(strata)))

  use <- "con.table() can't cross it. Use multi.table() or bar.chart()."
  .stop_if_multi(row, rname, use)
  .stop_if_multi(col, cname, use)
  .stop_if_multi(strata, sname, use)

  ## blanks are skipped answers
  row <- .blank_na(row); col <- .blank_na(col); strata <- .blank_na(strata)

  ## the order rule uses each variable as it came in (before other =)
  rkind <- .cat_kind(row); rlv0 <- if (is.factor(row)) levels(row) else NULL
  ckind <- .cat_kind(col); clv0 <- if (is.factor(col)) levels(col) else NULL
  .check_order(order, last, unique(c(.answers(row), .answers(col))))
  order <- .as_names(order); last <- .as_names(last)

  ## other = k: combine small row categories and small column categories,
  ## judged by their totals in the table (respondents with row, col and strata)
  small <- character(0)
  if (!is.null(other)) {
    ok <- !is.na(row) & !is.na(col)
    if (!is.null(strata)) ok <- ok & !is.na(strata)
    ar <- .apply_other(row, other, table(factor(row[ok], levels = levels(as.factor(row)))))
    ac <- .apply_other(col, other, table(factor(col[ok], levels = levels(as.factor(col)))))
    row <- ar$x; col <- ac$x
    small <- c(ar$small, ac$small)
  }

  ## every category of row and col, so each strata panel has the same shape,
  ## in display order (by the row / column totals of the whole table)
  ok <- !is.na(row) & !is.na(col)
  if (!is.null(strata)) ok <- ok & !is.na(strata)
  rlev <- .order_levels(table(factor(row[ok], levels = levels(as.factor(row)))),
                        rkind, rlv0, order, last)
  clev <- .order_levels(table(factor(col[ok], levels = levels(as.factor(col)))),
                        ckind, clv0, order, last)
  rlab <- .cat_labels(rlev, .numbered(rlev, FALSE))     # numbered + wrapped if long

  fmt_cell <- function(count, tp = NA, rp = NA, cp = NA, margin = FALSE) {
    parts <- character(0)
    if (freq || margin) parts <- c(parts, formatC(round(count), format = "d", big.mark = ","))
    if (total.pct && is.finite(tp)) parts <- c(parts, sprintf("%.2f", tp))
    if (row.pct   && is.finite(rp)) parts <- c(parts, sprintf("%.2f", rp))
    if (col.pct   && is.finite(cp)) parts <- c(parts, sprintf("%.2f", cp))
    paste(parts, collapse = "\n")
  }

  panel <- function(r, c) {
    tab <- table(factor(r, levels = rlev), factor(c, levels = clev))
    rl  <- rownames(tab); cl <- colnames(tab)
    R <- length(rl); C <- length(cl)
    rowtot <- rowSums(tab); coltot <- colSums(tab); grand <- sum(tab)
    pct <- function(n, d) if (d == 0) 0 else n / d * 100    # 0.00 for empty levels
    rows <- list()
    for (i in seq_len(R)) {
      cells <- character(C + 2); cells[1] <- rlab[i]
      for (j in seq_len(C))
        cells[j + 1] <- fmt_cell(tab[i, j], tp = pct(tab[i, j], grand),
                                 rp = pct(tab[i, j], rowtot[i]), cp = pct(tab[i, j], coltot[j]))
      cells[C + 2] <- fmt_cell(rowtot[i], tp = pct(rowtot[i], grand), margin = TRUE)
      rows[[i]] <- cells
    }
    tr <- character(C + 2); tr[1] <- "Total"
    for (j in seq_len(C)) tr[j + 1] <- fmt_cell(coltot[j], tp = pct(coltot[j], grand), margin = TRUE)
    tr[C + 2] <- fmt_cell(grand, tp = if (grand == 0) 0 else 100, margin = TRUE)
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

  parts <- character(0)
  if (freq)      parts <- c(parts, "Frequency")
  if (total.pct) parts <- c(parts, "Total Pct")
  if (row.pct)   parts <- c(parts, "Row Pct")
  if (col.pct)   parts <- c(parts, "Col Pct")
  leg <- paste(parts, collapse = " | ")

  structure(list(
    colkeys = colkeys, header = header, align = align,
    spanners = list(list(label = cname, cols = cat_idx)),
    body = body, title = main,
    footnote = .join_notes(paste0("Cell contents: ", leg), .other_note(small, other)),
    note = note, first_col_divider = TRUE),
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
##   note      NULL (default) = none. Text printed under the table,
##             e.g. note = "Note: ...". Long notes wrap; "\n" starts a new line.
## =====================================================================

q.table <- function(data, vars = NULL, labels = NULL, type = "econ",
                    headers = "long", digits = 2, main = NULL, note = NULL) {

  if (!is.data.frame(data)) stop("data must be a data frame.")
  .check_note(note)
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
                 footnote = NULL, note = note, first_col_divider = TRUE),
            class = "btable")
}


## =====================================================================
## bar.chart()  —  bar chart for one or two categorical variables
##
##   x         a vector / data-frame column -> the x-axis categories
##   group     optional second categorical variable; when given, draws a
##             side-by-side (grouped) bar chart with a legend
##   main      optional title
##   xlab      x-axis label; defaults to the variable name from x
##   ylab      y-axis label (default "Frequency")
##   col       fill color(s); default BUSN.fill (one-way) or brand shades (grouped)
##   border    bar border color; default BUSN.border
##   wrap      wrap x-axis labels to this many characters (default 12)
##   cex.main  title font size (default BUSN.cex.main)
##   cex.axis  axis / label / category / legend font size (default BUSN.cex.axis)
##   legend.title   heading for the legend; defaults to the group variable name
##   mar       plot margins in lines: c(bottom, left, top, right).
##             NULL (default) = automatic: a tight top and a bottom that fits
##             the category labels, the x-axis label and any note.
##   other     NULL (default) = off. other = 5 combines categories with 5 or
##             fewer respondents into one "Other" bar, after the others -- the
##             same categories the matching table combines (freq.table /
##             multi.table, or con.table for a grouped chart, where the legend
##             groups are combined the same way).
##   note      NULL (default) = none. Text printed under the chart, below the
##             x-axis label, starting at the y-axis, e.g. note = "Note: ...".
##             The bottom margin grows to fit; long notes wrap at the end of
##             the x-axis; "\n" starts a new line.
##   order     NULL (default) = none. Answers to put first, in this order.
##   last      NULL (default) = none. Answers to put at the end, in this order.
##             (Grouped chart: each applies to x or group, whichever has the answer.)
##
## The bars follow the same order as the matching table (see "Category order"
## above): numbers in number order, a factor in its level order, text from
## most to least frequent. On a select-all question, or one with an answer
## longer than 30 characters, the bars are numbered 1, 2, 3, ... and the
## matching table (freq.table / multi.table / con.table) is the key.
## Colors and font sizes come from the CONFIG block at the top of this file.
## Leaves headroom above the tallest bar and wraps long labels so none are
## dropped. Blank answers are skipped. A select-all x gets one bar per option
## (respondents who checked it; grouped: each respondent counted once per
## option in their group). A select-all group stops with a message.
## The category labels, x-axis label, note and legend are re-placed whenever
## the chart is redrawn at another size (png.print, resizing the Plots pane),
## so they never overlap. If the Plots pane is too small for the chart (and its
## note), it stops with a message instead of drawing a broken chart.
## Pairs with png.print():
##   bar.chart(d1$q05_class_standing, main = "Class Standing")            # one-way
##   bar.chart(d1$q05_class_standing, d1$q09_working, main = "...")       # side-by-side
##   png.print("standing_bar.png")
## =====================================================================

## brand-consistent shades (navy -> lighter navy) for grouped bars
.tint <- function(hex, p) {                    # p = fraction toward white (0..1)
  v <- col2rgb(hex) / 255
  v <- v + (1 - v) * p
  rgb(v[1], v[2], v[3])
}
.bar_palette <- function(n) {
  if (n <= 1) return(BUSN.fill)
  colorRampPalette(c(BUSN.fill, .tint(BUSN.fill, 0.55)))(n)
}

## the last chart bar.chart() drew: its arguments, and the plot as drawn.
## png.print() draws it again at the PNG's own size while it is still the
## current plot; any other plot is copied as before.
.busn_bar <- new.env()

## the current plot's drawing instructions (NULL if there is no plot to record)
.plot_record <- function()
  tryCatch(suppressWarnings(recordPlot()[[1]]), error = function(e) NULL)

bar.chart <- function(x, group = NULL, main = NULL, xlab = NULL,
                      ylab = "Frequency", col = NULL, border = BUSN.border,
                      wrap = 12, cex.main = BUSN.cex.main, cex.axis = BUSN.cex.axis,
                      legend.title = NULL, mar = NULL,
                      other = NULL, note = NULL, order = NULL, last = NULL) {

  if (is.null(xlab)) xlab <- sub(".*\\$", "", deparse(substitute(x)))
  .check_note(note)
  grouped <- !is.null(group)
  if (grouped && is.null(legend.title))
    legend.title <- sub(".*\\$", "", deparse(substitute(group)))
  ## everything needed to draw this chart again (png.print() redraws it at the PNG's size)
  redo <- list(x = x, group = group, main = main, xlab = xlab, ylab = ylab, col = col,
               border = border, wrap = wrap, cex.main = cex.main, cex.axis = cex.axis,
               legend.title = legend.title, mar = mar, other = other, note = note,
               order = order, last = last)
  x       <- .blank_na(x)                     # blanks are skipped answers
  group   <- .blank_na(group)
  multi   <- .is_multi(x)
  xkind   <- .cat_kind(x); xlv <- if (is.factor(x)) levels(x) else NULL
  xans    <- if (multi) .multi_answers(x) else .answers(x)

  if (grouped) {
    .stop_if_multi(group, legend.title,
                   "use it as x (the first argument), not as group.")
    gkind <- .cat_kind(group); glv <- if (is.factor(group)) levels(group) else NULL
    .check_order(order, last, unique(c(xans, .answers(group))))
    order <- .as_names(order); last <- .as_names(last)
    if (multi) {                              # one (group, option) pair per box checked
      mc    <- .multi_counts(x, other, order, last)   # options as in multi.table()
      parts <- mc$parts
      if (!is.null(other)) {                  # groups combined by respondents who answered x
        has   <- lengths(parts) > 0L
        group <- .apply_other(group, other,
                              table(factor(group[has], levels = levels(as.factor(group)))))$x
      }
      g      <- factor(rep(as.character(group), lengths(parts)),
                       levels = levels(as.factor(group)))     # every group stays in the legend
      opts   <- unlist(parts); if (is.null(opts)) opts <- character(0)
      counts <- table(g, factor(opts, levels = names(mc$counts)))
      gtot   <- table(factor(group, levels = levels(g))[lengths(parts) > 0L])  # respondents per group
    } else {
      if (!is.null(other)) {                  # same combining as con.table(x, group, other =)
        ok    <- !is.na(x) & !is.na(group)
        x     <- .apply_other(x, other, table(factor(x[ok], levels = levels(as.factor(x)))))$x
        group <- .apply_other(group, other,
                              table(factor(group[ok], levels = levels(as.factor(group)))))$x
      }
      counts <- table(group, x)               # rows = legend groups, cols = x categories
      counts <- counts[, .order_levels(colSums(counts), xkind, xlv, order, last), drop = FALSE]
      gtot   <- rowSums(counts)                 # respondents per group (as in con.table)
    }
    counts <- counts[.order_levels(gtot, gkind, glv, order, last), , drop = FALSE]
    xcats  <- colnames(counts)
    glev   <- rownames(counts)
    if (is.null(col)) col <- .bar_palette(length(glev))
    ymax   <- max(counts, na.rm = TRUE)
    ytop   <- ymax * 1.30                      # extra room for the legend
  } else {
    .check_order(order, last, xans)
    order <- .as_names(order); last <- .as_names(last)
    if (multi) {
      counts <- .multi_counts(x, other, order, last)$counts
    } else {
      counts <- table(.apply_other(x, other)$x)
      counts <- counts[.order_levels(counts, xkind, xlv, order, last)]
    }
    xcats  <- names(counts)
    if (is.null(col)) col <- BUSN.fill
    ymax   <- max(counts, na.rm = TRUE)
    ytop   <- ymax * 1.15
  }

  ## numbered bars (select-all, or an answer longer than 30 characters); the
  ## matching table shows the same numbers as "1. answer"
  labs <- if (.numbered(xcats, multi)) as.character(seq_along(xcats)) else
    vapply(xcats, function(s) paste(strwrap(s, width = wrap), collapse = "\n"), character(1))

  ## margins, in lines (the same at every size): a tight top, and a bottom with
  ## room for the deepest label, the x-axis label and the note
  maxln <- max(1L, vapply(labs, function(s) length(strsplit(s, "\n", fixed = TRUE)[[1]]),
                          integer(1)))
  m <- if (is.null(mar)) {
    c(cex.axis * (maxln + 0.3) + (if (nzchar(xlab)) cex.axis * 1.1 else 0) + 0.3,
      4.5,
      if (is.null(main) || !nzchar(paste(main, collapse = ""))) 1 else 2.2,
      1) + 0.1
  } else mar
  note_lines <- NULL
  if (!is.null(note)) {                       # from the y-axis to the end of the x-axis
    note_lines <- .chart_note(note, cex.axis, par("din")[1] - (m[2] + m[4]) * par("csi"))
    m[1] <- m[1] + cex.axis * (length(note_lines) + 0.4)
  }
  if ((m[1] + m[3]) * par("csi") > par("din")[2] - 0.6)
    stop("The Plots pane is too small for this chart",
         if (!is.null(note)) " and its note" else "",
         ". Make the Plots pane bigger",
         if (!is.null(note)) ", or shorten the note" else "", ".", call. = FALSE)

  op <- par(mar = m); on.exit(par(op))        # set plainly, before the chart starts
  tk <- environment(.chart_note)              # where the toolkit's helpers live

  mp <- barplot(counts, beside = grouped, axisnames = FALSE,
                main = main, xlab = "", ylab = ylab,
                col = col, border = border, ylim = c(0, ytop),
                cex.axis = cex.axis, cex.lab = cex.axis, cex.main = cex.main)

  ## category labels, xlab and note, placed in the SAME coordinate system so the
  ## gaps stay tight whether a label wraps or not, at any plot height.
  ## recordGraphics() re-runs this whenever the chart is redrawn at another size
  ## (png.print, resizing the Plots pane), so the spacing is always recomputed.
  at <- if (grouped) colMeans(mp) else as.vector(mp)
  recordGraphics({
    .fresh_metrics()                                       # measure on THIS device
    lh    <- strheight("Ag", cex = cex.axis)               # one text line, in y units
    lab_h <- max(strheight(labs, cex = cex.axis))          # deepest wrapped label, as drawn
    y_lab <- par("usr")[3] - 0.4 * lh                      # small gap below the axis
    text(at, y_lab, labels = labs, xpd = NA, adj = c(0.5, 1), cex = cex.axis)
    ## xlab sits a fixed gap below the deepest (possibly wrapped) label
    y_x   <- y_lab - lab_h - 0.5 * lh
    text(mean(par("usr")[1:2]), y_x,
         xlab, xpd = NA, adj = c(0.5, 1), cex = cex.axis)
    ## note: a gap below the xlab, lined up with the y-axis, in the lines wrapped above
    if (!is.null(note_lines)) {
      y_n <- y_x - (if (nzchar(xlab)) strheight(xlab, cex = cex.axis) else 0) - 0.8 * lh
      text(par("usr")[1], y_n,
           paste(note_lines, collapse = "\n"),
           xpd = NA, adj = c(0, 1), cex = cex.axis, col = "gray30")
    }
  }, list(at = at, labs = labs, xlab = xlab, cex.axis = cex.axis, note_lines = note_lines), tk)

  if (grouped)
    recordGraphics(
      legend("topright", legend = glev, fill = col, border = border,
             title = legend.title, bty = "n", cex = cex.axis, inset = 0.01),
      list(glev = glev, col = col, border = border, legend.title = legend.title,
           cex.axis = cex.axis),
      getNamespace("graphics"))

  ## put the margins back now (not on exit), then remember this chart exactly as
  ## drawn, so png.print() can tell whether it is still the current plot
  par(op); on.exit()
  .busn_bar$last <- list(args = redo, dl = .plot_record())
  invisible(mp)
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
##   res           output resolution (dots per inch)
##   width/height  plots only: size in INCHES; default 8 x 5, whatever the size
##                 of the Plots pane. width = NULL, height = NULL uses the
##                 current Plots pane size instead.
##
## A chart from bar.chart() is drawn again on the PNG (while it is still the
## current plot), so its margins and note are worked out for the PNG's size.
## Any other plot is copied from the Plots pane.
## =====================================================================

png.print <- function(x, file = NULL, res = 200, width = 8, height = 5) {

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
    if (is.null(width) || is.null(height)) {
      d <- dev.size("in")          # match the current (possibly resized) plot window
      width <- d[1]; height <- d[2]
    }
    last <- .busn_bar$last
    if (!is.null(last) && !is.null(last$dl) && all(par("mfrow") == 1L) &&
        identical(.plot_record(), last$dl)) {
      ## the current plot is the last bar.chart(): draw it again on the PNG, so
      ## its margins and note are worked out for the PNG's size
      cur <- dev.cur()
      png(file, width = width, height = height, units = "in", res = res, bg = "white")
      tryCatch(do.call(bar.chart, last$args),
               finally = { dev.off(); if (cur > 1) dev.set(cur); .busn_bar$last <- last })
    } else {                       # any other plot: copy it to the PNG
      dev.copy(png, file, width = width, height = height, units = "in", res = res)
      dev.off()
    }
  }
  invisible(if (is_table) x else NULL)
}
