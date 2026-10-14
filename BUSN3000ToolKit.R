## #####################################################################
## BUSN 3000H  —  Table Toolkit   (pure base R, ZERO packages)
## VERSION V3.3
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

## option counts (respondents who checked each) and n (respondents who answered).
## With other = k, options checked by k or fewer respondents become "Other",
## and "Other" counts each respondent once (not once per small option).
.multi_counts <- function(x, other = NULL) {
  parts <- .split_multi(x)
  picks <- unlist(parts)
  if (is.null(picks)) picks <- character(0)
  if (is.factor(x)) {                                     # keep a factor's level order
    lv <- unique(trimws(levels(x))); lv <- lv[lv != ""]
    if (all(picks %in% lv)) picks <- factor(picks, levels = lv)
  }
  counts <- table(picks)
  small  <- character(0)
  if (!is.null(other)) {
    .check_other(other)
    small <- .small_cats(counts, other)
    lv    <- names(counts)
    if (length(small) || "Other" %in% lv) {
      parts  <- lapply(parts, function(p) { p[p %in% small] <- "Other"; unique(p) })
      keep   <- setdiff(lv, c(small, "Other"))
      picks  <- unlist(parts)
      if (is.null(picks)) picks <- character(0)
      counts <- table(factor(picks, levels = c(keep, "Other")))
    }
  }
  list(counts = counts, n = sum(lengths(parts) > 0L), parts = parts, small = small)
}

## the Option | Frequency | Percent of Respondents table (freq.table and multi.table)
.multi_btable <- function(x, main, varname, other = NULL, note = NULL) {
  .check_note(note)
  mc <- .multi_counts(x, other)
  n  <- mc$n
  if (n == 0L) stop("Every value of x is blank or NA: no one answered this question.",
                    call. = FALSE)

  Frequency <- as.integer(mc$counts)
  Percent   <- 100 * Frequency / n
  lev       <- names(mc$counts)

  body <- lapply(seq_along(lev), function(i) list(type = "data", cells = c(
    lev[i],
    formatC(Frequency[i], format = "d", big.mark = ","),
    formatC(Percent[i],   format = "f", digits = 2))))

  structure(list(
    colkeys = c("level", "Frequency", "PctResp"),
    header  = c(varname, "Frequency", "Percent of Respondents"),
    align   = c("left", "right", "right"),
    spanners = list(), body = body,
    title = main,
    footnote = .join_notes(
      paste0("n = ", formatC(n, format = "d", big.mark = ","), " respondents",
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
##   category, placed last. A category already named "Other" absorbs them.
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

## a chart note split into lines that fit the chart's width ("\n" = new line).
## Uses the current figure width, so it is recomputed at every size.
.chart_note <- function(note, cex) {
  width <- max(10, floor((par("fin")[1] - 0.3) / (0.5 * par("cin")[2] * cex)))
  paras <- strsplit(paste(note, collapse = "\n"), "\n", fixed = TRUE)[[1]]
  unlist(lapply(paras, function(p) if (nzchar(trimws(p))) strwrap(p, width) else ""))
}


## =====================================================================
## freq.table()  —  one-way frequency table
##
##   x         a vector / data-frame column (e.g. d1$q05_class_standing)
##   main      optional title; NULL (default) = no title
##   varname   heading for the first column; defaults to the variable name
##             from x (d1$REGION -> "REGION"). Override when x was reassigned.
##   other     NULL (default) = off. other = 5 puts categories with 5 or fewer
##             respondents into one "Other" row, placed last.
##   note      NULL (default) = none. Text printed as the last line under the
##             table, e.g. note = "Note: ...". Long notes wrap; "\n" starts a new line.
##
## Columns: Frequency, Percent, Cumulative Frequency, Cumulative Percent
## Blank answers are skipped (not counted). A select-all column gets the
## multi.table() layout instead (Frequency and Percent of Respondents; no
## cumulative columns, since percents can sum to more than 100).
## =====================================================================

freq.table <- function(x, main = NULL, varname = NULL, other = NULL, note = NULL) {

  if (is.null(varname)) varname <- sub(".*\\$", "", deparse(substitute(x)))
  .check_note(note)
  x <- .blank_na(x)
  if (.is_multi(x)) return(.multi_btable(x, main, varname, other, note))

  ao        <- .apply_other(x, other)
  counts    <- table(ao$x)
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
    title = main, footnote = .other_note(ao$small, other), note = note,
    first_col_divider = TRUE),
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
##             fewer respondents into one "Other" row, placed last; "Other"
##             counts each respondent once, however many small options they checked.
##   note      NULL (default) = none. Text printed as the last line under the
##             table, e.g. note = "Note: ...". Long notes wrap; "\n" starts a new line.
##
## Blank and NA cells are respondents who skipped the question; n = the
## number of respondents who answered. Each cell is split at ";" and each
## option is counted once per respondent. On a single-choice column each cell
## is one option, so the percents sum to 100.
##
## Columns: Frequency (respondents who checked the option) and Percent of
## Respondents (Frequency / n). Percents can sum to more than 100.
## Row order follows factor levels, as in freq.table().
## =====================================================================

multi.table <- function(x, main = NULL, varname = NULL, other = NULL, note = NULL) {

  if (is.null(varname)) varname <- sub(".*\\$", "", deparse(substitute(x)))
  .multi_btable(x, main, varname, other, note)
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
##                placed last). The strata variable is never combined.
##   note         NULL (default) = none. Text printed as the last line under the
##                table, e.g. note = "Note: ...". Long notes wrap; "\n" starts a new line.
##
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
                    note = NULL) {

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

  ## every category of row and col, so each strata panel has the same shape
  rlev <- levels(as.factor(row))
  clev <- levels(as.factor(col))

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
      cells <- character(C + 2); cells[1] <- rl[i]
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
##   mar       plot margins in lines: c(bottom, left, top, right);
##             default c(5.5, 4.5, 4, 2) + 0.1
##   other     NULL (default) = off. other = 5 combines categories with 5 or
##             fewer respondents into one "Other" bar, placed last -- the same
##             categories the matching table combines (freq.table / multi.table,
##             or con.table for a grouped chart, where the legend groups are
##             combined the same way).
##   note      NULL (default) = none. Text printed at the bottom left, below
##             the x-axis label, e.g. note = "Note: ...". The bottom margin
##             grows to fit; long notes wrap; "\n" starts a new line.
##
## Colors and font sizes come from the CONFIG block at the top of this file.
## Leaves headroom above the tallest bar and wraps long labels so none are
## dropped. Blank answers are skipped. A select-all x gets one bar per option
## (respondents who checked it; grouped: each respondent counted once per
## option in their group). A select-all group stops with a message.
## The category labels, x-axis label and legend are re-placed whenever the
## chart is redrawn at another size (png.print, resizing the Plots pane), so
## they never overlap. Pairs with png.print():
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

bar.chart <- function(x, group = NULL, main = NULL, xlab = NULL,
                      ylab = "Frequency", col = NULL, border = BUSN.border,
                      wrap = 12, cex.main = BUSN.cex.main, cex.axis = BUSN.cex.axis,
                      legend.title = NULL, mar = c(5.5, 4.5, 4, 2) + 0.1,
                      other = NULL, note = NULL) {

  if (is.null(xlab)) xlab <- sub(".*\\$", "", deparse(substitute(x)))
  .check_note(note)
  grouped <- !is.null(group)
  if (grouped && is.null(legend.title))
    legend.title <- sub(".*\\$", "", deparse(substitute(group)))
  x       <- .blank_na(x)                     # blanks are skipped answers
  group   <- .blank_na(group)
  multi   <- .is_multi(x)

  if (grouped) {
    .stop_if_multi(group, legend.title,
                   "use it as x (the first argument), not as group.")
    if (multi) {                              # one (group, option) pair per box checked
      mc    <- .multi_counts(x, other)        # options combined as in multi.table()
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
    } else {
      if (!is.null(other)) {                  # same combining as con.table(x, group, other =)
        ok    <- !is.na(x) & !is.na(group)
        x     <- .apply_other(x, other, table(factor(x[ok], levels = levels(as.factor(x)))))$x
        group <- .apply_other(group, other,
                              table(factor(group[ok], levels = levels(as.factor(group)))))$x
      }
      counts <- table(group, x)               # rows = legend groups, cols = x categories
    }
    xcats  <- colnames(counts)
    glev   <- rownames(counts)
    if (is.null(col)) col <- .bar_palette(length(glev))
    ymax   <- max(counts, na.rm = TRUE)
    ytop   <- ymax * 1.30                      # extra room for the legend
  } else {
    counts <- if (multi) .multi_counts(x, other)$counts else table(.apply_other(x, other)$x)
    xcats  <- names(counts)
    if (is.null(col)) col <- BUSN.fill
    ymax   <- max(counts, na.rm = TRUE)
    ytop   <- ymax * 1.15
  }

  labs <- vapply(xcats, function(s) paste(strwrap(s, width = wrap), collapse = "\n"),
                 character(1))

  op <- par("mar"); on.exit(par(mar = op))
  tk <- environment(.chart_note)              # where the toolkit's helpers live
  ## bottom margin: mar, plus room for the note (recomputed at every redraw size)
  recordGraphics({
    m <- mar
    if (!is.null(note)) m[1] <- m[1] + length(.chart_note(note, cex.axis)) * cex.axis + 0.8
    par(mar = m)
  }, list(mar = mar, note = note, cex.axis = cex.axis), tk)

  mp <- barplot(counts, beside = grouped, axisnames = FALSE,
                main = main, xlab = "", ylab = ylab,
                col = col, border = border, ylim = c(0, ytop),
                cex.axis = cex.axis, cex.lab = cex.axis, cex.main = cex.main)

  ## wrapped category labels + xlab, placed in the SAME coordinate system so the
  ## gap stays tight whether a label wraps or not, at any plot height.
  ## recordGraphics() re-runs this whenever the chart is redrawn at another size
  ## (png.print, resizing the Plots pane), so the spacing is always recomputed.
  at <- if (grouped) colMeans(mp) else as.vector(mp)
  recordGraphics({
    lh    <- strheight("Ag", cex = cex.axis)               # one text line, in y units
    lab_h <- max(strheight(labs, cex = cex.axis))          # deepest wrapped label, as drawn
    y_lab <- par("usr")[3] - 0.4 * lh                      # small gap below the axis
    text(at, y_lab, labels = labs, xpd = NA, adj = c(0.5, 1), cex = cex.axis)
    ## xlab sits a fixed gap below the deepest (possibly wrapped) label
    y_x   <- y_lab - lab_h - 0.5 * lh
    text(mean(par("usr")[1:2]), y_x,
         xlab, xpd = NA, adj = c(0.5, 1), cex = cex.axis)
    ## note: bottom left, a gap below the xlab, wrapped to the chart width
    if (!is.null(note)) {
      y_n <- y_x - strheight(xlab, cex = cex.axis) - 0.8 * lh
      text(grconvertX(0.15, from = "inches", to = "user"), y_n,
           paste(.chart_note(note, cex.axis), collapse = "\n"),
           xpd = NA, adj = c(0, 1), cex = cex.axis, col = "gray30")
    }
  }, list(at = at, labs = labs, xlab = xlab, cex.axis = cex.axis, note = note), tk)

  if (grouped)
    recordGraphics(
      legend("topright", legend = glev, fill = col, border = border,
             title = legend.title, bty = "n", cex = cex.axis, inset = 0.01),
      list(glev = glev, col = col, border = border, legend.title = legend.title,
           cex.axis = cex.axis),
      getNamespace("graphics"))

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
    dev.copy(png, file, width = width, height = height, units = "in", res = res)
    dev.off()
  }
  invisible(if (is_table) x else NULL)
}
