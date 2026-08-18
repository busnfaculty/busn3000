# ============================================================
#  BUSN 3000  -  Class Activity 02:  Selection Bias
#  Dart-throwing sampling simulator
# ============================================================
#
#  STUDENTS:  Run this ENTIRE file once to load the tool
#             (Code > Run Region > Run All, or Ctrl/Cmd + Shift + Enter).
#             Then follow the handout. You only ever edit the small
#             "SET UP THIS RUN" block shown on the handout, e.g.:
#
#                 scenario <- "random_homeowners"
#                 n        <- 20
#                 run_experiment(scenario, n)
#
#  INSTRUCTOR: Everything you might want to change lives in the two
#              clearly-marked config sections below (DATA and CONFIG).
#              To swap in a different population, change DATA_FILE.
# ============================================================


# ------------------------------------------------------------
#  DATA  (instructor: swap the population here)
# ------------------------------------------------------------
DATA_URL  <- "https://raw.githubusercontent.com/busnfaculty/busn3000/main/"
DATA_FILE <- "sframe2.csv"                 # population file (swap to the 150-row set here)
VALUE_COL <- "Val_2024_CurrentValue"       # the variable we survey (home value, USD)
YEAR_COL  <- "YearBuilt"                    # used by one scenario's frame

# Load the population. Uses a local file if present, otherwise the URL.
.load_population <- function() {
  src <- if (file.exists(DATA_FILE)) DATA_FILE else paste0(DATA_URL, DATA_FILE)
  pop <- tryCatch(read.csv(src),
                  error = function(e) stop(
                    "Could not load the population data from:\n  ", src,
                    "\nCheck your internet connection or the DATA_FILE setting.",
                    call. = FALSE))
  pop
}

.POP    <- .load_population()
.VALUES <- .POP[[VALUE_COL]]
.YEARS  <- .POP[[YEAR_COL]]
.MU     <- mean(.VALUES)                    # the "truth" (unknown in real life)
.N      <- length(.VALUES)


# ------------------------------------------------------------
#  CONFIG  (instructor: timing, dart look, labels)
#  Every value here can also be overridden per call, e.g.
#      run_experiment("random_homeowners", n = 20, reps = 80)
# ------------------------------------------------------------
config <- list(

  ## how many darts (= how many repeated samples)
  reps            = 50,

  ## --- ANIMATION TIMING (seconds) ---
  pause_line      = 0.80,   # beat after the number line is drawn
  pause_bull      = 1.00,   # beat after the bullseye (truth) drops
  pause_start     = 0.28,   # gap between the first, slow, watchable darts
  pause_min       = 0.03,   # fastest gap the darts accelerate down to
  accelerate_after= 8,      # throw this many darts slowly before speeding up
  decay           = 0.82,   # 0-1: how quickly the gap shrinks once accelerating
                            #      (smaller = accelerates faster)

  ## --- DART LOOK ---
  show_streak     = TRUE,   # faint motion streak above each landing dart
  dart_len        = 0.150,  # dart length (vertical units; bigger = longer dart)
  dart_tilt       = 0.030,  # sideways lean as a fraction of the x-range (0 = straight down)
  stack_dy        = 0.052,  # vertical gap between darts stacked at the same value
  stack_binfrac   = 0.010,  # two sample means within this fraction of the x-range stack together

  ## --- COLORS ---
  col_line        = "black",
  col_bull_ring   = "#C0182A",
  col_bull_center = "#FFD21A",
  col_shaft       = "#8A8F98",
  col_shaft_hi    = "#D9DDE2",
  col_barrel      = "#C0182A",
  col_flight      = "#C0182A",
  col_flight_edge = "#7D0F1C",
  col_streak      = "#CFCFCF",

  ## --- LABELS ---
  mu_label        = "\u03bc (truth)",      # the Greek letter mu + "(truth)"
  x_label         = "average home value",
  money_k         = TRUE                    # label the axis in $k (e.g., $300k)
)


# ------------------------------------------------------------
#  SCENARIOS
#  Each scenario returns sampling weights over the population and a
#  neutral, method-based title. The weights encode HOW homeowners are
#  reached -- they never name the type of bias (that's the student's job).
# ------------------------------------------------------------
.scenario_spec <- function(scenario) {
  v <- .VALUES
  spec <- switch(scenario,

    "random_homeowners" = list(
      weights  = rep(1, .N),                       # everyone equally likely  (fair)
      title    = "Random Homeowners",
      subtitle = "computer picks homeowners at random from the complete Athens list"),

    "open_online_survey" = list(
      weights  = v^3,                              # pricier homes over-respond (tax anger)
      title    = "Open Online Survey",
      subtitle = "homeowners choose whether to opt in"),

    "outdated_address_list" = list(
      weights  = as.numeric(.YEARS <= 2005),       # frame excludes homes built after 2005
      title    = "Outdated Address List",
      subtitle = "random selection, but only from a 2005 list"),

    "sidewalk_intercept" = list(
      weights  = as.numeric(v >= quantile(v, 0.60)),  # only the upscale part of town
      title    = "Sidewalk Survey",
      subtitle = "whoever walks past one location"),

    "phone_refusals" = list(
      weights  = (max(v) - v + 1),                 # priciest homeowners tend to refuse
      title    = "Phone Survey",
      subtitle = "complete list, random calls, but the priciest owners refuse"),

    stop("Unknown scenario: '", scenario, "'. Check the spelling on your handout.",
         call. = FALSE)
  )
  if (all(spec$weights == 0)) stop("Scenario frame is empty.", call. = FALSE)
  spec
}


# ------------------------------------------------------------
#  Internal drawing helpers
# ------------------------------------------------------------

# Draw one dart with its TIP pointing down onto (x, ybase).
# Shaft and flights rise up-and-slightly-right, so it reads as
# a dart that just came down onto the number line.
.draw_dart <- function(x, ybase, cfg, xr) {
  dx <- cfg$dart_tilt * xr           # horizontal run of the whole dart
  dy <- cfg$dart_len                 # vertical rise of the whole dart
  bx <- x  + dx;  by <- ybase + dy   # back of the dart (up-right)

  # shaft (grey with a thin light highlight)
  segments(x, ybase, bx, by, col = cfg$col_shaft,    lwd = 3, lend = 1)
  segments(x, ybase, bx, by, col = cfg$col_shaft_hi, lwd = 1, lend = 1)

  # red barrel near the tip
  mx <- x + 0.30 * dx;  my <- ybase + 0.30 * dy
  segments(x, ybase, mx, my, col = cfg$col_barrel, lwd = 5, lend = 1)

  # flight: a small red triangle at the back of the shaft
  fx <- x + 0.82 * dx;  fy <- ybase + 0.82 * dy
  hw <- 0.045 * xr                                  # flight half-width in x-units
  polygon(c(bx, fx - hw, fx + hw),
          c(by, fy - 0.02, fy - 0.02),
          col = cfg$col_flight, border = cfg$col_flight_edge, lwd = 0.8)

  # the tip itself (the data point)
  points(x, ybase, pch = 19, cex = 0.35, col = "#333333")
}

# Nicely-spaced axis tick locations covering [lo, hi]
.nice_ticks <- function(lo, hi) {
  span <- hi - lo
  step <- 10^floor(log10(span))
  if (span / step < 3) step <- step / 2
  if (span / step > 8) step <- step * 2
  seq(floor(lo / step) * step, ceiling(hi / step) * step, by = step)
}


# ------------------------------------------------------------
#  MAIN FUNCTION
# ------------------------------------------------------------
run_experiment <- function(scenario,
                           n        = 20,
                           reps     = config$reps,
                           title    = NULL,      # NULL = use the scenario's neutral title
                           subtitle = NULL,
                           cfg      = config) {

  spec <- .scenario_spec(scenario)
  if (is.null(title))    title    <- spec$title
  if (is.null(subtitle)) subtitle <- spec$subtitle

  ## 1) Simulate: draw `reps` samples of size n (WITH replacement) and
  ##    record each sample mean. (With replacement so any sample size
  ##    works on any scenario, and to mimic drawing from a large population.)
  means <- numeric(reps)
  for (i in seq_len(reps)) {
    idx      <- sample.int(.N, n, replace = TRUE, prob = spec$weights)
    means[i] <- mean(.VALUES[idx])
  }

  ## 2) Set up the plotting window (axis fixed up front so nothing rescales
  ##    mid-animation). Axis spans the truth AND all the darts, with padding.
  lo   <- min(c(means, .MU));  hi <- max(c(means, .MU))
  pad  <- 0.18 * (hi - lo + 1)
  axlo <- lo - pad;  axhi <- hi + pad
  xr   <- axhi - axlo

  # stacking: assign each dart a height so nearby means pile upward
  binw   <- cfg$stack_binfrac * xr
  bins   <- round(means / binw)
  ybase  <- numeric(reps); seen <- list()
  for (i in seq_len(reps)) {
    b  <- as.character(bins[i])
    k  <- if (is.null(seen[[b]])) 0 else seen[[b]]
    ybase[i] <- k * cfg$stack_dy
    seen[[b]] <- k + 1
  }
  ytop <- max(ybase) + cfg$dart_len + 0.05

  op <- par(no.readonly = TRUE); on.exit(par(op))
  par(mar = c(3.4, 1.0, 3.2, 1.0))
  plot(NA, xlim = c(axlo, axhi), ylim = c(-0.42, max(0.85, ytop)),
       axes = FALSE, xlab = "", ylab = "")

  # titles
  title(main = title, cex.main = 1.5, font.main = 2, line = 1.4)
  if (nzchar(subtitle)) mtext(subtitle, side = 3, line = 0.1, cex = 0.95, col = "#555555")

  ## 3) Draw the number line (with arrowheads + ticks + $ labels), then pause
  y0 <- 0
  x1 <- axlo + 0.03 * xr;  x2 <- axhi - 0.03 * xr
  segments(x1, y0, x2, y0, col = cfg$col_line, lwd = 5, lend = 1)
  ah <- 0.022 * xr; av <- 0.045
  polygon(c(x1, x1 + ah, x1 + ah), c(y0, y0 + av, y0 - av), col = cfg$col_line, border = NA)
  polygon(c(x2, x2 - ah, x2 - ah), c(y0, y0 + av, y0 - av), col = cfg$col_line, border = NA)

  ticks <- .nice_ticks(x1, x2); ticks <- ticks[ticks >= x1 & ticks <= x2]
  segments(ticks, y0 - 0.028, ticks, y0 + 0.028, col = cfg$col_line, lwd = 2)
  labs <- if (cfg$money_k) paste0("$", round(ticks / 1000), "k") else format(round(ticks))
  text(ticks, y0 - 0.10, labs, cex = 1.0, font = 2)
  mtext(cfg$x_label, side = 1, line = 2.0, cex = 0.95, col = "#555555")

  Sys.sleep(cfg$pause_line)

  ## 4) Drop the bullseye at the truth (mu), then pause
  ring <- 0.055
  symbols(.MU, y0, circles = ring,        add = TRUE, inches = FALSE,
          bg = cfg$col_bull_ring, fg = cfg$col_flight_edge)
  symbols(.MU, y0, circles = ring * 0.42, add = TRUE, inches = FALSE,
          bg = cfg$col_bull_center, fg = NA)
  text(.MU, y0 - 0.20, cfg$mu_label, col = cfg$col_flight_edge, font = 2, cex = 1.05)
  Sys.sleep(cfg$pause_bull)

  ## 5) Rain the darts down one at a time, accelerating
  gap <- cfg$pause_start
  for (i in seq_len(reps)) {
    if (cfg$show_streak) {                # faint motion streak above the landing spot
      sx <- means[i] + cfg$dart_tilt * xr * 0.5
      segments(sx, ybase[i] + cfg$dart_len + 0.02,
               sx, ybase[i] + cfg$dart_len + 0.16,
               col = cfg$col_streak, lwd = 2, lend = 1)
    }
    .draw_dart(means[i], ybase[i], cfg, xr)

    Sys.sleep(gap)
    if (i >= cfg$accelerate_after) gap <- max(cfg$pause_min, gap * cfg$decay)
  }

  ## 6) Return the sample means invisibly (handy if you want them)
  invisible(list(scenario = scenario, n = n, reps = reps,
                 means = means, mu = .MU))
}


# ------------------------------------------------------------
message("Tool loaded. Population: ", .N, " homeowners.  ",
        "Now follow the handout and run each 'SET UP THIS RUN' block.")
# ============================================================
