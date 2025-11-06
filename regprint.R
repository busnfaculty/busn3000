#BUSN 4000 - V.1.0.5 - SPR26

regprint <- function(model, conf_level = 95, digits = NULL,
                     robust = c("none","HC0","HC1","HC2","HC3","HC4","HC4m","HC5"),
                     std_beta = FALSE) {

  # ---- validations ----
  if (!inherits(model, "lm")) {
    obj <- deparse(substitute(model))
    stop(sprintf("Error: %s is not a lm object. regprint() requires you fit a linear model first using the lm() function and save it as an object.", obj), call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1 || !is.finite(conf_level) ||
      conf_level < 1 || conf_level > 99) {
    stop("Error: 'conf_level' must be a single number between 1 and 99 (e.g., 90, 95).", call. = FALSE)
  }
  if (!is.null(digits)) {
    if (!is.numeric(digits) || length(digits) != 1 || !is.finite(digits) ||
        digits < 0 || digits != as.integer(digits)) {
      stop("Error: 'digits' must be a single nonnegative integer (e.g., 3, 4, 5).", call. = FALSE)
    }
  }
  robust <- match.arg(robust)

  level <- conf_level / 100
  lb_name <- sprintf("%d%% CI LB", conf_level)
  ub_name <- sprintf("%d%% CI UB", conf_level)

  # ---- formatting helpers (thousands separators) ----
  k_p  <- if (is.null(digits)) 4L else digits
  k_r2 <- if (is.null(digits)) 4L else digits
  k_oth<- if (is.null(digits)) 3L else digits

  fmt_fix <- function(x, k) ifelse(is.na(x), "NA", formatC(x, format="f", digits=k, big.mark=","))
  fmt4  <- function(x) fmt_fix(x, k_r2)   # Multiple R, R^2, Adj R^2
  fmt3  <- function(x) fmt_fix(x, k_oth)  # Everything else
  fmt_p <- function(p) {
    thr <- 10^(-k_p)
    below <- paste0("<", formatC(thr, format="f", digits=k_p, big.mark=""))
    ifelse(p < thr, below, formatC(p, format="f", digits=k_p, big.mark=""))
  }
  fmt_int <- function(x) ifelse(is.na(x), "NA", formatC(as.integer(x), format="d", big.mark=","))

  # ---- core pieces ----
  y_name <- as.character(formula(model))[2]
  sm <- summary(model)

  r2 <- sm$r.squared; adjr <- sm$adj.r.squared
  multR <- sqrt(r2); s_err <- sm$sigma
  n_used <- length(sm$residuals)
  n_miss <- if (is.null(model$na.action)) 0L else length(model$na.action)
  n_read <- n_used + n_miss

  mf <- model.frame(model)
  y  <- model.response(mf)
  e  <- resid(model)
  sse <- sum(e^2); sst <- sum((y - mean(y))^2); ssr <- sst - sse

  df_reg <- unname(sm$fstatistic["numdf"])
  df_res <- unname(sm$fstatistic["dendf"])
  df_tot <- df_reg + df_res
  ms_reg <- ssr / df_reg; ms_res <- sse / df_res
  fstat  <- unname(sm$fstatistic["value"])
  p_f    <- pf(fstat, df_reg, df_res, lower.tail = FALSE)

  # ---- coefficients (classic vs robust) ----
  coefs <- coef(model)
  if (robust == "none") {
    se    <- sm$coefficients[, "Std. Error"]
    tvals <- sm$coefficients[, "t value"]
    pvals <- sm$coefficients[, "Pr(>|t|)"]
    ci    <- confint(model, level = level)
  } else {
    if (!requireNamespace("sandwich", quietly = TRUE)) {
      stop("Robust SEs require package 'sandwich'. Install with install.packages('sandwich').", call. = FALSE)
    }
    V  <- sandwich::vcovHC(model, type = robust)
    se <- sqrt(diag(V))
    tvals <- coefs / se
    pvals <- 2 * pt(abs(tvals), df = df_res, lower.tail = FALSE)
    tcrit <- qt(1 - (1 - level)/2, df = df_res)
    ci <- cbind(coefs - tcrit * se, coefs + tcrit * se)
    colnames(ci) <- c("low","high")
  }

  ctab <- data.frame(
    "Coefficients"    = coefs,
    "Standard Error"  = se,
    "t-test"          = tvals,
    "p-value"         = pvals,
    check.names = FALSE
  )
  ctab[[lb_name]] <- ci[, 1]
  ctab[[ub_name]] <- ci[, 2]
  rownames(ctab) <- sub("^\\(Intercept\\)$", "Intercept", rownames(ctab))

  # ---- standardized betas ----
  if (isTRUE(std_beta)) {
    X <- model.matrix(model)
    sdy <- stats::sd(y)
    sdx <- apply(X, 2, stats::sd)
    stdb <- rep(NA_real_, length(coefs))
    idx <- which(colnames(X) != "(Intercept)")
    if (length(idx)) stdb[idx] <- coefs[idx] * sdx[idx] / sdy
    names(stdb) <- rownames(ctab)
    ctab$`Std. Beta` <- stdb
  }

  # ---- VIFs (exclude intercept) ----
  X <- model.matrix(model)
  pred_cols <- setdiff(colnames(X), "(Intercept)")
  Xp <- if (length(pred_cols)) X[, pred_cols, drop = FALSE] else NULL

  vif <- rep(NA_real_, nrow(ctab))  # NA for Intercept
  if (!is.null(Xp)) {
    if (ncol(Xp) == 1L) {
      vif[match(colnames(Xp), rownames(ctab))] <- 1
    } else {
      v <- numeric(ncol(Xp))
      for (j in seq_len(ncol(Xp))) {
        r2j <- summary(lm(Xp[, j] ~ Xp[, -j]))$r.squared
        v[j] <- 1 / (1 - r2j)
      }
      vif[match(colnames(Xp), rownames(ctab))] <- v
    }
  }
  ctab$VIF <- vif

  # helper: build format string with ≥10% spacing and a 3-space minimum
  build_fmt <- function(widths) {
    if (length(widths) == 1L) return(paste0("%", widths, "s\n"))
    sp <- pmax(3L, ceiling(0.10 * widths[-1]))
    paste0("%-", widths[1], "s",
           paste(mapply(function(s, w) paste0(strrep(" ", s), "%", w, "s"),
                        sp, widths[-1]), collapse = ""),
           "\n")
  }

  # ---- printing ----
  obj_name <- deparse(substitute(model))
  cat(sprintf("Linear Regression Model: %s\n", obj_name))
  cat(sprintf("Dependent Variable: %s\n\n", y_name))

  # Regression Statistics
  cat("Regression Statistics Table\n")
  rs_names <- c("Multiple R","R Square","Adjusted R Square",
                "Standard Error","N Obs Read","N Obs Missing","N Obs Used")
  rs_vals  <- c(fmt4(multR), fmt4(r2), fmt4(adjr), fmt3(s_err),
                fmt_int(n_read), fmt_int(n_miss), fmt_int(n_used))
  rs_w <- c(max(nchar(rs_names)), max(nchar(rs_vals)))
  rs_fmt <- build_fmt(rs_w)
  for (i in seq_along(rs_names)) cat(sprintf(rs_fmt, rs_names[i], rs_vals[i]))
  cat("\n")

  # ANOVA
  cat("ANOVA Table\n")
  a_hdr <- c("Source","df","SS","MS","F-Statistic","p-value")
  a_src <- c("Regression","Residual","Total")
  a_df  <- c(fmt_int(df_reg), fmt_int(df_res), fmt_int(df_tot))
  a_ss  <- c(fmt3(ssr), fmt3(sse), fmt3(sst))
  a_ms  <- c(fmt3(ms_reg), fmt3(ms_res), "")
  a_F   <- c(fmt3(fstat), "", "")
  a_p   <- c(fmt_p(p_f), "", "")
  a_cols <- list(a_src, a_df, a_ss, a_ms, a_F, a_p)
  a_w <- integer(length(a_cols))
  for (i in seq_along(a_cols)) a_w[i] <- max(nchar(a_hdr[i]), max(nchar(a_cols[[i]]), na.rm = TRUE))
  a_fmt <- build_fmt(a_w)
  cat(do.call(sprintf, c(a_fmt, as.list(a_hdr))))
  for (i in seq_along(a_src)) {
    row <- list(a_src[i], a_df[i], a_ss[i], a_ms[i], a_F[i], a_p[i])
    cat(do.call(sprintf, c(a_fmt, row)))
  }
  cat("\n")

  # Coefficients
  cat("Coefficients Table\n")
  c_hdr <- c("Variables","Coefficients","Standard Error","t-test","p-value",
             paste0(conf_level, "% CI LB"), paste0(conf_level, "% CI UB"))
  if (isTRUE(std_beta)) c_hdr <- c(c_hdr, "Std. Beta")
  c_hdr <- c(c_hdr, "VIF")

  c_vars <- rownames(ctab)
  c_coef <- fmt3(ctab$Coefficients)
  c_se   <- fmt3(ctab$`Standard Error`)
  c_t    <- fmt3(ctab$`t-test`)
  c_p    <- fmt_p(ctab$`p-value`)
  c_lb   <- fmt3(ctab[[lb_name]])
  c_ub   <- fmt3(ctab[[ub_name]])
  c_beta <- if (isTRUE(std_beta)) fmt3(ctab$`Std. Beta`) else NULL
  c_vif  <- ifelse(is.na(ctab$VIF), "", fmt3(ctab$VIF))

  c_cols <- list(c_vars, c_coef, c_se, c_t, c_p, c_lb, c_ub)
  if (isTRUE(std_beta)) c_cols <- c(c_cols, list(c_beta))
  c_cols <- c(c_cols, list(c_vif))

  c_w <- integer(length(c_cols))
  for (i in seq_along(c_cols)) c_w[i] <- max(nchar(c_hdr[i]), max(nchar(c_cols[[i]]), na.rm = TRUE))
  c_fmt <- build_fmt(c_w)
  cat(do.call(sprintf, c(c_fmt, as.list(c_hdr))))
  for (i in seq_len(nrow(ctab))) {
    row <- list(c_vars[i], c_coef[i], c_se[i], c_t[i], c_p[i], c_lb[i], c_ub[i])
    if (isTRUE(std_beta)) row <- c(row, c_beta[i])
    row <- c(row, c_vif[i])
    cat(do.call(sprintf, c(c_fmt, row)))
  }

  invisible(list(
    coefficients = ctab,
    anova = data.frame(
      Source = c("Regression","Residual","Total"),
      df = c(df_reg, df_res, df_tot),
      SS = c(ssr, sse, sst),
      MS = c(ms_reg, ms_res, NA_real_),
      `F-Statistic` = c(fstat, NA_real_, NA_real_),
      `p-value` = c(p_f, NA_real_, NA_real_),
      check.names = FALSE
    ),
    summary = list(
      Multiple_R = multR, R_Square = r2, Adj_R_Square = adjr,
      Standard_Error = s_err, N_Read = n_read, N_Missing = n_miss, N_Used = n_used
    ),
    robust = robust,
    conf_level = conf_level,
    digits = digits,
    std_beta = std_beta
  ))
}
