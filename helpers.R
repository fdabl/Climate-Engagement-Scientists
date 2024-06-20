#' Estimates the model using brms, takes the formula, a filename, and data
#' if use_model != null, then it updates the model provided
run_model <- function(form, filename, dat, force = FALSE, use_model = NULL, ...) {
  if (!file.exists(filename) || force) {
    # Weakly informative default prior (Gelman et al., 2008; except for the scaling)
    
    if (!is.null(use_model)) {
      fit <- update(use_model, newdata = dat, formula = form, recompile = FALSE, ...)
    } else {
      fit <- brm(form, data = dat, prior = prior(student_t(3, 0, 2.5), class = b), ...) 
    }
    saveRDS(fit, filename)
  } else {
    fit <- readRDS(filename)
  }
  
  fit
}


# Run individual regression model
run_individual_model <- function(form, pred, dat, type = 'advocacy_wnw', ml = FALSE, moderation = FALSE, ...) {
  filepath <- paste0('../models/marginal_', type, '_', tolower(pred), '.RDS')
  
  fit <- run_model(
    make_individual_form(form, pred, ml = ml, moderation = moderation), filepath,
    dat, family = bernoulli, ...
  )
  
  fit
}


# We run individual marginal models like above
run_individual_model_binom <- function(pred, dat, type = 'lifestyle', ml = FALSE, moderation = FALSE, ...) {
  filepath <- paste0('../models/marginal_binom_final_', type, '_', tolower(pred), '.RDS')
  
  fit <- run_model(
    make_individual_form_binom(pred, type = type, ml = ml, moderation = moderation), filepath,
    dat, family = binomial, ...
  )
  
  fit
}


# Add categories (background, intellectual, practical)
add_categories <- function(coefs, categories) {
  n <- nrow(coefs)
  coefs$category <- NULL
  
  for (i in seq(n)) {
    varname <- as.character(coefs$rowname[i])
    
    if (varname %in% categories$Background) {
      category <- 'Background'
    } else if (varname %in% categories$Intellectual) {
      category <- 'Intellectual'
    } else if (varname %in% categories$Practical) {
      category <- 'Practical'
    }
    
    coefs$category[i] <- category
  }
  
  coefs %>% 
    mutate(category = factor(category, levels = c('Intellectual', 'Practical', 'Background')))
}


# Get predictors from formula object
get_preds <- function(form) {
  predictor_vars <- strsplit(as.character(form), '~')[[3]]
  preds <- trimws(strsplit(predictor_vars, '\\+')[[1]])
  preds
}


# Create form for individual logistic regressions
make_individual_form <- function(form, pred, ml = FALSE, moderation = FALSE) {
  outcome <- strsplit(as.character(form), '~')[[2]]
  
  # If we want moderation, but the variable is not Research_std, we add the moderation!
  if (!(pred == 'Research_std' & moderation) & moderation) {
    pred <- ifelse(moderation, paste0(pred, ' * Research_std'), pred)
  }
  
  as.formula(
    paste0(outcome, ' ~ ', ifelse(ml, paste0(pred, ' + (', pred, ' | country)'), pred))
  )
}


# Create form for binomial regressions
make_individual_form_binom <- function(pred, type = 'lifestyle', ml = FALSE, moderation = FALSE) {
  
  if (type == 'lifestyle') {
    outcome <- 'nr_lifestyle_actions | trials(6)'
  } else {
    outcome <- 'nr_advocacy_actions | trials(7)'
  }
  
  pred <- ifelse(moderation, paste0(pred, ' * Research_std'), pred)
  
  as.formula(
    paste0(outcome, ' ~ ', ifelse(ml, paste0(pred, ' + (', pred, ' | country)'), pred))
  )
}


# Get individual regression models
get_mm <- function(form, preds, dat, type, fit_initial, ml = TRUE, moderation = FALSE) {
  
  models <- lapply(preds, function(pred) {
    run_individual_model(
      form, pred, dat, type = type, ml = ml,
      moderation = moderation, use_model = fit_initial, cores = 4
    )
  })
  
  models
}


# Compute main effects
get_main_effects <- function(
    model, predictor_name, type, outcome, metric = 'absolute',
    re_formula = NULL, newdata = NULL, comparison = 'difference', mult = 100
) {
  
  df <- data.frame(
    avg_comparisons(
      model, variables = predictor_name,
      re_formula = re_formula, newdata = newdata, comparison = comparison
    )
  )
  
  df <- df %>% 
    select(term, estimate, conf.low, conf.high) %>% 
    mutate(type = type, metric = metric, outcome = outcome)
  
  df <- df %>% 
    rename(
      rowname = term,
      Estimate = estimate,
      Q2.5 = conf.low,
      Q97.5 = conf.high
    ) %>% 
    mutate(
      Effect = 'Main',
      Estimate = Estimate * mult,
      Q2.5 = Q2.5 * mult,
      Q97.5 = Q97.5 * mult,
      newdata = ifelse(is.null(newdata), 'null', 'mean')
    )
  
  df
}


# Compute interaction effects
get_int_effects <- function(
    model, predictor_name, type, outcome, metric = 'absolute',
    re_formula = NULL, newdata = NULL, comparison = 'difference', mult = 100
) {
  
  df <- data.frame(
    avg_comparisons(
      model, variables = predictor_name, by = 'Research_std',
      re_formula = re_formula, newdata = newdata, comparison = comparison
    )
  )
  
  df <- df %>% 
    select(term, estimate, Research_std, conf.low, conf.high) %>% 
    mutate(type = type, metric = metric, outcome = outcome) %>% 
    arrange(Research_std) %>% 
    # first effect of Research_std at somewhat related to climate (~ mean),
    # than low and high Research_std
    slice(c(3, 1, 5))
  
  df <- df %>% 
    rename(
      rowname = term,
      Estimate = estimate,
      Q2.5 = conf.low,
      Q97.5 = conf.high
    ) %>% 
    mutate(
      Estimate = Estimate * mult,
      Q2.5 = Q2.5 * mult,
      Q97.5 = Q97.5 * mult,
      newdata = ifelse(is.null(newdata), 'null', 'mean')
    )
  
  df$Effect <- c('Main', 'Interaction_lo', 'Interaction_hi')
  df
}


# Get marginal effects at the mean, used for multiple regressions
get_effects_multiple <- function(model, predictor_names, type, outcome, re_formula = NULL, newdata = 'mean') {
  df <- avg_comparisons(model, newdata = newdata, re_formula = re_formula) %>% 
    data.frame() %>% 
    mutate(
      rowname = predictor_names[term],
      Estimate = estimate * 100,
      Q2.5 = conf.low * 100,
      Q97.5 = conf.high * 100,
      type = type,
      outcome = outcome,
      newdata = ifelse(is.null(newdata), 'null', 'mean')
    ) %>% 
    arrange(estimate) %>% 
    select(rowname, Estimate, Q2.5, Q97.5, type, outcome)
  
  df
}


# Visualizes coefficients
plot_coefs_both <- function(
    df_coefs, breaks = seq(-25, 25, 5), confint = TRUE, ylabel = 'Percentage change',
    filepath = NULL, order_type = 'Protest', yintercept = 0, xlimits = c(min(breaks), max(breaks)),
    title = '', title_hjust = 0.50, ...) {
  
  # Order:
  # Already do vs not willing to,
  # Already do vs willing to
  # Willing to vs not willing to
  cols <- c('#7570B3', '#1B9E77', '#D95F02')
  
  if (is.null(df_coefs$Effect)) {
    df_coefs$Effect <- 'Main'
  }
  
  if (is.null(df_coefs$metric)) {
    df_coefs$metric <- 'absolute'
  }
  
  # Order according to protest: Willing to vs not willing to
  order_both <- df_coefs %>%
    filter(type == !!order_type, Effect == 'Main', outcome == 'Willing to vs not willing to') %>%
    # filter(type == !!order_type, outcome == 'Already do vs willing to') %>%
    # filter(type == !!order_type, outcome == 'Already do vs not willing to') %>%
    arrange(category, desc((Estimate)))
  
  df_coefs$rowname <- factor(
    df_coefs$rowname, levels = rev(as.character(order_both$rowname))
  )
  
  if (length(unique(df_coefs$outcome)) == 2) {
    cols <- cols[-1]
  }
  
  if (all(df_coefs$Effect == 'Main')) {
    p <- ggplot(df_coefs, aes(x = rowname, y = Estimate, color = factor(outcome))) +
      geom_hline(aes(yintercept = yintercept), linewidth = 1, linetype = 'dotted') +
      geom_point(size = 2.2, position = position_dodge(width = 0.80)) +
      xlab('') +
      coord_flip() +
      facet_wrap(~ type) +
      scale_color_manual(name = '', values = cols)
      
  # Otherwise also visualize interactions
  } else {
    labels <- c('Not at all', 'A moderate amount', 'A great deal')
    p <- ggplot(df_coefs, aes(x = rowname, y = Estimate, color = factor(outcome), alpha = factor(Effect), shape = factor(Effect))) +
      geom_hline(aes(yintercept = yintercept), linewidth = 1, linetype = 'dotted') +
      geom_point(size = 2.2, position = position_dodge(width = 0.80)) +
      xlab('') +
      coord_flip() +
      facet_wrap(~ type) +
      scale_color_manual(name = '', values = cols) +
      scale_alpha_manual(name = '', values = c(0.50, 1, 0.25), labels = labels) +
      scale_shape_manual(name = '', values = c(15, 16, 17), labels = labels) +
      guides(color = guide_legend(order = 1), shape = guide_legend(order = 2), alpha = guide_legend(order = 2))
  }
  
  p <- p +
    scale_y_continuous(breaks = breaks, limits = xlimits) +
    theme_minimal() +
    theme(
      legend.position = 'top',
      legend.box = "vertical",  # Align legends vertically
      legend.margin = margin(6, 6, 6, 6),  # Adjust overall legend margin
      legend.box.margin = margin(0, 0, 0, 0),  # Adjust spacing around the entire set of legends
      legend.spacing.y = unit(0, "cm"),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 16),
      strip.text = element_text(size = 14),
      axis.text.x = element_text(size = 10),
      axis.title.x = element_text(size = 14),
      plot.title = element_text(hjust = title_hjust, size = 16)
    ) +
    ylab(ylabel) +
    ggtitle(title)
  
  if (confint) {
    p <- p + 
      geom_errorbar(
        aes(ymin = Q2.5, ymax = Q97.5), width = 0,
        linewidth = 1.1, position = position_dodge(width = 0.80)
      )
  }
  
  if (unique(df_coefs$metric) == 'relative') {
    symlog_trans <- function(base = exp(1)) {
      trans_new(
        name = "symlog",
        transform = function(x) sign(x) * log1p(abs(x) / base),
        inverse = function(x) sign(x) * (exp(abs(x) * base) - base),
        domain = c(-Inf, Inf)
     )
    }
    labels <- c(-100, -50, -25, -10, -3, 0, 3, 10, 25, 50, 100, 300)
    p <- p + scale_y_continuous(trans = symlog_trans(), breaks = labels, labels = labels)
  }
  
  if (!is.null(filepath)) {
    ggsave(filepath, p, ...)
  }
  
  p
}


# Wrapper for `get_country_effects_variation`
get_country_variation <- function(models, predictor_name, type, outcome) {
  m <- models
  varname <- rownames(fixef(m))[2]
  
  # Sanity check
  stopifnot(predictor_name == varname)
  
  get_country_effects_variation(m, type, varname = varname) %>% 
    mutate(outcome = outcome)
}


# Get the country variation around the fixed effects
get_country_effects_variation <- function(fit, varname, type) {
  b_var <- fixef(fit)[2, 1]
  
  random_effects <- ranef(fit)
  b_var_country <- random_effects$country[, , 2][, 1]
  
  b_country <- b_var + b_var_country
  
  ci_lo <- quantile(b_country, 0.025)
  ci_hi <- quantile(b_country, 0.975)
  
  df <- data.frame(
    'Effect' = c('Main'),
    'rowname' = varname,
    'Estimate' = b_var,
    'Q2.5' = ci_lo,
    'Q97.5' = ci_hi,
    'type' = type,
    'metric' = 'log odds'
  )
  
  rownames(df) <- NULL
  df
}


# Used to create the barrier data frame for visualisation
make_df <- function(dat_text, barriers, response_name, varname = 'BehLegal') {
  barriers <- setNames(barriers, paste0(response_name, seq(7)))
  
  dat_barriers <- dat_text %>% 
    select('ResponseId', !!varname, starts_with(response_name)) %>% 
    pivot_longer(cols = paste0(response_name, seq(7))) %>% 
    mutate(name = factor(name, levels = names(barriers), labels = barriers))
  
  dat_barriers
}


#' Adds barriers to the data frame
#' the barriers are recorded in the data frame as 
#' Barrier{Legal, EngPub}_{W, nW, D}_{1, ..., 7} for 
#' protest and advocacy, willing to, not willing to, already do / did
#' we save them using barrier_{1, ..., 7} for protest and barrier_{8, ..., 14} for advocacy
#' (see function call in main_analyses.Rmd)
add_barriers <- function(dat, barrier_type, barriers_numbers = seq(7)) {
  ns <- seq(7)
  
  nW <- paste0(barrier_type, '_nW_', ns)
  W <- paste0(barrier_type, '_W_', ns)
  D <- paste0(barrier_type, '_D_', ns)
  std <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  
  names <- paste0('barriers_', barriers_numbers)
  
  for (i in ns) {
    x <- replace_na(dat[[nW[i]]], 0) + replace_na(dat[[W[i]]], 0) + replace_na(dat[[D[i]]], 0)
    x[x == 0] <- NA
    
    dat[[names[i]]] <- std(x)
  }
  
  dat
}
