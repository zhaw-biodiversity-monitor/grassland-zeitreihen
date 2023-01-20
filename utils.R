read_all_layers <- function(file, exception = NA) {
  layer_names <- st_layers(file)$name
  layer_names <- layer_names[!(layer_names %in% exception)]
  sapply(layer_names, \(x)st_read(file, x), simplify = FALSE)
}

clean_names <- function(str) {
  str |>
    str_replace("_", " ") |>
    str_to_title()
}

# takes two vectors (e.g. Artenreichtum and n), groups each into discrete groups
# by quantile (probs), and returns a group index which can be used to colorize
# using a bivarate scale
get_bivariate_group <-
  function(vec1,
           vec2,
           prob1 = seq(0, 1, 0.25),
           prob2 = prob1,
           break1 = NA,
           break2 = NA) {
    stopifnot(length(vec1) == length(vec2))
    vecs <- list(vec1, vec2)       #
    probs <- list(prob1, prob2)     # -> create lists to use map2
    breaks <- list(break1, break2) #
    
    mybreaks <-
      map2(vecs, probs, \(x, y) quantile(x, y, na.rm = TRUE))
    mybreaks[!is.na(breaks)] <- breaks[!is.na(breaks)]
    
    cuts <-
      map2(vecs,
           mybreaks,
           \(x, y)cut(
             x,
             y,
             include.lowest = TRUE,
             dig.lab = 10,
             labels = FALSE
           ))
    
    # Get all *possible* factor levels (even those not in the data)
    
    fac_levels <- map(lengths(mybreaks) - 1, seq_len) |>
      expand.grid() |>
      apply(1, paste, collapse = "-") |>
      sort()
    do.call(cbind, cuts) |>
      apply(1, \(x) {
        paste(x, collapse = "-")
      }) |>
      factor(levels =  fac_levels)
  }

# from a list of datasets, select a perticular dataset based on the aggregation level and the "topic"
select_dataset <-
  function(list_of_datasets,
           selected_aggregation,
           selected_dataset,
           sep = "_") {
    layer_name <-
      paste(selected_aggregation, selected_dataset, sep = sep)
    na.omit(list_of_datasets[[layer_name]])
  }

# from here:
# https://github.com/rstudio/gt/blob/ff878e10d21a3ba897c5f99801b796da8fb637fa/R/helpers.R#L2496-L2536
adjust_luminance <- function(colors, steps) {
  stopifnot(steps < 2, steps > -2)
  rgb_matrix <- t(grDevices::col2rgb(colors, alpha = TRUE)) / 255
  alpha <- rgb_matrix[, "alpha"]
  luv_matrix <-
    grDevices::convertColor(rgb_matrix[, 1:3], "sRGB", "Luv")
  h <- atan2(luv_matrix[, "v"], luv_matrix[, "u"]) * 180 / pi
  c <- sqrt(luv_matrix[, "u"] ^ 2 + luv_matrix[, "v"] ^ 2)
  l <- luv_matrix[, "L"]
  y <- l / 100.
  x <- log(-(y / (y - 1)))
  y_2 <- 1 / (1 + exp(-(x + steps)))
  l <- y_2 * 100.
  grDevices::hcl(h, c, l, alpha = alpha)
}

# create a matrix with color palette
bivariate_matrix_luminocity <-
  function(mypal,
           n = length(mypal),
           combine_with = "cbind") {
    accumulate(seq_len(n - 1), \(x, y) adjust_luminance(x, 1), .init = mypal) |>
      rev() |>
      (\(x) do.call(combine_with, x))()
  }

bivariate_matrix_alpha <-
  function(mypal,
           n = length(mypal),
           alpha_range = c(0, 1)) {
    rgb_mat <- col2rgb(mypal) / 255
    a_from <- alpha_range[1]
    a_to <- alpha_range[2]
    alpha_seq <- seq(a_from, a_to, (a_to - a_from) / (n - 1))
    
    sapply(alpha_seq, function(alpha) {
      apply(rgb_mat, 2, \(x) rgb(x[1], x[2], x[3], alpha))
    })
  }