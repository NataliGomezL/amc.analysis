##' Check contiguous borders
#'
#' @export
qck_check_contiguous <- function(shp){
  check_one <- function(x, label = NULL){
    over <- sf::st_overlaps(x)
    problematic <- which(lengths(over) > 0)
    if (length(problematic) == 0) {
      message(if (is.null(label)) "All territorial borders are contiguous"
              else sprintf("[%s] All territorial borders are contiguous", label))
      return(TRUE)
    } else {
      prefix <- if (is.null(label)) "" else sprintf("[%s] ", label)
      for (i in problematic) {
        message(sprintf("%sPolygon %d overlaps with polygons: %s",
                        prefix, i, paste(over[[i]], collapse = ", ")))
      }
      return(FALSE)
    }
  }

  if (inherits(shp, "sf")) {
    return(check_one(shp))
  }

  if (is.list(shp) && all(vapply(shp, inherits, logical(1L), "sf"))) {
    nms <- names(shp)
    res <- logical(length(shp))
    for (k in seq_along(shp)) {
      lab <- if (!is.null(nms) && nzchar(nms[k])) nms[k] else paste0("item_", k)
      res[k] <- check_one(shp[[k]], label = lab)
    }
    if (all(res)) {
      message("All territorial borders are contiguous across all objects")
      return(TRUE)
    } else {
      message("Some territorial borders are not contiguous in at least one object")
      return(FALSE)
    }
  }

  stop("Input must be an sf object or a list of sf objects.")
}

###==========================================
# correct no contiguous borders
###==========================================

#' Correct no contiguous borders
#'
#' @export
qck_correct_contiguous <- function(shp) {

  # Packages
  library(rmapshaper)

  if (inherits(shp, "sf")) {
    # case: one sf
    return(ms_simplify(shp, keep = 1))
  }
  if (is.list(shp) && all(vapply(shp, inherits, logical(1L), "sf"))) {
    # case: list de sf
    return(lapply(shp, \(x) ms_simplify(x, keep = 1)))
  }
  stop("The argument 'shp' must be an sf object or a list of sf objects")
}

###==========================================
##' Plot overlap histogram
###==========================================

#' Plot overlap histogram
#'

qck_overlap_histog <- function(df_overlap, bins = 30){

  overlap_long <- df_overlap %>%
    mutate(years_A_B = paste0(year_A, "-", year_B)) %>%
    select(area_a_overlapped, area_b_overlapped , years_A_B) %>%
    pivot_longer(cols = c(area_a_overlapped, area_b_overlapped),
                 names_to = "type",
                 values_to = "overlap_area")

  p <- ggplot(overlap_long, aes(overlap_area)) +
    geom_histogram(bins = bins) +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    facet_wrap(~ years_A_B, nrow = 3) +
    labs(title = "Histogram of the dyad's area overlaps",
         x = "Area overlapped",
         y = "Count")

  return(p)
}

