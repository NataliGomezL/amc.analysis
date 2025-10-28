#' Make overlap analysis
#'
#' Function overlap_analysis uses the function ovr_get_overlap_pairs to compute the overlap area
#' between territorial units in consecutive periods.
#'
#' @param shps A list of spatial objects (`sf`) representing the territorial units for different periods.
#' Each element of the list corresponds to one period and share the same projected coordinate reference system (CRS).
#' @param id_unit_var The identifier variable present in each spatial object that uniquely defines the territorial unit
#' in a given period.
#' @param period_var A numerical variable present in each spatial object that defines the period in which the units are
#' being represented.
#' @param area_threshold A numeric value used to remove very small overlaps. Default is 1.
#' @param output_file Optional file path to save the results in `.fst` format. If `NULL`, the results
#' are only returned in memory without saving.
#' @export
amc_overlap_analysis <- function(shps,
                                 id_unit_var,
                                 period_var,
                                 area_threshold = 1,
                                 output_file = NULL) {

  ## check column names
  check_has_period <- sapply(shps, \(x) period_var %in% colnames(x))
  if(!all(check_has_period==TRUE)) {
    missing_year <- names(shps)[!check_has_period]
    stop(paste0("The following shapefiles do not contain the column '",
                period_var,
                "': ",
                paste(missing_year, collapse = ", "),
                ". Use the 'period_var' argument to specify the correct name."))
  }

  ## Check same period
  period <- lapply(shps, \(x) unique(x[[period_var]]))
  check_period <- purrr::map_lgl(period, ~ length(.x) == 1)

  if (!all(check_period)) {
    stop(paste0("Period must be the same in each shp"))
  }

  ## do same for id_unit_var
  check_has_id <- sapply(shps, \(x) id_unit_var %in% colnames(x))
  if(!all(check_has_id==TRUE)) {
    missing_id <- names(shps)[!check_has_id]
    stop(paste0("The following shapefiles do not contain the column '",
                id_unit_var,
                "': ",
                paste(missing_id, collapse = ", "),
                ". Use the 'id_unit_var' argument to specify the correct name."))
  }

  ## check duplicates ID
  check_duplicates <- sapply(shps, \(x) any(duplicated(x[[id_unit_var]])))

  if (any(check_duplicates)) {
    dup_idx <- which(check_duplicates)
    stop(paste0("Repeated IDs were found in the following shapefiles:",
                paste(dup_idx, collapse = ", "),
                ". Check out the column '", id_unit_var, "'."))
  }

  ## check same projections!?
  crs <- lapply(shps, \(x) sf::st_crs(x))
  check_crs <- sapply(crs[-1], \(x) x==crs[[1]])

  if (!all(check_crs)) {
    stop(paste0("Not all shapefiles share the same CRS. Consider transforming them with st_transform()."))
  }

  ## Check: ensure the shared CRS is projected (not geographic lon/lat)
  # (1) no missing CRS
  if (any(vapply(crs, is.na, logical(1)))) {
    idx <- which(vapply(crs, is.na, logical(1)))
    stop(paste0("Some shapefiles have no CRS: ",
                paste(names(shps)[idx], collapse = ", "),
                ". Set a valid projected CRS (e.g., EPSG:3035, 2056, etc.)."))
  }
  # (2) not longitude/latitude
  is_geographic <- sf::st_is_longlat(crs[[1]])
  if (is_geographic) {
    stop(paste0("The CRS is geographic (longitude/latitude). ",
                "Area calculations require a projected CRS (e.g., EPSG:3035, 3857, 2056). ",
                "Transform your data with st_transform()."))
  }

  ## now add here the id_unit_year = paste(id_unit_var, period_var)
  shps <- lapply(shps, \(x) mutate(x, period_id = paste0(.data[[period_var]], "_", .data[[id_unit_var]])) %>%
                   select(period_id, all_of(c(period_var, id_unit_var))))

  # Create an empty list
  out_list <- list()

  # Loop: compare consecutive shapefiles
  for (i in 1:(length(shps) - 1)) {
    period_i <- unique(pull(shps[[i]], !!sym(period_var)))
    period_i2 <- unique(pull(shps[[i+1]], !!sym(period_var)))
    out_list[[i]] <- ovr_get_overlap_pairs(sf  = shps[[i]],
                                           sf2 = shps[[i+1]],
                                           id_var = "period_id")  %>%
      ## add year info
      mutate(year_A = period_i,
             year_B = period_i2)

    message("Done for ", period_i, " - ", period_i2)

  }

  ## bind results
  out_all <- bind_rows(out_list) |>
    ## drop super small areas
    filter(units::drop_units(area_inter) > area_threshold)

  # Save if user requests it
  if (!is.null(output_file)) {
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
    fst::write_fst(out_all, output_file)
    message("Overlap analysis saved in: ", output_file)
  }

  return(out_all)
}

#' Get clusters
#'
#' @inheritParams amc_compare_threshold
#' @export
amc_get_clusters <- function(df_overlap, threshold=NULL){
  if(!is.numeric(threshold)) stop("Argument 'threshold' should be numeric")
  if(is.null(threshold)) stop("Please indicate a threshold value, which you can fine tune with ")
  df_overlap %>%
    filter(area_a_overlapped > threshold | area_b_overlapped > threshold) |>
    ovr_add_group()
}


#' compare_threshold
#'
#' @param df_overlap Output of function amc_overlap_analysis()
#' @export
amc_compare_threshold <- function(df_overlap,
                                  threshold=seq(0, 20, by=1),
                                  plot=TRUE){

  ## count units in groups
  group_count_unit <- function(group_string){
    map_int(stringr::str_split(group_string, " "), ~stringr::str_remove(., "^[0-9]+_") %>% unique() %>% n_distinct)
  }

  ## define function
  get_th_stats <- function(thresh){

    ovr_by_th <- amc_get_clusters(df_overlap, threshold = thresh)

    ## compute cluster stats
    n_clusters <- n_distinct(ovr_by_th$group)
    n_units_by_cluster <- map_int(unique(ovr_by_th$group), group_count_unit)

    ## assemble res
    tibble(n_clusters=n_clusters,
           max_n_units_by_cluster = max(n_units_by_cluster))
  }

  ## compute get_th_stats() on all thesholds
  res <- tibble(thresh = threshold) %>%
    mutate(data=map(.data$thresh, get_th_stats)) %>%
    unnest("data")

  # prepare for plot
  out_res_l <- res %>%
    # select(-tot_units_by_cluster) %>%
    pivot_longer(cols = -"thresh",
                 names_to = "variable",
                 values_to = "value")

  ## plot
  if(plot){
    plot <- out_res_l %>%
      mutate(variable_clean = case_match(.data$variable,
                                         "max_n_units_by_cluster"~ "Maximum number of units by cluster",
                                         "n_clusters"~ "Number of clusters",)) %>%
      # filter(thresh!=0) %>%
      ggplot(aes(x=.data$thresh, y=.data$value))+
      geom_line()+
      facet_wrap(~variable_clean, ncol=1, scales="free")+
      ggtitle("Effect of threshold on number and size of clusters") +
      scale_x_continuous(breaks = threshold) +
      scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
      xlab("Threshold") +
      ylab("Value")


    print(plot)
  }

  ## return result
  return(res)
}

#' Create AMC
#'
#' @inheritParams amc_compare_threshold
#' @export
amc_create_amc <- function(df_overlap,
                           shps,
                           threshold,
                           id_unit_var,
                           period_var,
                           ref_period){

  all_shps <- do.call(rbind, shps) %>% st_as_sf() %>%
    mutate(period_id = paste0(.data[[period_var]], "_", .data[[id_unit_var]]))

  df_overlap_add_group <- amc_get_clusters(df_overlap, threshold) %>%
    select(row_a, row_b, group, group_N)  %>%
    arrange(group)

  ## df overlap to long
  df_ids_l <- df_overlap_add_group %>%
    gather(row, period_id, row_a, row_b) %>%
    mutate(id = stringr::str_remove(period_id, "^[0-9]+_")) %>%
    select(-row) %>%
    distinct() %>%
    mutate(n_distinct_id=n_distinct(id),
           .by=group)

  ## now select the reference year
  ref_shp <- all_shps %>%
    right_join(df_ids_l, by ="period_id") %>%
    ## select the relevant ref period
    filter(.data[[period_var]] == ref_period)


  ## now create AMC
  AMC <- ref_shp %>%
    group_by(group) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>%
    mutate(amc_id = paste0("AMC", (stringr::str_pad(row_number(), width = 4, pad = "0")))) %>%
    select(!group)



  return(AMC)
}


#' Make crosswalk
#' @export
amc_create_crosswalk <- function(shps,
                                 amc_shp,
                                 id_unit_var,
                                 period_var,
                                 threshold,
                                 output_file = NULL) {

  ## check column names
  check_has_period <- sapply(shps, \(x) period_var %in% colnames(x))
  if(!all(check_has_period==TRUE)) {
    missing_year <- names(shps)[!check_has_period]
    stop(paste0("The following shapefiles do not contain the column '",
                period_var,
                "': ",
                paste(missing_year, collapse = ", "),
                ". Use the 'period_var' argument to specify the correct name."))
  }

  ## check that period_var is numeric
  check_period_numeric <- sapply(shps, \(x) is.numeric(x[[period_var]]))
  if (!all(check_period_numeric)) {
    non_numeric <- names(shps)[!check_period_numeric]
    stop(paste0("The column '", period_var, "' must be numeric in all shapefiles. ",
                "These shapefiles are not numeric: ",
                paste(non_numeric, collapse = ", "), "."))
  }

  ## Check same period
  period <- lapply(shps, \(x) unique(x[[period_var]]))
  check_period <- purrr::map_lgl(period, ~ length(.x) == 1)

  if (!all(check_period)) {
    stop(paste0("Period must be the same in each shp"))
  }

  ## do same for id_unit_var
  check_has_id <- sapply(shps, \(x) id_unit_var %in% colnames(x))
  if(!all(check_has_id==TRUE)) {
    missing_id <- names(shps)[!check_has_id]
    stop(paste0("The following shapefiles do not contain the column '",
                id_unit_var,
                "': ",
                paste(missing_id, collapse = ", "),
                ". Use the 'id_unit_var' argument to specify the correct name."))
  }

  ## check duplicates ID
  check_duplicates <- sapply(shps, \(x) any(duplicated(x[[id_unit_var]])))

  if (any(check_duplicates)) {
    dup_idx <- which(check_duplicates)
    stop(paste0("Repeated IDs were found in the following shapefiles:",
                paste(dup_idx, collapse = ", "),
                ". Check out the column '", id_unit_var, "'."))
  }

  ## check same projections!?
  crs <- lapply(shps, \(x) sf::st_crs(x))
  check_crs <- sapply(crs[-1], \(x) x==crs[[1]])

  if (!all(check_crs)) {
    stop(paste0("Not all shapefiles share the same CRS. Consider transforming them with st_transform()."))
  }

  ## now add here the id_unit_year = paste(id_unit_var, period_var)
  all_shps <- do.call(rbind, shps) %>%
    st_as_sf() %>%
    mutate(amc_id = paste0(.data[[period_var]], "_", .data[[id_unit_var]])) %>%
    select("amc_id")

  # compute overlap
  amc_overlap <- ovr_get_overlap_pairs(sf = amc_shp,
                                       sf2 = all_shps,
                                       id_var = amc_id)

  amc_overlap_add_group <- amc_get_clusters(amc_overlap, threshold) %>%
    arrange(group)


  amc_crosswalk <- amc_overlap_add_group %>%
    select(row_a, row_b, area_a_overlapped, area_b_overlapped) %>%
    rename(amc_id=row_a, period_id=row_b) %>%
    ## group and select the maximum value
    group_by(period_id) %>%
    slice_max(area_b_overlapped) %>%
    ungroup() %>%
    ## extract period
    separate(period_id, into = c("period", "id"), sep = "_", extra = "merge", fill = "right", remove = FALSE) %>%
    ## compute again weights
    group_by(amc_id, period) %>%
    mutate(weight =100*area_a_overlapped/sum(area_a_overlapped),
           n_units = n()) %>%
    ungroup() %>%
    select(-starts_with("area_")) %>%
    arrange(amc_id, period) %>%
    relocate(id, period, .after=amc_id)

  amc_crosswalk


  return(amc_crosswalk)
}
