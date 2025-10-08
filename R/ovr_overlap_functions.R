##' functions ovr_get_overlap_pairs, ovr_add_group and ovr_dyad_to_network are taken from
##' https://github.com/MatthieuStigler/Misc/tree/master/spatial/check_overlap

###==========================================
##' ovr_get_overlap_pairs
###==========================================

##' Function ovr_get_overlap_pairs compute the dyadic (1-1) overlap of polygons among a given
##' collection with. It returns a data frame where each row is an overlapping dyad A-B, with
##' respective areas and percentage of overlap with respect to A and B.
##' @param sf the sf object
##' @param id_var optional variable containing the row identifier
##' @param unit the unit in which to report the area
##' @param pre_filter whether to run first st_intersects to filter? Recommended as seems
##' @param inter_make_valid whether to repair potentially invalid values
##' @param sf2 NULL. to compare two groups of polygons
##' @returns a tibble where each row represents an overlapping A-B dyad, and columns
##' indicate the id of the dyad, the area of each polygon, their intersection and percentage of overlap
##' @import sf
##' @import dplyr
##' @import tidyr
##' @import purrr
##' @import units
##' @noRd

ovr_get_overlap_pairs <- function(sf, sf2=NULL, id_var = NULL, unit = "m2", pre_filter = TRUE,
                                  inter_make_valid = FALSE) {

  ## add id var if not there
  # id_var <- rlang::quo(ids)
  has_sf2 <- !is.null(sf2)

  ## pre filter
  if(pre_filter){
    ## one single data
    if(!has_sf2) {
      row_intersect_df <- sf::st_intersects(sf) %>%
        as.data.frame() %>%
        filter(.data$row.id!=.data$col.id)
      row_intersect <- sort(unique(unlist(row_intersect_df)))
      sf <- sf[row_intersect,]
      ## two datasets
    } else {
      row_intersect_df <- st_intersects(sf, sf2) %>%
        as.data.frame()
      sf <- sf[unique(row_intersect_df$row.id),]
      sf2 <- sf2[unique(row_intersect_df$col.id),]
    }
  }

  ## add id var if not specified
  if(rlang::quo_is_null(rlang::enquo(id_var))){
    sf <- sf %>%
      mutate(id_var = row_number())
    id_var <- rlang::quo(id_var)
  } else {
    if(!rlang::as_name(rlang::enquo(id_var)) %in% colnames(sf)) stop(paste0("Column `", rlang::as_name(rlang::enquo(id_var)), "` not in data?"))
    if(anyDuplicated(pull(sf, {{id_var}}))) stop("Duplicated ids found, function will not work correctly")
  }

  ## get pairwise intersection
  if(!has_sf2) sf2 <- sf
  inter <- st_intersection(select(sf,  row_a = {{id_var}}) %>% st_set_agr("constant"),
                           select(sf2, row_b = {{id_var}}) %>% st_set_agr("constant"))

  ##  eventually repair
  if(inter_make_valid) {
    inter <- inter %>%
      st_make_valid()
  }


  ## Compute area of intersect
  inter_df_raw <- inter %>%
    filter(.data$row_b!=.data$row_a) %>%
    mutate(area_inter = sf::st_area(sf::st_geometry(.))  %>% units::set_units(unit, mode = "standard")) %>%
    sf::st_set_geometry(NULL) %>%
    as_tibble()%>%
    mutate("dyad" = purrr::map2_chr(.data$row_a, .data$row_b, ~paste(sort(c(.x, .y)), collapse = " "))) %>%
    relocate("dyad")

  ## If only 1sf: dyads are duplicated, take second
  if(!has_sf2) {
    inter_df_raw <- inter_df_raw |>
      group_by(.data$dyad) %>%
      slice(2) %>%
      ungroup()
  }

  ## Compute polygon area for each intersecting ones
  ids_intersecting <- unique(c(inter_df_raw$row_a, inter_df_raw$row_b))
  if(has_sf2) sf <- rbind(sf, sf2)
  areas_indiv <- sf %>%
    select(id_var_new={{id_var}}) %>%
    filter(.data$id_var_new %in% ids_intersecting) %>%
    mutate(area = sf::st_area(sf::st_geometry(.)) %>% units::set_units(unit, mode = "standard"))%>%
    sf::st_set_geometry(NULL) %>%
    as_tibble()

  ## add indiv area to intersect area, compute overlap
  inter_df_raw%>%
    left_join(areas_indiv %>%
                rename(row_a = "id_var_new", area_a = "area"), by = "row_a") %>%
    left_join(areas_indiv %>%
                rename(row_b = "id_var_new", area_b = "area"), by = "row_b") %>%
    mutate(across(c("area_a", "area_b"), list(overlapped=~100*units::drop_units(area_inter/.))))

}


###==========================================
##' ovr_add_group
###==========================================

#' Add the group in which each dyad is,
#'
#' This adds all polygons that (indirectly) overlap
#'
#' @param df_inter the output from `ovr_get_overlap_pairs`
#' @param simplify_group_key Should the group key be simplifed ?
#' @noRd
ovr_add_group <- function(df_inter, simplify_group_key=FALSE){


  ## convert to igraph
  g <- df_inter %>%
    relocate("row_a", "row_b") %>%
    igraph::graph_from_data_frame(directed=FALSE)
  ## Extract info
  compo <- igraph::components(g)
  tab_group_letters <- tibble::enframe(purrr::map(igraph::groups(compo), ~paste(., collapse = " ")),
                                       name = "group_num", value = "group") %>%
    tidyr::unnest("group") %>%
    mutate(group_num=as.numeric(.data$group_num),
           group_N = compo$csize)

  tab_groups <- tibble(id=igraph::V(g)$name, group_num = compo$membership) %>%
    left_join(tab_group_letters, by = "group_num")

  ## Make sure id is as in original df
  if(!is.character(df_inter$row_a)){
    tab_groups <- tab_groups %>%
      mutate(id = methods::as(id, class(df_inter$row_a)))
  }

  ## add back to data
  res <- df_inter %>%
    left_join(tab_groups %>% select(id, "group", "group_N"), by = c("row_a"="id")) %>%
    relocate("group", "group_N", .after = "dyad")

  ## eventually substitue key
  if(simplify_group_key){
    new_kwys <- res %>%
      distinct(.data$group) %>%
      mutate(group_id= .data$intrnl_sub_key(n()))

    res <- res %>%
      left_join(new_kwys, by = "group") %>%
      select(-"group") %>%
      relocate(group=.data$group_id, .after = "dyad")

  }
  res
}


###==========================================
##' ovr_dyad_to_network
###==========================================

#' Function ovr_dyad_to_network convert dyad data to igraph network
#' @param df_inter the output from `ovr_get_overlap_pairs`
#' @noRd

ovr_dyad_to_network <- function(df_inter){

  ## convert to igraph
  df_inter %>%
    relocate("row_a", "row_b") %>%
    igraph::graph_from_data_frame(directed=FALSE)

}

