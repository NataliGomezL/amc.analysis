###==========================================
## plot territorial units by period
###==========================================

#' Plot territorial units by period
#' @param shps A list of spatial objects (`sf`) representing the territorial units for different periods.
#' Each element of the list corresponds to one period and share the same projected coordinate reference system (CRS).
#' @param id_unit_var The identifier variable present in each spatial object that uniquely defines the territorial unit
#' in a given period.
#' @param period_var A numerical variable present in each spatial object that defines the period in which the units are
#' being represented.
#' #' @param show_legend Logical. Should the legend be displayed? Default = TRUE.
#' @param legend_position Character. Position of the legend if shown. Options: "right", "bottom", "left", "top".
#' Default = "right".
#'
#' @export

plot_territorial_units <- function(shps,
                                   id_unit_var,
                                   period_var,
                                   show_legend = TRUE,
                                   show_label = FALSE,
                                   legend_position = "right"){

  all_shps <- do.call(rbind, shps) %>% st_as_sf()

  p <- ggplot(all_shps) +
    geom_sf(aes(fill = .data[[id_unit_var]])) +
    facet_wrap(vars(!!sym(period_var))) +
    labs(fill = "Territorial units")


  if(show_legend){
    p <- p + theme(legend.position = legend_position)
  } else {
    p <- p + theme(legend.position = "none")
  }

  if(show_label){
    p <- p + geom_sf_text(data = st_centroid(all_shps),
                          aes(label = .data[[id_unit_var]]),
                          size = 3,
                          color = "black")
  }

  p
}

###==========================================
##
###==========================================
#' @export

plot_AMC_byP <- function(shps,
                         id_unit_var,
                         period_var,
                         show_legend = TRUE,
                         legend_position = "right",
                         amc_df) {

  all_shps <- do.call(rbind, shps) %>% st_as_sf()

  p <- ggplot() +
    # polygon layer (territorial units)
    geom_sf(data = all_shps, aes(fill = .data[[id_unit_var]]), color = NA) +
    # black boundary AMCs
    geom_sf(data = amc_df, aes(color = "AMC borders"), fill = NA, lwd = 1, show.legend = "line") +
    geom_sf_text(data = st_point_on_surface(all_shps), aes(label = .data[[id_unit_var]]), size = 3, color = "black") +
    facet_wrap(vars(!!sym(period_var))) +
    scale_color_manual(values = c("AMC borders" = "black"), name = NULL) +
    labs(fill = "Territorial units",
         x = "",
         y = "") +
    guides(fill  = guide_legend(override.aes = list(colour = NA)),
           color = guide_legend(order = 1),
           fill  = guide_legend(order = 2)) +
    theme(legend.position = if (show_legend) legend_position else "none")

  p

}


