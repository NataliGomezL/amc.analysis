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
                                   legend_position = "right",
                                   label_size = 3) {

  all_shps <- do.call(rbind, shps) %>% sf::st_as_sf()

  p <- ggplot2::ggplot(all_shps) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[id_unit_var]])) +
    ggplot2::facet_wrap(vars(!!rlang::sym(period_var))) +
    ggplot2::labs(fill = "Territorial units", x = "", y = "")

  if (show_legend) {
    p <- p + ggplot2::theme(legend.position = legend_position)
  } else {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  if (show_label) {
    p <- p + ggplot2::geom_sf_text(data = sf::st_point_on_surface(all_shps),
                                   ggplot2::aes(label = .data[[id_unit_var]]),
                                   size = label_size,
                                   color = "black",
                                   inherit.aes = FALSE,
                                   check_overlap = TRUE)
  }

  p
}


###==========================================
## Plot AMC's and territorial units by period
###==========================================
#' @export

plot_AMC_byP <- function(shps,
                         id_unit_var,
                         period_var,
                         amc_df,
                         show_legend = TRUE,
                         show_label = FALSE,
                         legend_position = "right",
                         label_size = 3) {

  all_shps <- do.call(rbind, shps) %>% st_as_sf()

  p <- ggplot() +
    # polygon layer (territorial units)
    ggplot2::geom_sf(data = all_shps, aes(fill = .data[[id_unit_var]]), color = NA) +
    # black boundary AMCs
    ggplot2::geom_sf(data = amc_df, aes(color = "AMC borders"), fill = NA, lwd = 1, show.legend = "line") +
    ggplot2::facet_wrap(vars(!!sym(period_var))) +
    ggplot2::scale_color_manual(values = c("AMC borders" = "black"), name = NULL) +
    ggplot2::labs(fill = "Territorial units",
                  x = "",
                  y = "") +
    ggplot2::guides(fill  = guide_legend(override.aes = list(colour = NA)),
                    color = guide_legend(order = 1),
                    fill  = guide_legend(order = 2))

  if (show_legend) {
    p <- p + ggplot2::theme(legend.position = legend_position)
  } else {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  if (show_label) {
    p <- p + ggplot2::geom_sf_text(data = sf::st_point_on_surface(all_shps),
                                   ggplot2::aes(label = .data[[id_unit_var]]),
                                   size = label_size,
                                   color = "black",
                                   inherit.aes = FALSE,
                                   check_overlap = TRUE)
  }
  p

}


