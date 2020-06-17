#' Generate a global map for a single variable.
#'
#'
#' @description This general purpose function can be used to generate a global map for a single variable. It has few defaults but
#' the data supplied must contain a \code{country} variable for linking to mapping data. This function requires the 
#' installation of the `rnaturalearth` package.
#' @param data Dataframe containing variables to be mapped. Must contain a \code{country} variable.
#' @param variable A character string indicating the variable to map data for. This must be supplied.
#' @return A \code{ggplot2} object containing a global map.
#' @export
#'
#' @inheritParams theme_map
#' @importFrom ggplot2 ggplot aes geom_sf theme_minimal theme labs waiver coord_sf .data
#'
#' @examples
#'\dontrun{
#'df <- data.table::data.table(variable = "Increasing", country = "France") 
#'
#'global_map(df, variable = "variable")
#'}
global_map <- function(data = NULL, variable = NULL,
                       variable_label = NULL,
                       trans = "identity",
                       fill_labels = NULL,
                       scale_fill = NULL,
                       ...) {
  
  # Prep --------------------------------------------------------------------
  
  country <- NULL; subregion <- NULL;
  
  if (is.null(data)) {
    stop("A dataset must be supplied containing at least one variable to map.")
  }
  
  if (is.null(data$country)) {
    stop("A country variable must be present in order to link to mapping data.")
  }
  
  if (is.null(variable)) {
    stop("A variable must be supplied as a character string.")
  }
  
  if (is.null(variable_label)){
    variable_label <- variable
  }
  
  
  if (is.null(fill_labels)) {
    fill_labels <- ggplot2::waiver()
  }
  
  
  # Get countrywide ---------------------------------------------------------
  
  data <- data.table::as.data.table(data)[, 
                                          country_code := countrycode::countrycode(country,
                                                                                   origin = "country.name",
                                                                                   destination = "iso3c")]
  
  # Get shape file ----------------------------------------------------------
  
  ## Country level
  world <- rnaturalearth::ne_countries(scale='medium',
                                       returnclass = 'sf')
  ## Coastlines
  continents <- rnaturalearth::ne_coastline(scale = "medium",
                                            returnclass = "sf")
  
  
  # Link data and shape file ------------------------------------------------
  
  world_with_data <- suppressWarnings(
    merge(world, data[, `:=`(iso_a3 = country_code, country = NULL)],
          by = c("iso_a3"), all.x = TRUE)
  )
  
  # Make map ----------------------------------------------------------------
  map <- ggplot2::ggplot(world_with_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[variable]]), col = "white", size = 0.2) +
    ggplot2::geom_sf(data = continents, col = "darkgrey", alpha = 0.6, size = 0.2) +
    ggplot2::coord_sf()
  
  map <- 
    EpiNow::theme_map(map, continuous = is.numeric(world_with_data[[variable]]),
                      variable_label = variable_label,
                      trans = trans,
                      fill_labels = fill_labels,
                      scale_fill = scale_fill,
                      breaks = levels(world_with_data[[variable]]),
                      ...)
  
  
  return(map)
}


#' Generate a country map for a single variable.
#'
#'
#' @description This general purpose function can be used to generate a country map for a single variable. It has few defaults but
#' the data supplied must contain a \code{region_code} variable for linking to mapping data. This function requires 
#' the installation of the `rnaturalearth` package.
#' @param data Dataframe containing variables to be mapped. Must contain a \code{region_code} variable.
#' @param country Character string indicating the name of the country to be mapped.
#' @inheritParams global_map
#' @return A \code{ggplot2} object containing a country map.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_sf theme_minimal theme labs waiver .data
#'
#' @examples
#'
#'country_map
country_map <- function(data = NULL, country = NULL,
                        variable = NULL,
                        variable_label = NULL,
                        trans = "identity",
                        fill_labels = NULL,
                        scale_fill = NULL,
                        ...) {
  
  
  
  if (is.null(variable_label)) {
    variable_label <- variable
  }
  
  
  # Get shapes --------------------------------------------------------------
  
  
  country <- rnaturalearth::ne_countries(scale="large",
                                         country = country,
                                         returnclass = 'sf')
  
  regions <- rnaturalearth::ne_states(country, returnclass = "sf")
  
  
  ## Update linking code
  data <- data.table::as.data.table(date)[, provnum_ne := region_code]
  
  regions_with_data <-  
    merge(regions, data,
          by = c("provnum_ne"), all.x = TRUE)
  
  
  
  
  if (is.null(fill_labels)) {
    fill_labels <- ggplot2::waiver()
  }
  
  
  # Make map ----------------------------------------------------------------
  
  map <-  
    ggplot(regions_with_data) + 
    ggplot2::geom_sf(aes(fill = .data[[variable]]), col = "white", alpha = 0.8, size = 0.2) +
    ggplot2::geom_sf(data = country, col = "darkgrey", fill = NA, alpha = 1, size = 0.4)
  
  
  map <-
    EpiNow::theme_map(map, continuous = is.numeric(regions_with_data[[variable]]),
                      variable_label = variable_label,
                      trans = trans,
                      fill_labels = fill_labels,
                      scale_fill = NULL,
                      breaks = levels(regions_with_data[[variable]]),
                      ...)
  
  return(map)
}


#' Custom Map Theme
#'
#' @param map `ggplot2` map object 
#' @param continuous Logical defaults to `FALSE`. Is the fill variable continuous.
#' @param variable_label A character string indicating the variable label to use. If not supplied then the underlying
#' variable name is used.
#' @param trans A character string specifying the transform to use on the specified metric. Defaults to no
#' transform ("identity"). Other options include log scaling ("log") and log base 10 scaling
#' ("log10"). For a complete list of options see \code{ggplot2::continous_scale}.
#' @param fill_labels A function to use to allocate legend labels. An example (used below) is \code{scales::percent},
#' which can be used for percentage data.
#' @param scale_fill Function to use for scaling the fill. Defaults to a custom `ggplot2::scale_fill_manual`
#' @param breaks Breaks to use in legend. Defaults to `ggplot2::waiver`.
#' @param ... Additional arguments passed to the `scale_fill` function
#' @return A `ggplot2` object 
#' @importFrom ggplot2 waiver theme guides scale_fill_manual
#' @export
#'
#' @examples
#' 
#' 
#' ## Code 
#' theme_map
theme_map <- function(map = NULL, continuous = FALSE,
                      variable_label = NULL,
                      trans = "identity",
                      fill_labels = NULL,
                      scale_fill = NULL,
                      breaks = NULL, 
                      ...){
  
  
  if (is.null(scale_fill)) {
    scale_fill = ggplot2::scale_fill_manual
    values <- c(
      "Increasing" = "#e75f00",
      "Likely increasing" = "#fd9e49",
      "Likely decreasing" = "#5fa2ce",
      "Decreasing" = "#1170aa",
      "Unsure" = "#7b848f")
  }
  
  if (is.null(breaks)) {
    breaks <- ggplot2::waiver()
  }
  
  map <- map +
    cowplot::theme_map() +
    ggplot2::theme(legend.position = "bottom")
  
  # Add map details ---------------------------------------------------------
  
  if (continuous) {
    map <- map +
      ggplot2::guides(fill = ggplot2::guide_colorbar(title = variable_label,
                                                     barwidth = 15, barheight = 0.5)) +
      scale_fill(
        trans = trans,
        alpha = 0.7,
        labels = fill_labels,
        option = viridis_palette,
        na.value = "#c8d0d9"
      )
    
  }else{
    map <- map +
      ggplot2::guides(fill = ggplot2::guide_legend(title = variable_label, ncol = 2)) +
      scale_fill(
        values = values,
        labels = fill_labels,
        breaks = breaks,
        na.value = "#c8d0d9",
        drop = FALSE,
        ...
      )
  }
  
  
  return(map)
}