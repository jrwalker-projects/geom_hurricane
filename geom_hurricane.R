#' @rdname geom_hurricane
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 GeomPolygon
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 layer
#' @export
GeomHurricane <- ggplot2::ggproto("GeomPolygon", ggplot2::GeomPolygon,
                                  default_aes = ggplot2::aes(colour = "green", fill = NA, linetype = 1, size = 1, alpha = 0.6)
)
#' Displays a wind radii chart from a single storm observation
#'
#' \code{geom_hurricane} is a clone of ggplot2 geom_polygon that can be used to envoke stat_hurricane (the default stat for this
#' geom) to display a polygon (or multiple polygons) of four qurter circles from a location to show
#' wind radii from a location. On a map the center location is latitude and longitude and the four quarter circles
#' are the distances in nautical miles from the center for the wind speed provided to the northeast, northwest, 
#' southeast and southwest of the center. 
#' \code{GeomHurricane} is the \code{ggproto} environment object to enable \code{geom_hurricane}
#'
#' @param mapping Set of aesthetic mappings created by \code{aes()} or \code{aes_()}. If specified and inherit.aes = TRUE (the default), it is
#' combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:If NULL, the default, the data is inherited from the
#' plot data as specified in the call to \code{ggplot()}.A data.frame, or other object, will override the plot data. All objects will be
#' fortified to produce a data frame. See \code{fortify()} for which variables will be created.A function will be called with a 
#' single argument, the plot data. The return value must be a data.frame., and will be used as the layer data.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function
#' @param ... other arguments passed on to \code{layer()}. These are often aesthetics, used to set an aesthetic to a fixed value, 
#' like color = "red" or size = 3. They may also be parameters to the paired geom/stat.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped.
#' FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper
#' functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders().
#' @param Scale_radii the radius of the quarter circle polygons can be adjusted up or down with a scaling parameter above or 
#' below a value of 1. Default is 1. This is the only parameter added to the geom_polygon parameters. This parameter is passed on 
#' to \code{stat_hurricane} if the default stat is used.
#' \section {Aesthetics}{
#' geom_hurricane understands the following aesthetics (required aesthetics are in bold):
#'   \strong{x}
#'   \strong{y}
#' where x,y is the center location (for maps in latitude, longitude)
#' 
#' if the default stat - \code{stat_hurricane} - is used the following 4 aesthetics are required:
#'   \strong{r_ne}
#'   \strong{r_nw}
#'   \strong{r_se}
#'   \strong{r_sw}
#' where x,y is the center location (for maps in latitude, longitude) and the values in the four directions are in nautical miles
#'
#' the following aesthetics are optional:
#'   alpha
#'   colour
#'   fill
#'   group
#'   linetype
#'   size
#' }
#' 
#' @return a ggplot object that can be combined with ggplot or ggmap - this object behaves like a \code{geom_polygon} ggplot object
#'
#' @examples
#' hurri <- data.frame(latitude=c(29.1), longitude=c(-94.6), 
#'            wind_speed=as.factor(c(34,50,64)),ne=c(225,150,110),nw=c(125,75,45),se=c(200,160,90),sw=c(125,80,55))
#' ggplot() + 
#'  geom_hurricane(data = hurri, aes(x = longitude, y = latitude, r_ne=ne, r_nw=nw, r_se=se, r_sw=sw,
#'                               colour=wind_speed, fill=wind_speed),
#' \dontrun{
#' #A more complete example call placing the polygons on a map 
#' map_data <- ggmap::get_map("Louisiana", zoom = 6, maptype = "toner-background")
#' base_map <- ggmap::ggmap(map_data, extent = "device")
#' base_map +
#'  geom_hurricane(data = hurri, aes(x = longitude, y = latitude,
#'                                   r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#'                                   fill = wind_speed, color = wind_speed)) +
#'  scale_color_manual(name = "Wind speed (kts)", values = c("red", "orange", "yellow")) +
#'  scale_fill_manual(name = "Wind speed (kts)", values = c("red", "orange", "yellow"))
#'}
#'
#' @seealso \code{\link{stat_hurricane}} for the the related stat (the stat can be called directly)
#'
#' @references for more information on ggplot see http://ggplot2.org/ and http://ggplot2.tidyverse.org/
#'
#' @importFrom ggplot2 layer
#'
#' @export
geom_hurricane <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, 
                           scale_radii = 1, ...) {
  ggplot2::layer(
    stat = StatHurricane, geom = GeomHurricane, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(scale_radii = scale_radii, na.rm = na.rm, ...)
  )
}
#' @rdname stat_hurricane
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @importFrom geosphere destPoint
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr bindrows
#' @importFrom tidyr gather
#' @importFrom purrr pmap
#' @importFrom magrittr %>%
#' @export
StatHurricane <- ggplot2::ggproto("StatHurricane", ggplot2::Stat, 
                                  required_aes = c("x", "y", "r_ne", "r_se", "r_nw", "r_sw"),
                                  
                                  compute_group = function(data, scales, scale_radii) {
                                    nm_to_meters <- 1852 #conversion factor nautical miles to meters
                                    clon <- data$x[1] #longitude at center
                                    clat <- data$y[1] #latitude at center
                                    cntr_row <- data.frame(lon=clon, lat=clat) 
                                    
                                    new_arc <- function(dir, rad){
                                      my_arc <- geosphere::destPoint(c(clon, clat), #hurricane center
                                                                     dplyr::case_when( #bearing is the quadrant
                                                                       dir == "r_ne" ~ 0:90,
                                                                       dir == "r_se" ~ 90:180,
                                                                       dir == "r_sw" ~ 180:270,
                                                                       dir == "r_nw" ~ 270:360
                                                                     ), 
                                                                     rad*nm_to_meters*scale_radii) %>% #input is nautical miles - destPoint works in meters
                                        base::rbind(cntr_row) %>% #each quarter circle is the geosphere arc plus the center point
                                        dplyr::rename(x=lon, y=lat)
                                    }
                                    parms <- data %>%
                                      dplyr::select(r_ne, r_nw, r_se, r_sw) %>%
                                      tidyr::gather(direction, radius) 
                                    grid <- purrr::pmap(list(parms$direction, parms$radius), new_arc) %>%
                                      dplyr::bind_rows()
                                    return(grid)
                                  }
)
#' Calculate the position for a wind radii chart from a single storm observation
#'
#' \code{stat_hurricane} calculates a polygon (or multiple polygons) of four qurter circles from a location to show
#' wind radii from a location. On a map the center location is latitude and longitude and the four quarter circles
#' are the distances in nautical miles from the center for the wind speed provided to the northeast, northwest, 
#' southeast and southwest of the center.
#' \code{StatHurricane} is the \code{ggproto} environment object to enable \code{stat_hurricane}
#'
#' @param mapping Set of aesthetic mappings created by \code{aes()} or \code{aes_()}. If specified and inherit.aes = TRUE (the default), it is
#' combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:If NULL, the default, the data is inherited from the
#' plot data as specified in the call to \code{ggplot()}.A data.frame, or other object, will override the plot data. All objects will be
#' fortified to produce a data frame. See \code{fortify()} for which variables will be created.A function will be called with a 
#' single argument, the plot data. The return value must be a data.frame., and will be used as the layer data.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function
#' @param ... other arguments passed on to \code{layer()}. These are often aesthetics, used to set an aesthetic to a fixed value, 
#' like color = "red" or size = 3. They may also be parameters to the paired geom/stat.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped.
#' FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper
#' functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders().
#' @param Scale_radii the radius of the quarter circle polygons can be adjusted up or down with a scaling parameter above or below a value of 1. Default is 1. This is the only parameter added to the geom_polygon parameters.
#' \section {Aesthetics}{
#' stat_hurricane understands the following aesthetics (required aesthetics are in bold):
#'   \strong{x}
#'   \strong{y}
#'   \strong{r_ne}
#'   \strong{r_nw}
#'   \strong{r_se}
#'   \strong{r_sw}
#' where x,y is the center location (for maps in latitude, longitude) and the values in the four directions are in nautical miles
#'   alpha
#'   colour
#'   fill
#'   group
#'   linetype
#'   size
#' }
#' @return a ggplot object that can be combined with ggplot or ggmap - this object behaves like a \code{geom_polygon} ggplot object
#'
#' @examples
#' hurri <- data.frame(latitude=c(29.1), longitude=c(-94.6), 
#'            wind_speed=as.factor(c(34,50,64)),ne=c(225,150,110),nw=c(125,75,45),se=c(200,160,90),sw=c(125,80,55))
#' ggplot() + 
#'  stat_hurricane(data = hurri, aes(x = longitude, y = latitude, r_ne=ne, r_nw=nw, r_se=se, r_sw=sw,
#'                               colour=wind_speed, fill=wind_speed),
#' \dontrun{
#' #A more complete example call placing the polygons on a map 
#' map_data <- ggmap::get_map("Louisiana", zoom = 6, maptype = "toner-background")
#' base_map <- ggmap::ggmap(map_data, extent = "device")
#' base_map +
#'  stat_hurricane(data = hurri, aes(x = longitude, y = latitude,
#'                                   r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#'                                   fill = wind_speed, color = wind_speed)) +
#'  scale_color_manual(name = "Wind speed (kts)", values = c("red", "orange", "yellow")) +
#'  scale_fill_manual(name = "Wind speed (kts)", values = c("red", "orange", "yellow"))
#'}
#'
#' @seealso \code{\link{geom_hurricane}} for the the related geom (the stat can be called directly)
#'
#' @references for more information on ggplot see http://ggplot2.org/ and http://ggplot2.tidyverse.org/
#'
#' @importFrom ggplot2 layer
#'
#' @export
stat_hurricane <- function(mapping = NULL, data = NULL, geom = "polygon", 
                           position = "identity", na.rm = FALSE, show.legend = NA, 
                           inherit.aes = TRUE, scale_radii = 1, ...) {
  ggplot2::layer(
    stat = StatHurricane, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(scale_radii = scale_radii, na.rm = na.rm, ...)
  )
}
