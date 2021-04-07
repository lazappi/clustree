#' Clustree node points
#'
#' The clustree points geom is used to add points representing each node in the
#' graph to a clustering tree plot. It is equivalent to
#' [ggraph::geom_node_point()] or [ggplot2::geom_point()] but sets some default
#' aesthetics.
#'
#' @inheritParams ggraph::geom_node_point
#'
#' @section Aesthetics:
#' `geom_clustree_point` understand the following aesthetics. Bold aesthetics
#' are automatically set, but can be overridden.
#'
#' - **x**
#' - **y**
#' - alpha
#' - **colour = res**
#' - fill
#' - shape
#' - **size = size**
#' - stroke
#' - filter
#'
#' @export
#'
#' @seealso [plot_clustree()] for creating the plot object and
#' [geom_clustree_text()] and [geom_clustree_edge()] for other clustering tree
#' geoms. See [ggraph::geom_node_point()] for details on the underlying geom.
#'
#' @examples
#' graph <- build_clustree_graph(nba_clusts, pattern = "K(.*)")
#' plot_clustree(graph) +
#'     geom_clustree_point()
geom_clustree_point <- function(mapping = NULL,
                                data = NULL,
                                position = "identity",
                                show.legend = NA,
                                ...) {

    default_aes <- ggplot2::aes(x = .data$x, y = .data$y, colour = .data$res,
                                size = .data$size)
    mapping <- set_default_aes(mapping, default_aes)

    ggplot2::layer(
        data        = data,
        mapping     = mapping,
        stat        = ggraph::StatFilter,
        geom        = GeomClustreePoint,
        position    = position,
        show.legend = show.legend,
        inherit.aes = FALSE,
        params      = list(na.rm = FALSE, ...)
    )
}

GeomClustreePoint <- ggplot2::ggproto("GeomClustreeNode", GeomPoint)

#' Clustree node text
#'
#' The clustree text geom is used to add text labelling each node in a
#' graph to a clustering tree plot. It is equivalent to
#' [ggraph::geom_node_text()] or [ggplot2::geom_text()] but sets some default
#' aesthetics.
#'
#' @inheritParams ggraph::geom_node_text
#'
#' @section Aesthetics:
#' `geom_clustree_text` understands the following aesthetics. Bold aesthetics
#' are automatically set, but can be overridden.
#'
#' - **x**
#' - **y**
#' - **label = cluster**
#' - alpha
#' - angle
#' - colour
#' - family
#' - fontface
#' - hjust
#' - lineheight
#' - **size = 3**
#' - vjust
#'
#' @export
#'
#' @seealso [plot_clustree()] for creating the plot object and
#' [geom_clustree_point()] and [geom_clustree_edge()] for other clustering tree
#' geoms. See [ggraph::geom_node_text()] for details on the underlying geom.
#'
#' @examples
#' graph <- build_clustree_graph(nba_clusts, pattern = "K(.*)")
#' plot_clustree(graph) +
#'     geom_clustree_text()
geom_clustree_text <- function(mapping = NULL,
                               data = NULL,
                               position = "identity",
                               parse = FALSE,
                               nudge_x = 0,
                               nudge_y = 0,
                               check_overlap = FALSE,
                               show.legend = NA,
                               ...) {

    if (!missing(nudge_x) || !missing(nudge_y)) {
        if (!missing(position)) {
            abort("Specify either `position` or `nudge_x`/`nudge_y`")
        } else {
            position <- ggplot2::position_nudge(nudge_x, nudge_y)
        }
    }

    default_aes <- ggplot2::aes(x = .data$x, y = .data$y, label = .data$cluster)
    mapping <- set_default_aes(mapping, default_aes)

    default_params <- list(size = 3)
    params <- list(parse = parse, check_overlap = check_overlap,
                   na.rm = FALSE, ...)
    params <- set_default_params(params, default_params, mapping)

    ggplot2::layer(
        data        = data,
        mapping     = mapping,
        stat        = ggraph::StatFilter,
        geom        = GeomClustreeText,
        position    = position,
        show.legend = show.legend,
        inherit.aes = FALSE,
        params      = params
    )
}

GeomClustreeText <- ggplot2::ggproto("GeomClustreeText", GeomText)

#' Clustree edges
#'
#' The clustree edges geom is used to add lines representing edges in a
#' graph to a clustering tree plot. It is equivalent to
#' [ggraph::geom_edge_link()] but sets some default aesthetics.
#'
#' @inheritParams ggraph::geom_edge_link
#'
#' @section Aesthetics:
#' `geom_clustree_edge` understands the following aesthetics. Bold aesthetics
#' are automatically set, but can be overridden.
#'
#' - **x**
#' - **y**
#' - **xend**
#' - **yend**
#' - **edge_colour = count**
#' - **edge_width = 1.5**
#' - edge_linetype
#' - **edge_alpha = in_prop**
#' - filter
#' - start_cap
#' - **end_cap = ggraph::circle(15.5, "points")**
#' - label
#' - label_pos
#' - label_size
#' - angle
#' - hjust
#' - vjust
#' - family
#' - fontface
#' - lineheight
#'
#' @export
#'
#' @seealso [plot_clustree()] for creating the plot object and
#' [geom_clustree_point()] and [geom_clustree_text()] for other clustering tree
#' geoms. See [ggraph::geom_edge_link()] for details on the underlying geom.
#'
#' @examples
#' graph <- build_clustree_graph(nba_clusts, pattern = "K(.*)")
#' plot_clustree(graph) +
#'     geom_clustree_edge()
geom_clustree_edge <- function(mapping = NULL,
                               data = ggraph::get_edges("short"),
                               position = "identity",
                               arrow = grid::arrow(),
                               n = 100,
                               lineend = "butt",
                               linejoin = "round",
                               linemitre = 1,
                               label_colour = "black",
                               label_alpha = 1,
                               label_parse = FALSE,
                               check_overlap = FALSE,
                               angle_calc = "rot",
                               force_flip = TRUE,
                               label_dodge = NULL,
                               label_push = NULL,
                               show.legend = NA,
                               ...) {

    default_aes <- ggplot2::aes(
        x           = .data$x,
        y           = .data$y,
        xend        = .data$xend,
        yend        = .data$yend,
        group       = .data$edge.id,
        edge_colour = .data$count,
        edge_alpha  = .data$in_prop
    )
    mapping <- set_default_aes(mapping, default_aes, edge = TRUE)

    default_params <- list(
        edge_width = 1.5,
        arrow      = grid::arrow(
            length = grid::unit(7.5, "points"), ends = "last"
        ),
        end_cap    = ggraph::circle(15.5, "points")
    )
    params <- list(
        arrow         = arrow,
        lineend       = lineend,
        linejoin      = linejoin,
        linemitre     = linemitre,
        na.rm         = FALSE,
        n             = n,
        interpolate   = FALSE,
        label_colour  = label_colour,
        label_alpha   = label_alpha,
        label_parse   = label_parse,
        check_overlap = check_overlap,
        angle_calc    = angle_calc,
        force_flip    = force_flip,
        label_dodge   = label_dodge,
        label_push    = label_push,
        ...
    )
    params <- set_default_params(
        expand_edge_aes(params),
        default_params,
        mapping
    )

    ggplot2::layer(
        data        = data,
        mapping     = mapping,
        stat        = ggraph::StatEdgeLink,
        geom        = GeomClustreeEdge,
        position    = position,
        show.legend = show.legend,
        inherit.aes = FALSE,
        params      = expand_edge_aes(params)
    )
}

GeomClustreeEdge <- ggplot2::ggproto("GeomClustreeEdge", GeomEdgePath)

split_clustree_node_layer <- function(layer, split_by, levels) {
    if (!inherits(layer$data, "waiver")) {
        abort("Data in layer is not a waiver")
    }

    lapply(levels, function(.level) {
        split_layer <- ggedit::cloneLayer(layer)
        split_layer$data <- ggplot2::fortify(as.formula(paste0(
            "~ filter_plot_data(., '", split_by, "', ", .level, ")"
        )))
        split_layer
    })
}

filter_plot_data <- function(data, by, level) {

    filter_values <- factor(data[[by]])
    filter_levels <- levels(filter_values)

    if (level > length(filter_levels)) {
        abort(paste0("There are less than ", level, " levels"))
    }

    data[filter_values == filter_levels[level], ]
}
