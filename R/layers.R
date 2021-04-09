#' Alternate clustering tree layers
#'
#' Alternate layers of a `ggplot` object representing the nodes and edges of a
#' clustering tree so that they are plotted in the expected order. Usually used
#' with a plot created with [overlay_clustree()].
#'
#' @param plot A `ggplot` object
#' @param split_by Name of the column in `plot$data` to split layers by
#'
#' @details
#' The function first searches for layers in the plot created with
#' [`geom_clustree_point()`] and [`geom_clustree_edge()`] (exactly one of each).
#' If these are found the layers are extracted and split according to values in
#' `split_by`. The resulting split layers are then alternated (node, edge, node,
#' ...) and placed back in the original plot object. Any other layers already in
#' the plot should remain in the same position, except any placed between the
#' original node and edge layers which will be moved towards the top (later in
#' the plotting order).
#'
#' **Limitations**
#'
#' Because this function modifies the internal structure of a `ggplot` object it
#' has some limitations and assumptions. These mean it may not be compatible
#' with more complex clustering tree plots, such as when custom data has been
#' supplied to a layer. It may also cause other unwanted changes to the plot so
#' should be used with some amount of caution.
#'
#' @return A modified `ggplot` object, see [ggplot2::ggplot()] for details
#' @export
#'
#' @examples
#' graph <- build_clustree_graph(nba_clusts, pattern = "K(.*)")
#' graph <- summarise_metadata(graph, PC1 = mean(PC1), PC2 = mean(PC2))
#' overlay <- overlay_clustree(graph, x_dim = "PC1", y_dim = "PC2")
#' plot <- plot_clustree(overlay) +
#'     geom_clustree_point() +
#'     geom_clustree_edge()
#' alternate_clustree_layers(plot)
alternate_clustree_layers <- function(plot, split_by = "res") {

    abort_class(plot, "ggraph")

    which_nodes <- which_geom_layers(plot$layers, "GeomClustreePoint")
    which_edges <- which_geom_layers(plot$layers, "GeomClustreeEdge")

    if (length(which_nodes) != 1) {
        abort("There must be exactly 1 GeomClustreePoint layer")
    }

    if (length(which_edges) != 1) {
        abort("There must be exactly 1 GeomClustreeEdge layer")
    }

    split_levels <- seq_along(unique(plot$data[[split_by]]))

    split_nodes <- split_layer(
        plot$layers[[which_nodes]],
        split_by,
        split_levels
    )
    split_edges <- split_layer(
        plot$layers[[which_edges]],
        split_by,
        split_levels[-length(split_levels)],
        edge = TRUE
    )

    layer_list <- lapply(seq_along(split_edges), function(.idx) {
        c(split_nodes[.idx], split_edges[.idx])
    })
    layer_list <- c(layer_list, split_nodes[length(split_nodes)])
    layers_list <- rev(unlist(layer_list))

    layer_idx <- seq_along(plot$layers)
    which_min <- min(which_nodes, which_edges)
    which_max <- max(which_nodes, which_edges)
    before <- plot$layers[layer_idx < which_min]
    after <- plot$layers[layer_idx > which_min & layer_idx != which_max]

    plot$layers <- c(before, layers_list, after)

    return(plot)
}

which_geom_layers <- function(layers_list, geom_class) {

    classes_list <- lapply(layers_list, function(.layer) {
        class(.layer$geom)
    })

    is_geoms <- vapply(classes_list, function(.classes) {
        geom_class %in% .classes
    }, logical(1))

    which(is_geoms)
}

#' @importFrom stats as.formula
split_layer <- function(layer, split_by, levels, edge = FALSE) {

    if (edge) {
        if (!("ggproto_method" %in% class(layer$data))) {
            abort("Data in edge layer has been modified from default")
        }
        data_fun <- environment(layer$data)$f
        if (!isTRUE(all.equal(data_fun, ggraph::get_edges("short")))) {
            abort("Data in edge layer has been modified from default")
        }
    } else {
        if (!inherits(layer$data, "waiver")) {
            abort("Data in node layer is not a waiver")
        }
    }

    lapply(levels, function(.level) {
        filter_fun_name <- ifelse(edge, "filter_edge_data", "filter_node_data")
        filter_formula <- as.formula(paste0(
            "~ ", filter_fun_name, "(
                ., '", split_by, "', ", .level,
            ")"
        ))

        split_layer <- ggplot2::ggproto(NULL, layer)
        split_layer$data <- ggplot2::fortify(filter_formula)
        split_layer
    })
}

filter_node_data <- function(data, by, level) {

    filter_values <- factor(data[[by]])
    filter_levels <- levels(filter_values)

    if (level > length(filter_levels)) {
        abort(paste0("There are less than ", level, " levels"))
    }

    data <- data[filter_values == filter_levels[level], ]

    return(data)
}

filter_edge_data <- function(data, by, level) {

    by <- paste0("node1.", by)

    data <- ggraph::get_edges("short")(data)

    filter_values <- factor(data[[by]])
    filter_levels <- levels(filter_values)

    if (level > length(filter_levels)) {
        abort(paste0("There are less than ", level, " levels"))
    }

    data <- data[filter_values == filter_levels[level], ]

    return(data)
}
