#' Produce a DiagrammeR plot and show it and/or convert to SVG
#'
#' This renders a plot with the criteria tree. If the `criteriaTree` object
#' in the `criteria` object already contains the weights (as can be added with
#' [mdmcda::add_scorerWeights_to_criteriaTree()] or by a call
#' to [mdmcda::scorerWeights_to_profile()] or
#' [mdmda::combine_weights_and_criteria()]), those will be added and used to
#' color the tree and set the edge thicknesses.
#'
#' @param criteria The criteria object as produced by a call to, for
#' example, [load_criteria_from_xl()]. The object should contain the
#' criteria tree in `$criteriaTree`.
#' @param labels Optionally, labels to apply to the criteria nodes.
#' @param wrapLabels If applying labels, the number of characters to wrap
#' them to.
#' @param renderGraph Whether to show the graph.
#' @param returnSVG Whether to return SVG or not. Valid values are `FALSE`,
#' to return the `DiagrammeR` object; `TRUE`, to return the SVG;
#' and anything that's not logical to return both in a list.
#' @param outputFile If not NULL and pointing to a file in an existing
#' directory, the graph will be exported to this file.
#' @param ... Additional arguments are passed to `DiagrammeR`'s
#' `export_graph` function.
#'
#' @return Depending on the value of `returnSVG`, a `DiagrammeR` object;,
#' a character vector with SVG, or both in a list.
#' @export
plot_criteria <- function(criteria,
                          labels = NULL,
                          wrapLabels = 60,
                          renderGraph = TRUE,
                          returnSVG = FALSE,
                          outputFile = NULL,
                          ...) {

  if (is.null(labels)) {
    labels <-
      criteria$criteriaTree$Get('name');
  }
  labelNames <- names(labels);

  ### Get weights, if they are present
  originalWeights <-
    criteria$criteriaTree$Get('rescaled');
  finalWeights <-
    criteria$criteriaTree$Get('rescaled_product');
  originalWeights <-
    ifelse(is.na(originalWeights), 0, round(originalWeights, 2));
  finalWeights <-
    ifelse(is.na(finalWeights), 0, round(finalWeights, 2));

  ### Create graph
  graph <-
    data.tree::ToDiagrammeRGraph(
      criteria$criteriaTree
    );

  ### If weights were present, add and visualise thise
  if (sum(finalWeights, na.rm=TRUE) > 0) {

    labels <-
      paste0(labels,
             " (",
             finalWeights[names(labels)],
             ")");

    node_df <-
      DiagrammeR::get_node_df(graph);

    edge_df <-
      DiagrammeR::get_edge_df(graph);

    ### Store old labels (the identifiers) and add node weights
    node_df$criterion_id <- node_df$label;
    node_df$finalWeights <- finalWeights[node_df$criterion_id];

    ### Convenience vector to translate Diagrammer ids (numbers) to criterion_ids
    criterionId_to_dgrmId  <-
      stats::setNames(
        node_df$criterion_id,
        nm = node_df$id
      );

    ### Add identifiers to edges
    edge_df$to_criterion_id <-
      criterionId_to_dgrmId[edge_df$to];

    ### Add original weights to edges
    edge_df$originalWeights <-
      originalWeights[edge_df$to_criterion_id];

    ### Add weights to nodes
    graph <-
      DiagrammeR::set_node_attrs(
        graph,
        "finalWeights",
        node_df$finalWeights,
        node_df$id
      );

    ### Set node style to filled
    graph <-
      DiagrammeR::set_node_attrs(
        graph,
        "style",
        "filled"
      );

    ### Add colours based on node weights
    graph <-
      DiagrammeR::colorize_node_attrs(
        graph = graph,
        node_attr_from = "finalWeights",
        node_attr_to = "weightColors",
        palette = "viridis",
        alpha = 100,
        reverse_palette = TRUE
      );

    ### Get updated node_df
    node_df <-
      DiagrammeR::get_node_df(graph);

    ### Set node text color and pen width (fill doesn't work?)
    graph <-
      DiagrammeR::set_node_attrs(
        graph,
        "fillcolor",
        node_df$weightColors,
        node_df$id
      );

    ### Set edge labels
    graph <-
      DiagrammeR::set_edge_attrs(
        graph,
        "label",
        edge_df$originalWeights,
        edge_df$id
      );

    ### Set edge thickness
    graph <-
      DiagrammeR::set_edge_attrs(
        graph,
        "penwidth",
        .5 + (5 * edge_df$originalWeights),
        edge_df$id
      );
  }

  ### Wrap and set labels
  if (is.numeric(wrapLabels)) {
    labels <-
      unlist(
        lapply(
          lapply(
            labels,
            strwrap,
            width = wrapLabels
          ),
          paste0,
          collapse="\n"
        )
      );
  }
  names(labels) <- labelNames;

  ### Replace node labels
  graph <-
    DiagrammeR::set_node_attrs(
      graph,
      "label",
      labels[node_df$label],
      node_df$id
    );

  ### Final global settings
  graph <-
    DiagrammeR::add_global_graph_attrs(graph,
                                       "shape", "box",
                                       "node");
  graph <-
    DiagrammeR::add_global_graph_attrs(graph,
                                       "layout", "dot",
                                       "graph");
  graph <-
    DiagrammeR::add_global_graph_attrs(graph,
                                       "rankdir", "LR",
                                       "graph");
  graph <-
    DiagrammeR::add_global_graph_attrs(graph,
                                       "outputorder", "nodesfirst",
                                       "graph");

  ### Render graph
  if (renderGraph) {
    print(DiagrammeR::render_graph(graph));
  }

  ### Potentially save graph
  if (!is.null(outputFile)) {
    if (dir.exists(dirname(outputFile))) {
      DiagrammeR::export_graph(
        graph,
        file_name = outputFile,
        ...
      );
    } else {
      stop("You wanted to save the criteria plot to a file in a non-existent ",
           "directory ('", outputFile, "')!");
    }
  }

  if (is.logical(returnSVG) && (!returnSVG)) {
    return(invisible(graph));
  }

  ### Produce SVG
  dot_code <- DiagrammeR::generate_dot(graph);
  graphSvg <-
    DiagrammeRsvg::export_svg(DiagrammeR::grViz(dot_code));
  graphSvg <-
    sub(".*\n<svg ", "<svg ", graphSvg);
  graphSvg <- gsub('<svg width=\"[0-9]+pt\" height=\"[0-9]+pt\"\n viewBox=',
                   '<svg viewBox=',
                   graphSvg);

  if (is.logical(returnSVG) && (returnSVG)) {
    return(graphSvg);
  } else {
    return(list(graph = graph,
                graphSvg = graphSvg));
  }
}
