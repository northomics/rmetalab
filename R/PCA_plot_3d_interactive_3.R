

#________________________________________________________________________________________

#     PCA_plot_3d_interactive
#________________________________________________________________________________________

# ___Description___:
# interactive 3d plot of the pca result
# using 3d function of plotly


# ___Arguments___:
#
# grouping information is used just for plotting, if not given, there will be no grouping plot on the figure
# grouping information is required to be in a data.frame, with one column named as sample.name, one colum named as Groups

#____Usage____;

# PCA_plot_3d_interactive(data_matrix, grouping)$pca.plot

# ___Values___:
# a general objective of plotly
#

#colnames(meta_test) <- c("Sample.Name", "Groups")

#PCA_plot_3d_interactive(data_matrix = matrix_test, grouping = meta_test )
#' Title
#'
#' @param prcomp_out
#' @param grouping
#'
#' @return
#' @export
#'

PCA_plot_3d_interactive_3<-function(prcomp_out, grouping){
  suppressMessages(install.packages.auto(plotly))

  loading <- as.data.frame(prcomp_out$x)
  # reorder the meta (grouping information)
  grouping <- grouping[match(rownames(loading), grouping[,1]),]

  loading$Sample.Name <- rownames(loading)
  loading$Groups <- grouping[,2]

  #loading_merge <- as.data.frame(merge(grouping, loading, by.y=0, by.x = "Sample.Name"))
  #row.names(loading_merge)<-loading_merge$Sample.Name

  p1 <- plot_ly(loading, x = ~PC1, y = ~PC2, z = ~PC3, color = grouping[,2], colors = c('#BF382A', '#0C4B8E', "#1ABC9C")) %>%
    add_markers() %>%
    #add_text(loading_merge, x = ~PC1, y = ~PC2, z = ~PC3,text = ~Sample.Name) %>%
    add_text(text = grouping[,1]) %>%
    layout(scene = list(xaxis = list(title = 'PC1'),
                        yaxis = list(title = 'PC2'),
                        zaxis = list(title = 'PC3')))

  return(p1)
}
