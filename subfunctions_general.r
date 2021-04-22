







#________________________________________________________________________________________
#     color brewer section
#________________________________________________________________________________________

install.packages.auto("RColorBrewer")



# display.brewer.all() # to display the color pannel
# display.brewer.pal(7,"BrBG")
# display.brewer.pal(7,"Accent") good color pannel for qualative

# mycolor_gradient <- brewer.pal(7,"Greens")
# mycolor_discret <- brewer.pal(7,"Accent")

# brewer.pal
# mycol <- colorRampPalette(c("blue", "white", "red"))(100)





#________________________________________________________________________________________
#     output_SVG_png
#________________________________________________________________________________________

# ___Description___:
# 1: This function writes out any recorded or ggplot2 plot into svg and png format
# 2: for some grdevices not supported figures, use output_SVG_png2 for plotting

# ___Arguments___:
# path, it has to be with last "/",  default is current
# name tag
# frequently used with and height


#____Usage____;
# filename_tag and graph is mandatory


# ___Values___:
#  this function does not have return values, only write figure to hardrives
# if succeed in writing, return "1";



output_SVG_png <- function(path = "./plots/", filename_tag, graph, width = 10, height = 8){

  install.packages.auto(svglite) # the svg driver, letting R outpuf figure as avg format
  install.packages.auto(rsvg) # convert rsvg into other formats
  svglite(paste(path, filename_tag, ".svg", sep=""), width = width, height = height)
  replayPlot(graph) # replot the graph
  dev.off()
  rsvg_png(paste(path, filename_tag, ".svg", sep=""), paste(path, filename_tag, ".png", sep=""))
  return("File saved!")
}


output_SVG_png2 <- function(path = "./plots/",filename_tag, graph, width = 10, height = 8){
  install.packages.auto(rsvg) # convert rsvg into other formats
  svg(paste(path,filename_tag, ".svg", sep=""),onefile = TRUE, width = width, height = height)
  plot(graph) # replot the graph
  dev.off()
  rsvg_png(paste(path,filename_tag, ".svg", sep=""), paste(path,filename_tag, ".png", sep=""))
  return("File saved!")
}

output_SVG_png_plotlist <- function(path = "./plots/", plotlist, prefix = NULL){
  file_NamesTags <- names(plotlist)
  for(i in 1:length(plotlist)){
    try(output_SVG_png(path = path, filename_tag = paste(prefix, file_NamesTags[i], sep = "_"), graph = plotlist[[i]]), silent = TRUE)
  }
}




#________________________________________________________________________________________
#     PCA_plot
#________________________________________________________________________________________

# ___Description___:
# 1: classical 2d plot of


# ___Arguments___:
# data matrix of tidy format
# grouping information is used just for plotting, if not given, there will be no grouping plot on the figure
# grouping information is required to be in a data.frame, with one column named as sample.name, one colum named as Groups

#____Usage____;

# PCA_plot(data_matrix, grouping)$pca.plot

# ___Values___:
# pca.plot as a ggplot2 object, to plot out the PCA plot
#

#p <- PCA_plot(data_matrix = matrix_test, grouping = meta_test)



PCA_plot<-function(data_matrix, grouping){
  suppressMessages(install.packages.auto(ggplot2))
  suppressMessages(install.packages.auto(ggfortify))# for autoplot

  # reorder the meta (grouping information)
  grouping <- grouping[match(colnames(data_matrix), grouping[,1]),]

  # check the order
  if(any(colnames(data_matrix) !=  grouping[,1])){stop("unmatched sample name/colun names")}



  data_matrix_t<-t(data_matrix)

  if(missing(grouping)){
    p1<-autoplot(prcomp(data_matrix_t), label = TRUE )
  }else{

    # combine the
    data_matrix_t_merge <- merge(grouping, data_matrix_t, by.y=0, by.x = colnames(grouping)[1])
    row.names(data_matrix_t_merge)<-data_matrix_t_merge[,1]
    p1<-autoplot(prcomp(data_matrix_t), data = data_matrix_t_merge, colour = colnames(data_matrix_t_merge)[2],label = TRUE )

  }

  p1<-p1+labs(title = "PCA Clustering")
  p1<-p1+theme_bw()+theme(plot.title = element_text(hjust = 0.5))

  return(list(pca.plot = p1))

}


#________________________________________________________________________________________
#     PCA_plot_with_ellipse_kmeans
#     PCA_plot_with_ellipse_kmeans_2
#________________________________________________________________________________________

# ___Description___:
# 1: do pca on columns,
# 2: plot a pca plot, with eliipse on the groups
# 3: note that is a indirect strategy, using result of prcomp to plot points, using kmeans grouping result to plot groups
# # level of 0.95 as confidence to draw the ellipse


# ___Argument___:
# a data matrix, with  samples as columns, and features as rows,

#___Usage___:
# p <- PCA_plot_with_ellipse_kmeans(t(iris[,1:4]),iris$Species)
# p


# ___Values___:
#  a ggplot2 plot, with points and ellipse
#


PCA_plot_with_ellipse_kmeans <- function(data_matrix, grouping){
  suppressMessages(install.packages.auto(ggplot2))
  pca    <- prcomp(t(data_matrix), retx=T, scale.=T) # do pca
  scores <- pca$x                       # scores for first three PC's
  #scores <- pca$x[,1:3]
  # k-means clustering [assume 3 clusters]
  number_of_groups <- length(levels(as.factor(grouping[,2])))
  km  <- kmeans(scores, centers=number_of_groups, nstart=5)

  #ggdata <- data.frame(scores, Cluster=km$cluster, Groups=grouping)
  ggdata <- data.frame(scores, Cluster=km$cluster)


  p<- ggplot(ggdata) +
    geom_point(aes(x=PC1, y=PC2, colour=factor(Cluster)), size=3, shape=20) +
    geom_text(aes(x=PC1, y=PC2, color=factor(Cluster),label=colnames(data_matrix)))+
    stat_ellipse(aes(x=PC1,y=PC2,fill=factor(Cluster)), geom="polygon", level=0.95, alpha=0.2) +
    guides(color=guide_legend("Cluster"),fill=guide_legend("Cluster"))+
    theme_bw()+theme(plot.title = element_text(hjust = 0.5))

  return(p)

}






PCA_plot_3d_interactive_1<-function(data_matrix, grouping){
  suppressMessages(install.packages.auto(plotly))

  data_matrix_t<-t(data_matrix)
  loading <- prcomp(data_matrix_t)$x
  colnames(meta_test) <- c("Sample.Name", "Groups")

  loading_merge <- as.data.frame(merge(grouping, loading, by.y=0, by.x = "Sample.Name"))
  row.names(loading_merge)<-loading_merge$Sample.Name


  p1 <- plot_ly(loading_merge, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Groups, colors = c('#BF382A', '#0C4B8E', "#1ABC9C")) %>%
    add_markers() %>%
    #add_text(loading_merge, x = ~PC1, y = ~PC2, z = ~PC3,text = ~Sample.Name) %>%
    add_text(text = ~Sample.Name) %>%
    layout(scene = list(xaxis = list(title = 'PC1'),
                        yaxis = list(title = 'PC2'),
                        zaxis = list(title = 'PC3')))

  return(p1)
}

PCA_plot_3d_interactive_2<-function(data_matrix, grouping){
  suppressMessages(install.packages.auto(plotly))

  data_matrix_t<-t(data_matrix)
  loading <- as.data.frame(prcomp(data_matrix_t)$x)

  grouping <- grouping[match(rownames(loading), grouping[,1]),]


  p1 <- plot_ly(loading, x = ~PC1, y = ~PC2, z = ~PC3, color = grouping[,2], colors = c('#BF382A', '#0C4B8E', "#1ABC9C")) %>%
    add_markers() %>%
    #add_text(loading_merge, x = ~PC1, y = ~PC2, z = ~PC3,text = ~Sample.Name) %>%
    add_text(text = grouping[,1]) %>%
    layout(scene = list(xaxis = list(title = 'PC1'),
                        yaxis = list(title = 'PC2'),
                        zaxis = list(title = 'PC3')))

  return(p1)
}





# sub function for visualization



#________________________________________________________________________________________
#     simple_venn_plot_from_list
#________________________________________________________________________________________

# ___Description___:
# 1: a simple function to transform list of vectors to a data matrix,
# 2: the shiny version can be found: http://jolars.co/shiny/eulerr/
# another venn tool: http://www.biovenn.nl/index.php

# ___Arguments___:
# 1: the function accepts a list of vectors, with names, see the usage
#

#____Usage____;
# p <-simple_venn_plot_from_list(vector_list = list( "list_A"= A, "list_B" = B, "list_C" = C))

# ___Values___:
# a plot
#

# ___Note____;
# this object is grid based one, so no need to record

# test:

# A <- sample (LETTERS, 10)
# B <- sample (LETTERS, 15)
# C <- sample (LETTERS, 7)
# D <- sample (LETTERS, 20)

# simple_venn_plot_from_list(vector_list = list( "list_A"= A, "list_B" = B))
# simple_venn_plot_from_list(vector_list = list( "list_A"= A, "list_B" = B, "list_C" = C))
# simple_venn_plot_from_list(vector_list = list( "list_A"= A, "list_B" = B, "list_C" = C, "list_D" = D))



simple_venn_plot_from_list <-function(vector_list = list( "list_A"= A, "list_B" = B, "list_C" = C)){

  install.packages.auto(eulerr) #euler
  install.packages.auto("RColorBrewer") #   color brewer

  all <- unique(unlist(vector_list))
  match_list <- lapply(vector_list,function(x) all %in% x)
  presence_matrix <- matrix(unlist(match_list), ncol =length(match_list) , byrow = FALSE)
  colnames(presence_matrix) <- names(vector_list)

  fit <- euler(presence_matrix)
  mycolor_discret <- brewer.pal(4,"Set1") [1:length(vector_list)]
  p <- plot(fit, counts = TRUE, fill_opacity = 0.7, lty = 0,fontface = "italic", fill = mycolor_discret)
  return(p) # this object is grid based one, so no need to record

}



#________________________________________________________________________________________

#     correlation_matrix_plot
#________________________________________________________________________________________

# ___Description___:
# 1: do correlation analysis of columns,
# 2: do visualization of result correlation matrix, with p values
# 3: a simple wrap of two funcitons: Hmisc::rcorr and corrplot::corrplot

# ___Arguments___:
# data matrix:
# a data matrix to do the column correlatin,

#  method_type:  Character, the visualization method of correlation matrix to be used. Currently,
# it supports seven methods, named "circle" (default), "square", "ellipse", "number", "pie", "shade" and "color".
# See examples for details.
# The areas of circles or squares show the absolute value of corresponding correlation coefficients. Method "pie" and "shade" came from Michael Friendly's job (with some adjustment about the shade added on), and "ellipse" came from D.J. Murdoch and E.D. Chow's job, see in section References.

# order_type:
# corresponds the order or corrplot::corrplot

# "original" for original order (default).
# "AOE" for the angular order of the eigenvectors.
# "FPC" for the first principal component order.
# "hclust" for the hierarchical clustering order.
# "alphabet" for alphabetical order.


#____Usage____;
# t <- correlation_matrix_plot(mtcars, order_type = "hclust", plot_type ="color")
# t$corrplot
# t$corrmatrix

# ___Values___:
#  a corrplot and a very detailed matrix plot, see PerformanceAnalytics::chart.Correlation for more details
#  very cool plot


correlation_matrix_plot <- function(data_matrix, order_type = "hclust", plot_type ="circle" ){

  suppressMessages(install.packages.auto(Hmisc))
  suppressMessages(install.packages.auto(corrplot))
  suppressMessages(install.packages.auto(PerformanceAnalytics))

  correlation_matrix <- rcorr(as.matrix(data_matrix))
  win.metafile()
  dev.control('enable')
  corrplot(correlation_matrix$r, type="lower", order=order_type,  p.mat = correlation_matrix$P, sig.level = 0.01, insig = "pch", method =plot_type)
  p1 <- recordPlot()
  dev.off()

  win.metafile()
  dev.control('enable')
  suppressWarnings(chart.Correlation(data_matrix, histogram=TRUE, pch= "+"))
  p2 <- recordPlot()
  dev.off()

  return(list(corrplot = p1, corrmatrix = p2))
}



#________________________________________________________________________________________
#     matrix_ggboxplot
#________________________________________________________________________________________

# ___Description___:
# ggplot2 is powerful at boxplot and violinplot, but needs some pre-process of the datamatrix, a bit tricky sometimes
# here, the steps are wrapped up to give out an easy way

# ___Arguments___:
# data_matrix: data matrix
#  xlabel, ylabel, maintitle

#____Usage____;
# boxplot_ressult <- matrix_ggboxplot(data_matrix, xlabel="Sample", ylabel = "Value", maintitle = "Distribution")
# plot by: boxplot_ressult$boxplot, boxplot_ressult$violinplot

# ___Values___:
# a list of plot, the first object boxplot, second one is violinplot



matrix_ggboxplot<-function(data_matrix, xlabel="Samples", ylabel = "Value", maintitle = "Distribution"){
  suppressMessages(install.packages.auto(ggplot2))
  data_matrix_melt<-reshape2::melt(as.matrix(data_matrix))
  # in data_matrix_melt, Var1 is the orignal row.names, Var2 is the orignial column names, value is the orignial values

  p1<-ggplot(data_matrix_melt, aes(x = Var2, y = value, fill=Var2))+geom_boxplot()
  p1<-p1+labs(x = xlabel,y=ylabel,title = maintitle, fill = xlabel)
  p1<-p1+theme_bw()+theme(plot.title = element_text(hjust = 0.5))

  p2<-ggplot(data_matrix_melt, aes(x = Var2, y = value, fill=Var2)) +geom_jitter(shape=21,alpha=0.3) +geom_violin()
  p2<-p2+labs(x = xlabel,y=ylabel,title = maintitle)
  p2<-p2+theme_bw()+theme(plot.title = element_text(hjust = 0.5)) + guides(fill=guide_legend(title=xlabel))

  return(list(boxplot = p1, violinplot = p2))
}


#________________________________________________________________________________________
#     matrix_quick_heatmap
#________________________________________________________________________________________

# bug fixed
# 20191002 add a line to replace the infinite values, heatmap.2 can deal with NA, but not infinte values


matrix_quick_heatmap <- function(matrix,
                                 scale = "row",
                                 col_groupcolor_factor = NULL ,
                                 row_groupcolor_factor = NULL,
                                 Col_tree = TRUE){

  install.packages.auto(gplots)

  matrix[is.infinite(matrix)] <- NA # in case there is infinite values

  svg(tempfile(),onefile = TRUE)
  dev.control('enable')

  if(is.null(row_groupcolor_factor) & is.null(col_groupcolor_factor)){
    # if no grouping information provided
    if(Col_tree){
      heatmap.2(matrix, col=bluered, trace = "none",
                scale = scale,
                keysize = 1.5,
                density.info = "none",
                key.title  = "",
                key.xlab = "",
                key.ylab = "")
    }else{
      heatmap.2(matrix, col=bluered, trace = "none",
                scale = scale,
                keysize = 1.5,
                density.info = "none",
                Colv  = FALSE,
                dendrogram = "row",
                key.title  = "",
                key.xlab = "",
                key.ylab = "")
    }



  }else if((length(col_groupcolor_factor) > 0) & is.null(row_groupcolor_factor)){
    # if only colum grouping information provided

    col_groupcolor_factor <- col_groupcolor_factor[order(col_groupcolor_factor[,2]),]
    matrix<- matrix[,match(col_groupcolor_factor[,1],colnames(matrix))]

    color_labeling <- factor(col_groupcolor_factor[,2]) # removing unused factors
    levels(color_labeling) <- rainbow(length(levels(color_labeling))) # rename the
    color_labeling <- as.vector(color_labeling)
    if(Col_tree){

      heatmap.2(matrix,
                col=bluered,
                ColSideColors = color_labeling,
                trace = "none",
                scale = scale,
                keysize = 1.5,
                density.info = "none",
                key.title  = "",
                key.xlab = "",
                key.ylab = "")


    }else{
      heatmap.2(matrix,
                col=bluered,
                ColSideColors = color_labeling,
                trace = "none",
                scale = scale,
                Colv  = FALSE,
                dendrogram = "row",
                keysize = 1.5,
                density.info = "none",
                key.title  = "",
                key.xlab = "",
                key.ylab = ""
      )
    }

  }else if((length(row_groupcolor_factor) > 0) & is.null(col_groupcolor_factor)){
    # if only row grouping information provided

    row_groupcolor_factor <- row_groupcolor_factor[match(rownames(matrix), row_groupcolor_factor[,1]),]

    row_groupcolor_factor <- row_groupcolor_factor[order(row_groupcolor_factor[,2]),]
    matrix<- matrix[match(row_groupcolor_factor[,1],rownames(matrix)),]

    color_labeling <- factor(row_groupcolor_factor) # removing unused factors
    levels(color_labeling) <- rainbow(length(levels(color_labeling))) # rename the
    color_labeling <- as.vector(color_labeling)
    if(Col_tree){
      heatmap.2(matrix,
                col=bluered,
                ColSideColors = color_labeling,
                trace = "none",
                scale = scale,
                keysize = 1.5,
                density.info = "none",
                key.title  = "",
                key.xlab = "",
                key.ylab = "" )

    }else{
      heatmap.2(matrix,
                col=bluered,
                ColSideColors = color_labeling,
                trace = "none",
                scale = scale,
                Colv  = FALSE,
                dendrogram = "row",
                keysize = 1.5,
                density.info = "none",
                key.title  = "",
                key.xlab = "",
                key.ylab = ""
      )
    }
  }
  p1 <- recordPlot()
  dev.off()
  return(p1)
}




# visualize the datamatrix institutively
matrix_display <- function(matrix,method="square",title.tag = "Display", ...){

  suppressMessages(install.packages.auto(corrplot))

  postscript("temp")
  dev.control('enable')

  # corrplot is powerful to visualize
  corrplot(matrix, type = "full",is.corr = FALSE, order = "original",
           tl.col = "black",tl.cex = 0.5,  tl.srt = 45,title = title.tag, method=method,
           addgrid.col =NA, outline = FALSE)

  p1 <- recordPlot()
  dev.off()

  return(plot = p1)
}


# data_matrix process and plot --------------------------------------------------------



#________________________________________________________________________________________
#     subfunction
#     matrix_ANOVA
#________________________________________________________________________________________
#     do ANOVA test on matrix, according to given grouping information
matrix_ANOVA<-function(data,groups) {
  p_value<-apply(data,1,function(x) anova(lm(as.numeric(x)~as.factor(groups)))$Pr[1])
  return(p_value)
}

#________________________________________________________________________________________
#     subfunction
#     matrix_ttest
#________________________________________________________________________________________

# do preset t test among two groups
# for this function, you need to input the data, grouping informatin, and two group names for comparison
matrix_ttest<-function(data,group_info,group_name1,group_name2) {
  group1<-which(group_info==group_name1)
  group2<-which(group_info==group_name2)
  p_value<-apply(data,1,function(x) t.test(x[group1],x[group2])$p.value)
  return(p_value)
}


#similarly
matrix_fold_change<-function(data,group_info,group_name1,group_name2) {
  group1<-which(group_info==group_name1)
  group2<-which(group_info==group_name2)

  fold_change<-apply(data,1,function(x) sum(x[group1])/sum(x[group2]))

  return(fold_change)
}






#________________________________________________________________________________________
#     wrapper
#     PCA_wrapper
#________________________________________________________________________________________

# this function is just a wrapper of the PCA analysis
# input is a general data matrix(row as features, column as samples)

# will do sequential KNN using rrcovNA::impSeqRob

# data_meta is a data.frame with two columns of sample and grouping
# ncomp: number of component to keep, it cannot be larger than the smaller dimention
# Q is the strengh of imputation

PCA_wrapper_mixOmics <- function(data_matrix = NULL, data_meta = NULL, ncomp = 10, inputation = TRUE, Q = 0.75, ...){
  suppressMessages(install.packages.auto(mixOmics))


  # reorder the meta (grouping information)
  data_meta <- data_meta[match(colnames(data_matrix), as.data.frame(data_meta)[,1]),]

  # check the order
  if(any(colnames(data_matrix) !=  data_meta[,1])){stop("unmatched sample name/colun names")}


  # take the values from the input
  X <- as.matrix(t(data_matrix))
  Y <- as.data.frame(data_meta)[,2]

  # filter and imputat the mising value
  if(inputation){
    X <- tidy_IntensityMarix_process(X, Q = Q, Imputation = TRUE)
  }


  if(ncomp > ncol(data_matrix)){
    ncomp <-  ncol(data_matrix)
  }

  # this part could be done by any method

  my.pca <- mixOmics::pca(X, ncomp = ncomp, center = TRUE, scale = TRUE)

  PCA_analysis <- PCA_post_analysis(my.pca, Y)

  return(list(
    PCA_result = my.pca,
    plot_PCA_Scree =  PCA_analysis$plot_Scree,
    plot_PCA_component =  PCA_analysis$plot_component

  ))

}





#________________________________________________________________________________________
#     wrapper
#     PCA_PLSDA_wrapper
#________________________________________________________________________________________

# this function is just a wrapper of the PCA and PLSDA analysis
# input is a general data matrix(row as features, column as samples)

# will do sequential KNN using rrcovNA::impSeqRob

PCA_PLSDA_wrapper <- function(data_matrix = NULL, data_meta = NULL, ncomp = 10, inputation = TRUE, Q = 0.75, VIP_threshold = 1){
  suppressMessages(install.packages.auto(mixOmics))

  rownames(data_matrix) <- 1:nrow(data_matrix)
  # reorder the meta (grouping information)
  data_meta <- data_meta[match(colnames(data_matrix), as.data.frame(data_meta)[,1]),] # revised @201906013

  # check the order
  if(any(colnames(data_matrix) !=  data_meta[,1])){stop("unmatched sample name/colun names")}


  # take the values from the input
  X <- as.matrix(t(data_matrix))
  Y <- as.data.frame(data_meta)[,2]# revised @201906013

  # filter and imputat the mising value
  if(inputation){
    X <- tidy_IntensityMarix_process(X, Q = Q, Imputation = TRUE)
  }


  if(ncomp > ncol(data_matrix)){
    ncomp <-  ncol(data_matrix)
  }



  # this part could be done by any method
  my.pca <- mixOmics::pca(X, ncomp = ncomp, center = TRUE, scale = TRUE)
  PCA_analysis<-PCA_post_analysis(my.pca, Y)

  # this is the plsda part, start a model, then test, how many component is good
  my.plsda <- mixOmics::plsda(X, Y, ncomp = 3)
  PLSDA_analysis <- PLSDA_post_analysis(X, my.plsda, VIP_threshold = VIP_threshold)


  return( list(
    plot_PCA_Scree =  PCA_analysis$plot_Scree,
    plot_PCA_component =  PCA_analysis$plot_component,
    plot_PLSDA_component =  PLSDA_analysis$plot_PLSDA_component,
    plot_PLSDA_heatmap = PLSDA_analysis$plot_PLSDA_heatmap,
    plot_PLSDA_AUC = PLSDA_analysis$plot_auc,
    plot_PLSDA_VIP = PLSDA_analysis$plot_vip_distribution,
    plot_PLSDA_VIP_heatmap = PLSDA_analysis$plot_vip_filtered_variables,
    data_PLSDA_VIP_filtered_features = t(PLSDA_analysis$vip.filtered.variables),
    data_PLSDA_VIP =  PLSDA_analysis$vip.all
  ))

}






PCA_post_analysis <- function(result.pca, Y){

  if(missing (result.pca)){
    print("No Input!")
  }else{

    install.packages.auto(mixOmics)
    my.pca <-result.pca


    postscript("temp")
    dev.control('enable')
    plot(my.pca)
    p1 <- recordPlot()
    dev.off()

    postscript("temp")
    dev.control('enable')
    plotIndiv(my.pca, comp = c(1, 2), ind.names = TRUE,  group = Y, ellipse = TRUE,legend = TRUE, title = 'PCA plot, PCA comp 1 - 2')
    p2 <- recordPlot()
    dev.off()

    return(list(plot_Scree = p1,
                plot_component = p2
    ))
  }
}




#________________________________________________________________________________________
#     PLSDA_CrossValication
#________________________________________________________________________________________

# ___Description___:
# 1: do cross-validation of a setup plsda model
# 2: the purpose includes 2, one is to evaluate the stability, the other is to choose what is the best number of component to choose, for a final model

# ___Arguments___:
# result_plsda: mixOmics plsda resulted object
# validation method, see ?perf for more options, could be LOO if sample size is not big, say less than 50
# folds, see ?perf for more details, usually setup as 10
# # NOTE that nrepeat is usually 50~100 times

#____Usage____;

# ___Values___:
#
#


PLSDA_CrossValication <-function(result_plsda, validation = "Mfold", folds =5, nrepeat =5 ){
  suppressMessages(install.packages.auto(mixOmics))
  print ("Performing crossing validation, will be very slow depending on the settings")
  perf.my.plsda.start <- mixOmics::perf(my.plsda.start, validation = 'Mfold', folds = fold, progressBar = TRUE, auc = TRUE, nrepeat = 2)
  # here add as many plot and return in the list
  #plotIndiv(my.plsda.start, comp = c(1, 2), ind.names = TRUE,  group = Y, ellipse = TRUE,legend = TRUE, title = 'PLSDA plot, Comp 1 - 2')
  plot.distance<-plot(perf.my.plsda.start, overlay = 'measure', sd = TRUE)
  return(list(plot.distance = plot.distance

  ))
}

#________________________________________________________________________________________
#  PLSDA_post_analysis
#________________________________________________________________________________________

# ___Description___:
# 1: orignial data matrix, for extracing variables
# 2: The main target is to extract the VIP and the variables with VIP higher than VIP_threshold set
# 3: there are also some other figures output

# ___Arguments___:
# 1: result.plsda, mixOmics plsda resulted object
# 2: VIP_threshold

#____Usage____;

# ___Values___:
# a list of figures and datamatrix
# use summary to view


# Postprocess of PSLDA
PLSDA_post_analysis<-function(X, my.plsda, VIP_threshold = 1){
  suppressMessages(install.packages.auto(mixOmics))
  # ploting and output
  Y<-my.plsda$Y

  postscript("temp")
  dev.control('enable')
  plotIndiv(my.plsda, comp = c(1, 2), ind.names = TRUE,  group = Y, ellipse = TRUE,legend = TRUE, title = 'PLSDA plot, Comp 1 - 2')
  p1 <- recordPlot()
  dev.off()


  my.side.color <- color.mixo(as.numeric(Y))

  postscript("temp")
  dev.control('enable')
  cim(my.plsda, row.sideColors = my.side.color, row.names = Y)
  p2 <- recordPlot()
  dev.off()


  #plot an auc
  postscript("temp")
  dev.control('enable')
  my.plsda.auroc = mixOmics::auroc(my.plsda, roc.comp = 1)
  p3 <- recordPlot()
  dev.off()

  # extract all the VIPs
  my.vip<-vip(my.plsda)
  #write.table(my.vip,"Out_VIP_table.txt",sep="\t",row.names = TRUE,col.names = NA)

  # oveall distribution of all the vips
  my.vip.plot <- matrix_ggboxplot(my.vip, xlabel="Component", ylabel = "VIP Score", maintitle = "VIP Score Across Component")
  p4 <- my.vip.plot$violinplot


  # filter vips, keeping >1, and ouput all the corresponding features/variables
  my.vip.filtered <- my.vip[my.vip[,1]> VIP_threshold,]
  my.vip.filtered <- my.vip.filtered[order(my.vip.filtered[,1], decreasing = TRUE),]
  my.vip.filtered.variables <- X[,as.numeric(row.names(my.vip.filtered))]
  #write.table(my.vip.filtered.variables,"Out_ProteinGroups_filtered_VIP.txt",sep="\t",row.names = TRUE,col.names = NA)

  # plot the heatmap of the orginal features
  my.vip.filtered.variables.scaled<-scale(my.vip.filtered.variables)
  postscript("temp")
  dev.control('enable')
  cim(my.vip.filtered.variables.scaled,
      row.sideColors = my.side.color,
      row.names = Y,
      col.names = FALSE,
      row.cex = 0.5,
      scale = TRUE,
      center =TRUE)
  p5 <- recordPlot()
  dev.off()


  return(list( plot_PLSDA_component = p1,
               plot_PLSDA_heatmap = p2,
               plot_auc = p3,
               plot_vip_distribution =p4,
               plot_vip_filtered_variables = p5,
               vip.all = my.vip,
               vip.filtered.variables = my.vip.filtered.variables

  ))
}






#________________________________________________________________________________________
#     flattenCorrMatrix
#________________________________________________________________________________________

# ___Description___:
# 1: flatten a matrix from a correlation analysis, into a data.frame with 5 columns

# ___Arguments___:
# result_rcorr has to be the result object of Hmisc::rcorr function

#____Usage____;
# flattenCorrMatrix(result_rcorr)

# Column 1 : A
# Column 2 : B
# Column 3 : correation
# Column 4 : the p-values of the correlations
# Column 5 : N of values used after removing

# example
# x <- c(-2, -1, 0, 1, 2)
# y <- c(4,   1, 0, 1, 4)
# z <- c(1,   2, 3, 4, NA)
# v <- c(1,   2, 3, 4, 5)
# result <- rcorr(cbind(x,y,z,v))
# t <-flattenCorrMatrix(result)

flattenCorrMatrix <- function(result_rcorr) {
  cormat <- result_rcorr$r
  nmat <-result_rcorr$n
  pmat <- result_rcorr$P

  ut <- upper.tri(cormat)
  data.frame(
    from = rownames(cormat)[row(cormat)[ut]],
    to = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut],
    N_used = nmat[ut]
  )
}




matrix_filtering<-function(data.matrix, threshold = 0.01, keep_sign = "<"){

  data.matrix <- data.matrix
  t <- data.matrix
  threshold <- threshold
  keep_sign = "<"

  if(keep_sign == "<"){
    t[data.matrix >= threshold]<-NA
  }else if(keep_sign == "<="){
    t[data.matrix > threshold]<-NA
  }else if(keep_sign == ">"){
    t[data.matrix <= threshold]<-NA
  }else if(keep_sign == ">="){
    t[data.matrix < threshold]<-NA
  }

  data.matrix_filtered<-t[!apply(is.na(t),1,all),]

}

# data_preprocess ---------------------------------------------------------




#________________________________________________________________________________________
#     readin_tidied_proteingroups
#________________________________________________________________________________________

# ___Description___:
# 1: read in tidied proteingroups
# 2: read in tidied experimental desgin


# ___Arguments___:
# file_progeinGroups: full path for tidied progeinGroups ouput
# file_experimentDesgin: full path for tidied experiment ouput



#____Usage____;

# ___Values___:
# a list of three values
# dat_matrix: a tidy data matrix
# groups: a tidy and matched sample grouping file
# a quaility checking data frame to see if the sample order in the table is the same between the datamatrix and the grouping file


readin_tidied_proteingroups <- function(file_progeinGroups="",
                                        file_experimentDesgin= ""){

  # read in protgroups
  if(nchar(file_progeinGroups) > 0) {
    proteinGroups<-read.delim(file_progeinGroups,header=TRUE, row.names = 1,  check.names = FALSE)
    # make sure that check.names = FALSE, otherwise the there will be an X added to the colnames
  }else{
    print("file does not exist, check your input!")
  }


  if(nchar(file_experimentDesgin) > 0){

    # read in experimental desgin
    grouping<-read.delim(file_experimentDesgin,header=TRUE, row.names = 1)
    # filtering proteinGroups, only keeping expression data
    alignment_check<-cbind(as.character(grouping[,1]),rownames(proteinGroups))

    return(list(data_matrix = proteinGroups,
                groups = grouping,
                alignment_check = alignment_check
    ))

  }else{
    return(list(data_matrix = proteinGroups ))

  }
}



#________________________________________________________________________________________
#     readin_raw_proteingroups_filterrows
#________________________________________________________________________________________

# ___Description___:
# 1: read in raw proteingroups
# 2: only filter out rows with contaminant, revresed, Only.identified.by.site


# ___Arguments___:
# file_progeinGroups: full path for tidied progeinGroups ouput
# rows_marked_with: usually "Only.identified.by.site", "Reverse","Potential.contaminant".
# note that it might be a bit different between different versions of maxquant



#____Usage____;

# ___Values___:
# a list of 1 value
# dat_matrix: a row filtered value data matrix

readin_raw_proteingroups_filterrows <- function(file_progeinGroups="proteinGroups.txt",
                                                rows_marked_with = c("Only.identified.by.site", "Reverse","Potential.contaminant"))
{

  # read in protgroups
  print("Reading in ProteinGroups ....")
  proteinGroups<-read.delim(file_progeinGroups,header=TRUE, blank.lines.skip = TRUE)
  print("ProteinGroups read in")
  # filtering proteinGroups, only keeping expression data

  proteinGroups_filtered<-PG_filterout_rows(proteinGroups,rows_marked_with = rows_marked_with)

  print("ProteinGroups filtered")

  return(list(data_matrix = proteinGroups_filtered))
}


















#________________________________________________________________________________________
#     IntensityMarix_process
#________________________________________________________________________________________

# ___Description___:
# 1: very basic data process: log10 transformation, normalization on column or row, missing value imputation, etc
# 2: This function uses homemade subfunction "missingvalue_filtering", be sure to load it first

# ___Argument___:
# IntensityMatrix: is the data input, which is intensity matrix selected columns from the proteinGroup file, it could be the LFQ inetnsity, intensity, or intensity for specific labeling state
# Normalize_columns: if set TRUE, normlize column (experiment) means to the same level (to zero) first. Some method, like PCA, have scale function built in, therefore no normalization need before
# Normalize_rows: if set TRUE, normalize row (proteingroyps) means to the same level (tosezro) then. Some method, like PCA, have scale function built in, therefore no normalization need before
#
# threshold: refer to the function of  "missingvalue_filtering"
#             briefly, 1 as Q100, 0.5 as Q50,
# Imputation: if set TRUE, do missing value imputation. Only works when there are missing values todo:choose differnt method,

#___Usage___:
#IntensityMarix_process(IntensityMarix, threshold = 1, Imputation = TRUE, Normalize_columns = TRUE, Normalize_rows = TRUE)

# ___Values___:
#
#




IntensityMarix_process<-function(IntensityMarix, threshold = 1, Imputation = TRUE, Normalize_columns = TRUE, Normalize_rows = TRUE){

  IntensityMarix[IntensityMarix==0]<-NaN # replace the 0 with NaN
  IntensityMarix_log10<-log10(IntensityMarix) # take log10

  # missing value filtering
  tempt_filter_result <- missingvalue_filtering(IntensityMarix_log10, threshold)
  IntensityMarix_log10_NAfiltered <- tempt_filter_result$data_qualified # home made function [missingvalue_filtering]
  IntensityMarix_log10_NAfiltered_filtering_summary <-tempt_filter_result$filtering_summary

  p1<-matrix_ggboxplot(IntensityMarix_log10_NAfiltered, maintitle = "Distribution of NA-filtered")$violinplot
  p2<-matrix_ggboxplot(IntensityMarix_log10_NAfiltered, maintitle = "Distribution of NA-filtered")$boxplot

  if(Imputation == "TRUE"){  # missing value imputation
    IntensityMarix<-rrcovNA::impSeqRob(IntensityMarix_log10_NAfiltered)$x
  }

  if (Normalize_columns == "TRUE"){ # do column scaling, keep in mind that the scale function in R is scaling by column
    IntensityMarix<-scale(IntensityMarix)
  }

  if (Normalize_rows == "TRUE"){ # scaling of each protein
    IntensityMarix<-t(scale(t(IntensityMarix)))
  }

  p3<-matrix_ggboxplot(IntensityMarix, maintitle = "Distribution of NA-filtered & Processed")$violinplot
  p4<-matrix_ggboxplot(IntensityMarix, maintitle = "Distribution of NA-filtered & Processed")$boxplot


  write.table(IntensityMarix,paste("Out_ProteinGroups_NAfiltered_Scaled",".txt",sep=""),sep="\t",row.names = TRUE,col.names = NA)
  return(list(IntensityMarix_processed = IntensityMarix,
              IntensityMarix_filtering_summary = IntensityMarix_log10_NAfiltered_filtering_summary,
              violinplot.before = p1,
              boxplot.before = p2,
              violinplot.after = p3,
              boxplot.after = p4

  ))
}


# 20181002 fix a bug for filtering out rows, originally it was applied on columns

tidy_IntensityMarix_process<-function(IntensityMarix, Q = 0.75, Imputation = TRUE){
  suppressMessages(install.packages.auto(rrcovNA))

  # IntensityMarix<-IntensityMarix[,rowSums(IntensityMarix == 0) < (ncol(IntensityMarix)*(1-Q))]

  # filter out rows with too many missing values
  IntensityMarix  <- IntensityMarix[rowSums(is.na(IntensityMarix)) <= ncol(IntensityMarix)*(1-Q) , ]

  IntensityMarix<-log10(IntensityMarix)
  IntensityMarix[IntensityMarix==-Inf]<-NA
  if(Imputation=="TRUE"){
    IntensityMarix<-t(rrcovNA::impSeqRob(t(IntensityMarix))$x)
  }
  IntensityMarix # now is a inputed(log10(intensity)), items with too many missing values ware filtered out
  # intensity matrix is a data.frame, with raw intensity (without log transformation) with column as samples, and row as items
  # Q is the Q cutoff for percentatage of missing value filtering, 0.75 means items with 75% of msising value will be filtered out
}


#________________________________________________________________________________________

#     missingvalue_filtering
#________________________________________________________________________________________

# ___Description___:
# filter a matrix out rows/clumn with more than NA/infinte preset(inf values could be generated from log transformation or dividing conversion),
# threshold is the number of the valid values, rows with more valid values than threshold will be kept as qualified
# only do row-wise filtering, transpose first if do column-wise filtering

#__Usage__:
# missingvalue_filtering(data, threshold=3)

# ___Arguments___:
# data: data matrix with missing values, NA/inf
# threshold: how many (percentage) non-missingvalues are required to be in the matrix
#             can be two types, one is the nnumber of the missing values,the other one is the so called Q value, which is the percentage(0 <= Q <= 1) to the number of columns
#             Q value == 1, require no missing values,
#             Q value == 0, no filtering,
#             Q value will be converted to the number of missing value (1 < number < ncol(data))
#             will report an error if not setup in this range
#             in this function, celing is used to convert the percentage, if not expected, try floor/round etc

# ___Values___:
# qualified data matrix, not.qulified data.matrix, number of rows of quailfied/ not qulified.
#



missingvalue_filtering<-function(data.matrix, threshold = 1){
  data.matrix[is.infinite(as.matrix(data.matrix))]<-NA # the is.infinite function does not work on data.frame,
  # in case there are infinte values there

  if(threshold < 0|threshold > ncol(data.matrix) ){
    print ("Threshold cannont be smaller than 0 or bigger than the number of columns, please check you threshold setting and rerun!!!")
  }else{

    if(threshold<=1){ # concet the q value to the real missing value number
      threshold <- ceiling(ncol(data.matrix)*threshold)
    }

    data_qualified<-data.matrix[which(apply(data.matrix,1,function(x)(sum(is.na(x)))<=(ncol(data.matrix)-threshold))),]
    data_not.qualified<-data.matrix[which(apply(data.matrix,1,function(x)(sum(is.na(x)))>(ncol(data.matrix)-threshold))),]
    return(list(data_qualified=data_qualified,
                filtering_summary = list(data_not.qualified=data_not.qualified,
                                         number.qualified=nrow(data_qualified),
                                         number.not.qualified=nrow(data_not.qualified))
    ))

  }
}



#________________________________________________________________________________________
#     extract_proteinlist_ineachsample
#________________________________________________________________________________________

# ___Description___:
# 1: extract protein list from given clean proteingroups matrix





extract_proteinlist_ineachsample <- function(proteinGroups_tidied_input){
  proteinGroups_tidied = proteinGroups_tidied_input
  protein_names <- as.vector(rownames(proteinGroups_tidied)) # this protein names is the primary proteinIDs in the protein group files
  proteins_listoflist_eachsample <- lapply(as.list(proteinGroups_tidied),function(x){protein_names[which((x != 0))]})


  return(list(protein_names_list = proteins_listoflist_eachsample)
  )
}

# more general form for prcessing

extract_idlist_each_column <- function(data_matrix){
  ids <- as.vector(rownames(data_matrix)) # this protein names is the primary proteinIDs in the protein group files
  ids_list_each_colum <- lapply(as.list(data_matrix),function(x){ids[which((x != 0))]})
  return(ids_list_each_colum)
}

# this works
readin_tsv<- function(file="proteinGroups.txt"){

  # read in protgroups
  print(paste("Reading in", file, "...."))
  tsv<-read.delim(file,header=TRUE, blank.lines.skip = TRUE)
  print(paste(file, "read in"))
  number_of_row = nrow(tsv)
  number_of_columns = ncol(tsv)
  head = head(tsv)
  # filtering proteinGroups, only keeping expression data
  return(list(data_matrix = tsv,
              number_of_row = number_of_row,
              number_of_columns= number_of_columns,
              head_of_table =  number_of_columns
  ))
}

# this works
readin_experiment_desgin <- function(file_experimentDesgin = "experimentalDesign.txt"){

  # read in experimental desgin

  if(nchar(file_experimentDesgin) > 0){

    grouping<-read.delim(file_experimentDesgin,header=TRUE, na.strings = "NA")

    # clean the experiemntal desgin, in case there are some rawfiles not used in the grouping
    grouping <- replace(grouping, grouping == "", NA)
    temp<-apply(grouping,1,function(x) length(which(is.na(x))))
    grouping<-grouping[which(temp==0),]
    print(paste(file_experimentDesgin, " read in and tidied up!"))
    return(grouping)

  }else{
    print("wrong path")
  }

}
# this works
readin_meta_data <- function(meta_table = "experimentalDesign.txt"){

  # read in experimental desgin

  if(nchar(meta_table) > 0){

    grouping<-read.delim(meta_table,header=TRUE, na.strings = "NA")

    # clean the experiemntal desgin, in case there are some rawfiles not used in the grouping
    grouping <- replace(grouping, grouping == "", NA)
    temp<-apply(grouping,1,function(x) length(which(is.na(x))))
    grouping<-grouping[which(temp==0),]
    print(paste(meta_table, " read in and tydied up!"))
    return(grouping)

  }else{
    print("wrong path")
  }

}



#________________________________________________________________________________________
#     PG_filterout_rows
#________________________________________________________________________________________

# ___Application___:
# 1: parse the dataframe of proteingroups, filtering out the contaminant, reversed, and ided only by site


# ___input___:
# proteingroups is the dataframe read-in directly from proteingroups.txt (as default from Maxquant)
# do not open and edit it in excel
# rows_marked_with: rows marked in these columns as "+" will be deleted


# ___output___:
# a filtered data matrix


PG_filterout_rows<-function(proteinGroups, remove_rows_marked_with = c("Only.identified.by.site", "Reverse","Potential.contaminant")){

  suppressMessages(install.packages.auto(dplyr))
  suppressMessages(install.packages.auto(lazyeval)) # this is very important for passing column names as parameters to the function

  proteinGroups_filtered <- proteinGroups
  proteinGroups_filtered$temp.rownames <- rownames(proteinGroups_filtered)

  for(filter_name in remove_rows_marked_with)  {
    print(paste("filtering by",filter_name))
    filter_criteria <- lazyeval::interp(quote(x != "+"), x = as.name(filter_name))
    proteinGroups_filtered<-filter(proteinGroups_filtered, filter_criteria)

  }

  # notice that the rownames are silently dropped druing filter, even there are row names,
  # set the column names by using a temp column
  rownames(proteinGroups_filtered) <- proteinGroups_filtered$temp.rowname
  proteinGroups_filtered <- proteinGroups_filtered[,-ncol(proteinGroups_filtered)]

  return(proteinGroups_filtered)
}




#________________________________________________________________________________________
#     PG_filtering
#________________________________________________________________________________________

# ___Application___:
# 1: parse the dataframe of proteingroups, filtering out the contaminant, reversed, and ided only by site
# 2: only keep the user defined expression columns, and return data matrix

#____input____
# proteingroups is the dataframe read-in directly from proteingroups.txt (as default from Maxquant)
# do not open and edit it in excel
# rows_marked_with: rows marked in these columns as "+" will be deleted
# cols_starts_with: column names start with, used the maxiumn length of the common characters

# ___output___:
# a data matrix

PG_filtering<-function(proteinGroups,rows_marked_with = c("Only.identified.by.site", "Reverse","Potential.contaminant"), cols_starts_with = "LFQ.intensity."){

  # removing rows marked with
  proteinGroups_filtered<-PG_filterout_rows(proteinGroups, rows_marked_with)
  # keeping columns starts with
  proteinGroups_filtered <- select(proteinGroups_filtered, starts_with(cols_starts_with))
  #write.table(proteinGroups_filtered,"Out_ProteinGroups_filtered.txt",sep="\t",row.names = TRUE,col.names = NA)
  return(proteinGroups_filtered)
}




#________________________________________________________________________________________
#     tidy_protein_groups
#________________________________________________________________________________________

# ___Description___:
# 1: read in proteingroups
# 2: read in experimental desgin
# 3: filtering proteingroups by rows
# 4: filtering proteingroups by columns
# 5: removing rows with all 0
# 5: filtering experimental desgin, only keep samples with grouping information
# 6: filtering proteingroups, only keeping samples with grouping information
# 7: tranpose the filtered proteingroups: rows as samples/observations, columns as variables
# 8: reordering the grouping information, to math the ordering of the samples in proteingroups
# 9: double check the of the sample names are matching
# 10: return both the data matrix and experimental design/grouping

# ___Arguments___:
# file_progeinGroups: full path for progeinGroups
# row.names: The column name to be the rownames when readin the table, this column cannot be redundant.
# proteingroups is the dataframe read-in directly from proteingroups.txt (as default from Maxquant)
# do not open and edit it in excel
# rows_marked_with: rows marked in these columns as "+" will be deleted
# cols_starts_with: column names start with, used the maxiumn length of the common characters


#____Usage____;

# ___Values___:
# a list of three values
# dat_matrix: a tidy data matrix
# groups: a tidy and matched sample grouping file
# a quaility checking data frame to see if the sample order in the table is the same between the datamatrix and the grouping file


tidy_proteingroups <- function(proteinGroups = NULL,
                               experimentDesgin = NULL,
                               remove_rows_marked_with = c("Only.identified.by.site", "Reverse","Potential.contaminant"),
                               keep_cols_starts_with = "LFQ.intensity.", # the columns to be selected starting with, use the maximum lengh
                               filter_all_zero = TRUE){
  suppressMessages(install.packages.auto(dplyr)) # select command


  # rownames to be the primary protein ID
  protein.ids_split <- strsplit(as.vector(proteinGroups$Protein.IDs), ";") # this is a list of list of split names
  protein_primary_ids <- unlist(lapply(protein.ids_split, function(x) x[1])) # only keep the first one
  rownames(proteinGroups) <- protein_primary_ids # rename the rownames of the matrix


  # removing rows marked with
  if (length(remove_rows_marked_with) >=1 ){
    proteinGroups_filtered<-PG_filterout_rows(proteinGroups, remove_rows_marked_with)
  }

  # keeping columns starts with
  if (length(keep_cols_starts_with) >=1 ){
    proteinGroups_filtered <- select(proteinGroups_filtered, starts_with(keep_cols_starts_with)) # select command in dplyr package
  }

  # shorten column names, by removing the "starts with"
  colnames(proteinGroups_filtered)<-gsub(keep_cols_starts_with, "", colnames(proteinGroups_filtered))

  #remove rows with all 0, this is only for data value matrix
  if (filter_all_zero){
    proteinGroups_filtered<-proteinGroups_filtered[-which(apply(proteinGroups_filtered,1,function(x)all(x == 0))),]
    print("ProteinGroups filtered")
  }



  # further select columns in Experiment desgin, if not all used in Experiment desgin
  if(class(experimentDesgin) ==  "data.frame"){
    proteinGroups_filtered<- proteinGroups_filtered[,which(colnames(proteinGroups_filtered) %in% experimentDesgin[,1])]
    experimentDesgin<-experimentDesgin[match(colnames(proteinGroups_filtered),as.character(experimentDesgin[,1])),] # just in case the order is not the same
    # check if the experiment is aligned properly
    alignment_check<-cbind(as.character(experimentDesgin[,1]),colnames(proteinGroups_filtered))

    #write.table(proteinGroups_filtered,"Out_ProteinGroups_tidied_up.txt",sep="\t",row.names = TRUE,col.names = NA)
    #write.table(experimentDesgin,"Out_ProteinGroups_expdesign.txt",sep="\t",row.names = TRUE,col.names = NA)

    return(list(data_matrix = proteinGroups_filtered,
                groups = experimentDesgin,
                alignment_check = alignment_check
    ))

  }else{
    #write.table(proteinGroups_filtered,"Out_ProteinGroups_tidied_up.txt",sep="\t",row.names = TRUE,col.names = NA)
    return(list(data_matrix = proteinGroups_filtered,
                experimentDesgin = experimentDesgin))
  }

}




#________________________________________________________________________________________

#     filter_PSM
#________________________________________________________________________________________

# ___Description___:
# 1: this function is for MSfragger result parsing and filter, not working for others
# 2:

# ___Arguments___:
# file input could be peptXML, or tsv
# others could be just as default

#____Usage____;
#filter_PSM(file = "MS_QC_60min.pepXML")
#t<- filter_PSM(file = "MS_QC_60min.tsv")


# ___Values___:
# will write a table out,
# return a data.frame of filtered table



filter_PSM <- function(file = "file", pepFDR=0.01, score_for_filtering = "hyperscore", decoyprefix="REVERSED_" ){

  suppressMessages(install.packages.auto(pepXMLTab))
  suppressMessages(install.packages.auto(tools))

  filetype = file_ext(file) #library(tools)

  if (filetype == "pepXML"){ # if this is a a peppxml file, parse it first
    tsv<-pepXML2tab(file)
  } else if(filetype == "tsv"){# if tsv, read in
    tsv <- read.delim(file, header =FALSE)
    header= c(
      "scanID", "precursor_neutral_mass","retention_time_sec","assumed_charge", "hit_rank","peptide",
      "peptide_prev_aa","peptide_next_aa","protein","num_matched_ions","tot_num_ions of matched theoretical fragment ions",
      "calc_neutral_pep_mass","massdiff","num_tol_term","num_missed_cleavages","modifications",
      "hyperscore","next_score","intercept_of_ep_model","slope_of_pe_mode")
    colnames(tsv) <- header
  }else{
    print ("Wrong input file type!")
  }

  passed <- PSMfilter(tsv, pepFDR=0.01, scorecolumn='hyperscore', hitrank=1, minpeplen=6, decoyprefix='REVERSED_')

  filename_base <-file_path_sans_ext(file) #library(tools)
  write.table(passed,paste(filename_base,"_FDR_filtered.txt", sep=""),sep="\t",row.names = TRUE,col.names = NA ,quote = FALSE)
  return(list(filtered = passed))
}


#________________________________________________________________________________________
#     PSMfilter
#________________________________________________________________________________________

# ___Description___:
# 1: This function function overide the same one from pepXMLTab, because the orginal one has a bug
# 2: default setting for readin tables is to convert characters into factor, therefore you cannot use nchar to count ther length anymore
# use as.character to change the target column back


PSMfilter <- function (PSMtab, pepFDR = 0.01, scorecolumn = "mvh", hitrank = 1,
                       minpeplen = 6, decoyprefix = "rev_", ...)
{
  if (dim(PSMtab)[1] == 0) {
    message("No input data")
  }
  else {
    PSMtab <- PSMtab[PSMtab$hit_rank <= hitrank, ]
    PSMtab[, "peptide"] <-as.character(PSMtab[, "peptide"]) # this line is added to fix the bug,
    peptide <- PSMtab[, "peptide"]
    idx <- which(nchar(peptide) < minpeplen)
    if (length(idx) > 0)
      PSMtab <- PSMtab[-idx, ]
    NTT <- rep(0, dim(PSMtab)[1])
    cut1 <- grep("[KR-]", PSMtab[, "peptide_prev_aa"], fixed = FALSE)
    NTT[cut1] <- 1
    cut2 <- union(grep("[KR]", substring(PSMtab[, "peptide"],
                                         nchar(PSMtab[, "peptide"])), fixed = FALSE), grep("-",
                                                                                           PSMtab[, "peptide_next_aa"], fixed = TRUE))
    NTT[cut2] <- 1
    NTT[intersect(cut1, cut2)] <- 2
    PSMtab <- cbind(PSMtab, NTT)
    PSMtabssubs <- split(PSMtab, paste(PSMtab$assumed_charge,
                                       PSMtab$NTT))
    PSMsubpass <- lapply(PSMtabssubs, function(y) {
      protein <- y[, "protein"]
      index <- grep(decoyprefix, protein, fixed = TRUE)
      if (dim(y)[1] == 1) {
        if (length(index) == 0)
          res <- y
      }
      else {
        proORrev <- rep(0, dim(y)[1])
        index <- grep(decoyprefix, protein, fixed = TRUE)
        proORrev[index] <- 1
        score <- y[, scorecolumn]
        score <- as.numeric(score)
        tmp <- cbind(score, proORrev)
        tmp <- tmp[order(score, decreasing = TRUE), ]
        FP <- cumsum(tmp[, 2])
        tmp <- cbind(tmp, FP)
        calFDR <- unlist(lapply(1:dim(y)[1], function(x) (2 * tmp[x, "FP"])/x))
        if (TRUE %in% (calFDR <= pepFDR)) {
          cutoff <- tmp[max(which(calFDR <= pepFDR)),
                        "score"]
          res <- y[which(as.numeric(y[, scorecolumn]) >=
                           cutoff), ]
          res
        }
      }
    })
    PSMpass <- do.call(rbind.data.frame, PSMsubpass)
    rownames(PSMpass) <- NULL
    PSMpass
  }
}


# General_functions -------------------------------------------------------



#________________________________________________________________________________________
#     do_foreach_file_in_folder
#________________________________________________________________________________________

# ___Description___:
# 1: This function could be used as a template, to iternate among all files of a specific file type
# for automation

# ___Arguments___:
# dir name, and file type, with extention names (without dot)
# and fuunction/command names


do_foreach_file_in_folder <- function(function_name, file_extention = c("tsv", "pepXML"), dir){

  suppressMessages(install.packages.auto(tools))
  if(missing(dir)){
    dir <- getwd()
  }else(
    setwd(dir)
  )

  inputfiles <- list_files_with_exts(dir, c("tsv", "pepXML"), full.names = FALSE)

  for(each_inputfile in inputfiles){
    print(paste("processing", each_inputfile, "...."))
    function_name(each_inputfile)
  }

}



#test
# savetable2html(iris, file = "./html/table_network.html")


savetable2html <- function(data_table =  NULL, file = NULL){
  # prepare the packge
  suppressMessages(install.packages.auto(DT))

  dv <- datatable(data_table, filter = 'top', extensions = c('Buttons','ColReorder', 'Scroller'),
                  options = list(
                    autoWidth = TRUE,
                    dom = 'Bfrtip',
                    buttons = c('colvis','copy', 'csv', 'excel', 'pdf', 'print'),
                    colReorder = TRUE,
                    deferRender = TRUE,
                    scrollY = 200,
                    scroller = TRUE)
  )

  saveWidget2html(widget = dv, file = file )
}


# Interactive plotting by js ----------------------------------------------

# the plot_input has to be a string, with quote
# test:
#   g<-qplot(rnorm(100),rnorm(100))
#   i_plot<-ggplotly(g)
#   single_plot_to_html(plot_input =  "i_plot", header = "interactive plot to html test",HTML_filename = "scatter_plot_Test")

# the temparary rmd will be in ./RmarkDown/
# and the html will be in ./html/
# all path is configurable

# knit a complete rmarkdown file
# path can be customorized for both source and output, even filename of the output file
# the knit_root_dir is veyr important, which set the root to knit is the upper dir of the markdown file,
# which means that render will pretend the input rmd file is in the upper dir. by setting this has another advandage is that
# when you write the rmd wfile, you also assume that the file is in the root dir(which is set here)




single_plot_to_html <- function(plot_input = NULL, header = "", HTML_filename = "myplot" ){

  cat("---","\n", sep = "", file = "./RmarkDown/temp.Rmd")
  cat("title: \"",header,"\"", "\n", sep = "",file = "./RmarkDown/temp.Rmd", append=TRUE)
  cat("output: html_document","\n",sep = "",file = "./RmarkDown/temp.Rmd", append=TRUE)
  cat("---","\n",sep = "",file = "./RmarkDown/temp.Rmd", append=TRUE)

  cat("```{r, echo=FALSE, message=FALSE, warning=FALSE, fig.width= 10}","\n",sep = "",file = "./RmarkDown/temp.Rmd", append=TRUE)
  cat(plot_input,"\n",sep = "",file = "./RmarkDown/temp.Rmd", append=TRUE)
  cat("```","\n",sep = "",file = "./RmarkDown/temp.Rmd", append=TRUE)

  rmarkdown::render(input= "./RmarkDown/temp.Rmd", output_format = "html_document", knit_root_dir = "../",
                    output_file = paste(HTML_filename, ".html", sep = ""), output_dir = "./html/")

}


# saveWidget in htmlwidgets does not support relative path,
#
# here is the modification to use the convert the relative path to absolut path
# by doing so,saveWidget2 can save any  htmlwidgets based objects into html, them can be embeded into any webpage
# note that, if not in the present dir, saveWidget will not be able to delete the associated folder
# therefore one extra line is used to delete the folder

# test
#   g<-qplot(rnorm(100),rnorm(100))
#   i_plot<-ggplotly(g)
#  saveWidget2html(widget = i_plot, file ="./html/plotly_test.html" )


saveWidget2html <- function(widget, file, selfcontained = TRUE, libdir = NULL,
                        background = "white", knitrOptions = list()){
  suppressMessages(install.packages.auto(htmlwidgets))
  # see more http://www.htmlwidgets.org/

  file <- file.path(normalizePath(dirname(file)),basename(file))
  saveWidget(widget = widget, file = file, selfcontained =selfcontained, libdir= libdir,
             background = background, knitrOptions = knitrOptions)


  libdir <- paste(dirname(file),"/", tools::file_path_sans_ext(basename(file)),
                 "_files", sep = "")
  feedback <- unlink(libdir, recursive = TRUE, force = TRUE)
  return(feedback)
}





# co_occurence_visNetwork uses starts from a data matrix,(with rows as entries, columns as samples)
# and a meta file with two column names
# it will do the correlatin matrix first, then filter out interactions with low correlation or p-value (set by user)
# then do a second round of correlation, only keep the nodes with at least one quailified interactions
# do a matrix correlation plot
# output some dynamic or static plot, with interactivity


# data_meta format, two columns, names and grouping
# layout options:
# "layout_as_star"       "layout_components"    "layout_in_circle"     "layout_nicely"
# "layout_on_grid"       "layout_on_sphere"     "layout_randomly"      "layout_with_dh"
# "layout_with_drl"      "layout_with_fr"       "layout_with_gem"      "layout_with_graphopt"
# "layout_with_kk"       "layout_with_lgl"      "layout_with_mds"



co_occurence_visNetwork <-function(data_matrix =  NULL,
                                   data_meta = NULL,
                                   correlation_type = "spearman",
                                   correlation_threshold = 0.7,
                                   p_threshold = 0.05,
                                   edge_width_cex = 5,
                                   node_size_cex = 10,
                                   node_label_cex = 0.5,
                                   igraph_laylout = "layout_components",
                                   dynamic = FALSE,
                                   advanced = FALSE
){
  # prepare the packages
  suppressMessages(install.packages.auto(Hmisc))
  suppressMessages(install.packages.auto(corrplot))
  suppressMessages(install.packages.auto(igraph))
  suppressMessages(install.packages.auto(visNetwork))
  # see online document http://datastorm-open.github.io/visNetwork

  if(is.null(data_matrix)){ stop("data_matrix has to be defined!")}else{
    # calculate the correlation matrix
    data_matrix <- as.matrix(data_matrix) # just in case
    correlation_matrix <- rcorr(t(data_matrix), type = correlation_type)

    # filter the interections/correlations, using p value and correlation
    df_edge <- flattenCorrMatrix(correlation_matrix)
    df_edge_filtered <- df_edge[df_edge$p < p_threshold,]
    df_edge_filtered <- df_edge_filtered[abs(df_edge_filtered$cor) > correlation_threshold,]
    #df_edge_filtered <- df_edge_filtered[df_edge_filtered$cor > correlation_threshold,]

    # only keep the nodes with qualified edges for plotting the corrlatin plot
    data_matrix_filtered <- data_matrix[match(unique(c(df_edge_filtered[,1],
                                                       df_edge_filtered[,2])),rownames(data_matrix)),]

    # recalculate the correlation matrix and plot
    correlation_matrix_filtered <- rcorr(as.matrix(t(data_matrix_filtered)))
    mycol <- colorRampPalette(c("blue", "white", "red"))(100)

    # plot the corrleation matrix
    pdf("temp")
    dev.control('enable')
    corrplot(correlation_matrix_filtered$r, type="upper", order="original", col = mycol, p.mat = correlation_matrix_filtered$P,
             sig.level = 0.01, insig = "blank", tl.col = "black",tl.cex = 0.5,tl.srt = 30,
             method ="square", addgrid.col =NA, outline = FALSE, diag=FALSE)
    p_correlation <- recordPlot()
    dev.off()


    # only keep the filtered nodes/vertix
    data_meta_vertix <- data_meta[match(rownames(data_matrix_filtered),data_meta[,1]),]

    # reorganizing the node/vertix data for network plot
    colnames(data_meta_vertix) <- c("id","group")
    data_meta_vertix$label <- data_meta_vertix$id

    # for the purpose of using degeree and layout
    my_net <- graph_from_data_frame(d=df_edge_filtered, vertices=data_meta_vertix, directed=F)

    # Compute node degree (# of links) and use it to set node size:
    deg <- degree(my_net, mode="all")
    node_size <- ((deg-range(deg)[1])/(range(deg)[2]-range(deg)[1])+0.5)*node_size_cex

    data_meta_vertix$value <- node_size
    V(my_net)$size <- node_size

    # Set edge width based on correlation
    w <- abs(df_edge_filtered$cor)
    edge_width <- ((w - range(w)[1])/(range(w)[2]-range(w)[1])+0.1)*edge_width_cex
    df_edge_filtered$value <- edge_width
    E(my_net)$width <- edge_width

    # set the edge labeling and tooltip when hoover
    #df_edge_filtered$label <- df_edge_filtered$cor
    df_edge_filtered$title <- paste("correlation=", df_edge_filtered$cor, "; p=", df_edge_filtered$p, "; N=",df_edge_filtered$N_used, sep = "")


    ### igraph specific prepartion

    # set the colors
    # choose the color scheme here
    colors <- rainbow(length(levels(as.factor(V(my_net)$group))))
    names(colors) <- levels(as.factor(V(my_net)$group))
    V(my_net)$color <- colors[V(my_net)$group]

    # set the outline color of the node
    V(my_net)$frame.color <- "white"

    #change arrow size and edge color:
    #E(my_net)$arrow.size <- .2
    E(my_net)$edge.color <- "gray80"



    # cluster the nodes
    pdf("temp")
    dev.control('enable')
    my_clp <- cluster_label_prop(my_net)
    plot(my_clp, my_net, edge.curved=.1, vertex.label.cex = node_label_cex)
    p_igraphnetwork_cluster <- recordPlot()
    dev.off()


    if(dynamic){
      # interactive plot using visNetwork
      p_visnetwork <- visNetwork(data_meta_vertix, df_edge_filtered, main="",height = "1000px",width = "100%") %>%
        visOptions(highlightNearest = TRUE,nodesIdSelection = TRUE,
                   manipulation = TRUE, selectedBy = "group") %>%
        visLegend( position = "right") %>%
        visConfigure(enabled = advanced) %>%
        visEdges(smooth = TRUE) %>%
        visExport(type = "png", name = "export-network",
                  float = "right", label = "Save Network", style= "")

    }else{
      # using igraph layout native
      p_visnetwork <-visNetwork(data_meta_vertix, df_edge_filtered, main="", height = "1000px",width = "100%") %>%
        visOptions(highlightNearest = TRUE,nodesIdSelection = TRUE,
                   manipulation = TRUE, selectedBy = "group") %>%
        visIgraphLayout(layout = igraph_laylout) %>%
        visLegend( position = "right") %>%
        visConfigure(enabled = advanced) %>%
        visExport(type = "png", name = "export-network",
                  float = "right", label = "Save Network", style= "")
    }





    # using igraph layout native with smooth
    #visNetwork(data_meta_vertix, df_edge_filtered, main="") %>%
    #  visIgraphLayout()  %>%
    #  visEdges(smooth = TRUE)


    # export a network file

    return(list(plot_correlation_filtered = p_correlation,
                plot_network_static_cluster = p_igraphnetwork_cluster,
                plot_network_interactive = p_visnetwork,
                data_correlation_orignial = df_edge,
                data_for_network = df_edge_filtered,
                data_for_network_vertics = data_meta_vertix
    ))

  }

}


# layout_style: fruchtermanreingold, kamadakawai, circle

co_occurence_ggnetwork <-function(data_matrix =  NULL, data_meta = NULL, correlation_type = "spearman",
                                  correlation_threshold = 0.7, p_threshold = 0.01, node_size = 10,
                                  node_label_size = 2, layout_style = "kamadakawai"){

  # prepare the packages
  #install.packages.auto(sna)
  install.packages.auto(network)
  install.packages.auto(geomnet)

  if(is.null(data_matrix)){ stop("data_matrix has to be defined!")}else{
    # calculate the correlation matrix
    data_matrix <- as.matrix(data_matrix) # just in case
    correlation_matrix <- rcorr(t(data_matrix), type = correlation_type)

    # filter the interections/correlations, using p value and correlation
    df_edge <- flattenCorrMatrix(correlation_matrix)
    df_edge_filtered <- df_edge[df_edge$p < p_threshold,]
    df_edge_filtered <- df_edge_filtered[abs(df_edge_filtered$cor) > correlation_threshold,]
    #df_edge_filtered <- df_edge_filtered[df_edge_filtered$cor > correlation_threshold,]

    # only keep the nodes with qualified edges for plotting the corrlatin plot
    data_matrix_filtered <- data_matrix[match(unique(c(df_edge_filtered[,1],
                                                       df_edge_filtered[,2])),rownames(data_matrix)),]

    # recalculate the correlation matrix
    correlation_matrix_filtered <- rcorr(as.matrix(t(data_matrix_filtered)))

    mycol <- colorRampPalette(c("blue", "white", "red"))(100)


    postscript("temp")
    dev.control('enable')

    corrplot(correlation_matrix_filtered$r, type="upper", order="original", col = mycol, p.mat = correlation_matrix_filtered$P,
             sig.level = 0.01, insig = "blank", tl.col = "black",tl.cex = 0.5,tl.srt = 30,
             method ="square", addgrid.col =NA, outline = FALSE, diag=FALSE)

    p_correlation <- recordPlot()
    dev.off()


    # prepare the network data

    # using ggnetwork for network
    # data preparation for ggnet2 and ggnetwork
    co.net <- network::network(df_edge_filtered[, 1:2], directed = FALSE)
    # create edge attribute (number of trips)
    co.net %e% "cor"<- df_edge_filtered$cor
    co.net %e% "p_value"<- df_edge_filtered$p
    co.net %e% "N_used"<- df_edge_filtered$N_used

    if(!is.null(data_meta)){

      data_meta_vertix <- data_meta[match(rownames(data_matrix_filtered),data_meta[,1]),]
      # create vertex attribute if you have type/soucue information to be marked on the map
      co.net %v% "type" <- data_meta_vertix[,2]

    }

    set.seed(1981)

    data_network <- ggnetwork(co.net, layout.alg = layout_style)
    # scale the cor for easy plotting
    #data_network$cor <-  data_network$cor * edge_width_cex


    p_network <- ggplot(data = data_network,
                        aes(x, y, xend = xend, yend = yend)) +
      geom_edges(aes(size = abs(cor)), color = "grey50") +
      geom_nodes(aes(color = type), size = node_size) +
      geom_nodetext(aes(label = vertex.names), size = node_label_size) +
      scale_size_continuous("Correlation",range = range(abs(data_network$cor), na.rm =  TRUE))+
      scale_colour_manual("Source", values =  rainbow(length(levels(as.factor(type)))))+
      theme_blank() +
      theme(legend.position = "bottom", legend.box = "horizontal")



    return(list(plot_correlation_filtered = p_correlation,
                plot.network = p_network,
                data_for_network = df_edge_filtered,
                data_for_network_vertics = data_meta_vertix,
                data_for_plot = data_network
    ))

  }

}

# layout options:
# "layout_as_star"       "layout_components"    "layout_in_circle"     "layout_nicely"
# "layout_on_grid"       "layout_on_sphere"     "layout_randomly"      "layout_with_dh"
# "layout_with_drl"      "layout_with_fr"       "layout_with_gem"      "layout_with_graphopt"
# "layout_with_kk"       "layout_with_lgl"      "layout_with_mds"



co_occurence_igraph <-function(data_matrix =  NULL, data_meta = NULL, correlation_type = "spearman",
                               correlation_threshold = 0.6, p_threshold = 0.01, node_size_cex = 10,
                               edge_width_cex = 5, node_label_cex = 0.5, layout_style = "kamadakawai"){

  # prepare the packages
  suppressMessages(install.packages.auto(igraph))
  suppressMessages(install.packages.auto(Hmisc))
  suppressMessages(install.packages.auto(corrplot))

  if(is.null(data_matrix)){ stop("data_matrix has to be defined!")}else{
    # calculate the correlation matrix
    data_matrix <- as.matrix(data_matrix) # just in case

    correlation_matrix <- rcorr(t(data_matrix), type = correlation_type)

    # working section
    #matrix_Adjacency <- correlation_matrix$r

    #matrix_Adjacency[correlation_matrix$r >= 0.7 | correlation_matrix$r <= -0.7] <- 1
    #matrix_Adjacency[matrix_Adjacency != 1] <- 0

    #matrix_Adjacency[correlation_matrix$p < 0.01 ] <-0
    #matrix_Adjacency[correlation_matrix$n < ncol(data_matrix)/2 ] <-0

    #my_net2 <- graph_from_adjacency_matrix(matrix_Adjacency, mode = "undirected", diag = FALSE)
    #plot(my_net2)








    # filter the interections/correlations, using p value and correlation
    df_edge <- flattenCorrMatrix(correlation_matrix)
    df_edge_filtered <- df_edge[df_edge$p < p_threshold,]
    df_edge_filtered <- df_edge_filtered[abs(df_edge_filtered$cor) > correlation_threshold,]
    #df_edge_filtered <- df_edge_filtered[df_edge_filtered$cor > correlation_threshold,]

    # only keep the nodes with qualified edges for plotting the corrlatin plot
    data_matrix_filtered <- data_matrix[match(unique(c(df_edge_filtered[,1],
                                                       df_edge_filtered[,2])),rownames(data_matrix)),]

    # recalculate the correlation matrix
    correlation_matrix_filtered <- rcorr(as.matrix(t(data_matrix_filtered)))

    mycol <- colorRampPalette(c("blue", "white", "red"))(100)


    postscript("temp")
    dev.control('enable')

    corrplot(correlation_matrix_filtered$r, type="upper", order="original", col = mycol, p.mat = correlation_matrix_filtered$P,
             sig.level = 0.01, insig = "blank", tl.col = "black",tl.cex = 0.5,tl.srt = 30,
             method ="square", addgrid.col =NA, outline = FALSE, diag=FALSE)

    p_correlation <- recordPlot()
    dev.off()


    data_meta_vertix <- data_meta[match(rownames(data_matrix_filtered),data_meta[,1]),]


    #working section

    my_net <- graph_from_data_frame(d=df_edge_filtered, vertices=data_meta_vertix, directed=F)

    # Compute node degree (#links) and use it to set node size:
    deg <- degree(my_net, mode="all")

    V(my_net)$size <- ((deg-range(deg)[1])/(range(deg)[2]-range(deg)[1])+0.5)*10

    # set the colors
    # choose the color scheme here
    colors <- rainbow(length(levels(as.factor(V(my_net)$type))))

    names(colors) <- levels(as.factor(V(my_net)$type))
    V(my_net)$color <- colors[V(my_net)$type]

    # Set edge width based on correlation
    w <- E(my_net)$cor
    E(my_net)$width <- ((w - range(w)[1])/(range(w)[2]-range(w)[1])+0.1)*edge_width_cex

    # set the outline color of the node
    V(my_net)$frame.color <- "white"

    #change arrow size and edge color:
    #E(my_net)$arrow.size <- .2
    E(my_net)$edge.color <- "gray80"


    # plot all layout

    #layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1]
    ## Remove layouts that do not apply to our graph.
    #layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

    #par(mfrow=c(3,3), mar=c(1,1,1,1))
    #for (layout in layouts) {
    #  print(layout)
    #  l <- do.call(layout, list(my_net))
    #  plot(my_net, edge.curved=.1,vertex.label.cex = node_label_cex,layout=l, main=layout) }


    postscript("temp")
    dev.control('enable')

    #plot(my_net, edge.curved=.1, vertex.label.cex = node_label_cex)
    plot(my_net, edge.curved=.1, vertex.label.cex = node_label_cex, layout=layout_with_fr)

    legend(x=-1.1, y=-1.1, levels(as.factor(V(my_net)$type)), pch=21,
           col="#777777", pt.bg=colors, pt.cex=2.5, bty="n", ncol=3)

    p_network <- recordPlot()
    dev.off()


    # sparse the network
    my_net_sp <- delete_edges(my_net, E(my_net)[cor<0.9])
    plot(my_net_sp, edge.curved=.1, vertex.label.cex = node_label_cex, layout=layout_with_fr)

    # cluster the nodes
    my_clp <- cluster_label_prop(my_net)
    plot(my_clp, my_net, edge.curved=.1, vertex.label.cex = node_label_cex)

    # mark specific nodes
    # V(my_net) # check the node index, then locate the index, and mark them
    # plot(my_net, mark.groups=c(5:6), mark.col="#C5E5E7", mark.border=NA)


    # interactive plot using visNetwork
    install.packages.auto(visNetwork)
    visNetwork(data_meta_vertix, df_edge_filtered, width="100%", height="400px", main="Network!")


    # export a network file

    #working section


    return(list(plot_correlation_filtered = p_correlation,
                plot.network = p_network,
                data_for_network = df_edge_filtered,
                data_for_network_vertics = data_meta_vertix,
                data_for_plot = data_network
    ))

  }

}


# subfunctions, as the function name says
combine_list_to_matrix <-function(vector_list){

  all <- unique(unlist(vector_list))
  match_list <- lapply(vector_list,function(x) all %in% x)
  presence_matrix <- matrix(unlist(match_list), ncol =length(match_list) , byrow = FALSE)
  colnames(presence_matrix) <- names(vector_list)
  rownames(presence_matrix) <-all
  presence_matrix[which(presence_matrix == FALSE)] <- 0
  presence_matrix[which(presence_matrix == TRUE)] <- 1
  return(presence_matrix)
  # this function convert a vector list into a expression matrix, with presence as 1, and absence as 0

  #test_list <- list(A = letters[1:10], B =  letters[5:15])
  #combine_list_to_matrix(test_list)
}





