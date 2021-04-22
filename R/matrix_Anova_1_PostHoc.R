#________________________________________________________________________________________
#     subfunction
#     matrix_PostHoc
#________________________________________________________________________________________

# this funtion use the homemade function "PostHoc" to return the post hoc analysis on a matrix
# imput: data matrix, and grouping infomation
# ouput: p values and pairs with significance

#' wrapper to apply Anova and post hoc on data matrix in a rowwise way
#'
#' @param data data matrix
#' @param groups factor of grouping, vector or factor
#'
#' @return a data frame
#' @export
#'
#' @examples
#'
matrix_Anova_1_PostHoc<-function(data,groups, p_anova = 0.05, p_ttest = 0.05) {
  p_PostHoc_pairs <-as.data.frame(apply(data,1,Anova_1_PostHoc,factor=groups, p_anova = p_anova, p_ttest = p_ttest))

  if(nlevels(as.factor(groups))>=3){

    colnames(p_PostHoc_pairs)<-"ANOVA & p-adjusted"

  }else if(nlevels(as.factor(groups)) == 2){
    colnames(p_PostHoc_pairs)<- "ttest & adjusted"
  }

  return(p_PostHoc_pairs)
}

