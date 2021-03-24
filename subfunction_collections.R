

# this file has the collected high quality, not redundunt, and highly re-usable functions.
# the main philosophy is that no more/separate function will be added, if a bit revision on the current function could work



# the function can be used as a general function to extract function for generaa

# proteinGroups is the data.frame from  read.delim, with check.names false, and string as factors  flase
#proteinGroups <- read.delim("proteinGroups.txt", header = TRUE,check.names = FALSE, stringsAsFactors = FALSE)

#log
# 20190919 v1, add options for superSILAC ratio extration



tidy_proteingroups <- function(proteinGroups){

  # extract the primary protein ID,
  protein.ids_split <- strsplit(as.vector(proteinGroups$"Protein IDs"), ";| ") # this is a list of list of split names ; for maxquant result, space( ) for Kai's open-search result
  protein_primary_ids <- unlist(lapply(protein.ids_split, function(x) x[1])) # only keep the first one
  #rownames(proteinGroups) <- protein_primary_ids # rename the rownames of the matrix

  # do the row wise filtering
  grep("\\+",proteinGroups[,grep( "ontaminant", colnames(proteinGroups))])


  index_contaminant <- grep("\\+",proteinGroups[,grep( "ontaminant", colnames(proteinGroups))])
  # note that + is a special character
  # different versions of maxquant has different tag to label the contaminant
  index_reverse <- grep("\\+", proteinGroups$Reverse)
  index_to_remove <- c(index_contaminant,index_reverse)

  if(length(index_to_remove) >0){
    proteinGroups <- proteinGroups[-index_to_remove,] # filtered table
    protein_primary_ids <- protein_primary_ids[-index_to_remove] # filtered ids
  }


  n_contaminant <- length(index_contaminant)
  n_reversed <- length(index_reverse)


  # extract the intensity column matrix
  if(any(grepl("LFQ intensity ", colnames(proteinGroups)))){ # if there are LFQ intensity columns, take out the LFQ columns
    quantification_type <- "LFQ_Intenisty"
    quantification_matrix <- proteinGroups[,grep("LFQ intensity ", colnames(proteinGroups)),drop =  FALSE]
    colnames(quantification_matrix)<-gsub("LFQ intensity ", "", colnames(quantification_matrix))

  }else if(any(grepl("Ratio H/L normalized ", colnames(proteinGroups)))){ # if there are Ratio H/L, it is usually superSILAC labeling
    quantification_type <- "Ratio_H_L_normalized"
    quantification_matrix <- proteinGroups[,grep("Ratio H/L normalized ", colnames(proteinGroups)),drop =  FALSE]
    colnames(quantification_matrix)<-gsub("Ratio H/L normalized ", "", colnames(quantification_matrix))

  } else{ # otherwise take out intensity column， even only one column
    # note that this intensity column is not the total intensity column, there is a sapce rigth after the string Intensity
    quantification_type = "Raw_intensity"
    quantification_matrix <-   proteinGroups[,grep("Intensity ", colnames(proteinGroups)),drop =  FALSE]
    colnames(quantification_matrix)<-gsub("Intensity ", "", colnames(quantification_matrix))
  }


  return(list("quantification_matrix" = quantification_matrix,
              "quantification_type" = quantification_type,
              "n_contaminant" = n_contaminant,
              "n_reversed" = n_reversed,
              "n_unique_peptides" = proteinGroups$"Unique peptides",
              "score" = proteinGroups$Score,
              "protein_primary_ids" =protein_primary_ids

  ))

}
