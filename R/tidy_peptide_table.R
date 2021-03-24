
#' clean and sort the peptide table input into a list.
#'
#' Cleaning include removing contaminant, reverse hits, extrat the peptide intensity matrix ready to use,
#' extract the experiment design, put the peptide length, sequence, missing cleavage site into vectors ready to use
#'
#'
#' @param peptide.txt a data.frame read from peptide.txt
#'
#' @return an object list, including a peptide intensity matrix, vectors of peptide_sequence,length, misscleavage
#'
#' @examples
#'  file_path <- system.file("extdata","peptides_1000.txt", package = "metalab")
#'  peptide_table <- read.delim(file_path, header = TRUE,check.names = FALSE, stringsAsFactors = FALSE) #
#'  t <- tidy_peptide_table(peptide_table)
#'  t$intensity_matrix
#'  t$summary_rawfiles
#'  t$experiment
#'
#' @export
#'
tidy_peptide_table <- function(peptide.txt){

  peptide_sequence <- peptide.txt$Sequence # only keep the first one

  # do the row wise filtering
  index_contaminant <- grep("\\+", peptide.txt$`Potential contaminant`) # note that + is a special character
  index_reverse <- grep("\\+", peptide.txt$Reverse)
  index_to_remove <- c(index_contaminant,index_reverse)

  if(length(index_to_remove) >0){ # some times there are no rows to remove
    peptide.txt <- peptide.txt[-index_to_remove,] # filtered table
    peptide_sequence <- peptide_sequence[-index_to_remove] # filtered ids
  }

  n_contaminant <- length(index_contaminant)
  n_reversed <- length(index_reverse)


  # extra the intensity column matrix
  if(any(grepl("LFQ intensity ", colnames(peptide.txt)))){ # if there are LFQ intensity columns, use the LFQ columns for the intensity

    # todo, TMT file

    intensity_columns <- peptide.txt[,grep("LFQ intensity ", colnames(peptide.txt))]
    colnames(intensity_columns) <- gsub("LFQ intensity ", "", colnames(intensity_columns)) # rename the column

  }else{ # otherwise take out intensity column, even if there is one column, without any experiment desgin

    intensity_columns <-   peptide.txt[,grep("Intensity ", colnames(peptide.txt)),drop =  FALSE]
    colnames(intensity_columns)<-gsub("Intensity ", "", colnames(intensity_columns))

  }

  return(list("intensity_matrix" = intensity_columns,
              "experiment" = colnames(intensity_columns),
              "peptide_sequence" =peptide_sequence,
              "n_contaminant" = n_contaminant,
              "n_reversed" = n_reversed,
              "score" = peptide.txt$Score,
              "Charges" =peptide.txt$Charges,
              "length" = peptide.txt$Length,
              "misscleavage" = peptide.txt$"Missed cleavages"

  ))

}
