prots <- read.csv("data4.csv", header = TRUE, sep = ";")

test_p1 <- function(df, col_name="V"){
  
  num_cols <- ncol(df) * 20
  cols_onehot <- list()
  for (i in 1:num_cols) {
    cols_onehot <- append(cols_onehot, paste(col_name,i,sep=""))
  }
  
  df2 <- df
  for (i in cols_onehot) {
    df2[,i] <- 0
  }
  aminoacidos <- c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I", "L", "K", "M", "F", "P", "S", "T", "W", "Y","V")
  aa_oh <- list()
  for(j in df2){
    seq <- split((df2[j,2]), "")[[1]]
  
  for(i in seq){
    pos <- which(aminoacidos == i)
    oh_amino <- rep(0, length(aminoacidos))
    oh_amino[pos] = 1
    aa_oh <- append(aa_oh, as.numeric(oh_amino))
  }
  }
  return(aa_oh)
  }

t1 <- test_p1(prots)


one_hot <- function(df, col_name='V',remove_col_seq = TRUE){
  
  num_cols <- ncol(df) * 20
  cols_onehot <- list()
  for (i in 1:num_cols) {
    cols_onehot <- append(cols_onehot, paste(col_name,i,sep=""))
  }
  
  df2 <- df
  for (i in cols_onehot) {
    df2[,i] <- 0
  }
  
  aminoacidos <- c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I", "L", "K", "M", "F", "P", "S", "T", "W", "Y","V")
  aa_oh <- list()
  
  for(i in df){
    pos <- which(aminoacidos == i)
    oh_amino <- rep(0, length(aminoacidos))
    oh_amino[pos] = 1
    aa_oh <- append(aa_oh, c(oh_amino))
  }
  
  for (k in aa_oh) {
    ind <- grep(paste(col_name,k,sep=""), colnames(df2))
    df2[i,ind] <- aa_oh[k]
  }
  return(df2)
}

t2 <- one_hot(prots[-18])
