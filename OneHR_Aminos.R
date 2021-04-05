prots <- read.csv("data4.csv", header = TRUE, sep = ";")

one_hot <- function(df, col_name='V'){
  
  num_cols <- ncol(df) * 20
  cols_onehot <- c()
  for (i in 1:num_cols) {
    cols_onehot <- append(cols_onehot, paste(col_name,i,sep=""))
  }
  
  df2 <- data.frame(matrix(nrow = 0, ncol = num_cols))
  
  aminoacidos <- c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I", "L", "K", "M", "F", "P", "S", "T", "W", "Y","V")
  
  for(i in 1:nrow(df)){
    aa_oh <- c()
    for(c in 1:ncol(df)){
      pos <- which(aminoacidos == df[i,c])
      oh_amino <- rep(0, length(aminoacidos))
      oh_amino[pos] <- 1
      aa_oh <- append(aa_oh, oh_amino)
    }
    df2 <- rbind(df2, aa_oh)
  }
  colnames(df2) <- cols_onehot
  
  return(df2)
}