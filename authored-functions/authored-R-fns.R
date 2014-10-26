


replace.vars <- function(replacement.matrix, directory, file.pattern, table.only=TRUE ) {
  
  targ.files <- list.files(path = directory, pattern=file.pattern, full.names=TRUE)
  
  for ( i in targ.files) {
    tex.text <- readLines(i)
    for ( j in 1:nrow(replacement.matrix)) {
      if (replacement.matrix[j, 2]=="") {next}
      table.only.vec <- grep("hline", tex.text) 
      table.only.vec <- (1:length(tex.text))[min(table.only.vec)<=1:length(tex.text)  &
          1:length(tex.text)<=max(table.only.vec)]
      if (!table.only) {table.only.vec <- 1:length(tex.text)}
      tex.text[table.only.vec] <- gsub(pattern=replacement.matrix[j, 1], 
        replacement=replacement.matrix[j, 2], x=tex.text[table.only.vec],  fixed=TRUE)
    }
    cat(tex.text, file=i, sep="\n")
  }
  NULL
}
  
# Example usage:
# var.names <- read.csv("/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/var names.csv", stringsAsFactors=FALSE)

#replace.vars(replacement.matrix=var.names, 
#  directory="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/", 
#  file.pattern="table.*tex", table.only=TRUE)
  



replace.vars.string <- function(replacement.matrix, input.table, html=FALSE, table.only=TRUE ) {
  
    tex.text <- input.table
    for ( j in 1:nrow(replacement.matrix)) {
      if (replacement.matrix[j, 2]=="") {next}
      if (!html) {
      table.only.vec <- grep("hline", tex.text) 
      table.only.vec <- (1:length(tex.text))[min(table.only.vec)<=1:length(tex.text)  &
          1:length(tex.text)<=max(table.only.vec)]
      }
      if (!table.only) {table.only.vec <- 1:length(tex.text)}
      tex.text[table.only.vec] <- gsub(pattern=replacement.matrix[j, 1], 
        replacement=replacement.matrix[j, 2], x=tex.text[table.only.vec],  fixed=TRUE)
    }
    tex.text
}

