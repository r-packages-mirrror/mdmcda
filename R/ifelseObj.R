### Copied from `ufs`, https://r-packages.gitlab.io/ufs
ifelseObj <- function(condition, ifTrue, ifFalse) {
  if (condition) {
    return(ifTrue);
  }
  else {
    return(ifFalse);
  }
}
