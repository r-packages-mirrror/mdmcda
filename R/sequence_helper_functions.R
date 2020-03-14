myLag <-
  function(x, firstElement=NULL) {
    if (is.null(firstElement)) {
      return(x[c(length(x), 1:(length(x)-1))]);
    } else {
      return(x[c(firstElement, 1:(length(x)-1))]);
    }
  }

sameAsPrev <-
  function(x, firstElement=NULL)
    return(c(firstElement, tail(x, -1) == myLag(tail(x, -1),
                                                firstElement=firstElement)));

newSequenceAt <-
  function(x, shift=TRUE) {
    if (shift) {
      return(which(!sameAsPrev(x)));
    } else {
      return(c(1,
               which(!sameAsPrev(x,
                                 firstElement=TRUE))));
    }
  }

newSequenceEnds <-
  function(x, shift=TRUE) {
    return(c(tail(newSequenceAt(x, shift=shift), -1) - 1, length(x)));
  }

sequenceNumbering <-
  function(x, shift=TRUE) {
    as.vector(mapply(seq,
                     newSequenceAt(x, shift=shift) -
                       newSequenceAt(x, shift=shift) + 1,
                     newSequenceEnds(x, shift=shift) -
                       newSequenceAt(x, shift=shift) + 1));
  }
