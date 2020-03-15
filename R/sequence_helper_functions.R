### By Andrew, from:
### https://stackoverflow.com/questions/3558988/basic-lag-in-r-vector-dataframe
lagpad <- function(x, lag,
                   padding = NA) {
  if (lag>0) {
    if (!is.na(padding) && tolower(padding)=="shift") {
      padding <-
        tail(x, lag);
    } else {
      padding <-
        rep(padding, lag);
    }
    return(
      c(padding, x)[1:length(x)]
    );
  } else {
    if (!is.na(padding) && tolower(padding)=="shift") {
      padding <-
        head(x, -lag);
    } else {
      padding <-
        rep(padding, -lag);
    }
    return(
      c(x[(-lag+1) : length(x)], padding)
    );
  }
}

myLag <-
  function(x, firstElement=NULL) {
    if (is.null(firstElement)) {
      return(x[c(length(x), 1:(length(x)-1))]);
    } else {
      return(c(firstElement,
               x[1:(length(x)-1)]));
    }
  }

sameAsPrev <-
  function(x, firstElement=Inf) {

    if (!is.na(firstElement) && tolower(firstElement)=="last") {
      firstElement <- tail(x, 1);
    } else if (!is.na(firstElement) && tolower(firstElement)=="first") {
      firstElement <- head(x, 1);
    }

    res <-
      c(firstElement, tail(x, -1)) ==
      lagpad(x,
             lag = 1,
             padding=x[1]);

    return(res);

    # if (is.null(firstElement)) {
    #   return(tail(x, -1) == lagpad(tail(x, -1)));
    # } else {
    #   return(c(firstElement, tail(x, -1) == myLag(tail(x, -1),
    #                                               firstElement=x[1])));
    # }
  }

newSequenceAt <-
  function(x, shift=TRUE) {
    if (shift) {
      return(which(!sameAsPrev(x)));
    } else {
      return(c(1,
               which(!sameAsPrev(x,
                                 firstElement=FALSE))));
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
