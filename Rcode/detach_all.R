#Run this function to detach all data frames even if they have been attached multiple times

repeat{
  x <- lapply(X = intersect(search(), objects()),
              FUN = function(X){detach(name = X, character.only = TRUE)})
  
  y <- lapply(x, function(X){cat(attr(X,"name"), "\n")})
  
  if(identical(x, list())){break}
}

