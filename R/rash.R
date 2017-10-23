"new.h" <- function(...) new.env(hash=TRUE, ...)
"is.h"  <- function(hash) {
    ifelse(  is.environment(hash) &&
           ! is.null(env.profile(hash)),
        return(TRUE), return(FALSE))
}
"len.h" <- function(hash) {
    if(! is.h(hash)) {
        warning("need hash"); return(NULL)
    }
    return(length(ls(envir=hash)))
}
"set.h" <- function(key, val, hash) {
    if(! is.h(hash)) {
        warning("missing hash"); return(NULL)
    }
    assign(as.character(key), val, envir=hash)
}
"setnum.h" <- function(key, val, hash) {
   if(is.null(val)) {
      set.h(key, val, hash);
   } else {
      set.h(key, as.numeric(val), hash);
   }
}
"setint.h" <- function(key, val, hash) {
   if(is.null(val)) {
      set.h(key, val, hash);
   } else {
      set.h(key, as.integer(val), hash);
   }
}
"setchar.h" <- function(key, val, hash) {
   if(is.null(val)) {
      set.h(key, val, hash);
   } else {
      set.h(key, as.character(val), hash);
   }
}
"setlog.h" <- function(key, val, hash) {
   if(is.null(val)) val <- FALSE;
   val <- as.logical(val);
   if(is.na(val)) val <- FALSE;
   set.h(key, val, hash);
}
"get.h" <- function(key, hash, default=NULL) {
    if(! is.h(hash)) {
        warning("need hash"); return(NULL)
    }
    key <- as.character(key)
    ifelse(exists(key, envir=hash),
        return(get(key, hash)), return(default))
}
"del.h" <- function(key, hash) {
    if(! is.h(hash)) {
        warning("need hash"); return(NULL)
    }
    key <- as.character(key)
    if(exists(key, envir=hash)) {
        rm(list=c(key), envir=hash)
        return(TRUE)
    }
    return(FALSE)
}
"haskey.h"  <- function(key, hash) {
    if(! is.h(hash)) {
        warning("need hash"); return(NULL)
    }
    return(exists(as.character(key), envir=hash))
}
"keys.h" <- function(hash) {
    if(! is.h(hash)) {
        warning("missing hash"); return(NULL)
    }
    return(ls(envir=hash))
}
"vals.h" <- function(hash) {
    if(! is.h(hash)) {
        warning("need hash"); return(NULL)
    }
    keys <- keys.h(hash)
    n <- length(keys)
    if(n == 0) return(NULL)
    vals <- vector(length=n)
    for(i in 1:n) {
       vals[i] <- get.h(keys[i], hash)
    }
    v <- vector(length=n)
    v <- sapply(1:n, function(i) return(vals[[i]]))
    return(v)
}
"each.h" <- function(hash) {
    if(! is.h(hash)) {
        warning("need hash"); return(NULL)
    }
    return(as.list(hash))
}
"clear.h" <- function(hash) {
    if(! is.h(hash)) {
        warning("need hash"); return(NULL)
    }
    rm(list=keys.h(hash), envir=hash)
}
"tag.h" <- function(hash) {
    tag <- capture.output(hash)
    attributes(hash) <- list(hexid=tag)
}
"copy.h" <- function(hash) {
  if(! is.h(hash)) {
    warning("need hash"); return(NULL)
  }
  tmp.h <- new.h()
  keys <- keys.h(hash)
  for(i in 1:length(keys)) {
    val <- get.h(keys[i], hash)
    set.h(keys[i], val, tmp.h)
  }
  return(tmp.h)
}
