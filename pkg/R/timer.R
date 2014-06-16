timer = local({
  start = Sys.time()
  function(set=FALSE) {
    if(set) {
      start <<- Sys.time()
      return(0)
    }
    return(as.numeric(Sys.time() - start))
  }
})
