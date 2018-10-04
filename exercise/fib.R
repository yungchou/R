fibseq = function(x){

  fib = function(x){
    if(x==1) return(1)
    if(x==2) return(1)
    return( fib(x-1)+fib(x-2) )
  }

  fibseq=c()
  for (i in 1:x){ fibseq<-c(fibseq,fib(i)) }
  return(cat('fibonaci(',x,') = ',fibseq))
}

fibseq(8)