#' @author Vinod kumar Dasari
#' @param It takes m,n as two integer numbers.
#' @return It returns the greatest common divisor as integer.
#' @references https://en.wikipedia.org/wiki/Euclidean algorithm
#' @example euclidean(100,1000)

euclidean<-function(m,n)
{
  if(m>n)
  {
    minn<-n
    maxx<-m
  }
  else
  {
    minn<-m
    maxx<-n
  }
 while(TRUE)
 {
   if(maxx%%minn==0)
     return(minn)
   else
   {
     temp<-maxx
     maxx<-minn
     minn<-temp%%minn
   }
 }
}

euclidean(23,27)
