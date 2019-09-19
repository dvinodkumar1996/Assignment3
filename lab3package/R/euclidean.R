#' @title Finds the greatest common divisor.
#' @author Vinod kumar Dasari and Gowtham Kukkemane Mahalingabhat
#' @param m,n It takes m,n as two integer numbers.
#' @return It returns the greatest common divisor as integer.
#' @references https://en.wikipedia.org/wiki/Euclidean algorithm
#' @examples 
#' euclidean(100,1000)
#' @export

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
     return(abs(minn))
   else
   {
     temp<-maxx
     maxx<-minn
     minn<-temp%%minn
   }
 }
}


