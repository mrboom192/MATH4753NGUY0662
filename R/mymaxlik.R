#' Maximum Likelihood Estimation with Plotting
#'
#' This function computes the maximum likelihood estimate (MLE) of a parameter by evaluating
#' the likelihood function across a range of values and then identifying the value that maximizes the likelihood.
#' It also plots the likelihood function with an indication of the MLE.
#'
#' @param lfun A likelihood function that takes two arguments: an observed data vector `x` and a parameter value.
#' @param x A numeric vector of observed data.
#' @param param A numeric vector of possible parameter values to evaluate for maximum likelihood.
#' @param ... Additional graphical parameters to pass to the plot.
#' @return A list with elements:
#' \item{i}{The index of the maximum likelihood parameter value in `param`.}
#' \item{parami}{The parameter value that maximizes the likelihood.}
#' \item{yi}{The maximum likelihood value.}
#' \item{slope}{The slope around the maximum likelihood value (a vector), used to check for a maximum.}
#' @importFrom graphics plot abline points axis
#' @export
mymaxlik=function(lfun,x,param,...){
  # how many param values are there?
  np=length(param)
  # outer -- notice the order, x then param
  # this produces a matrix – try outer(1:4,5:10,function(x,y) paste(x,y,sep=" "))   to understand
  z=outer(x,param,lfun) # A
  # z is a matrix where each x,param is replaced with the function evaluated at those values
  y=apply(z,2,sum)
  
  # y is a vector made up of the column sums
  # Each y is the log lik for a new parameter value
  plot(param,y,col="Blue",type="l",lwd=2,...)
  # which gives the index for the value of y >= max.
  # there could be a max between two values of the parameter, therefore 2 indices
  # the first max will take the larger indice
  i=max(which(y==max(y))) # B
  abline(v=param[i],lwd=2,col="Red")
  
  # plots a nice point where the max lik is
  points(param[i],y[i],pch=19,cex=1.5,col="Black")
  axis(3,param[i],round(param[i],2))
  #check slopes. If it is a max the slope should change sign from + to 
  # We should get three + and two -vs
  ifelse(i-3>=1 & i+2<=np, slope<-(y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]),slope<-"NA")
  return(list(i=i,parami=param[i],yi=y[i],slope=slope))
}