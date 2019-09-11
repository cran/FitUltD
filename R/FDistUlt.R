#' Fits a set of observations (random variable) to test whether is drawn from a certain distribution
#'
#' @param X A random sample to be fitted.
#' @param n.obs A positive integer, is the length of the random sample to be generated
#' @param ref Aumber of clusters to use by the kmeans function to split the distribution, if isn't a number, uses mclust classification by default.
#' @param crt Criteria to be given to FDist() function
#' @param plot FALSE. If TRUE, generates a plot of the density function.
#' @param subplot FALSE. If TRUE, generates the plot of the mixed density function's partitions.
#' @param p.val_min Minimum p.value to be given to non-reject the null hypothesis.
#'
#' @return A list with the density functions, a random sample, a  data frame with the KS and AD p.values results, the corresponding plots an the random numbers generator functions
#' @export
#'
#' @importFrom purrr map
#' @importFrom purrr map_lgl
#' @importFrom assertthat is.error
#' @importFrom ADGofTest ad.test
#' @importFrom MASS fitdistr
#' @importFrom fitdistrplus fitdist
#' @importFrom mclust Mclust
#' @importFrom mclust mclustBIC
#' @importFrom cowplot plot_grid
#' @importFrom ggplot2 is.ggplot
#'
#' @examples
#'
#' set.seed(31109)
#' X<-c(rnorm(193,189,12),rweibull(182,401,87),rgamma(190,40,19))
#'
#' A_X<-FDistUlt(X,plot=TRUE,subplot=TRUE)
#'
#' A_X<-FDistUlt(X,plot=TRUE,subplot=TRUE,p.val_min=.005)
#'
#' # Functions generated
#' A_X[[1]][[1]]()
#' # Random sample
#' A_X[[2]]
#'
#' #Distributions
#' A_X[[3]]
#'
#' # Plots
#' par(mfrow=c(1,2))
#' A_X[[4]][[1]]
#' A_X[[4]][[2]]
#'
#' # More functions
#' A_X[[5]][[1]]()
#'
#'
FDistUlt<-function(X,n.obs=length(X),ref="OP",crt=1,plot=FALSE,subplot=FALSE,p.val_min=.05){
  if(!is.numeric(ref)){}else{
    if(ref>length(X)/3){warning("Number of clusters must be less than input length/3")
      return(NULL)}}
  desc<-function(X,fns=FALSE,ref.=ref,crt.=crt,subplot.=subplot,p.val_min.=p.val_min){
    eval<-function(X,fns.=fns,crt.=crt,subplot.=subplot,p.val_min.=p.val_min){
      FIT<-FDist(X,length(X),crit = crt,plot = subplot,p.val_min=p.val_min)
      FIT
    }
    div<-function(X,ref.=ref){
      df<-data.frame(A=1:length(X),B=X)
      Enteros<-X-floor(X)==0
      if(any(Enteros)){
        if(all(Enteros)){
          if(!is.numeric(ref)){
            mod1<-mclust::Mclust(X,modelNames=c("E", "V"))$classification
            if(length(table(mod1))==1){
              df$CL<-kmeans(df,2)$cluster
            }else{
              df$CL<-mod1
            }
          }else{
            df$CL<-kmeans(df,ref)$cluster
          }
        }else{
          df$CL<-ifelse(Enteros,1,2)
        }
      }else{
        if(!is.numeric(ref)){
          mod1<-mclust::Mclust(X)$classification
          if(length(table(mod1))==1){
            df$CL<-kmeans(df,2)$cluster
          }else{
            df$CL<-mod1
          }
        }else{
          df$CL<-kmeans(df,ref)$cluster
        }
      }
      CLS<-purrr::map(unique(df$CL),~df[df$CL==.x,2])
      CLS
      return(CLS)
    }
    suppressWarnings(EV<-eval(X,fns))
    if(is.null(EV)){
      if(length(X)>40){
        DV<-purrr::map(div(X),~desc(.x,fns))
        return(DV)
      }else{
        FN<-rnorm
        formals(FN)[1]<-length(X)
        formals(FN)[2]<-mean(X)
        formals(FN)[3]<-ifelse(length(X)==1,0,sd(X))
        return(list(paste0("normal(",mean(X),",",ifelse(length(X)==1,0,sd(X)),")"),FN,FN(),
                    data.frame(Dist="norm",AD_p.v=1,KS_p.v=1,estimate1=mean(X),estimate2=sd(X),estimateLL1=0,estimateLL2=1,PV_S=2)
        ))
      }
    }else{
      return(EV)
    }
  }
  FCNS<-desc(X)
  flattenlist <- function(x){
    morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
    out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
    if(sum(morelists)){
      base::Recall(out)
    }else{
      return(out)
    }
  }
  superficie<-flattenlist(FCNS)
  FUN<-superficie[purrr::map_lgl(superficie,~"function" %in% class(.x))]
  Global_FUN<-superficie[purrr::map_lgl(superficie,~"gl_fun" %in% class(.x))]
  Dist<-unlist(superficie[purrr::map_lgl(superficie,is.character)])
  PLTS<-superficie[purrr::map_lgl(superficie,ggplot2::is.ggplot)]
  dfss<-superficie[purrr::map_lgl(superficie,~is.data.frame(.x))]
  PV<-do.call("rbind",dfss[purrr::map_lgl(dfss,~ncol(.x)==9)])
  Len<-MA<-c()
  repp<-floor(n.obs/length(X))+1
  for (OBS in 1:repp) {
    for (mst in 1:length(FUN)) {
      ljsd<-FUN[[mst]]()
      MA<-c(MA,ljsd)
      if(OBS==1){
        Len<-c(Len,length(ljsd)/length(X))
      }
    }
  }
  MA<-sample(MA,n.obs)
  pv1<-data.frame(Distribution=Dist[nchar(Dist)!=0],Dist_Prop=Len[nchar(Dist)!=0])
  p.v<-try(cbind(pv1,PV))
  if(assertthat::is.error(pv1)){p.v<-pv1}
  cp<-plt<-c()
  if(plot){
    DF<-rbind(data.frame(A="Fit",DT=MA),
              data.frame(A="Real",DT=X))
    plt <- ggplot2::ggplot(DF,ggplot2::aes(x=DF$DT,fill=DF$A)) + ggplot2::geom_density(alpha=0.55)+ggplot2::ggtitle("Original Dist.")
    plt
  }
  TPlts<-c()
  if(subplot){
    cp<-cowplot::plot_grid(plotlist = PLTS, ncol = floor(sqrt(length(PLTS))))
  }
  TPlts<-list(plt,cp)
  return(list(unlist(FUN),MA,p.v,TPlts,Global_FUN))
}
