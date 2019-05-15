#' Fit to a mixed univariate distributions
#'
#' @param X A random sample to be fitted.
#' @param n.obs A positive integer, is the length of the random sample to be generated
#' @param ref Aumber of clusters to use by the kmeans function to split the distribution, if isn't a number, uses mclust classification by default.
#' @param crt Criteria to be given to FDist() function
#' @param plot FALSE. If TRUE, generates a plot of the density function.
#' @param subplot FALSE. If TRUE, generates the plot of the mixed density function's partitions.
#' @param p.val_min Minimum p.value to be given to non-reject the null hypothesis.
#'
#' @return A list with whe density functions, a random sample, a  data frame with p.values results, the corresponding plots an the random numbers generator functions
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
#' X<-c(rnorm(73,189,12),rweibull(82,401,87),rgamma(90,40,19))
#' A_X<-FDistUlt(X,plot=TRUE,subplot=TRUE)
#' #A_X[[1]][[1]]()
#' #A_X[[2]]
#' #A_X[[3]]
#' #x11();A_X[[4]][[1]]
#' #x11();A_X[[4]][[2]]
#' #A_X[[5]][[1]]()
#'
#'
FDistUlt<-function(X,n.obs=length(X),ref="OP",crt=1,plot=FALSE,subplot=FALSE,p.val_min=.05){
  if(!is.numeric(ref)){}
  else{ if(ref>length(X)/3){warning("Number of clusters must be less than input length/3")
    return(NULL)}}
  desc<-function(X,fns=FALSE,ref.=ref,crt.=crt,subplot.=subplot,p.val_min.=p.val_min){
    eval<-function(X,fns.=fns,crt.=crt,subplot.=subplot,p.val_min.=p.val_min){
      FIT<-FDist(X,length(X),criteria = crt,plot = subplot,p.val_min=p.val_min)
      FIT
    }
    div<-function(X,ref.=ref){
      df<-data.frame(A=1:length(X),B=X)
      Enteros<-X-floor(X)==0
      if(any(Enteros)){
        df$CL<-ifelse(Enteros,1,2)
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
        return(list(paste("normal(",mean(X),",",ifelse(length(X)==1,0,sd(X)),")"),FN,FN(),data.frame(AD_p.v=0,KS_p.v=0,Chs_p.v=0)))
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
  PV<-do.call("rbind",superficie[purrr::map_lgl(superficie,is.data.frame)])
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
  p.v<-cbind(data.frame(Distribucion=Dist[nchar(Dist)!=0],Prop_dist=Len[nchar(Dist)!=0]),PV)
  cp<-plt<-c()
  if(plot){
    DF<-rbind(data.frame(A="Fit",DT=MA),
              data.frame(A="Real",DT=X))
    plt <- ggplot2::ggplot(DF,ggplot2::aes(x=DF$DT,fill=DF$A)) + ggplot2::geom_density(alpha=0.55)+ggplot2::ggtitle("Dist. Original")
    plt
  }
  TPlts<-c()
  if(subplot){
    cp<-cowplot::plot_grid(plotlist = PLTS, ncol = floor(sqrt(length(PLTS))))
  }
  TPlts<-list(plt,cp)
  return(list(unlist(FUN),MA,p.v,TPlts,Global_FUN))
}
