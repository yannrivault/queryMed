##################@
pddi_plot <- function(drug= "", type="name", direction="object", source=NULL, contraindication= NULL, plot=TRUE, level=4, mypalette=NULL, weight=NULL){
  
  data(DIKB)
  data(ATC)  
  lev <- paste("atc",level, sep="")
  if(level >1){
  levm1 <- paste("atc",level-1, sep="")
  }
  else{
    levm1 <- lev
  }
  lab <- paste("label",level, sep="")

  
  # filter by data sources     
  if(!is.null(source)){
    DIKB <-DIKB[DIKB$source%in%source,]
    if(is.null(nrow(x))) {
      return(warning("No data for the source"))
    }
  }
  
  # looking more specifically for contraindication (or not)
  if(!is.null(contraindication)){
    if(contraindication==TRUE){
      DIKB<-DIKB[DIKB$contraindication==TRUE,]
      if(is.null(nrow(x))) {
        return(warning("No contraindication found"))
      }
    } 
  }
  # # looking more specifically for precaution (or not)
  # if(!is.null(precaution)){
  #   if(precaution==TRUE){
  #     DIKB<-DIKB[DIKB$precaution==TRUE,]
  #     if(is.null(nrow(x))) {
  #       return(warning("No precaution found"))
  #     }
  #   } 
  # }  
  # drug can be as a character,as a drung bank ID, or an ATC code
  if(direction=="object"){
    x <- switch(type,
                name = DIKB[DIKB$object==drug,],
                DBI = DIKB[DIKB$drug1==drug, ],
                ATC = DIKB[DIKB$atc1==drug, ])
    if(is.null(nrow(x))) {
      return(warning("No data found"))
    }
  }  
  if(direction=="precipitant"){
      x <- switch(type,
                  name = DIKB[DIKB$precipitant==drug,],
                  DBI = DIKB[DIKB$drug2==drug, ],
                  ATC = DIKB[DIKB$atc2==drug, ])
      if(is.null(nrow(x))) {
        return(warning("No data found"))
      }
  }    
  x = as.data.table(x)
  x <- x[!is.na(x$atc2)|!is.na(x$atc1),]
   
  if(direction=="object"){
    statx <- x[, .N,by=list(precipitant, atc2, drug2)]
  }
  if(direction=="precipitant"){
    statx <- x[, .N, by=list(object, atc1, drug1)]
  }
  statx <- data.frame(statx)
  names(statx) <-   c("interact", "atc5", "dbi", "n")
  statx <- merge(statx, ATC[, c("atc5", lev, lab, levm1)], by="atc5")
  
  if(plot==TRUE){

    statx <- as.data.table(statx)
    statx<-statx[,.(n=sum(n)), by=c(lab, levm1)]
    statx <- data.frame(statx)

   
    root <- unique(statx[, levm1])
    n = length(root)
      #mypalette<- colorRampPalette(brewer.pal(12,"Set3"))(n)
      #mypalette<- colorRampPalette(brewer.pal(9,"YlOrRd"))(n)
    if(is.null(mypalette)){
      mypalette<- colorRampPalette(brewer.pal(11,"Spectral"))(n)
    }
    mypalette = cbind(mypalette, "root"= as.character(root))
    statx <- merge(statx, mypalette, by.x=levm1, by.y="root")
  
    if(!is.null(weight)){
      statx<- statx[statx$n>=weight, ]
    }
    if(is.null(nrow(x))) {
      return(warning("No data found"))
    }

    g1 <- graph_from_edgelist(as.matrix(cbind(drug, as.character(statx[,2]))), directed=F)
    statx$n[statx$n == 1 ] <- 0
    E(g1)$weight <- statx$n
    E(g1)$width <- 1+E(g1)$weight*2
    V(g1)$size <- 20
    V(g1)$frame.color <- "white"
    V(g1)$color <-  c("#FFFFCC", as.character(statx$mypalette))
    l1 <- layout_as_star(g1)
    plot(g1, edge.color="grey", layout=l1)
  }
  return(statx)
}


