#'  Potential drug-drug interaction visualization
#'   
#'  This function proposes a graph or a radarplot representation of potential drug-drug interaction as listed in DIKB data base.
#' 
#' @param drug character vector that specifies drug names or Drug Bank Identifiers (<https://www.drugbank.ca/>) (DBI) or Anatomical Therapeutic Chemical compound's codes (<http://www.who.int/classifications/atcddd/en/>) (ATC)
#' @param type   character vector that specifies whether drugs are drug name (default), DBI or ATC code.
#' @param direction character vector that specifies whether the queried drug is an object or an precipitant as defined by DIKB 
#' @param nbinteractions numeric that states the minimun number of sources listing the interaction or the number of time the interactions have been listed by a source.
#' @param source character vector that specifies the original source of the potential drug-drug interaction. It could be CredibleMeds, DDI-Corpus-2011, DDI-Corpus-2013, Drugbank, FrenchDB,  HEP,  HIV,   NDF-RT, NLM-Corpus, ONC-HighPriority, ONC-NonInteruptive, OSCAR,  PK-Corpus, World-Vista, Kegg (See <https://github.com/dbmi-pitt/public-PDDI-analysis> for more details). 
#' @param contraindication If true filter on the contraindication listed in the Kegg data source.
#' @param plot character vector that specifies the type of graph. If "graph" is used a star graph will be displayed for one drug and its interactant. If "radar" is used a radarplot will be displayed to compare two or more drugs and their interactants. If NULL (default) the function return a data.frame of counted interactions.  
#' @param level numeric ranging from 1 to 5 that indicates the ATC level to use to display the potential interacting drugs with the drug of interest.  
#' @param mypalette character vector that specifies color of the nodes. If null the default is defined using brewer.pal and colorRampPalette from RBrewerColor and grDevices, respectively.
#' @param weight numeric defining a threshold to filter out graph nodes and edges when the potential drug-drug interaction are listed less than  that threshold by different sources.
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom igraph graph_from_edgelist layout_as_star V E "E<-" "V<-"
#' @importFrom fmsb radarchart
#' @importFrom RColorBrewer brewer.pal
#' @importFrom utils data
#' @importFrom data.table as.data.table .N
#' 
#' @details For the star graph centered on the drug of interest. The color of the nodes depends on the ATC level specified in the function cal; grouping molecules by ATC classes. The thickness of the edges depends on the number of times the  potential drug-drug interaction is listed by different sources.
#'  
#' @return  If the argument graph is set to "graph" or "radard" a plot is returned and the associated data.frame is returned. When the graph argument is set to FALSE anly the data.frame is returned.
#' @author N. Le Meur
#'  
#' @seealso [DIKB] dataset and igraph package
#' @export
#' @examples
#' pddi_plot("CLOPIDOGREL", "name", plot="graph", level=4)
#' pddi_plot("CLOPIDOGREL", "name", plot="graph", level=5, nbinteractions=2)
#' pddi_plot(c("CLOPIDOGREL","PRASUGREL"), "name",  plot="radar", level=3, nbinteractions=2)

pddi_plot <- function(drug, type="name", direction="object", 
                      nbinteractions= 1, source=NULL, contraindication= NULL, 
                      plot="graph", level=4, mypalette=NULL, weight=NULL){

  utils::data(DIKB)
  utils::data(ATC)  
  precipitant <- atc2 <- drug2 <- object <- atc1 <- drug1 <- NULL

  lev <- paste("atc",level, sep="")
  if(level >1){
  levm1 <- paste("atc",level-1, sep="")
  } else{
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

  # drug can be as a character,as a drung bank ID, or an ATC code
  if(direction=="object"){
    x <- switch(type,
                name = DIKB[DIKB$object%in%drug,],
                DBI = DIKB[DIKB$drug1%in%drug, ],
                ATC = DIKB[DIKB$atc1%in%drug, ])
    if(is.null(nrow(x))) {
      return(warning("No data found"))
    }
  }

  if(direction=="precipitant"){
      x <- switch(type,
                  name = DIKB[DIKB$precipitant%in%drug,],
                  DBI = DIKB[DIKB$drug2%in%drug, ],
                  ATC = DIKB[DIKB$atc2%in%drug, ])
      if(is.null(nrow(x))) {
        return(warning("No data found"))
      }
  }    
  x = as.data.table(x)
  x <- x[!is.na(x$atc2)|!is.na(x$atc1),]

  if(direction=="object"){
    statx <- x[, .N, by=list(precipitant, atc2, drug2, object)]
  }
  if(direction=="precipitant"){
    statx <- x[, .N, by=list(object, atc1, drug1, precipitant)]
  }
  statx <- data.frame(statx)

# filter on number of sources stating the interaction
  statx <- statx[statx$N >= nbinteractions, ]
  names(statx) <-   c("interact", "atc5", "dbi", "target", "n")

  verif1 <- sum(ATC$atc5%in%statx$atc5)

  if(verif1>1){
    if(level==5){
      statx <- merge(statx, ATC[, c("atc5", lab, levm1)], by="atc5")
    }else{
      statx <- merge(statx, ATC[, c("atc5", lev, lab, levm1)], by="atc5")
    }
    
    if(plot=="graph"){
      pddi_graph(x=statx, drug=drug, lab=lab, level=levm1, nbinteractions=nbinteractions, mypalette=mypalette, weight=weight)
    }
    if(plot=="radar"){
      pddi_radar(x=statx, drug=drug, lab=lab, level=levm1)
    }
  }
  if(verif1==0){
    warning("No match in ATC database")
  }
  return(statx)
}

#--------------------- graph plot
# for 1 drug
pddi_graph <- function(x, drug, lab, level, nbinteractions, mypalette=NULL, weight=NULL){
  
    print(x)
    x <- as.data.table(x)
    x<-x[,.(n=sum(n)), by=c("target", lab, level)]
    x <- data.frame(x)

    root <- unique(x[, level])
    n <- length(root)

    if(is.null(mypalette)){
      mypalette <- colorRampPalette(brewer.pal(11,"Spectral"))(n)
    }
    mypalette <- cbind(mypalette, "root"= as.character(root))
    x <- merge(x, mypalette, by.x=level, by.y="root")
    
    if(!is.null(weight)){
      x <- x[x$n>=weight, ]
    }
    if(is.null(nrow(x))) {
      return(warning("No data found"))
    }
    
    g1 <- graph_from_edgelist(as.matrix(cbind(as.character(x[,2]), as.character(x[,3]))), directed=F)

    x$n[x$n == 1 ] <- 0
    #g1 <- set_edge_attr(g1, weight, E(g1),  statx$n)
    E(g1)$weight <- x$n
    E(g1)$width <- 1+E(g1)$weight*2
    V(g1)$size <- 20
    V(g1)$frame.color <- "white"


    #if(level==5 & nbinteractions==1){
    #  V(g1)$color <-  c(as.character(x$mypalette))
    #}
   # else{
    #  print( V(g1))
     # V(g1)$color <-  c(rep("#FFFFCC", length(drug)) , as.character(x$mypalette))
      V(g1)$color <-  colorRampPalette(brewer.pal(11,"Spectral"))(length(V(g1)))
    #}
    l1 <- layout_as_star(g1)
    plot(g1, edge.color="grey", layout=l1)
}





#----------------------- Radard plots
# for 2 drugs
pddi_radar <- function(x, drug, lab, level){
    
    x <- as.data.table(x)
    x<-as.data.frame(x[,.(n=sum(n)), by=c("target", lab, level)])
    
    v<- tidyr::spread(x[, -3], lab, n)
    rownames(v) <- v[,1]
    v <- v[,-1]
    v[is.na(v)] <- 0

    v2 = data.frame(rbind(rep(max(v), ncol(v)) , rep(-1, ncol(v)) , v))
   
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
    
    radarchart(v2,
               axistype=2 , 
               #custom polygon
               pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
               #custom the grid
               cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
               #custom labels
               vlcex=0.8)
    legend(x=1.2 , y=1.3, legend = rownames(v2)[3:nrow(v2)], bty = "n", pch=20 , col=colors_in , text.col = "darkgrey", cex=1, pt.cex=1)
  }



