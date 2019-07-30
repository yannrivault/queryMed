#'  Potential drug-drug interaction visualization
#'   
#'  This function proposes a graph or a radarplot representation of potential drug-drug interaction as listed in DIKB data base.
#' 
#' @param drug character vector that specifies drug names or Drug Bank Identifiers (<https://www.drugbank.ca/>) (DBI) or Anatomical Therapeutic Chemical compound's codes (<http://www.who.int/classifications/atcddd/en/>) (ATC)
#' @param nbsources numeric that states the minimun number of sources listing the interaction.
#' @param level numeric ranging from 1 to 5 that indicates the ATC level to use to display the potential interacting drugs with the drug of interest.  
#' @param plot character vector that specifies the type of graph. If "graph" is used a star graph will be displayed for one drug and its interactant. If "radar" is used a radarplot will be displayed to compare two or more drugs and their interactants. If NULL (default) the function return a data.frame of counted interactions.  
#' @param mypalette character vector that specifies color of the nodes. If null the default is defined using brewer.pal and colorRampPalette from RBrewerColor and grDevices, respectively.
#' @param weight numeric defining a threshold to filter out graph nodes and edges when the potential drug-drug interaction are listed less than  that threshold by different sources.
#'
#' @importFrom grDevices colorRampPalette rgb
#' @importFrom igraph graph_from_edgelist layout_as_star V E "E<-" "V<-" plot.igraph
#' @importFrom fmsb radarchart
#' @importFrom RColorBrewer brewer.pal
#' @importFrom utils data
#' @importFrom data.table as.data.table .N
#' @importFrom graphics legend
#' 
#' @details For the star graph centered on the drug of interest. The color of the nodes depends on the ATC level specified in the function cal; grouping molecules by ATC classes. The thickness of the edges depends on the number of times the  potential drug-drug interaction is listed by different sources.
#'  
#' @return  If the argument graph is set to "graph" or "radard" a plot is returned and the associated data.frame is returned. When the graph argument is set to FALSE anly the data.frame is returned.
#' @author N. Le Meur
#'  
#' @seealso [DIKB] dataset and igraph package
#' @export
#' @examples
#' pddi_plot("clopidogrel", level=4,  plot="graph")
#' pddi_plot("clopidogrel", level=5, plot="graph", weight=1.5)
#' pddi_plot(c("clopidogrel","prasugrel"), nbsources = 2, level=5, plot="radar")

pddi_plot <- function(drug, nbsources= 1, level=4, plot="graph",  mypalette=NULL, weight=NULL){

  utils::data(DIKB)

  # drug can be as a character,as a drung bank ID, or an ATC code
  type <- ifelse( (length(grep("^DB", drug))/length(drug))>=1, "dbi", 
                  ifelse( length(grep("^[A-V][0-9].*[0-9]$", drug))/length(drug) >= 1 & nchar(drug)==7, "atc", "name"))
  

  statx <- switch(type,
                name = DIKB[DIKB$label5.atc.object%in%drug,],
                dbi = DIKB[DIKB$drug.id.object%in%drug, ],
                atc = DIKB[DIKB$atc.object%in%drug, ])
    if(is.null(nrow(statx))) {
      return(warning("No data found, verify that all you drug ids or name are of the same type (DBI, ATC or name)"))
    }
  
  statx = as.data.table(statx)
  
  # filter on number of sources stating the interaction
  statx <- statx[rowSums(statx[, 2:6], na.rm=T) >= nbsources, ]
 
  print(statx)
    if(plot=="graph"){
      pddi_graph(x=statx, drug=drug, level=level, nbsources=nbsources, mypalette=mypalette, weight=weight)
    }
    if(plot=="radar"){
      pddi_radar(x=statx, drug=drug, level=level)
    }

  return(statx)
}

#--------------------- graph plot
# for 1 drug
pddi_graph <- function(x, drug, nbsources, level, mypalette=mypalette, weight=NULL){
  
    level <- paste("label", level, ".atc.precipitant", sep="")
    x <- as.data.table(x)
    # add root atc info for future color palette
    x <- cbind(x, "root"=substr(unlist(x[,14]), 1, 1))
    # mean score to integrate levels
    x<-x[,.(n=mean(score.max, na.rm=T)), by=c("drug.id.object", level, "root")]
    x <- data.frame(x)
    # remove drug with no atc label (-need to see how to do for DBI queries)
    x<- x[!is.na(x[,2]), ]
    
    # compute color palette
    root <- unique(x$root)
    n <- length(root)
    if(is.null(mypalette)){
      mypalette <- colorRampPalette(brewer.pal(11,"Spectral"))(length(root))
    }
    mypalette <- cbind(mypalette, "root"= root)
    print(mypalette)
    x <- merge(x, mypalette, by="root")

    print(x)
    if(!is.null(weight)){
      x <- x[x$n>=weight, ]
    }
    if(is.null(nrow(x))) {
      return(warning("No data found"))
    }
    
    g1 <- graph_from_edgelist(as.matrix(cbind(drug, as.character(x[,3]))), directed=F)


    E(g1)$weight <- x$n
    E(g1)$width <- 3^E(g1)$weight
    V(g1)$size <- 20
    V(g1)$frame.color <- "white"
    print(x)

    if(level==5 & nbsources==1){
     V(g1)$color <-  c(as.character(x$mypalette))
    }
   else{
   # print( V(g1))
     V(g1)$color <-  c(rep("#FFFFCC", length(drug)) , as.character(x$mypalette))
    #V(g1)$color <-  colorRampPalette(brewer.pal(11,"Spectral"))(length(V(g1)))
    }
    l1 <- layout_as_star(g1)
    if(level==5){
      plot.igraph(g1, edge.color="grey", layout=l1, edge.label = E(g1)$weight)
    }
    else{
      plot.igraph(g1, edge.color="grey", layout=l1)
    }
}



#----------------------- Radard plots
# for 2 drugs
pddi_radar <- function(x, drug, level){
    
  level <- paste("label", level, ".atc.precipitant", sep="")
  x <- as.data.table(x)
  x<-as.data.frame(x[,.(n=mean(score.max, na.rm=T)), by=c("drug.id.object", level)])
print(x)
  v<- tidyr::spread(x, level, n)
  rownames(v) <- drug
  v <- v[,-1]
  v[is.na(v)] <- 0

    v2 = data.frame(rbind(rep(max(v), ncol(v)) , rep(-1, ncol(v)) , v))
   
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
    # import from library fmsb
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



