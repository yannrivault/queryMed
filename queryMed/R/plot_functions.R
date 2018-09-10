#'  Potential drug-drug interaction graph
#'   
#'  This function proposes a graph representation of potential drug-drug interaction as listed in DIKB data base.
#' 
#' @param drug character vector that specifies one drug name or one Drug Bank Identifier (<https://www.drugbank.ca/>) (DBI) or one Anatomical Therapeutic Chemical compound's code (<http://www.who.int/classifications/atcddd/en/>) (ATC)
#' @param type   character vector that specifies whether drug is a drug name (default), DBI or ATC code.
#' @param direction character vector that specifies whether the queried drug is an object or an precipitant as defined by DIKB 
#' @param nbinteractions numeric that states the minimun number of sources listing the interaction or the number of time the interactions have been listed by a source.
#' @param source character vector that specifies the original source of the potential drug-drug interaction. It could be CredibleMeds, DDI-Corpus-2011, DDI-Corpus-2013, Drugbank, FrenchDB,  HEP,  HIV,   NDF-RT, NLM-Corpus, ONC-HighPriority, ONC-NonInteruptive, OSCAR,  PK-Corpus, World-Vista, Kegg (See <https://github.com/dbmi-pitt/public-PDDI-analysis> for more details). 
#' @param contraindication If true filter on the contraindication listed in the Kegg data source.
#' @param plot Logical whether or not to display the plot. If False the function return the data.frame used to build the plot.  
#' @param level numeric ranging from 1 to 5 that indicates the ATC level to use to display the potential interacting drugs with the drug of interest.  
#' @param mypalette character vector that specifies color of the nodes. If null the default is defined using brewer.pal and colorRampPalette from RBrewerColor and grDevices, respectively.
#' @param weight numeric defining a threshold to filter out graph nodes and edges when the potential drug-drug interaction are listed less than  that threshold by different sources.
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom igraph graph_from_edgelist layout_as_star V E "E<-" "V<-"
#' @importFrom RColorBrewer brewer.pal
#' @importFrom utils data
#' @importFrom data.table as.data.table .N
#' 
#' @details the graph is a star graph centered on the drug of interest. The color of the nodes depends on the ATC level specified in the function cal; grouping molecules by ATC classes. The thickness of the edges depends on the number of times the  potential drug-drug interaction is listed by different sources.
#'  
#' @return  If the argument graph is set to TRUE a graph is plotted and the associated data.frame is returned. When the graph argument is set to FALSE anly the data.frame is returned.
#' @author N. Le Meur
#'  
#' @seealso [DIKB] dataset and igraph package
#' @export
#' @examples
#' pddi_plot("CLOPIDOGREL", "name", level=4)
#' pddi_plot("CLOPIDOGREL", "name", level=5, nbinteractions=2)


pddi_plot <- function(drug, type="name", direction="object", 
                      nbinteractions= 1, source=NULL, contraindication= NULL, 
                      plot=TRUE, level=4, mypalette=NULL, weight=NULL){

  DIKB<- utils::data(DIKB)
  ATC <- utils::data(ATC)  
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
    statx <- x[, .N, by=list(precipitant, atc2, drug2)]
  }
  if(direction=="precipitant"){
    statx <- x[, .N, by=list(object, atc1, drug1)]
  }
  statx <- data.frame(statx)
# filter on number of sources stating the interaction
  statx <- statx[statx$N >= nbinteractions, ]
  names(statx) <-   c("interact", "atc5", "dbi", "n")
  if(level==5){
    statx <- merge(statx, ATC[, c("atc5", lab, levm1)], by="atc5")
  }else{
    statx <- merge(statx, ATC[, c("atc5", lev, lab, levm1)], by="atc5")
  }


  if(plot==TRUE){

    statx <- as.data.table(statx)
    statx<-statx[,.(n=sum(n)), by=c(lab, levm1)]
    statx <- data.frame(statx)

    root <- unique(statx[, levm1])
    n <- length(root)
      #mypalette<- colorRampPalette(brewer.pal(12,"Set3"))(n)
      #mypalette<- colorRampPalette(brewer.pal(9,"YlOrRd"))(n)
    if(is.null(mypalette)){
      mypalette <- colorRampPalette(brewer.pal(11,"Spectral"))(n)
    }
    mypalette <- cbind(mypalette, "root"= as.character(root))
    statx <- merge(statx, mypalette, by.x=levm1, by.y="root")
    if(!is.null(weight)){
      statx <- statx[statx$n>=weight, ]
    }
    if(is.null(nrow(x))) {
      return(warning("No data found"))
    }

    g1 <- graph_from_edgelist(as.matrix(cbind(drug, as.character(statx[,2]))), directed=F)
    statx$n[statx$n == 1 ] <- 0
    #g1 <- set_edge_attr(g1, weight, E(g1),  statx$n)
    E(g1)$weight <- statx$n
    E(g1)$width <- 1+E(g1)$weight*2
    V(g1)$size <- 20
    V(g1)$frame.color <- "white"
    if(level==5 & nbinteractions==1){
      V(g1)$color <-  c(as.character(statx$mypalette))
    }
    else{
      V(g1)$color <-  c("#FFFFCC", as.character(statx$mypalette))
    }
    l1 <- layout_as_star(g1)
    plot(g1, edge.color="grey", layout=l1)
  }
  return(statx)
}


