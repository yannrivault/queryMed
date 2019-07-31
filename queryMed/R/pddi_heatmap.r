#'  drug-drug interaction visualization
#'   
#'  This function proposes a heatmap representation of drug-drug interaction as listed in DIKB data base
#' 
#' @param data character object that specifies data base name
#' @param object character vector that specifies drug name(s)
#' @param level.precipitant numeric that states the interaction level number between choosen object and precipitants (between 1 and 5, from largest ATC level to precipitant drugs name)
#'  
#' @importFrom ggplot2 ggplot aes theme_minimal theme element_text scale_fill_gradient2 geom_tile geom_text xlab ylab
#' @importFrom plotly ggplotly
#'  
#' @return  A heatmap plot is returned.
#' @author S. Tessier
#'  
#' @seealso [DIKB] dataset
#' @export
#' @examples
#' data(DIKB)
#' pddi_heatmap(data = DIKB, object = "CLOPIDOGREL", level.precipitant = 4)
#' pddi_heatmap(data = DIKB, object = c("TADALAFIL","MORPHINE"), level.precipitant = 1)


pddi_heatmap <- function(data, object, level.precipitant){
  
  ifelse(level.precipitant==5, level <- 18, 
         ifelse(level.precipitant==4, level <- 24,
                ifelse(level.precipitant==3, level <- 22,
                       ifelse(level.precipitant==2, level <- 20,
                              ifelse(level.precipitant==1, level <- 16))))
  )
  
  work.data <- data[which(DIKB$object==object),c(17,level,10)]
  
  if(level!=18){
    if(length(object)>=2){
      
      for (i in 1:length(object)) {
        
        if(i == 1){
          
          a <- data[which(DIKB$object==object[1]),c(17,level,10)]
          
          a <- data.frame(object[1],
                          rownames(data.frame(tapply(a[,3], as.character(a[,2]), mean))),
                          data.frame(tapply(a[,3], as.character(a[,2]), mean))[,1])
          
          colnames(a) <- c("A","B","C")
          
          work.data <- a
          
        }else{
          
          a <- data[which(DIKB$object==object[i]),c(17,level,10)]
          
          a <- data.frame(object[i],
                          rownames(data.frame(tapply(a[,3], as.character(a[,2]), mean))),
                          data.frame(tapply(a[,3], as.character(a[,2]), mean))[,1])
          
          colnames(a) <- c("A","B","C")
          
          work.data <- rbind(work.data,a)
          
        }
        
      }
      
    }else{
      
      work.data <- data.frame(object,
                              rownames(data.frame(tapply(work.data[,3], as.character(work.data[,2]), mean))),
                              data.frame(tapply(work.data[,3], as.character(work.data[,2]), mean))[,1])
      
    } 
  }
  
  plot.score <- ggplot(data = work.data, aes(x=work.data[,1], y=work.data[,2], fill=work.data[,3])) +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 7, hjust = 1),
          axis.text.y = element_text(size = 7)) +
    scale_fill_gradient2(low = "darkolivegreen4", high = "firebrick4", mid = "darkorange3", 
                         midpoint = 2, limit = c(1,3), space = "Lab",
                         name="Precaution score") +
    geom_tile()
  
  plot.score <- plot.score +
    geom_text(aes(work.data[,1], work.data[,2], label = round(work.data[,3],3)), color = "white", size = 4) +
    xlab("Object") + ylab(colnames(data)[level])
  
  return(ggplotly(plot.score))
  # return(plot.score)
}
