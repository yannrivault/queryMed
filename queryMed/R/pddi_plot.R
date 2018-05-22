pddi_plot <- function(drug= "CLOPIDOGREL", type="name", direction="object", mypalette=NULL, weight=NULL){

# drug can be as a character be a character or a ATC code
if(direction=="object"){
  x <- switch(type,
         name = DIKB[DIKB$object==drug,],
         DBI = DIKB[DIKB$drug1==drug,],
         ATC = DIKB[DIKB$atc1==drug,])
  x = as.data.table(x)
  x$root  <- substr(x$atc2,1, 4)
  x<- x[!is.na(x$atc2),]
  statx = x[, .N,by=list(precipitant, object, root)]
}

if(direction=="precipitant"){
  x <- switch(type,
              name = DIKB[DIKB$precipitant==drug,],
              DBI = DIKB[DIKB$drug2==drug,],
              ATC = DIKB[DIKB$atc2==drug,])
  x = as.data.table(x)
  x$root  <- substr(x$atc1,1, 4)
  x<- x[!is.na(x$atc2),]
  statx = x[, .N,by=list(object, precipitant, root)]
}

if(direction=="both"){

  x <- switch(type,
              name = DIKB[DIKB$object==drug | DIKB$precipitant==drug,],
              DBI = DIKB[DIKB$drug1==drug |  DIKB$drug2==drug,],
              ATC = DIKB[DIKB$atc1==drug | DIKB$atc2==drug,])
  x$atc2root  <- substr(x$atc2,1, 4)
  x$atc1root  <- substr(x$atc1,1, 4)
  x<- x[!is.na(x$atc2) ,]
  x<- x[!is.na(x$atc1), ]
  print("ok")
  if(type=="atc"){
    statObj = as.data.frame(x[, .N,by=list(atc1, atc2, atc2root)])
    statPre = as.data.frame(x[, .N,by=list(atc2, atc1, atc1root)])
    print(tail(statObj))
  }
  else{
    statObj = as.data.frame(x[, .N,by=list(precipitant, object, atc2root)])
    statPre = as.data.frame(x[, .N,by=list(object, precipitant, atc1root)])
  }
  colnames(statObj) <- colnames(statPre) <- c("name","atc","root","N")
  statx = rbind(statObj, statPre)
  statx <- unique(statx)
  print(tail(statx))
}

if(is.null(mypalette) & type=="atc"){
  statx <- as.data.table(statx)
  mypalette<-brewer.pal(11,"Set3")
  mypalette = cbind(mypalette, "root"=unique(statx$root))
  statx <- merge(statx, mypalette, by="root")
}

  if(!is.null(weight)){
    statx<- statx[N>=weight]
  }

g1 <- graph_from_edgelist(as.matrix(cbind(drug, statx[,2])), directed=F)
statx$N[statx$N == 1 ] <- 0
E(g1)$weight <- statx$N         
E(g1)$width <- 1+E(g1)$weight*2
V(g1)$size <- 20
V(g1)$frame.color <- "white"
V(g1)$color <-  c("lightgrey", statx$mypalette)
l1 <- layout_as_star(g1)
plot(g1, edge.color="grey", layout=l1)
}





pddi_atc_plot <- function(){
  
  # retrieve parents and root
  x2 <- x 
  x2$atc2parent  <- substr(x$atc2,1, 5)
  x2$atc2root  <- substr(x$atc2,1, 4)
  
  x2$atc1parent  <- substr(x$atc1,1, 5)
  x2$atc1root  <- substr(x$atc1,1, 4)
  x2<- x2[!is.na(x2$atc2),]
  
  
# ancestor I
res1<-c()
res2<-c()
for (i in 1:nrow(x2)){
  temp<- get_ancestors(x2$atc2[i], ontology="ATC", api_key="e6f2d058-f206-4ac7-a8f9-60b84c9e57dc")
  res1[i] <- temp[1,2]
  res2[i] <- temp[2,2]
}
x2$anc1 <- res1 
x2$anc2 <- res2 


statx1 = x2[, .N, by=list(anc1,root, parent)]
nbdrug1 = x2[, length(unique(precipitant)), by=parent]

statx2 = x2[, .N, by=list(anc2,root)]
nbdrugx2 = x2[, .N, by=root]

colnames(nbdrug1) = c("parent","nbdrug")
colnames(nbdrugx2) = c("root","nbdrug")
statx1 = merge(statx1, nbdrug1, by="parent")
statx2 = merge(statx2, nbdrugx2, by="root")

statx1<- merge(statx1, mypalette, by="root")
statx2<- merge(statx2, mypalette, by="root")

g2 <- graph_from_edgelist(as.matrix(cbind(drug, statx1[,3])), directed=F ) 
statx1$N[statx1$N == 1 ] <- 0
E(g2)$weight <- statx1$N         
E(g2)$width <- 1+E(g2)$weight
V(g2)$size <- c(8, statx1$nbdrug)*6
V(g2)$vertex.label.cex <-2
V(g2)$frame.color <- "white"
V(g2)$color <-  c("lightgrey", statx1$mypalette)
#V(g2)$color <- "orange"
l2 <- layout_as_star(g2)
plot(g2, layout=l2)

# ancestor II
g3 <- graph_from_edgelist(as.matrix(cbind(drug, statx2[,2])), directed=F) 
statx2$N[statx2$N == 1 ] <- 0
E(g3)$weight <- statx2$N         
E(g3)$width <- 1+E(g3)$weight
w= c(15,20,15,20, rep(15,7))
V(g3)$size <- c(20, statx2$nbdrug+w)
V(g3)$vertex.label.cex <-0.5
V(g3)$frame.color <- "white"
V(g3)$color <-  c("lightgrey", statx2$mypalette)
l3 <- layout_as_star(g3)
plot(g3, vertex.label.cex= 0.8, layout=l3)
}