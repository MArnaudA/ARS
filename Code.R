################################### Projet ARS #################################
library(igraph)

wikipedia<-read.graph("wikipedia.gml",format="gml")
V(wikipedia)[[2]]$wikiid

################################ Fonction Résumé ###############################

resume <- function(graph){
  print("vertices : ")
  print(vcount(graph))
  print("edges : ")
  print(ecount(graph))
  print("density : ")
  print(graph.density(graph))
  print("transitivity : ")
  print(transitivity(g))
}

resume(wikipedia)

############################# Distribution de degrés ###########################

degreeDistRepres <- function(graph){
  d <- degree.distribution(graph)
  plot(d,type="l")
}

degreeDistRepres(g)

############################## Modularité R ####################################
neighbors(g1,1,mode="out")$wikiid

mod_R <-function(g,C,B,S){
  #R = B_{in}/B_{out}
  #C,B,S sont trois sous ensembles disjoints de V(g), g est un graphe
  Bid=sapply(B, id_from_wikiid,g)
  Sid=sapply(S, id_from_wikiid,g)
  bin<-length(E(g)[Bid %->% Bid])
  bout<-length(E(g)[Bid %->% Sid])
  return (bin/(bin+bout))
}
id_from_wikiid(16090,g)

################################# Modularité M #################################

mod_M<-function(g,C,B,S){
  #D = B union C
  #M = D_{in}/D_{out}
  D <- union(C,B)
  din <- length(E(g)[D %--% D])
  dout <- length(E(g)[D %--% S])
  return(din/dout)
}

################################# Modularité L #################################

mod_L<-function(g,C,B,S){
  D<-union(C,B)
  lin<-sum(sapply(D,neighbors_in,g,D))/length(D)
  lout <-sum(sapply(B,neighbors_in,g,S))/length(B)
  return(lin/lout)
}


neighbors_in<-function(n,g,E){
  #return the numbrer of neighbor of node n in set Ein graph g
  return(length(intersect(neighbors(g,n),E)))
  
}
########### Fonctions de calcul de la communauté égo-centrée ###################

update <- function(n,g,C,B,S){
  # move n in S to D
  S<- S[S!=n]
  D<- union(C,B)
  if(all(neighbors(g,id_from_wikiid(n,g),mode="out")$wikiid %in% D)){
    # add n to C
    C <- union(C,n)
    
  }
  else{
    #add n to B
    B<-union(B,n)
    new_s=setdiff(neighbors(g,id_from_wikiid(n,g),mode="out")$wikiid,union(D,S))
    if(length(new_s)>0){
      S<-union(S,new_s)
    }
    for(b in B){
      if(all(neighbors(g,id_from_wikiid(b,g),mode="out")$wikiid %in% D)){
        B<-B[B!=b]
        C<-union(C,b)
      }
    }
  }
  return(list(C=C,B=B,S=S))
} 

n=S[[1]]
neighbors(g1,id_from_wikiid(426845,g1),mode="out")$wikiid # pas de voisin

compute_quality<-function(n,g,C,B,S,mod){
  # calcule la qualité d'une communité
  # n est un sommet de S
  res <- update(n,g,C,B,S)
  C<-res$C
  B<-res$B
  S<-res$S
  return(mod(g,C,B,S))
}
target=V(g1)[[1]]$wikiid
mod=mod_R
g=g1

local_com <- function(target,g,mod){
  #initialisation
  if(is.igraph(g) && target %in% V(g)$wikiid){
    C<-c()
    B<-c(target)
    S<- c(V(g)[neighbors(g,id_from_wikiid(target,g),mode="out")]$wikiid)
    Q<-0
    new_Q<-0
    while((length(S)>0) && (new_Q>=Q)){
      QS<-sapply(S,compute_quality,g,C,B,S,mod)
      new_Q<-max(QS)
      if(new_Q>=Q){
        s_node<-S[which.max(QS)]
        res<-update(s_node,g,C,B,S)
        C<-res$C
        B<-res$B
        S<-res$S
        Q<-new_Q
      }
    }
    return(union(C,B))
  }
  else{
    stop("invalid arguments")
  }
}

ego_partition<-function(target,g,mod){
  res<-local_com(target,g,mod)
  res_not_com<-V(g)[!(wikiid %in% res)]$wikiid
  return(list(res,res_not_com))
}

################################ Test ##########################################


# Plus grande composante connexe de g
g1<-largest_component(wikipedia)


# Communauté égo-centrée avec sommet cible = 1

ego<-ego_partition(V(g1)[[10]]$wikiid,g1,mod_R)
V(g1)[[10]]$label
ego_ids<-sapply(ego[[1]],id_from_wikiid,g1)

plot.igraph(induced.subgraph(g1,ego_ids))

V(g1)[[1]]$label # Homochronous

neighbors(g1,1,mode="out")

neighbors(g1,id_from_wikiid(V(g1)[[1]]$wikiid,g),mode="out")$wikiid

id_from_wikiid<-function(wikiid,g){
  return(which(V(g)$wikiid==wikiid))
}

id_from_wikiid(V(g1)[[1]]$wikiid,g)
E(wikipedia)[1:10]


neighbors(g1,id_from_wikiid(59603,g),mode="out")$wikiid


