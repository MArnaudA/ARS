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
  print(transitivity(graph))
}

resume(wikipedia)
mode="out"
############################# Distribution de degrés ###########################

degreeDistRepres <- function(graph){
  d <- degree.distribution(graph)
  plot(d,type="l")
}

degreeDistRepres(wikipedia)

############################## Modularité R ####################################


mod_R <-function(g,C,B,S){
  #R = B_{in}/B_{out}
  #C,B,S sont trois sous ensembles disjoints de V(g), g est un graphe
  Bid=sapply(B, id_from_wikiid,g)
  Sid=sapply(S, id_from_wikiid,g)
  bin<-length(E(g)[Bid %->% Bid])
  bout<-length(E(g)[Bid %->% Sid])
  return (bin/(bin+bout))
}


################################# Modularité M #################################

mod_M<-function(g,C,B,S){
  #D = B union C
  #M = D_{in}/D_{out}
  D <- union(C,B)
  D_id <- sapply(D, id_from_wikiid,g)
  Sid=sapply(S, id_from_wikiid,g)
  din <- length(E(g)[D_id %->% D_id])
  dout <- length(E(g)[D_id %->% Sid])
  return(din/dout)
}

################################# Modularité L #################################

mod_L<-function(g,C,B,S){
  D<-union(C,B)
  D_id = sapply(D, id_from_wikiid, g)
  B_id = sapply(B, id_from_wikiid, g)
  S_id = sapply(S, id_from_wikiid, g)
  lin<-sum(sapply(D_id,neighbors_in,g,D_id))/length(D_id)
  lout <-sum(sapply(B_id,neighbors_in,g,S_id))/length(B_id)
  return(lin/lout)
}
sum(c(1,2,3))

neighbors_in<-function(n,g,E){
  #return the numbrer of neighbor of node n in set E in graph g
  return(length(intersect(neighbors(g,n,mode=mode),E)))
  
}
########### Fonctions de calcul de la communauté égo-centrée ###################

update <- function(n,g,C,B,S){
  # move n in S to D
  S<- S[S!=n]
  D<- union(C,B)
  if(all(neighbors(g,id_from_wikiid(n,g),mode=mode)$wikiid %in% D)){
    # add n to C
    C <- union(C,n)
    
  }
  else{
    #add n to B
    B<-union(B,n)
    new_s=setdiff(neighbors(g,id_from_wikiid(n,g),mode=mode)$wikiid,union(D,S))
    if(length(new_s)>0){
      S<-union(S,new_s)
    }
    for(b in B){
      if(all(neighbors(g,id_from_wikiid(b,g),mode=mode)$wikiid %in% D)){
        B<-B[B!=b]
        C<-union(C,b)
      }
    }
  }
  return(list(C=C,B=B,S=S))
} 
n=1997
compute_quality<-function(n,g,C,B,S,mod){
  # calcule la qualité d'une communité
  # n est un sommet de S
  res <- update(n,g,C,B,S)
  C<-res$C
  B<-res$B
  S<-res$S
  return(mod(g,C,B,S))
}

local_com <- function(target,g,mod){
  #initialisation
  if(is.igraph(g) && target %in% V(g)$wikiid){
    C<-c()
    B<-c(target)
    S<- c(V(g)[neighbors(g,id_from_wikiid(target,g),mode=mode)]$wikiid)
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
target=V(g1)[[10]]$wikiid
g=largest
mod=mod_L
ego_partition<-function(target,g,mod){
  res<-local_com(target,g,mod)
  res_not_com<-V(g)[!(wikiid %in% res)]$wikiid
  return(list(res,res_not_com))
  
}

id_from_wikiid<-function(wikiid,g){
  return(which(V(g)$wikiid==wikiid))
}

################################ Test ##########################################


# Plus grande composante connexe de g
g1<-largest_component(wikipedia)


# Communauté égo-centrée avec sommet cible = 10
mode="out"

V(g1)[[180]]$label # Director telephone system
egoR<-ego_partition(V(g1)[[180]]$wikiid,g1,mod_R) # 
egoM<-ego_partition(V(g1)[[180]]$wikiid,g1,mod_M)
egoL<-ego_partition(V(g1)[[180]]$wikiid,g1,mod_L)
ego_ids_R<-sapply(egoR[[1]],id_from_wikiid,g1)
ego_ids_M<-sapply(egoM[[1]],id_from_wikiid,g1)
ego_ids_L<-sapply(egoL[[1]],id_from_wikiid,g1)
ego_ids_R
ego_ids_M
ego_ids_L

plot.igraph(induced.subgraph(g1,ego_ids_R))
E(g1)
?delete_edges

V(g1)[[1]]$label # Homochronous

neighbors(g1,1,mode=mode)

neighbors(g1,id_from_wikiid(V(g1)[[1]]$wikiid,wikipedia),mode=mode)$wikiid


id_from_wikiid(V(g1)[[1]]$wikiid,g)
E(wikipedia)[1:10]


neighbors(g1,id_from_wikiid(59603,g),mode=mode)$wikiid

train=g1
test=delete_edges(train,sample(E(train),10))
length(E(train))
length(E(test))

wiki_deletion<-function(graph,percentage){ 
  
  # Prend la composante connexe la plus grande et supprime un pourcentage d'arêtes parmi cette composante
  
  largestComp <- largest_component(graph)
  graph_edges_deleted<-delete_edges(largestComp,sample(E(largestComp),percentage*length(E(g))))
  graph_edges_deleted
}

degree_distribution(largest_component(wikipedia))*length(V(largest_component(wikipedia)))

largest<-largest_component(wikipedia)
V(largest)[degree(largest)==max(degree(largest,mode=mode))]$label
?degree


