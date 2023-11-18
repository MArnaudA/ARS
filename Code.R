################################### Projet ARS #################################
library(igraph)

g<-read.graph("wikipedia.gml",format="gml")


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

resume(g)

############################# Distribution de degrés ###########################

degreeDistRepres <- function(graph){
  d <- degree.distribution(graph)
  plot(d,type="l")
}

degreeDistRepres(g)

############################## Modularité R ####################################

mod_R <-function(g,C,B,S){
  #R = B_{in}/B_{out}
  #C,B,S sont trois sous ensembles disjoints de V(g), g est un graphe
  bin<-length(E(g)[B %--% B])
  bout<-length(E(g)[B %--% S])
  return (bin/(bin+bout))
}

################ Application modularité R à l'exemple du cours #################

g<-graph.empty(directed=FALSE)
g<-add.vertices(g,6)
g<-add.edges(g,c(1,2,2,3,2,4,4,5,4,6,5,6))
plot(g)
C=c()
B=c(4,5)
S=c(2,6)
mod_R(g,C,B,S)

################################# Modularité M #################################

mod_M<-function(g,C,B,S){
  #D = B union C
  #M = D_{in}/D_{out}
  D <- union(C,B)
  din <- length(E(g)[D %--% D])
  dout <- length(E(g)[B %--% S])
  return(din/dout)
}

################################# Modularité L #################################

mod_L<-function(g,C,B,S){
  D<-union(C,B)
  lin<-sum(sapply(D,neighbors_in,g,D))/length(D)
  lout <-sum(sapply(B,neighbors_in,g,S))/length(B)
  return(lin/lout)
}

########### Fonctions de calcul de la communauté égo-centrée ###################

update <- function(n,g,C,B,S){
  # move n in S to D
  S<- S[S!=n]
  D<- union(C,B)
  if(all(neighbors(g,n) %in% D)){
    # add n to C
    C <- union(C,n)
  }
  else{
    #add n to B
    B<-union(B,n)
    new_s=setdiff(neighbors(g,n),union(D,S))
    if(length(new_s)>0){
      S<-union(S,new_s)
    }
    for(b in B){
      if(all(neighbors(g,b)%in% D)){
        B<-B[B!=b]
        C<-union(C,b)
      }
    }
  }
  return(list(C=C,B=B,S=S))
} 

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
  if(is.igraph(g) && target %in% V(g)){
    C<-c()
    B<-c(target)
    S<- c(V(g)[neighbors(g,target)]$id)
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
  res_not_com<-V(g)[!(id %in% res)]$id
  return(list(res,res_not_com))
}

################################ Test ##########################################


# Plus grande composante connexe de g
g1<-largest_component(g)

# Communauté égo-centrée avec sommet cible = 1
ego<-ego_partition(1,g1,mod_R)
ego

V(g1)[[1]]$label # Homochonous

neighbors(g1,1,mode="total")

x<-induced_subgraph(g1,ego[[1]])
x
plot(x)

