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

ego_ids_M
ego_ids_L
induced.subgraph(g1,ego_ids_R)
plot.igraph(induced.subgraph(g1,ego_ids_R))
E(g1)
?delete_edges

############################ Fonction eval ######################
ego_ids_R
gg= induced.subgraph(g1,ego_ids_R)
V(gg)[[1]]$label
sort(neighbors(graph = induced.subgraph(g1,ego_ids_R), v = 2, mode = 'out' )$label)
c(neighbors(graph = induced.subgraph(g1,ego_ids_R), v = 1, mode = 'out' )$label)
identical(sort(neighbors(graph = induced.subgraph(g1,ego_ids_R), v = 2, mode = 'out' )$label), neighbors(graph = induced.subgraph(g1,ego_ids_R), v = 2, mode = 'out' )$label)
?neighbors
?identical
#sommet_a_relier()
evaluation_test<-function(graphe_recommender, graphe_connexe_avant, sommet_relier, sommet_relier_avec_sommet_isole){
Accuracy=0
Accuracy_avec_sommet_isole=0
  for (k in sommet_relier_avec_sommet_isole){
    
    if (identical(sort(neighbors(graphe_recommender, v = id_from_wikiid(k, graphe_recommender), mode = 'out' )$label),neighbors(graphe_connexe_avant, v = id_from_wikiid(k,graphe_connexe_avant), mode = 'out' )$label)) {
      Accuracy_avec_sommet_isole=1+Accuracy_avec_sommet_isole
    }

    
  }

for (k in sommet_relier){
  
  if (identical(sort(neighbors(graphe_recommender, v = id_from_wikiid(k,graphe_recommender), mode = 'out' )$label), sort(neighbors(graphe_connexe_avant, v = id_from_wikiid(k, graphe_connexe_avant), mode = 'out' )$label))) {
    Accuracy=1+Accuracy
  }
  print("Accuracy sans sommet isole :\n")
  print(Accuracy/length(sommet_relier))
  print("Accuracy avec sommet isole :\n")
  print(Accuracy_avec_sommet_isole/length(sommet_relier_avec_sommet_isole))
  
}


  
  
}

########################################################### Ajout lien ###################################################
############################# Fonction sommet candidat ##############
ego_ids_R
gi=induced.subgraph(g1,ego_ids_R)
gi
unique(unlist(vertex_attr(gi, "wikiid")))
list(vertex_attr(gi, "wikiid"))
list.gr
V(gi)$wikiid
unique(unlist(neighbors(gi, v = id_from_wikiid(1448859,gi))))

list(neighbors(gi, v = id_from_wikiid(1448859,gi))$wikiid)


### retourne les id ( et non pas les wki id) des sommets candidats
sommet_candidat<-function(communaute, sommet_cible, borda_liste){
  ensemble_sommet_voisin= neighbors(communaute, v = id_from_wikiid(sommet_cible, communaute))
  ensemble_sommet_voisin
  resultat <- setdiff(borda_liste,ensemble_sommet_voisin)
  plot.igraph(gi, vertex.size=c, vertex.shapes= "rectangle" )
}
centrality_vis(gi, similarity.jaccard())

?similarity.jaccard



### LEs similarité doivent donner des matrices
BORDA_ER=function(communaute, similarity, sommet_cible, sommet_candidat){
  l_ranking=c()
  a=0
  sommet_cible_id=id_from_wikiid(sommet_cible, gi)
  for (si in similarity){
    e=similarity(communaute,method =si)[sommet_cible_id, ] # pour avoir la similarité du sommet cible avec les autres sommets
    a=similarity(communaute, method=si)[sommet_cible_id, ]
    l=c()
    for (k in e){
      count=0
      for (t in e){
        if (k>t){
          print(t)
          count=count+1
        }
      }
      l=c(l,count)
      }
      
    l_ranking=c(l_ranking,list(l))
    
  }
  
  L_rank=c()
  a=length(a)
  for (k in 1:a){
    L_sum=0
    for (l in l_ranking){
      L_sum=L_sum+ l[k]
    }
    L_rank=c(L_rank,L_sum)
  }
  
  return(order(L_rank, decreasing = TRUE)) # renvoie la position de l'indice 
}

oo=BORDA_ER(gi, similarity =('jaccard'), sommet_candidat = resultat, sommet_cible = 1448859)


############################################################################
######################### Ajout Lien 

ajout_lien<- function(graph_rajout, communaute, similarity, sommet_cible, nb_ajout){ ## le sommet cible est le wiki_id du sommet
  ## nb-ajout pour le nombre de lien à rajouter 
  
  ### Bordas/similarite 
  # ON recupere la liste des sommets(leur id) de la communaute classé selon leur similarite avec le sommet cible
  Bordas=BORDA_ER(communaute, similarity, sommet_cible)
  
  ## Sommet candidat
  # on recupere la liste des sommets candidats en enlevant les sommets non candidats de Bordas (mais on garde l'ordre)
  L=sommet_candidat(communaute, sommet_cible, Bordas)
  
  ## ajout de lien
  for(k in 1:nb_ajout){
    graph_rajout <- add_edges(graph_rajout, c(id_from_wikiid(sommet_cible, graph_rajout), id_from_wikiid(V(graph_rajout)$wikiid[L[k+1]], communaute)))
  }
  return(graph_rajout)
  
}

gii=ajout_lien(gi, gi,similarity =('jaccard'), 1448859, 1)
gii
gi=induced.subgraph(g1,ego_ids_R)
gi

plot.igraph(gi)
plot.igraph(gii)

##################################################################################################################################


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
}# creer dans la fonction la liste de sommet tire aleatoirement avec des conditions puis delete_edges 

degree_distribution(largest_component(wikipedia))*length(V(largest_component(wikipedia)))

largest<-largest_component(wikipedia)
V(largest)[degree(largest)==max(degree(largest,mode=mode))]$label
?degree

#sommet_a_relier()


