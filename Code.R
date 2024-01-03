################################### Projet ARS #################################
library(igraph)
library(docstring)
library(purrr) # pour la fonction map

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

# compute_quality<-function(n,g,C,B,S,mod){
#   # calcule la qualité d'une communité
#   # n est un sommet de S
#   res <- update(n,g,C,B,S)
#   C<-res$C
#   B<-res$B
#   S<-res$S
#   return(mod(g,C,B,S)) 
# }

#target = v[[1]]
#g = largest_component(wikipedia)
#mod_vector = c(mod_R,mod_M,mod_L)

local_com <- function(target,g,mod_vector){
  #initialisation
  if(is.igraph(g) && target %in% V(g)$wikiid){
    C<-c()
    B<-c(target)
    S<- c(V(g)[neighbors(g,id_from_wikiid(target,g),mode=mode)]$wikiid)
    Q<-0
    new_Q<-list(0,0)
    while((length(S)>0) && (new_Q[[2]]>=Q)){
      new_Q<-Q_generator(g,C,B,S,mod_vector)
      
      # QS<-sapply(S,compute_quality,g,C,B,S,mod)
      # new_Q<-max(QS)
      if(new_Q[[2]]>=Q){
        s_node<- new_Q[[1]]
        res<-update(s_node,g,C,B,S)
        C<-res$C
        B<-res$B
        S<-res$S
        Q<-new_Q[[2]]
        new_Q<-Q_generator(g,C,B,S,mod_vector)
      }
    }
    return(union(C,B))
  }
  else{
    stop("invalid arguments")
  }
}
#target = v[[1]]
#g = graph_edges_deleted

ego_partition<-function(target,g,mod_vector){
  res<-local_com(target,g,mod_vector)
  res_not_com<-V(g)[!(wikiid %in% res)]$wikiid
  return(list(res,res_not_com))
  
}

id_from_wikiid<-function(wikiid,g=wikipedia){
  return(which(V(g)$wikiid==wikiid))
}

wiki_deletion<-function(graph,percentage){ 
  #' Suppression de liens sur la composante connexe la plus grande
  #' du graphe
  #'
  #' wiki_deletion prend la composante connexe la plus grande du graphe fourni 
  #' et supprime le pourcentage d'arêtes fourni parmi cette composante
  #' 
  #' @param graph : Graphe auquel il faut supprimer des arêtes
  #' @param percentage : Pourcentage de suppression d'arêtes
  #' 
  #' @return Le graphe avec liens supprimés et la liste de listes (sommets
  #' ayant au moins une arête manquante,nombre d'arêtes manquantes pour le 
  #' sommet, sommets conservés dans la composante connexe la plus grande après
  #' suppression des arêtes)
  
  # Recherche de la plus grande composante connexe du graphe
  largestComp <- largest_component(graph)
  
  # Suppression de liens de façon aléatoire dans cette composante connexe
  graph_edges_deleted<-delete_edges(largestComp,sample(E(largestComp),percentage*length(E(largestComp))))
  
  # Récupération des sommets ayant au moins une arête retirée
  vertices_missing_link<-V(largestComp)$wikiid[degree(largestComp,mode="out")!=degree(graph_edges_deleted,mode="out")]
  
  # Liste des tuples sommets-liens manquants
  number_edges_removed_list<-c()
  for (vertex in vertices_missing_link){
    initial_degree<-degree(largestComp)[[id_from_wikiid(vertex,graph_edges_deleted)]]
    new_degree<-degree(graph_edges_deleted)[[id_from_wikiid(vertex,graph_edges_deleted)]]
    vertice_num_missing_links_tuple<-c(vertex,initial_degree-new_degree)
    
    number_edges_removed_list<-c(number_edges_removed_list,list(vertice_num_missing_links_tuple))
  }
  
  # Constitution de la liste des sommets ayant perdu un lien, mais restant dans
  # la plus grande composante connexe. 
  lg_comp_deleted_graph <-largest_component(graph_edges_deleted)
  
  list_edges_kept <- vertices_missing_link[vertices_missing_link %in% V(lg_comp_deleted_graph)$wikiid]
  
  return (list(graph_edges_deleted,number_edges_removed_list,list_edges_kept))
}




########################## Algorithme de vote de Borda #########################


Borda<-function(matrix_Q_values){
  # Liste de tous les classements
  listRankings<-c()
  
  # Comptage de Borda : On compte le nombre de sommets dont la modularité est 
  # inférieure au sommet considéré
  for(modularity in 1:length(matrix_Q_values[,1])){
    
    # liste de classement d'une modularité
    ranking_inside_modularity <-c()
    
    for(vertice in 1:length(matrix_Q_values[1,])){ # Pour chaque sommet
      count=0
      for(verticebis in 1:length(matrix_Q_values[1,])){ 
        if(matrix_Q_values[modularity,verticebis]<matrix_Q_values[modularity,vertice]){
          count=count+1
        }
      }
      ranking_inside_modularity<-c(ranking_inside_modularity,count)
    }
    listRankings<-c(listRankings,list(ranking_inside_modularity))
  }
  
  # À ce point, listRankings contient pour chaque fonction de modularité, 
  # pour chaque sommet, le nombre de sommets ayant une modularité inférieure au 
  # premier sommet.
  # Par exemple, listRankings[[1]] contient le classement des sommets pour la
  # première modularité
  
  bordaRanking=c()
  for(i in 1:length(listRankings[[1]])){ # Le nombre de sommets
    sum=0
    for(j in 1:length(listRankings)){# Nombre de modularités
      sum=sum+listRankings[[j]][i]
    }
    bordaRanking=c(bordaRanking,sum)
  }
  return(rank(bordaRanking,ties.method = 'random')) # l'indice du maximum de la liste correspond au 
  # meilleur sommet
}


####################### Fonction de modularité moyenne #########################


Q_generator <- function(g, C, B, S, mod_vector) {
  
  #' Fonction de modularité principale.
  #' 
  #' @param g : Graphe.
  #' @param C : Vecteur d'indices des sommets dans le coeur de communauté.
  #' @param B : Vecteur d'indices des sommets dans le bord de communauté.
  #' @param S : Vecteur d'indices des sommets dans le shell de communauté.
  #' @param mod_vector : Liste de modularités.
  #' 
  #' @return Le couple (sommet_optimal,Q(sommet_optimal)).
  
  n <- length(mod_vector)
  p <- length(S)
  
  # Valeur de chaque sommet
  # pour chaque modularité
  # plus une ligne pour les
  # indices (ou plutôt wikiid).
  Q_values <- matrix(rep(0,n*p),nrow=n)
  
  for (j in 1:p){
    
    # Génération de la communauté hypothétique
    # (si j était dans la communauté)
    res <- update(S[[j]],g,C,B,S)
    
    for (i in 1:n)
    {Q_values[i,j] <- mod_vector[[i]](g, res$C, res$B, res$S)}
  }
  
  # Classement par modularité

  vertices_ranking <- Borda(Q_values)
  best_vertice_index <-which.max(vertices_ranking)
  Q_average = mean(Q_values[,best_vertice_index])
  
  # Renvoie id sommet à ajouter et la qualité associée.
  return(list(S[[best_vertice_index]],Q_average))
}

########################## Recommendation de lien #################################
### LEs similarité doivent donner des matrices
####################### Similarite katz#################################################
# ENTREES: 
# g: le graphe
# order: ordre maximal. Tronqué au-delà.
# beta: paramètre d'horizon
# SORTIE: matrice de similarité
# (avec coeffs diagonaux à zéro)
katz_similarity <- function (g, order, beta){
  
  l <- vcount(g)
  
  search_matrix <- matrix(1:l, nrow = l, ncol = l)
  
  # Puissance k de la matrice d'adjacence.
  adj_M_k <- as_adjacency_matrix(g)
  
  search_dgcmatrix <- adj_M_k
  
  for (k in 2:order){
    
    adj_M_k <- adj_M_k%*%adj_M_k
    
    search_dgcmatrix <- search_dgcmatrix + (beta^k)*adj_M_k
    
  }
  
  for (i in 1:l){
    
    for (j in 1:l){
      
      if((i!=j)&(!are.connected(g,i,j))){
        search_matrix[i,j] <- search_dgcmatrix[i,j]
      }else{# Ignore les cycles
        search_matrix[i,j] <- 0
        
      }
    }
  }
  
  return(search_matrix)
  
}

### retourne les id ( et non pas les wki id) des sommets candidats
sommet_candidat<-function(communaute, sommet_cible, borda_liste){
  
  #' Fonction permettant de retirer les sommets non candidats de la liste issus de de la fonction Bordas_similarité
  #' 
  #' @param communaute : la communauté ego centre.
  #' @param  sommet_cible: le wikiid du sommet cible
  #' @param borda_liste: Liste des sommets de la communaute trie selon leur similarité avec le sommet cible avec la méthode Bordas.
  #
  #' 
  #' @return La liste des sommets candidats trie selon leur similarité avec le sommet cible avec la méthode Bordas.
  ensemble_sommet_voisin= neighbors(communaute, v = id_from_wikiid(sommet_cible, communaute))
  resultat <- setdiff(borda_liste,ensemble_sommet_voisin)
  return (resultat)
}

BORDA_ER=function(communaute, similarity, sommet_cible, sommet_candidat){
  l_ranking=c()
  a=0
  sommet_cible_id=id_from_wikiid(sommet_cible, communaute)
  for (si in similarity){
    if(si=="katz"){
      e= katz_similarity(communaute,order = 3,beta = 0.01)[sommet_cible_id, ]
      a=e
    }
    else{
    e=similarity(communaute,method =si)[sommet_cible_id, ] # pour avoir la similarité du sommet cible avec les autres sommets
    a=e
    }
    l=c()
    for (k in e){
      count=0
      for (t in e){
        if (k>t){
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



ajout_lien<- function(graph_rajout, communaute, similarities, sommet_cible, nb_ajout){ ## le sommet cible est le wiki_id du sommet
  ## nb-ajout pour le nombre de lien à rajouter 
  
  ### Bordas/similarite 
  # ON recupere la liste des sommets(leur id) de la communaute classé selon leur similarite avec le sommet cible
  Bordas=BORDA_ER(communaute, similarities, sommet_cible)
  commu_à_2_sommet=0
  ## Sommet candidat
  # on recupere la liste des sommets candidats en enlevant les sommets non candidats de Bordas (mais on garde l'ordre)
  L=sommet_candidat(communaute, sommet_cible, Bordas)
  ## ajout de lien
  if(length(L)!=1 ){
  for(k in 1:length(L)){
    if (k+1<=nb_ajout){
    graph_rajout <- add_edges(graph_rajout, c(id_from_wikiid(sommet_cible, graph_rajout), id_from_wikiid(V(communaute)$wikiid[L[k+1]], graph_rajout)))
  }}}
  else{
    commu_à_2_sommet=1
  }
  return(list(graph_rajout, commu_à_2_sommet))
  
}


################################ Test ##########################################
?Q_generator
?wiki_deletion

# Définition vecteur de modularité
mod_vector = c(mod_R,mod_M,mod_L)

# Test wiki_deletion 
x<-wiki_deletion(wikipedia, 0.01)

# Affectation du résultat de wiki deletion
graph_edges_deleted<-x[[1]]
graph_edges_deleted
vertices_missing_links<-x[[2]]
list_edges_kept<-x[[3]]
graph_edges_deleted
list_edges_kept
length(vertices_missing_links) # 755
length(list_edges_kept) # 702

vertices_missing_links[[1]][[1]] %in% list_edges_kept # TRUE : v appartient a la composante connexe la 
                            # plus grande après suppression des arêtes
v <- vertices_missing_links[[1]]
v[1]
# test de ego partition

com <- ego_partition(2695433,graph_edges_deleted,mod_vector)[[1]]
com

com_id <- flatten_int(map(com, id_from_wikiid, graph_edges_deleted))

katz_similarity(g = g1, order = 3,beta = 0.01)
g1 <-induced.subgraph(graph_edges_deleted,com_id)
g1
plot.igraph(g1)
V(g1)
similarities=c("jaccard","invlogweighted","katz")
graph_edges_deleted_add=ajout_lien(graph_rajout = graph_edges_deleted, communaute = g1, similarity =similarities,sommet_cible = v[[1]], nb_ajout = v[2])
g2=ajout_lien(graph_rajout = g1, communaute = g1, similarity =similarities,sommet_cible = v[[1]], nb_ajout = v[2])
g2[1]
plot.igraph(g2[[1]])
resume(graph_edges_deleted_add)

neighbors(graph_edges_deleted_add, v = id_from_wikiid(v[1], graph_edges_deleted_add),mode="out")$label
plot.igraph(g1_add)
id_from_wikiid(v[[1]],graph_edges_deleted)
V(graph_edges_deleted)[[52]]$label # Isopropyl nitrate

for(v in vertices_missing_links){
  print(ego_partition(v[[1]],graph_edges_deleted,mod_vector)[[1]])
}

################### Fonction Finale #########################################################
recommendation_de_lien_mesure=function(wikipedia, percentage){
  # Test wiki_deletion 
  largestComp <- largest_component(wikipedia)
  x<-wiki_deletion(wikipedia, percentage)
  # Affectation du résultat de wiki deletion
  graph_edges_deleted<-x[[1]]
  vertices_missing_links<-x[[2]]
  list_edges_kept<-x[[3]]
  #compte les sommet ayant une communaute de 2
  commu_à_2_sommets=0
  # on ajoute les lien au sommets présent dans la composante complexe la plus grande de x
  for (k in 1:length(vertices_missing_links)){
    print(k)
  if (vertices_missing_links[[k]][[1]] %in% list_edges_kept){
    v <- vertices_missing_links[[k]] #Sommet cible
    print(v)
    # communaute ego-centre
    com <- ego_partition(v[[1]],graph_edges_deleted,mod_vector)[[1]]
    com_id <- flatten_int(map(com, id_from_wikiid, graph_edges_deleted))
    gv <-induced.subgraph(graph_edges_deleted,com_id)# communaute ego-centre
    # on regarde 
    print(gv)
    #ajout lien
    resultat= ajout_lien(graph_rajout = graph_edges_deleted, communaute = gv, similarities =c("jaccard","invlogweighted","katz"),sommet_cible = v[[1]], nb_ajout = v[[2]])
    graph_edges_deleted=resultat[[1]]
    if(resultat[[2]]==1){
      commu_à_2_sommets=commu_à_2_sommets+1
    }
  }
    
    
  }
  print("commu_à_2_sommets=")
  print(commu_à_2_sommets)
  # Evaluation
  
  Accuracy=0
  for (k in 1:length(vertices_missing_links)){
    if (vertices_missing_links[[k]][[1]] %in% list_edges_kept){
      v <- vertices_missing_links[[k]][1]
      if (identical(sort(neighbors(graph_edges_deleted, v = id_from_wikiid(v, graph_edges_deleted), mode = 'out' )$label),neighbors(largestComp, v = id_from_wikiid(v,largestComp), mode = 'out' )$label)) {
        Accuracy=1+Accuracy
        }
    }
    
    
  }
  print("Accuracy sans sommet isole :\n")
  print(Accuracy/length(list_edges_kept))
  print("Accuracy avec sommet isole :\n")
  print(Accuracy/length(vertices_missing_links) )
}


recommendation_de_lien_mesure(wikipedia, 0.01)
## regarfr errrueur à cause du nb d'ajout 
