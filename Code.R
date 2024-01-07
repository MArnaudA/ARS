################################### Projet ARS #################################
library(igraph)
<<<<<<< Updated upstream
library(docstring)

wikipedia<-read.graph("wikipedia.gml",format="gml")
V(wikipedia)[[2]]$wikiid
=======

g<-read.graph("wikipedia.gml",format="gml")
plot(g,vertex.label=NA,vertex.size=0.1,arrows.size=0.1)

>>>>>>> Stashed changes

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

<<<<<<< Updated upstream
degreeDistRepres(wikipedia)

############################## Modularité R ####################################
=======
degreeDistRepres(g)
>>>>>>> Stashed changes

clusters(g)

mod_R <-function(g,C,B,S){
  #R = B_{in}/B_{out}
  #C,B,S sont trois sous ensembles disjoints de V(g), g est un graphe
<<<<<<< Updated upstream
  Bid=sapply(B, id_from_wikiid,g)
  Sid=sapply(S, id_from_wikiid,g)
  bin<-length(E(g)[Bid %->% Bid])
  bout<-length(E(g)[Bid %->% Sid])
=======
  bin<-length(E(g)[B %--% B])
  bout<-length(E(g)[B %--% S])
>>>>>>> Stashed changes
  return (bin/(bin+bout))
}
#Exemple du cours
g<-graph.empty(directed=FALSE)
g<-add.vertices(g,6)
g<-add.edges(g,c(1,2,2,3,2,4,4,5,4,6,5,6))
plot(g)
C=c()
B=c(4,5)
S=c(2,6)
mod_R(g,C,B,S)

#2
mod_M<-function(g,C,B,S){
  #D = B union C
  #M = D_{in}/D_{out}
  D <- union(C,B)
  din <- length(E(g)[D %--% D])
  dout <- length(E(g)[B %--% S])
  return(din/dout)
}

#3
mod_L<-function(g,C,B,S){
  D<-union(C,B)
  lin<-sum(sapply(D,neighbors_in,g,D))/length(D)
  lout <-sum(sapply(B,neighbors_in,g,S))/length(B)
  return(lin/lout)
}
<<<<<<< Updated upstream

neighbors_in<-function(n,g,E){
  #return the numbrer of neighbor of node n in set E in graph g
  return(length(intersect(neighbors(g,n,mode=mode),E)))
  
}
########### Fonctions de calcul de la communauté égo-centrée ###################

=======
#4
>>>>>>> Stashed changes
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
<<<<<<< Updated upstream
  if(is.igraph(g) && target %in% V(g)$wikiid){
    C<-c()
    B<-c(target)
    S<- c(V(g)[neighbors(g,id_from_wikiid(target,g),mode=mode)]$wikiid)
=======
  if(is.igraph(g) && target %in% V(g)){
    C<-c()
    B<-c(target)
    S<- c(V(g)[neighbors(g,target)]$id)
>>>>>>> Stashed changes
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

<<<<<<< Updated upstream
ego_partition<-function(target,g,mod){
  res<-local_com(target,g,mod)
  res_not_com<-V(g)[!(wikiid %in% res)]$wikiid
  return(list(res,res_not_com))
  
}

id_from_wikiid<-function(wikiid,g=wikipedia){
  return(which(V(g)$wikiid==wikiid))
=======
#5

ego_partition<-function(target,g,mod){
  res<-local_com(target,g,mod)
  res_not_com<-V(g)[!(id %in% res)]$id
  return(list(res,res_not_com))
>>>>>>> Stashed changes
}
<<<<<<< Updated upstream
g=dolphins
target=1
ego_partition(1,g,mod_R)
=======


wiki_deletion<-function(graph,percentage=0.01){ 
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
<<<<<<< Updated upstream
  vertices_missing_link<-V(largestComp)[degree(largestComp,mode="out")!=degree(graph_edges_deleted,mode="out")]
=======
  vertices_missing_link<-V(largestComp)$wikiid[degree(largestComp,mode="out")!=degree(graph_edges_deleted,mode="out")]
>>>>>>> Stashed changes
  
  # Liste des tuples sommets-liens manquants
  number_edges_removed_list<-c()
  for (vertex in vertices_missing_link){
<<<<<<< Updated upstream
    initial_degree<-degree(largestComp)[[vertex]]
    new_degree<-degree(graph_edges_deleted)[[vertex]]
=======
    initial_degree<-degree(largestComp)[[id_from_wikiid(vertex,graph_edges_deleted)]]
    new_degree<-degree(graph_edges_deleted)[[id_from_wikiid(vertex,graph_edges_deleted)]]
>>>>>>> Stashed changes
    vertice_num_missing_links_tuple<-c(vertex,initial_degree-new_degree)
    
    number_edges_removed_list<-c(number_edges_removed_list,list(vertice_num_missing_links_tuple))
  }
  
  # Constitution de la liste des sommets ayant perdu un lien, mais restant dans
  # la plus grande composante connexe. 
  lg_comp_deleted_graph <-largest_component(graph_edges_deleted)
<<<<<<< Updated upstream
  list_edges_kept <- vertices_missing_links %in% V(lg_comp_deleted_graph)
=======
>>>>>>> Stashed changes
  
  
<<<<<<< Updated upstream
  
=======
>>>>>>> Stashed changes
  return (list(graph_edges_deleted,number_edges_removed_list,list_edges_kept))
}

################################ Test ##########################################

<<<<<<< Updated upstream
?wiki_deletion

g1<-largest_component(wikipedia)
x<-wiki_deletion(wikipedia)
graph_edges_deleted<-x[[1]]
vertices_missing_links<-x[[2]]

clusters(graph_edges_deleted)
length(vertices_missing_links)

lg_comp_deleted_graph <-largest_component(graph_edges_deleted)
list <- vertices_missing_links %in% V(lg_comp_deleted_graph)
length(list)
=======


########################## Algorithme de vote de Borda #########################
>>>>>>> Stashed changes


g1=largest_component(wikipedia)
mode="out"




V(g1)[[180]]$label # Art gallery problem
egoR<-ego_partition(V(g1)[[180]]$wikiid,g1,mod_R)
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

<<<<<<< Updated upstream
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
=======

####################### Fonction de modularité moyenne #########################
>>>>>>> Stashed changes

list(neighbors(gi, v = id_from_wikiid(1448859,gi))$wikiid)

<<<<<<< Updated upstream
=======
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
>>>>>>> Stashed changes

### retourne les id ( et non pas les wki id) des sommets candidats
sommet_candidat<-function(communaute, sommet_cible, borda_liste){
  ensemble_sommet_voisin= neighbors(communaute, v = id_from_wikiid(sommet_cible, communaute))
  ensemble_sommet_voisin
  resultat <- setdiff(borda_liste,ensemble_sommet_voisin)
  plot.igraph(gi, vertex.size=c, vertex.shapes= "rectangle" )
}
centrality_vis(gi, similarity.jaccard())

<<<<<<< Updated upstream
?similarity.jaccard



### LEs similarité doivent donner des matrices
BORDA_ER=function(communaute, similarity, sommet_cible, sommet_candidat){
=======
BORDA_ER=function(communaute, similarity, sommet_cible){
  #' Fonction permettant faire un classement de Borda de la similarité du sommet cible avec les sommets de la communauté
  #' 
  #' @param communaute : la communauté ego centre.
  #' @param  sommet_cible: le wikiid du sommet cible
  #' @param similarity: Liste des similarité utilisées 
  #
  #' 
  #' @return La liste des sommets trie selon leur similarité avec le sommet cible avec la méthode Bordas.
>>>>>>> Stashed changes
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


<<<<<<< Updated upstream
############################################################################
######################### Ajout Lien 

ajout_lien<- function(graph_rajout, communaute, similarity, sommet_cible, nb_ajout){ ## le sommet cible est le wiki_id du sommet
  ## nb-ajout pour le nombre de lien à rajouter 
  
=======
ajout_lien<- function(graph_rajout, communaute, similarities, sommet_cible, nb_ajout){ ## le sommet cible est le wiki_id du sommet
  #' Fonction finale permettant l'ajout du ou des liens au sommet cible en utilisant la fonction bordas_er et sommet candidat
  #' 
  #' @param graph_rajout : le graphe sur lequelle les liens sont ajoutés
  #' @param communaute : communaute egie centre du sommet cible
  #' @param  sommet_cible: le wikiid du sommet cible
  #' @param similarity: Liste des similarité utilisées 
  #' @param nb_ajout: Nombre d'ajout à effectuer sur le sommet cible
  #' 
  #' @return le grpahe rajout avec les liens ajoutés, le nombre de sommet auquelle l'ajout de leins n'a pas pas avoir lieu
>>>>>>> Stashed changes
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

<<<<<<< Updated upstream
plot.igraph(gi)
plot.igraph(gii)

##################################################################################################################################


V(g1)[[1]]$label # Homochronous

neighbors(g1,1,mode=mode)

neighbors(g1,id_from_wikiid(V(g1)[[1]]$wikiid,wikipedia),mode=mode)$wikiid


id_from_wikiid(V(g1)[[1]]$wikiid,g)
E(wikipedia)[1:10]
=======
################################ Test ##########################################
?Q_generator
?wiki_deletion
?similarity
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

vertices_missing_links[[2]][[1]] %in% list_edges_kept # TRUE : v appartient a la composante connexe la 
                            # plus grande après suppression des arêtes
v <- vertices_missing_links[[2]]
v[1]
# test de ego partition

com <- ego_partition(v[1],graph_edges_deleted,mod_vector)[[1]]

com_id <- flatten_int(map(com, id_from_wikiid, graph_edges_deleted))

katz_similarity(g = g1, order = 3,beta = 0.01)
g1 <-induced.subgraph(graph_edges_deleted,com_id)
resume(g1)
plot.igraph(g1)
V(g1)
similarities=c("jaccard","invlogweighted","katz")
graph_edges_deleted_add=ajout_lien(graph_rajout = graph_edges_deleted, communaute = g1, similarity =similarities,sommet_cible = v[[1]], nb_ajout = v[2])
g2=ajout_lien(g1, communaute = g1, similarities =similarities,sommet_cible = v[[1]], nb_ajout = v[2])
resume(g2[[1]])
resume(g1)
g2[1]
plot.igraph(g2[[1]])
resume(graph_edges_deleted_add)
>>>>>>> Stashed changes


neighbors(g1,id_from_wikiid(59603,g),mode=mode)$wikiid

<<<<<<< Updated upstream
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
=======



=======
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
  
  graphe_rajout=graph_edges_deleted
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
    resultat= ajout_lien(graph_rajout =graphe_rajout , communaute = gv, similarities =c("jaccard","invlogweighted","katz"),sommet_cible = v[[1]], nb_ajout = v[[2]])
    graphe_rajout=resultat[[1]]
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
      if (identical(sort(neighbors(graphe_rajout, v = id_from_wikiid(v, graph_edges_deleted), mode = 'out' )$label),neighbors(largestComp, v = id_from_wikiid(v,largestComp), mode = 'out' )$label)) {
        Accuracy=1+Accuracy
        }
    }
    
    
  }
  print("Accuracy sans sommet isole :\n")
  print(Accuracy/length(list_edges_kept))
  print("Accuracy avec sommet isole :\n")
  print(Accuracy/length(vertices_missing_links) )
  
  return (c(Accuracy/length(list_edges_kept),Accuracy/length(vertices_missing_links) ))
}




recommendation_de_lien_mesure(wikipedia, 0.01)

###################### Graphique Erreur
L_accuracy=c()
L_accuracy_sommet_isole=c()
for(k in 1:50:1){
  k=k/100
  Accuracy=0
  Accuracy_sommet_isole=0
  for (j in 1:5){
    Accuracy=Accuracy+recommendation_de_lien_mesure(wikipedia, k)[[1]]
    Accuracy_sommet_isole=Accuracy_sommet_isole+recommendation_de_lien_mesure(wikipedia, k)[[2]]
  }
  L_accuracy=c(L_accuracy, Accuracy/5)
  L_accuracy_sommet_isole=c(L_accuracy_sommet_isole, Accuracy_sommet_isole/5)
}
library(ggplot2)
#test
x <- seq(1, 5, by = 1)
L_accuracy=c(0.1,0.3,0.5,0.12,0.14)
L_accuracy_sommet_isole=c(0.05,0.10,0.47,0.15,0.80)

data <- data.frame(x, L_accuracy, L_accuracy_sommet_isole)

ggplot(data, aes(x = x)) +
  geom_line(aes(y = L_accuracy, color = "Accuracy"), size = 1) +
  geom_line(aes(y = L_accuracy_sommet_isole, color = "Accuracy avec sommet isole"), size = 1) +
  labs(title = "Graphique de la précision avec sommet isolé et sans en fonction de x",
       x = "Pourcentage de liens supprimés (en %)",
       y = "Précision") +
  scale_color_manual(values = c("Accuracy" = "blue", "Accuracy avec sommet isole" = "red")) +
  theme_minimal()



>>>>>>> Stashed changes
