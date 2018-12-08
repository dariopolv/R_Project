pre_set <- function() {
  print("Pre-set della matrice iniziale. ")
  n <- readline("Inserisci il numero di vertici della matrice: ")
  if(check_fun(n) == FALSE) {
    print("Inserire solo valori numerici. Riavvio programma inserimento. ")
    pre_set()
  }
  else {
    x <<- as.numeric(n)
    v <<- c()
    for(i in 1:x) {
      v[i] <<- readline("Inserisci il nome del vertice: ")
      if(controllo_lettere(v[i]) == FALSE) {
        print("I nomi del vertice devono essere lettere. Riavvio della funzione... ")
        v <<- c()
        pre_set()
        break()
      }
    }
    a <<- matrix(nrow = x, ncol = x, dimnames = list(v,v))
  }
  return(a)
}

inserimento_da_tastiera <- function() {
  print("Nota: L'inserimento di valori non numerici riavvierà il programma di inserimento dall'inizio.")
  for (i in 1:x) {
    for (j in 1:x) {
      arco <<- readline(cat("inserisci arco DA:", v[i], " A:", v[j]))
      if(check_fun(arco) == FALSE){
        print("Valore inserito non numerico. Riavvio del programma di inserimento...")
        return(FALSE)
      } 
      else {
        arco <<- as.numeric(arco)
        a[i,j] <<- arco
      }
    }
  }
  return(TRUE)
}

inserimento_da_file <- function() {
  print("Nota: I dati nel file di testo devono essere separati da: ',' ")
  file <- file.choose()
  a <<- read.table(file, sep = ",", row.names = v, col.names = v )
  a <<- as.matrix(a)
  return(a)
}

start <- function() {
  matrice_iniziale <- 10
  while (matrice_iniziale != 0) {
    print("Scegliere il metodo di inserimento dati nella matrice iniziale: ")
    print("1 -> Inserimento matrice iniziale da tastiera")
    print("2 -> Inserimento matrice iniziale da file.")
    matrice_iniziale <- readline()
    if(matrice_iniziale > 2 || check_fun(matrice_iniziale) == FALSE) {
      print("Comando non riconosciuto.")
    }
    else {
      matrice_iniziale <- as.numeric(matrice_iniziale)
      if(matrice_iniziale == 1) {
        c <- FALSE
        while(c == FALSE) { 
          c <- inserimento_da_tastiera()
        }
        print(a)
        matrice_iniziale <- 0
      }
      if(matrice_iniziale == 2) {
        inserimento_da_file()
        matrice_iniziale <- 0
      }
    }
  }
}


check_fun <- function(read) {
  check <<- FALSE
  for (i in 0:9) {
    if(read == i) {
      check <<- TRUE
    }
  }
  return(check)
}


check_function <- function(read) {
  bool <<- FALSE
  for (i in 1:length(v)) {
    if(read == v[i]) {
      bool <<- TRUE
    }
  }
  return(bool)
}

controllo_lettere <- function(read) {
  verifica <<- FALSE
  for (i in 1:length(letters)) {
    if(read == letters[i]) {
      verifica <<- TRUE
    }
  }
  return(verifica)
} 

library("igraph", lib.loc="~/R/i686-pc-linux-gnu-library/3.2")
Graph <- setRefClass("Graph", fields = list(a = "matrix"), methods = list(
  
  esiste_arco <- function() {
    partenza <- readline("Vertice di partenza: ")
    arrivo <- readline("Vertice di arrivo: ")
    if(check_function(partenza) && check_function(arrivo)) {
      if(a[partenza,arrivo] != 0){
        print("Esiste un arco")
      } else {
        print("Non esiste un arco")
      }  
    }
    else {
      print("Almeno uno dei vertici inseriti non esiste. ")
      esiste_arco()
    }
  },
  
  vertici_adiacenti <- function() {
    d <- readline("Inserisci un vertice: ")
    if(check_function(d)) {
      for (i in 1:x) {
        old_value <- a[d,d]
        a[d,d] <- 0
        if (a[d,i] != 0) {
          print(v[i])
        }
        a[d,d] <- old_value
      }
    }
    else {
      print("Il vertice inserito non esiste. ")
      vertici_adiacenti()
    }
  },
  
  aggiungi_vertice <- function() {
    x <<- x+1
    b <- matrix(c(0), nrow = x, ncol = x)
    for (i in 1:(x-1)) {
      for (j in 1:(x-1)) {
        b[i,j] <- a[i,j]
      }
    }
    vettore_iniziale <- v
    v[x] <<- readline("Inserisci il vertice da aggiungere: ")
    if(controllo_lettere(v[x])) {
      variabile1 <- match(v[x], vettore_iniziale)
      if(is.na(variabile1)) {
        colnames(b) <- v
        rownames(b) <- v
        for (i in 1:x) {
          k <- readline(cat("inserisci un arco DA: ", v[i], " A: ", v[x]))
          b[i,x] <- as.numeric(k)
        }
        for (s in 1:(x-1)) {
          k <- readline(cat("inserisci arco DA: ", v[x], "A: ", v[s]))
          b[x,s] <- as.numeric(k)
        }
        a <<- b
      } else {
        print("Il vertice inserito è già presente")
        x <<- x - 1 
      }
    }
    else {
      print("Valore inserito non corretto. Riavvio della funzione...")
      x <<- x - 1
      aggiungi_vertice()
    }
  },
  
  rimuovi_vertice <- function() {
    r <- readline("Inserisci il vertice da rimuovere: ")
    if(check_function(r) == TRUE) {
      for(i in 1:x) {
        if(r == v[i]){
          v <<- v[-c(i)]
          x <<- x-1
          a <<- a[-i,-i]
          break
        }
      }
    }
    else {
      print("Il vertice inserito non esiste")
    }
  },
  
  inserisci_arco <- function() {
    partenza <- readline("Inserisci il vertice di partenza: ")
    arrivo <- readline("Inserisci il vertice di arrivo: ")
    if(check_function(partenza) && check_function(arrivo)) {
      if(a[partenza,arrivo] == 0){
        r <- readline("Inserisci il peso: ")
        peso <- as.numeric(r)
        a[partenza,arrivo] <<- peso
      } else {
        print("L'arco già esiste")
      }
    }
    else {
      print("Almeno uno dei vertici inseriti non esiste. ")
      inserisci_arco()
    }
  },
  
  rimuovi_arco <- function() {
    v1 <- readline("Inserisci il vertice di partenza: ")
    v2 <- readline("Inserisci il vertice di arrivo: ") 
    if(check_function(v1) && check_function(v2)) {
      if(a[v1,v2] != 0) {
        a[v1,v2] <<- 0
        cat("Arco da: ", v1, " a: ", v2, " rimosso con successo.")
      } else {
        cat("Non esiste un arco da: ", v1, " a: ", v2)
      }
    }
    else {
      print("Almeno uno dei vertici inseriti non esiste. ") 
      rimuovi_arco()
    }
  },
  
  valori_associati_vertici <- function() {
    vec <- c()
    for (i in 1:x) {
      cat("Inserire valore associato al vertice", v[i])
      vec[i] <- readline()
      if(controllo_lettere(vec[i]) == TRUE) {
        print("Valore inserito non numerico. Ritorno al menù iniziale...")
        break
      }
      vec <<- as.numeric(vec)
    }
    print(vec)
  },
  valore_associato_arco <- function() {
    arcx <- readline("Inserire il primo vertice: ")
    arcy <- readline("Inserire il secondo vertice: ") 
    if(check_function(arcx) && check_function(arcy)) {
      value <- readline("Inserire il valore da associare: ") 
      if(controllo_lettere(value) == TRUE) {
        print("Valore da associare inserito non numerico. ")
        valore_associato_arco()
      }
      else {
        valore <- as.numeric(value)
        a[arcx, arcy] <<- valore
      }
    }
    else {
      print("Almeno uno dei vertici inseriti non esiste. ")
      valore_associato_arco()
    }
  }
))
crea_grafo <- function(a) {
  grafo <<- graph_from_adjacency_matrix(adjmatrix = a, mode = c("directed"), weighted = TRUE)
  plot.igraph(grafo)
  return(grafo)
  grafo
}

lista_adiac <- function(a) {
  nomi_vertici <- rownames(a)
  for (i in 1:x) {
    vertici <- c()
    n <- 1
    for (j in 1:x) {
      if(a[i,j] != 0) {
        vertici[n] <- nomi_vertici[j]
        n <- n + 1
      }
    }
    lista_adiacenza <- list(c(nomi_vertici[i], "->",  vertici))
    print(lista_adiacenza)
  }
}

edge_list <- function(a) {
  for (i in 1:x) {
    for (j in 1:x) {
      if(a[i,j] != 0) {
        v1 <- colnames(a)[i]
        v2 <- rownames(a)[j]
        cat(v1, "->", v2, "\n")      
      }
    }
  }
}
edgelist_dataframe <- function(a) {
  cont <- 1
  vertice_arrivo <- c()
  vertice_partenza <- c()
  peso <- c()
  for (i in 1:x) {
    for (j in 1:x) {
      if(a[i,j] != 0) {
        vertice_partenza[cont] <- colnames(a)[i]
        vertice_arrivo[cont] <- rownames(a)[j]
        peso[cont] <- a[i,j]
        cont <- cont + 1
      }
    }
  }
  df <- data.frame(vertice_partenza, vertice_arrivo, peso)
  print(df)
}
kruskal <- function(a) {
  comando <- 10   
  print("Inserire: ") 
  print("0 -> Per tornare al menù.")
  print("1 -> Per visualizzare il grafo della matrice iniziale.")
  print("2 -> Per visualizzare il grafo dopo l'applicazione dell'algoritmo.")
  while (comando != 0) {
    comando <- readline()
    if(check_fun(comando) == TRUE && comando <= 2) {
      as.numeric(comando)
      
      if(comando == 1) {
        gr <- graph_from_adjacency_matrix(a, mode = "undirected", weighted = TRUE)
        plot.igraph(gr)
        print("Inserire: ") 
        print("0 -> Per tornare al menù.")
        print("1 -> Per visualizzare nuovamente il grafo della matrice iniziale.")
        print("2 -> Per visualizzare il grafo dopo l'applicazione dell'algoritmo.")
      }
      if(comando == 2) {
        vec <- c()
        count <- 1
        bool <- FALSE
        funzione <- function(vec) {
          for (i in 1:x) {
            if(vec[count] == vec[i] && i <= x && vec[count] != 0) {
              bool <<- TRUE
            }
            else {
              bool <<- FALSE
            }
          }
          return(bool)
        }
        if(isSymmetric(a)) {
          cont <- 1
          vertice_arrivo <- c()
          vertice_partenza <- c()
          peso <- c()
          for (i in 1:x) {
            for (j in 1:x) {
              if(a[i,j] == a[j,i]) {
                a[j,i] <- 0
              }
              if(a[i,j] != 0) {
                vertice_partenza[cont] <- colnames(a)[i]
                vertice_arrivo[cont] <- rownames(a)[j]
                peso[cont] <- a[i,j]
                cont <- cont + 1
              }
            }
          }
          vettore_peso <- peso
          arrivo <- c()
          partenza <- c()
          ordina_peso <- c()
          for (i in 1:length(peso)) {
            for (j in 1:length(peso)) {
              if(peso[j] == min(peso)) {
                cont <- j
              }
            }
            ordina_peso[i] <- min(peso)
            peso[cont] <- 50 
            partenza[i] <- vertice_partenza[cont]
            arrivo[i] <- vertice_arrivo[cont]
          }
          y <- 1
          for (i in 1:length(vettore_peso)) {
            
            if(vettore_peso[i] == ordina_peso[y]) {
              
              y <- y + 1
            }
          }
          for (i in 1:x) {
            for (j in 1:x) {
              a[i,j] <- 0
            }
          }
          for (i in 1:x) {
            for (j in 1:x) {
              a[i,j] <- 0
            }
          }
          s <- 0
          vettore_controllo <- c()
          for (i in 1:x) {
            vettore_controllo[i] <- 0 
          }
          for (i in 1:x) {
            s <- s + 1
            var <- match(partenza[i], v)
            var2 <- match(arrivo[i], v)
            if(vettore_controllo[var] == 0 && vettore_controllo[var2] == 0) {
              a[var,var2] <- ordina_peso[i]
              vettore_controllo[var] <- s
              vettore_controllo[var2] <- s
            }
            if(vettore_controllo[var] != vettore_controllo[var2]) {
              if(vettore_controllo[var] == 0) {
                a[var,var2] <- ordina_peso[i]
                vettore_controllo[var] <- vettore_controllo[var2]
              } 
              else if(vettore_controllo[var2] == 0) {
                a[var,var2] <- ordina_peso[i]
                vettore_controllo[var2] <- vettore_controllo[var]
              }
              else {
                a[var,var2] <- ordina_peso[i]
                save_controllovar2 <- vettore_controllo[var2]
                for (m in 1:length(vettore_controllo)) {
                  if(vettore_controllo[m] == save_controllovar2) {
                    vettore_controllo[m] <- vettore_controllo[var]
                  }
                }
              }
            }
            if(funzione(vettore_controllo)) {
              i <- x
              j <- x
            }
          }
          grafo_kruskal <- graph_from_adjacency_matrix(a, mode = "undirected", weighted = TRUE)
          plot.igraph(grafo_kruskal)
          comando <- 0
        }
        else {
          print("La matrice non è simmetrica. Impossibile applicare Kruskal su un grafo orientato.")
          comando <- 0
        }
      }
    }
    else {
      print("Comando non riconosciuto")
      print("Inserire: ") 
      print("0 -> Per tornare al menù.")
      print("1 -> Per visualizzare nuovamente il grafo della matrice iniziale.")
      print("2 -> Per visualizzare il grafo dopo l'applicazione dell'algoritmo.")
      
    }
  }
}


dijkstra_algorithm <- function() {
  comando <- 10   
  print("Inserire: ") 
  print("0 -> Per tornare al menù.")
  print("1 -> Per visualizzare il grafo della matrice iniziale.")
  print("2 -> Per visualizzare il grafo dopo l'applicazione dell'algoritmo.")
  while (comando != 0) {
    comando <- readline()
    if(check_fun(comando) == TRUE && comando <= 2) {
      as.numeric(comando)
      
      if(comando == 1) {
        gr <- graph_from_adjacency_matrix(a, mode = "directed", weighted = TRUE)
        plot.igraph(gr)
        print("Inserire: ") 
        print("0 -> Per tornare al menù.")
        print("1 -> Per visualizzare nuovamente il grafo della matrice iniziale.")
        print("2 -> Per visualizzare il grafo dopo l'applicazione dell'algoritmo.")
      }
      if(comando == 2) {
        b <- matrix(data = c(0), nrow = x, ncol = x, dimnames = list(v,v))
        vettore <- c()
        for (i in 1:x) {
          vettore[i] <- Inf
        }
        bool <- c()
        for (i in 1:x) {
          bool[i] <- FALSE
        }
        vertice_partenza <- readline("Inserire il vertice di partenza: ")
        posizione_partenza <- match(vertice_partenza, v)
        bool[posizione_partenza] <- TRUE
        analisi_vettore <- a[posizione_partenza, ]
        analisi_vettore <- as.numeric(analisi_vettore)
        vertice_precedente <- c()
        for (i in 1:x) {
          vertice_precedente[i] <- 0
        }
        valore_minimo <- 0
        
        for (j in 1:x) {
          for (i in 1:x) {
            if(analisi_vettore[i] != 0) {
              if(vettore[i] > analisi_vettore[i] + valore_minimo) {
                vettore[i] <- analisi_vettore[i] + valore_minimo
                vertice_precedente[i] <- posizione_partenza
              }
            }
          }
          valore_minimo <- min(vettore)
          posizione_vettore <- match(valore_minimo, vettore)
          if(bool[posizione_vettore] == TRUE) {
            vettore[posizione_vettore] <- Inf
            bool[posizione_vettore] <- TRUE
          } else {
            vettore[posizione_vettore] <- Inf
            bool[posizione_vettore] <- TRUE  
            a[vertice_precedente[posizione_vettore], posizione_vettore]
            b[vertice_precedente[posizione_vettore],posizione_vettore] <- a[vertice_precedente[posizione_vettore],posizione_vettore]  
            posizione_partenza <- posizione_vettore
          }
          analisi_vettore <- a[posizione_partenza,]
          analisi_vettore <- as.numeric(analisi_vettore)
        }
        grafo_dijkstra <- graph_from_adjacency_matrix(adjmatrix = b, mode = "directed", weighted = TRUE)
        plot.igraph(grafo_dijkstra)
        comando <- 0
      }
    }
    else {
      print("Comando non riconosciuto")
      print("Inserire: ") 
      print("0 -> Per tornare al menù.")
      print("1 -> Per visualizzare nuovamente il grafo della matrice iniziale.")
      print("2 -> Per visualizzare il grafo dopo l'applicazione dell'algoritmo.")
    }
  }
}


bfs <- function() {
  comando <- 10   
  print("Inserire: ") 
  print("0 -> Per tornare al menù.")
  print("1 -> Per visualizzare il grafo della matrice iniziale.")
  print("2 -> Per visualizzare il grafo dopo l'applicazione dell'algoritmo.")
  while (comando != 0) {
    comando <- readline()
    if(check_fun(comando) == TRUE && comando <= 2) {
      as.numeric(comando)  
      if(comando == 1) {
        gr <- graph_from_adjacency_matrix(a, mode = "directed", weighted = TRUE)
        plot.igraph(gr)
        print("Inserire: ") 
        print("0 -> Per tornare al menù.")
        print("1 -> Per visualizzare nuovamente il grafo della matrice iniziale.")
        print("2 -> Per visualizzare il grafo dopo l'applicazione dell'algoritmo.")
      }
      if(comando == 2) {
        cont <- 1
        partenza <- c()
        arrivo <- c()
        for (i in 1:x) {
          for (j in 1:x) {
            if(a[i,j] != 0)  {
              partenza[cont] <- v[i]
              arrivo[cont] <- v[j]
              cont <- cont + 1
            }
          }
        }
        bool <- c()
        for (i in 1:x) {
          bool[i] <- FALSE
        }
        controllo <- FALSE
        b <- a
        b[] <- 0
        v_part <- readline("Inserire il vertice di partenza: ")
        pos_part <- match(v_part,partenza)
        while (controllo != TRUE) {
          check <- match(partenza[pos_part], v)
          check2 <- match(arrivo[pos_part], v)   
          if(bool[check] != TRUE || bool[check2] != TRUE) {
            bool[check] <- TRUE
            bool[check2] <- TRUE
            b[check,check2] <- 1
            pos_part <- check2
          } else {
            pos_part <- pos_part + 1
          } 
          for (i in 1:x) {
            if(bool[i] != TRUE) {
              controllo <- FALSE
            }
            else  {
              controllo <- TRUE
            }
          }
        }
        grafo_bfs <- graph_from_adjacency_matrix(adjmatrix = b, mode = "directed", weighted = TRUE)
        plot.igraph(grafo_bfs)
        comando <- 0
      }
    }
    else {
      print("Comando non riconosciuto")
      print("Inserire: ") 
      print("0 -> Per tornare al menù.")
      print("1 -> Per visualizzare nuovamente il grafo della matrice iniziale.")
      print("2 -> Per visualizzare il grafo dopo l'applicazione dell'algoritmo.")
    }
  }
}

Interprete <- function(a) {
  print("Inserire un comando dalla lista: ")
  print("0 -> Esci dal menù. ")
  print("1 -> Aggiungi un vertice alla matrice.")
  print("2 -> Rimuovi un vertice dalla matrice.")
  print("3 -> Ritorna i vertici adiacenti ad un vertice.")
  print("4 -> Verifica l'esistenza di un arco dal vertice x al vertice y.")
  print("5 -> Inserisci un arco dal vertice x al vertice y se non c'è.")
  print("6 -> Rimuovi un arco dal vertice x al vertice y se c'è.")
  print("7 -> Imposta i valori associati ai vertici della matrice.")
  print("8 -> Imposta il valore associato ad un arco dal vertice x al vertice y.")
  print("9 -> Crea il grafo associato alla matrice di adiacenza.")
  print("10 -> Stampa la matrice.")
  print("11 -> Mostra la lista di adiacenza dei vertici.")
  print("12 -> Mostra la edge list.")
  print("13 -> Mostra la edge list con pesi.")
  print("14 -> Applica algoritmo di Kruskal al grafo:")
  print("Applicabile solo a grafi non orientati.")
  print("15 -> Tornare all'inserimento dati nella matrice. ")
  print("Lasciando il numero e il nome dei vertici invariati")
  print("16 -> Tornare all'inserimento dati nella matrice. ")
  print("Cambiando il numero e il nome dei vertici")
  print("17 -> Applica algoritmo di Dijkstra al grafo:")
  print("18 -> Applica algoritmo di Ricerca in ampiezza al grafo: ")
  
  i <- readline()
  i <<- as.numeric(i)
  if(i == 1) {
    aggiungi_vertice()
    print(a)
  }
  
  if(i == 2) {
    rimuovi_vertice()
    print(a)
  }
  if(i == 3) {
    vertici_adiacenti()
    print(a)
  }
  if(i == 4) {
    esiste_arco()
    print(a)
  }
  
  if(i == 5) {
    inserisci_arco()
    print(a)
  }
  if(i == 6) {
    rimuovi_arco()
    print(a)  
  }
  if(i == 7) {
    valori_associati_vertici()
    print(a)
  }
  if(i == 8) {
    valore_associato_arco()
    print(a)
  }
  if(i == 9) {
    crea_grafo(a)
    print(a)
  }
  if(i == 10) {
    print(a)
  }
  if(i == 11) {
    lista_adiac(a)
    print(a)
  }
  if(i == 12) {
    edge_list(a)
    print(a)
  }
  if(i == 13) {
    edgelist_dataframe(a)
    print(a)
  }
  if(i == 14) {
    kruskal(a)
    print(a)
  }
  if(i == 15) {
    start()
    return(a)
  }
  if(i == 16) {
    pre_set()
    start()
  }
  if(i == 17) {
    dijkstra_algorithm()
    print(a)
  }
  if(i == 18) {
    bfs()
    print(a)
  }
}

pre_set()
start()

j <- 5
while(j != 0) {
  show_menu <- readline("Vuoi mostrare il menù? y/n \nPremere y per mostrare il menù o n per uscire. \n")
  if (show_menu == "y") {
    Interprete(a)
  }
  if(show_menu == "n") {
    break
  }
  if(j == 0) {
    print("Vuoi tornare al menù? y/n")
    response <- readline()
    if(response == 'y') {
      j <- 5
    }
    if(response == 'n') {
      j <- 0
    }
  }
}