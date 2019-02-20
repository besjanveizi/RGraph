#### PROGETTO R ####


################------- CODA -------################
coda<-setRefClass("coda",fields=list(size="numeric",head="environment",end="environment"),
                  methods=list(
                    isEmpty=function(){
                      if(identical(size,numeric(0)))  size<<-0
                      if(size==0) return (TRUE)
                      else return(FALSE)
                    },
                    enqueue=function(x){
                      if(isEmpty()){
                        head$valore<<-x
                        head$successivo<<-new.env()
                        parent.env(head$successivo)<<-head
                        end<<-head
                      }
                      else{
                        end<<-end$successivo
                        end$valore<<-x
                        end$successivo<<-new.env()
                        parent.env(end$successivo)<<-end
                      }
                      size<<-size+1
                    },
                    dequeue=function(){
                      if(isEmpty()) 
                        cat("La coda e' vuota")
                      else{
                        elem<-head$valore
                        size<<-size-1
                        head<<-head$successivo
                        return(elem)
                      }
                    }
                  ))


################------- GRAPH -------################
#La classe RC Graph contiene la matrice di adiacenza, le liste dei vertici e degli archi, e i metodi richiesti
Graph <- setRefClass("Graph",fields = list(matriceAdiacenza = "matrix",vertici = "list",archi = "list"),
                     methods = list(
          #1a.1 - verificare se c'e' un arco dal vertice x al vertice y
                       checkArco = function(x,y) {
                         if(!esiste(x)) return(cat("Il vertice",x,"non e' presente nel grafo"))
                         if(!esiste(y)) return(cat("Il vertice",y,"non e' presente nel grafo"))
                         if(matriceAdiacenza[x,y] == 0) cat("L'arco tra il vertice",x,"e il vertice",y,"non esiste")
                         else cat("L'arco tra il vertice",x,"e il vertice",y,"esiste ed ha valore",matriceAdiacenza[x,y])
                       },
          
          #1a.2 - elencare tutti i vertici y tali che vi sia un arco dal vertice x al vertice y
                       verticiAdiacenti = function(x) {
                         if(!esiste(x)) return(cat("Il vertice",x,"non e' presente nel grafo"))
                         #Crea una lista di adiacenza per aggiungere i vertici adiacenti al nodo x
                         listaAdj <- c()
                         for(col in colnames(matriceAdiacenza)) 
                           if(matriceAdiacenza[x,col] != 0) listaAdj <- append(listaAdj, col)
                         #Se la lista rimane vuota, significa che non c'è nessun vertice adiacente a x
                         if(is.null(listaAdj)) cat("Il vertice",x,"non ha nessun vertice adiacente")
                         else if(length(listaAdj)==1) cat("Il vertice adiacente al vertice",x,"e':",listaAdj)
                         else cat("Il vertici adiacenti al vertice",x,"sono:",listaAdj)
                       },
          
          #1a.3 - aggiungere il vertice x, se non c'è
                       addVertice = function(x,v) {
                         if(esiste(x)) return(cat("Il vertice",x,"e' già presente nel grafo"))
                         if(!(is.numeric(v))) return(cat("Il valore del vertice inserito non e' un numero"))
                         if(v < 0) return(cat("Il valore del vertice non puo' essere negativo"))
                         #Crea il nuovo environment vertice da aggiungere alla lista vertici
                         vertice <- new.env()
                         vertice$nome <- x
                         vertice$valore <- v
                         vertici<<-append(vertici,vertice)
                         #Crea due matrici temporanee, una di una sola riga ed una di una sola colonna,
                         #e le uniamo a quella originale in modo da aggiungere il nuovo vertice
                         newCol <- matrix(0, nrow=nrow(matriceAdiacenza), ncol=1)
                         colnames(newCol) <- x
                         matriceAdiacenza <<- cbind(matriceAdiacenza, newCol)
                         newRow <- matrix(0, nrow=1, ncol=ncol(matriceAdiacenza))
                         rownames(newRow) <- x
                         matriceAdiacenza <<- rbind(matriceAdiacenza, newRow)
                       },
          
          #1a.4 - rimuovere il vertice x, se c'è
                       deleteVertice = function(x) {
                         if(!esiste(x)) return(cat("Il vertice",x,"non e' presente nel grafo"))
                         #Elimina gli eventuali archi che sono legati al vertice x
                         for(riga in rownames(matriceAdiacenza))
                           if(matriceAdiacenza[riga,x] != 0) deleteArco(riga,x)
                         for(colonna in colnames(matriceAdiacenza))
                           if(matriceAdiacenza[x,colonna] != 0) deleteArco(x,colonna)
                         #Rimuovi l'environment vertice "x" dalla lista vertici
                         vertici <<- vertici[-(which(rownames(matriceAdiacenza) == x))]
                         #Aggiorna la matrice di adiacenza rimuovendo riga e colonna "x"
                         matriceAdiacenza <<- matriceAdiacenza[-(which(rownames(matriceAdiacenza) == x)), -(which(colnames(matriceAdiacenza) == x)) ,drop = FALSE]
                       },
          
          #1a.5 - aggiungere l'arco dal vertice x al vertice y, se non c'è
                       addArco = function(x,y,v) {
                         if(!esiste(x)) return(cat("Il vertice",x,"non e' presente nel grafo"))
                         if(!esiste(y)) return(cat("Il vertice",y,"non e' presente nel grafo"))
                         if(matriceAdiacenza[x,y] != 0) return(cat("L'arco ",x,"->",y," è già presente", sep=""))
                         if(!(is.numeric(v))) return(cat("Il valore dell'arco inserito deve essere un numero"))
                         if(v < 1) return(cat("Il valore di un arco inserito non può essere nullo o negativo"))
                         #Crea l'environment arco da aggiungere alla lista archi
                         arco <- new.env()
                         arco$sorgente <- x
                         arco$destinazione <- y
                         arco$valore <- v
                         archi <<- append(archi, arco)
                         #Modifica la matrice di adiacenza inserendo il nuovo valore dell'arco creato
                         matriceAdiacenza[x,y] <<- v
                       },
          
          #1a.6 - rimuovere l'arco dal vertice x al vertice y, se c'è
                       deleteArco = function(x,y) { 
                         if(!esiste(x)) return(cat("Il vertice",x,"non e' presente nel grafo"))
                         if(!esiste(y)) return(cat("Il vertice",y,"non e' presente nel grafo"))
                         if(matriceAdiacenza[x,y] == 0) return(cat("L'arco ",x,"->",y," non e' presente nel grafo", sep=""))
                         #Rimuovi l'environment arco x->y dalla lista archi
                         posizione <- 0
                         for(arco in archi) {
                           posizione <- posizione + 1
                           if(arco$sorgente == x && arco$destinazione == y) break
                         }
                         archi <<- archi[-posizione]
                         #Aggiorna la matrice di adiacenza
                         matriceAdiacenza[x,y] <<- 0
                       },
          
          #1a.7 - restituire il valore associato al vertice x
                       getValoreVertice = function(x) {
                         if(!esiste(x)) return(cat("Il vertice",x,"non e' presente nel grafo"))
                         #Cerca nella lista vertici la corrispondenza nome di x e restituisci il campo valore
                         for(vertice in vertici) if(vertice$nome == x)  return(vertice$valore)   
                       },
          
          #1a.8 - impostare il valore assegnato al vertice x a v
                       setValoreVertice = function(x,v) {
                         if(!esiste(x)) return(cat("Il vertice",x,"non e' presente nel grafo"))
                         if(!(is.numeric(v))) return(cat("Il valore del vertice da modificare deve essere un numero"))
                         if(v < 0) return(cat("Il valore del vertice da modificare non puo' essere negativo"))
                         #Scorri nella lista vertici finche' non trovi la corrispondenza nome di x e poi cambia il valore
                         for(vertice in vertici)
                           if(vertice$nome == x)  {
                             vertice$valore<-(v)
                             cat("Cambiato il valore del vertice",vertice$nome,"in",vertice$valore,"\n")
                             break
                           }   
                       },
          
          #1a.9 - restituire il valore associato all'arco (x,y)
                       getValoreArco = function(x,y) {
                         if(!esiste(x)) return(cat("Il vertice",x,"non e' presente nel grafo"))
                         if(!esiste(y)) return(cat("Il vertice",y,"non e' presente nel grafo"))
                         if(matriceAdiacenza[x,y] == 0) return(cat("L'arco ",x,"->",y," non e' presente nel grafo", sep=""))
                         matriceAdiacenza[x,y]
                       },
          
          #1a.10 - impostare il valore associato all'arco (x,y) a v
                       setValoreArco = function(x,y,v) {
                         if(!esiste(x)) return(cat("Il vertice",x,"non e' presente nel grafo"))
                         if(!esiste(y)) return(cat("Il vertice",y,"non e' presente nel grafo"))
                         if(matriceAdiacenza[x,y] == 0) return(cat("L'arco ",x,"->",y," non e' presente nel grafo", sep=""))
                         if(!(is.numeric(v))) return(cat("Attenzione! Il valore dell'arco da modificare non è un numero"))
                         if(v < 1) return(cat("Il valore di un arco da modificare non puo' essere negativo o nullo"))
                         #Scorri nella lista finchè' non trovi la corrispondenza sorgente e destinazione dell'arco e poi cambia il valore
                         for(arco in archi) 
                           if(arco$sorgente == x && arco$destinazione == y) {
                             arco$valore <- (v)
                             cat("Cambiato il valore dell'arco ",arco$sorgente,"->",arco$destinazione," in ",arco$valore,"\n", sep="")
                             break
                           }
                         #Aggiorna la matrice di adiacenza
                         matriceAdiacenza[x,y] <<- v
                       },
          
          #extra1 - verificare l'esistenza di un vertice nel grafo
                      esiste = function(x) {
                        if(!(x %in% rownames(matriceAdiacenza))) return(FALSE)
                        else return(TRUE)
                      },
          
          #extra2 - stampare i vertici del grafo
                       stampaVertici= function(){
                         if(length(vertici)==0) cat("Non sono presenti vertici")
                         else{
                           cat("I vertici del grafo sono:",'\n')
                           for(vertice in vertici) cat("nome:",vertice$nome,"  valore",vertice$valore,'\n')
                         }
                       },
          
          #extra3 - stampare gli archi del grafo
                       stampaArchi= function(){
                         if(length(archi)==0) cat("Non sono presenti archi")
                         else{
                           cat("Gli archi del grafo sono:",'\n')
                           for(arco in archi) cat("vertice sorgente:",arco$sorgente,"  vertice destinazione:",arco$destinazione,"  valore",arco$valore,'\n')
                         }
                       },
          
          #extra4 - inizializzare il grafo
                      reset = function() {
                        matriceAdiacenza <<- matrix(0, nrow=0, ncol=0)
                        vertici <<- list()
                        archi <<- list()
                      },
          
          #1b - rappresentare il grafo con la struttura dati di lista di adiacenza
                       creaListaAdiacenza = function() {
                         if(length(matriceAdiacenza)==0) cat("Non si puo' creare la lista di adiacenza perche' non vi sono vertici nel grafo")
                         else{
                           listaAdiacenza <- list()
                           for(row in rownames(matriceAdiacenza)) {
                             #se un vertice non ha vertici adiacenti, inserisco nella lista un valore vuoto
                             if(length(which(matriceAdiacenza[row,] != 0)) == 0) listaAdiacenza <- append(listaAdiacenza, "")
                             #altrimenti vengono messi in lista sotto forma di vettore tutti i nomi dei vertici adiacenti
                             else listaAdiacenza <- append(listaAdiacenza, list(names(which(matriceAdiacenza[row,] != 0))))
                             names(listaAdiacenza)[length(listaAdiacenza)] <- row
                           }
                           listaAdiacenza
                         }
                       },
          
          #1c - rappresentare il grafo con la struttura dati di edge list
                       creaEdgeList = function() {
                         if(length(archi)==0) cat("Non si puo' creare la edge list perche' non vi sono archi nel grafo")
                         else {
                           edgeList <- list()
                           for(arco in archi) edgeList <- append(edgeList,list(c(arco$sorgente, arco$destinazione,arco$valore)))
                           edgeList
                         } 
                       },
          
          #2 - visualizza il grafo inserito
                       plotGrafo = function(matriceGrafo, verticiGrafo, archiGrafo) { 
                         if(length(matriceGrafo) == 0) return(cat("Non ci sono vertici nel grafo"))
                         #Installazione e caricamento, se necessario, della libreria "networkD3", utilizzata per visualizzare il grafo inserito
                         if(!("networkD3" %in% rownames(installed.packages()))) install.packages("networkD3")
                         library(networkD3)
                         #creo la matriceVertici
                         #numero righe = numero vertici nel grafo
                         #3 colonne
                         i <- 0
                         matriceVertici <- matrix(0, nrow=length(vertici), ncol=3)
                         for(vertice in verticiGrafo) {
                           i <- i + 1
                           #nome del vertice | valore del vertice | gruppo = 1
                           matriceVertici[i,] <- c(vertice$nome, vertice$valore, 1)
                         }
                         #Trasformo la matriceVertici in dataframeVertici
                         #numero le righe ed nomino le tre colonne (nome, valore, gruppo)
                         dataframeVertici <- data.frame(matriceVertici)
                         righe <- c(1:nrow(matriceVertici))
                         colonne <- c("nome", "valore", "gruppo")
                         dimnames(dataframeVertici) <- list(righe, colonne)
                         #Crea una matriceArchi
                         #numero righe = numero archi nel grafo
                         #3 colonne
                         i <- 0
                         #Se il grafo non ha archi, matriceArchi ha una sola riga con soli 0
                         if(length(archiGrafo) == 0) matriceArchi <- matrix(0, nrow=1, ncol=3)
                         else {
                           matriceArchi <- matrix(0, nrow=length(archiGrafo), ncol=3)
                           for(arco in archiGrafo) {
                           i <- i + 1
                           #indice vertice sorgente | indice vertice destinazione | valore dell'arco
                           matriceArchi[i,1] <- which(rownames(matriceGrafo) == arco$sorgente) - 1
                           matriceArchi[i,2] <- which(colnames(matriceGrafo) == arco$destinazione) - 1
                           # il parametro "valore" influisce in modo eccessivo nel plot del grafo, 
                           #percio' usiamo la sua radice quadrata
                           matriceArchi[i,3] <- sqrt(matriceGrafo[arco$sorgente, arco$destinazione])
                           }
                         }
                         #Trasformiamo la matriceArchi in dataframeArchi
                         #numerio le righe ed nomino le tre colonne (sorgente, destinazione, valore)
                         dataframeArchi <- data.frame(matriceArchi)
                         if(length(archiGrafo) == 0) righe <- c(1)
                         else righe <- c(1:nrow(matriceArchi))
                         colonne <- c("sorgente", "destinazione", "valore")
                         dimnames(dataframeArchi) <- list(righe, colonne)
                         #se la matrice e' simmetrica allora viene plottato un grafo non orientato
                         if(isSymmetric.matrix(matriceGrafo)) {
                           forceNetwork(Nodes = dataframeVertici, NodeID = "nome", Nodesize = "valore", Group = "gruppo",
                                                         Links = dataframeArchi, Source = "sorgente", Target = "destinazione", Value = "valore", 
                                                         linkColour = "#3b4fe5", fontSize = 20, fontFamily = "serif", opacityNoHover = 1)
                         }
                         #altrimenti viene plottato un grafo orientato
                         else {
                           forceNetwork(Nodes = dataframeVertici, NodeID = "nome", Nodesize = "valore", Group = "gruppo",
                                                         Links = dataframeArchi, Source = "sorgente", Target = "destinazione", Value = "valore",
                                                         linkColour = "#3b4fe5", arrows = "directed", fontSize = 20, opacityNoHover = 1)
                         }
                       },
          
          #3 - implementare l'attraversamento in ampiezza del grafo
                       BFS = function(sorgente) {
                         if(!esiste(sorgente)) return(cat("Il vertice",sorgente,"non e' presente nel grafo"))
                         if(length(which(matriceAdiacenza != 0)) == 0) return(cat("Il grafo non ha archi"))
                         if(isSymmetric.matrix(matriceAdiacenza)) return(cat("Questo metodo non puo' eseguire il BFS in un grafo non orientato"))
                         #Controlla che non siano presenti archi multipli
                         for(arco in archi) {
                           x<-arco$sorgente
                           y<-arco$destinazione
                           k<-0
                           for(arco in archi) {
                             if((arco$sorgente == y)&&(arco$destinazione == x)) k<-k +1 
                             if(k==1) return(cat("Non sono ammessi archi multipli"))
                           }
                         }
                         
                         matriceApp <- matriceAdiacenza
                         verticiApp <- vertici
                         archiApp <- archi
                         reset()
                         
                         #verticiVisitati e' un array booleano di tutti i vertici del grafo
                         #e ci dice quali vertici sono stati gia' scoperti e quali ancora no
                         verticiVisitati<-rep(FALSE,length(verticiApp))
                         names(verticiVisitati)<-rownames(matriceApp)
                         
                         #finito e' una variabile booleana per dire che ho visitato tutti i vertici del mio grafo
                         finito<- FALSE
                         while(!finito) {
                           distanza <- 0
                           cat("Inserisco il vertice",sorgente,"\n")
                           addVertice(sorgente, distanza)
                           #daVisitare è la coda che gestisce in ordine BST quali vertici devo visitare
                           daVisitare<-coda()
                           daVisitare$enqueue(sorgente)
                           verticiVisitati[sorgente]<-TRUE
                           
                           #questo while verra' eseguito ogni volta che visitiamo un vertice adiacente a quello precedente
                           while(!daVisitare$isEmpty()) {
                             distanza <- (distanza + 1)
                             verticeVisitato<-daVisitare$dequeue()
                             cat("Sto visitando il vertice",verticeVisitato,"\n")
                             #salviamo tutti i vertici adiacenti al vertice di turno in "adiacenti"
                             adiacenti<-matriceApp[verticeVisitato,]
                             adiacenti<-adiacenti[adiacenti!= 0]
                             print(adiacenti)
                             print(verticiVisitati)
                             #se non ho vertici adiacenti, passo al prossimo in coda
                             if(length(adiacenti)==0) next
                             #controllo se tra i vertici adiacenti, ci sono alcuni ancora non scoperti
                             for(i in 1:length(adiacenti)){
                               scoperto<-names(adiacenti[i])
                               #nel caso non lo siano, si scoprono e si aggiungono alla coda
                               if(verticiVisitati[scoperto]==FALSE){
                                 cat("Inserisco il vertice",scoperto,"\n")
                                 addVertice(scoperto, distanza)
                                 addArco(verticeVisitato, scoperto,1)
                                 verticiVisitati[scoperto]<-TRUE
                                 print(verticiVisitati)
                                 daVisitare$enqueue(scoperto)
                               }
                             }
                           }
                           #controllo che ho visitato tutti i vertici del mio grafo e non ci siano altre componenti
                           for(i in 1:length(verticiVisitati)) {
                             if(verticiVisitati[i]==TRUE) {
                               finito<-TRUE
                               next
                             }
                             else {
                               finito<-FALSE
                               sorgente<-names(verticiVisitati[i])
                               break
                             }
                           }
                            if(!finito) cat("Rifaccio la visita in ampiezza con sorgente",sorgente,"\n")
                          }
                         plotMatrice <- matriceAdiacenza
                         matriceAdiacenza <<- matriceApp
                         plotVertici <- vertici
                         vertici <<- verticiApp
                         plotArchi <- archi
                         archi <<- archiApp
                         plotGrafo(plotMatrice, plotVertici, plotArchi)  
                       },
                       
          #4 - eseguire l'algoritmo di Dijkstra sul grafo e plottare il risultato
                       dijkstra = function(sorgente) {
                         if(!esiste(sorgente)) return(cat("Il vertice",sorgente,"non e' presente nel grafo"))
                         if(length(which(matriceAdiacenza != 0)) == 0) return(cat("Il grafo non ha archi"))
                         if(isSymmetric.matrix(matriceAdiacenza)) return(cat("Questo metodo non funziona su grafi non orientati"))
                         
                         matriceApp <- matriceAdiacenza
                         verticiApp <- vertici
                         archiApp <- archi
                         reset()
                         
                         #creo la lista verticiAnalizzati per controllare i vertici aggiunti e aggiungo sorgente
                         addVertice(sorgente, 0)
                         verticiAnalizzati <- list(sorgente)
                         #ad ogni ciclo while inserirò un arcoMinimo alla lista archi
                         while(length(verticiAnalizzati) < length(verticiApp)) {
                           arcoMinimo <- list(NULL, NULL, 0, 0)
                           
                           #ciclo tra gli archi per tovare in candidato arcoMinimo
                           for(arco in archiApp) {
                             if(arco$sorgente %in% verticiAnalizzati && !(arco$destinazione %in% verticiAnalizzati)) {
                               somma<- arco$valore+getValoreVertice(arco$sorgente)
                               if(!esiste(arco$destinazione)) addVertice(arco$destinazione, somma)
                               #rilassamento
                               else if(getValoreVertice(arco$destinazione) > somma) setValoreVertice(arco$destinazione, somma)
                               if(arcoMinimo[4] == 0 || somma < arcoMinimo[4]) arcoMinimo <- list(arco$sorgente, arco$destinazione, arco$valore, somma)
                             }
                           }
                           
                           #se il vertice e' isolato allora usciamo dal while
                           if(arcoMinimo[4] == 0) break
                           
                           #altrimenti aggiungo arcoMinimo e il vertice destinazione viene inserito tra i verticiAggiunti
                           addArco(arcoMinimo[[1]], arcoMinimo[[2]], arcoMinimo[[3]])
                           verticiAnalizzati <- append(verticiAnalizzati, arcoMinimo[[2]])
                         }
                         if(length(archi) == 0) cat("Attenzione, il vertice",sorgente,"è isolato")
                         
                         start$stampaVertici()
                         start$stampaArchi()
                         plotMatrice <- matriceAdiacenza
                         matriceAdiacenza <<- matriceApp
                         plotVertici <- vertici
                         vertici <<- verticiApp
                         plotArchi <- archi
                         archi <<- archiApp
                         plotGrafo(plotMatrice, plotVertici, plotArchi)
                       },
          
          #5 - eseguire l'algoritmo di Kruskal sul grafo e plottare il risultato
                       kruskal = function() {
                         if(!(isSymmetric.matrix(matriceAdiacenza))) return(cat("Kruskal non può essere eseguito su un grafo orientato"))
                         if(length(which(matriceAdiacenza != 0)) == 0) return(cat("Il grafo non ha archi"))
                         
                         matriceApp <- matriceAdiacenza
                         archiApp <- archi
                         archiOrdinati <- archi
                         matriceAdiacenza[] <<- 0
                         archi <<- list()
                         
                         archiTemp <- list()
                         matriceTemp <- matriceAdiacenza
                         
                         #START TIMER
                         startTimer<- Sys.time()
                         
                         #elimino archi multipli
                         for(arco in archiOrdinati) {
                           x<-arco$sorgente
                           y<-arco$destinazione
                           v<-arco$valore
                           if(matriceTemp[x,y]==0 && matriceTemp[y,x]==0) {
                             archiTemp <- append(archiTemp, arco)
                             matriceTemp[x,y]=v
                           }
                         }
                         archiOrdinati<-archiTemp
                         
                         #ordino gli archi per valore senza contare cappi
                         while(length(archiOrdinati) > 0) {
                           #ricerco arcoMinimo tra archiOrdinati
                           arcoMinimo <- list(NULL, NULL, 0)
                           for(arco in archiOrdinati) 
                             if(arcoMinimo[3] == 0 || arco$valore < arcoMinimo[3]) arcoMinimo <- list(arco$sorgente, arco$destinazione, arco$valore)
                           #aggiungo arcoMinimo alla lista archi (se non e' un cappio)
                           if(!(arcoMinimo[[1]] == arcoMinimo[[2]])) addArco(arcoMinimo[[1]], arcoMinimo[[2]], arcoMinimo[[3]])
                           #rimuovo arcoMinimo da archiOrdinati
                           trovato <- 0
                           for(arco in archiOrdinati) {
                             trovato <- trovato + 1
                             if(arco$sorgente == arcoMinimo[[1]] && arco$destinazione == arcoMinimo[[2]]) break
                           }
                           archiOrdinati <- archiOrdinati[-trovato]
                         }
                         
                         matriceAdiacenza[] <<- 0
                         archiOrdinati <- archi
                         archi <<- list()
                         
                         pathList <- list("")
                         #per ogni arco che tengo in considerazione devo controllare se sorgente e/o destinazione 
                         #si trovano in pathList, una lista che contiene i diversi cammini controllando se ci sono cicli
                         for(arco in archiOrdinati) {
                           #La variabile "ciclo" verrà settata a TRUE se viene rilevato un ciclo, in modo da non aggiungere l'arco corrispondente
                           ciclo <- FALSE
                           #controllo se sia sorgente che destinazione si trovano all'interno dello stesso cammino, quindi ho un ciclo
                           for(cammino in 1:length(pathList)) {
                             if(arco$sorgente %in% pathList[[cammino]] && arco$destinazione %in% pathList[[cammino]]) {
                               ciclo <- TRUE
                               break
                             }
                             
                             #verifico se cercando sorgente/destinazione trovo un riscontro del reciproco
                             riscontro <- FALSE
                             #controllo se, dato sorgente presente in un cammino, destinazione sta in un altro cammino
                             if(arco$sorgente %in% pathList[[cammino]]) {
                               for(camminoDest in cammino:length(pathList)) {
                                 if(arco$destinazione %in% pathList[[camminoDest]]) {
                                   riscontro <- TRUE
                                   #se destinazione si trova in un altro cammino, collego i due cammini ed elimino camminoDest
                                   pathList[[cammino]] <- append(pathList[[cammino]], pathList[[camminoDest]])
                                   pathList <- pathList[-camminoDest]
                                   break
                                 }
                               }
                               #se destinazione non si trova in altri cammini, la inserisco al cammino della sorgente
                               if(!riscontro) pathList[[cammino]] <- append(pathList[[cammino]], arco$destinazione)
                               break
                             }
                             #controllo se, data destinazione presente in un cammino, sorgente sta in un altro cammino
                             if(arco$destinazione %in% pathList[[cammino]]) {
                               for(camminoSorg in cammino:length(pathList)) {
                                 if(arco$sorgente %in% pathList[[camminoSorg]]) {
                                   riscontro <- TRUE
                                   #se sorgente si trova in un altro cammino, collego i due cammini ed elimino camminoSorg
                                   pathList[[cammino]] <- append(pathList[[cammino]], pathList[[camminoSorg]])
                                   pathList <- pathList[-camminoSorg]
                                   break
                                 }
                               }
                               #se sorgente non si trova in altri cammini, la inserisco al cammino della destinazione
                               if(!riscontro) pathList[[cammino]] <- append(pathList[[cammino]], arco$sorgente)
                               break
                             }
                             
                             #se arrivo alla fine della pathList (quindi se non ci trovo sorgente o destinazione), inserisco l'arco in un nuovo cammino
                             if(cammino == length(pathList)) pathList <- append(pathList, list(c(arco$sorgente, arco$destinazione)))
                             #elimino il primo elemento vuoto della lista appena creata
                             if(pathList[[1]][1] == "") pathList <- pathList[-1]
                           }
                           #se non ho ciclo, aggiungo l'arco e in suo reciproco
                           if(!ciclo) {
                             addArco(arco$sorgente, arco$destinazione, arco$valore)
                             addArco(arco$destinazione, arco$sorgente, arco$valore)
                           }
                         }
                         
                         #STOP TIMER
                         stopTimer<-Sys.time()
                         #calcolo complessita teorica e quella del metodo
                         complessitaTeorica<-(length(archiApp)*log2(length(vertici)))
                         complessitaCodice<-(stopTimer-startTimer)
                         cat("Complessità computazionale teorica",complessitaTeorica,'\n')
                         cat("Complessità computazionale del programma",complessitaCodice,'\n')
                         
                         plotMatrice <- matriceAdiacenza
                         matriceAdiacenza <<- matriceApp
                         plotArchi <- archi
                         archi <<- archiApp
                         plotGrafo(plotMatrice, vertici, plotArchi)
                       }
                     )
)


################------- TEST AREA -------################

start<-Graph$new()
start$matriceAdiacenza

#aggiungi vertice a con valore 1
  start$addVertice("a",1)
  #stampa i vertici del grafo
  start$stampaVertici()
#error check addVertice(x,v)
  start$addVertice("a",4) #vertice gia' presente nel grafo
  start$addVertice("b","1") #valore non numerico
  start$addVertice("b",-13) #valore negativo

#rimuovi il vertice "a" dal grafo
  start$deleteVertice("a")
  start$stampaVertici()
  start$matriceAdiacenza
#error check deleteVertice(x)
  start$deleteVertice("a") #vertice non presente nel grafo

#get & set valore di un vertice nel grafo
  start$addVertice("a",1)
  start$getValoreVertice("a")
  start$stampaVertici()
  start$setValoreVertice("a",2)
  start$stampaVertici()
#error check getValoreVertice(x) & setValoreVertice(x,v)
  start$getValoreVertice("b") #vertice non presente nel grafo
  start$setValoreVertice("b") #vertice non presente nel grafo
  start$setValoreVertice("a","3") #valore non numerico
  start$setValoreVertice("a",-5) #valore negativo

#aggiungi l'arco a-5->b
  start$addVertice("b",4)
  start$matriceAdiacenza
  start$addArco("a","b",5)
  start$stampaVertici()
  start$stampaArchi()
  start$matriceAdiacenza
#error check addArco(x,y,v)
  start$addArco("x","b",5) #vertice non presente nel grafo
  start$addArco("a","b",5) #arco già presente
  start$addArco("b","a","6") #valore non numerico
  start$addArco("b","a",0) #valore nullo
  start$addArco("b","a",-3) #valore negativo

#rimuovi l'arco a->b
  start$deleteArco("a","b")
  start$stampaArchi()
  start$matriceAdiacenza
#error check deleteArco(x,y)
  start$deleteArco("x","b") #vertice non presente nel grafo
  start$deleteArco("a","b") #arco gia' non esiste nel grafo

#get & set valore di arco nel grafo
  start$addArco("a","b",5)
  start$getValoreArco("a","b")
  start$setValoreArco("a","b",3)
  start$stampaArchi()
  start$matriceAdiacenza
#error check getValoreArco(x,y) & setValoreArco(x,y,v)
  start$getValoreArco("x","b") #vertice non presente nel grafo
  start$setValoreArco("x","b",4) #vertice non presente nel grafo
  start$getValoreArco("b","a") #arco non presente nel grafo
  start$setValoreArco("b","a",3) #arco non presente nel grafo
  start$setValoreArco("a","b","3") #valore non numerico
  start$setValoreArco("a","b",0) #valore nullo
  start$setValoreArco("a","b",-8) #valore negativo
  
#controlla se esiste l'arco a->b
  start$checkArco("a","b")
#error check checkArco(x,y)
  start$checkArco("x","b") #vertice non presente nel grafo
  start$checkArco("b","a") #arco non presente nel grafo
  
#elenca tutti i vertici adiacenti aa "a"
  start$verticiAdiacenti("a")
  start$addVertice("c",4)
  start$addArco("a","c",2)
  start$verticiAdiacenti("a")
  start$matriceAdiacenza
#error check verticiAdiacenti(x)
  start$verticiAdiacenti("x") #vertice non presente nel grafo
  start$verticiAdiacenti("b") #non ci sono vertici adiacenti

#reset grafo
  start$reset()
  start$matriceAdiacenza
  
#esempio di grafo orientato senza archi multipli o cappi
  start$addVertice("a",2)
  start$addVertice("b",4)
  start$addVertice("c",4)
  start$addVertice("d",5)
  start$addVertice("e",8)
  start$addVertice("f",12)
  start$addVertice("h",2)
  start$matriceAdiacenza
  start$addArco("a","b",4)
  start$addArco("c","d",12)
  start$addArco("a","c",2)
  start$addArco("c","b",7)
  start$addArco("b","d",5)
  start$addArco("e","f",2)
  start$addArco("e","c",8)
  start$matriceAdiacenza
  
#rappresentare il grafo con una lista di adiacenza
  start$creaListaAdiacenza()

#rappresentare il grafo con una edge list
  start$creaEdgeList()
  
#plot del grafo non orientato
  start$plotGrafo(start$matriceAdiacenza,start$vertici,start$archi)
  
#visita in ampiezza a partire da "a" 
  start$BFS("a")
  
#Dijkstra a partire da "a"
  start$dijkstra("a")
  

#aggiungo archi per creare un grafo non orientato
  start$addArco("d","c",12)
  start$addArco("f","e",2)
  start$addArco("c","e",8)
  start$addArco("c","c",1)
  start$addArco("b","a",4)
  start$addArco("d","b",5)
  start$addArco("c","a",2)
  start$addArco("b","c",7)
#plot del grafo non orientato
  start$plotGrafo(start$matriceAdiacenza,start$vertici,start$archi)
#esegui Kruskal sul grafo non orientato
  start$kruskal()
