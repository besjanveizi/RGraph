# RGraph
Implementazione di un grafo e metodi BFS, Dijkstra e Kruskall in R

Classe RC coda()
implementa una coda (FIFO)
viene usata nel metodo BFS(sorgente) - vedi 3
contiene 3 campi:
	 size: numerico che indica lunghezza della coda;
	 head e end: environment inizio e fine della coda, possiedono due campi, valore e 			    successivo, quest'ultimo è un environment che punta al prossimo 				    (elemento della coda) environment.
contiene 3 metodi:
	 isEmpty(): controlla se la coda è vuota confrontando size con 0.
	 enqueue(): aggiunge un elemento alla coda.
Nel caso la coda fosse già vuota, si aggiunge l'elemento in head$valore, si fa diventare head padre di head$successivo e  facciamo puntare head e end allo stesso environment poiché vi è un solo elemento.
Se invece la coda contiene qualche elemento eseguiamo le stesse operazioni sopra usando tuttavia l'environment end, ovviamente non facciamo puntare head e end allo  stesso environment.
 dequeue(): rimuove l'elemento in testa dalla coda (FIFO) e lo restituisce
Se la coda è vuota, allora si restituisce un messaggio, altrimenti si elimina il valore in head facendo puntare head$successivo a head e si decrementando size e quindi si restituisce il valore.


Classe RC Graph():
serve per implementare un grafo ed è composta dai campi 
matriceAdiacenza: matrice per rappresentare il grafo
vertici: lista di environment vertice, questi costituito dai campi nome e valore
archi: lista di environment arco, questi costituito dai campi sorgente, destinazione e valore

Siamo consapevoli che avere una matrice di adiacenza è più che sufficiente per rappresentare, eseguire operazioni di modifica o rimozione nel grafo, tuttavia abbiamo optato all'aggiunta di liste di environment vertici ed archi poiché l'iterazione di tutti i vertici/archi è più veloce. Nonostante dobbiamo eseguire due volte la stessa operazione, per tenere le liste e la matrice contemporaneamente aggiornate, siamo arrivati alla conclusione che sfruttare la natura implementativa di ciascuno dei due data structures, semplifica notevolmente il codice di metodi come BFS, kruskal e dijkstra. Sarebbe comunque importante tener a mente che problemi sotto un aspetto più fine alla realtà producono grafi molto sparsi ed implementazioni di BFS o DFS diventano meno complicate per il fetching dei nodi adiacenti con le liste di adiacenza.
I metodi della classe Graph() sono spiegati singolarmente in seguito.

1a.1 – checkArco(x,y): verificare se c'è un arco dal vertice x al vertice y
si controlla innanzitutto se i vertici x e y sono presenti nel grafo (esiste(x)- vedi extra1), dopodiché si verifica se sono legati con un arco. In qualunque caso il metodo restituisce un messaggio cosicché l'utente sia informato sul legame che vi è (o non è) tra i due nodi nel grafo.

1a.2 – verticiAdiacenti(x): elencare tutti i vertici adiacenti ad x
si controlla che il vertice x sia presente nel grafo e dopo si sfrutta la lista listaAdj() per aggiungere i vertici adiacenti ad x, quindi, per ogni colonna (si potevano anche usare le righe) della matrice di adiacenza, si controlla se vi è un arco in posizione matriceAdiacenza[x,col]. Il metodo è predisposto di messaggi di notifica per ogni suo risultato.

1a.3 – addVertice(x,v):  aggiungere il vertice x, se c'è
si controlla che il vertice x che si sta inserendo non sia già presente nel grafo e che il valore sia un numero positivo. Avendo due tipi di data structures (matrice e lista di environment) dobbiamo inserire il nuovo vertice sia nella lista vertici (append nuovo environment vertice nella lista vertici) che nella matriceAdiacenza. Successivamente creiamo due matrici temporanee, una di una sola riga (di nome x) e l'altra di una sola colonna (di nome x), che uniamo poi con le righe – rbind() - e le colonne - cbind() - della nostra matriceAdiacenza.

1a.4 – deleteVertice(x): rimuovere il vertice x, se c'è
si controlla che il vertice sia effettivamente presente nel grafo, quindi si eliminano eventuali archi che sono legati al vertice x. Dobbiamo tener conto delle tre data structures matriceAdiacenza e le liste di environment archi e vertici. Per l'eliminazione degli archi dalla lista archi si usa deleteArco() - vedi 1a.6 – mentre, essendo unica e posizionale la natura dei vertici sia nella lista vertici che nella matriceAdiacenza, possiamo identificare l'indice del vertice da eliminare usando la funzione which()sulla matrice. Da notare l'uso dell'argomento drop=FALSE nell'aggiornamento della matrice per mantenere il data type matrix nonostante il subsetting.

1a.5 – addArco(x,y,v): aggiungere l'arco dal vertice x al vertice y, se non c'è
si controlla che i due vertici sono già presenti nel grafo e che il valore settato per il nuovo arco (che non sia già presente, quindi inizialmente settato a 0) sia un numero positivo non nullo: settare un arco a 0 vale a dire eliminarlo (deleteArco(x,y) – vedi 1a.6) ma non ha senso nel presente metodo aggiungere un nuovo arco tra due vertici presenti e settarlo a 0 quando già lo è di principio. Dobbiamo aggiornare due data structures: la lista archi usando la funzione append del nuovo environment vertice e la matrice settando a v il valore di matriceAdiacenza[x,y].

1a.6 – deleteArco(x,y): rimuovere l'arco dal vertice x al vertice y, se c'è
si controlla l'esistenza dei due vertici x e y e che siano legati da un arco. Per aggiornare la lista archi si scorrono tutti gli environment arco e si cerca quello desiderato (cercando arco$sorgente = x e arco$destinazione = y), l'aggiornamento della matriceAdiacenza è semplicemente il settaggio della cella [x,y] a 0.



1a.7 – getValoreVertice(x): restituire il valore associato al vertice x
si controlla che il vertice x sia presente nel grafo, dopodiché si cerca nella lista vertici la corrispondenza nome di x e si restituisce il campo valore.

1a.8 – setValoreVertice(x,v): impostare il valore assegnato al vertice x a v
si controlla che il vertice x sia presente nel grafo e che il valore sia un numero positivo quindi si scorrono gli environment vertice nella lista vertici finché non trovo la corrispondenza nome di x e poi cambio il valore.

1a.9 – getValoreArco(x,y): restituire il valore associato all'arco (x,y)
si controlla che i vertici x e y e l'arco x→y sono presenti nel grafo, quindi si restituisce matriceAdiacenza[x,y]
1a.10 – setValoreArco(x,y,v): impostare il valore associato all'arco (x,y) a v
si controlla che i due vertici e l'arco x→y sono presenti nel grafo e che il valore da settare sia un numero positivo non nullo: come si può notare, questo metodo prevede che l'arco 
x→y sia già presente nel grafo, quindi matriceAdiacenza[x,y] non deve essere a 0 (poiché in tal caso si deve richiamare addArco(x,y,v) – vedi 1a.5), inoltre il nuovo valore v che settiamo non può essere 0, perché ciò significherebbe eliminarlo (che viene fatto da deleteArco(x,y) – vedi 1a.5). Bisogna aggiornare la lista vertici, scorrendo finché non si trova la corrispondenza sorgente e destinazione dell'arco x→y e poi si cambia il campo valore, e matriceAdiacenza[x,y] con il nuovo valore v.

extra1 – esiste(x): verificare l'esistenza di un vertice nel grafo
metodo funzionale che restituisce TRUE se x si trova tra le righe della matrice di adiacenza, FALSE altrimenti.

extra2 – stampaVertici(): stampare i vertici del grafo

extra3 – stampaArchi(): stampare gli archi del grafo

extra4 – reset(): inizializza il grafo
inizializza matriceAdiacenza e le liste vertici e archi

1b – creaListaAdiacenza(): rappresentare il grafo con la struttura dati di lista di adiacenza
Perché si possa creare una lista di adiacenza del grafo è necessario che ci siano vertici, quindi si prosegue a creare listaAdiacenza() scorrendo tutti i vertici (nel codice vengono usate le righe della marticeAdiacenza), inserendo (in append) un valore vuoto se un vertice non presenta adiacenze, altrimenti si inseriscono (sempre in append) i vertici adiacenti. Per comodità e corrispondenza abbiamo cambiato i nomi degli elementi della lista con quelli dei vertici.

1c – creaEdgeList(): rappresentare il grafo con la struttura dati di edge list
Perché si possa creare una edge list del grafo è necessario che ci siano archi, dunque si scorrono tutti gli archi nella lista archi e, ad ogni elemento della edge list, inserisco (in append) arco$sorgente, arco$destinazione, arco$valore.

2 – plotGrafo(matriceGrafo, verticiGrafo, archiGrafo): visualizzare il grafo
La condizione principale è sicuramente la presenza di vertici nel grafo, perché altrimenti non c'è nulla da visualizzare, tuttavia il codice prevede anche il controllo che la libreria “networkD3” sia installata. Dobbiamo tener conto che i plot di questa libreria sono creati usando JavaScript che è 0-based, mentre R è 1-based. Dipendentemente dal tipo di grafo che vogliamo rappresentare ci sono diverse funzioni che questa libreria ci offre, noi useremo forceNetwork che ha bisogno dei dataframe dei vertici e di quello degli archi. Al metodo vengono passati la matrice di adiacenza del grafo (matriceGrafo), la lista di environment dei vertici (verticiGrafo) e degli archi (archiGrafo). Dunque proseguiamo come segue:
creiamo matriceVertici 
con tante righe (numerate a partire da 1) quanti sono i vertici e tre colonne, nome del vertice, valore del vertice e gruppo. I primi due valori si possono ottenere dalla lista di environment verticiGrafo che viene passato al metodo, mentre la terza colonna serve per indicare una differenza in più tra i vari vertici nel grafo, appunto appartenenti ad un gruppo diverso, tuttavia nel nostro caso non serve, perciò l'abbiamo settata a 1 come valore di default per tutti i vertici.
Trasformiamo matriceVertici in dataframeVertici e aggiustiamo i nomi delle righe e delle colonne come riportato sopra con il metodo dimnames()
creiamo matriceArchi
con tante righe (numerate a partire da 1) quanti sono gli archi nel grafo e tre colonne, indice del vertice sorgente, indice del vertice destinazione e valore dell'arco.  La libreria “networkD3”, essendo 0-based, richiede che gli indici dei vertici che vengono inseriti nella matriceArchi partano da 0: perciò, una volta trovato l'indice del nostro vertice sorgente/destinazione nella matriceGrafo, facciamo -1 (R e' 1-based). Il parametro "valore" influisce in modo eccessivo nel plot del grafo, perciò usiamo la sua radice quadrata come riferimento.
Trasformiamo matriceArchi in dataframeArchi e aggiustiamo i nomi delle righe e delle colonne come riportato sopra con il metodo dimnames().
Una volta arrivati a questo punto abbiamo due dataframe, dataframeVertici e dataframeArchi, che ci servono per il metodo forceNetwork definito nella nostra libreria networkD3. Il codice prevede la differenza tra un grafo orientato e non per rappresentarlo con le frecce o semplicemente con collegamenti non direzionali.

3 – BFS(sorgente): implementare l'attraversamento in ampiezza del grafo
Al metodo viene passato in input un vertice sorgente da cui si vuole partire la visita in ampiezza, perciò dobbiamo controllare che esso si trovi nel grafo. Ovviamente il metodo prevede che il grafo abbia archi che possa attraversare, inoltre per semplicità, non sono ammessi archi multipli, quindi, per estensione, nemmeno grafi non orientati. Avendo già definito metodi di manipolazione della nostra classe Graph possiamo usare i campi matriceAdiacenza e le due liste vertici e archi per eseguire il BFS. Dunque salviamo il loro contenuto in variabili temporanee (matriceApp, verticiApp, archiApp) e inizializziamo il grafo. Creiamo un array booleano verticiVisitati che contenga tutti i vertici del nostro grafo come nomi: ci servirà per capire se un vertice è già stato visitato (quindi scoperto e, al più, scoperti anche i vertici adiacenti) o meno. Il metodo prevede che ci siano grafo sparsi perciò, finita una componente strettamente connessa, si controlla se ci sono altri nodi in verticiVisitati che sono ancora rimasti FALSE: nel caso lo siano si deve rifare il BFS a partire da uno di quei nodi. All'inizio il nodo sorgente è quello che inserisco in input al metodo (a valore distanza 0, perché è il vertice da cui parte il BFS). Ogni volta che eseguo il BFS per ciascuna componente devo anche creare una nuova coda daVisitare() - vedi coda() - che mi permette di gestire i vertici ancora da visitare. La scoperta dei vertici adiacenti è effettuata dal secondo ciclo while che controlla se la coda daVisitare è vuota. Si estrae l'elemento in testa dalla coda e lo si salva in verticeVisitato, dunque si salvano i suoi vertici adiacenti nell'array adiacenti e poi si scorrono tutti per controllare se sono già stati scoperti; nel caso non lo siano, aggiungiamo ciascuno alla lista vertici e archi (quest'ultima ha come nodo sorgente  verticeVisitato, come destinazione il nodo scoperto e come valore dell'arco 1) e infine in coda a daVisitare.
Una volta che la coda è vuota ed esco dal while interno, bisogna controllare che ho visitato tutti i vertici del mio grafo e non ci siano altre componenti, perché nel caso ci siano, si ritorna al primo while con sorgente aggiornata ad uno dei nodi che ancora non sono stati scoperti.
Spostiamo matriceAdiacenza e le liste vertici e archi in variabili apposite per la stampa e rimettiamo come prima i valori che abbiamo salvato in  matriceApp, verticiApp e archiApp.
Per visualizzare il risultato della visita in ampiezza usiamo il metodo plotGrafo() - vedi 2.



4 – dijkstra(sorgente): eseguire l'algoritmo di Dijkstra sul grafo e plottare il risultato
Al metodo viene passato un vertice sorgente presente nel grafo da cui si vuole partire l'algoritmo dei cammini minimi: dunque il grafo deve avere archi e non deve essere non orientato. Riuso i campi matriceAdiacenza e le due liste vertici e archi della classe Graph, dunque salviamo il loro contenuto in variabili temporanee (matriceApp, verticiApp, archiApp) e inizializziamo il grafo. Usiamo una lista verticiAnalizzati() per controllare i vertici che siamo sicuri che abbiamo registrato il cammino minimo per arrivarci dalla sorgente. L'arcoMinimo è una lista di vertice sorgente, vertice destinazione, valore dell'arco e somma del valore dell’arco e del valore del vertice sorgente che viene registrato ogni ciclo del primo while: tutto ciò mi permette di capire quale arco devo effettivamente aggiungere per eseguire Dijkstra. All'interno del while c'è un for loop che trova tra tutti gli archi del grafo, il candidato arcoMinimo da registrare. La scelta di usare una lista di verticiAnalizzati() è utile in questo punto per capire se un vertice sorgete si trova già nella lista ma quello destinazione ancora no: in questo modo sappiamo che questo arco non l'abbiamo ancora preso in considerazione come arcoMinimo. Dunque, sulla variabile somma ci segniamo il valore dell’arco più il valore del vertice sorgente. Ora si deve aggiungere il vertice destinazione alla lista vertici (quindi alla matriceAdiacenza – vedi addVertice(x,v), 1a.3) con somma come valore del vertice. Nel caso il vertice sia già stato aggiunto bisogna controllare se si può effettuare il rilassamento con il nuovo valore somma, se ovviamente è minore del valore già registrato del vertice stesso. Ultima operazione del for è quella di assicurarsi che arcoMinimo è aggiornato come candidato da inserire nella lista archi. 
Appena esco dal for, controllo che non ho eseguito operazioni su un vertice isolato, basta vedere se arcoMinimo[4] è ancora rimasto a 0, e in tal caso bisogna uscire dal while perché significa che Dijkstra non può andar più avanti. Altrimenti, significa che ho trovato un arco minimo e posso aggiungerlo alla lista archi e concatenare in verticiAnalizzati il vertice destinazione dell'arco appena aggiunto, quindi posso ricominciare il while. 
Uscito dal ciclo principale significa che ho registrato il cammino minimo che parte dal vertice sorgente fino a (possibilmente) tutti gli altri vertici del grafo. Pertanto si può capire che Dijkstra, in un grafo orientato, può essere in grado di visitare tutti i vertici se effettivamente c'è un cammino che li collega al nodo sorgente dell'algoritmo. Infatti stampando i vertici e gli archi  ottenuti possiamo capire il tipo di cammino fatto e usare tali data structures per plottare il risultato.




5 – kruskal(): eseguire l'algoritmo di Kruskal sul grafo e plottare il risultato
L'algoritmo di Kruskal deve essere fatto su grafi non orientati. Per il metodo useremo l'implementazione della lista archi e della matriceAdiacenza della nostra classe Graph, dunque mi salvo i loro contenuti in archiApp e matriceApp e li inizializzo. Salvo la lista archi anche in archiOrdinati, una lista che dovremo manipolare per avere qli archi da prendere in considerazione per eseguire Kruskal. A questo punto salvo la data e il tempo con il metodo Sys.time() nella variabile startTimer.
Prima di tutto elimino gli archi multipli e i cappi e li ordino per arco$valore in archiOrdinati. Tra due archi reciproci, il codice dà precedenza a quello inserito per primo (es. se prima inserisco a→b e poi b→a, il metodo prende in considerazione solo a→b): tuttavia ciò non importa perché si prevedono ambi i casi nella ricerca del minimum spanning tree (MSP) – ripreso e ampliato nella nota *.
Creo una lista pathList per controllare se, per ogni arco in archiOrdinati, sorgente e/o destinazione si trovano nella lista e quindi se trovo un ciclo.
per ogni arco che tengo in considerazione devo controllare se sorgente e/o destinazione 
si trovano in checlistCicli, una lista che contiene i diversi cammini controllando se ci sono cicli. 
(nota *) Essendo la posizione di sorgente e destinazione rilevante alla scelta dell'arco che sta in archiOrdinati, bisogna fare due diversi controlli se non troviamo un ciclo (ovvero se sorgente e destinazione non si trovano nello stesso cammino in pathList()) e quindi, se trovo un riscontro con un capo di un arco che ho già inserito in un cammino della pathList(). La prima condizione esige se, dato un vertice sorgente presente in un cammino, quello di destinazione sta in un altro cammino: nel caso ciò sia vero, collego i due cammini ed elimino quello dove si trovava il nodo destinazione, altrimenti inserisco il vertice di destinazione allo stesso cammino di quello di sorgente. La seconda condizione, invece, sollecita lo stesso costrutto ma in forma reciproca al primo. 
Nel caso il nodo sorgente e destinazione non si trovano nella pathList(), significa che devo creare un nuovo cammino e inserire questi due vertici. Nel caso l'arco non crei un ciclo nel MST, si aggiunge insieme al suo reciproco alla lista archi.
Ora il metodo è finito, perciò salvo la data e il tempo con il metodo Sys.time() nella variabile stopTimer così da avere la complessità del mio codice e posso compararlo con la complessità teorica dell'algoritmo di Kruskal = #archi*log2#vertici.
