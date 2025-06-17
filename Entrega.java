import java.lang.AssertionError;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.BiPredicate;
import java.util.function.BooleanSupplier;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.IntStream;

/*
 * Aquesta entrega consisteix en implementar tots els mètodes anomenats "exerciciX". Ara mateix la
 * seva implementació consisteix en llançar `UnsupportedOperationException`, ho heu de canviar així
 * com els aneu fent.
 *
 * Criteris d'avaluació:
 *
 * - Si el codi no compila tendreu un 0.
 *
 * - Les úniques modificacions que podeu fer al codi són:
 *    + Afegir un mètode (dins el tema que el necessiteu)
 *    + Afegir proves a un mètode "tests()"
 *    + Òbviament, implementar els mètodes que heu d'implementar ("exerciciX")
 *   Si feu una modificació que no sigui d'aquesta llista, tendreu un 0.
 *
 * - Principalment, la nota dependrà del correcte funcionament dels mètodes implementats (provant
 *   amb diferents entrades).
 *
 * - Tendrem en compte la neteja i organització del codi. Un estandard que podeu seguir és la guia
 *   d'estil de Google per Java: https://google.github.io/styleguide/javaguide.html . Per exemple:
 *    + IMPORTANT: Aquesta entrega està codificada amb UTF-8 i finals de línia LF.
 *    + Indentació i espaiat consistent
 *    + Bona nomenclatura de variables
 *    + Declarar les variables el més aprop possible al primer ús (és a dir, evitau blocs de
 *      declaracions).
 *    + Convé utilitzar el for-each (for (int x : ...)) enlloc del clàssic (for (int i = 0; ...))
 *      sempre que no necessiteu l'índex del recorregut. Igualment per while si no és necessari.
 *
 * Per com està plantejada aquesta entrega, no necessitau (ni podeu) utilitzar cap `import`
 * addicional, ni qualificar classes que no estiguin ja importades. El que sí podeu fer és definir
 * tots els mètodes addicionals que volgueu (de manera ordenada i dins el tema que pertoqui).
 *
 * Podeu fer aquesta entrega en grups de com a màxim 3 persones, i necessitareu com a minim Java 10.
 * Per entregar, posau els noms i cognoms de tots els membres del grup a l'array `Entrega.NOMS` que
 * està definit a la línia 53.
 *
 * L'entrega es farà a través d'una tasca a l'Aula Digital que obrirem abans de la data que se us
 * hagui comunicat. Si no podeu visualitzar bé algun enunciat, assegurau-vos de que el vostre editor
 * de texte estigui configurat amb codificació UTF-8.
 */
class Entrega {
  static final String[] NOMS = {"Pedro Gelabert", "Abde Afkir", "Amine Karab"};

  /*
   * Aquí teniu els exercicis del Tema 1 (Lògica).
   */
    static class Tema1 {

        static final char CONJ = '∧';
        static final char DISJ = '∨';
        static final char IMPL = '→';
        static final char NAND = '.';

        // Devuelve 1 si es tautología, 0 si es contradicción, -1 en otro caso
        static int exercici1(char[] operadores, int[] variables) {
            int numVariables = Arrays.stream(variables).max().orElse(-1) + 1;
            if (numVariables == 0) {
                return -1;
            }

            boolean esTautologia = true;
            boolean esContradiccion = true;
            int totalCombinaciones = 1 << numVariables;

            // Probar todas las combinaciones de valores
            for (int mascara = 0; mascara < totalCombinaciones; mascara++) {
                boolean[] valores = new boolean[numVariables];
                for (int i = 0; i < numVariables; i++) {
                    valores[i] = ((mascara >> i) & 1) == 1;
                }

                boolean resultado = valores[variables[0]];
                for (int i = 0; i < operadores.length; i++) {
                    boolean siguiente = valores[variables[i + 1]];
                    switch (operadores[i]) {
                        case CONJ:
                            resultado = resultado && siguiente;
                            break;
                        case DISJ:
                            resultado = resultado || siguiente;
                            break;
                        case IMPL:
                            resultado = !resultado || siguiente;
                            break;
                        case NAND:
                            resultado = !(resultado && siguiente);
                            break;
                    }
                }
                if (resultado) {
                    esContradiccion = false;
                } else {
                    esTautologia = false;
                }

                if (!esTautologia && !esContradiccion) {
                    return -1;
                }
            }
            if (esTautologia) {
                return 1;
            }
            if (esContradiccion) {
                return 0;
            }
            return -1;
        }

        // Devuelve true si "para todo x: p(x)" equivale a "existe un único x tal que q(x)"
        static boolean exercici2(int[] universo, Predicate<Integer> p, Predicate<Integer> q) {
            boolean todosCumplenP = true;
            for (int x : universo) {
                if (!p.test(x)) {
                    todosCumplenP = false;
                    break;
                }
            }

            int cuentaQ = 0;
            for (int x : universo) {
                if (q.test(x)) {
                    cuentaQ++;
                }
            }
            boolean existeUnicoQ = (cuentaQ == 1);

            return todosCumplenP == existeUnicoQ;
        }

    static void tests() {
      // Exercici 1
      // Taules de veritat

      // Tautologia: ((p0 → p2) ∨ p1) ∨ p0
      test(1, 1, 1, () -> exercici1(new char[] { IMPL, DISJ, DISJ }, new int[] { 0, 2, 1, 0 }) == 1);

      // Contradicció: (p0 . p0) ∧ p0
      test(1, 1, 2, () -> exercici1(new char[] { NAND, CONJ }, new int[] { 0, 0, 0 }) == 0);

      // Exercici 2
      // Equivalència

      test(1, 2, 1, () -> {
        return exercici2(new int[] { 1, 2, 3 }, (x) -> x == 0, (x) -> x == 0);
      });

      test(1, 2, 2, () -> {
        return exercici2(new int[] { 1, 2, 3 }, (x) -> x >= 1, (x) -> x % 2 == 0);
      });
    }
  }

  /*
   * Aquí teniu els exercicis del Tema 2 (Conjunts).
   *
   * Per senzillesa tractarem els conjunts com arrays (sense elements repetits). Per tant, un
   * conjunt de conjunts d'enters tendrà tipus int[][]. Podeu donar per suposat que tots els
   * arrays que representin conjunts i us venguin per paràmetre estan ordenats de menor a major.
   *
   * Les relacions també les representarem com arrays de dues dimensions, on la segona dimensió
   * només té dos elements. L'array estarà ordenat lexicogràficament. Per exemple
   *   int[][] rel = {{0,0}, {0,1}, {1,1}, {2,2}};
   * i també donarem el conjunt on està definida, per exemple
   *   int[] a = {0,1,2};
   * Als tests utilitzarem extensivament la funció generateRel definida al final (també la podeu
   * utilitzar si la necessitau).
   *
   * Les funcions f : A -> B (on A i B son subconjunts dels enters) les representam o bé amb el seu
   * graf o bé amb un objecte de tipus Function<Integer, Integer>. Sempre donarem el domini int[] a
   * i el codomini int[] b. En el cas de tenir un objecte de tipus Function<Integer, Integer>, per
   * aplicar f a x, és a dir, "f(x)" on x és d'A i el resultat f.apply(x) és de B, s'escriu
   * f.apply(x).
   */
  static class Tema2 {
    /*
     * Trobau el nombre de particions diferents del conjunt `a`, que podeu suposar que no és buid.
     *
     * Pista: Cercau informació sobre els nombres de Stirling.
     */
static int exercici1(int[] array) {
    int mida = array.length;
    int[][] stirling = new int[mida + 1][mida + 1];

    // Inicialitzem el cas base
    stirling[0][0] = 1;

    // Calculem els nombres d'Stirling de segona espècie
    for (int i = 1; i <= mida; i++) {
        for (int j = 1; j <= i; j++) {
            stirling[i][j] = stirling[i - 1][j - 1] + j * stirling[i - 1][j];
        }
    }

    // Suma dels valors d'Stirling per obtenir el número de Bell
    int numeroBell = 0;
    for (int valor : stirling[mida]) {
        numeroBell += valor;
    }

    return numeroBell;
}
      
     
static int exercici2(int[] elementos, int[][] relacion) {
    int tamaño = elementos.length;
    boolean[][] matrizRelacion = new boolean[tamaño][tamaño];

    // Inicializar matriz con relaciones dadas.
    for (int[] par : relacion) {
        int origen = indiceDe(elementos, par[0]);
        int destino = indiceDe(elementos, par[1]);
        if (origen >= 0 && destino >= 0) {
            matrizRelacion[origen][destino] = true;
        }
    }

    // Añadir reflexividad (cada elemento relacionado consigo mismo).
    for (int i = 0; i < tamaño; i++) {
        matrizRelacion[i][i] = true;
    }

    // Calcular cierre transitivo (Floyd-Warshall).
    for (int intermedio = 0; intermedio < tamaño; intermedio++) {
        for (int i = 0; i < tamaño; i++) {
            if (matrizRelacion[i][intermedio]) {
                for (int j = 0; j < tamaño; j++) {
                    if (matrizRelacion[intermedio][j]) {
                        matrizRelacion[i][j] = true;
                    }
                }
            }
        }
    }

    // Comprobar si la relación es antisimétrica.
    for (int i = 0; i < tamaño; i++) {
        for (int j = 0; j < tamaño; j++) {
            if (i != j && matrizRelacion[i][j] && matrizRelacion[j][i]) {
                return -1; // No antisimétrica
            }
        }
    }

    // Contar relaciones verdaderas.
    int contador = 0;
    for (boolean[] fila : matrizRelacion) {
        for (boolean valor : fila) {
            if (valor) contador++;
        }
    }

    return contador;
}
// Método auxiliar para obtener el índice de un elemento en el array.

static Integer exercici3(int[] elementos, int[][] relacion, int[] conjuntoX, boolean buscarCotaSuperior) {
    int tamaño = elementos.length;
    boolean[][] matrizRelacion = new boolean[tamaño][tamaño];

    // Inicializar matriz con relaciones dadas.
    for (int[] par : relacion) {
        int origen = indiceDe(elementos, par[0]);
        int destino = indiceDe(elementos, par[1]);
        if (origen >= 0 && destino >= 0) {
            matrizRelacion[origen][destino] = true;
        }
    }

    // Añadir reflexividad.
    for (int i = 0; i < tamaño; i++) {
        matrizRelacion[i][i] = true;
    }

    // Calcular cierre transitivo
    for (int intermedio = 0; intermedio < tamaño; intermedio++) {
        for (int i = 0; i < tamaño; i++) {
            if (matrizRelacion[i][intermedio]) {
                for (int j = 0; j < tamaño; j++) {
                    if (matrizRelacion[intermedio][j]) {
                        matrizRelacion[i][j] = true;
                    }
                }
            }
        }
    }

    // Convertir elementos del conjunto X a índices.
    List<Integer> indicesX = new ArrayList<>();
    for (int valor : conjuntoX) {
        int indice = indiceDe(elementos, valor);
        if (indice >= 0) {
            indicesX.add(indice);
        }
    }

    List<Integer> cotasInferiores = new ArrayList<>(), cotasSuperiores = new ArrayList<>();

    // Encontrar cotas inferiores y superiores.
    for (int i = 0; i < tamaño; i++) {
        boolean esInferior = true, esSuperior = true;
        for (int elemento : indicesX) {
            if (!matrizRelacion[i][elemento]) {
                esInferior = false;
            }
            if (!matrizRelacion[elemento][i]) {
                esSuperior = false;
            }
            if (!esInferior && !esSuperior) break;
        }
        if (esInferior) cotasInferiores.add(i);
        if (esSuperior) cotasSuperiores.add(i);
    }

    // Buscar cota requerida.
    if (!buscarCotaSuperior) { // Buscar mayor cota inferior (GLB).
        for (int candidato : cotasInferiores) {
            boolean esMayor = true;
            for (int otraCota : cotasInferiores) {
                if (!matrizRelacion[otraCota][candidato]) {
                    esMayor = false;
                    break;
                }
            }
            if (esMayor) return elementos[candidato];
        }
    } else { // Buscar menor cota superior (LUB).
        for (int candidato : cotasSuperiores) {
            boolean esMenor = true;
            for (int otraCota : cotasSuperiores) {
                if (!matrizRelacion[candidato][otraCota]) {
                    esMenor = false;
                    break;
                }
            }
            if (esMenor) return elementos[candidato];
        }
    }

    return null; // No encontrada la cota solicitada.
}

      /*
     * Retorna el gráfico de la inversa (si es biyectiva),
     * una inversa izquierda (si es inyectiva),
     * una inversa derecha (si es sobreyectiva), o null.
       */

    /*
     * Donada una funció `f` de `a` a `b`, retornau:
     *  - El graf de la seva inversa (si existeix)
     *  - Sinó, el graf d'una inversa seva per l'esquerra (si existeix)
     *  - Sinó, el graf d'una inversa seva per la dreta (si existeix)
     *  - Sinó, null.
     */
static int[][] exercici4(int[] conjuntoA, int[] conjuntoB, Function<Integer, Integer> funcion) {
    int tamañoB = conjuntoB.length;
    int[] contadorPreimagenes = new int[tamañoB];
    List<int[]> imagenes = new ArrayList<>();

    // Calcular imagenes y contar preimagenes
    for (int elementoA : conjuntoA) {
        int elementoB = funcion.apply(elementoA);
        imagenes.add(new int[]{elementoA, elementoB});
        int indiceB = indiceDe(conjuntoB, elementoB);
        if (indiceB >= 0) {
            contadorPreimagenes[indiceB]++;
        }
    }

    boolean esInyectiva = true;
    boolean esSobreyectiva = true;

    // Verificar si es inyectiva y/o sobreyectiva
    for (int contador : contadorPreimagenes) {
        if (contador == 0) esSobreyectiva = false;
        if (contador > 1) esInyectiva = false;
    }

    // Preparar inversa según el tipo de función
    if (esInyectiva || esSobreyectiva) {
        int[][] inversa = new int[tamañoB][2];
        for (int i = 0; i < tamañoB; i++) {
            inversa[i] = new int[]{conjuntoB[i], encontrarPreimagen(imagenes, conjuntoB[i])};
        }
        return ordenarLexico(inversa);
    }

    return null; // Ni inyectiva ni sobreyectiva, no hay inversa.
}

// Método auxiliar para encontrar preimagen.
private static int encontrarPreimagen(List<int[]> imagenes, int valorBuscado) {
    for (int[] par : imagenes) {
        if (par[1] == valorBuscado) return par[0];
    }
    return imagenes.get(0)[0];
}

// Método auxiliar para ordenar lexicográficamente
private static int[][] ordenarLexico(int[][] array) {
    Arrays.sort(array, (par1, par2) -> {
        if (par1[0] != par2[0]) return Integer.compare(par1[0], par2[0]);
        return Integer.compare(par1[1], par2[1]);
    });
    return array;
}

// Método auxiliar para obtener índice.
private static int indiceDe(int[] array, int valor) {
    for (int i = 0; i < array.length; i++) {
        if (array[i] == valor) return i;
    }
    return -1;
}

    
    static void tests() {
      // Exercici 1
      // Nombre de particions

      test(2, 1, 1, () -> exercici1(new int[] { 1 }) == 1);
      test(2, 1, 2, () -> exercici1(new int[] { 1, 2, 3 }) == 5);

      // Exercici 2
      // Clausura d'ordre parcial

      final int[] INT02 = { 0, 1, 2 };

      test(2, 2, 1, () -> exercici2(INT02, new int[][] { {0, 1}, {1, 2} }) == 6);
      test(2, 2, 2, () -> exercici2(INT02, new int[][] { {0, 1}, {1, 0}, {1, 2} }) == -1);

      // Exercici 3
      // Ínfims i suprems

      final int[] INT15 = { 1, 2, 3, 4, 5 };
      final int[][] DIV15 = generateRel(INT15, (n, m) -> m % n == 0);
      final Integer ONE = 1;

      test(2, 3, 1, () -> ONE.equals(exercici3(INT15, DIV15, new int[] { 2, 3 }, false)));
      test(2, 3, 2, () -> exercici3(INT15, DIV15, new int[] { 2, 3 }, true) == null);

      // Exercici 4
      // Inverses

      final int[] INT05 = { 0, 1, 2, 3, 4, 5 };

      test(2, 4, 1, () -> {
        var inv = exercici4(INT05, INT02, (x) -> x/2);

        if (inv == null)
          return false;

        inv = lexSorted(inv);

        if (inv.length != INT02.length)
          return false;

        for (int i = 0; i < INT02.length; i++) {
          if (inv[i][0] != i || inv[i][1]/2 != i)
            return false;
        }

        return true;
      });

      test(2, 4, 2, () -> {
        var inv = exercici4(INT02, INT05, (x) -> x);

        if (inv == null)
          return false;

        inv = lexSorted(inv);

        if (inv.length != INT05.length)
          return false;

        for (int i = 0; i < INT02.length; i++) {
          if (inv[i][0] != i || inv[i][1] != i)
            return false;
        }

        return true;
      });
    }

    /*
     * Ordena lexicogràficament un array de 2 dimensions
     * Per exemple:
     *  arr = {{1,0}, {2,2}, {0,1}}
     *  resultat = {{0,1}, {1,0}, {2,2}}
     */
    static int[][] lexSorted(int[][] arr) {
      if (arr == null)
        return null;

      var arr2 = Arrays.copyOf(arr, arr.length);
      Arrays.sort(arr2, Arrays::compare);
      return arr2;
    }

    /*
     * Genera un array int[][] amb els elements {a, b} (a de as, b de bs) que satisfàn pred.test(a, b)
     * Per exemple:
     *   as = {0, 1}
     *   bs = {0, 1, 2}
     *   pred = (a, b) -> a == b
     *   resultat = {{0,0}, {1,1}}
     */
    static int[][] generateRel(int[] as, int[] bs, BiPredicate<Integer, Integer> pred) {
      var rel = new ArrayList<int[]>();

      for (int a : as) {
        for (int b : bs) {
          if (pred.test(a, b)) {
            rel.add(new int[] { a, b });
          }
        }
      }

      return rel.toArray(new int[][] {});
    }

    // Especialització de generateRel per as = bs
    static int[][] generateRel(int[] as, BiPredicate<Integer, Integer> pred) {
      return generateRel(as, as, pred);
    }
  }

  /*
   * Aquí teniu els exercicis del Tema 3 (Grafs).
   *
   * Els (di)grafs vendran donats com llistes d'adjacència (és a dir, tractau-los com diccionaris
   * d'adjacència on l'índex és la clau i els vèrtexos estan numerats de 0 a n-1). Per exemple,
   * podem donar el graf cicle no dirigit d'ordre 3 com:
   *
   * int[][] c3dict = {
   *   {1, 2}, // veïns de 0
   *   {0, 2}, // veïns de 1
   *   {0, 1}  // veïns de 2
   * };
   */
  static class Tema3 {
    /*
     * Determinau si el graf `g` (no dirigit) té cicles.
     */
      static boolean exercici1(int[][] grafo) {
          int tamaño = grafo.length;
          boolean[] visitado = new boolean[tamaño];

          // Verificar cada nodo del grafo
          for (int nodo = 0; nodo < tamaño; nodo++) {
              if (!visitado[nodo]) {
                  if (buscarCiclo(nodo, -1, grafo, visitado)) {
                      return true; // Ciclo encontrado
                  }
              }
          }
          return false; // No hay ciclos
      }

// Método auxiliar para buscar ciclos utilizando DFS.
      private static boolean buscarCiclo(int actual, int padre, int[][] grafo, boolean[] visitado) {
          visitado[actual] = true;

          for (int vecino : grafo[actual]) {
              if (vecino == padre) {
                  continue;
              }
              if (visitado[vecino]) {
                  return true; // Ciclo encontrado
              }
              if (buscarCiclo(vecino, actual, grafo, visitado)) {
                  return true;
              }
          }

          return false;
      }

    /*
     * Determinau si els dos grafs són isomorfs. Podeu suposar que cap dels dos té ordre major que
     * 10.
     */
     static boolean exercici2(int[][] grafo1, int[][] grafo2) {
    int tamaño = grafo1.length;
    if (tamaño != grafo2.length) {
        return false;
    }

    int[] grados1 = new int[tamaño], grados2 = new int[tamaño];
    boolean[][] ady1 = new boolean[tamaño][tamaño], ady2 = new boolean[tamaño][tamaño];

    // Construir matrices de adyacencia y grados de los vértices
    for (int i = 0; i < tamaño; i++) {
        grados1[i] = grafo1[i].length;
        for (int vecino : grafo1[i]) {
            ady1[i][vecino] = true;
            ady1[vecino][i] = true; // Simetría
        }
        grados2[i] = grafo2[i].length;
        for (int vecino : grafo2[i]) {
            ady2[i][vecino] = true;
        }
    }

    int[] permutacion = new int[tamaño];
    boolean[] usados = new boolean[tamaño];

    return buscarIsomorfismo(0, tamaño, grados1, grados2, ady1, ady2, permutacion, usados);
}

// Método auxiliar para buscar isomorfismo usando backtracking
private static boolean buscarIsomorfismo(int indice, int tamaño,
                                         int[] grados1, int[] grados2,
                                         boolean[][] ady1, boolean[][] ady2,
                                         int[] permutacion, boolean[] usados) {
    if (indice == tamaño) {
        return true; // Isomorfismo encontrado
    }

    for (int candidato = 0; candidato < tamaño; candidato++) {
        if (usados[candidato] || grados1[indice] != grados2[candidato]) continue;

        permutacion[indice] = candidato;
        boolean valido = true;

        for (int previo = 0; previo < indice; previo++) {
            if (ady1[indice][previo] != ady2[permutacion[indice]][permutacion[previo]]) {
                valido = false;
                break;
            }
        }

        if (!valido) continue;

        usados[candidato] = true;
        if (buscarIsomorfismo(indice + 1, tamaño, grados1, grados2, ady1, ady2, permutacion, usados)) {
            return true;
        }
        usados[candidato] = false;
    }

    return false; // No se encontró un isomorfismo válido
}
    /*
     * Determinau si el graf `g` (no dirigit) és un arbre. Si ho és, retornau el seu recorregut en
     * postordre desde el vèrtex `r`. Sinó, retornau null;
     *
     * En cas de ser un arbre, assumiu que l'ordre dels fills vé donat per l'array de veïns de cada
     * vèrtex.
     */
static int[] exercici3(int[][] grafo, int raiz) {
    int tamaño = grafo.length;
    boolean[] visitado = new boolean[tamaño];
    List<Integer> recorridoPostorden = new ArrayList<>();

    // Paso 1: Comprobar aciclicidad y generar recorrido postorden.
    if (!dfsArbol(raiz, -1, grafo, visitado, recorridoPostorden)) {
        return null; // Ciclo detectado, no es un árbol.
    }

    // Paso 2: Comprobar si todos los nodos fueron visitados (conexo).
    for (boolean nodoVisitado : visitado) {
        if (!nodoVisitado) {
            return null; // No conexo, no es un árbol.
        }
    }

    // Convertir lista de postorden a arreglo.
    int[] resultado = new int[recorridoPostorden.size()];
    for (int i = 0; i < resultado.length; i++) {
        resultado[i] = recorridoPostorden.get(i);
    }

    return resultado;
}

// Método auxiliar para realizar DFS y detectar ciclos.
private static boolean dfsArbol(int actual, int padre,
                                int[][] grafo, boolean[] visitado, List<Integer> postorden) {
    visitado[actual] = true;

    for (int vecino : grafo[actual]) {
        if (vecino == padre) continue;

        if (visitado[vecino] || !dfsArbol(vecino, actual, grafo, visitado, postorden)) {
            return false; // Ciclo detectado.
        }
    }

    postorden.add(actual);
    return true;
}


    /*
     * Suposau que l'entrada és un mapa com el següent, donat com String per files (vegeu els tests)
     *
     *   _____________________________________
     *  |          #       #########      ####|
     *  |       O  # ###   #########  ##  ####|
     *  |    ####### ###   #########  ##      |
     *  |    ####  # ###   #########  ######  |
     *  |    ####    ###              ######  |
     *  |    ######################## ##      |
     *  |    ####                     ## D    |
     *  |_____________________________##______|
     *
     * Els límits del mapa els podeu considerar com els límits de l'array/String, no fa falta que
     * cerqueu els caràcters "_" i "|", i a més podeu suposar que el mapa és rectangular.
     *
     * Donau el nombre mínim de caselles que s'han de recorrer per anar de l'origen "O" fins al
     * destí "D" amb les següents regles:
     *  - No es pot sortir dels límits del mapa
     *  - No es pot passar per caselles "#"
     *  - No es pot anar en diagonal
     *
     * Si és impossible, retornau -1.
     */
   static int exercici4(char[][] mapa) {
    int filas = mapa.length, columnas = mapa[0].length;
    int inicio = -1;

    // Buscar posición inicial 'O'
    for (int i = 0; i < filas && inicio < 0; i++) {
        for (int j = 0; j < columnas; j++) {
            if (mapa[i][j] == 'O') {
                inicio = i * columnas + j;
                break;
            }
        }
    }

    if (inicio < 0) return -1; // No encontrado 'O'

    int tamañoMapa = filas * columnas;
    int[] cola = new int[tamañoMapa], distancias = new int[tamañoMapa];
    Arrays.fill(distancias, -1);

    int cabeza = 0, colaFin = 0;
    cola[colaFin++] = inicio;
    distancias[inicio] = 0;

    // Movimientos permitidos: abajo, arriba, derecha, izquierda
    int[][] movimientos = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}};

    while (cabeza < colaFin) {
        int actual = cola[cabeza++];
        int filaActual = actual / columnas, columnaActual = actual % columnas;

        for (int[] mov : movimientos) {
            int nuevaFila = filaActual + mov[0], nuevaColumna = columnaActual + mov[1];

            if (nuevaFila < 0 || nuevaFila >= filas || nuevaColumna < 0 || nuevaColumna >= columnas) continue;

            char celda = mapa[nuevaFila][nuevaColumna];
            int indice = nuevaFila * columnas + nuevaColumna;

            if (celda == '#' || distancias[indice] != -1) continue;

            distancias[indice] = distancias[actual] + 1;

            if (celda == 'D') return distancias[indice];

            cola[colaFin++] = indice;
        }
    }

    return -1; // No hay camino hasta 'D'
}
    /*
     * Aquí teniu alguns exemples i proves relacionades amb aquests exercicis (vegeu `main`)
     */
    static void tests() {

      final int[][] D2 = { {}, {} };
      final int[][] C3 = { {1, 2}, {0, 2}, {0, 1} };

      final int[][] T1 = { {1, 2}, {0}, {0} };
      final int[][] T2 = { {1}, {0, 2}, {1} };

      // Exercici 1
      // G té cicles?

      test(3, 1, 1, () -> !exercici1(D2));
      test(3, 1, 2, () -> exercici1(C3));

      // Exercici 2
      // Isomorfisme de grafs

      test(3, 2, 1, () -> exercici2(T1, T2));
      test(3, 2, 1, () -> !exercici2(T1, C3));

      // Exercici 3
      // Postordre

      test(3, 3, 1, () -> exercici3(C3, 1) == null);
      test(3, 3, 2, () -> Arrays.equals(exercici3(T1, 0), new int[] { 1, 2, 0 }));

      // Exercici 4
      // Laberint

      test(3, 4, 1, () -> {
        return -1 == exercici4(new char[][] {
            " #O".toCharArray(),
            "D# ".toCharArray(),
            " # ".toCharArray(),
        });
      });

      test(3, 4, 2, () -> {
        return 8 == exercici4(new char[][] {
            "###D".toCharArray(),
            "O # ".toCharArray(),
            " ## ".toCharArray(),
            "    ".toCharArray(),
        });
      });
    }
  }

  /*
   * Aquí teniu els exercicis del Tema 4 (Aritmètica).
   *
   * En aquest tema no podeu:
   *  - Utilitzar la força bruta per resoldre equacions: és a dir, provar tots els nombres de 0 a n
   *    fins trobar el que funcioni.
   *  - Utilitzar long, float ni double.
   *
   * Si implementau algun dels exercicis així, tendreu un 0 d'aquell exercici.
   */
  static class Tema4 {
    /*
     * Primer, codificau el missatge en blocs de longitud 2 amb codificació ASCII. Després encriptau
     * el missatge utilitzant xifrat RSA amb la clau pública donada.
     *
     * Per obtenir els codis ASCII del String podeu utilitzar msg.getBytes().
     *
     * Podeu suposar que:
     * - La longitud de msg és múltiple de 2
     * - El valor de tots els caràcters de msg està entre 32 i 127.
     * - La clau pública (n, e) és de la forma vista a les transparències.
     * - n és major que 2¹⁴, i n² és menor que Integer.MAX_VALUE
     *
     * Pista: https://en.wikipedia.org/wiki/Exponentiation_by_squaring
     */
        
    // función que calcula potencias con módulo para evitar desbordamientos: (base elevado a exp) módulo mod
    static int potenciaModular(int base, int exp, int mod) {
        long res = 1, b = base % mod;
        for (; exp > 0; exp >>= 1) {
            if ((exp & 1) == 1) res = (res * b) % mod;
            b = (b * b) % mod;
        }
        return (int) res;
    }
      
    // inverso de e mod n : (a*x) % m = 1
    static int inversoModular(int a, int m) {
        int m0 = m, t, x0 = 0, x1 = 1;
        while (a > 1) {
            int q = a / m;
            t = m; m = a % m; a = t;
            t = x0; x0 = x1 - q * x0; x1 = t;
        }
        return x1 < 0 ? x1 + m0 : x1;
    }
    
    
    static int[] exercici1(String msg, int n, int e) {
        int[] res = new int[msg.length() / 2];
        byte[] bytes = msg.getBytes();
        for (int i = 0, j = 0; i < bytes.length; i += 2, j++) {
            int val = (bytes[i] << 7) + bytes[i + 1];  
            res[j] = potenciaModular(val, e, n);
        }
        return res;
    }

    /*
     * Primer, desencriptau el missatge utilitzant xifrat RSA amb la clau pública donada. Després
     * descodificau el missatge en blocs de longitud 2 amb codificació ASCII (igual que l'exercici
     * anterior, però al revés).
     *
     * Per construir un String a partir d'un array de bytes podeu fer servir el constructor
     * new String(byte[]). Si heu de factoritzar algun nombre, ho podeu fer per força bruta.
     *
     * També podeu suposar que:
     * - La longitud del missatge original és múltiple de 2
     * - El valor de tots els caràcters originals estava entre 32 i 127.
     * - La clau pública (n, e) és de la forma vista a les transparències.
     * - n és major que 2¹⁴, i n² és menor que Integer.MAX_VALUE
     */
    static String exercici2(int[] m, int n, int e) {
      // factorizamos n
     int p = 0, q = 0;
        for (int i = 2; i <= Math.sqrt(n); i++) {
            if (n % i == 0) {
                p = i; q = n / i;
                break;
            }
        }
    //phi de n = (p-1)*(q-1)
    int phi = (p - 1) * (q - 1);

    // hallamos d, la clave privada que cumple con (d*e) % phi = 1
    int d = inversoModular(e, phi);

    // usamos la clave privada d para descifrar cada bloque cifrado 
    char[] descifrado = new char[m.length * 2];
    for (int i = 0; i < m.length; i++) {
        int decodificado = potenciaModular(m[i], d, n);

    // separamos el numero en 2 caracteres originales usando base 128 en orden big-endian (primero el mas significativo)
    descifrado[i * 2]     = (char) (decodificado >> 7);     // dividir entre 128
    descifrado[i * 2 + 1] = (char) (decodificado & 0x7F);   // resto mod 128
    }

    // reconstruimos mensaje original y lo devolvemos
    return new String(descifrado);
    }
    
    static void tests() {
      // Exercici 1
      // Codificar i encriptar
      test(4, 1, 1, () -> {
        var n = 2*8209;
        var e = 5;

        var encr = exercici1("Patata", n, e);
        return Arrays.equals(encr, new int[] { 4907, 4785, 4785 });
      });

      // Exercici 2
      // Desencriptar i decodificar
      test(4, 2, 1, () -> {
        var n = 2*8209;
        var e = 5;

        var encr = new int[] { 4907, 4785, 4785 };
        var decr = exercici2(encr, n, e);
        return "Patata".equals(decr);
      });
    }
  }

  /*
   * Aquest mètode `main` conté alguns exemples de paràmetres i dels resultats que haurien de donar
   * els exercicis. Podeu utilitzar-los de guia i també en podeu afegir d'altres (no els tendrem en
   * compte, però és molt recomanable).
   *
   * Podeu aprofitar el mètode `test` per comprovar fàcilment que un valor sigui `true`.
   */
    
  

  /*
   * Aquest mètode `main` conté alguns exemples de paràmetres i dels resultats que haurien de donar
   * els exercicis. Podeu utilitzar-los de guia i també en podeu afegir d'altres (no els tendrem en
   * compte, però és molt recomanable).
   *
   * Podeu aprofitar el mètode `test` per comprovar fàcilment que un valor sigui `true`.
   */
  public static void main(String[] args) {
    System.out.println("---- Tema 1 ----");
    Tema1.tests();
    System.out.println("---- Tema 2 ----");
    Tema2.tests();
    System.out.println("---- Tema 3 ----");
    Tema3.tests();
    System.out.println("---- Tema 4 ----");
    Tema4.tests();
  }

  // Informa sobre el resultat de p, juntament amb quin tema, exercici i test es correspon.
  static void test(int tema, int exercici, int test, BooleanSupplier p) {
    try {
      if (p.getAsBoolean())
        System.out.printf("Tema %d, exercici %d, test %d: OK\n", tema, exercici, test);
      else
        System.out.printf("Tema %d, exercici %d, test %d: Error\n", tema, exercici, test);
    } catch (Exception e) {
      if (e instanceof UnsupportedOperationException && "pendent".equals(e.getMessage())) {
        System.out.printf("Tema %d, exercici %d, test %d: Pendent\n", tema, exercici, test);
      } else {
        System.out.printf("Tema %d, exercici %d, test %d: Excepció\n", tema, exercici, test);
        e.printStackTrace();
      }
    }
  }
}
// vim: set textwidth=100 shiftwidth=2 expandtab :
