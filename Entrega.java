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
  static final String[] NOMS = {Pedro Gelabert, Abde Afkir, Amine Karab};

  /*
   * Aquí teniu els exercicis del Tema 1 (Lògica).
   */
  static class Tema1 {
    /*
     * Determinau si l'expressió és una tautologia o no:
     *
     * (((vars[0] ops[0] vars[1]) ops[1] vars[2]) ops[2] vars[3]) ...
     *
     * Aquí, vars.length == ops.length+1, i cap dels dos arrays és buid. Podeu suposar que els
     * identificadors de les variables van de 0 a N-1, i tenim N variables diferents (mai més de 20
     * variables).
     *
     * Cada ops[i] pot ser: CONJ, DISJ, IMPL, NAND.
     *
     * Retornau:
     *   1 si és una tautologia
     *   0 si és una contradicció
     *   -1 en qualsevol altre cas.
     *
     * Vegeu els tests per exemples.
     */
    static final char CONJ = '∧';
    static final char DISJ = '∨';
    static final char IMPL = '→';
    static final char NAND = '.';

    static int exercici1(char[] ops, int[] vars) {
      throw new UnsupportedOperationException("pendent");
    }

    /*
     * Aquest mètode té de paràmetre l'univers (representat com un array) i els predicats
     * adients `p` i `q`. Per avaluar aquest predicat, si `x` és un element de l'univers, podeu
     * fer-ho com `p.test(x)`, que té com resultat un booleà (true si `P(x)` és cert).
     *
     * Amb l'univers i els predicats `p` i `q` donats, returnau true si la següent proposició és
     * certa.
     *
     * (∀x : P(x)) <-> (∃!x : Q(x))
     */
    static boolean exercici2(int[] universe, Predicate<Integer> p, Predicate<Integer> q) {
      throw new UnsupportedOperationException("pendent");
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
      static int exercici1(int[] a) {
          int n = a.length;
          int[][] S = new int[n + 1][n + 1];
          S[0][0] = 1;
          for (int i = 1; i <= n; i++) {
              for (int k = 1; k <= i; k++) {
                  // S[i][k] = S(i-1, k-1) + k * S(i-1, k)
                  S[i][k] = S[i - 1][k - 1] + k * S[i - 1][k];
              }
          }
          int bell = 0;
          // Suma de los Números de Stirling S(n, k) para obtener B_n.
          for (int x : S[n]) {
              bell += x;
          }
          return bell;
      }
      
     
      static int exercici2(int[] a, int[][] rel) {
          int n = a.length;
          boolean[][] R = new boolean[n][n];

          // Inicializar relación R.
          for (int[] p : rel) {
              int i = indexOf(a, p[0]);
              int j = indexOf(a, p[1]);
              if (i >= 0 && j >= 0) {
                  R[i][j] = true;
              }
          }

          // Añadir reflexividad.
          for (int i = 0; i < n; i++) {
              R[i][i] = true;
          }

          // Calcular cierre transitivo (Floyd-Warshall).
          for (int k = 0; k < n; k++) {
              for (int i = 0; i < n; i++) {
                  if (R[i][k]) {
                      for (int j = 0; j < n; j++) {
                          if (R[k][j]) {
                              R[i][j] = true;
                          }
                      }
                  }
              }
          }

          // Comprobar antisimetría.
          for (int i = 0; i < n; i++) {
              for (int j = 0; j < n; j++) {
                  if (i != j && R[i][j] && R[j][i]) {
                      return -1; // No es antisimétrica.
                  }
              }
          }

          // Contar elementos en la relación final.
          int count = 0;
          for (boolean[] row : R) {
              for (boolean b : row) {
                  if (b) {
                      count++;
                  }
              }
          }
          return count;
      }
    
      static Integer exercici3(int[] a, int[][] rel, int[] x, boolean op) {
          int n = a.length;
          boolean[][] R = new boolean[n][n];

          // Inicializar R con las relaciones.
          for (int[] p : rel) {
              int i = indexOf(a, p[0]);
              int j = indexOf(a, p[1]);
              if (i >= 0 && j >= 0) {
                  R[i][j] = true;
              }
          }

          // Añadir reflexividad.
          for (int i = 0; i < n; i++) {
              R[i][i] = true;
          }

          // Calcular cierre transitivo.
          for (int k = 0; k < n; k++) {
              for (int i = 0; i < n; i++) {
                  if (R[i][k]) {
                      for (int j = 0; j < n; j++) {
                          if (R[k][j]) {
                              R[i][j] = true;
                          }
                      }
                  }
              }
          }

          // Mapear elementos de 'x' a índices.
          List<Integer> xs = new ArrayList<>();
          for (int v : x) {
              int idx = indexOf(a, v);
              if (idx >= 0) {
                  xs.add(idx);
              }
          }

          List<Integer> LB = new ArrayList<>(), UB = new ArrayList<>();

          // Encontrar todas las cotas inferiores (LB) y superiores (UB).
          for (int i = 0; i < n; i++) {
              boolean lower = true, upper = true;
              for (int e : xs) {
                  if (!R[i][e]) {
                      lower = false;
                  }
                  if (!R[e][i]) {
                      upper = false;
                  }
                  if (!lower && !upper) {
                      break;
                  }
              }
              if (lower) {
                  LB.add(i);
              }
              if (upper) {
                  UB.add(i);
              }
          }

          if (!op) { // Buscar GLB (Greatest Lower Bound).
              for (int p : LB) {
                  boolean isGLB = true;
                  for (int q : LB) {
                      if (!R[q][p]) {
                          isGLB = false;
                          break;
                      }
                  }
                  if (isGLB) {
                      return a[p];
                  }
              }
          } else { // Buscar LUB (Least Upper Bound).
              for (int p : UB) {
                  boolean isLUB = true;
                  for (int q : UB) {
                      if (!R[p][q]) {
                          isLUB = false;
                          break;
                      }
                  }
                  if (isLUB) {
                      return a[p];
                  }
              }
          }
          return null; // No se encontró GLB/LUB.
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
      static int[][] exercici4(int[] a, int[] b, Function<Integer, Integer> f) {
          int m = b.length;
          int[] counts = new int[m];
          List<int[]> images = new ArrayList<>();
          // Recollim totes les imatges x → f(x) i comptem preimatges per cada y∈b
          for (int xVal : a) {
              int yVal = f.apply(xVal);
              images.add(new int[]{xVal, yVal});
              int j = indexOf(b, yVal);
              if (j >= 0) {
                  counts[j]++;
              }
          }
          boolean injectiva = true;
          boolean sobreyectiva = true;
          for (int c : counts) {
              if (c == 0) {
                  sobreyectiva = false;
              }
              if (c > 1) {
                  injectiva = false;
              }
          }
          // 1) Biyectiva → inversa exacta
          if (injectiva && sobreyectiva) {
              int[][] inv = new int[m][2];
              for (int i = 0; i < m; i++) {
                  inv[i] = new int[]{b[i], findPreimage(images, b[i])};
              }
              return lexSorted(inv);
          }
          // 2) Injectiva només → inversa per l'esquerra
          if (injectiva) {
              int[][] invL = new int[m][2];
              for (int i = 0; i < m; i++) {
                  invL[i] = new int[]{b[i], findPreimage(images, b[i])};
              }
              return lexSorted(invL);
          }
          // 3) Sobreyectiva només → inversa per la dreta
          if (sobreyectiva) {
              int[][] invR = new int[m][2];
              for (int i = 0; i < m; i++) {
                  invR[i] = new int[]{b[i], findPreimage(images, b[i])};
              }
              return lexSorted(invR);
          }
          // 4) Ni injectiva ni sobreyectiva → no hi ha inversa
          return null;
      }

// Helper: retorna algun x tal que f(x)=y, o un element arbitrari de a
      private static int findPreimage(List<int[]> images, int y) {
          for (int[] p : images) {
              if (p[1] == y) {
                  return p[0];
              }
          }
          return images.get(0)[0];
      }
        private static int indexOf(int[] arr, int value) {
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] == value) {
        return i;
    }
  }
  return -1;


    /*
     * Aquí teniu alguns exemples i proves relacionades amb aquests exercicis (vegeu `main`)
     */
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
    static boolean exercici1(int[][] g) {
          int n = g.length;
          boolean[] visited = new boolean[n];
          for (int v = 0; v < n; v++) {
              if (!visited[v]) {
                  if (dfsCycle(v, -1, g, visited)) {
                      return true;
                  }
              }
          }
          return false;
      }

      private static boolean dfsCycle(int v, int parent, int[][] g, boolean[] visited) {
          visited[v] = true;
          for (int nei : g[v]) {
              if (nei == parent) {
                  continue;
              }
              // Si l'element veí ja s'ha visitat, hi ha un cicle
              if (visited[nei]) {
                  return true;
              }
              // Si des d'aquest veí es detecta un cicle recursivament
              if (dfsCycle(nei, v, g, visited)) {
                  return true;
              }
          }
          return false;
      }

    /*
     * Determinau si els dos grafs són isomorfs. Podeu suposar que cap dels dos té ordre major que
     * 10.
     */
      static boolean exercici2(int[][] g1, int[][] g2) {
          int n = g1.length;
          if (n != g2.length) {
              return false;
          }

          int[] deg1 = new int[n], deg2 = new int[n];
          boolean[][] adj1 = new boolean[n][n], adj2 = new boolean[n][n];
          for (int i = 0; i < n; i++) {
              deg1[i] = g1[i].length;
              for (int j : g1[i]) {
                  adj1[i][j] = true;
                  adj1[j][i] = true; // Asegura que la matriz es simétrica para grafos no dirigidos
              }
              deg2[i] = g2[i].length;
              for (int j : g2[i]) {
                  adj2[i][j] = true;
              }
          }

          int[] perm = new int[n];
          boolean[] used = new boolean[n];
          return isoBacktrack(0, n, deg1, deg2, adj1, adj2, perm, used);
      }

      private static boolean isoBacktrack(int idx, int n,
              int[] deg1, int[] deg2,
              boolean[][] adj1, boolean[][] adj2,
              int[] perm, boolean[] used) {
          if (idx == n) {
              // Si hemos asignado una permutación para todos los vértices, hemos encontrado un isomorfismo.
              return true;
          }
          for (int j = 0; j < n; j++) {
              // Poda: Si el vértice j ya está usado o si los grados no coinciden, saltar.
              if (used[j] || deg1[idx] != deg2[j]) {
                  continue;
              }

              perm[idx] = j; // Asignar el vértice j de g2 al vértice idx de g1

              boolean ok = true;
              // Verificar la consistencia de las adyacencias con los vértices ya mapeados (0 a idx-1)
              for (int k = 0; k < idx; k++) {
                  // La adyacencia entre (idx, k) en g1 debe coincidir con la adyacencia entre (perm[idx], perm[k]) en g2.
                  if (adj1[idx][k] != adj2[perm[idx]][perm[k]]) {
                      ok = false;
                      break;
                  }
              }
              if (!ok) {
                  // Si la adyacencia no es consistente, esta permutación no es válida, probar con el siguiente j.
                  continue;
              }

              used[j] = true; // Marcar j como usado
              if (isoBacktrack(idx + 1, n, deg1, deg2, adj1, adj2, perm, used)) {
                  return true; // Si la recursión encuentra una solución, propagarla.
              }
              used[j] = false; // Backtrack: desmarcar j para explorar otras permutaciones.
          }
          return false; // No se encontró ninguna permutación válida para idx.
      }
    /*
     * Determinau si el graf `g` (no dirigit) és un arbre. Si ho és, retornau el seu recorregut en
     * postordre desde el vèrtex `r`. Sinó, retornau null;
     *
     * En cas de ser un arbre, assumiu que l'ordre dels fills vé donat per l'array de veïns de cada
     * vèrtex.
     */
      static int[] exercici3(int[][] g, int r) {
          int n = g.length;
          boolean[] visited = new boolean[n];
          List<Integer> post = new ArrayList<>();

          // Un grafo es un árbol si es acíclico y conexo.
          // Paso 1: DFS para detectar ciclos y construir el postorden.
          if (!dfsTree(r, -1, g, visited, post)) {
              return null; // Ciclo detectado, no es un árbol.
          }

          // Paso 2: Verificar la conexidad.
          for (boolean v : visited) {
              if (!v) {
                  return null; // No todos los nodos fueron visitados, no es un árbol.
              }
          }

          // Si es acíclico y conexo, es un árbol. Convertir postorden a array.
          int[] res = new int[post.size()];
          for (int i = 0; i < res.length; i++) {
              res[i] = post.get(i);
          }
          return res;
      }

      private static boolean dfsTree(int v, int parent,
              int[][] g, boolean[] visited, List<Integer> post) {
          visited[v] = true; // Marcar como visitado.

          for (int nei : g[v]) {
              if (nei == parent) {
                  continue; // Ignorar el padre.
              }

              if (visited[nei]) {
                  return false; // Vecino ya visitado (y no es el padre) -> ciclo.
              }

              if (!dfsTree(nei, v, g, visited, post)) {
                  return false; // Ciclo encontrado en un subárbol.
              }
          }

          post.add(v); // Añadir al postorden.
          return true; // No hay ciclo en este subárbol.
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
          // Nombre de files i columnes
          int rows = mapa.length, cols = mapa[0].length;
          // Índex lineal de l'origen 'O'
          int start = -1;
          // Cerquem 'O' i guardem la seva posició (fila * cols + columna)
          for (int i = 0; i < rows && start < 0; i++) {
              for (int j = 0; j < cols; j++) {
                  if (mapa[i][j] == 'O') {
                      start = i * cols + j;
                      break;  // aturem el bucle interior quan trobem 'O'
                  }
              }
          }
          // Si no hi ha cap 'O', retornem -1
          if (start < 0) {
              return -1;
          }

          // Mida màxima de la cua = total de cel·les
          int max = rows * cols;
          // q: cua per BFS, dist: distàncies inicialitzades a -1 (no visitat)
          int[] q = new int[max], dist = new int[max];
          Arrays.fill(dist, -1);
          // Variables per gestionar la cua: head = índex de capçalera, tail = índex de cua
          int head = 0, tail = 0;
          // Iniciem BFS des de 'start'
          q[tail++] = start;
          dist[start] = 0;

          // Moviments en 4 direccions: baix, dalt, dreta, esquerra
          int[][] dirs = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}};
          // Processam la cua fins buidar-la
          while (head < tail) {
              // Desencolem la cel·la actual
              int cur = q[head++];
              int cr = cur / cols, cc = cur % cols;  // fila i columna corresponents
              // Exploració dels veïns
              for (int[] d : dirs) {
                  int nr = cr + d[0], nc = cc + d[1];
                  // Saltar si surtim del mapa
                  if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) {
                      continue;
                  }
                  char c = mapa[nr][nc];
                  int idx = nr * cols + nc;  // índex lineal del veí
                  // Saltar si és mur '#' o ja visitat (dist[idx] != -1)
                  if (c == '#' || dist[idx] != -1) {
                      continue;
                  }
                  // Assignem la distància (dist del pare + 1)
                  dist[idx] = dist[cur] + 1;
                  // Si és destí 'D', retornem distància mínima trobada
                  if (c == 'D') {
                      return dist[idx];
                  }
                  // Enfilem aquest veí per continuar el BFS
                  q[tail++] = idx;
              }
          }
          // Si acabem i no trobem 'D', retornem -1
          return -1;
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
    // Els penjarem més envant

    static void tests() {
    }
  }

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
