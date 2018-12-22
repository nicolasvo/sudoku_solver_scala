import scala.annotation.tailrec

class Sudoku(size: Int = 0, hints_matrix: Array[Array[Int]]=Array(), sudoku_matrix: Array[Array[Int]]=Array()) {

  var grid_size = 0
  var sub_grid_size: Int = 0

  var hints = hints_matrix

  var sudoku_grid =  Array.fill[Array[Int]](size)(Array.fill[Int](size)(0))

  var indices_grid = Array.fill[Array[Int]](grid_size*grid_size-hints.length)(Array.fill[Int](2)(0))
  var indices_grid_list = Array.fill[Array[Int]](grid_size*grid_size-hints.length)(Array.fill[Int](2)(0))
  var iter = 0

  var map_possible = collection.mutable.Map[(Int, Int), List[Int]]()

  if (sudoku_matrix.length==0){
    // Regarde si la taille du sudoku est valide
    if (size != 4 && size != 9 && size != 16) {
      print("Error: The grid size is not valid! Should be 4, 9 or 16.")

    }

    // Variables qui contiennent la taille du sudoku et la taille des sous-carres
    grid_size = size
    sub_grid_size = Math.sqrt(size).toInt

    hints = hints_matrix

    // La grille du sudoku
    sudoku_grid = Array.fill[Array[Int]](size)(Array.fill[Int](size)(0))

    // Variables utilisees pour le choix de la case aleatoire
    indices_grid = Array.fill[Array[Int]](grid_size*grid_size-hints.length)(Array.fill[Int](2)(0))

    var r=0
    for (i <- 0 until grid_size){
      for (j <- 0 until grid_size){
        if (check_if_hint(i,j)){
        }else{
          indices_grid(r)=Array(i,j)
          r+=1
        }
      }
    }

    val indices_grid_list = scala.util.Random.shuffle(indices_grid.toList)

    var iter = 0

    // On remplit la grille avec les indices
    for (hint <- hints) {
      sudoku_grid(hint(0))(hint(1)) = hint(2)
    }

  }else{

    // La grille du sudoku
    sudoku_grid = sudoku_matrix

    // Variables qui contiennent la taille du sudoku et la taille des sous-carres
    grid_size = sudoku_grid.length
    sub_grid_size = Math.sqrt(grid_size).toInt

    if (grid_size != 4 && grid_size != 9 && grid_size != 16) {
      print("Error: The grid size is not valid! Should be 4, 9 or 16.")

    }

    var number_of_hint = 0

    for (i <- 0 until grid_size){
      for (j <- 0 until grid_size){
        if (sudoku_grid(i)(j)!=0){
          number_of_hint+=1
        }
      }
    }

    hints =  Array.fill[Array[Int]](number_of_hint)(Array.fill[Int](3)(0))
    number_of_hint = 0

    for (i <- 0 until grid_size){
      for (j <- 0 until grid_size){
        if (sudoku_grid(i)(j)!=0){
          hints(number_of_hint)=Array(i,j, sudoku_grid(i)(j))
          number_of_hint+=1
        }
      }
    }
  }

  var x_first_indice=0
  var y_first_indice=0

  def get_first_indices(): Unit= {
    for (i <- 0 until grid_size) {
      for (j <- 0 until grid_size) {
        if (sudoku_grid(i)(j) == 0) {
          x_first_indice=i
          y_first_indice=j
          return
        }
      }
    }
  }

  get_first_indices()


  def show_grid(): Unit = {
    var i=0
    var j=0
    for (a <- sudoku_grid) {
      print(" ")
      for (b <- a) {
        if (b == 0) {
          print("-")
        } else {
          print(b)
        }
        print(" ")
        i += 1
        if (i % sub_grid_size == 0 && i < grid_size - 1) {
          print("| ")
        }
      }
      i = 0
      j+=1
      println(" ")
      if(j % sub_grid_size == 0 && j < grid_size - 1){
        for (f <- 0 until 2*grid_size + 3*(sub_grid_size-1)) {
          print("-")
        }
        println(" ")
      }
    }
  }

  def check_line(n: Int, x: Int, y: Int, i: Int): Boolean = {

    if (i == grid_size){ true}
    else if ( i == y ){ check_line(n, x, y, i + 1)}
    else if ( sudoku_grid(x)(i) == n ){ false}
    else{ check_line(n, x, y, i + 1)}
  }

  def check_column(n: Int, x: Int, y: Int, j: Int): Boolean ={

    if (j == grid_size){ true}
    else if ( j == x ){ check_column(n, x, y, j + 1)}
    else if ( sudoku_grid(j)(y) == n ){ false}
    else{ check_column(n, x, y, j + 1)}
  }

  def check_square(n: Int, x: Int, y: Int, i: Int, j: Int): Boolean = {
    if (i == sub_grid_size){ true}
    else if (sudoku_grid(i + sub_grid_size * (x / sub_grid_size))(j + sub_grid_size * (y / sub_grid_size)) == n && !(i == x - (x / sub_grid_size) * sub_grid_size && j == y - (y / sub_grid_size) * sub_grid_size) ){ false}
    else if (j != sub_grid_size - 1 ){ check_square(n, x, y, i, j + 1)}
    else{ check_square(n, x, y, i + 1, 0)}
  }

  // Regarde si une case du Sudoku est correct en examinant les lignes, colonnes et le carre
  def check_number(x: Int, y: Int): Boolean ={

    check_line(sudoku_grid(x)(y), x, y, 0) && check_column (sudoku_grid(x)(y), x, y, 0) && check_square(sudoku_grid(x)(y), x, y, 0, 0)

  }

  // Regarde si le sudoku est correct en examinant toutes les cases
  def check_sudoku(): Boolean ={

    var x=0
    var y=0
    for (a <- sudoku_grid) {
      for (b <- a) {
        if (b==0){
          println("The sudoku is not completed yet...")
          return false
        }
        if (check_number(x, y)){
          y+=1
        }else{
          println("The sudoku is wrong, it has some mistakes...")
          return false
        }
      }
      x+=1
      y=0
    }
    println("The sudoku is correct!")
    true
  }

  // Regarde si la case dont les coordonnees passees en parametres est un indice du sudoku
  def check_if_hint(x: Int, y: Int): Boolean ={
    for (a <- hints) {
      if(a(0)==x && a(1)==y){
        return true
      }
    }
    false
  }

  // Fonction pour recuperer la prochaine case (de gauche a droite, haut en bas)
  def get_next_indices(x: Int, y: Int): Array[Int] = {
    if (y<grid_size-1){

      if (check_if_hint(x,y+1)){
        if (x==grid_size-1 && y==grid_size-2){
          return Array(grid_size-1,grid_size-1)
        }
        return get_next_indices(x,y+1)
      }
      else {
        return Array(x,y+1)
      }
    }
    if (check_if_hint(x+1, 0)){
      get_next_indices(x+1, 0)
    }
    else {
      Array(x+1, 0)
    }
  }

  // Fonction pour recuperer la case precedente (de droite a gauche, bas en haut)
  @tailrec
  final def get_previous_indices(x: Int, y: Int): Array[Int] = {
    if (y==0) {
      if (check_if_hint(x-1, grid_size-1)) {get_previous_indices(x-1, grid_size-1)}
      else {return Array(x-1, grid_size-1)}
    } else {
      if (check_if_hint(x, y-1)) {get_previous_indices(x, y-1)}
      else {return Array(x, y-1)}
    }
  }



//    if (y==0){
//      if (check_if_hint(x-1, grid_size-1)){
//        return get_previous_indices(x - 1, grid_size - 1)
//      }
//      else {
//        return Array(x - 1, grid_size - 1)
//      }
//    }
//
//    if (check_if_hint(x, y-1)){
//      get_previous_indices(x, y-1)
//    }
//    else {
//      Array(x, y-1)
//    }
//  }


  def get_next_value(x: Int, y: Int): Int = {
    val tuple = (x, y)
    if (map_possible.contains(tuple)) {
      if (!(map_possible(tuple).isEmpty)) {
        val init :+ last = map_possible(tuple)
        // Pop value from indice list of possible values
        map_possible.update(tuple, init)
        //println("... There: " + last)
        return last
      } else {
        // If no value left, backtrack
        map_possible = map_possible - tuple
        return 666
      }
    } else {
      map_possible += (tuple -> scala.util.Random.shuffle(List.range(1, size+1)))
      val init :+ last = map_possible(tuple)
      // Pop value from list of possible values
      map_possible.update(tuple, init)
      //println("... Here: " + last)
      return last
    }
  }

  // Fonctions du solveur de sudoku avec choix des cases en commençant par la première case
  
  def backtrack(x: Int, y: Int): Array[Int] = {
    //sudoku_grid(x)(y)=0;
    val arr = get_previous_indices(x, y);
    return arr
    //backtrack(arr(0), arr(1))
  }

//  @tailrec
//  final def solve_sudoku(x: Int = x_first_indice, y: Int = y_first_indice): Unit = {
//    // Si la case du sudoku est vide (=0) on la met a 1
//    if (sudoku_grid(x)(y)==0){ sudoku_grid(x)(y)=1; solve_sudoku(x, y)}
//    // Si la derniere case du sudoku satisfait les contraintes
//    else if (x==grid_size-1 && y==grid_size-1 && check_number(x, y)){ println("Sudoku solved!")}
//    // Si la case satisfait les contraintes
//    else if ( check_number(x, y)){val arr = get_next_indices(x,y); solve_sudoku(arr(0), arr(1))}
//    // Si la case n'est pas a la valeur maximum (et la case ne satisfait pas les contraintes)
//    else if (sudoku_grid(x)(y)<grid_size){sudoku_grid(x)(y)+=1; solve_sudoku(x, y)}
//    // Erreur rebouclage case initiale
//    else if (x==x_first_indice && y==y_first_indice){println("The Sudoku is wrong!!!")}
//    // Il faut retourner en arriere
//    else{sudoku_grid(x)(y)=0; val arr2 = get_previous_indices(x, y); val arr = backtrack(arr2(0),arr2(1)); solve_sudoku(arr(0), arr(1))}
//  }
  
  @tailrec
  final def solve_sudoku(x: Int = x_first_indice, y: Int = y_first_indice): Unit = {
    // Backtracking
    if (sudoku_grid(x)(y)==666) {
      sudoku_grid(x)(y)=0;
      if (x==x_first_indice && y==y_first_indice) {println("The puzzle is wrong!!!"); return false}
      //println("... FF VIII")
      val array_previous_indice = get_previous_indices(x, y)
      //println("... FF IX: " + array_previous_indice(0), array_previous_indice(1))
      //val array_backtrack = backtrack(array_previous_indice(0), array_previous_indice(1))
      //println(" ... Backtracking to: " + (array_backtrack(0), array_backtrack(1)))
      val x_previous = array_previous_indice(0)
      val y_previous = array_previous_indice(1)
      //println("... Previous indices: " + (x_previous, y_previous))
      sudoku_grid(x_previous)(y_previous) = get_next_value(x_previous, y_previous)
      solve_sudoku(x_previous, y_previous)

    }
    else if (sudoku_grid(x)(y)==0) {
      sudoku_grid(x)(y)=get_next_value(x, y);
      //println("... Grid under construction")
      //show_grid()
      solve_sudoku(x, y);
    }
    else if (check_number(x, y)) {
      //println("... N E X T")
      if (x==grid_size-1 && y==grid_size-1) {println("FINALLY OVER."); return true}
      val array_next_indice = get_next_indices(x, y)
      solve_sudoku(array_next_indice(0), array_next_indice(1))
    }
    else if (!check_number(x, y)) {
      //println("... Try again" + (x, y))
      //println("List: " + map_possible(x, y))
      val next_value = get_next_value(x, y)
      sudoku_grid(x)(y)=next_value
      //println("... Next value: " + next_value)
      
      solve_sudoku(x, y)
    }
  }

  // Generate a new sudoku
  def generate_sudoku(size: Int): Array[Array[Int]] = {
    sudoku_grid = Array.ofDim[Int](size, size)
    solve_sudoku()
    println("... New grid")
    show_grid()
    // Randomly remove numbers from the grid (set them to 0)
    for ((row, i) <- sudoku_grid.zipWithIndex) {
      //print("... boi")
      val list_remove = scala.util.Random.shuffle(List.range(0, size)).take(size/2)
      //val list_remove = scala.util.Random.shuffle(List.range(0, size)).take(1)
      //println(i + " : " + list_remove)
      for (j <- list_remove) {
        sudoku_grid(i)(j) = 0
      }
    }
    //println("... Randomly remove")
    //show_grid()
    //println()
    return sudoku_grid
  }

}

// Sudoku 9*9
val a = new Sudoku(size=9, sudoku_matrix= Array(
  Array(1,0,0,0,2,8,6,0,4),
  Array(0,0,0,5,4,7,1,0,2),
  Array(7,4,2,6,1,0,0,0,0),
  Array(4,0,0,2,0,5,0,3,1),
  Array(0,2,0,1,0,0,8,6,9),
  Array(9,0,0,8,6,0,0,2,5),
  Array(5,0,0,9,3,0,0,4,7),
  Array(0,9,7,4,8,1,0,0,0),
  Array(6,0,4,7,0,0,9,1,0),
))
println("... Dolce")
a.show_grid()
a.solve_sudoku()
a.show_grid()

val b = new Sudoku(size=9)
val new_grid = b.generate_sudoku(size=9)
val c = new Sudoku(size=9, sudoku_matrix = new_grid)
println("L U K E .")
c.show_grid()
c.solve_sudoku()
c.show_grid()
