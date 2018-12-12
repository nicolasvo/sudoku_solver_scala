import scala.annotation.tailrec

class Sudoku(size: Int = 0, hints_matrix: Array[Array[Int]]=Array(), sudoku_matrix: Array[Array[Int]]=Array()) {

  var grid_size = 0
  var sub_grid_size: Int = 0

  var hints = hints_matrix

  var sudoku_grid =  Array.fill[Array[Int]](size)(Array.fill[Int](size)(0))

  var indices_grid = Array.fill[Array[Int]](grid_size*grid_size-hints.length)(Array.fill[Int](2)(0))
  var indices_grid_list = Array.fill[Array[Int]](grid_size*grid_size-hints.length)(Array.fill[Int](2)(0))
  var iter = 0

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

    if (size != 4 && size != 9 && size != 16) {
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

    var hints =  Array.fill[Array[Int]](number_of_hint)(Array.fill[Int](3)(0))
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
  def get_previous_indices(x: Int, y: Int): Array[Int] = {
    if (y==0){
      if (check_if_hint(x-1, grid_size-1)){
        return get_previous_indices(x - 1, grid_size - 1)
      }
      else {
        return Array(x - 1, grid_size - 1)
      }
    }

    if (check_if_hint(x, y-1)){
      get_previous_indices(x, y-1)
    }
    else {
      Array(x, y-1)
    }
  }

  // Fonction pour recuperer la prochaine case depuis la liste d'indice ordonnee aleatoirement
  def get_next_indices2(): Array[Int] = {
    iter+=1
    indices_grid_list(iter)
  }

  // Fonction pour recuperer la case precedente depuis la liste d'indice ordonnee aleatoirement
  def get_previous_indices2(): Array[Int] = {
    iter-=1
    indices_grid_list(iter)
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

  // Fonctions du solveur de sudoku avec choix des cases en commençant par la première case
  @tailrec
  final def backtrack(x: Int, y: Int): Array[Int] = {
    // min() possible
    if (sudoku_grid(x)(y)>=grid_size ){sudoku_grid(x)(y)=0; val arr = get_previous_indices(x, y);  backtrack(arr(0), arr(1))}
    else {sudoku_grid(x)(y)+=1; Array(x,y)}
  }

  @tailrec
  final def solve_sudoku(x: Int = x_first_indice, y: Int = y_first_indice): Unit = {

    // Si la case du sudoku est vide (=0) on la met a 1
    if (sudoku_grid(x)(y)==0){ sudoku_grid(x)(y)=1; solve_sudoku(x, y)}
    // Si la derniere case du sudoku satisfait les contraintes
    else if (x==grid_size-1 && y==grid_size-1 && check_number(x, y)){ println("Sudoku solved!")}
    // Si la case satisfait les contraintes
    else if ( check_number(x, y)){val arr = get_next_indices(x,y); solve_sudoku(arr(0), arr(1))}
    // Si la case n'est pas a la valeur maximum (et la case ne satisfait pas les contraintes)
    else if (sudoku_grid(x)(y)<grid_size){sudoku_grid(x)(y)+=1; solve_sudoku(x, y)}
    // Erreur rebouclage case initiale
    else if (x==x_first_indice && y==y_first_indice){println("The Sudoku is wrong!!!")}
    // Il faut retourner en arriere
    else{sudoku_grid(x)(y)=0; val arr2 = get_previous_indices(x, y); val arr = backtrack(arr2(0),arr2(1)); solve_sudoku(arr(0), arr(1))}
  }

  // Fonctions du solveur de sudoku avec choix des cases aléatoirement
  @tailrec
  final def backtrack2(x: Int, y: Int): Array[Int] = {
    // min() possible
    // 1ere ligne fait buguer?
    if (sudoku_grid(x)(y)>=grid_size ){sudoku_grid(x)(y)=0; val arr = get_previous_indices2();  backtrack2(arr(0), arr(1))}
    else {sudoku_grid(x)(y)+=1; Array(x,y)}
  }

  @tailrec
  final def solve_sudoku2(x: Int, y: Int): Unit = {

    // Si la case du sudoku est vide (=0) on la met a 1
    if (sudoku_grid(x)(y)==0){ sudoku_grid(x)(y)=1; solve_sudoku2(x, y)}
    // Si la derniere case du sudoku satisfait les contraintes
    else if (x==indices_grid_list.last(0) && y==indices_grid_list.last(1) && check_number(x, y)){ println("Sudoku solved!")}
    // Si la case satisfait les contraintes
    else if ( check_number(x, y)){val arr = get_next_indices2(); solve_sudoku2(arr(0), arr(1))}
    // Si la case n'est pas a la valeur maximum (et la case ne satisfait pas les contraintes)
    else if (sudoku_grid(x)(y)<grid_size){sudoku_grid(x)(y)+=1; solve_sudoku2(x, y)}
    // Il faut retourner en arriere
    else{sudoku_grid(x)(y)=0; val arr2 = get_previous_indices2(); val arr = backtrack2(arr2(0),arr2(1)); solve_sudoku2(arr(0), arr(1))}
  }

}

// Sudoku 4*4 wrong
var sudoku_solver_wrong = new Sudoku(4, Array(
  Array(0,0,1),
  Array(0,1,2),
  Array(0,2,3),
  Array(1,3,4),
))
sudoku_solver_wrong.show_grid()
sudoku_solver_wrong.check_sudoku()
sudoku_solver_wrong.solve_sudoku()
sudoku_solver_wrong.check_sudoku()
sudoku_solver_wrong.show_grid()

// Sudoku 4*4 wrong defines by the other way
var sudoku_solver_wrong2 = new Sudoku(sudoku_matrix= Array(
  Array(1,2,3,0),
  Array(0,0,0,4),
  Array(0,0,0,0),
  Array(0,0,0,0),
))
sudoku_solver_wrong2.show_grid()
sudoku_solver_wrong2.check_sudoku()
sudoku_solver_wrong2.solve_sudoku()
sudoku_solver_wrong2.check_sudoku()
sudoku_solver_wrong2.show_grid()

/*
// Sudoku 4*4 complet
var sudoku_solver0 = new Sudoku(4, Array(
  Array(0,0,1),
  Array(0,1,2),
  Array(0,2,4),
  Array(0,3,3),
  Array(1,0,4),
  Array(1,1,3),
  Array(1,2,1),
  Array(1,3,2),
  Array(2,0,3),
  Array(2,1,4),
  Array(2,2,2),
  Array(2,3,1),
  Array(3,0,2),
  Array(3,1,1),
  Array(3,2,3),
  Array(3,3,4)
))
sudoku_solver0.show_grid()
sudoku_solver0.check_sudoku()

// Sudoku 4*4
var sudoku_solver1 = new Sudoku(4, Array(
  Array(0,1,2),
  Array(0,2,4),
  Array(1,3,2),
  Array(2,0,3),
  Array(3,1,1),
  Array(3,2,3)
))
sudoku_solver1.show_grid()
sudoku_solver1.check_sudoku()
sudoku_solver1.solve_sudoku()
sudoku_solver1.check_sudoku()
sudoku_solver1.show_grid()

// Sudoku 9*9 facile
var sudoku_solver2 = new Sudoku(9, Array(
  Array(0,1,8),
  Array(0,3,9),
  Array(0,5,1),
  Array(0,7,5),
  Array(1,2,2),
  Array(1,3,6),
  Array(1,4,8),
  Array(1,5,7),
  Array(1,6,3),
  Array(2,2,3),
  Array(2,6,6),
  Array(3,0,3),
  Array(3,1,9),
  Array(3,7,6),
  Array(3,8,5),
  Array(4,0,6),
  Array(4,3,4),
  Array(4,4,7),
  Array(4,5,5),
  Array(4,8,3),
  Array(5,0,5),
  Array(5,1,7),
  Array(5,7,8),
  Array(5,8,4),
  Array(6,2,9),
  Array(6,6,8),
  Array(7,2,5),
  Array(7,3,1),
  Array(7,4,2),
  Array(7,5,4),
  Array(7,6,9),
  Array(8,1,4),
  Array(8,3,8),
  Array(8,5,3),
  Array(8,7,2)
))
sudoku_solver2.show_grid()
sudoku_solver2.check_sudoku()
//sudoku_solver2.solve_sudoku()
sudoku_solver2.solve_sudoku2(sudoku_solver2.indices_grid_list.head(0),sudoku_solver2.indices_grid_list.head(1))
sudoku_solver2.check_sudoku()
sudoku_solver2.show_grid()


// Sudoku 9*9 difficle
var sudoku_solver3 = new Sudoku(9, Array(
  Array(0,0,7),
  Array(0,6,4),
  Array(1,1,2),
  Array(1,4,7),
  Array(1,7,8),
  Array(2,2,3),
  Array(2,5,8),
  Array(2,8,9),
  Array(3,3,5),
  Array(3,6,3),
  Array(4,1,6),
  Array(4,4,2),
  Array(4,7,9),
  Array(5,2,1),
  Array(5,5,7),
  Array(5,8,6),
  Array(6,3,3),
  Array(6,6,9),
  Array(7,1,3),
  Array(7,4,4),
  Array(7,7,6),
  Array(8,2,9),
  Array(8,5,1),
  Array(8,8,5)
))
sudoku_solver3.show_grid()
sudoku_solver3.check_sudoku()
sudoku_solver3.solve_sudoku()
//sudoku_solver3.solve_sudoku2(sudoku_solver3.indices_grid_list.head(0),sudoku_solver3.indices_grid_list.head(1))
sudoku_solver3.check_sudoku()
sudoku_solver3.show_grid()
**/
