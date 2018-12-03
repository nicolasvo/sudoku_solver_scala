import scala.annotation.tailrec

class Sudoku(size: Int, hints_matrix: Array[Array[Int]]) {

  if (size != 4 && size != 9 && size != 16) {
    print("Error: The grid size is not valid! Should be 4, 9 or 16.")

  }

  val grid_size = size
  val sub_grid_size: Int = Math.sqrt(size).toInt

  val hints = hints_matrix

  var sudoku_grid = Array.fill[Array[Int]](size)(Array.fill[Int](size)(0))

  for (hint <- hints) {
    sudoku_grid(hint(0))(hint(1)) = hint(2)
  }

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

  def check_number(x: Int, y: Int): Boolean ={

    check_line(sudoku_grid(x)(y), x, y, 0) && check_column (sudoku_grid(x)(y), x, y, 0) && check_square(sudoku_grid(x)(y), x, y, 0, 0)

  }

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

  def get_next_indices(x: Int, y: Int): Array[Int] = {
    if (y<grid_size-1){
      if (check_if_hint(x,y+1)){
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

  def get_previous_indices(x: Int, y: Int): Array[Int] = {
    //printf("(%d %d) : %d", x, y, sudoku_grid(x)(y))
    //printf("    - %d",sudoku_grid(0)(0))
    //println(" ")
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

  def check_if_hint(x: Int, y: Int): Boolean ={
    for (a <- hints) {
      if(a(0)==x && a(1)==y){
        return true
      }
    }
    false
  }

  @tailrec
  final def backtrack(x: Int, y: Int): Array[Int] = {
    // min() possible
    // 1ere ligne fait buguer?
    if (sudoku_grid(x)(y)>=grid_size ){sudoku_grid(x)(y)=0; val arr = get_previous_indices(x, y);  backtrack(arr(0), arr(1))}
    else {sudoku_grid(x)(y)+=1; Array(x,y)}
  }

  @tailrec
  final def solve_sudoku(x: Int, y: Int): Unit = {
    // Si la case du sudoku est vide (=0) on la met a 1
    if (sudoku_grid(x)(y)==0){ sudoku_grid(x)(y)=1; solve_sudoku(x, y)}
    // Si la derniere case du sudoku satisfait les contraintes
    else if (x==grid_size-1 && y==grid_size-1 && check_number(x, y)){ println("Sudoku solved!")}
    // Si la case satisfait les contraintes
    else if ( check_number(x, y)){val arr = get_next_indices(x,y); solve_sudoku(arr(0), arr(1))}
    // Si la case n'est pas a la valeur maximum (et la case ne satisfait pas les contraintes)
    else if (sudoku_grid(x)(y)<grid_size){sudoku_grid(x)(y)+=1; solve_sudoku(x, y)}
    // Il faut retourner en arriere
    else{println(" "); show_grid() ;println(' '); sudoku_grid(x)(y)=0; val arr2 = get_previous_indices(x, y); val arr = backtrack(arr2(0),arr2(1)); solve_sudoku(arr(0), arr(1))}
  }

}


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
sudoku_solver1.solve_sudoku(0,0)
sudoku_solver1.check_sudoku()
sudoku_solver1.show_grid()


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
sudoku_solver2.solve_sudoku(0,0)
sudoku_solver2.check_sudoku()
sudoku_solver2.show_grid()


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
sudoku_solver3.solve_sudoku(0,0)
sudoku_solver3.check_sudoku()
sudoku_solver3.show_grid()

