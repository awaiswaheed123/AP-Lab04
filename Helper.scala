import scala.io.StdIn

object Helper {
  
  //Method for Populating the Matrices
  def populate(n : Int) = {
    def getUserInput(): Int = {
      print("Please Enter next Element: ")
          return StdIn.readInt()
    }
    def loop(acc: Vector[Vector[Int]], i: Int): Vector[Vector[Int]] = {
        if(i==n)
          return acc
        val v1 = for(j <- Vector.range( 0 , n)) 
          yield(getUserInput())
        return loop(acc :+ v1, i+1)
    }
    loop(Vector(),0)
  }
  
  //Method for Displaying the matrices
  def display(x:Vector[Vector[Int]]) = {
    for(y<-x){
      for(z<- y){
        print(z.toString + "		")
      }
      print("\n")
    }  
  }
  
  //Method to Matrix Addition
  def addMatrix(first: Vector[Vector[Int]],second: Vector[Vector[Int]]): Vector[Vector[Int]] = {
      val len = first(0).length
      def loop(acc: Vector[Vector[Int]] , i: Int): Vector[Vector[Int]] = {
          if(i==len)
              return acc
          val v1 = for(j <- Vector.range(0 , len))
              yield(first(i)(j)+second(i)(j))
          return loop(acc :+ v1, i+1)
      }
      loop(Vector(), 0)
  }
  
    //Method to Matrix Subtraction
  def subMatrix(first: Vector[Vector[Int]],second: Vector[Vector[Int]]): Vector[Vector[Int]] = {
      val len = first(0).length
      def loop(acc: Vector[Vector[Int]] , i: Int): Vector[Vector[Int]] = {
          if(i==len)
              return acc
          val v1 = for(j <- Vector.range(0 , len))
              yield(first(i)(j) - second(i)(j))
          return loop(acc :+ v1, i+1)
      }
      loop(Vector(), 0)
  }
  
  //Method to split The matrix according to the i and j
  def SplitMatrix(m1 : Vector[Vector[Int]], i: Int , j: Int): Vector[Vector[Int]] = {
     val len = m1.length
     val a11 = m1.slice(i , i+len/2)
     
    val a110 = for(k <- a11) yield (k.slice(j , j + len/2))
    a110
  }
  
  
  //Method to join Matrices into one
  def joinMatrix(a11:Vector[Vector[Int]],a12:Vector[Vector[Int]],a21:Vector[Vector[Int]],a22:Vector[Vector[Int]]):Vector[Vector[Int]] = {
    val len = a11.length
    val upper = for(i <- Vector.range(0 , len)) yield (a11(i)++a12(i))
    val lower = for(i <- Vector.range(0 , len)) yield (a21(i)++a22(i))
     upper ++ lower
  }
  
  def starssen(first: Vector[Vector[Int]],second: Vector[Vector[Int]]): Vector[Vector[Int]] = {
    val len = first.length
   if(len == 1)
     return Vector(Vector(first(0)(0)*second(0)(0)))
         
     //splitting first matrix
   val a11 = Helper.SplitMatrix(first, 0,0)
   val a12 = Helper.SplitMatrix(first, len/2 ,0)
   val a21 = Helper.SplitMatrix(first, 0, len/2)
   val a22 = Helper.SplitMatrix(first, len/2 , len/2)

   //Splitting second matrix
   val b11 = Helper.SplitMatrix(second, 0,0)
   val b12 = Helper.SplitMatrix(second, len/2 ,0)
   val b21 = Helper.SplitMatrix(second, 0, len/2)
   val b22 = Helper.SplitMatrix(second, len/2 , len/2)

   
   //Formulae
   val m1 = starssen(addMatrix(a11,a22), addMatrix(b11,b22))
   val m2 = starssen(addMatrix(a21,a22),b11)
   val m3 = starssen(a11,subMatrix(b12,b22))
   val m4 = starssen(a22,subMatrix(b21,b11))
   val m5 = starssen(addMatrix(a11,a12),b22)
   val m6 = starssen(subMatrix(a21,a11),addMatrix(b11,b12))
   val m7 = starssen(subMatrix(a12,a22),addMatrix(b21,b22))
    
   val c1 = addMatrix(subMatrix(addMatrix(m1,m4),m5),m7)
   val c3 = addMatrix(m3,m5) 
   val c2 = addMatrix(m2,m4)  
   val c4 = addMatrix(subMatrix(m1,m2),addMatrix(m3,m6))
   
   joinMatrix(c1,c2,c3,c4)
  }
}