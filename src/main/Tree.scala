sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def arbol = Branch(Branch(Leaf(7), Branch(Leaf(28),Leaf(32))), Leaf(3))

//  def main(args: Array[String]): Unit =
//    println(sizeViaFold(arbol))
//    //println(depth(arbol))
//    println(maximumViaFold(arbol))
//    println(map(arbol)(_ * 2))

  def size[A](t: Tree[A]):Int = t match{
    case Leaf(_)=> 1
    case Branch(l,r)=>1+size(l)+size(r)

  }
  //3.26
  def maximum[A](t : Tree[Int]):Int= t match{
    case Leaf(x)=>x
    case Branch(l,r)=> maximum(l).max(maximum(r))
  }

  //3.27
  def depth(t: Tree[Int]):Int = t match{
    case Leaf(_)=> 0
    case Branch(l,r)=>1 + depth(l).max(depth(r))
  }
  //3.28
  def map[A,B](t: Tree[A])(f: A => B):Tree[B]= t match {
    case Leaf(a)=> Leaf(f(a))

    case Branch(l,r)=> Branch(map(l)(f),map(r)(f))
  }

  def fold[A,B](t : Tree[A])(f: A => B)(g: (B,B)=> B): B = t match{
    case Leaf(a)=> f(a)
    case Branch(l,r)=> g(fold(l)(f)(g),fold(r)(f)(g))

  }
  def sizeViaFold[A](t :Tree[A]):Int=
    fold(t)(a => 1)(1+_+_)

  def maximumViaFold(t : Tree[Int]): Int=
    fold(t)(a=>a)( (x,y)=> (x.max(y)))

}