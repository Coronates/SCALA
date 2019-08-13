//sealed trait Stream[+A] {
//  def toList: List[A] = this match {
//    case Cons(h,t) => Cons(h(),t().toList)
//    case _ => List()
//  }
//
//
//}
//case object Empty extends Stream[Nothing]
//case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]
//
//object Stream{
//  val ones: Stream[Int] =  cons(1, ones)
//
//
//
//  def cons[A](hd: => A, t1: => Stream[A]): Stream[A]={
//    lazy val head = hd
//    lazy val tail = t1
//    Cons(()=>head,()=>tail)
//
//  def empty [A]:Stream[A]= empty
//
//    def apply[A](as: A*): Stream[A]=
//      if(as.isEmpty) empty else cons(as.head, apply(as.tail:_*))
//
//
//  }
//
//
//
//}