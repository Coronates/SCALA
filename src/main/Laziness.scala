import List._
import Stream._
trait Stream[+A] {


  //helper functions


  def toListRecursive: List[A] = this match {
    case Cons(h,t) => h() :: t().toListRecursive
    case _ => List()
  }

  /*
  The above solution will stack overflow for large streams, since it's
  not tail-recursive. Here is a tail-recursive implementation. At each
  step we cons onto the front of the `acc` list, which will result in the
  reverse of the stream. Then at the end we reverse the result to get the
  correct order again.
  */
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  /*
  In order to avoid the `reverse` at the end, we could write it using a
  mutable list buffer and an explicit loop instead. Note that the mutable
  list buffer never escapes our `toList` method, so this function is
  still _pure_.
  */
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h,t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  /*
    Create a new Stream[A] from taking the n first elements from this. We can achieve that by recursively
    calling take on the invoked tail of a cons cell. We make sure that the tail is not invoked unless
    we need to, by handling the special case where n == 1 separately. If n == 0, we can avoid looking
    at the stream at all.
  */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  /*
    Create a new Stream[A] from this, but ignore the n first elements. This can be achieved by recursively calling
    drop on the invoked tail of a cons cell. Note that the implementation is also tail recursive.
  */
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /*
  It's a common Scala style to write method calls without `.` notation, as in `t() takeWhile f`.
  */
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if f(h()) => cons(h(), t() takeWhile f)
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  /*
  Since `&&` is non-strict in its second argument, this terminates the traversal as soon as a nonmatching element is found.
  */
  def forAll(f: A => Boolean): Boolean =
    foldRight(true)((a,b) => f(a) && b)

  def takeWhile_1(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) =>
      if (f(h)) cons(h,t)
      else      empty)

  def headOption: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h,t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) =>
      if (f(h)) cons(h, t)
      else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)

}


object Stream{
  val ones: Stream[Int] =  cons(1, ones)


  def main(args: Array[String]): Unit =
    println(ones.take(5))


  def empty [A]:Stream[A]= empty

  def cons[A](hd: => A, t1: => Stream[A]): Stream[A]={
    lazy val head = hd
    lazy val tail = t1
    Cons(()=>head,()=>tail)
  }
  def headOption:Option[A]=this match{
    case Empty => None
    case cons(h,t)=>Some(h())
  }


}