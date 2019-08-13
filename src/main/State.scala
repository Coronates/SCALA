


trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def main(args: Array[String]): Unit = {
    println(ints(9)(Simple(48)))
    //println()
    }

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

    def randomPair(rng: RNG): ((Int, Int), RNG) = {
      val (i1, rng2) = rng.nextInt
      val (i2, rng3) = rng2.nextInt
      ((i1, i2), rng3)
    }


    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, rng2) = rng.nextInt
      (if (i < 0) -i else i, rng2)
    }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    (i / Int.MaxValue.toDouble, rng2)
  }
  def double3(rng: RNG):((Double,Double,Double), RNG)={
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)

  }
  def doubleInt(rng: RNG): ((Double,Int), RNG)={
    val (d1, rng1)= double(rng)
    val(i1, rng2) = rng1.nextInt
    ((d1,i1),rng2)

  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG)={
      if (count == 0)
        (List(), rng)
      else {
        val (h, rng1) = rng.nextInt
        val (t, rng2) = ints(count - 1)(rng1)
//        (h::t, rng2)
        (Cons(h,t), rng2)
      }

  }




}





