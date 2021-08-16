package breeze.codegen
import scala.meta._

object CodegenExpandDemo {
  def main(args: Array[String]): Unit = {
    val input =
      source"""
               object Foo {
    // TODO: relax field requirement
    @expand
    implicit def implOps_SVT_SVT_eq_SVT_Field[T,
      @expand.args(OpAdd, OpSub, OpDiv, OpMod, OpPow) Op <: OpType](
        implicit @expand.sequence[Op](f.+(_, _), f.-(_, _), f./(_, _), f.%(_, _), f.pow(_, _)) op: Op): Op.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] =
      new Op.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] {
        def apply(a: SparseVector[T], b: SparseVector[T]): SparseVector[T] = {
          require(b.length == a.length, "Vectors must be the same length!")
          val f = implicitly[Field[T]]
          val result: VectorBuilder[T] = new VectorBuilder[T](a.length)
          var i: Int = 0
          while (i < a.length) {
            val r = op(a(i), b(i))
            if (r != f.zero)
              result.add(i, r)
            i += 1
          }
          result.toSparseVector(true, true)
        }
      }

       // TODO: relax field requirement
  @expand
  implicit def implOpSVT_Field_SVT[@expand.args(OpAdd, OpSub, OpDiv, OpMod, OpPow) Op <: OpType, T: Field: ClassTag](
      implicit @expand.sequence[Op]({ f.+(_, _) }, { f.-(_, _) }, { f./(_, _) }, { f.%(_, _) }, { f.pow(_, _) })
      op: Op.Impl2[T, T, T]): Op.Impl2[SparseVector[T], T, SparseVector[T]] = {

    new Op.Impl2[SparseVector[T], T, SparseVector[T]] {
      def apply(a: SparseVector[T], b: T): SparseVector[T] = {
        val result: VectorBuilder[T] = new VectorBuilder[T](a.length)
        val f = implicitly[Field[T]]
        var i: Int = 0
        while (i < a.length) {
          val r = op(a(i), b)
          if (r != f.zero)
            result.add(i, r)
          i += 1
        }
        result.toSparseVector(true, true)
      }
    }
  }

}
              """

    val out = CodegenExpand.processTree(input)
    println(out)

  }
}
