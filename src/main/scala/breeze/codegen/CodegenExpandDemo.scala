package breeze.codegen
import scala.meta._

object CodegenExpandDemo {
  def main(args: Array[String]): Unit = {
    val input =
      source"""
               object Foo {
 @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def m_m_UpdateOp[
      @expand.args(Long, BigInt, Complex) T,
      @expand.args(OpDiv, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ / _ },{ _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T]): BinaryUpdateRegistry[Matrix[T], Matrix[T], Op.type] =
    new BinaryUpdateRegistry[Matrix[T], Matrix[T], Op.type] {
      override def bindingMissing(a: Matrix[T], b: Matrix[T]): Unit = {
        var c = 0

        while (c < a.cols) {
          var r = 0
          while (r < a.rows) {
            a(r, c) = op(a(r, c), b(r, c))
            r += 1
          }
          c += 1
        }

      }
    }
}
              """

    val out = CodegenExpand.processTree(input)
    println(out)

  }
}
