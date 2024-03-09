import org.scalatest.flatspec.AnyFlatSpec

class A_StarSpec extends AnyFlatSpec {
  val experiments: Map[(Int, Int), List[Graph]] = (for {
    size <- 5 to 25 by 5
    colors <- 4 to 10
  } yield ((size, colors), get_experiments(size, colors))).toMap
}
