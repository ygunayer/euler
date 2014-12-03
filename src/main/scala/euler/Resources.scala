import scala.io.Source

package object euler {
  lazy val problem22Names = Source.fromURL(getClass.getResource("p022_names.txt"))
}