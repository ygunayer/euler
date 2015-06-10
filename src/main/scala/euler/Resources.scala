import scala.io.Source

package object euler {
  lazy val problem22Names = Source.fromURL(getClass.getResource("p022_names.txt"))
  lazy val problem42Words = Source.fromURL(getClass.getResource("p042_words.txt"))
}