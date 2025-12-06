import scala.io.{Source, Codec}
import scala.util.{Try, Success, Failure}

case class Booking(
                    id: String,
                    destinationCountry: String,
                    destinationCity: String,
                    noOfPeople: Int,
                    hotelName: String,
                    price: Double,
                    discount: Double,
                    profitMargin: Double
                  )

object DataLoader {
  implicit val codec: Codec = Codec("ISO-8859-1")

  def loadBookings(filename: String): List[Booking] = {
    Try(Source.fromFile(filename)) match {
      case Success(source) =>
        val lines = source.getLines().drop(1).toList
        source.close()
        lines.flatMap(DataParser.parseLine)
      case Failure(e) =>
        println(s"Error loading file '$filename': ${e.getMessage}")
        List.empty[Booking]
    }
  }
}

object DataParser {
  private def parseDouble(s: String): Double = Try(s.trim.toDouble).getOrElse(0.0)
  private def parsePercent(s: String): Double = Try(s.replace("%", "").trim.toDouble / 100.0).getOrElse(0.0)

  def parseLine(line: String): Option[Booking] = {
    val cols = line.split(",").map(_.trim)
    if (cols.length > 23) {
      Some(Booking(
        id = cols(0),
        destinationCountry = cols(9),
        destinationCity = cols(10),
        noOfPeople = Try(cols(11).toInt).getOrElse(1),
        hotelName = cols(16),
        price = parseDouble(cols(20)),
        discount = parsePercent(cols(21)),
        profitMargin = parseDouble(cols(23))
      ))
    } else None
  }
}

object MathUtils {
  def normalize(value: Double, min: Double, max: Double): Double = {
    if (max == min) 0.0 else (value - min) / (max - min)
  }
}

trait AnalysisStrategy {
  def analyze(data: List[Booking]): Unit
}

//Question
class CountryAnalysis extends AnalysisStrategy {
  def analyze(data: List[Booking]): Unit = {
    if (data.isEmpty) return
    val topCountry = data
      .groupBy(_.destinationCountry)
      .map { case (c, list) => (c, list.size) }
      .maxBy(_._2)

    //Question 1 output
    println(f"   Country        : ${topCountry._1}")
    println(f"   Total Bookings : ${topCountry._2}")
  }
}

