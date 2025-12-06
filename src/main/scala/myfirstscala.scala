import scala.io.{Source, Codec}
import scala.util.{Try, Success, Failure}

// Defines structure for booking data
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
  //Handles special symbols in the CSV File
  implicit val codec: Codec = Codec("ISO-8859-1")
 //Loads the file safely using Try to handle function
  def loadBookings(filename: String): List[Booking] = {
    Try(Source.fromFile(filename)) match {
      case Success(source) =>
        val lines = source.getLines().drop(1).toList
        source.close()
        //Parse Line error Handling
        lines.flatMap(DataParser.parseLine)
      case Failure(e) =>
        println(s"Error loading file '$filename': ${e.getMessage}")
        List.empty[Booking]
    }
  }
}

object DataParser {
  //Converts Strings to doubles
  private def parseDouble(s: String): Double = Try(s.trim.toDouble).getOrElse(0.0)
  //Handles Percentage Strings
  private def parsePercent(s: String): Double = Try(s.replace("%", "").trim.toDouble / 100.0).getOrElse(0.0)

  // Turns a text line into a Booking object
  def parseLine(line: String): Option[Booking] = {
    val cols = line.split(",").map(_.trim)
    // Checks if the row has enough columns
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
  // Standard math formula to scale numbers between 0 and 1
  def normalize(value: Double, min: Double, max: Double): Double = {
    if (max == min) 0.0 else (value - min) / (max - min)
  }
}
// Template for all the analysis questions
trait AnalysisStrategy {
  def analyze(data: List[Booking]): Unit
}

//Question 1
class CountryAnalysis extends AnalysisStrategy {
  def analyze(data: List[Booking]): Unit = {
    if (data.isEmpty) return
    val topCountry = data
      .groupBy(_.destinationCountry)
      .map { case (c, list) => (c, list.size) }
      .maxBy(_._2)

    //Question 1 output
    println("\n1. Country with Highest Bookings")
    println("=" * 50)
    println(f"   Country        : ${topCountry._1}")
    println(f"   Total Bookings : ${topCountry._2}")
  }
}

class EconomicalAnalysis extends AnalysisStrategy {
  def analyze(data: List[Booking]): Unit = {
    if (data.isEmpty) return

    val stats = data
      .groupBy(b => (b.hotelName, b.destinationCountry, b.destinationCity))
      .map { case ((hotel, country, city), list) =>
        (
          hotel, country, city,
          list.map(_.price).sum / list.size,
          list.map(_.discount).sum / list.size,
          list.map(_.profitMargin).sum / list.size
        )
      }.toList

    val prices = stats.map(_._4)
    val discs = stats.map(_._5)
    val margins = stats.map(_._6)

    val (minP, maxP) = (prices.min, prices.max)
    val (minD, maxD) = (discs.min, discs.max)
    val (minM, maxM) = (margins.min, margins.max)

    val scored = stats.map { case (hotel, country, city, price, disc, margin) =>
      val priceScore = 1.0 - MathUtils.normalize(price, minP, maxP)
      val margScore = 1.0 - MathUtils.normalize(margin, minM, maxM)
      val discScore = MathUtils.normalize(disc, minD, maxD)

      val totalScore = (priceScore + discScore + margScore) / 3.0
      (hotel, country, city, totalScore)
    }

    //q2 output
    val winner = scored.maxBy(_._4)
    println("\n2. Most Economical Hotel Option")
    println("(Criteria: Avg Price (Low), Avg Discount (High), Avg Margin (Low))")
    println("=" * 50)
    println(f"   Destination Country : ${winner._2}")
    println(f"   Destination City    : ${winner._3}")
    println(f"   Hotel Name          : ${winner._1}")
    println(f"   Economical Score    : ${winner._4}%.4f")
  }
}

class ProfitableAnalysis extends AnalysisStrategy {
  def analyze(data: List[Booking]): Unit = {
    if (data.isEmpty) return

    val stats = data
      .groupBy(b => (b.hotelName, b.destinationCountry, b.destinationCity))
      .map { case ((hotel, country, city), list) =>
        (
          hotel, country, city,
          list.map(_.noOfPeople).sum,
          list.map(_.profitMargin).sum / list.size
        )
      }.toList

    val visitors = stats.map(_._4.toDouble)
    val margins = stats.map(_._5)
    val (minV, maxV) = (visitors.min, visitors.max)
    val (minM, maxM) = (margins.min, margins.max)

    val scored = stats.map { case (hotel, country, city, vis, marg) =>
      val visScore = MathUtils.normalize(vis.toDouble, minV, maxV)
      val margScore = MathUtils.normalize(marg, minM, maxM)
      val totalScore = (visScore + margScore) / 2.0
      (hotel, country, city, totalScore, vis, marg)
    }

    val winner = scored.maxBy(_._4)
    println("\n3. Most Profitable Hotel")
    println("(Criteria: High Visitor Volume & High Profit Margin)")
    println("=" * 50)
    println(f"   Destination Country : ${winner._2}")
    println(f"   Destination City    : ${winner._3}")
    println(f"   Hotel Name          : ${winner._1}")
    println(f"   Total Visitors      : ${winner._5}")
    println(f"   Avg Profit Margin   : ${winner._6 * 100}%.1f%%")

  }
}

object HotelAnalysisProgram {
  def main(args: Array[String]): Unit = {
    val filename = "Hotel_Dataset.csv"
    println("System: Initializing Data Load...")
    val bookings = DataLoader.loadBookings(filename)

    if (bookings.nonEmpty) {
      println(s"System: Loaded ${bookings.size} bookings successfully.")

      val strategies: List[AnalysisStrategy] = List(
        new CountryAnalysis(),
        new EconomicalAnalysis(),
        new ProfitableAnalysis()
      )

      strategies.foreach(_.analyze(bookings))
    } else {
      println("System: Aborting analysis due to data load failure.")
    }
  }
}