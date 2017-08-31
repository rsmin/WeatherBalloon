import UnitConvert._

/** Compute the statistics based on the log data from balloon
  * @return print out the staticstics on console
  * @author Xiaodong Meng
*/

object Static {
/* main entrance of the application. It takes one arguement of input file path*/
  def main(args: Array[String]): Unit = {
    println("Usage of Static:")
    println("please input data path as the input parameter")
    var filename = "rawDataFromBalloon.txt"

    if(args.length>=1)
      filename = args(0)

    val inputFile = new FileRead(filename)
    val static = new Static
    inputFile.lines.foreach(static.load)

    println(static)
  }
}

/*this class computes the statistics for each input record.
The statistics inludes the minnial temperature, max temperature, average temperature, the distance that the balloon travels as well as the number of observation points the balloon visited.
 */
class Static {
  private val delimiter = '|'
  private var minTemperature: Float = Float.PositiveInfinity
  private var maxTemperature: Float = Float.NegativeInfinity
  private var totalTemperature: Float = 0
  private var meanTemperature: Float = 0F
  private var numTemperatureRecord: Long = 0L
  private var numObervation: Long = 0L
  private var totalDistance: Double = 0F
  private var lastLocation: (Float, Float) = (0F, 0F)

  /**this method computes the distance that balloon travels from the last location
  *@param tuple2 is the co-ordinate location of the records in the formate of(x,y)
   */
  private def distanceCompute(tuple2: (Float, Float)) = {
    val travelDistance = math.sqrt(math.pow(tuple2._1 - lastLocation._1, 2) + math.pow(tuple2._2 - lastLocation._2, 2))
    totalDistance = totalDistance + travelDistance

    lastLocation = tuple2
    numObervation = numObervation + 1
  }

  /**this method computes temperate ralated values
    *@param temp the temperature that in the input records
    */
  private def temperatureCompute(temp: Float) = {
    if (temp < minTemperature)
      minTemperature = temp
    if (temp > maxTemperature)
      maxTemperature = temp
    totalTemperature = totalTemperature + temp
    numTemperatureRecord = numTemperatureRecord + 1
    meanTemperature = totalTemperature / numTemperatureRecord
  }

  /**parse a input string and process the data
  *@param line one record read from balloon log
   */
  def load(line: String): Unit = {
    val pattern = line.split(delimiter)
    if (pattern.length != 4) {
      println("record: " + line + " is broken")
      return Unit
    }
    val time = pattern(0)
    val location = pattern(1)
    val temp = pattern(2)
    val obs = pattern(3)
    if (obs != "Null" && temp != "Null") {
      val tempInC = convertTemperature(obs2TempType(obs), 0, temp.toFloat)
      temperatureCompute(tempInC)
    }

    if (obs != "Null" && time != "Null" && location != "Null") {
      val locationArray = location.split(',')
      if (locationArray.length == 2) {
        val locationTup = (locationArray(0).toFloat, locationArray(1).toFloat)
        val locationTupinKm = (convertDistance(obs2DisType(obs), 0, locationTup._1), convertDistance(obs2DisType(obs), 0, locationTup._2))

        distanceCompute(locationTupinKm)
      }
    }
  }

  def getMaxTemperature: String = {
    maxTemperature.toString
  }

  def getMinTemperature: String = {
    minTemperature.toString
  }

  def getMeanTemperature: String = {
    maxTemperature.toString
  }

  def getNumberOfObs: String = {
    numObervation.toString
  }

  def getTotalDistance: String = {
    totalDistance.toString
  }

  /*organizing all statistic results into a string*/
  override def toString: String = {
    val buildOutput = new StringBuilder
    buildOutput.append("The following is the statistics: \n\r")
    buildOutput.append("The max temperature is: " + getMaxTemperature + "\n\r")
    buildOutput.append("The min temperature is: " + getMinTemperature + "\n\r")
    buildOutput.append("The mean temperature is: " + getMeanTemperature + "\n\r")
    buildOutput.append("The number of observations is: " + getNumberOfObs + "\n\r")
    buildOutput.append("The total distance is: " + getTotalDistance + "\n\r")
    buildOutput.toString()
  }
}