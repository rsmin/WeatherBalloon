import java.util.Date
import scala.util.Random
import UnitConvert._

/** A weather balloon log data generator.
  *
  * @author Xiaodong Meng
  */


object DataGen {

  /** Entance of the simulator
    */
  def main(args: Array[String]): Unit = {
    println("Usage of DataGen:")
    println("-n       the Number records need to be generated. Default value is 5000000")
    println("-r       the Ratio of invalid records in the data file. Default value is 0.001")
    println("-f       the FileName of the data file. Default value is rawDataFromBalloon.txt")
    println("-u       the Upper bound of temperature. Default value is 50 degree")
    println("-l       the Lower bound of temperature. Default value is -50 degree")
    println("-a       the Area bound. Default value is 100 ")

    var numberOfRecords = 5000000L
    var invalidRecordRate = 0.001F
    var lowestCelsius = -50F
    var highestCelsius = 50F
    var areaBound = 100
    var fileName = "rawDataFromBalloon.txt"

    for (arg <- args) (
      arg match {
        case "-n" => numberOfRecords = args(args.indexOf("-n") + 1).toLong
        case "-e" => invalidRecordRate = args(args.indexOf("-e") + 1).toFloat
        case "-h" => highestCelsius = args(args.indexOf("-h") + 1).toFloat
        case "-l" => {
          assume(args(args.indexOf("-l") + 1).toFloat > -273.15, "The lowest temperatur in the universe is -273.15 degree")
          lowestCelsius = args(args.indexOf("-l") + 1).toFloat
        }
        case "-f" => fileName = args(args.indexOf("-f") + 1)
        case "-a" => areaBound = args(args.indexOf("-a") + 1).toInt
        case _ =>
      }
      )

    val generator = new DataGen(invalidRecordRate, lowestCelsius, highestCelsius, areaBound)
    val recordWriter = new FileWrite(fileName)
    for (i <- 0L until numberOfRecords)
      recordWriter.write(generator.toRecordString)
    recordWriter.close

  }

}

/** this class generate data based on the input parameters
  *
  * @param invalidRecordRate the number of percentage recodes that is invalid.The invalid field is marked as Null
  * @param lowestCelsius     the lowest value of temperature in Celsius that the balloon can record.
  * @param highestCelsius    the highest value of temperature in Celsius that the balloon can recorded.
  * @param areaBound         the max distance in both co-ordinate x and y can travel in one record
  *The maximal number of date that balloon can stay in the sky is assumed at 7 days, which is defined by maxLandingPeriod
  */
class DataGen(invalidRecordRate: Float, lowestCelsius: Float, highestCelsius: Float, areaBound: Int) {
  val delimiter = "|"
  val tempatureRange = (highestCelsius - lowestCelsius).toInt

  val randSeed = Random

  val maxLandingPeriod: Int = 7 * 24 * 60 * 60 * 1000
  val observatoryList: Array[String] = Array("AU", "US", "FR", "All Others")

  var initialTime: Long = randSeed.nextLong() % (new Date().getTime)


/*this method randomly picks up invalid records based on the invalid record rate*/
  def setInvalidRecord: Boolean = {
    if (randSeed.nextFloat() < invalidRecordRate)
      true
    else
      false
  }

  /*this method randonmly generate temperatur value*/
  def celsiusRand: Float = {
    randSeed.nextInt(tempatureRange) + lowestCelsius
  }

  /*convert the temperature into coorect unit*/
  def tempGen(observation: String): String = {
    if (setInvalidRecord)
      return "Null"
    val temp = celsiusRand

    convertTemperature(0, obs2TempType(observation), temp).toString
  }

  /*Generate time in UTC formate*, the time is randomly computed but record in sorted sequence*/
  def timeGen: String = {
    if (setInvalidRecord)
      return "Null"
    val time = new java.text.SimpleDateFormat("yyyy-MM-dd'T'hh:mm").format(new Date(initialTime))
    initialTime = initialTime + randSeed.nextInt(maxLandingPeriod)
    time
  }

  /* generate the random location string in formate "x,y"*/
  def locationGen(observation: String): String = {
    if (setInvalidRecord)
      return "Null"
    val xInKm = randSeed.nextInt(areaBound)
    val yInKm = randSeed.nextInt(areaBound)
    val x = convertDistance(0, obs2DisType(observation), xInKm)
    val y = convertDistance(0, obs2DisType(observation), yInKm)
    x + "," + y
  }

  /*generate random obervation location*/
  def observatoryGen: String = {
    if (setInvalidRecord)
      return "Null"
    val index = randSeed.nextInt(observatoryList.length)
    observatoryList(index)
  }

  /* parse patterns into a complete record*/
  def toRecordString: String = {
    val stringBuf = new StringBuffer()
    val time = timeGen
    val observatory = observatoryGen
    val temperature = tempGen(observatory)
    val location = locationGen(observatory)
    stringBuf.append(time)
    stringBuf.append(delimiter)
    stringBuf.append(location)
    stringBuf.append(delimiter)
    stringBuf.append(temperature)
    stringBuf.append(delimiter)
    stringBuf.append(observatory)

    stringBuf.toString
  }
}