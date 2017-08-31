import UnitConvert._

/** This application convert temperature and distance unit based on customer command
  * @author Xiaodong Meng
  */

object NormalUnit {
  def main(args: Array[String]): Unit = {
    println("Usage of NormalUnit:")
    println("-i       the Input file name. Default value is rawDataFromBalloon.txt")
    println("-o       the Output file Name. Default value is normalizedBallonData.txt")
    println("-d       the output distance Unit. Options are km, m and mile. Default value is km")
    println("-t       the output temperature Unit. Options are c,f and k for celsius, fahrenheit and kelvin. Default value is c")
    var numberOfRecords = 5000000L
    //var numberOfRecords = 50L
    var distUnit = "km"
    var tempUnit = "c"
    var inputFileName = "rawDataFromBalloon.txt"
    var outputFileName = "normalizedBallonData.txt"

    for (arg <- args) (
      arg match {
        case "-i" => inputFileName = args(args.indexOf("-i") + 1)
        case "-o" => outputFileName = args(args.indexOf("-o") + 1)
        case "-d" => {
          val inputUnit = args(args.indexOf("-d") + 1).toLowerCase
          if (!Array("km", "m", "mile").contains(inputUnit)) {
            println("input distance unit " + inputUnit + " cannot identified")
            System.exit(0)
          }
          distUnit = inputUnit
        }
        case "-t" => {
          val inputUnit = args(args.indexOf("-t") + 1).toLowerCase
          if (!Array("c", "f", "k").contains(inputUnit)) {
            println("input temperature unit " + inputUnit + " cannot identified")
            System.exit(0)
          }
          tempUnit = inputUnit
        }
        case _ =>
      }
      )

    val reader = new FileRead(inputFileName)
    val writer = new FileWrite(outputFileName)

    reader.lines.foreach(line => writer.write(converLine(line, distUnit, tempUnit)))
    writer.close
  }

  val delimiter = '|'

  def converLine(inputLine: String, distanceUnit: String, tempUnit: String): String = {
    val pattern: Array[String] = inputLine.split(delimiter)
    if (pattern.length != 4) {
      println("record: " + inputLine + " is broken")
      return inputLine
    }
    val inputTimeStr = pattern(0)
    val inputLocationStr = pattern(1)
    val inputTempStr = pattern(2)
    val inputObsStr = pattern(3)
    if (inputObsStr == "Null" || !Array("AU", "US", "FR", "All Others").contains(inputObsStr))
      return inputLine

    val outputTemp = try {
      convertTemperature(obs2TempType(inputObsStr), temp2TempType(tempUnit), inputTempStr.toFloat).toString
    } catch {
      case ex: Exception => {
        println("Temperature in line" + inputLine + "cannot process"); "Null"
      }
    }

    val outputDist: String = try {
      val locationArray = inputLocationStr.split(',')
      val locationTup = (locationArray(0).toFloat, locationArray(1).toFloat)
      val x = convertDistance(obs2DisType(inputObsStr), dist2DisType(distanceUnit), locationTup._1)
      val y = convertDistance(obs2DisType(inputObsStr), dist2DisType(distanceUnit), locationTup._2)
      x.toString + ',' + y.toString
    } catch {
      case ex: Exception => {
        println("Location in line" + inputLine + "cannot process"); "Null"
      }
    }

    val stringBuf = new StringBuffer()
    stringBuf.append(inputTimeStr)
    stringBuf.append(delimiter)
    stringBuf.append(outputDist)
    stringBuf.append(delimiter)
    stringBuf.append(outputTemp)
    stringBuf.append(delimiter)
    stringBuf.append(inputObsStr)

    stringBuf.toString
  }
}