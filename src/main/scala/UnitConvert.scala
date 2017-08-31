/** This object provide fundamental convertion between units
  * @author Xiaodong Meng
  */


object UnitConvert {

  private def celsius2Fahrenheit(celsius: Float): Float = {
    celsius * 1.8F + 32
  }

  private def celsius2Kelvin(celsius: Float): Float = {
    celsius + 273.15F
  }

  private def fahrenheit2celsius(fahrenheit: Float): Float = {
    (fahrenheit - 32) / 1.8F
  }

  private def fahrenheit2Kelvin(fahrenheit: Float): Float = {
    celsius2Kelvin(fahrenheit2celsius(fahrenheit))
  }

  private def kelvin2Celsius(kelvin: Float): Float = {
    kelvin - 273.15F
  }

  private def kelvin2Fahrenheit(kelvin: Float): Float = {
    celsius2Fahrenheit(kelvin2Celsius(kelvin))
  }

  private def km2Meter(m: Float): Float = {
    m * 1000
  }

  private def km2Mile(m: Float): Float = {
    m / 1.609344F
  }

  private def meter2Km(m: Float): Float = {
    m / 1000
  }

  private def meter2Mile(m: Float): Float = {
    km2Mile(meter2Km(m))
  }

  private def mile2Km(m: Float): Float = {
    m * 1.609344F
  }

  private def mile2Miler(m: Float): Float = {
    km2Meter(mile2Km(m))
  }

  /*
* outputType 0 stand for celsius; 1 stand for fahrenheit and 2 stand for kelvin
*
 */
  def convertTemperature(inputType: Int, outputType: Int, temperature: Float): Float = {
    inputType match {
      case 0 => {
        outputType match {
          case 0 => temperature
          case 1 => celsius2Fahrenheit(temperature)
          case 2 => celsius2Kelvin(temperature)
        }
      }
      case 1 => {
        outputType match {
          case 0 => fahrenheit2celsius(temperature)
          case 1 => temperature
          case 2 => fahrenheit2Kelvin(temperature)
        }
      }
      case 2 =>
        outputType match {
          case 0 => kelvin2Celsius(temperature)
          case 1 => kelvin2Fahrenheit(temperature)
          case 2 => temperature
        }
    }
  }

  /*
  * outputType 0 stand for km; 1 stand for meter and 2 stand for Mile
  *
   */
  def convertDistance(inputType: Int, outputType: Int, distance: Float): Float = {
    val result = inputType match {
      case 0 => {
        outputType match {
          case 0 => distance
          case 1 => km2Meter(distance)
          case 2 => km2Mile(distance)
        }
      }
      case 1 => {
        outputType match {
          case 0 => meter2Km(distance)
          case 1 => distance
          case 2 => meter2Mile(distance)
        }
      }
      case 2 =>
        outputType match {
          case 0 => mile2Km(distance)
          case 1 => mile2Miler(distance)
          case 2 => distance
        }
    }

    if (result == Float.NegativeInfinity || result == Float.PositiveInfinity) {
      println("inputType: " + inputType)
      println("outputType: " + outputType)
      println("Distance: " + distance)
    }
    result
  }

  def obs2TempType(observation: String): Int = {
    observation match {
      case "AU" => 0
      case "US" => 1
      case "FR" | "All Others" => 2
      case _ => 0
    }
  }

  def obs2DisType(observation: String): Int = {
    observation match {
      case "AU" | "All Others" => 0
      case "FR" => 1
      case "US" => 2
      case _ => 0
    }
  }

  def temp2TempType(temp: String): Int = {
    temp match {
      case c => 0
      case f => 1
      case k => 2
      case _ => 0
    }
  }

  def dist2DisType(dist: String): Int = {
    dist match {
      case "km" => 0
      case "m" => 1
      case "mile" => 2
      case _ => 0
    }
  }
}
