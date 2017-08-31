import java.io.{File, FileWriter, IOException, PrintWriter}
import scala.io.Source


/** This class provides write file functions
  * @author Xiaodong Meng
  */
@throws[IOException]
class FileWrite(fileName:String){
  val outputFile =new PrintWriter(new File(fileName))
  def write(inputString:String):Unit={
    outputFile.println(inputString)
}

  def close:Unit={
    outputFile.close()
  }

}

/** This class provides read file function
  * @author Xiaodong Meng
  */
@throws[IOException]
class FileRead(fileName:String){
  val inputFile = Source.fromFile(fileName)
  def lines = inputFile.getLines()

}
