import scala.reflect.runtime.universe._
import org.apache.log4j.Logger

class CommissionCalculator {
  sealed trait CommissionDisplay {
    def totalDisplayCommission: String
  }

  abstract class Commission

  case class ClientSideCommission(value: Int) extends Commission

  case class StreetSideCommission(value: Int) extends Commission

  class TotalCommission[T <: Commission : TypeTag] {
    def getTotalCommission(commissionList: List[T]): String = {
      commissionList.totalDisplayCommission
    }
  }

  private implicit class GenericExtension[T <: Commission : TypeTag](commissionList: List[T]) extends CommissionDisplay {

    override def totalDisplayCommission: String = {
      val result: (String, Int) = typeOf[T] match {
        case clientSide if clientSide =:= typeOf[ClientSideCommission] => ("Client Side Commission", commissionList.foldLeft(0) { (sum, clientCost) => sum + clientCost.value})
        case streetSide if streetSide =:= typeOf[StreetSideCommission] => ("Street Side Commission", commissionList.foldLeft(0) { (sum, streetCost) => sum + streetCost.value})
        case commission if commission =:= typeOf[Commission] => ("Commission", commissionList.foldLeft(0) { (sum, commission) => sum + commission.value})
      }
      s"The ${result._1} is: ${result._2}"
    }
  }
}

object CommissionCalculator extends App {
  val log = Logger.getLogger(this.getClass)
  val obj = new CommissionCalculator
  val streetSideList: List[obj.StreetSideCommission] = List()
  val clientSideList: List[obj.ClientSideCommission] = List()
  val clientSideCalculator = new obj.TotalCommission[obj.Commission]()
  log.info("\n1. Calculating Client Side Commissions \n")
  log.info(s"${clientSideCalculator.getTotalCommission(clientSideList)}")
  log.info("\n")
  val streetSideCalculator: obj.TotalCommission[obj.StreetSideCommission] = new obj.TotalCommission
  log.info("\n2. Calculating Street Side Commissions \n")
  log.info(s"${streetSideCalculator.getTotalCommission(streetSideList)}")
}