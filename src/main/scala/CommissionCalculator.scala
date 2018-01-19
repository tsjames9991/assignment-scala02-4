class CommissionCalculator {

  abstract class Commission

  case class ClientSideCommission(value: Int) extends Commission
  case class StreetSideCommission(value: Int) extends Commission

  sealed trait CommissionDisplay {
    def totalDisplayCommission: String
  }

  class TotalCommission {
    type T >: Commission
  }
}
