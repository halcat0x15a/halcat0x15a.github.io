package fpinscala.state

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    inputs match {
      case Nil => for (machine <- State.get[Machine]) yield (machine.candies, machine.coins)
      case input :: inputs =>
        State.get[Machine].flatMap { machine =>
          if (machine.candies > 0)
            input match {
              case Coin if machine.locked =>
                State.set(Machine(false, machine.candies, machine.coins)).flatMap(_ => simulateMachine(inputs))
              case Turn if !machine.locked =>
                State.set(Machine(true, machine.candies - 1, machine.coins + 1)).flatMap(_ => simulateMachine(inputs))
              case _ =>
                simulateMachine(inputs)
            }
          else
            simulateMachine(inputs)
        }
    }

}
