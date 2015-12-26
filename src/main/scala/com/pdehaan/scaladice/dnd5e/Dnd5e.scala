package com.pdehaan.scaladice.dnd5e
import com.pdehaan.scaladice.Distribution

object Dnd5e
{
    import Distribution._

    type Roll = Distribution[Int]

    val d4 = d(4)
    val d6 = d(6)
    val d8 = d(8)
    val d10 = d(10)
    val d20 = d(20)

    val advantage = d20.repeat(2).keep(1)
    val disadvantage = d20.repeat(2).keepLowest(1)

    sealed trait AttackRoll
    case object Crit extends AttackRoll
    case object Hit extends AttackRoll
    case object Miss extends AttackRoll
    case object Fumble extends AttackRoll

    case class DC(dc: Int)

    def dc(dc: Int) = fixed(DC(dc))

    def dc(range: Roll) = range map { DC }

    implicit def fix(n: Int): Distribution[Int] = fixed(n)

    implicit class DCOps(dc: Distribution[DC])
    {
        def attack(
            base: Roll = d20,
            bonus: Roll = fixed(0),
            threat: Int = 20): Distribution[AttackRoll] = base flatMap {
            case n if n >= threat => fixed(Crit)
            case 1 => fixed(Fumble)
            case n => bonus zip dc map {
                case (b, d) if n + b >= d.dc => Hit
                case _ => Miss
            }
        }
    }

    implicit class AttackRollOps(atk: Distribution[AttackRoll])
    {
        def damage(weapon: Roll, bonus: Roll): Roll = atk flatMap {
            case Crit => weapon.repeat(2).sum + bonus
            case Hit => weapon + bonus
            case _ => fixed(0)
        }

        def damage(weapon: Roll, bonus: Int): Roll = damage(weapon, fixed(bonus))
    }
}
