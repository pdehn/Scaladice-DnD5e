package com.pdehaan.scaladice.dnd5e.examples

import com.pdehaan.scaladice.dnd5e.Dnd5e

object Maneuvers
{
    import Dnd5e._
    import com.pdehaan.scaladice.Distribution._

    def tripAttack(): Unit =
    {
        println(
            """
              |=== Trip Attack Evaluation ===
              |
              |Class: Fighter (Battlemaster) 5
              |Strength: 18 (+4)
              |Weapon: Longsword = +7 to hit, 1d8+4 slashing damage
              |
              |Attacking twice, adding 'trip attack' to the first attack,
              |possibly forcing the target prone.
              |
              |Target: Gnoll = AC 15, STR 16 (+2)
            """.stripMargin)

        val atk = dc(15).attack(
            base = d20,
            bonus = 7)

        println("--- 1st attack result ---")
        println(atk.hist)
        println()

        val dmg = atk.damage(
            weapon = d(8),
            bonus = 4 + d(8) // extra d8 damage from maneuver
        )

        println(f"--- 1st attack dmg total (${dmg.ev}%.1f) ---")
        println(dmg.hist)
        println()

        val knockedProne = atk.flatMap {
            case Crit | Hit => (d20 + 2).map(_ < 15)
            case _ => fixed(false)
        }

        println("--- target prone after 1st attack ---")
        println(knockedProne.hist)
        println()

        // second attack has advantage if target is prone
        val atk2base = knockedProne.flatMap {
            case true => advantage
            case false => d20
        }

        val atk2 = dc(15).attack(
            base = atk2base,
            bonus = 7)

        println("--- 2nd attack result ---")
        println(atk2.hist)
        println()

        val dmg2 = atk2.damage(
            weapon = d(8),
            bonus = 4)

        println(f"--- 2nd attack dmg total (${dmg2.ev}%.1f) ---")
        println(dmg2.hist)
        println()

        println(
            """
              |--- Summary ---
              |
              |If these are the only attack we're making, most of our extra
              |damage is gained just from adding 1d8 to the first attack.
              |
              |Gaining advantage on subsequenct attacks does add a noticable
              |benefit for subsequent attacks though, so it's particularly
              |if more attacks can take advantage of it (e.g. via our own action
              |surge, or subsequent melee allies' turns.)
            """.stripMargin)
    }

    def precisePowerAttack(): Unit =
    {
        println(
            """
              |=== Power Attack Evaluation w/ Precision Strike ===
              |
              |Class: Fighter (Battlemaster) 5
              |Strength: 18 (+4)
              |Weapon: Greatsword = +7 to hit, 2d6+4 slashing damage
              |
              |Evaluating a single attack, using a "power attack" (from the
              |GWM feat) to increase damage output (-5 to hit for +10 damage).
              |
              |Several variations are run - with and without precise strike,
              |and with and without power attack.
              |
              |Target: Gnoll = AC 15, STR 16 (+2)
            """.stripMargin)

        val weapon = d(6).repeat(2).sum

        val normalAtk = dc(15).attack(
            base = d20,
            bonus = 7)

        val normalDmg = normalAtk.damage(
            weapon = weapon,
            bonus = 4)

        println(f"--- Normal Attack Damage (${normalDmg.ev}%.1f) ---")
        println(normalDmg.hist)
        println()

        val powerAtk = dc(15).attack(
            base = d20,
            bonus = 7 - 5)

        val powerDmg = powerAtk.damage(
            weapon = weapon,
            bonus = 4 + 10)

        println(f"--- Power Attack Damage (${powerDmg.ev}%.1f) ---")
        println(powerDmg.hist)
        println()

        val preciseNormalAtk = dc(15).attack(
            base = d20,
            bonus = 7 + d(8))

        val preciseNormalDmg = preciseNormalAtk.damage(
            weapon = weapon,
            bonus = 4)

        println(f"--- Precise Normal Attack Damage (${preciseNormalDmg.ev}%.1f) ---")
        println(preciseNormalDmg.hist)
        println()

        val precisePowerAtk = dc(15).attack(
            base = d20,
            bonus = 7 - 5 + d(8))

        val precisePowerDmg = precisePowerAtk.damage(
            weapon = weapon,
            bonus = 4 + 10)

        println(f"--- Precise Power Attack Damage (${precisePowerDmg.ev}%.1f) ---")
        println(precisePowerDmg.hist)
        println()

        println(
            f"""
               |--- Summary ---
               |
               |Normal damage:         ${normalDmg.ev}%.1f
               |Power damage:          ${powerDmg.ev}%.1f
               |Precise Normal damage: ${preciseNormalDmg.ev}%.1f
               |Precise Power damage:  ${precisePowerDmg.ev}%.1f
               |
               |In this scenario, we gain an average +5 damage for using a
               |power attack, as long as we have a precise strike available.
               |
               |Even better, our hit-percentage is only marginally effected
               |(62.5 vs 65), so we don't suffer a major loss in consistency.
               |
               |With both an action surge and 4 maneuver dice available, our
               |character can do an average 54 damage, not including a bonus
               |attack from GWM or the likelihood that we could use maneuvers
               |for additional damage (e.g. trip attack) assuming our attack
               |hits without the precise attack bonus.
             """.stripMargin)
    }

    def main(args: Array[String]): Unit =
    {
        tripAttack()

        precisePowerAttack()
    }
}
