package com.pdehaan.scaladice.dnd5e.examples

import com.pdehaan.scaladice.Distribution._

object RollAttributes
{
    def main(args: Array[String]) =
    {
        println("=== Rolled Attribute Generation ===")
        println()

        // distribution for 4d6 keep 3
        val attr = d(6).repeat(4).keep(3)

        // combinations of 6 attrs, ignoring order (the sequences represented are
        // actually sorted, so that sequences with the same values are collapsed)
        val attrs = attr.repeatUnordered(6)

        val expectedAttrs = for (n <- 0 to 5)
        // take the Nth element of each sequence
            yield attrs.nth(n)

        print("4d6k3 expected results = ")
        println(expectedAttrs.map(_.ev.round).mkString(", "))
        println()

        // Compute the point buy cost of a particular attribute value. This is
        // an approximation, as D&D 5e only allows point buy scores in the
        // interval [8, 15]
        def cost(score: Int) =
            if (score > 14) 6 + 2 * (score - 14)
            else score - 8

        // sum the costs for various attribute roll results
        val costs = attrs.map(_.map(cost).sum)

        println(f"--- 'at least' point buy value (AVG: ${costs.ev}%.1f) ---")
        println(costs.inverseCumulativeHist)
        println()


        for ((d, n) <- expectedAttrs.zipWithIndex)
        {
            println(s"--- attibute $n ---")
            println(d.hist)
            println()
        }
    }
}
