package net.dkgi.sudoku.test

import net.dkgi.sudoku.Sudoku

object SudokuTest {

	val N = 100
	val u = 100
	val r = new util.Random

	def main(args: Array[String]) {
		val statistics = for {n <- 1 to N} yield {
			val sudoku = new Sudoku(for {i <- 0 to 8} yield for {j <- 0 to 8}
				yield r.nextInt(u) match {
					case v if (1 to 9).contains(v) => Set(v)
					case _ => (1 to 9).toSet
				})

			val before = System.currentTimeMillis
			val (solved, error) = sudoku.solved match {
				case Some(sol) => (true, sol.isConsistent)
				case _ => (false, false)
			}
			val time = System.currentTimeMillis - before
			
			println(n + ": " + time + "\t" + solved + "\t" + error)
			(time, solved, error)
		}

	}

}
