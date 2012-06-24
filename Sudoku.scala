package net.dkgi.sudoku

/**
	* Represents a Sudoku field.
	*/
class Sudoku(val field: IndexedSeq[IndexedSeq[Set[Int]]]) {

	/**
		* Solves the current Sudoku puzzle.
		* @return The Solution to the puzzle or None if no solution exists
		*/
	 def solved: Option[Sudoku] = {
		if (!reduced.isSolved) {
			if (reduced.isSolvable) {
				(None.asInstanceOf[Option[Sudoku]] /: reduced.guessed)((o, s) => o match {
					case solution @ Some(_) => solution
					case None => s.solved
				})
			} else {
				None
			}
		} else {
			Some(reduced)
		}
	}

	/**
		* Deterministically reduces the puzzle until the puzzle is solved or further
		* choices require guessing.
		* @return The reduced puzzle
		*/
	lazy val reduced: Sudoku = {
		val r = reducedOnce
		if (this == r) this else r.reduced
	}

	/**
		* Performs a single reduction step.
		* @return The reduced puzzle
		* @see #reduced
		*/
	private def reducedOnce: Sudoku = {
		(this /: setValues)((s, p) => {
			val (i, j, v) = p
			val f = (s: Set[Int]) => s - v
			s.mapRow(i, j)(f).mapCol(i, j)(f).mapBlock(i, j)(f)
		})
	}

	/**
		* Generates a sequence of Sudoku puzzles where each represents a mutually
		* exclusive guessing decision. Either one of these puzzles has a solution or
		* the overall puzzle is not solvable.
		* @return The list of new puzzles
		*/
	private lazy val guessed: Seq[Sudoku] = {
		var (i, j) = (-1, -1)
		var min = 10
		for {k <- 0 to 8; l <- 0 to 8}
			if (field(k)(l).size != 1 && field(k)(l).size < min) {
				i = k; j = l; min = field(i)(j).size
			}

		if ((i, j) == (-1, -1)) Seq.empty else {
			for {p <- field(i)(j).toSeq}
				yield new Sudoku(field.updated(i, field(i).updated(j, Set(p))))
		}
	}

	/**
		* Indicates whether the puzzle is solved.
		* @return True if the puzzle is solved
		*/
	def isSolved: Boolean = 
		field.flatten.forall((s: Set[Int]) => s.size == 1)

	/**
		* Indicates whether the puzzle is consistent with the rules of Sudoku.
		* @return True if it is consistent, false otherwise
		*/
	def isConsistent: Boolean = {
		val rowsConsistent = field.forall(row => (Set[Int]() /: row)(_++_) == (1 to 9).toSet)
		val colsConsistent = columns.forall(col => (Set[Int]() /: col)(_++_) == (1 to 9).toSet)
		val blocksConsistent = blocks.forall(block => (Set[Int]() /: block)(_++_) == (1 to 9).toSet)
		rowsConsistent && colsConsistent && blocksConsistent
	}

	/**
		* Indicates whether there is still hope that a solution to the Sudoku can be
		* found.
		* @return True if a solution is possible
		*/
	private def isSolvable: Boolean = 
		!field.flatten.exists((s: Set[Int]) => s.isEmpty)

	/**
		* Generates triples of coordinates in the field and the corresponding value
		* that is fixed.
		* @return The sequence of triples
		*/
	private[this] def setValues: IndexedSeq[(Int, Int, Int)] = {
		for {i <- 0 to 8; j <- 0 to 8 if field(i)(j).size == 1}
			yield (i, j, field(i)(j).head)
	}

	override def equals(other: Any) = other match {
		case that: Sudoku => that.canEqual(this) && field == that.field
		case _ => false
	}

	def canEqual(other: Any): Boolean = 
		other.isInstanceOf[Sudoku]

	override def toString: String = {
		field.map(_.map(s => s.size match {
			case 1 => s.head.toString
			case _ => "_"
		}).mkString(" ")).mkString("\n")
	}

	/**
		* The transposed field. That is the matrix as column major representation.
		* @return Column major representation of the puzzle
		*/
	private def columns: IndexedSeq[IndexedSeq[Set[Int]]] = 
		for {j <- 0 to 8} yield for {i <- 0 to 8} yield field(i)(j)

	/**
		* The 9 blocks of the puzzle as sequences.
		* @return A sequence of blocks
		*/
	private def blocks: IndexedSeq[IndexedSeq[Set[Int]]] = {
		for {i <- 0 to 2; j <- 0 to 2}
			yield for {k <- 3*i until 3*(i+1); l <- 3*j until 3*(j+1)} yield field(k)(l)
	}

	/**
		* Maps a function to all fields of the same row of the given coordinates.
		* The field given in the argument itself is excluded from this
		* transformation.
		* @return The transformed puzzle
		*/
	private def mapRow(row: Int, col: Int)(f: Set[Int] => Set[Int]): Sudoku = {
		val mapped = for {i <- 0 to 8} yield for {j <- 0 to 8}
			yield if (i == row && j != col) f(field(i)(j)) else field(i)(j)
		new Sudoku(mapped)
	}

	/**
		* Maps a function to all fields of the same column of the given coordinates.
		* The field given in the argument itself is excluded from this
		* transformation.
		* @return The transformed puzzle
		*/
	private def mapCol(row: Int, col: Int)(f: Set[Int] => Set[Int]): Sudoku = {
		val mapped = for {i <- 0 to 8} yield for {j <- 0 to 8}
			yield if (j == col && i != row) f(field(i)(j)) else field(i)(j)
		new Sudoku(mapped)
	}

	/**
		* Maps a function to all fields of the same block of the given coordinates.
		* The field given in the argument itself is excluded from this
		* transformation.
		* @return The transformed puzzle
		*/
	private def mapBlock(row: Int, col: Int)(f: Set[Int] => Set[Int]) = {
		val lr = (row / 3) * 3; val rr = lr until lr + 3
		val lc = (col / 3) * 3; val cr = lc until lc + 3
		val mapped = for {i <- 0 to 8} yield for {j <- 0 to 8 } 
			yield if (rr.contains(i) && cr.contains(j) && !(i == row && j == col)) f(field(i)(j)) else	field(i)(j)
		new Sudoku(mapped)
	}

}
