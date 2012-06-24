package net.dkgi.sudoku

import swing._
import swing.event._
import java.awt.Dimension
import java.awt.Color
import java.awt.Font

import Swing.pair2Dimension
import collection.JavaConversions.enumerationAsScalaIterator

/** 
	* The Interface defines the user interface presenting a table for
	* entering the puzzle and a button to solve it.
	*/
object Interface {

	/**
		* The basic user interface consisting of a table, a status label and a
		* solve button.
		*/
	lazy val ui = new BorderPanel {
		border = Swing.EmptyBorder(5, 5, 5, 5)

		val sudokuModel = new SudokuTableModel 
		val table = new Table(9, 9) {
			border = Swing.LineBorder(Color.GRAY)
			font = new Font("SansSerif", Font.BOLD, 24)
			gridColor = Color.GRAY
			model = sudokuModel
			rowHeight = 28
			selection.elementMode = Table.ElementMode.Cell
			peer.setDefaultRenderer(java.lang.Class.forName("java.lang.Object"), new SudokuTableCellRenderer)
			peer.setTableHeader(null)
			peer.getColumnModel.getColumns.foreach(_.setMaxWidth(30))
		}
		add(table, BorderPanel.Position.Center)

		val bottomPanel = new BorderPanel {
			val status = new Label("")
			add(status, BorderPanel.Position.West)

			val solve: Button = new Button(Action("Solve") {
				(new SudokuWorker(sudokuModel, status, solve)).start
			})
			add(solve, BorderPanel.Position.East)
		}
		add(bottomPanel, BorderPanel.Position.South)
	}

	/** The main method sets the look and feel to native and shows the main frame.
		*
		* @param args	Command line input is ignored
		*/
	def main(args: Array[String]) {
		try {
			import javax.swing.UIManager
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
		} catch {
			case e => println(e.getMessage)
		}

		val mainFrame = new MainFrame {
			title = "Sudoku Solver"
			contents = ui
		}
		mainFrame.visible = true
	}
	
}

/**
	* A SwingWorker that solves the Sudoku providing feedback in the status label.
	*
	* @param model	The table model cotaining the puzzle
	* @param status	The label for the feedback
	* @param solve	The button to disable while solving
	*/
class SudokuWorker(
		val model: SudokuTableModel,
		val status: Label,
		val solve: Button
	) extends SwingWorker {

	override def act() {
		Swing.onEDT({
			solve.enabled = false
			status.text = "Solving..."
		})

		val field = for {i <- 0 to 8}
			yield for {j <- 0 to 8}
				yield model.rowData(i)(j) match {
					case 0 => (1 to 9).toSet
					case i => Set(i)
				}

		val solution = (new Sudoku(field)).solved

		Swing.onEDT({
			solution match {
				case Some(s) =>
					for {i <- 0 to 8; j <- 0 to 8} 
						model.setValueAt(if (s.field(i)(j).size == 1) {
							s.field(i)(j).head
						} else 0, i , j) 
					status.text = "Solved"
				case None =>
					status.text = "No solution found"
			}
			solve.enabled = true
		})
	}

}

/**
	* The table model containing a Sudoku puzzle. 1 to 9 represent the puzzle
	* entries while a 0 represents an unknown value.
	*/
class SudokuTableModel() extends javax.swing.table.AbstractTableModel { 

	val rowData = Array.ofDim[Int](9, 9)

	def getRowCount() = 9
	def getColumnCount() = 9
	def getValueAt(row: Int, col: Int): AnyRef = rowData(row)(col).asInstanceOf[AnyRef] 
	override def isCellEditable(row: Int, column: Int) = true 
	override def setValueAt(value: Any, row: Int, col: Int) { 
		rowData(row)(col) = value match {
			case i: Int => if (1.to(9).contains(i)) i else 0
			case s: String => try {
					val i = s.toInt
					if (1.to(9).contains(i)) i else 0
				} catch {
					case _ => 0
				}
			case _ => 0
		}
		fireTableCellUpdated(row, col) 
	} 

}


/**
	* The custom cell renderer for the Sudoku table.
	*/
class SudokuTableCellRenderer() extends javax.swing.table.DefaultTableCellRenderer {

	setForeground(Color.BLACK)
	setHorizontalTextPosition(javax.swing.SwingConstants.TRAILING)
	
	override def getTableCellRendererComponent(table: javax.swing.JTable, value: Object, isSelected: Boolean, hasFocus: Boolean, row: Int, col: Int): java.awt.Component = {
		val cell = value.asInstanceOf[Int] match {
			case 0 => super.getTableCellRendererComponent(table, "", isSelected, hasFocus, row, col)
			case _ => super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, col)
		}

		val rd = (row / 3) % 2
		val cd = (col / 3) % 2
		val color = if (hasFocus) {
			Color.RED
		} else if ((rd == 1) ^ (cd == 1)) {
			Color.LIGHT_GRAY 
		} else { 
			Color.WHITE
		}

		cell.setBackground(color)
		cell.asInstanceOf[javax.swing.JLabel].setHorizontalAlignment(javax.swing.SwingConstants.CENTER)

		return cell
	}

}
