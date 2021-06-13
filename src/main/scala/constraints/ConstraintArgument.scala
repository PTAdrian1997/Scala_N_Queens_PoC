package constraints

trait ConstraintArgument

case class ConflictAvoidingArgument(rowId1: Int, colId1: Int, rowId2: Int, colId2: Int) extends ConstraintArgument