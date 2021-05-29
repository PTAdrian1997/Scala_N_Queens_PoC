package constraints

trait Constraint[C <: ConstraintArgument] {
  def checkConstraint: Boolean
}

object Constraint {

   implicit class ConflictAvoidingConstraint(argument: ConflictAvoidingArgument)
     extends Constraint[ConflictAvoidingArgument] {
     override def checkConstraint: Boolean =
       Math.abs(argument.rowId1 - argument.rowId2) != Math.abs(argument.colId1 - argument.colId2) &&
         argument.rowId1 != argument.rowId2 &&
         argument.colId1 != argument.colId2
   }

}