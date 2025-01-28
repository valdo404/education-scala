package io.univalence.education

trait Toto {
  def faireQuelqueChose(str: String): String
}

@main def TotoMain(): Unit = {
  val toto = new TotoPédale()
  println(toto.faireQuelqueChose("Hello, "))
}

class TotoPédale extends Toto {
  override def faireQuelqueChose(str: String): String = {
    str + "je pédale"
  }
}

trait DataType

case class TableStructure(fields: List[TableField])

case class TableField(name: String, dataType: DataType)

case class TableCatalog(tables: Map[String, TableStructure])

trait ExecutionPlan

case class Project(/* Add your project fields here */) extends ExecutionPlan

trait SqlExecutionPlanOptimizer {
  def optimize(executionPlan: ExecutionPlan, tableCatalog: TableCatalog): ExecutionPlan
}

case class Catalog()

trait Rule {
  def apply(executionPlan: ExecutionPlan, catalog: Catalog): (ExecutionPlan, Catalog)
}

class ExpressionSimplificationRule extends Rule {
  override def apply(executionPlan: ExecutionPlan, catalog: Catalog): (ExecutionPlan, Catalog) = {
    executionPlan match {
      case p: Project => (simplify(p), catalog)
      case _ => (executionPlan, catalog)
    }
  }
  
  def simplify(p: Project): Project = {
    ???
  }
}
