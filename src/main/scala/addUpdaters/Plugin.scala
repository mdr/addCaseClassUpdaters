package addUpdaters

import scala.tools.nsc
import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.{ Transform, TypingTransformers }
import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.plugins.Plugin

class AddUpdatersPlugin(val global: Global) extends Plugin {
 
  val name = "addUpdaters"
  val description = "Add update methods to case classes"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent with Transform with TreeDSL {

    val global = AddUpdatersPlugin.this.global
    import global._
    import global.definitions._

    override val runsAfter = List("parser")

    val phaseName = "addUpdaters"

    def newTransformer(unit: CompilationUnit) = new AddUpdatesTransformer(unit)

    class AddUpdatesTransformer(unit: CompilationUnit) extends Transformer {

      override def transform(tree: Tree): Tree = tree match {
        case classDef: ClassDef if classDef.mods hasFlag Flags.CASE =>
          // TODO: call super.transform(cd) first to handle nested case classes
          addUpdaters(classDef)
        case _ =>
          super.transform(tree)
      }

      private def addUpdaters(classDef: ClassDef): ClassDef = {
        val extraMethods: List[Tree] = getCaseFields(classDef) map (makeUpdateMethod _ tupled)
        val template = classDef.impl
        val newTemplate = atPos(template.pos)(template.copy(body = template.body ++ extraMethods))
        atPos(classDef.pos)(classDef.copy(impl = newTemplate))
      }

      private def getCaseFields(classDef: ClassDef): List[(String, Tree)] = {
        var nameAndTypeTree: List[(String, Tree)] = Nil
        val traverser = new Traverser { // TODO: just get immediate members
          override def traverse(t: Tree) = t match {
            case vd @ ValDef(mods, name, type_, _) if mods hasFlag Flags.CASEACCESSOR =>
              nameAndTypeTree = (name.toString, type_) :: nameAndTypeTree
            case t =>
              super.traverse(t)
          }
        }
        traverser.traverse(classDef.impl)
        nameAndTypeTree
      }

      private def makeUpdateMethod(fieldName: String, type_ : Tree): DefDef = {
        val parameterName = "new" + fieldName.capitalize
        val updateMethodName = "with" + fieldName.capitalize
        val valDef = ValDef(NoMods, parameterName, type_, EmptyTree)
        val updateMethodBody = Apply(
          Ident("copy"),
          List(
            AssignOrNamedArg(
              Ident(fieldName),
              Ident(parameterName))))
        DefDef(NoMods, updateMethodName, List(), List(List(valDef)), TypeTree(), updateMethodBody)
      }

    }

  }
}