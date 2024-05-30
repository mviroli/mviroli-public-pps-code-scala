package u13.selftypes

object ModelModule:
  trait Model:
    // here comes all your model methods
    def m(): Int
  trait Provider:
    val model: Model
  trait Component:
    class ModelImpl extends Model:
      // here come all your model methods implementation
      def m() = 1
  trait Interface extends Provider with Component

object ViewModule:
  trait View:
    // here come all your view methods
    def show(i: Int): Unit
  trait Provider:
    val view: View
  type Requirements = ControllerModule.Provider
  trait Component:
    context: Requirements =>
    class ViewImpl extends View:
      // here come all your view methods implementation
      def show(i: Int): Unit = println(i)
      def update(): Unit = context.controller.notifyChange("changhed")
  trait Interface extends Provider with Component:
    self: Requirements =>

object ControllerModule:
  trait Controller:
    // here come all your view methods
    def notifyChange(s: String): Unit
  trait Provider:
    val controller: Controller
  type Requirements = ViewModule.Provider with ModelModule.Provider
  trait Component:
    context: Requirements =>
    class ControllerImpl extends Controller:
      // here come all your view methods implementation
      def notifyChange(s: String): Unit =
        context.view.show(context.model.m())
  trait Interface extends Provider with Component:
    self: Requirements =>

object MVC
  extends ModelModule.Interface
    with ViewModule.Interface
    with ControllerModule.Interface:

  // Instantiation of components, dependencies are implicit
  override val model = new ModelImpl()
  override val view = new ViewImpl()
  override val controller = new ControllerImpl()

  @main def main(): Unit =
    view.show(1)