package u13.selftypes

object Dependency:

  // Abstraction
  trait Logger:
    def log(s: String): Unit
  // Implementation
  class LoggerImpl extends Logger:
    def log(s: String): Unit = println(s)
  // Abstract dependency for others
  trait LoggerDependency:
    val logger: Logger

  // Abstraction
  trait User:
    def loggableOperation: Unit
  // Contextualised Implementation
  trait UserComponent:
    loggerDependency: LoggerDependency =>
    class UserImpl extends User:
      def loggableOperation: Unit = loggerDependency.logger.log("hello!")

  object Application extends LoggerDependency with UserComponent:
    // Filling the dependency
    override val logger: Logger = new LoggerImpl
    // User now knows what the logger is
    def userFactory(): User = new UserImpl
    @main def mainApp =
      val user = userFactory()
      user.loggableOperation



