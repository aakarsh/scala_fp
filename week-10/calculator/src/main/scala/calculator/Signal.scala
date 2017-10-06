package calculator

import scala.util.DynamicVariable

/**
 * Something here.
 */
class Signal[T](expr: => T) {

  import Signal._

  /**
   * Expression which when evaluated will yeild a new value
   * for this signal.
   */
  private var myExpr: () => T = _

  /**
   * Current value of the signal.
   */
  private var myValue: T = _
  
  /**
   * Obersvers are the set of signals which
   * depend on present signal value
   */
  private var observers: Set[Signal[_]] = Set()

  /**
   * Observed are the set of signals on which
   * the preset signal depends.
   */
  private var observed: List[Signal[_]] = Nil

  /**
   * Change the expression which updates the value of
   * of the current signal. expr is call by name.
   * Stored in myExpr.
   */
  update(expr)

  protected def computeValue(): Unit = {

    /**
     * For ever signal that is observed. I.E Which we depend on
     * remove ourselves from it. Disconnecting ourselves.
     */
    for (sig <- observed)
      sig.observers -= this

    /**
     * Empty the observed set.
     */
    observed = Nil

    /**
     * Execute myExpr with caller temporarily bound
     * to curent signal. The value is accessible by myExpr
     * using underlying value attibute. Execute the expression
     * Retreive next state returned from expression.
     */
    val newValue = caller.withValue(this)(myExpr())

    /**
     * Disable the following "optimization" for the assignment, because we
     * want to be able to track the actual dependency graph in the tests.
     */

    //if (myValue != newValue) { No change in value will not lead to state update.
      myValue = newValue
    
    /**
     * 
     */
      val obs = observers
      observers = Set()

    /**
     * Now that we have computed our new value let all our observers.
     * update themselves.
     */
      obs.foreach( _.computeValue())
    //}
  }

  /**
   * Change the call by value expression but don't
   * run it.
   */
  protected def update(expr: => T): Unit = {
    myExpr = () => expr
    computeValue()
  }

  /**
   *  
   */
  def apply() = {
    observers += caller.value
    assert(!caller.value.observers.contains(this), "cyclic signal definition")
    caller.value.observed ::= this
    myValue
  }
}

class Var[T](expr: => T) extends Signal[T](expr) {

  /**
   * Varaible whose underlying expression can be updated.
   */
  override def update(expr: => T): Unit = 
    super.update(expr)
}

object Var {

  /**
   * 
   */
  def apply[T](expr: => T) = 
    new Var(expr)
}


object NoSignal extends Signal[Nothing](???) {

  /**
   * Don't do anything..
   */
  override def computeValue() = ()
}

object Signal {

  /**
   * Caller is a DynamicVariable. which can be used to provide a temporarily
   * masked execution context. 
   */
  val caller = new DynamicVariable[Signal[_]](NoSignal)

  /**
   * Return Signal
   */
  def apply[T](expr: => T) = new Signal(expr)


}
