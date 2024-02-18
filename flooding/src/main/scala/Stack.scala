/** List based implementation of a Stack of generic types with null safety included
  * @param args the elements that are in the stack to begin with
  * @tparam A the type of elements in the stack
  */
class Stack[A](args: A*) extends Iterable[A] {
  private var values: List[A] = args.toList

  def push(elem: A): Unit = values = elem :: values

  def peek: Option[A] = if size > 0 then Some(values.head) else None

  def pop: Option[A] = if size > 0 then {
    val ret = values.head
    values = values.tail
    Some(ret)
  } else None

  override def iterator: Iterator[A] = values.iterator
}
