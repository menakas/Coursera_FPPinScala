package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if c == 0 || c == r then
      1
    else
      pascal(c-1,r-1) +  pascal(c,r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    def balanceCheck(chars: List[Char], stack: List[Char]): Boolean =
      if chars.isEmpty && stack.isEmpty then
          true
      else if chars.isEmpty && !stack.isEmpty then
          false
      else if chars.head == '(' then
          balanceCheck(chars.tail, List(chars.head) ::: stack)
      else if chars.head == ')' && !stack.isEmpty && stack.head == '(' then
          balanceCheck(chars.tail, stack.tail)
      else if chars.head == ')' then
          false
      else
          balanceCheck(chars.tail,stack)

    balanceCheck(chars, "".toList)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if coins.isEmpty || money < 0 then
      0
    else if 0 == money then
      1
    else
      countChange(money - coins.head, coins) +
      countChange(money, coins.tail)


def f(a: String)(b: Int)(c: Boolean): String =
  "(" + a + ", " + b + ", " + c + ")"

val partialApplication1 = f("Scala")

val partialApplication2 = partialApplication1(42)
