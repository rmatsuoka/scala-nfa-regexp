package com.example.regexp

abstract class State
class Character(val accept: Char => Boolean, _out: => State) extends State {
  lazy val out = _out
}
class Split(_out1: => State, _out2: => State) extends State {
  lazy val out1 = _out1
  lazy val out2 = _out2
}
class Match extends State

object State {
  val `match` = new Match()
  def ASTtoNFA(e: AST): State = {
    generate(e, `match`)
  }

  private def generate(e: AST, out: State): State = {
    e match {
      case Alternation(e1, e2) =>
        new Split(generate(e1, out), generate(e2, out))
      case Catenation(e1, e2) => generate(e1, generate(e2, out))
      case Question(e)        => new Split(generate(e, out), out)
      case Star(e) => {
        /* Split(out1, out2)の引数は名前渡しであり、かつ遅延評価である。したがってgenerate(e,s)はs.out1が初めて呼び出されたときの一回しか評価されない。
         * つまりgenerate(e,s)はsがコンストラクトされた後に（つまりsが存在してから）評価される。従ってsは問題なく評価できる。
         * もし、Splitが名前渡しではなかったら、generate(e,s)はただちに評価されることになる。
         * するとs、すなわちnew Split(generate(e,s), out)はただちに評価される。これはgenerate(e,s)の評価する無限ループに陥る。
         * 要するに先にsを作って、その後にgenerate(e,s)を評価すれば良いということである。
         * ちなみにgenerate(e,s)の引数sを名前渡しにする代わりにSplitの引数を値渡しにするのは、sがいつ評価されるのかわからなくなるので悪い方法だ。
         */
        lazy val s: State = new Split(generate(e, s), out)
        return s
      }
      case Plus(e) => {
        lazy val s: State = generate(e, new Split(s, out))
        return s
      }
      case Literal(c) => new Character((d: Char) => c == d, out)
    }
  }
}

class Regexp(state: State) {
  def matchString(str: String): Boolean = {
    val finalStates = str.toList.foldLeft(followUnlabeledArrows(state)) {
      (set, c) => step(set, c)
    }

    finalStates contains State.`match`
  }

  /** sから始まり、Splitの空動作を遷移し、到達できるすべてのStateの集合を返す。sがCharacterもしくはMatchならsのみが属する集合を返す。
    * @param s
    * @return
    *   sから空動作によって到達できるすべてのStateの集合
    */
  private def followUnlabeledArrows(s: State): Set[State] = {
    s match {
      case s: Character => Set[State](s)
      case s: Match     => Set[State](s)
      case s: Split =>
        followUnlabeledArrows(s.out1) ++ followUnlabeledArrows(s.out2)
    }
  }

  /** setのそれぞれの要素について、cが入力されたときの次の状態を求める。求めたの状態の集合を返す。
    * @param set
    *   現在の状態の集合（Splitは含まれてはいけない）
    * @param c
    *   入力する文字
    * @return
    *   cが入力されたときの次の状態の集合 (Splitは含まれない)
    */
  private def step(set: Set[State], c: Char): Set[State] = {
    set.foldLeft(Set[State]()) { (set, s) =>
      s match {
        case s: Character =>
          if (s.accept(c)) set ++ followUnlabeledArrows(s.out) else set
        case _: Match => set
        case _: Split => throw new Exception("must not reach here")
      }
    }
  }
}

object Regexp {
  def compilePostfix(pattern: String): Regexp = {
    new Regexp(State.ASTtoNFA(AST.parsePostfix(pattern)))
  }

  /** 正規表現のルール
    *
    * | e+  | eを1回以上  |
    * |:----|:--------|
    * | e*  | eを0回以上  |
    * | e?  | eが0回か1回 |
    * | (e) | eをグループ化 |
    *
    * 文字セット(.や[charset]、\d)などはない。 ^(先頭マッチ), $(末尾マッチ)などもない。
    * @param pattern
    * @return
    */
  def compile(pattern: String): Regexp = {
    new Regexp(State.ASTtoNFA(AST.parse(pattern)))
  }
}
