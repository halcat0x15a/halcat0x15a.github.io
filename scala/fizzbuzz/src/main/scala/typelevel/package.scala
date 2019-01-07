package object typelevel {
  type _0 = Zero
  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]
  type _6 = Succ[_5]
  type _7 = Succ[_6]
  type _8 = Succ[_7]
  type _9 = Succ[_8]
  type _10 = Succ[_9]
  type _11 = Succ[_10]
  type _12 = Succ[_11]
  type _13 = Succ[_12]
  type _14 = Succ[_13]
  type _15 = Succ[_14]

  type +[N <: Nat, M <: Nat] = Plus[N, M]
  type -[N <: Nat, M <: Nat] = Minus[N, M]
  type *[N <: Nat, M <: Nat] = Mult[N, M]
  type /[N <: Nat, M <: Nat] = Div[N, M]
  type %[N <: Nat, M <: Nat] = Mod[N, M]
  type <[N <: Nat, M <: Nat] = LT[N, M]

  type ==[Exp, R] = Exp { type Result = R }

  implicitly[_2 < _3]
  implicitly[_3 < _6]

  implicitly[_2 + _3 == _5]
  implicitly[_6 + _9 == _15]

  implicitly[_6 - _4 == _2]
  implicitly[_9 - _6 == _3]

  implicitly[_2 * _0 == _0]
  implicitly[_3 * _2 == _6]

  implicitly[_6 / _4 == _1]
  implicitly[_9 / _3 == _3]

  implicitly[_3 % _2 == _1]
  implicitly[_8 % _3 == _2]
  implicitly[_1 % _3 == _1]
}
