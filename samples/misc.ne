hello = fn name {
  print "Hello, {name}!"
}

fizzbuzz = fn {
  (range 0 100)
  |> map (
      fn num {
        if zero (num % 15)
          then "FizzBuzz"
        else if zero (num % 3)
          then "Fizz"
        else if zero (num % 5)
          then "Buzz"
        else
          string num
      }
    )
  |> each print
}


factorial = fn n {
  match n {
    1 -> 1
    n -> n * factorial (n - 1)
  }
}


counter = fn {
  counter = mut 0
  div [
    button "+" [ click: fn (counter := counter + 1) ],
    p ["total ", strong "{counter}"],
    button "-" [ click: fn (counter := counter - 1) ],
  ]
}


sum  = fold (+) 0

prod = fold (*) 1


optional = Some value