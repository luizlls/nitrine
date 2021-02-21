hello(name) =
  print "Hello, {name}!"


fizzbuzz() =
  (range 0 100)
  |> map (
    fn num ->
      if zero (num % 15)
        then "FizzBuzz"
      else if zero (num % 3)
        then "Fizz"
      else if zero (num % 5)
        then "Buzz"
      else
        string num
    )
  |> each print


factorial(n) =
  if n <= 1 then 1 else n * factorial (n - 1)


counter() = do
  counter = mut 0;
  div [
    button "+" { click() = counter := counter + 1 },
    p ["total ", strong "{counter}"],
    button "-" { click() = counter := counter - 1 },
  ]


sum  = fold (+) 0

prod = fold (*) 1
