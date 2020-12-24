hello() =
  print("Hello, World")


greet() = do {
  print("Your name?")
  name = read("> ")
  print("Hi! {name}")
}


fizzbuzz() =
  range(0, 100)
  |> map(
    fn num ->
      if zero(num % 15)
        then "FizzBuzz"
      else if zero(num % 3)
        then "Fizz"
      else if zero(num % 5)
        then "Buzz"
      else
        string(num)
  )
  |> each(print)
