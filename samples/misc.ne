hello() =
  print("Hello, World")


greet() = do {
  print("What is your name?")
  name = read("> ")
  print(format("Hi! {}", name))
}


fizzbuzz(num) =
  if zero(num % 15)
    then "FizzBuzz"
  else if zero(num % 3)
    then "Fizz"
  else if zero(num % 5)
    then "Buzz"
  else
    string(num)
