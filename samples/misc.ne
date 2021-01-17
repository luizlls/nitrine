hello() =
  print "Hello, World"


greet() =
  print "Your name?";
  name = read "> ";
  print "Hi! {name}"


fizzbuzz() =
  (range 0 100)
  |> map (
    fn num ->
      if zero? (num % 15)
        then "FizzBuzz"
      else if zero? (num % 3)
        then "Fizz"
      else if zero? (num % 5)
        then "Buzz"
      else
        string num
    )
  |> each print


factorial(n) =
  if n <= 1 then 1 else n * factorial (n - 1)


counter(action) =
  count = 0;
  div [
    button "click me!" {
      click() =
        count = count + 1; action(),
    },
    "count: {count}"
  ]