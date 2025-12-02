import random

languages = [
    "Python", "JavaScript", "TypeScript", "Rust", "Go",
    "Java", "C#", "C++", "Swift", "Kotlin",
    "Ruby", "PHP", "Scala", "Haskell", "Elixir",
    "Erlang", "Clojure", "F#", "OCaml", "Julia",
    "R", "Lua", "Dart", "Zig", "Nim",
    "Crystal", "D", "V", "Odin", "Gleam",
    "Racket", "Scheme", "Common Lisp", "Prolog", "Mercury",
    "Picat", "Idris", "Agda", "PureScript", "Elm",
    "ReasonML", "Haxe", "Groovy", "Raku", "Bash",
    "PowerShell", "Ada", "Factor", "Awk", "Perl"
]

# Select a random language
selected = random.choice(languages)

print(f"Your Advent of Code language is: {selected}")
