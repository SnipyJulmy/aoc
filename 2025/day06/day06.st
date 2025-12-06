"Advent of Code 2025 : Day 06"
"Command for running : pharo Pharo.image st day06.st --args ../inputs/day06-example.txt"

| args inputFile contents lines ops lengths numbers cephalopodNumbers streams score1 score2 toCephalopodNumbers transpose computeScoring |

"transpose an array of array"
transpose := [ :matrix |
  (1 to: (matrix first size)) collect: [ :col |
    matrix collect: [ :row | row at: col ]
  ] as: Array
].

" convert a group of human number to a group cephalopod number"
toCephalopodNumbers := [ :group |
  | matrix |
  matrix := group collect: [ :each | each asArray ].
  matrix := transpose value: matrix.
  matrix collect: [ :row | row joinUsing: '' ]
].

computeScoring := [ :numbers :ops |
  (1 to: (numbers size)) inject: 0 into: [ :acc :idx |
    | ns op |
    ns := numbers at: idx.
    op := ops at: idx.
    (op = '*')
      ifTrue: [
        acc + (ns inject: 1 into: [ :acc :n | acc * n]) 
      ]
      ifFalse: [
        acc + (ns inject: 0 into: [ :acc :n | acc + n]) 
      ].
  ]
].

args := Smalltalk arguments.
inputFile := args at: 3.
contents := inputFile asFileReference contents.
lines := (inputFile asFileReference contents lines) asArray.

ops := ((lines last) 
  copyWithRegex: '\s([*+])'
  matchesTranslatedUsing: [ :match | '|', match last asString])
  splitOn: '|'.

" determine the length and op of each group"
lengths := ops collect: [ :each | each size ].
ops := ops collect: [ :op | op trim ].

streams := (1 to: ((lines size) -1)) collect: [ :i |
  (lines at: i) readStream
].

numbers := lengths collect: [ :len |
  (1 to: (streams size)) collect: [ :idx |
    | token stream |
    stream := streams at: idx.
    token := stream next: len.
    stream atEnd ifFalse: [ stream next ].
    token
  ]
].

cephalopodNumbers := numbers collect: [ :each | toCephalopodNumbers value: each ].

numbers := numbers collect: [ :each | 
  each collect: [ :n | n asInteger]
].

cephalopodNumbers := cephalopodNumbers collect: [ :each | 
  each collect: [ :n | n asInteger]
].

score1 := computeScoring value: numbers value: ops.
score2 := computeScoring value: cephalopodNumbers value: ops.

Stdio stdout
  nextPutAll: 'Part 1: '; print: score1; lf;
  nextPutAll: 'Part 2: '; print: score2.

Smalltalk snapshot: false andQuit: true.
