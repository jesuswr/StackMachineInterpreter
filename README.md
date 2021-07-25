
# StackMachineInterpreter

Simple stack machine interpreter in haskell

Example:
For the following code:
```
s,i = 0,0
while (i < 10) do
	s,i = s + (i * i) / 2, i + 1
```
The stack machine code is in ``txt.txt``and when you do ``runhaskell Main.hs txt.txt`` you get:
```
Print operation: s = Num 140
Print operation: i = Num 10
```
