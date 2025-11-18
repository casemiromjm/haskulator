# haskulator
Project developed as Functional Programming Project for PFL 2025/26 course @FEUP

## How to run

```bash
git clone git@github.com:casemiromjm/haskulator.git && cd haskulator
```

Manually compile and execute code

```bash
ghc Calculator && ./Calculator
```

Use haskulator.sh file to run haskulator
```bash
./haskulator.sh
```
User clean.sh to remove compilation related files
```bash
./clean.sh
```

## Features
Supports operators: addition, subtraction, multiplication, integer division and modulus.

```python
1+1
2
3*3
9
...
```

Also support variable assignment:

```python
x=2
2
x=x*x
4
y=3
3
x+y
7
...
```

## Flaws
Currently, the code crashes when trying to divide by 0 and when the user tries to use an undefined variable.
