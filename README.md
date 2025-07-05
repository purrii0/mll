# 🚀 MLL - *My Little Language*

**A lightweight, stack-based interpreted language with a playful core.**

## ✨ What is MLL?

**MLL** (short for *My Little Language*) is a tiny interpreted language 

It uses a **stack-based execution model**.


## ⚙️ Supported instructions

| Instruction          | Description                                  |
|----------------------|---------------------------------------------|
| `PUSH <value>`        | Push an integer onto the stack              |
| `POP`                 | Remove and print the top of the stack       |
| `ADD`                 | Pop two values, push their sum              |
| `SUB`                 | Pop two values, push (second - top)         |
| `JUMP.EQ.0 <label>`   | Jump to `<label>` if top == 0 (non-destructive) |
| `JUMP.GT.0 <label>`   | Jump to `<label>` if top > 0 (non-destructive) |
| `PRINT "<text>"`      | Print the given text                       |
| `HALT`                | Stop program execution immediately         |

It also supports **labels** for jumps:

```ml
START:
PUSH 10
...
JUMP.EQ.0 END
...
END:
PRINT "Done!"
HALT
```

## 🔥 Getting started
Write your .mll program in any text editor.

Run it through your interpreter:

```bash
./main.exe program.mll
```

## Example Program
program to check if the number is even or 
```ml
READ

loop:
PUSH 2
SUB

JUMP.GT.0 loop
JUMP.EQ.0 EVEN

PRINT "odd"
HALT

EVEN:
PRINT "even"
HALT
```