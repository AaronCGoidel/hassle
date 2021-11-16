# Hassle
A dumb Brainfuck interpreter written in Haskell

- 100% function**al**
- 50% function**ing**

# Usage
Clone this repo and run `hassle.hs`

By compiling
```bash
ghc hassle.hs
./hassle
```

Or in interactive mode
```bash
ghci hassle.hs
```

By default, if you compile and run `hassle.hs`, it will run a test suite and then will execute the following bf programs:
- Hello, World!
- Calculate 10 digits of pi

You can run your own programs by envoking `runbf <bf instructions> <input>` with whatever bf program and string of input bytes you'd like.

# TODO
- handle errors gracefully (...or at all)
- improved I/O
