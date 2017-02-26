# noughtsandcrosses

## The Challenge
 Someone suggested that we should have a noughts-and-crosses challenge to get us into writing Haskell code. So I decided to write a framework that would allow us to write our own "bots" and play them against each other.
 
I don't really speak Haskell or understand functional programming very well, so the attached code could probably be improved significantly. But if you're interested in testing your Haskell skillz and noughts-and-crosses game theory, I challenge you to submit a bot!
 
## How it works
 
The board is represented by a 9-element array of '-' for blank, 'O' for noughts and 'X' for crosses.
Crosses gets to move first.
You need to supply a function called "move" which takes in a symbol (as a char) and a board (as a 9-array), and returns a 0-based index indicating where you would like to place your symbol.
The reason for passing the symbol to your move function is so that you know which are "your" cells and which are the enemy cells.
If you return an unacceptable move, the game ends with an error; at some point we can change this to be a loss.
You can put anything else in your file, and feel free to use functions from the Utils,hs. It's all functional, so there's no state to be changed. So if you want to run a full tree search over available moves by playing the board, you won't affect any state until you actually return from your move function.
To compile and run, download the 4 files, Utils.hs, Player1.hs, Player2.hs and nac.hs, compile using "ghc nac.hs" and then execute nac.exe. (ghc --make nac.hs; ./nac in Linux)

## Weird features

Unfortunately, this is where I ran into my limitations with Haskell. I can't figure out how to inject a module at run-time. Ideally, we would just specify "Bob.hs" and "Sam.hs" and then execute nac.exe Bob Sam. But I couldn't do that, partly because it's statically linked and partly because the file name needs to be the same as the module name (inside the file). So you need to edit Player1.hs or Player2.hs to implement your strategy. nac.hs specifies that Player1.move plays first and Player2.move plays second. To run this as a tournament, a batch script is needed that takes e.g. Bob.hs and renames it to Player1.hs, and then changes "module Bob" to "module Player1" inside the file. Not difficult, but not as smooth as I'd like. So any suggestions would be much appreciated.