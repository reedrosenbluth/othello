#Othello

This is a haskell implemenation of the classic game Othello (Reversi). I'm using the library [Threepenny-gui](https://www.haskell.org/haskellwiki/Threepenny-gui) to make the GUI. Threepenny-gui includes layout cominators to easily create HTML interfaces without using much CSS. It also has a [Functional Reactive Programming](https://www.haskell.org/haskellwiki/Functional_Reactive_Programming) library built-in.

Although the use of the FRP library is optional with Threepenny-gui, I decided to learn how to use it. Thus far, the bulk of the time i've spent working on this project has been doing so.

Right now, the game works in 2-player mode. It is missing some of the logic to prevent you
from making illegal moves, but so long that you make legal moves the game will work.

##Instructions
To run this code simply enter these two lines in the terminal

	ghc othello.hs
	./othello
	
Then visit `http://127.0.0.1:8023/`