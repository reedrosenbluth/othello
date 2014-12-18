#Othello

This is a haskell implemenation of the classic game Othello (Reversi). I'm using the library [Threepenny-gui](https://www.haskell.org/haskellwiki/Threepenny-gui) to make the GUI. Threepenny-gui includes layout cominators to easily create HTML interfaces without using much CSS. It also has a [Functional Reactive Programming](https://www.haskell.org/haskellwiki/Functional_Reactive_Programming) library built-in.

##Instructions
To run this code create a cabal sandbox and build the project:

    cabal sandbox init
    cabal install --only-dependencies
    cabal build

Then run:

    ./dist/build/othello/othello
	
And visit `http://127.0.0.1:8023/`

To test the AI:

    cabal install --enable-tests
    cabal configure --enable-tests
    cabal repl test
    *Main> main
