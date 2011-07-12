sources := Functors.hs
targets := functors.png

all : $(targets)

functors.png : Functors.hs
	runhaskell '$<'
	optipng -o4 '$@' || :

clean ::
	$(RM) $(targets)
