sources := Functors.hs
targets := functors.png

all : $(targets)

functors.png : Functors.hs
	runhaskell -Wall -Werror '$<'
	optipng -o4 '$@' || :

clean ::
	$(RM) $(targets)
