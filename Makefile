source := Functors.hs
targets := functors.png function-instance.png

all : $(targets)

$(targets) : $(source)
	runhaskell -Wall -Werror $<
	optipng -o4 $(targets) || :

clean ::
	$(RM) $(targets)
