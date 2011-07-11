sources := functors.dot
targets := $(sources:.dot=.png)

all : $(targets)

%.png : %.dot
	dot -Tpng -o'$@' '$<'
	optipng -o4 '$@' || :

clean ::
	$(RM) $(targets)
