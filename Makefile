source := Functors.hs
targets := \
	functors.png			\
	function-instance-fmap.png	\
	function-instance-ap.png	\
	function-instance-bind.png	\
	function-instance-lifta2.png	\
	on.png

all : $(targets)

$(targets) : $(source)
	runhaskell -Wall -Werror $<
	optipng -o4 $(targets) || :

clean ::
	$(RM) $(targets)
