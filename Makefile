all:
	erl -make

dialyzer:
	dialyzer ebin/
