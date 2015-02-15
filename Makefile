all:
	erl -pa ebin/ -make

dialyzer:
	dialyzer ebin/
