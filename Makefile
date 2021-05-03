.PHONY: test repl

test:
	scheme --libdirs src --script script/run-tests.scm test

repl:
	scheme --libdirs src

