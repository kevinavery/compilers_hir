
# Compiles the program into "pytrans" executable
all: runner.rkt pytrans-stub.rkt
	raco exe -o pytrans runner.rkt 

# Accepts a Python source file on STDIN and sends output to STDOUT.
# Example: make run < test.py > test.py.trans
run: all
	@pylex | @pyparse | make -s trans

# Accepts the output of project 2 on STDIN and sends output to STDOUT.
# Example: make trans < test.py.parsed > test.py.trans
trans: all
	./pytrans

# Runs a single test as specified.
# Example: make spec test=relativePath/tst.py
# (relativePath must contain tst.py.parsed and tst.py.trans)
spec:
	racket runner.rkt ${test}

# Runs a single test as specified, with interpreter enabled.
# - If the transformed program is different, this will attempt to 
# interpret and compare the expected and actual resulting programs.
# - If the transformed program is the same, this will not bother
# to interpret the program.
speci:
	racket runner.rkt ${test} -i

# Runs all tests in specified directory.
# Example: make tests dir=relativePath
# (relativePath must contain tst.py, tst.py.parsed, and tst.py.trans)
tests: all
	for i in ${dir}/*.py; do ./pytrans $$i -i; done

