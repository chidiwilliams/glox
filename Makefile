# Creates a .golden file and a .input file
# for a new test in the typechecker package
add_typechecker_test:
	@read -p "Enter test name: " testname; \
	touch typechecker/testdata/$$testname.golden; \
	touch typechecker/testdata/$$testname.input
