add_typechecker_test:
	@read -p "Enter test name: " testname; \
	touch typechecker/testdata/$$testname.golden; \
	touch typechecker/testdata/$$testname.input
