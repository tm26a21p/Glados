#!/usr/bin/env bash

if [ ! -d "Tests" ];
then
	mkdir Tests
fi

while IFS= read -u3 -r -d '' file; do
	bigName=${file#"test/fileTest/"}
	bigName=${bigName%".erl"}
	testName="Tests/$bigName.test"
	echo -e "\e[35m$testName"; echo -e "\e[0m"
	./glados $file
    ./test/functionalTesting/$bigName.escript > $testName
    rm $bigName.beam
done 3< <(find test/fileTest -name "*.erl" -print0)

value=0

while IFS= read -u3 -r -d '' file; do
	testName=${file#"Tests/"}
	testName=${testName%".test"}
	if diff $file TestsResult/$testName.test
	then
		echo -e "\e[32mOK" : $testName
	else
		echo -e "\e[31mFAIL" : $testName
		value=84
	fi
	echo -e "\e[0m"
done 3< <(find Tests -name "*.test" -print0)

exit $value;