FileStream fileIn: 'Sum.st' .

sumArguments := Smalltalk arguments .

sumRequest := Sum new: sumArguments .
sumRequest run