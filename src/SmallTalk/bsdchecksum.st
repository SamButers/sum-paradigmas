target := FileStream open: 'anzu.png' .

checksum := 0 .

targetByte := target next .
[targetByte ~= nil] whileTrue: [
	byteValue := targetByte value .

	checksum := (checksum bitShift: -1) + ((checksum bitAnd: 1) bitShift: 15) .
	checksum := checksum + byteValue .
	checksum := checksum bitAnd: 16rFFFF .

	targetByte := target next .
]

checksum printNl

target close !