Object subclass: Sum [
	| filename flag |
	<comment: 'My instances are able to simulate the behavior of the UNIX sum utility.'>

	Sum class >> new: args [
		| r |
		r := super new .
		r init: args .
		^r
	]

	init: args [
		| argsSize currentArg |
		argsSize := args size .

		(argsSize > 0) ifTrue: [
			1 to: argsSize do: [ :i |
				currentArg := args at: i .
				((currentArg at: 1) == $-) ifTrue: [ self setFlag: currentArg ] ;
				ifFalse: [ filename := currentArg ] .
			] .
		];

		ifFalse: [
			'No implementation.' displayNl .
		] .
	]

	setFlag: arg [
		| terminalArguments validAlgorithms argSize |
		terminalArguments := #('--help' '--version') .
		validAlgorithms := #('s' 'r') .

		(arg = '--help') ifTrue: [ self printHelp ] ;

		ifFalse: [
			(arg = '--version') ifTrue: [ self printVersion ] ;

			ifFalse: [
				argSize := arg size .

				2 to: argSize do: [ :i | (validAlgorithms includes: (arg at: i)) ifTrue: [ flag := (arg at i) ] ;
				ifFalse: [ self printInvalidArg: (arg at: i) ]]
			]
		]
	]

	printHelp [
		'Usage: sum [OPTION]... [FILE]...
Print checksum and block counts for each FILE.

With no FILE, or when FILE is -, read standard input.

  -r              use BSD sum algorithm, use 1K blocks
  -s, --sysv      use System V sum algorithm, use 512 bytes blocks
      --help     display this help and exit
      --version  output version information and exit

GNU coreutils online help: <https://www.gnu.org/software/coreutils/>
Full documentation at: <https://www.gnu.org/software/coreutils/sum>
or available locally via: info ''(coreutils) sum invocation''' displayNl .
		ObjectMemory quit: 0
	]

	printVersion [
		'sum (GNU coreutils) 8.30
Copyright (C) 2018 Free Software Foundation, Inc.
License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.

Written by Kayvan Aghaiepour and David MacKenzie.' displayNl .
		ObjectMemory quit: 0
	]

	printInvalidArg: arg [
		'sum: invalid option -- ''' display .
		arg display .
		'''' displayNl .
		'Try ''sum --help'' for more information.' displayNl .
		ObjectMemory quit: 0
	]
]
