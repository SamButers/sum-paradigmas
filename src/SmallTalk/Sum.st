Object subclass: Sum [
	| targets flag |
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
		targets := Set new .
		flag := $r .

		(argsSize > 0) ifTrue: [
			1 to: argsSize do: [ :i |
				currentArg := args at: i .
				((currentArg at: 1) = $-) ifTrue: [ self setFlag: currentArg ] ;
				ifFalse: [ targets add: currentArg ] .
			] .
		];

		ifFalse: [
			'No implementation.' displayNl .
		] .
	]

	setFlag: arg [
		| validAlgorithms argSize |
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

	run [
		| currentTarget |
		self openTargets .
		(flag = $r) ifTrue: [
			1 to: (targets size) do: [ :i |
				currentTarget := targets at: i .

				(currentTarget = nil) ifTrue: [ self printInvalidFile: currentTarget ] ;
				ifFalse: [
					self bsdChecksum: currentTarget .
					'    ' display .
					(((currentTarget size) / 1024.0) ceiling) displayNl .
					currentTarget close
				] .
			]
		] ;
		ifFalse: [ 'Not implemented.' displayNl ]
	]

	bsdChecksum: target [
		| checksum byteValue targetByte |

		checksum := 0 .
		targetByte := target next .
		[targetByte ~= nil] whileTrue: [
			byteValue := targetByte value .

			checksum := (checksum bitShift: -1) + ((checksum bitAnd: 1) bitShift: 15) .
			checksum := checksum + byteValue .
			checksum := checksum bitAnd: 16rFFFF .

			targetByte := target next .
		] .

		checksum display
	]

	openTargets [
		| filepath targetsArray |

		targetsArray := targets asArray .
		1 to: (targets size) do: [:i | 
			filepath := targetsArray at: i .

			((File exists: filepath) = true) ifTrue: [ targetsArray at: 1 put: (FileStream open: filepath) ] ;
			ifFalse: [ targetsArray at: 1 put: nil ]
		] .

		targets := targetsArray
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
