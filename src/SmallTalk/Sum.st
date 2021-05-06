Object subclass: Sum [
	| targets targetsStream flag blockDigits |
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
		targets := OrderedCollection new .
		flag := $r .

		(argsSize > 0) ifTrue: [
			1 to: argsSize do: [ :i |
				currentArg := args at: i .
				((currentArg at: 1) = $-) ifTrue: [ self setFlag: currentArg ] ;
				ifFalse: [ targets addLast: currentArg ] .
			] .
		];

		ifFalse: [
			'No implementation.' displayNl .
		] .
	]

	setFlag: arg [
		| validAlgorithms argSize |
		validAlgorithms := #($s $r) .

		(arg = '--help') ifTrue: [ self printHelp ] ;

		ifFalse: [
			(arg = '--version') ifTrue: [ self printVersion ] ;

			ifFalse: [
				argSize := arg size .

				2 to: argSize do: [ :i | (validAlgorithms includes: (arg at: i)) ifTrue: [ flag := (arg at: i) ] ;
				ifFalse: [ self printInvalidArg: (arg at: i) ]]
			]
		]
	]

	run [
		| currentTarget targetsAmount blocks |
		self openTargets .
		targetsAmount := targets size .

		(flag = $r) ifTrue: [
			blocks := self generate1KBlocks .

			1 to: targetsAmount do: [ :i |
				currentTarget := targetsStream at: i .

				(currentTarget = nil) ifTrue: [ self printInvalidFile: currentTarget ] ;
				ifFalse: [
					self bsdChecksum: currentTarget .
					'    ' display .
					self display1KBlock: (blocks at: i) .

					(targetsAmount > 1) ifTrue: [ 
						' ' display .
						(targets at: i) display
					] .

					'' displayNl .
					currentTarget close
				] .
			]
		] ;

		ifFalse: [
			1 to: targetsAmount do: [ :i |
				currentTarget := targetsStream at: i .

				(currentTarget = nil) ifTrue: [ self printInvalidFile: currentTarget ] ;
				ifFalse: [
					self sysvChecksum: currentTarget .
					' ' display .
					self display512Blocks: currentTarget .

					(targetsAmount > 1) ifTrue: [ 
						' ' display .
						(targets at: i) display
					] .

					'' displayNl .
					currentTarget close
				] .
			]
		]
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

	sysvChecksum: target [
		| s r checksum byteValue targetByte |

		checksum := 0 .
		s := 0 .
		r := 0 .
		targetByte := target next .
		[targetByte ~= nil] whileTrue: [
			byteValue := targetByte value .
			s := s + byteValue .

			targetByte := target next .
		] .

		r := (s bitAnd: 16rFFFF) + ((s bitAnd: 16rFFFFFFFF) bitShift: -16) .
		checksum := (r bitAnd: 16rFFFF) + (r bitShift: -16) .

		checksum display
	]

	generate1KBlocks [
		| blocks block blockSize target |

		blocks := OrderedCollection new .
		blockDigits := 0 .
		1 to: (targets size) do: [ :i | 
			target := targetsStream at: i .

			block := (((target size) / 1024.0) ceiling) asString .
			blocks addLast: block .
			blockSize := block size .

			(blockSize > blockDigits) ifTrue: [ blockDigits := blockSize ]
		] .

		^blocks
	]

	display512Blocks: target [
		(((target size) / 512.0) ceiling) display
	]

	display1KBlock: block [
		| blockSize |

		blockSize := block size .
		0 to: (blockDigits - blockSize) do: [ :i | ' ' display ] .

		block display
	]

	openTargets [
		| filepath |

		targetsStream := targets asArray .

		1 to: (targets size) do: [:i | 
			filepath := targets at: i .

			((File exists: filepath) = true) ifTrue: [ targetsStream at: i put: (FileStream open: filepath) ] ;
			ifFalse: [ targetsStream at: i put: nil ]
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
