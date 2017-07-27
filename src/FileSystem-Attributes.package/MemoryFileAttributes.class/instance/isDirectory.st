testing
isDirectory

	^store nodeAt: path
		ifPresent: [ :node | node isDirectory ]
		ifAbsent: [ false ].