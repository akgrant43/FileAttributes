testing
isFile

	^store nodeAt: path
		ifPresent: [ :node | node isFile ]
		ifAbsent: [ false ].