mode bits
isDirectory: aPath
	"Answer a boolean indicating whether the supplied path is a Directory"

	^[ self modeIsDirectory: (self modeOf: aPath) ]
		on: FileDoesNotExist
		do: [ false ].