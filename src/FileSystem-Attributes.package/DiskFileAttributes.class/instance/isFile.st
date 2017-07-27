testing - stat
isFile
	"Answer true if the receiver is a file (exists and is not a directory)"
	
	path isRoot ifTrue: [ ^false ].
	^[ (Primitives modeIsDirectory: self mode) not ]
		on: FileDoesNotExist
		do: [ false ].