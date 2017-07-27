testing - stat
isDirectory
	"Answer true if the receiver is a directory"

	path isRoot ifTrue: [ ^true ].
	^[ Primitives modeIsDirectory: self mode ]
		on: FileDoesNotExist
		do: [ false ].