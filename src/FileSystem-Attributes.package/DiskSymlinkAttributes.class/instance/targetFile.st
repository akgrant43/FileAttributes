accessing
targetFile
	"Answer the Path pointing to the real file.
	If the receiver is a symbolic link, answer a file reference of the target.
	If not, answer the receivers file"

	| fileString |
	fileString := self statAttributes first.
	^fileString
		ifNil: [ path ]
		ifNotNil: [ fileString asPath ].
