cache management
getStatAttributes
	"Store the receivers stat() attributes for future reference"

	statAttributes := Primitives fileAttributes: self pathString mask: self statAttributeMask.
	(statAttributes at: 1) ifNotNil: 
		[ statAttributes at: 1 put: (Primitives decode: (statAttributes at: 1)) ].
	^statAttributes