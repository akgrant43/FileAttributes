cache management
getAllAttributes
	"Store all the receivers attributes for future reference"

	| allAttributes |
	
	allAttributes := Primitives fileAttributes: self pathString mask: self allAttributeMask.
	statAttributes := allAttributes at: 1.
	accessAttributes := allAttributes at: 2.
	(statAttributes at: 1) ifNotNil: 
		[ statAttributes at: 1 put: (Primitives decode: (statAttributes at: 1)) ].
