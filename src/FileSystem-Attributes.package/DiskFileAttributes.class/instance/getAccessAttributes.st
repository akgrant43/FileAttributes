cache management
getAccessAttributes
	"Store the receivers access() attributes for future reference"

	^accessAttributes := Primitives fileAttributes: self pathString mask: self accessAttributeMask