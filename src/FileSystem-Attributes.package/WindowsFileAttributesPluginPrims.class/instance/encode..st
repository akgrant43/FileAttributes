private - encoding
encode: aString
	"On Windows the primitives take native strings"

	^aString ifEmpty: [ 'C:\' ]