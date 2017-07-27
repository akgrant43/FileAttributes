private - accessing
allAttributeMask
	"Answer the mask used to retrieve all attributes"

	^self accessAttributeMask bitOr: self statAttributeMask