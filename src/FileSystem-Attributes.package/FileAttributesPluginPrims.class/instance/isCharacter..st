mode bits
isCharacter: aPath
	"Answer a boolean indicating whether the supplied path is a Character file"

	^self modeIsCharacter: (self modeOf: aPath)
