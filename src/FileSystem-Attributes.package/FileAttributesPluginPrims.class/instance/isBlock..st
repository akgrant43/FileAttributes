mode bits
isBlock: aPath
	"Answer a boolean indicating whether the supplied path is a Block file"

	^self modeIsBlock: (self modeOf: aPath)
