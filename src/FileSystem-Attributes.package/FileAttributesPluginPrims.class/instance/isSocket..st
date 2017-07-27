mode bits
isSocket: aPath
	"Answer a boolean indicating whether the supplied path is a Socket file"

	^self modeIsSocket: (self modeOf: aPath)
