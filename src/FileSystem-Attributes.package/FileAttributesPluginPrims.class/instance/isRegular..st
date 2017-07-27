mode bits
isRegular: aPath
	"Answer a boolean indicating whether the supplied path is a Regular file"

	^self modeIsRegular: (self modeOf: aPath)
