mode bits
isFIFO: aPath
	"Answer a boolean indicating whether the supplied path is a FIFO file"

	^self modeIsFIFO: (self modeOf: aPath)
