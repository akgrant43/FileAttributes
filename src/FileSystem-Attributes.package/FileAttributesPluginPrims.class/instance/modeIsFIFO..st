mode bits
modeIsFIFO: aMode
	"Answer a boolean indicating whether the supplied mode has the FIFO type bit set"

	^(aMode bitAnd: S_IFMT) = S_IFIFO