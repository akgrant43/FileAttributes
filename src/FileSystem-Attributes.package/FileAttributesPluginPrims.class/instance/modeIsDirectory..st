mode bits
modeIsDirectory: aMode
	"Answer a boolean indicating whether the supplied mode has the Directory type bit set"

	^(aMode bitAnd: S_IFMT) = S_IFDIR