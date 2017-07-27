mode bits
modeIsSocket: aMode
	"Answer a boolean indicating whether the supplied mode has the Socket type bit set"

	^(aMode bitAnd: S_IFMT) = S_IFSOCK