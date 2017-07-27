mode bits
modeIsRegular: aMode
	"Answer a boolean indicating whether the supplied mode has the Regular type bit set"

	^(aMode bitAnd: S_IFMT) = S_IFREG