mode bits
modeIsBlock: aMode
	"Answer a boolean indicating whether the supplied mode has the Block type bit set"

	^(aMode bitAnd: S_IFMT) = S_IFBLK