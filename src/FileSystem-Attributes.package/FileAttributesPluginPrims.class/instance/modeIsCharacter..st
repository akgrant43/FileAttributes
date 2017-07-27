mode bits
modeIsCharacter: aMode
	"Answer a boolean indicating whether the supplied mode has the Character type bit set"

	^(aMode bitAnd: S_IFMT) = S_IFCHR