mode bits
isSymlink: aPath
	"Answer a boolean indicating whether the supplied path is a Symlink file"

	^self fileAttribute: aPath number: 16
