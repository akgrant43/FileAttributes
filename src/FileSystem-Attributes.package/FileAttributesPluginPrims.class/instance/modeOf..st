mode bits
modeOf: aPath
	"Answer the statBuf.st_mode for the supplied path"
	
	^self fileAttribute: aPath number: 2