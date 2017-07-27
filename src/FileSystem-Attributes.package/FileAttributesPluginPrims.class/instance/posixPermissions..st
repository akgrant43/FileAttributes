mode bits
posixPermissions: aPath
	"Answer the posix permissions for the supplied path"

	^(self modeOf: aPath) bitAnd: 8r777
