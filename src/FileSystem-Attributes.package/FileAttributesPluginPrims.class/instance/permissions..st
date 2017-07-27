mode bits
permissions: aPath
	"Answer the FileSystemPermission's for the supplied path"

	^FileSystemPermission posixPermissions: (self posixPermissions: aPath)
