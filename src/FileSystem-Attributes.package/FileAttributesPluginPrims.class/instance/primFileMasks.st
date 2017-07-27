primitives
primFileMasks
	"Answer an array of well known masks:
	
	1: S_IFMT
	2: S_IFSOCK
	3: S_IFLNK
	4: S_IFREG
	5: S_IFBLK
	6: S_IFDIR
	7: S_IFCHR
	8: S_IFIFO

	For more information, see: http://man7.org/linux/man-pages/man2/stat.2.html
	"
	<primitive: 'primitiveFileMasks' module: 'FileAttributesPlugin'>
	^self primitiveFailed