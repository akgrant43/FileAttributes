helper methods
attributesCollection
	"Answer an array of FileAttributes for the test resource file.
	This is currently a single entry, but leaving the infrastructure in place in case it is extended."
	
	^{
		self tempFileAttributes.
	}