tests
testTargetFile

	"The temporary file isn't a symbolic link, so the targetFile isn't supported"
	self should: [ self tempFileResource file targetFile ] raise: FileAttributeNotSupported.
	self should: [ self tempFileResource file attributes targetFile ] raise: FileAttributeNotSupported.