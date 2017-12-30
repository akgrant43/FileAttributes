private - encoding
fixPath: pathString
	"An empty path string is assumed to refer to the current directory. opendir()
	on an empty string will fail, so give it an explicit path to the current
	directory."

	^ pathString isEmpty
		ifTrue: ['.']
		ifFalse: [pathString]