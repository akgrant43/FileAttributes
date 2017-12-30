attributes
fileAttributes: aString mask: attributeMask

	^self primFileAttributes: (self encode: aString) mask: attributeMask