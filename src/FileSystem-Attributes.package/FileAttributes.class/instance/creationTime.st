accessing
creationTime
	"Answer the files creation time.
	Different platforms will interpret this in different ways."

	^FileAttributeNotSupported signalWith: self path