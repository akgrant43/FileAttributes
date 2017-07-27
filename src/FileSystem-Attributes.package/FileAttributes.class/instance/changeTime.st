accessing
changeTime
	"Answer the files change time"

	^FileAttributeNotSupported signalWith: self path