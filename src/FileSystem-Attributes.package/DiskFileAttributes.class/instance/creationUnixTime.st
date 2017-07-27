accessing
creationUnixTime
	"Answer the receivers creation time.  If this is not available, answer the change time, i.e. the time at which the receivers metadata was last updated."

	^(self statAttributes at: 12) ifNil: [ self statAttributes at: 11 ]