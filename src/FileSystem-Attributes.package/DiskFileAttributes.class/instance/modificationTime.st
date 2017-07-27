accessing
modificationTime
	^DateAndTime fromUnixTime: (self statAttributes at: 10)