private - accessing
statAttributes
	^ statAttributes ifNil: [ self getStatAttributes ]