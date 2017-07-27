accessing
changeTime

	| unixTime |

	unixTime := self statAttributes at: 11.
	^unixTime 
		ifNil: [ nil ]
		ifNotNil: [ DateAndTime fromUnixTime: unixTime ].