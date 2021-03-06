running
setUp

	| ws |

	file := FileLocator temp / ('FileAttributesTests-', UUIDGenerator next asString, '.txt').
	beforeCreationTime := DateAndTime now.
	"Remove the nanoseconds"
	beforeCreationTime := DateAndTime
		date: beforeCreationTime asDate
		time: (Time seconds: beforeCreationTime secondsSinceMidnightLocalTime nanoSeconds: 0).
	ws := file writeStream.
	[
		ws nextPutAll: 'Created by FileAttributesTestsResources>>setUp '.
		beforeCreationTime printOn: ws.
	] ensure: [ ws close ].
	afterCreationTime := DateAndTime now rounded.
	"Round up to the next second"
	afterCreationTime := DateAndTime
		date: beforeCreationTime asDate
		time: (Time seconds: afterCreationTime secondsSinceMidnightLocalTime + 1 nanoSeconds: 0).
