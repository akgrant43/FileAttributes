# FileAttributes

This repository contains the Pharo Smalltalk code to support and integrate the FileAttributesPlugin.

Assuming you have a Pharo VM with the FileAttributesPlugin available, to load the code:

```smalltalk
Metacello new
	repository: 'github://akgrant43/FileAttributes/src';
	baseline: 'FileAttributes';
	load.
#DiskFileAttributes asClass initializeWithPrimitives: #FileAttributesPluginPrims asClass new.
```

Note: The DiskFileAttributes initialization will be automatically performed once the code has been integrated in to the core system.

Source for the plugin itself is available from smalltalkhub.com:

```smalltalk
MCHttpRepository
	location: 'http://smalltalkhub.com/mc/Alistair/FileAttributesPlugin/main'
	user: ''
	password: ''
```

