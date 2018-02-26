# SUPERSEEDED

This code in this repository has been superseeded by:

https://github.com/akgrant43/pharo/tree/21368-Integrate-FileAttributesPlugin

See also: https://pharo.fogbugz.com/f/cases/21368/Integrate-FileAttributesPlugin





# FileAttributes

This repository contains the Pharo Smalltalk code to support and integrate the FileAttributesPlugin.

Integration in to Pharo is being tracked in the following issues:

- https://pharo.fogbugz.com/f/cases/18279/isSymlink-seems-to-be-broken-on-Linux

Assuming you have a Pharo VM with the FileAttributesPlugin available, to load the code:

```smalltalk
| prims |

Metacello new
	repository: 'github://akgrant43/FileAttributes/src';
	baseline: 'FileAttributes';
	load.
"The clunky initialisation code below is only for intermediate testing, 
and not required once the integration has been completed."
OSPlatform current isWindows 
	ifTrue: [prims := #WindowsFileAttributesPluginPrims asClass new]
	ifFalse: [prims := #UnixFileAttributesPluginPrims asClass new].
#DiskFileAttributes asClass initializeWithPrimitives: prims.
```

Note: The DiskFileAttributes initialization will be automatically performed once the code has been integrated in to the core system.

Source for the plugin itself is available from smalltalkhub.com:

```smalltalk
MCHttpRepository
	location: 'http://smalltalkhub.com/mc/Alistair/FileAttributesPlugin/main'
	user: ''
	password: ''
```

Once the core classes have been installed (but before integration), it is possible to run all the automated tests associated with files, i.e.

1. Open the Test Runner from the world menu
1. Select all packages that contain the substring 'file'
1. Select all tests
1. Run Selected 

A number of tests will fail:

- FileReferenceAttributeTests related failures are due to the lack of integration methods (which will be added next).
- FileHandleTest(FileSystemHandleTest)>>#testTruncate is an existing problem (fixed in Pharo 7).
- On Pharo 6.1 you may see FileLocatorTest>>#testMoveTo also fail (fixed in Pharo 7).

If you see additional failures, run the tests in a clean image and compare to that baseline.

If you see only these failures, everything is good so far.

At this stage the public interface to the new functionality is available through:

- FileReference>>attributes
- FileReference>>symlinkAttributes
- DiskFileAttributes
- DiskSymlinkAttributes


Integrating the new file attributes functionality in and making it the default behaviour is currently stored as a change set since the development was done under Pharo 6.  A github pull request will be created prior to submitting the patch.

To integrate the new functionality and make it the default behaviour:


```smalltalk
| fpi |


fpi := ('.' asFileReference allChildrenMatching: 'FileAttributesPluginIntegration.cs') first.
ChangeSet fileIntoNewChangeSet: fpi asFileReference asAbsolute fullName.
DiskStore current class useFilePlugin.
```

Re-running the test runner as described above should result in zero failures (ignoring #testTruncate and #testMoveTo (existing issues)).

