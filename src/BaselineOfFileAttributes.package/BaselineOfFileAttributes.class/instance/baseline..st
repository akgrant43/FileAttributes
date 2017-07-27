baseline
baseline: spec
	<baseline>

	spec for: #common do: [ 
		spec blessing: #baseline.
		
		spec
			package: #'FileSystem-Attributes';
			package: #'FileSystem-Tests-Attributes';
			group: 'Core' with: #(#'FileSystem-Attributes');
			group: 'Tests' with: #(#'FileSystem-Tests-Attributes');
			group: 'all' with: #(#Core #Tests);
			group: 'default' with: #(#all).
		]