unit DUnitConsts;

interface

resourcestring
  sTitle = 'DUnit - An Extreme Testing Framework';

  sPopupTitle   = 'TestCase Run-Time Applied Properties';
  sPopupPrevious = ' Previous';
  sPopupRun     = ' Run Selected Test';
  sPopupNext    = ' Next';

  sSetupDecorator = 'Setup decorator (%s)';
  sTestMemory = 'Test memory of %s';
  sMemoryChanged = 'Memory use changed by %d bytes';

  sNilListener = 'listener is nil';
  sFailedSetup = 'SetUp FAILED: ';
  sFailedTearDown = 'TearDown FAILED: ';
  sNonNilTestResult = 'Expected non nil TestResult';
  sTests = 'Tests';
  sIdenticalContent = 'Memory content was identical:';
  sExpectedException = 'Expected exception "%s" but there was none. %s';
  sEmptyTest = 'Empty test';
  sExceptionNothig = 'nothing';
  sAllowLeakArrayValues = 'Too many values in for AllowedLeakArray. Limit = ';
  sNoChecksExecuted = 'No checks executed in TestCase';
  sExceptionUnexpected = 'unexpected exception';
  sMethodNotFound = 'Method not found: "';

  sLoadModule = 'Could not load module %s: %s';
  sExportFunction = 'Module "%s" does not export a "Test" function: %s';
  sReturnInterface = 'Module "%s.Test" did not return an ITest';

  sPrintError = 'There was %d error:';
  sPrintErrors = 'There were %d errors:';
  sFailedTestDetails = '%3d) %s: %s'#13#10'     at %s'#13#10'      "%s"';
  sPrintFailure = 'There was %d failure:';
  sPrintFailures = 'There were %d failures:';
  sTestResultsOk = 'OK: %d tests';
  sTestResultsFailures = 'FAILURES!!!';
  sTestResults = 'Test Results:';
  sRunCount = 'Run:      %8d';
  sFailureCount = 'Failures: %8d';
  sErrorsCount = 'Errors:   %8d';
  sDUnitTesting = 'DUnit / Testing';
  sDecodeTime = 'Time: %d:%2.2d:%2.2d.%d';
  sPressReturn = 'Press <RETURN> to continue.';

  sUnsupportedTypeInfo = '<Unsupported type info>';
{$IFDEF CLR}
  sUnkownFieldType = '<Unknown>';
{$ENDIF CLR}

implementation

end.
