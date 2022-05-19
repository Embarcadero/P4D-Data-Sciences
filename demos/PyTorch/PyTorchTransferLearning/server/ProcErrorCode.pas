unit ProcErrorCode;

interface

const
  cGENERIC_ERROR_EXIT_CODE = $000A;
  cPYTHON_ERROR_EXIT_CODE  = $000B;
  cPYTHON_DLL_ERROR_EXIT_CODE = $000C;
  cPYTHON_DLL_MAP_ERROR_EXIT_CODE = $000D;

resourcestring
  mGENERIC_ERROR_EXIT_MSG = 'Process failed with an unknown error.';
  mPYTHON_ERROR_EXIT_MSG = 'A Python error has occurred.';
  mPYTHON_DLL_ERROR_EXIT_MSG = 'The process has failed loading the Python interpreter.';
  mPYTHON_DLL_MAP_ERROR_EXIT_MSG = 'The process has failed mapping the Python interpreter.';

implementation

end.
