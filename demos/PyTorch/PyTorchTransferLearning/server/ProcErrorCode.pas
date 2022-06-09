(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


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
