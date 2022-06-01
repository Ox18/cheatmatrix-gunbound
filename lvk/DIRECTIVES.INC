{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2003, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This file contains compiler directives to instruct the compiler to
    compile my components and units in a way that I can vouch for. If you
    change any of these compiler directives, components may stop
    working properly.
}

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:51 $
// $Revision: 2 $
// $Archive: /Components/LVK/source/DIRECTIVES.INC $

{$B-} // short-circuit boolean evaluation
{$E-} // what is this ?
{$F-} // what is this ?
{$H+} // long strings
{$I+} // I/O checking
{$J-} // const means constant
{$K-} // what is this ?
{$N+} // what is this ?
{$P+} // open string parameters
{$S-} // what is this ?
{$T-} // typed address pointers
{$V+} // var-string checking
{$X+} // extended syntax allowed
{$Z1} // minimum enum size

{$IFDEF DELPHI6UP}
{$WARN SYMBOL_DEPRECATED OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_LIBRARY OFF}
{$WARN UNIT_PLATFORM OFF}
{$WARN UNIT_DEPRECATED OFF}
{$ENDIF}
{$IFDEF DELPHI7UP}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}
 
