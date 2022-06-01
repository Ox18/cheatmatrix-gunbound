{ Description:
    This unit contains the basic framework for the preprocessor code for
    the active scripting classes.
}
unit lvkActiveScriptPreProcessor;

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:50 $
// $Revision: 4 $
// $Archive: /Components/LVK/source/lvkActiveScriptPreProcessor.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes;

type
  { Description:
      A script preprocessor class must inherit from this class and implement the
      two methods HandlesLanguage and Execute.
  }
  TScriptPreProcessorHandler = class
  public
    constructor Create; virtual;

    { Description:
        This method is called to check if the class can handle (preprocess)
        script code written in the given language. Examples of language names
        can be VBScript, JScript, JavaScript, Python, etc.

        If the framework gets a positive response, it will call the
        Execute@string@TStrings method to preprocess the script code.
      Parameters:
        Language - The name of the language to check for.
      Returns:
        True if the class handles the language with the given name, False if
        not.
      See also:
        Execute@string@TStrings
    }
    class function HandlesLanguage(const Language: string): Boolean;
      virtual; abstract;

    { Description:
        This method is the one that actually does the preprocessing. The
        purpose of this method is to take the script code in the Code parameter,
        and change it. The change depends on the preprocessor class
        implementation.
      Parameters:
        Language - The name of the language that the script code is written in.
          The preprocessor will typically use this to write code in the
          proper language.
        Code - The script code to read, preprocess and return. The changed
          script code should be written into this object.
      See also:
        HandlesLanguage
    }
    procedure Execute(const Language: string; const Code: TStrings);
      virtual; abstract;
  end;

  EPreProcessor = class(Exception);

  TScriptPreProcessorHandlerClass = class of TScriptPreProcessorHandler;

{ Description:
    This procedure is used to register a new script preprocessor class with
    the framework. The active scripting engine will simply call the
    PreProcess@string@string method to preprocess the code, and this
    function will iterate through all the registered preprocessors and ask
    each of them in turn to preprocess the code.
  Parameters:
    PreProcessorHandlerClass - The class type to register.
  See also:
    PreProcess, TScriptPreProcessorHandler
}
procedure RegisterPreProcessorHandler(
  const PreProcessorHandlerClass: TScriptPreProcessorHandlerClass);

{ Description:
    This function takes the given script code, runs through all the registered
    preprocessor classes, asks each one in turn if they can handle the given
    language, and if it can, runs the code through that preprocessor.

    The output is given to the next preprocessor who can handle the language,
    or returned back to the calling code.
  Parameters:
    Language - The language that the script code is written in.
    Input - The input script code to preprocess.
  Returns:
    The preprocessed script code.
  See also:
    RegisterPreProcessorHandler
}
function PreProcess(const Language, Input: string): string;

implementation

uses
  Windows, lvkRegExp;

var
  PreProcessorClasses : array of TScriptPreProcessorHandlerClass  = nil;

procedure RegisterPreProcessorHandler(
  const PreProcessorHandlerClass: TScriptPreProcessorHandlerClass);
begin
  SetLength(PreProcessorClasses, Length(PreProcessorClasses)+1);
  PreProcessorClasses[High(PreProcessorClasses)] := PreProcessorHandlerClass;
end;

function PreProcess(const Language, Input: string): string;
var
  PreProcessors : packed array of TScriptPreProcessorHandler;
  Index         : Integer;
  ScriptCode    : TStrings;
begin
  if Length(PreProcessorClasses) = 0 then
  begin
    Result := Input;
    Exit;
  end;

  try
    SetLength(PreProcessors, 0);
    for Index := 0 to Length(PreProcessorClasses)-1 do
      if PreProcessorClasses[Index].HandlesLanguage(Language) then
      begin
        SetLength(PreProcessors, Length(PreProcessors)+1);
        PreProcessors[High(PreProcessors)] := PreProcessorClasses[Index].Create;
      end;

    ScriptCode := TStringList.Create;
    try
      ScriptCode.Text := Input;

      for Index := 0 to Length(PreProcessors)-1 do
        PreProcessors[Index].Execute(Language, ScriptCode);

      Result := ScriptCode.Text;
    finally
      ScriptCode.Free;
    end;
  finally
    for Index := 0 to Length(PreProcessors)-1 do
      PreProcessors[Index].Free;
  end;
end;

{ TScriptPreProcessorHandler }

constructor TScriptPreProcessorHandler.Create;
begin
  inherited;
end;

end.
