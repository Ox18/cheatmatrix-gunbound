{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{* Portions contributed by Wim van der Vegt                                   *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the TlvkActiveScript class and the
    ElvkActiveScriptCompile exception class.
}
unit lvkActiveScript;

// $Author: Lasse V. Karlsen $
// $Revision: 15 $
// $Date: 17.04.03 15:00 $
// $Archive: /Components/LVK/Source/lvkActiveScript.pas $

// Reference: activscp.h
// Reference: olescrpt.h

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  {$IFDEF DELPHI6UP}
  Variants,
  {$ENDIF}
  Registry, SysUtils, Classes, Windows, ActiveX, ComObj,
  lvkActiveScriptInterfaces, lvkActiveScriptSite, lvkActiveScriptPreProcessor,
  lvkVersion, lvkSQLGlobal, lvkVBScriptPreProcessor, lvkSQLScriptPreProcessor,
  lvkFileScriptPreProcessor;

type
  { Description:
      See the help page for IlvkActiveScript.GetIdentifierList@TStrings
      for more information about this type.
  }
  TGetIdentifierListInclude = (gilModules, gilObjects);
  // <COMBINE TGetIdentifierListInclude>
  TGetIdentifierListIncludes = set of TGetIdentifierListInclude;

const
  DEFAULT_LIST_INCLUDES = [Low(TGetIdentifierListInclude)..High(TGetIdentifierListInclude)];

type
  PObjectRec = ^TObjectRec;
  TObjectRec = record
    // Name of object
    Name      : string;

    // Pointer to object instance
    Instance  : IDispatch;

    // Should this objects contents be registered globally?
    Global    : Boolean;
  end;

  TTypeLibraryRec = record
    CLSID         : TCLSID;
    MajorVersion  : LongWord;
    MinorVersion  : LongWord;
    IsControl     : Boolean;
  end;

  TlvkActiveScript = class;
  TlvkActiveScriptModuleCollection = class;
  TScriptFilename = string;

  { Description:
      This class contains the code for implementing module support in the
      TlvkActiveScript class. It handles all the necessary details about
      loading code from files. It can contain the scriptcode as a set of
      code lines in the ScriptCode property, or as a filename reference.
    See also:
      TlvkActiveScriptModuleCollection, TlvkActiveScript,
      TlvkActiveScript.Modules
  }
  TlvkActiveScriptModule = class(TCollectionItem)
  private
    FActiveScript   : TlvkActiveScript;
    FName           : string;
    FScriptCode     : TStrings;
    FScriptFilename : TScriptFilename;
    FGlobalMembers  : Boolean;
    FPreProcess     : Boolean;

    procedure Invalidate;

    procedure ScriptChanged(Sender: TObject);
    procedure SetName(const Value: string);
    procedure SetScriptCode(const Value: TStrings);
    procedure SetScriptFilename(const Value: TScriptFilename);
    procedure SetGlobalMembers(const Value: Boolean);
    procedure InitializeModule(const ActiveScript: TlvkActiveScript);
    function GetPreProcess: Boolean;
    procedure SetPreProcess(const Value: Boolean);

  protected
    { Description:
        This method returns the display name to use in the collection editor.
        By default this is the inherited value, but it will return the name
        of the script file loaded, or the value "inline code" if the code
        is stored in the collection.
    }
    function GetDisplayName: string; override;

    { Description:
        This method returns the script code to load into the scripting engine.
        It will either load the code from file, or return the code in the
        internal script code storage.
      Return value:
        String containing the script code.
    }
    function GetScriptCode: string;

  public
    procedure Assign(Source: TPersistent); override;

    { Description:
        This constructor creates and initializes the module item and connects
        it to the given collection. Internally, a call to InitializeModule
        is executed.
      Parameters:
        Collection - The collection that owns the item.
      See also:
        CreateDefault
    }
    constructor Create(Collection: TCollection); override;

    { Description:
        This constructor is used to create the default module, which has no
        name and is not owned by any collection. Internally, a call to
        InitializeModule is executed.
      Parameters:
        ActiveScript - The owning ActiveScript class.
      See also:
        Create
    }
    constructor CreateDefault(const ActiveScript: TlvkActiveScript);

    { Description:
        Default destructor. Unloads script code and deallocates all internal
        objects.
    }
    destructor Destroy; override;

  published
    { Description:
        The Name property determines the name of the module. All modules,
        except the default module, needs to have a unique name given to it. The
        name can not contain spaces or any other delimiter characters and has
        to be on the form of:

        * Start with a letter (a-z or A-Z)
        * Rest of name can be letters (a-z or A-Z), digits (0-9) or
          underscores (_)
    }
    property Name: string read FName write SetName;

    { Description:
        The ScriptCode property holds a copy of the current script code to load
        from this module.

        Note: For each module you can set either the ScriptCode or the
          ScriptFilename property, but not both. Setting one will clear the
          other.
    }
    property ScriptCode: TStrings read FScriptCode write SetScriptCode;

    { Description:
        The ScriptFilename property contains the name of the file to load
        script code from for this module.

        Note: For each module you can set either the ScriptCode or the
          ScriptFilename property, but not both. Setting one will clear the
          other.
        Note: This property can use environment strings in the format
          \%VARIABLENAME\% and those variables will be replaced by the contents
          of those variables. A variable with the name of \%APPLICATION\% also
          exists and holds the value of the path of the exe file currently
          running, including a terminating backslash.
    }
    property ScriptFilename: TScriptFilename read FScriptFilename write SetScriptFilename;

    { Description:
        This property is used to determine the scope of the members (procedures,
        functions and variables) available in the module.

        If you set the value of this property to True, all members will be
        available in all other modules as though they were part of the script
        engine themselves, like this (in a different module):

          SomeProcedureInThatOtherModule(10)

        If you set the value to False, any access to the members of this module
        must be prefixed with the name of the module and a dot (.), like this:

          ModuleName.SomeProcedureInThatOtherModule(10)
    }
    property GlobalMembers: Boolean read FGlobalMembers write SetGlobalMembers;

    { Description:
        This property determines wether the script code in this module is to
        be preprocessed or note. If you set this to True, make sure you also
        include one or more of the preprocessor units in a uses clause
        somewhere. The currently available preprocessor units are:

          lvkVBScriptPreProcessor
          lvkSQLScriptPreProcessor
    }
    property PreProcess: Boolean read GetPreProcess write SetPreProcess;
  end;

  { Description:
      This class implements a simple collection of modules to load code from in
      a TlvkActiveScript class.
    See also:
      TlvkActiveScriptModule, TlvkActiveScript,
      TlvkActiveScript.Modules
  }
  TlvkActiveScriptModuleCollection = class(TOwnedCollection)
  private
    FActiveScript   : TlvkActiveScript;
    FNextNameIndex  : Integer;

    function GetItem(const Index: Integer): TlvkActiveScriptModule;
    function GetItemByName(const Name: string): TlvkActiveScriptModule;
    procedure SetItem(const Index: Integer; const Value: TlvkActiveScriptModule);

  protected
    { Description:
        This method is used internally for all new modules and it will
        assign a new, unique, name to each. The reason for this is that no
        module may have a blank name. Names will follow the pattern of
        "Module1", "Module2", "Module3", ...
    }
    function NewModuleName: string;

  public
    { Description:
        This method creates and initializes the module collection class
        and hooks it up to the activescript object that owns it.
    }
    constructor Create(const ActiveScript: TlvkActiveScript);

    { Description:
        This property returns a reference to the ActiveScript object that owns
        this module collection.
    }
    property ActiveScript: TlvkActiveScript read FActiveScript;

    { Description:
        This adds a new module to the collection and returns a reference to the
        newly added module object.

        It works exactly like
        <EXTLINK borland://TCollectionItem.Add>TCollectionItem.Add</EXTLINK>
        except that it returns an object of type TlvkActiveScriptModule.

        If you specify a modulename, that modulename will be used, unless a
        module by that name already exists. If you leave the ModuleName
        parameter blank and/or ommit it, a default module name will be given
        to the module, starting with Module1 and incrementing the numeric
        part of it for each new module.
      See also:
        Insert
    }
    function Add(const ModuleName: string=''): TlvkActiveScriptModule;

    { Description:
        This finds a module with the given ID and returns a reference to the
        module object.

        It works exactly like
        <EXTLINK borland://TCollectionItem.FindItemID>TCollectionItem.FindItemID</EXTLINK>
        except that it returns an object of type TlvkActiveScriptModule.
    }
    function FindItemID(ID: Integer): TlvkActiveScriptModule;

    { Description:
        This inserts a new module in the collection, at the given index
        position, and returns a reference to the newly inserted module object.

        It works exactly like
        <EXTLINK borland://TCollectionItem.Add>TCollectionItem.Add</EXTLINK>
        except that it returns an object of type TlvkActiveScriptModule.
      Parameters:
        Index - Where in the collection to add the new module.
      See also:
        Add
    }
    function Insert(Index: Integer): TlvkActiveScriptModule;

    { Description:
        This array-property gives access to all the modules stored in the
        collection, by their index number, ranging from 0 up to Count-1.

        It works exactly like
        <EXTLINK borland://TCollectionItem.Items>TCollectionItem.Items</EXTLINK>
        except that it returns an object of type TlvkActiveScriptModule.

        This property is the default property of the collection object so you
        can use the collection object as an ordinary array to quickly and
        easily access the modules stored in it, like this:

          ShowMessage(ScriptObject.Modules[0].Name);
      Parameters:
        Index - The index of the module object to retrieve or set.
      See also:
        ItemsByName
    }
    property Items[const Index: Integer]: TlvkActiveScriptModule read GetItem write SetItem; default;

    { Description:
        This array-property gives access to all the modules stored in the
        collection, by their name.

        If you specify a name that does not exist, then ItemsByName will
        return a NIL pointer.
      Parameters:
        Name - The name of the module object to retrieve.
      See also:
        Items
    }
    property ItemsByName[const Name: string]: TlvkActiveScriptModule read GetItemByName;
  end;

  { Description:
      This is the main interface for the TlvkActiveScript class. All methods
      that TlvkActiveScript makes available that is intended for end-user
      programmers are exposed through this interface.

      This interface is now the preferred way of using the class, since this
      facilitates mixing of languages (see the accompanying demo).
    See also:
      TlvkActiveScript
  }
  IlvkActiveScript = interface
    ['{84716336-83D3-45EE-98EE-CA0C54EBD4D6}']

    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion;

    // <COMBINE Language>
    function GetLanguage: string;
    // <COMBINE Language>
    procedure SetLanguage(const Value: string);
    { Description:
        Property that holds the current language of the code being used in
        the object. Can be changed to a new language to re-use the same object
        for scripts of different languages. Will automatically invalidate the
        script object if you change the name of the language. For an example,
        take a look at TlvkActiveScript.

        Note: All modules in a single TlvkActiveScript class must use the
          same language. If you want to mix languages, you must use several
          TlvkActiveScript instances, load the code for one language into one
          and for the other language into the other, and then add the two
          IDispatch interfaces to the other instance's object-list. One of the
          demos show how to do this.
      See also:
        GetLanguage, SetLanguage, Invalidate
    }
    property Language: string read GetLanguage write SetLanguage;

    // <COMBINE ScriptCode>
    function GetScriptCode: TStrings;
    // <COMBINE ScriptCode>
    procedure SetScriptCode(const Value: TStrings);
    { Description:
        Gives the programmer direct access to the script code to load and
        execute in the engine. If you change the script code, the engine will
        automatically be invalidated to force a recompile. Note that you cannot
        set both ScriptCode and ScriptFilename at the same time, only the last
        you set will be used. For an example, take a look at TlvkActiveScript.
      See also:
        ScriptFilename, SetScriptCode, SetScriptFilename
    }
    property ScriptCode: TStrings read GetScriptCode write SetScriptCode;

    // <COMBINE ScriptFilename>
    function GetScriptFilename: TScriptFilename;
    // <COMBINE ScriptFilename>
    procedure SetScriptFilename(const Value: TScriptFilename);

    { Description:
        Allows the programmer to set a filename to load script code from. If
        you change this property the engine will automatically be invalidated
        to force a recompile. Note that you cannot set both ScriptCode and
        ScriptFilename at the same time, only the last you set will be used.
      See also:
        ScriptCode, GetScriptFilename, SetScriptFilename, SetScriptCode
      Example:
<CODE>
<B>procedure</B> Form1.Button1Click(Sender: TObject);
<B>var</B>
  Script: TlvkActiveScript;
<B>begin</B>
  Script := TlvkActiveScript.Create(Application.Handle);
  <B>try</B>
    Script.Language := 'Python'; <I>// Requires ActivePython to be installed</I>
    Script.ScriptFilename := 'C:\Python20\win32\Demos\timer_demo.py';
    Script.Compile;
    Form1.Caption := Script.Intf.test(1, 2); <I>// Caption should be set to 13</I>
  <B>finally</B>
    Script.Free;
  <B>end</B>;
<B>end</B>;
</CODE>
    }
    property ScriptFilename: TScriptFilename read GetScriptFilename write SetScriptFilename;

    { Description:
        Compiles the script code and registers all objects with the Microsoft
        ActiveScript Engine. Will be used automatically if you use the Intf
        property. For an example, take a look at TlvkActiveScript.
      See also:
        Invalidate, ScriptCode, ScriptFilename
    }
    procedure Compile;

    { Description:
        This method will combine Invalidate and Compile into one method.
      See also:
        Invalidate, Compile
    }
    procedure Recompile;

    { Description:
        Clears the script object by removing all compiled code, unregistering
        objects with the script engine etc. Internally used to force a
        recompile when certain properties and settings are changed.
      See also:
        Compile, Recompile
    }
    procedure Invalidate;

    { Description:
        Converts the last script error into an exception. If no error has
        occured, this function will return False. If an error has occured,
        the function will not return but raise an exception instead.
      Parameters:
        If you have an exception object, you can pass the exception object
        to the RaiseLastScriptError method and it may use information found in
        that exception object to produce a better script exception. Pass
        nil (or ommit the parameter altogether) if you don't have an exception
        object.
      See also:
        Compile, Intf
    }
    function RaiseLastScriptError(const ConvertException: Exception=nil): Boolean;

    // <COMBINE Intf>
    function GetDispatch: OleVariant;
    { Description:
        This property returns a IDispatch interface that gives the programmer
        direct access to any functions, procedures and variables contained in
        the script code. Note that using this property will automatically
        compile the script code if necessary. For an example, take a look at
        TlvkActiveScript.
      See also:
        GetDispatch, Compile
    }
    property Intf: OleVariant read GetDispatch;

    { Description:
        Use the Eval function to quickly evaluate expressions in the scripting
        engine.

        Expressions follow the same syntax as any expression that returns a
        value, including calls to functions defined in the script.

        If we take a simple example in VBScript, Expression can be anything
        that can take the place of <expr> in the following VBScript statement:

          Result = <expr>

        The Result will be returned from the function.
      Parameters:
        Expression - The code expression to evaluate.
        ModuleName - The name of the module to use as scope for evaluating the
          expression.
      See also:
        Call
    }
    function Eval(const ModuleName, Expression: string): OleVariant; overload;
    // <COMBINE Eval@string@string>
    function Eval(const Expression: string): OleVariant; overload;

    { Description:
        The Call methods will call a function or procedure in the script code
        and return the result from the function, or Unassigned in the event of
        a procedure call.

        The reason for the various overloaded methods are the syntax
        specifications:

        Value := Scr.Call('FuncName');<P>
        Value := Scr.Call('FuncName', ['Func', 'With', 'Parameters']);<P>
        Value := Scr.Call('Module', 'FuncName');<P>
        Value := Scr.Call('Module', 'FuncName', ['Func', 'With', 'Parameters']);<P>
      Parameters:
        ModuleName - The name of the module that the function is defined in, or
          blank string if in the default module or namespace.
        FunctionName - The name of the function or procedure to call.
        Args - A list of OleVariant parameters to use as parameters to the
          function or procedure.
    }
    function Call(const ModuleName, FunctionName: string; const Args: array of OleVariant): OleVariant; overload;
    // <COMBINE Call@string@string@array of OleVariant>
    function Call(const FunctionName: string): OleVariant; overload;
    // <COMBINE Call@string@string@array of OleVariant>
    function Call(const FunctionName: string; const Args: array of OleVariant): OleVariant; overload;
    // <COMBINE Call@string@string@array of OleVariant>
    function Call(const ModuleName, FunctionName: string): OleVariant; overload;

    { Description:
        Registers a new object with the script object. You can register any
        object that supports an IDispatch interface, including IDispatch
        interfaces you might get from other TlvkActiveScript objects (through
        their Intf property). Adding a new object will automatically invalidate
        the script engine.
      Parameters:
        Name     - Name to use when registering the object. This name will be
          available for the script code to use.
        Instance - IDispatch interface reference.
        Global   - Should the members (methods and properties) of this object
          be globally available (that is, without having to prefix them with
          the name of the object) or not.
      Remarks:
        The name is not case sensitive.
      See also:
        RemoveObject, RemoveObjects, GetObject
      Example:
<CODE>
<B>procedure</B> Form1.Button1Click(Sender: TObject);
<B>var</B>
  Script: TlvkActiveScript;
<B>begin</B>
  Script := TlvkActiveScript.Create(Application.Handle);
  <B>try</B>
    Script.Language := 'Python'; <I>// Requires ActivePython to be installed</I>
    Script.AddObject('Form', TlvkRTTIDispatch.Create(Form1, False), False);
    <B>with</B> Script.ScriptCode <B>do</B>
    <B>begin</B>
      Clear;
      Add('def test():');
      Add('  Form.Left = Form.Left - 10');
      Add('  Form.Width = Form.Width + 20');
    <B>end</B>;
    Script.Compile;
    Script.Intf.test; <I>// Will expand the width of the form by 20</I>
  <B>finally</B>
    Script.Free;
  <B>end</B>;
<B>end</B>;
</CODE>
    }
    procedure AddObject(const Name: string; const Instance: IDispatch; const Global: Boolean);

    { Description:
        This method will remove all objects registered with the activescript
        object.
      See also:
        AddObject, RemoveObject
    }
    procedure RemoveObjects;

    { Description:
        This method will register a type library with the scripting engine.
        Doing this will make all enumerations and constants in the library
        available for use in the scripting code.

        Note: Adding a type library to the scripting engine will force a
          recompile.
      Parameters:
        CLSID - The CLSID of the type library to add.
        MajorVersion - The major version number of the type library.
        MinorVersion - The minor version number of the type library.
        IsControl - Does the CLSID refer to a control or a type library?
      See also:
        AddTypeLibrary@string, RemoveTypeLibrary@TCLSID,
        RemoveTypeLibrary@string
      Example:
<CODE>
  // This will add the ADODB type library to the scripting engine
  ScriptObject.AddTypeLibrary(LIBID_ADODB, 2, 1, False);
</CODE>
    }
    procedure AddTypeLibrary(const CLSID: TCLSID; const MajorVersion, MinorVersion: LongWord; const IsControl: Boolean=False); overload;

    { Description:
        This method will add a type library to the script engine object.
        Doing this will make all enumerated types and constants in the
        type library available for use in the script code.

        It will look through registry and check the activex/com object
        specified to find out which type library to load.

        Note: Adding a type library will force a recompile of the code.
      Parameters:
        ProgID - Name of activex/com object to load type library for.
      See also:
        AddTypeLibrary@TCLSID@LongWord@LongWord@Boolean,
        RemoveTypeLibrary@TCLSID, RemoveTypeLibrary@string
    }
    procedure AddTypeLibrary(const ProgID: string); overload;

    { Description:
        This method will remove a type library from the script engine object.

        Note: Remvong a type library will force a recompile of the code.
      Parameters:
        CLSID - ClassID of the type library to remove.
      See also:
        AddTypeLibrary@TCLSID@LongWord@LongWord@Boolean,
        AddTypeLibrary@string,
        RemoveTypeLibrary@string
    }
    procedure RemoveTypeLibrary(const CLSID: TCLSID); overload;

    { Description:
        This method will remove a type library from the script engine object.

        Note: Remvong a type library will force a recompile of the code.
      Parameters:
        ProgID - Name of activex/com object to remove type library for.
      See also:
        AddTypeLibrary@TCLSID@LongWord@LongWord@Boolean,
        AddTypeLibrary@string,
        RemoveTypeLibrary@TCLSID
    }
    procedure RemoveTypeLibrary(const ProgID: string); overload;

    { Description:
        This method will remove all registered type libraries with the
        active script object.
      See also:
        AddTypeLibrary@TCLSID@LongWord@LongWord@Boolean,
        AddTypeLibrary@string,
        RemoveTypeLibrary@string,
        RemoveTypeLibrary@TCLSID
    }
    procedure RemoveTypeLibraries;

    { Description:
        This method populates the List parameter with a list of identifiers
        available in the script. This list will contain:

          * Global variables
          * Functions
          * Procedures
          * Objects added to the global scope
          * Modules (if you list identifiers in the default module)
      Parameters:
        List - TStrings instance which will get the list of identifiers (must
          be preallocated by the callee)
        ModuleName - Name of module to list identifiers in (leave blank or
          use the overloaded method without ModuleName for the default
          module)
    }
    procedure GetIdentifierList(const ModuleName: string; const List: TStrings;
      const IncludeIdentifiers: TGetIdentifierListIncludes=DEFAULT_LIST_INCLUDES); overload;
    // <COMBINE GetIdentifierList@string@TStrings@TGetIdentifierListIncludes>
    procedure GetIdentifierList(const List: TStrings;
      const IncludeIdentifiers: TGetIdentifierListIncludes=DEFAULT_LIST_INCLUDES); overload;

    { Description:
        Removes a object from the list of objects the script object holds.
        You would use this to make sure the object is no longer available to
        the script code. Removing an object automatically invalidates the
        script engine.
      Parameters:
        Name - Name of object to remove.
      Remarks:
        The name is not case sensitive.
      See also:
        RemoveObjects, AddObject, GetObject
      Example:
<CODE>
<B>procedure</B> Form1.Button1Click(Sender: TObject);
<B>var</B>
  Script: TlvkActiveScript;
<B>begin</B>
  Script := TlvkActiveScript.Create(Application.Handle);
  <B>try</B>
    ...
    Script.AddObject('Form', TlvkRTTIDispatch.Create(Form1, False), False);
    ...
    Script.RemoveObject('Form');
    ...
</CODE>
    }
    procedure RemoveObject(const Name: string);

    { Description:
        Retrieves the IDispatch interface for the object with the given name.
      Parameters:
        Name - Name of object to retrieve.
      Remarks:
        The name is not case sensitive.
      See also:
        AddObject, RemoveObject
    }
    function GetObject(const Name: string): IDispatch;

    // <COMBINE OnBeforeCompile>
    function GetOnBeforeCompile: TNotifyEvent;
    // <COMBINE OnBeforeCompile>
    procedure SetOnBeforeCompile(const Value: TNotifyEvent);
    { Description:
        This event will be called just before the script code is to be compiled.
      See also:
        OnAfterCompile, OnBeforeInvalidate, OnAfterInvalidate
    }
    property OnBeforeCompile: TNotifyEvent read GetOnBeforeCompile write SetOnBeforeCompile;

    // <COMBINE OnAfterCompile>
    function GetOnAfterCompile: TNotifyEvent;
    // <COMBINE OnAfterCompile>
    procedure SetOnAfterCompile(const Value: TNotifyEvent);
    { Description:
        This event will be called just after the script code has been compiled.
      See also:
        OnBeforeCompile, OnBeforeInvalidate, OnAfterInvalidate
    }
    property OnAfterCompile: TNotifyEvent read GetOnAfterCompile write SetOnAfterCompile;

    // <COMBINE OnBeforeInvalidate>
    function GetOnBeforeInvalidate: TNotifyEvent;
    // <COMBINE OnBeforeInvalidate>
    procedure SetOnBeforeInvalidate(const Value: TNotifyEvent);
    { Description:
        This event will be called just before the script code is to be
        invalidated.
      See also:
        OnBeforeCompile, OnAfterCompile, OnAfterInvalidate
    }
    property OnBeforeInvalidate: TNotifyEvent read GetOnBeforeInvalidate write SetOnBeforeInvalidate;

    // <COMBINE OnAfterInvalidate>
    function GetOnAfterInvalidate: TNotifyEvent;
    // <COMBINE OnAfterInvalidate>
    procedure SetOnAfterInvalidate(const Value: TNotifyEvent);
    { Description:
        This event will be called just before the script code has been
        invalidated.
      See also:
        OnBeforeCompile, OnAfterCompile, OnBeforeInvalidate
    }
    property OnAfterInvalidate: TNotifyEvent read GetOnAfterInvalidate write SetOnAfterInvalidate;

    // <COMBINE Modules>
    function GetModules: TlvkActiveScriptModuleCollection;
    // <COMBINE Modules>
    procedure SetModules(const Value: TlvkActiveScriptModuleCollection);
    { Description:
        The Modules collection property gives the programmer access to the
        modules contained in the activescript object. The programmer can add
        more modules if needed, for instance to store or refer to library code
        available at all times.
    }
    property Modules: TlvkActiveScriptModuleCollection read GetModules write SetModules;

    { Description:
        This method simply checks if a module exists. Basically it looks
        inside the scripting engine and sees if there's a dispatchable object
        with the given name. This will handle modules you might have added
        from within the scripts.
      Parameters:
        ModuleName - The name of the module to check for.
      See also:
        IdentifierExists
    }
    function ModuleExists(const ModuleName: string): Boolean;

    { Description:
        This method will check if there's an identifier in the scripting
        engine with the given name.
      Parameters:
        ModuleName - The name of the module to look in.
        IdentifierName - The name of the identifier to look for.
      See also:
        ModuleExists
      Example:
<PRE>
  <B>if</B> Script.IdentifierExists('CalculateAverage') <B>then</B>
    Average := Script.Intf.CalculateAverage(ListOfValues)
  <B>else</B>
    <B>raise</B> Exception.Create('Unable to calculate average rate');
</PRE>
    }
    function IdentifierExists(const ModuleName, IdentifierName: string): Boolean; overload;
    // <COMBINE IdentifierExists@string@string>
    function IdentifierExists(const IdentifierName: string): Boolean; overload;

    // <COMBINE ActiveScript>
    function GetActiveScript: IActiveScript;
    { Description:
        The ActiveScript property gives direct access to the IActiveScript
        interface that the internal code uses to execute all script-related
        code through.

        Note that the IActiveScript interface will only be available after
        you've compiled the code. Any call to the rest of the methods and
        properties in this object will automatically compile the code if needed,
        but reading the ActiveScript property will not, and will thus return
        NIL if the code is not compiled.

        Be warned that you should not, unless you know precisely what you're
        doing and how this will affect the TlvkActiveScript object, change
        or execute any of the methods or properties on this interface.
    }
    property ActiveScript: IActiveScript read GetActiveScript;

    { Description:
        This method will populate the List parameter with a list of the
        installed scripting languages. Some of the languages support encoding,
        and you can select wether or not you want these in the list as well.
      Parameters:
        List - List object that will get a list of the language names.
        IncludeEncodedVersions - Wether or not you want the encoded versions.
    }
    procedure GetInstalledLanguages(const List: TStrings;
      const IncludeEncodedVersions: Boolean=False);

    // <COMBINE PreProcess>
    function GetPreProcess: Boolean;
    // <COMBINE PreProcess>
    procedure SetPreProcess(const Value: Boolean);
    { Description:
        Set this property to True to run the script code through the
        preprocessor before loading it into the script engine.
    }
    property PreProcess: Boolean read GetPreProcess write SetPreProcess;
  end;

{ Description:
    This class contains the necessary code to load, compile, and execute script
    code through the Microsoft ActiveScript Engine.
  Example:
<CODE>
<B>procedure</B> Form1.Button1Click(Sender: TObject);
<B>var</B>
  Script: TlvkActiveScript;
<B>begin</B>
  Script := TlvkActiveScript.Create(Application.Handle);
  <B>try</B>
    Script.Language := 'Python'; <I>// Requires ActivePython to be installed</I>
    <B>with</B> Script.ScriptCode <B>do</B>
    <B>begin</B>
      Clear;
      Add('def test(a, b):');
      Add('  return 10+a+b');
    <B>end</B>;
    Script.Compile;
    Form1.Caption := Script.Intf.test(1, 2); <I>// Caption should be set to 13</I>
  <B>finally</B>
    Script.Free;
  <B>end</B>;
<B>end</B>;
</CODE>
}
  TlvkActiveScript = class(TPersistent, IUnknown, IDispatch, IlvkActiveScript)
  private
    FRefCount           : Integer;
    FActiveScriptSite   : TlvkActiveScriptSite;
    FActiveScript       : IActiveScript;
    FLanguage           : string;
    FDefaultModule      : TlvkActiveScriptModule;
    FScriptManager      : IDispatch;
    FLastModuleName     : string;
    FObjects            : TList;
    FWScript            : IDispatch;
    FSQLGlobal          : IDispatch;

    FHandle             : THandle;
    FTypeLibraries      : array of TTypeLibraryRec;
    FModules            : TlvkActiveScriptModuleCollection;

    FOnBeforeCompile    : TNotifyEvent;
    FOnAfterCompile     : TNotifyEvent;
    FOnBeforeInvalidate : TNotifyEvent;
    FOnAfterInvalidate  : TNotifyEvent;

    function GetLanguage: string; virtual;
    procedure SetLanguage(const Value: string); virtual;
    function GetScriptCode: TStrings; virtual;
    procedure SetScriptCode(const Value: TStrings); virtual;
    function GetScriptFilename: TScriptFilename; virtual;
    procedure SetScriptFilename(const Value: TScriptFilename); virtual;

    procedure DoBeforeCompile;
    procedure DoAfterCompile;
    procedure DoBeforeInvalidate;
    procedure DoAfterInvalidate;
    procedure SetModules(const Value: TlvkActiveScriptModuleCollection);
    function GetModules: TlvkActiveScriptModuleCollection;
    function GetOnAfterCompile: TNotifyEvent;
    function GetOnAfterInvalidate: TNotifyEvent;
    function GetOnBeforeCompile: TNotifyEvent;
    function GetOnBeforeInvalidate: TNotifyEvent;
    procedure SetOnAfterCompile(const Value: TNotifyEvent);
    procedure SetOnAfterInvalidate(const Value: TNotifyEvent);
    procedure SetOnBeforeCompile(const Value: TNotifyEvent);
    procedure SetOnBeforeInvalidate(const Value: TNotifyEvent);
    function GetActiveScript: IActiveScript;

    // IUnknown interface
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;

    // IDispatch interface
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;

  protected
    { Description:
        Removes all registered objects from the script engine. Since registered
        objects are held by a IDispatch interface reference, the objects will
        be destroyed if no other reference to them exists. Internally used
        before a TlvkActiveScript object is destroyed.
      See also:
        Compile, Invalidate
    }
    procedure ClearObjects; virtual;

    { Description:
        Loads the script code into the script engine. Calls GetScriptCode to
        get hold of the code to load.
      See also:
        Compile
    }
    procedure LoadCode; virtual;

    { Description:
        Registers all objects that the script object holds a reference to with
        the Microsoft ActiveScript Engine.
      See also:
        ClearObjects, AddObject, RemoveObject
    }
    procedure AddObjects; virtual;

    // <ALIAS IlvkActiveScript.GetDispatch>
    function GetDispatch: OleVariant; virtual;

    { Description:
        This internal method is used to load a type library and return its
        attributes.
    }
    function GetTLBAttr(const ProgID: string; out Attr: TLIBATTR): Boolean;

  public
    // IUnknown interface
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;

    procedure Assign(Source: TPersistent); override;

    { Description:
        Creates a new instance of the TlvkActiveScript class and prepares it
        for use. For an example, take a look at TlvkActiveScript.
      Parameters:
        WindowHandle - Handle of parent window. Is used if the script code
          displays message boxes or similar. Can be set to 0 to stop script
          from displaying such message boxes.      Return value:
      See also:
        Destroy
    }
    constructor Create(const WindowHandle: THandle=0);

    { Description:
        Destroys the instance of the TlvkActiveScript class. Will unregister
        all objects (possibly destroying them if no other reference to them
        exists) and clean up after use. For an example, take a look at
        TlvkActiveScript.
      See also:
        Create
    }
    destructor Destroy; override;

    // <ALIAS IlvkActiveScript.Compile>
    procedure Compile; virtual;
    // <ALIAS IlvkActiveScript.Recompile>
    procedure Recompile; virtual;
    // <ALIAS IlvkActiveScript.Invalidate>
    procedure Invalidate; virtual;
    // <ALIAS IlvkActiveScript.RaiseLastScriptError@Exception>
    function RaiseLastScriptError(const ConvertException: Exception=nil): Boolean; virtual;
    // <ALIAS IlvkActiveScript.Language>
    property Language: string read GetLanguage write SetLanguage;
    // <ALIAS IlvkActiveScript.ScriptCode>
    property ScriptCode: TStrings read GetScriptCode write SetScriptCode;
    // <ALIAS IlvkActiveScript.ScriptFilename>
    property ScriptFilename: TScriptFilename read GetScriptFilename write SetScriptFilename;
    // <ALIAS IlvkActiveScript.Intf>
    property Intf: OleVariant read GetDispatch;
    // <ALIAS IlvkActiveScript.Eval@string>
    function Eval(const Expression: string): OleVariant; overload;
    // <ALIAS IlvkActiveScript.Eval@string@string>
    function Eval(const ModuleName, Expression: string): OleVariant; overload;
    // <ALIAS IlvkActiveScript.Call@string@string@array of OleVariant>
    function Call(const FunctionName: string; const Args: array of OleVariant): OleVariant; overload;
    // <ALIAS IlvkActiveScript.Call@string@string@array of OleVariant>
    function Call(const FunctionName: string): OleVariant; overload;
    // <ALIAS IlvkActiveScript.Call@string@string@array of OleVariant>
    function Call(const ModuleName, FunctionName: string; const Args: array of OleVariant): OleVariant; overload;
    // <ALIAS IlvkActiveScript.Call@string@string@array of OleVariant>
    function Call(const ModuleName, FunctionName: string): OleVariant; overload;
    // <ALIAS IlvkActiveScript.AddObject@string@IDispatch@Boolean>
    procedure AddObject(const Name: string; const Instance: IDispatch; const Global: Boolean); virtual;
    // <ALIAS IlvkActiveScript.RemoveObjects>
    procedure RemoveObjects; virtual;
    // <ALIAS IlvkActiveScript.AddTypeLibrary@TCLSID@LongWord@LongWord@Boolean>
    procedure AddTypeLibrary(const CLSID: TCLSID; const MajorVersion, MinorVersion: LongWord; const IsControl: Boolean=False); overload;
    // <ALIAS IlvkActiveScript.AddTypeLibrary@string>
    procedure AddTypeLibrary(const ProgID: string); overload;
    // <ALIAS IlvkActiveScript.RemoveTypeLibrary@TCLSID>
    procedure RemoveTypeLibrary(const CLSID: TCLSID); overload;
    // <ALIAS IlvkActiveScript.RemoveTypeLibrary@string>
    procedure RemoveTypeLibrary(const ProgID: string); overload;
    // <ALIAS IlvkActiveScript.RemoveTypeLibraries>
    procedure RemoveTypeLibraries;
    // <ALIAS IlvkActiveScript.RemoveObject@string>
    procedure RemoveObject(const Name: string); virtual;
    // <ALIAS IlvkActiveScript.GetObject@string>
    function GetObject(const Name: string): IDispatch; virtual;
    // <ALIAS IlvkActiveScript.ModuleExists@string>
    function ModuleExists(const ModuleName: string): Boolean;
    // <ALIAS IlvkActiveScript.IdentifierExists@string>
    function IdentifierExists(const IdentifierName: string): Boolean; overload;
    // <ALIAS IlvkActiveScript.IdentifierExists@string@string>
    function IdentifierExists(const ModuleName, IdentifierName: string): Boolean; overload;
    // <ALIAS IlvkActiveScript.GetIdentifierList@TStrings@TGetIdentifierListIncludes>
    procedure GetIdentifierList(const List: TStrings;
      const IncludeIdentifiers: TGetIdentifierListIncludes=DEFAULT_LIST_INCLUDES); overload;
    // <ALIAS IlvkActiveScript.GetIdentifierList@TStrings@TGetIdentifierListIncludes>
    procedure GetIdentifierList(const ModuleName: string; const List: TStrings;
      const IncludeIdentifiers: TGetIdentifierListIncludes=DEFAULT_LIST_INCLUDES); overload;
    // <ALIAS IlvkActiveScript.GetPreProcess>
    function GetPreProcess: Boolean;
    // <ALIAS IlvkActiveScript.SetPreProcess@Boolean>
    procedure SetPreProcess(const Value: Boolean);

    { Description:
        This event will be called just before the script code is to be compiled.
      See also:
        OnAfterCompile, OnBeforeInvalidate, OnAfterInvalidate
    }
    property OnBeforeCompile: TNotifyEvent read GetOnBeforeCompile write SetOnBeforeCompile;

    { Description:
        This event will be called just after the script code has been compiled.
      See also:
        OnBeforeCompile, OnBeforeInvalidate, OnAfterInvalidate
    }
    property OnAfterCompile: TNotifyEvent read GetOnAfterCompile write SetOnAfterCompile;

    { Description:
        This event will be called just before the script code is to be
        invalidated.
      See also:
        OnBeforeCompile, OnAfterCompile, OnAfterInvalidate
    }
    property OnBeforeInvalidate: TNotifyEvent read GetOnBeforeInvalidate write SetOnBeforeInvalidate;

    { Description:
        This event will be called just before the script code has been
        invalidated.
      See also:
        OnBeforeCompile, OnAfterCompile, OnBeforeInvalidate
    }
    property OnAfterInvalidate: TNotifyEvent read GetOnAfterInvalidate write SetOnAfterInvalidate;

    { Description:
        The Modules collection property gives the programmer access to the
        modules contained in the activescript object. The programmer can add
        more modules if needed, for instance to store or refer to library code
        available at all times.
    }
    property Modules: TlvkActiveScriptModuleCollection read GetModules write SetModules;

    { Description:
        The ActiveScript property gives direct access to the IActiveScript
        interface that the internal code uses to execute all script-related
        code through.

        Note that the IActiveScript interface will only be available after
        you've compiled the code. Any call to the rest of the methods and
        properties in this object will automatically compile the code if needed,
        but reading the ActiveScript property will not, and will thus return
        NIL if the code is not compiled.

        Be warned that you should not, unless you know precisely what you're
        doing and how this will affect the TlvkActiveScript object, change
        or execute any of the methods or properties on this interface.
    }
    property ActiveScript: IActiveScript read GetActiveScript;

    { Description:
        This property is used internally.
    }
    property LastModuleName: string read FLastModuleName write FLastModuleName;

    // <ALIAS IlvkActiveScript.GetInstalledLanguages@TStrings@Boolean>
    procedure GetInstalledLanguages(const List: TStrings;
      const IncludeEncodedVersions: Boolean=False);

    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
  end;

const
  iLineNo         = 1;
  iCharPos        = 2;
  iSource         = 3;
  iDescription    = 4;
  iHelpFile       = 5;
  iHelpContext    = 6;
  iErrorCode      = 7;
  iHResult        = 8;

type
  { Description:
      This class is used for all exceptions from lvkActiveScript code not
      specifically linked to a position in the source.
    See also:
      ElvkActiveScriptCode
  }
  ElvkActiveScript = class(Exception);

  { Description:
      Base class of all exceptions raised from the TlvkActiveScript class and
      related classes.
    See also:
      ElvkActiveScriptCompile, ElvkActiveScriptRuntime
  }
  ElvkActiveScriptCode = class(Exception)
  private
    FError          : TlvkActiveScriptErrorRec;
    FLastModuleName : string;

  protected
    { Description:
        Internal method used to retrieve property values for all string
        properties of this class.
      Parameters:
        Index - Index of property to retrieve (See remarks).
      Return value:
        String value of property.
      Remarks:
        The Index can be one of the following values:
          * iSource         = 3
          * iDescription    = 4
          * iHelpFile       = 5
      See also:
        GetInteger, Source, Description, HelpFile
    }
    function GetString(const Index: Integer): string;

    { Description:
        Internal method used to retrieve property values for all integer
        properties of this class.
      Parameters:
        Index - Index of property to retrieve (See remarks).
      Return value:
        Integer value of property.
      Remarks:
        The Index can be one of the following values:
          * iLineNo         = 1
          * iCharPos        = 2
          * iHelpContext    = 6
          * iErrorCode      = 7
          * iHResult        = 8
      See also:
        GetString, LineNo, CharPos, HelpContext, ErrorCode, HResult
    }
    function GetInteger(const Index: Integer): Integer;

  public
    { Description:
        Creates a new instance of the ElvkActiveScriptCompile class and stores
        the error data for future reference. This exception class is used
        internally when compiling scripts.
      Parameters:
        Error - Record that holds the information about the error.
        LastModuleName - Which module that was last accessed.
      See also:
        IlvkActiveScript.Compile
    }
    constructor Create(const Error: TlvkActiveScriptErrorRec;
      const LastModuleName: string='');

    { Description:
        This property returns the line number in the script code that the error
        occured on (1=first line, etc.).
      See also:
        CharPos
    }
    property LineNo: Integer index iLineNo read GetInteger;

    { Description:
        This property returns the character position in the script code that
        the error occured on, relative to the start of the line
        (1=first character, etc.).
      See also:
        LineNo
    }
    property CharPos: Integer index iCharPos read GetInteger;

    { Description:
        This property returns the source of the error, compilation, runtime,
        etc.
      See also:
        Description
    }
    property Source: string index iSource read GetString;

    { Description:
        This property returns a textual description (most likely a predefined
        error message) of the error that occured.
      See also:
        Source, CharPos, LineNo
    }
    property Description: string index iDescription read GetString;

    { Description:
        If the error is described in a helpfile, this property will return
        the name of that helpfile, otherwise this property will return a
        blank string.
      See also:
        HelpContext
    }
    property HelpFile: string index iHelpFile read GetString;

    { Description:
        If the error is described in a helpfile, this property will return the
        context id of the page describing it, otherwise this property will
        return a blank string.
      See also:
        HelpFile
    }
    property HelpContext: Integer index iHelpContext read GetInteger;

    { Description:
        This property will return an error code for the error. Most likely this
        will be an internal errorcode and might not be of much use.
      See also:
        HResult
    }
    property ErrorCode: Integer index iErrorCode read GetInteger;

    { Description:
        If the error is also a normal COM type of error, then this property
        will return the HResult code that corresponds to that error, otherwise
        this property will return 0 or -1.
      See also:
        ErrorCode
    }
    property HResult: Integer index iHResult read GetInteger;

    { Description:
        This property gives you the last module name that was accessed through
        the TlvkActiveScript object.

        Note: This is not always accurate. If you use Call or Eval, it will
          be set to the module name you used. If you call module functions
          from inside your script it may or may not be set to the correct
          module depending on wether the script engine asks TlvkActiveScript
          for information about that module.
    }
    property LastModuleName: string read FLastModuleName;
  end;

type
  { Description:
      This exception class is raised when there is a error during compilation
      of the script code in TlvkActiveScript.Compile.
    See also:
      ElvkActiveScriptCode, ElvkActiveScriptRuntime, TlvkActiveScript
  }
  ElvkActiveScriptCompile = class(ElvkActiveScriptCode);

  { Description:
      This exception class is raised when there is an error during runtime
      execution of the script code.
    See also:
      ElvkActiveScriptCode, ElvkActiveScriptCompile
  }
  ElvkActiveScriptRuntime = class(ElvkActiveScriptCode);

{ Description:
    This function can be used to convert from a CharPos/LineNo coordinate to
    a text position in the scriptcode. This is useful for setting the
    SelStart property of a TMemo containing code.
  Parameters:
    LineNo      - Line number.
    CharPos     - Position on line.
    ScriptCode  - TStrings containing the script code.
  Return value:
    The offset of the text character that the CharPos/LineNo coordinate refers
    to.
  See also:
    ElvkActiveScriptCompile
  Example:
<CODE>
<B>procedure</B> Form1.Button1Click(Sender: TObject);
<B>var</B>
  Script: TlvkActiveScript;
<B>begin</B>
  Script := TlvkActiveScript.Create(Application.Handle);
  <B>try</B>
    Script.Language := 'Python'; <I>// Requires ActivePython to be installed</I>
    Script.ScriptCode.Assign(eCode.Text);
    <B>try</B>
      Script.Compile;
    <B>except</B>
      <B>on</B> E: ElvkActiveScriptCompile <B>do</B>
      <B>begin</B>
        eCode.SelStart := ConvertToTextPos(E.LineNo, E.CharPos, eCode.Lines);
        eCode.SetFocus;
        <B>raise</B>;
      <B>end</B>;
    <B>end</B>;
    Form1.Caption := Script.Intf.test(1, 2); <I>// Caption should be set to 13</I>
  <B>finally</B>
    Script.Free;
  <B>end</B>;
<B>end</B>;
</CODE>
}
function ConvertToTextPos(const LineNo, CharPos: Integer; const ScriptCode: TStrings): Integer;

{ Description:
    This method will take the script filename specified and replace
    environment strings in it with their values.

    If the environment string \%APPLICATION\% occurs in the filename, it will
    be replaced with the full path to the application executable, including
    a trailing backslash.

    The function returns the new filename.
  Parameters:
    ScriptFilename - The filename to expand.
}
function ExpandScriptFilename(const ScriptFilename: string): string;

{ Description:
    LoadTypeLibrary will load the type library associated with the given
    COM object specified by its ProgID.

    The function will return NIL if no object with the given ProgID exists,
    or an error occurs while loading the type library.
}
function LoadTypeLibrary(const ProgID: string): ITypeLib;

resourcestring
  { Description:
      This text is used when raising an exception when you try to compile a
      script without setting the language first.
    See also:
      TlvkActiveScript.Language, TlvkActiveScript.Compile
  }
  SNoLanguage       = 'No language set';

  { Description:
      This text is used when raising an exception during compilation of a
      script.
    See also:
      TlvkActiveScript.Compile
  }
  SCompileException = '%s: %s occured at position %d in line %d';

  { Description:
      This text is used when raising an exception either in
      ElvkActiveScriptCompile.GetString or ElvkActiveScriptCompile.GetInteger,
      when the wrong index is passed to it.
    See also:
      ElvkActiveScriptCompile.GetInteger, ElvkActiveScriptCompile.GetString
  }
  SInvalidIndex     = 'Invalid Index parameter passed to %s';

{ Description:
    This function works like the Visual Basic function GetObject.
  Parameters:
    Name - The name or url to the object to get.
}
function GetObject(const Name: string): IDispatch;

implementation

uses
  lvkRTTIDispatch, lvkCollection;

const
  SCRIPT_MANAGER_NAME         = 'lvkActiveScriptScriptManager';
  SCRIPT_WSCRIPT              = 'WSCRIPT';
  SCRIPT_SQLGLOBAL            = 'lvkSQLGlobal';
  SCRIPTTYPELIB_ISCONTROL     = $10;
  SCRIPTTYPELIB_ISPERSISTENT  = $40;

  IID_NULL  : TGUID = '{00000000-0000-0000-0000-000000000000}';

function GetObject(const Name: string): IDispatch;
var
  Moniker     : IMoniker;
  Eaten       : integer;
  BindContext : IBindCtx;
  Dispatch    : IDispatch;
begin
  OleCheck(CreateBindCtx(0, BindContext));
  OleCheck(MkParseDisplayName(BindContext, PWideChar(WideString(Name)),
    Eaten, Moniker));
  OleCheck(Moniker.BindToObject(BindContext, nil, IDispatch, Dispatch));
  Result := Dispatch;
end;

function ConvertToTextPos(const LineNo, CharPos: Integer; const ScriptCode: TStrings): Integer;
var
  y : Integer;
begin
  Result := 0;
  y := 1;

  while y < LineNo do
  begin
    Inc(Result, Length(ScriptCode[y-1])+2);
    Inc(y);
  end;
  Inc(Result, CharPos-1);
end;

function ExpandScriptFilename(const ScriptFilename: string): string;
var
  ExpandedFilename1 : string;
  ExpandedFilename2 : array[0..MAX_PATH] of Char;
begin
  ExpandedFilename1 := StringReplace(ScriptFilename, '%APPLICATION%',
    ExtractFilePath(ParamStr(0)), [rfReplaceAll, rfIgnoreCase]);
  Win32Check(ExpandEnvironmentStrings(PChar(ExpandedFilename1),
    ExpandedFilename2, SizeOf(ExpandedFilename2)) > 0);

  Result := ExpandedFilename2;
end;

function LoadTypeLibrary(const ProgID: string): ITypeLib;
var
  Reg     : TRegistry;
  CLSID   : string;
  Server  : string;
begin
  Result := nil;

  try
    Reg := TRegistry.Create;
    try
      CLSID := GUIDToString(ProgIDToClassID(ProgID));
      Reg.RootKey := HKEY_CLASSES_ROOT;

      if Reg.OpenKey('CLSID\' + CLSID + '\InProcServer32', False) then
      begin
        Server := Reg.ReadString('');
        Reg.CloseKey;
      end else if Reg.OpenKey('CLSID\' + CLSID + '\InProcHandler32', False) then
      begin
        Server := Reg.ReadString('');
        Reg.CloseKey;
      end else if Reg.OpenKey('CLSID\' + CLSID + '\LocalServer32', False) then
      begin
        Server := Reg.ReadString('');
        Reg.CloseKey;
      end else
        Exit;

      if LoadTypeLib(PWideChar(WideString(Server)), Result) <> S_OK then
        Result := nil;
    finally
      Reg.Free;
    end;
  except
    Result := nil;
  end;
end;

{ TlvkScriptManager }

type
  TlvkScriptManager = class(TlvkRTTIBaseDispatch)
  private
    FOwner  : TlvkActiveScript;

  public
    constructor Create(const Owner: TlvkActiveScript);

  published
    function IncludeFile(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT;
    function LoadTypeLibrary(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT;
  end;

  TlvkWScript = class(TlvkRTTIBaseDispatch)
  private
    FOwner  : TlvkActiveScript;

  public
    constructor Create(const Owner: TlvkActiveScript);

  published
    function CreateObject(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT;
    function GetObject(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT;
    function CreateCollection(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT;
    function Assert(var Parameters: array of Variant; out FunctionResult: Variant): HRESULT;
  end;

  TlvkEventSink = class(TInterfacedObject, IUnknown, IDispatch)
  private
    FOwner    : TlvkActiveScript;
    FIID      : TGUID;
    FTypeInfo : ITypeInfo;
    FPrefix   : string;

  protected
    // IUnknown interface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;

    // IDispatch interface
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;

  public
    constructor Create(const Owner: TlvkActiveScript; const IID: TGUID; const Prefix: string; const TypeInfo: ITypeInfo);
  end;

  TEventSinkRec = record
    ConnectionPoint : IConnectionPoint;
    Cookie          : Integer;
  end;

  TlvkEventSinkWrapper = class(TInterfacedObject, IDispatch)
  private
    FOwner      : TlvkActiveScript;
    FObject     : IDispatch;
    FEventSinks : array of TEventSinkRec;

  protected
    // IDispatch interface
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;

  public
    constructor Create(const Owner: TlvkActiveScript;
      const ProgID, Prefix: string; const ConnectToActive: Boolean);
    destructor Destroy; override;
  end;


{ TlvkActiveScript }

const
  CNoLanguage = '';

procedure TlvkActiveScript.AddObject(const Name: string;
  const Instance: IDispatch; const Global: Boolean);
var
  ObjectRec : PObjectRec;
begin
  if GetObject(Name)<>nil then
    RemoveObject(Name);

  New(ObjectRec);
  try
    ObjectRec.Name := Name;
    ObjectRec.Instance := Instance;
    ObjectRec.Global := Global;
    FObjects.Add(ObjectRec);
  except
    Dispose(ObjectRec);
    raise;
  end;
  Invalidate;
end;

procedure TlvkActiveScript.AddObjects;
var
  i         : Integer;
  ObjectRec : PObjectRec;
  Flags     : Cardinal;
const
  Options   : array[Boolean] of LongWord = (
    0, SCRIPTTYPELIB_ISCONTROL
  );
begin
  for i := 0 to FObjects.Count-1 do
  begin
    ObjectRec := FObjects[i];
    Flags := SCRIPTITEM_ISVISIBLE;
    if ObjectRec.Global then
      Flags := Flags or SCRIPTITEM_GLOBALMEMBERS;
    OleCheck(FActiveScript.AddNamedItem(PWideChar(WideString(ObjectRec.Name)),
      Flags));
  end;

  for i := Low(FTypeLibraries) to High(FTypeLibraries) do
    OleCheck(FActiveScript.AddTypeLib(
      FTypeLibraries[i].CLSID,
      FTypeLibraries[i].MajorVersion,
      FTypeLibraries[i].MinorVersion,
      Options[FTypeLibraries[i].IsControl]));

  OleCheck(FActiveScript.AddNamedItem(SCRIPT_MANAGER_NAME,
    SCRIPTITEM_ISVISIBLE or SCRIPTITEM_GLOBALMEMBERS));
  OleCheck(FActiveScript.AddNamedItem(SCRIPT_WSCRIPT,
    SCRIPTITEM_ISVISIBLE or SCRIPTITEM_GLOBALMEMBERS));
  OleCheck(FActiveScript.AddNamedItem(SCRIPT_SQLGLOBAL,
    SCRIPTITEM_ISVISIBLE or SCRIPTITEM_GLOBALMEMBERS));
end;

procedure TlvkActiveScript.AddTypeLibrary(const CLSID: TCLSID;
  const MajorVersion, MinorVersion: LongWord; const IsControl: Boolean);
begin
  SetLength(FTypeLibraries, Length(FTypeLibraries)+1);
  FTypeLibraries[High(FTypeLibraries)].CLSID := CLSID;
  FTypeLibraries[High(FTypeLibraries)].MajorVersion := MajorVersion;
  FTypeLibraries[High(FTypeLibraries)].MinorVersion := MinorVersion;
  FTypeLibraries[High(FTypeLibraries)].IsControl := IsControl;
  Invalidate;
end;

procedure TlvkActiveScript.AddTypeLibrary(const ProgID: string);
var
  attr  : TLIBATTR;
begin
  if not GetTLBAttr(ProgID, attr) then
    raise ElvkActiveScript.CreateFmt('Type library for %s not found', [ProgID]);
  AddTypeLibrary(Attr.guid, Attr.wMajorVerNum, Attr.wMinorVerNum, False);
end;

procedure TlvkActiveScript.ClearObjects;
var
  p : PObjectRec;
begin
  while FObjects.Count>0 do
  begin
    p := FObjects[0];
    Dispose(p);
    FObjects.Delete(0);
  end;
end;

procedure TlvkActiveScript.Compile;
begin
  if not Assigned(FActiveScript) then
  begin
    if FLanguage = CNoLanguage then
      raise ElvkActiveScriptCompile.CreateRes(@SNoLanguage);

    DoBeforeCompile;

    FSQLGlobal := TlvkSQLGlobal.Create as IDispatch;
    OleCheck(CoCreateInstance(ProgIDToClassID(FLanguage), nil, CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER,
      IActiveScript, FActiveScript));

    FActiveScript.SetScriptSite(FActiveScriptSite);
    AddObjects;
    LoadCode;

    DoAfterCompile;
  end;
end;

constructor TlvkActiveScript.Create(const WindowHandle: THandle);
begin
  FDefaultModule := TlvkActiveScriptModule.CreateDefault(Self);
  FObjects := TList.Create;
  FActiveScriptSite := TlvkActiveScriptSite.Create(Self, WindowHandle);
  FActiveScript := nil;
  FModules := TlvkActiveScriptModuleCollection.Create(Self);
  FLanguage := CNoLanguage;
  FHandle := WindowHandle;
  FScriptManager := TlvkScriptManager.Create(Self) as IDispatch;
  FWScript := TlvkWScript.Create(Self) as IDispatch;
end;

destructor TlvkActiveScript.Destroy;
begin
  ClearObjects;
  Invalidate;
  FScriptManager := nil;
  FWScript := nil;
  FSQLGlobal := nil;
  FModules.Free;
  FActiveScriptSite.Free;
  FreeAndNil(FObjects);
  FreeAndNil(FDefaultModule);
  inherited;
end;

procedure TlvkActiveScript.DoAfterCompile;
begin
  if Assigned(FOnAfterCompile) then
    FOnAfterCompile(Self);
end;

procedure TlvkActiveScript.DoAfterInvalidate;
begin
  if Assigned(FOnAfterInvalidate) then
    FOnAfterInvalidate(Self);
end;

procedure TlvkActiveScript.DoBeforeCompile;
begin
  if Assigned(FOnBeforeCompile) then
    FOnBeforeCompile(Self);
end;

procedure TlvkActiveScript.DoBeforeInvalidate;
begin
  if Assigned(FOnBeforeInvalidate) then
    FOnBeforeInvalidate(Self);
end;

function TlvkActiveScript.GetDispatch: OleVariant;
var
  Disp  : IDispatch;
begin
  Compile;

  OleCheck(FActiveScript.GetScriptDispatch(nil, Disp));
  Result := Disp;
end;

function TlvkActiveScript.GetObject(const Name: string): IDispatch;
var
  ObjectRec : PObjectRec;
  i         : Integer;
begin
  Result := nil;

  if CompareText(Name, SCRIPT_MANAGER_NAME) = 0 then
    Result := FScriptManager
  else if CompareText(Name, SCRIPT_WSCRIPT) = 0 then
    Result := FWScript
  else if CompareText(Name, SCRIPT_SQLGLOBAL) = 0 then
    Result := FSQLGlobal
  else
    for i := 0 to FObjects.Count-1 do
    begin
      ObjectRec := FObjects[i];
      if CompareText(ObjectRec.Name, Name)=0 then
      begin
        Result := ObjectRec.Instance;
        Break;
      end;
    end;
end;

function TlvkActiveScript.GetTLBAttr(const ProgID: string;
  out Attr: TLIBATTR): Boolean;
var
  TLB     : ITypeLib;
  AttrPtr : PTLibAttr;
  Reg     : TRegistry;
  CLSID   : string;
  Server  : string;
begin
  Result := False;

  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CLASSES_ROOT;
      if not Reg.OpenKey(ProgID + '\CLSID', False) then
        Exit;
      CLSID := Reg.ReadString('');
      Reg.CloseKey;
      if Reg.OpenKey('CLSID\' + CLSID + '\InProcServer32', False) then
      begin
        Server := Reg.ReadString('');
        Reg.CloseKey;
      end else if Reg.OpenKey('CLSID\' + CLSID + '\LocalServer32', False) then
      begin
        Server := Reg.ReadString('');
        Reg.CloseKey;
      end else if Reg.OpenKey('CLSID\' + CLSID + '\InProcHandler32', False) then
      begin
        Server := Reg.ReadString('');
        Reg.CloseKey;
      end else
        Exit;

      if Server <> '' then
      begin
        if Server[1] = '"' then
        begin
          Delete(Server, 1, 1);
          if Pos('"', Server) > 0 then
            Server := Copy(Server, 1, Pos('"', Server)-1);
        end else if Server[1] = '''' then
        begin
          Delete(Server, 1, 1);
          if Pos('''', Server) > 0 then
            Server := Copy(Server, 1, Pos('''', Server)-1);
        end else if Pos(' ', Server) > 0 then
          Server := Copy(Server, 1, Pos(' ', Server)-1);

        if LoadTypeLib(PWideChar(WideString(Server)), TLB) <> S_OK then
          Exit;
        try
          TLB.GetLibAttr(AttrPtr);
          try
            Attr := AttrPtr^;
            Result := True;
          finally
            TLB.ReleaseTLibAttr(AttrPtr);
          end;
        finally
          TLB := nil;
        end;
      end;
    finally
      Reg.Free;
    end;
  except
    Result := False;
  end;
end;

procedure TlvkActiveScript.Invalidate;
begin
  if Assigned(FActiveScript) then
  begin
    DoBeforeInvalidate;
    FActiveScript.SetScriptState(SCRIPTSTATE_CLOSED);
    FActiveScript := nil;
    FSQLGlobal := nil;

    DoAfterInvalidate;
  end;
end;

procedure TlvkActiveScript.LoadCode;
var
  Parser        : IActiveScriptParse;
  VarOut        : OleVariant;
  rc            : HRESULT;
  Error         : IActiveScriptSiteError;
  ScriptCode    : string;
  Module        : TlvkActiveScriptModule;
  Index         : Integer;
begin
  Parser := FActiveScript as IActiveScriptParse;
  Parser.InitNew;

  if (FActiveScriptSite as IActiveScriptSite).QueryInterface(IActiveScriptSiteError, Error) <> S_OK then
    Error := nil;

  if Assigned(Error) then
    Error.ClearError;

  // Add code for default module
  ScriptCode := FDefaultModule.GetScriptCode;
  LastModuleName := FDefaultModule.Name;
  rc := Parser.ParseScriptText(PWideChar(WideString(ScriptCode)),
    nil, nil, PWideChar(WideString('')), 0, 0, SCRIPTTEXT_ISVISIBLE, VarOut,
    nil);
  if rc <> S_OK then
  begin
    if Assigned(Error) and Error.ErrorOccured then
      raise ElvkActiveScriptCompile.Create(Error.Error, LastModuleName)
    else
      OleCheck(rc);
  end;

  for Index := 0 to FModules.Count-1 do
  begin
    // Register module with script object
    Module := FModules[Index];
    LastModuleName := Module.Name;
    rc := FActiveScript.AddNamedItem(PWideChar(WideString(Module.Name)),
      {SCRIPTITEM_GLOBALMEMBERS or }SCRIPTITEM_ISVISIBLE or SCRIPTITEM_CODEONLY);
    if rc <> S_OK then
    begin
      if Assigned(Error) and Error.ErrorOccured then
        raise ElvkActiveScriptCompile.Create(Error.Error, LastModuleName)
      else
        OleCheck(rc);
    end;

    // Add code for module
    ScriptCode := Module.GetScriptCode;

    rc := Parser.ParseScriptText(PWideChar(WideString(ScriptCode)),
      PWideChar(WideString(Module.Name)), nil, PWideChar(WideString('')), 0, 0,
      SCRIPTITEM_CODEONLY or SCRIPTTEXT_ISVISIBLE, VarOut, nil);
    if rc <> S_OK then
    begin
      if Assigned(Error) and Error.ErrorOccured then
        raise ElvkActiveScriptCompile.Create(Error.Error, LastModuleName)
      else
        OleCheck(rc);
    end;
  end;
end;

function TlvkActiveScript.RaiseLastScriptError(const ConvertException: Exception=nil): Boolean;
var
  Error : IActiveScriptSiteError;

  function GetCallerAddress: Pointer;
  asm
    mov   eax, [ebp]
  end;

begin
  Result := False;

  if Assigned(ConvertException) then
  begin
    if ConvertException is EOleSysError then
    begin
      case EOleSysError(ConvertException).ErrorCode of
        {$WARNINGS OFF}
        $800A01C2:
          raise EOleSysError.Create(SysErrorMessage(DISP_E_BADPARAMCOUNT),
            DISP_E_BADPARAMCOUNT, 0) at GetCallerAddress;
        {$WARNINGS ON}
      end;
    end;
  end;

  if (FActiveScriptSite as IActiveScriptSite).QueryInterface(IActiveScriptSiteError, Error) <> S_OK then
    Error := nil;

  if Assigned(Error) and Error.ErrorOccured then
  begin
    case Error.Error.Info.scode of
      LongInt($800A03EA):
        raise ElvkActiveScriptCompile.Create(Error.Error, FLastModuleName) at GetCallerAddress;

      LongInt($800A000B):
        raise ElvkActiveScriptRunTime.Create(Error.Error, FLastModuleName) at GetCallerAddress;

    else
      raise ElvkActiveScriptCode.Create(Error.Error, FLastModuleName) at GetCallerAddress;
    end;
  end;
end;

procedure TlvkActiveScript.Recompile;
begin
  Invalidate;
  Compile;
end;

procedure TlvkActiveScript.RemoveObject(const Name: string);
var
  ObjectRec : PObjectRec;
  i         : Integer;
begin
  for i := 0 to FObjects.Count-1 do
  begin
    ObjectRec := FObjects[i];
    if CompareText(ObjectRec.Name, Name)=0 then
    begin
      FObjects.Delete(i);
      Dispose(ObjectRec);
      Invalidate;
      Exit;
    end;
  end;
end;

procedure TlvkActiveScript.RemoveTypeLibrary(const CLSID: TCLSID);
var
  i : Integer;
begin
  i := Low(FTypeLibraries);
  while i <= High(FTypeLibraries) do
  begin
    if CompareMem(@FTypeLibraries[i].CLSID, @CLSID, SizeOf(CLSID)) then
    begin
      while i < High(FTypeLibraries) do
      begin
        FTypeLibraries[i] := FTypeLibraries[i+1];
        Inc(i);
      end;

      SetLength(FTypeLibraries, Length(FTypeLibraries)-1);
    end;

    Inc(i);
  end;
end;

procedure TlvkActiveScript.RemoveTypeLibraries;
begin
  SetLength(FTypeLibraries, 0);
end;

procedure TlvkActiveScript.RemoveTypeLibrary(const ProgID: string);
var
  attr  : TLIBATTR;
begin
  if not GetTLBAttr(ProgID, attr) then
    raise ElvkActiveScript.CreateFmt('Type library for %s not found', [ProgID]);
  RemoveTypeLibrary(Attr.guid);
end;

procedure TlvkActiveScript.SetLanguage(const Value: string);
begin
  if FLanguage<>Value then
  begin
    Invalidate;
    FLanguage := Value;
  end;
end;

procedure TlvkActiveScript.SetScriptCode(const Value: TStrings);
begin
  FDefaultModule.ScriptCode := Value;
end;

procedure TlvkActiveScript.SetScriptFilename(const Value: TScriptFilename);
begin
  FDefaultModule.ScriptFilename := Value;
end;

procedure TlvkActiveScript.RemoveObjects;
var
  o     : PObjectRec;
  Index : Integer;
begin
  for Index := FObjects.Count-1 downto 0 do
  begin
    o := PObjectRec(FObjects[Index]);
    Dispose(o);
  end;

  FObjects.Clear;
  Invalidate;
end;

function TlvkActiveScript.Eval(const Expression: string): OleVariant;
begin
  Result := Eval('', Expression);
end;

procedure TlvkActiveScript.SetModules(
  const Value: TlvkActiveScriptModuleCollection);
begin
  if Assigned(Value) then
    FModules.Assign(Value)
  else
    FModules.Clear;
end;

function TlvkActiveScript.GetScriptFilename: TScriptFilename;
begin
  Result := FDefaultModule.ScriptFilename;
end;

function TlvkActiveScript.Call(const ModuleName, FunctionName: string;
  const Args: array of OleVariant): OleVariant;
var
  Dispatch    : IDispatch;
  rc          : HRESULT;
  Index       : Integer;
  Error       : IActiveScriptSiteError;

  Params      : TDispParams;
  ParamValues : PVariantArgList;
  FuncNameW   : WideString;
  FuncNameWP  : PWideChar;
  DispIDs     : PDispIDList;
  DispID      : TDispID;

  function GetCallerAddress: Pointer;
  asm
    mov   eax, [ebp]
  end;

begin
  Compile;

  LastModuleName := ModuleName;

  // Clear pending errors
  if (FActiveScriptSite as IActiveScriptSite).QueryInterface(IActiveScriptSiteError, Error) <> S_OK then
    Error := nil;

  if Assigned(Error) then
    Error.ClearError;

  // Zero data structures
  FillChar(Params, SizeOf(Params), #0);

  // Get IDispatch interface to script contents
  OleCheck(FActiveScript.GetScriptDispatch(PWideChar(WideString(ModuleName)),
    Dispatch));

  GetMem(ParamValues, Length(Args) * SizeOf(TVariantArg));
  try
    // Copy parameters to list
    Params.rgvarg := ParamValues;
    Params.cArgs := Length(Args);
    for Index := 0 to Length(Args)-1 do
    begin
      ParamValues[Index].vt := VT_BYREF or VT_VARIANT;
      ParamValues[Index].pvarVal := @Args[Index];
    end;

    // Prepare function name as widestring
    FuncNameW := FunctionName;
    FuncNameWP := PWideChar(FuncNameW);

    // Find DispID of procedure to call
    New(DispIDs);
    try
      OleCheck(Dispatch.GetIDsOfNames(IID_NULL, @FuncNameWP, 1,
        LOCALE_SYSTEM_DEFAULT, DispIDs));
      DispID := DispIDs^[0];
    finally
      Dispose(DispIDs);
    end;

    // Run the procedure
    rc := Dispatch.Invoke(DispID, IID_NULL,
      LOCALE_SYSTEM_DEFAULT, DISPATCH_METHOD, Params, @Result,
      nil, nil);

    if rc <> S_OK then
    begin
      if Assigned(Error) and Error.ErrorOccured then
        case Error.Error.Info.scode of
          LongInt($800A03EA):
            raise ElvkActiveScriptCompile.Create(Error.Error, FLastModuleName) at GetCallerAddress;

          LongInt($800A000B):
            raise ElvkActiveScriptRunTime.Create(Error.Error, FLastModuleName) at GetCallerAddress;

        else
          raise ElvkActiveScriptCode.Create(Error.Error, FLastModuleName) at GetCallerAddress;
        end
      else
        OleCheck(rc);
    end;
  finally
    FreeMem(ParamValues);
  end;
end;

function TlvkActiveScript.Call(const FunctionName: string;
  const Args: array of OleVariant): OleVariant;
begin
  Result := Call('', FunctionName, Args);
end;

function TlvkActiveScript.Call(const FunctionName: string): OleVariant;
begin
  Result := Call('', FunctionName, []);
end;

function TlvkActiveScript.Call(const ModuleName,
  FunctionName: string): OleVariant;
begin
  Result := Call(ModuleName, FunctionName, []);
end;

function TlvkActiveScript.GetScriptCode: TStrings;
begin
  Result := FDefaultModule.FScriptCode;
end;

function TlvkActiveScript._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TlvkActiveScript._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TlvkActiveScript.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
var
  Disp  : IDispatch;
begin
  Compile;

  OleCheck(FActiveScript.GetScriptDispatch(nil, Disp));
  Result := Disp.GetIDsOfNames(IID, Names, NameCount, LocaleID, DispIDs);
end;

function TlvkActiveScript.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
var
  Disp  : IDispatch;
begin
  Compile;

  OleCheck(FActiveScript.GetScriptDispatch(nil, Disp));
  Result := Disp.GetTypeInfo(Index, LocaleID, TypeInfo);
end;

function TlvkActiveScript.GetTypeInfoCount(out Count: Integer): HResult;
var
  Disp  : IDispatch;
begin
  Compile;

  OleCheck(FActiveScript.GetScriptDispatch(nil, Disp));
  Result := Disp.GetTypeInfoCount(Count);
end;

function TlvkActiveScript.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
var
  Disp  : IDispatch;
begin
  Compile;

  OleCheck(FActiveScript.GetScriptDispatch(nil, Disp));
  Result := Disp.Invoke(DispID, IID, LocaleID, Flags, Params, VarResult,
    ExcepInfo, ArgErr);
end;

function TlvkActiveScript.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

procedure TlvkActiveScript.Assign(Source: TPersistent);
var
  Script    : TlvkActiveScript;
  Index     : Integer;
  DstObject : PObjectRec;
  SrcObject : PObjectRec;
begin
  if Source is TlvkActiveScript then
  begin
    Invalidate;
    Script := TlvkActiveScript(Source);

    // Copy language setting and default module
    FDefaultModule.Assign(Script.FDefaultModule);

    // Copy all modules
    Modules.Assign(Script.Modules);

    // Copy all registered objects
    ClearObjects;
    for Index := 0 to Script.FObjects.Count-1 do
    begin
      SrcObject := Script.FObjects[Index];

      New(DstObject);
      FObjects.Add(DstObject);
      DstObject^ := SrcObject^;
    end;

    // Copy type libraries
    FTypeLibraries := Script.FTypeLibraries;

    // Copy event handlers
    FOnBeforeCompile := Script.OnBeforeCompile;
    FOnAfterCompile := Script.OnAfterCompile;
    FOnBeforeInvalidate := Script.OnBeforeInvalidate;
    FOnAfterInvalidate := Script.OnAfterInvalidate;

    // Copy rest
    FHandle := Script.FHandle;
  end else
    inherited;
end;

function TlvkActiveScript.GetLanguage: string;
begin
  Result := FLanguage;
end;

function TlvkActiveScript.GetOnAfterCompile: TNotifyEvent;
begin
  Result := FOnAfterCompile;
end;

function TlvkActiveScript.GetOnAfterInvalidate: TNotifyEvent;
begin
  Result := FOnAfterInvalidate;
end;

function TlvkActiveScript.GetOnBeforeCompile: TNotifyEvent;
begin
  Result := FOnBeforeCompile;
end;

function TlvkActiveScript.GetOnBeforeInvalidate: TNotifyEvent;
begin
  Result := FOnBeforeInvalidate;
end;

procedure TlvkActiveScript.SetOnAfterCompile(const Value: TNotifyEvent);
begin
  FOnAfterCompile := Value;
end;

procedure TlvkActiveScript.SetOnAfterInvalidate(const Value: TNotifyEvent);
begin
  FOnAfterInvalidate := Value;
end;

procedure TlvkActiveScript.SetOnBeforeCompile(const Value: TNotifyEvent);
begin
  FOnBeforeCompile := Value;
end;

procedure TlvkActiveScript.SetOnBeforeInvalidate(
  const Value: TNotifyEvent);
begin
  FOnBeforeInvalidate := Value;
end;

function TlvkActiveScript.GetModules: TlvkActiveScriptModuleCollection;
begin
  Result := FModules;
end;

function TlvkActiveScript.GetActiveScript: IActiveScript;
begin
  Result := FActiveScript;
end;

procedure TlvkActiveScript.AfterConstruction;
begin
// Release the constructor's implicit refcount
  InterlockedDecrement(FRefCount);
end;

procedure TlvkActiveScript.BeforeDestruction;
begin
  if RefCount <> 0 then
    RunError(204);
end;

// Set an implicit refcount so that refcounting
// during construction won't destroy the object.
class function TlvkActiveScript.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TlvkActiveScript(Result).FRefCount := 1;
end;

function TlvkActiveScript.Eval(const ModuleName,
  Expression: string): OleVariant;
var
  Parser        : IActiveScriptParse;
  VarOut        : OleVariant;
  rc            : HRESULT;
  Error         : IActiveScriptSiteError;
begin
  Compile;

  LastModuleName := ModuleName;

  Parser := FActiveScript as IActiveScriptParse;

  if (FActiveScriptSite as IActiveScriptSite).QueryInterface(IActiveScriptSiteError, Error) <> S_OK then
    Error := nil;

  if Assigned(Error) then
    Error.ClearError;

  rc := Parser.ParseScriptText(PWideChar(WideString(Expression)),
    PWideChar(WideString(ModuleName)), nil, nil, 0, 0, SCRIPTTEXT_ISEXPRESSION,
    VarOut, nil);
  if rc <> S_OK then
  begin
    if Assigned(Error) and Error.ErrorOccured then
      raise ElvkActiveScriptCompile.Create(Error.Error, LastModuleName)
    else
      OleCheck(rc);
  end else
    Result := VarOut;
end;

function TlvkActiveScript.IdentifierExists(
  const IdentifierName: string): Boolean;
begin
  Result := IdentifierExists('', IdentifierName);
end;

function TlvkActiveScript.IdentifierExists(const ModuleName,
  IdentifierName: string): Boolean;
var
  Dispatch    : IDispatch;
  rc          : HRESULT;
  IdentNameW  : WideString;
  IdentNameWP : PWideChar;
  DispIDs     : PDispIDList;
begin
  Compile;

  // Get IDispatch interface to script contents
  if ModuleName = '' then
    rc := FActiveScript.GetScriptDispatch(nil, Dispatch)
  else
    rc := FActiveScript.GetScriptDispatch(PWideChar(WideString(ModuleName)),
      Dispatch);
  if rc = S_OK then
  begin
    // Prepare function name as widestring
    IdentNameW := IdentifierName;
    IdentNameWP := PWideChar(IdentNameW);

    // Find DispID of procedure to call
    New(DispIDs);
    try
      rc := Dispatch.GetIDsOfNames(IID_NULL, @IdentNameWP, 1,
        LOCALE_SYSTEM_DEFAULT, DispIDs);
      Result := (rc = S_OK);
    finally
      Dispose(DispIDs);
    end;
  end else
    // Module not found, so identifier cannot exist either
    Result := False;
end;

function TlvkActiveScript.ModuleExists(const ModuleName: string): Boolean;
var
  rc        : HRESULT;
  Dispatch  : IDispatch;
begin
  Compile;

  rc := FActiveScript.GetScriptDispatch(PWideChar(WideString(ModuleName)),
    Dispatch);

  Result := (rc = S_OK) and Assigned(Dispatch);
end;

procedure TlvkActiveScript.GetIdentifierList(const List: TStrings;
  const IncludeIdentifiers: TGetIdentifierListIncludes);
begin
  GetIdentifierList('', List, IncludeIdentifiers);
end;

procedure TlvkActiveScript.GetIdentifierList(const ModuleName: string;
  const List: TStrings; const IncludeIdentifiers: TGetIdentifierListIncludes);
var
  Disp    : IDispatch;
  DispEx  : IDispatchEx;
  rc      : HRESULT;
  DispID  : TDispID;
  Name    : PWideChar;
  Include : Boolean;
begin
  Assert(Assigned(List));
  Compile;

  OleCheck(FActiveScript.GetScriptDispatch(PWideChar(WideString(ModuleName)),
    Disp));
  OleCheck(Disp.QueryInterface(IDispatchEx, DispEx));

  rc := DispEx.GetNextDispID(fdexEnumAll, DISPID_STARTENUM, DispID);
  while rc = S_OK do
  begin
    rc := DispEx.GetMemberName(DispID, Name);
    if rc = S_OK then
    try
      Include := True;
      if Comparetext(Name, SCRIPT_MANAGER_NAME) = 0 then
        Include := False
      else if Assigned(Modules.GetItemByName(Name)) then
        Include := gilModules in IncludeIdentifiers
      else if Assigned(GetObject(Name)) then
        Include := gilObjects in IncludeIdentifiers;

      if Include then
        List.Add(Name);
    finally
      SysFreeString(Name);
    end;

    rc := DispEx.GetNextDispID(fdexEnumAll, DispID, DispID);
  end;
end;

procedure TlvkActiveScript.GetInstalledLanguages(const List: TStrings;
  const IncludeEncodedVersions: Boolean);
var
  CatInfo           : ICatInformation;
  Category          : TGUID;
  rc                : HRESULT;
  Enumerator        : IEnumGUID;
  ClassID           : TCLSID;
  ProgID            : string;
  Fetched           : Cardinal;
begin
  CatInfo := CreateComObject(CLSID_StdComponentCategoryMgr) as ICatInformation;
  Category := CATID_ActiveScriptParse;
  rc := CatInfo.EnumClassesOfCategories(1, @Category, 0, nil, Enumerator);
  if rc = S_OK then
  begin
    while Enumerator.Next(1, ClassID, Fetched) = S_OK do
    begin
      ProgID := ClassIDToProgID(ClassID);

      if (Length(ProgID) > 7) and
        (CompareText(Copy(ProgID, Length(ProgID)-6, 7), '.Encode') = 0) and
        (not IncludeEncodedVersions) then
      begin
        Continue;
      end;
      List.Add(ProgID);
    end;
  end;
end;

function TlvkActiveScript.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TlvkActiveScript.GetPreProcess: Boolean;
begin
  Result := FDefaultModule.PreProcess;
end;

procedure TlvkActiveScript.SetPreProcess(const Value: Boolean);
begin
  FDefaultModule.PreProcess := Value;
end;

{ ElvkActiveScriptCode }

constructor ElvkActiveScriptCode.Create(
  const Error: TlvkActiveScriptErrorRec;
  const LastModuleName: string);
begin
  FError := Error;
  if FError.Info.bstrDescription = '' then
    FError.Info.bstrDescription := SysErrorMessage(FError.Info.scode);
  if FError.Info.bstrSource = '' then
    FError.Info.bstrSource := 'TlvkActiveScript';
   FLastModuleName := LastModuleName;

  inherited CreateFmt(SCompileException, [Source, Description, CharPos, LineNo]);
end;

function ElvkActiveScriptCode.GetInteger(const Index: Integer): Integer;
begin
  case Index of
    iLineNo         : Result := FError.lineNo;
    iCharPos        : Result := FError.charPos;
    iHelpContext    : Result := FError.Info.dwHelpContext;
    iErrorCode      : Result := FError.Info.wCode;
    iHResult        : Result := FError.Info.scode;
  else
    raise Exception.CreateFmt(SInvalidIndex, ['ElvkActiveScriptCode.GetInteger']);
  end;
end;

function ElvkActiveScriptCode.GetString(const Index: Integer): string;
begin
  case Index of
    iSource       : Result := FError.Info.bstrSource;
    iDescription  : Result := FError.Info.bstrDescription;
    iHelpFile     : Result := FError.Info.bstrHelpFile;
  else
    raise Exception.CreateFmt(SInvalidIndex, ['ElvkActiveScriptCode.GetString']);
  end;
end;

{ TlvkActiveScriptModule }

procedure TlvkActiveScriptModule.Assign(Source: TPersistent);
var
  Module  : TlvkActiveScriptModule;
begin
  if Source is TlvkActiveScriptModule then
  begin
    Module := TlvkActiveScriptModule(Source);
    FName := Module.Name;
    FScriptCode.Assign(Module.ScriptCode);
    FScriptFilename := Module.ScriptFilename;
    FGlobalMembers := Module.GlobalMembers;
  end else
    inherited;
end;

constructor TlvkActiveScriptModule.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  Assert(Assigned(Collection));
  InitializeModule(
    (Collection as TlvkActiveScriptModuleCollection).ActiveScript);
  FName := (Collection as TlvkActiveScriptModuleCollection).NewModuleName;
end;

constructor TlvkActiveScriptModule.CreateDefault(
  const ActiveScript: TlvkActiveScript);
begin
  inherited Create(nil);

  Assert(Assigned(ActiveScript));
  InitializeModule(ActiveScript);
end;

destructor TlvkActiveScriptModule.Destroy;
begin
  FScriptCode.Free;

  inherited;
end;

function TlvkActiveScriptModule.GetDisplayName: string;
begin
  if FName <> '' then
  begin
    Result := FName;

    if FScriptFilename <> '' then
      Result := Result + ': ' + ExtractFilename(FScriptFilename)
    else if FScriptCode.Count > 0 then
      Result := Result + ': inline code';
  end else
    Result := inherited GetDisplayName;
end;

function TlvkActiveScriptModule.GetPreProcess: Boolean;
begin
  Result := FPreProcess;
end;

function TlvkActiveScriptModule.GetScriptCode: string;
var
  ScriptFile  : TStream;
  Filename    : string;
begin
  if FScriptFilename <> '' then
  begin
    Filename := ExpandScriptFilename(FScriptFilename);

    ScriptFile := TFileStream.Create(Filename,
      fmOpenRead or fmShareDenyWrite);
    try
      SetLength(Result, ScriptFile.Size);
      if ScriptFile.Size>0 then
        ScriptFile.ReadBuffer(Result[1], ScriptFile.Size);
    finally
      ScriptFile.Free;
    end;
  end else
    Result := FScriptCode.Text;

  if FPreProcess and (Result <> '') then
    Result := lvkActiveScriptPreProcessor.PreProcess(FActiveScript.Language,
      Result);
end;

procedure TlvkActiveScriptModule.InitializeModule(
  const ActiveScript: TlvkActiveScript);
begin
  Assert(Assigned(ActiveScript));
  FActiveScript := ActiveScript;

  FName := '';
  FScriptCode := TStringList.Create;
  (FScriptCode as TStringList).OnChange := ScriptChanged;
  FScriptFilename := '';
end;

procedure TlvkActiveScriptModule.Invalidate;
begin
  FActiveScript.Invalidate;
end;

procedure TlvkActiveScriptModule.ScriptChanged(Sender: TObject);
begin
  FScriptFilename := '';
  FActiveScript.Invalidate;
end;

procedure TlvkActiveScriptModule.SetGlobalMembers(const Value: Boolean);
begin
  if FGlobalMembers <> Value then
  begin
    FGlobalMembers := Value;
    Invalidate;
  end;
end;

procedure TlvkActiveScriptModule.SetName(const Value: string);
var
  TrimmedName : string;
begin
  TrimmedName := Trim(Value);
  if TrimmedName = '' then
    raise ElvkActiveScript.Create('You cannot have a module without a name');

  if FName <> TrimmedName then
  begin
    if Assigned(Collection) then
    begin
      if Assigned((Collection as TlvkActiveScriptModuleCollection).ItemsByName[TrimmedName]) then
        raise ElvkActiveScript.CreateFmt('A module with the name "%s" already exists', [TrimmedName]);
    end;

    FName := TrimmedName;
    Invalidate;
  end;
end;

procedure TlvkActiveScriptModule.SetPreProcess(const Value: Boolean);
begin
  if FPreProcess <> Value then
  begin
    FPreProcess := Value;
    Invalidate;
  end;
end;

procedure TlvkActiveScriptModule.SetScriptCode(const Value: TStrings);
begin
  if Assigned(Value) then
    FScriptCode.Assign(Value)
  else
    FScriptCode.Clear;
  FScriptFilename := '';
  Invalidate;
end;

procedure TlvkActiveScriptModule.SetScriptFilename(const Value: TScriptFilename);
begin
  if Value <> FScriptFilename then
  begin
    FScriptCode.Clear;
    FScriptFilename := Value;
    Invalidate;
  end;
end;

{ TlvkActiveScriptModuleCollection }

function TlvkActiveScriptModuleCollection.Add(const ModuleName: string): TlvkActiveScriptModule;
begin
  Result := inherited Add as TlvkActiveScriptModule;
  if ModuleName <> '' then
    Result.Name := ModuleName;
end;

constructor TlvkActiveScriptModuleCollection.Create(
  const ActiveScript: TlvkActiveScript);
begin
  inherited Create(ActiveScript, TlvkActiveScriptModule);
  FActiveScript := ActiveScript;
end;

function TlvkActiveScriptModuleCollection.FindItemID(
  ID: Integer): TlvkActiveScriptModule;
begin
  Result := inherited FindItemID(ID) as TlvkActiveScriptModule;
end;

function TlvkActiveScriptModuleCollection.GetItem(
  const Index: Integer): TlvkActiveScriptModule;
begin
  Result := inherited Items[Index] as TlvkActiveScriptModule;
end;

function TlvkActiveScriptModuleCollection.GetItemByName(
  const Name: string): TlvkActiveScriptModule;
var
  Index : Integer;
begin
  Result := nil;
  for Index := 0 to Count-1 do
  begin
    if CompareText(Items[Index].Name, Name) = 0 then
    begin
      Result := Items[Index];
      Break;
    end;
  end;
end;

function TlvkActiveScriptModuleCollection.Insert(
  Index: Integer): TlvkActiveScriptModule;
begin
  Result := inherited Insert(Index) as TlvkActiveScriptModule;
end;

function TlvkActiveScriptModuleCollection.NewModuleName: string;
begin
  Inc(FNextNameIndex);
  Result := Format('Module%d', [FNextNameIndex]);
end;

procedure TlvkActiveScriptModuleCollection.SetItem(const Index: Integer;
  const Value: TlvkActiveScriptModule);
begin
  inherited Items[Index] := Value;
end;

{ TlvkScriptManager }

constructor TlvkScriptManager.Create(const Owner: TlvkActiveScript);
begin
  inherited Create;

  FOwner := Owner;
end;

function TlvkScriptManager.IncludeFile(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
var
  Filename  : string;
  Index     : Integer;
  Found     : Boolean;

  function FindFile(var Filename: string;
    const ReferenceFilename: string): Boolean;
  var
    Temp  : string;
  begin
    Temp := ExtractFilePath(ReferenceFilename);

    if Copy(Filename, 1, 1) = '\' then
      Temp := Temp + Copy(Filename, 2, Length(Filename)-1)
    else
      Temp := Temp + Filename;

    Result := FileExists(Temp);
    if Result then
      Filename := Temp;
  end;

  function LoadFile(const Filename: string): HRESULT;
  var
    Parser            : IActiveScriptParse;
    Stream            : TStream;
    ScriptCode        : string;
    Error             : IActiveScriptSiteError;
    ActiveScriptSite  : IActiveScriptSite;
    VarOut            : OleVariant;
  begin
    Parser := FOwner.ActiveScript as IActiveScriptParse;
    Parser.InitNew;

    OleCheck(FOwner.ActiveScript.GetScriptSite(IActiveScriptSite,
      ActiveScriptSite));
    if ActiveScriptSite.QueryInterface(IActiveScriptSiteError, Error) <> S_OK then
      Error := nil;

    if Assigned(Error) then
      Error.ClearError;

    Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
    try
      SetLength(ScriptCode, Stream.Size);
      if Stream.Size > 0 then
        Stream.ReadBuffer(ScriptCode[1], Stream.Size);
    finally
      Stream.Free;
    end;

    Result := Parser.ParseScriptText(PWideChar(WideString(ScriptCode)),
      nil, nil, PWideChar(WideString('')), 0, 0, SCRIPTTEXT_ISVISIBLE, VarOut,
      nil);
  end;

begin
  if Length(Parameters) = 1 then
  begin
    Filename := ExpandScriptFilename(Parameters[0]);

    if Filename = '' then
      Result := DISP_E_TYPEMISMATCH
    else begin
      if Pos(':', Filename) = 0 then
      begin
        // Is unqualified filename, attempt to find file

        Found := False;
        if FOwner.ScriptFilename <> '' then
          Found := FindFile(Filename, FOwner.ScriptFilename);

        if not Found then
          for Index := 0 to FOwner.Modules.Count-1 do
            if FOwner.Modules[Index].ScriptFilename <> '' then
            begin
              Found := FindFile(Filename, FOwner.Modules[Index].ScriptFilename);
              if Found then
                Break;
            end;

        if not Found then
          FindFile(Filename, ParamStr(0));
      end;

      if FileExists(Filename) then
        Result := LoadFile(Filename)
      else
        Result := DISP_E_PARAMNOTFOUND;
    end;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

function TlvkScriptManager.LoadTypeLibrary(
  var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
var
  attr  : TLIBATTR;
begin
  if Length(Parameters) = 1 then
  begin
    if not FOwner.GetTLBAttr(Parameters[0], attr) then
      Result := DISP_E_PARAMNOTFOUND
    else begin
      Result := FOwner.ActiveScript.AddTypeLib(attr.guid, attr.wMajorVerNum,
        attr.wMinorVerNum, 0);
    end;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

{ TlvkWScript }

function TlvkWScript.GetObject(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
begin
  if Length(Parameters) = 1 then
  begin
    try
      Result := S_OK;
      FunctionResult := lvkActiveScript.GetObject(Parameters[0]);
    except
      on E: EOleException do
        Result := E.ErrorCode;
      on E: EOleSysError do
        Result := E.ErrorCode;
      else
        Result := E_FAIL;
    end;
  end else if Length(Parameters) = 2 then
  begin
    try
      Result := S_OK;
      FunctionResult := TlvkEventSinkWrapper.Create(FOwner, Parameters[0], Parameters[1], True) as IDispatch;
    except
      on E: EOleException do
        Result := E.ErrorCode;
      on E: EOleSysError do
        Result := E.ErrorCode;
      else
        Result := E_FAIL;
    end;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

constructor TlvkWScript.Create(const Owner: TlvkActiveScript);
begin
  inherited Create;

  FOwner := Owner;
end;

function TlvkWScript.CreateObject(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
begin
  if Length(Parameters) = 1 then
  begin
    try
      Result := S_OK;
      FunctionResult := CreateOleObject(Parameters[0]);
    except
      on E: EOleException do
        Result := E.ErrorCode;
      on E: EOleSysError do
        Result := E.ErrorCode;
      else
        Result := E_FAIL;
    end;
  end else if Length(Parameters) = 2 then
  begin
    try
      Result := S_OK;
      FunctionResult := TlvkEventSinkWrapper.Create(FOwner, Parameters[0], Parameters[1], False) as IDispatch;
    except
      on E: EOleException do
        Result := E.ErrorCode;
      on E: EOleSysError do
        Result := E.ErrorCode;
      else
        Result := E_FAIL;
    end;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

function TlvkWScript.CreateCollection(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
begin
  if Length(Parameters) = 0 then
  begin
    FunctionResult := lvkCollection.CreateCollection as IDispatch;
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

function TlvkWScript.Assert(var Parameters: array of Variant;
  out FunctionResult: Variant): HRESULT;
begin
  if Length(Parameters) in [1, 2] then
  begin
    Result := S_OK;
    try
      if Length(Parameters) = 1 then
        System.Assert(Boolean(Parameters[0]))
      else
        System.Assert(Boolean(Parameters[0]), Parameters[1]);
    except
      on E: EAssertionFailed do
        raise EOleError.Create('Assertion failed: ' + E.Message);
    end;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

{ TlvkEventSink }

constructor TlvkEventSink.Create(const Owner: TlvkActiveScript;
  const IID: TGUID; const Prefix: string; const TypeInfo: ITypeInfo);
begin
  inherited Create;

  FOwner := Owner;
  FIID := IID;
  FPrefix := Trim(Prefix);
  FTypeInfo := TypeInfo;
end;

function TlvkEventSink.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TlvkEventSink.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TlvkEventSink.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Count := 0;
  Result := S_OK;
end;

function TlvkEventSink.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
var
  Name            : TBStr;
  NameCount       : Integer;
  ScriptDispID    : TDispID;
  NameString      : WideString;
  NamePtr         : PWideChar;
  DispIDs         : PDispIDList;
begin
  Result := E_NOTIMPL;

  if FTypeInfo.GetNames(DispID, @Name, 1, NameCount) = S_OK then
  begin
    if NameCount = 1 then
    begin
      try
        NameString := FPrefix + Name;
        NamePtr := PWideChar(NameString);

        New(DispIDs);
        try
          Result := FOwner.GetIDsOfNames(IID_NULL, @NamePtr, 1,
            LOCALE_SYSTEM_DEFAULT, DispIDs);
          ScriptDispID := DispIDs^[0];
        finally
          Dispose(DispIDs);
        end;
      finally
        SysFreeString(Name);
      end;

      if Result = S_OK then
      try
        Result := FOwner.Invoke(ScriptDispID, IID, LocaleID,
          Flags, Params, VarResult, ExcepInfo, ArgErr);
      except
        on E: Exception do
          if not FOwner.RaiseLastScriptError(E) then
            raise;
      end;
    end;
  end;
end;

function TlvkEventSink.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if IsEqualGUID(IID, FIID) then
  begin
    IDispatch(Obj) := Self;
    Result := S_OK;
  end else
    Result := inherited QueryInterface(IID, Obj);
end;

{ TlvkEventSinkWrapper }

constructor TlvkEventSinkWrapper.Create(const Owner: TlvkActiveScript;
  const ProgID, Prefix: string; const ConnectToActive: Boolean);
var
  ConnectionPointContainer  : IConnectionPointContainer;
  ConnectionPointEnumerator : IEnumConnectionPoints;
  ConnectionPoint           : IConnectionPoint;
  TypeLib                   : ITypeLib;
  IID                       : TGUID;
  TypeInfo                  : ITypeInfo;
  EventSinkObject           : IUnknown;
  Cookie                    : Integer;
begin
  inherited Create;

  FOwner := Owner;

  if ConnectToActive then
    FObject := GetActiveOleObject(ProgID)
  else
    FObject := CreateOleObject(ProgID);
    
  if FObject.QueryInterface(IConnectionPointContainer, ConnectionPointContainer) = S_OK then
  begin
    TypeLib := LoadTypeLibrary(ProgID);
    if Assigned(TypeLib) then
    begin
      if ConnectionPointContainer.EnumConnectionPoints(ConnectionPointEnumerator) = S_OK then
      begin
        while ConnectionPointEnumerator.Next(1, ConnectionPoint, nil) = S_OK do
        begin
          if not Assigned(ConnectionPoint) then
            Break;
          OleCheck(ConnectionPoint.GetConnectionInterface(IID));
          if TypeLib.GetTypeInfoOfGuid(IID, TypeInfo) = S_OK then
          begin
            EventSinkObject := TlvkEventSink.Create(FOwner, IID, Prefix, TypeInfo);
            ConnectionPoint.Advise(EventSinkObject, Cookie);
            SetLength(FEventSinks, Length(FEventSinks)+1);
            FEventSinks[High(FEventSinks)].Cookie := Cookie;
            FEventSinks[High(FEventSinks)].ConnectionPoint := ConnectionPoint;
          end;
        end;
      end;
    end;
  end;
end;

destructor TlvkEventSinkWrapper.Destroy;
var
  Index : Integer;
begin
  for Index := Low(FEventSinks) to High(FEventSinks) do
    OleCheck(FEventSinks[Index].ConnectionPoint.Unadvise(FEventSinks[Index].Cookie));
  FEventSinks := nil;
  FObject := nil;

  inherited;
end;

function TlvkEventSinkWrapper.GetIDsOfNames(const IID: TGUID;
  Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := FObject.GetIDsOfNames(IID, Names, NameCount, LocaleID, DispIDs);
end;

function TlvkEventSinkWrapper.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := FObject.GetTypeInfo(Index, LocaleID, TypeInfo);
end;

function TlvkEventSinkWrapper.GetTypeInfoCount(
  out Count: Integer): HResult;
begin
  Result := FObject.GetTypeInfoCount(Count);
end;

function TlvkEventSinkWrapper.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := FObject.Invoke(DispID, IID, LocaleID, Flags, Params, VarResult,
    ExcepInfo, ArgErr);
end;

end.

