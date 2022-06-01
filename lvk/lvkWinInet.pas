{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains objects and interfaces for WinInet use, to download
    and upload files from/to ftp and web servers.
}
unit lvkWinInet;

// $Author: Lasse V. Karlsen $
// $Revision: 9 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkWinInet.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, WinInet, lvkRegExp, lvkThread, lvkVersion, lvkTypes;

const
  DEFAULT_FTP_PORT    = 21;
  DEFAULT_HTTP_PORT   = 80;
  DEFAULT_HTTPS_PORT  = 443;
  
type
  IInternetSession = interface;

  { Description:
      This event handler will be called for each status message sent to the
      objects by the WinInet internet library. See the
      IInternetSession.OnCallback event handler property.
    Parameters:
      Session - The session object that the status message originated in.
      StatusCode - An INTERNET_STATUS_xxx status code describing the event.
      StatusInformation - Pointer to any accompanying status information for
        the event. You must check the various status codes to see what kind
        of data to expect here.
      StatusInformationLength - Amount of bytes that StatusInformation points
        to.
    See also:
      IInternetSession.OnCallback
  }
  TInternetCallback = procedure(const Session: IInternetSession;
    const StatusCode: Cardinal;
    const StatusInformation: Pointer;
    const StatusInformationLength: Cardinal) of object;

  { Description:
      This interface wraps an object that stores authentication information,
      basically just a username and/or password.
  }
  IInternetAuthentication = interface
    ['{3090823E-A4C5-4B7B-AACC-533780971957}']

    // <COMBINE Username>
    function GetUsername: string;
    // <COMBINE Username>
    procedure SetUsername(const NewValue: string);
    { Description:
        This property holds the username for the authentication token.
      See also:
        Password
    }
    property Username: string read GetUsername write SetUsername;

    // <COMBINE Password>
    function GetPassword: string;
    // <COMBINE Password>
    procedure SetPassword(const NewValue: string);
    { Description:
        This property holds the password for the authentication token.
      See also:
        Username
    }
    property Password: string read GetPassword write SetPassword;
  end;

  { Description:
      This type enumerates the kinds of proxies that the WinInet session
      object supports.
  }
  TInternetProxyType = (
    // Don't use a proxy when connecting to the host.
    ptNone,
    // ptPreConfigured - Use whatever setting is stored for the current user
    // in the Internet tools control panel.
    ptPreConfigured,
    // Same as above, but don't automatically turn on proxy support if it
    // detects that a proxy is used.
    ptPreconfiguredWithoutAutoProxy,
    // Force using a proxy.
    ptProxy);

  { Description:
      This interface wraps an object responsible for holding proxy information
      for the session object. You can specify what kind of proxy to use,
      and any authentication information to use when connecting through that
      proxy.
  }
  IInternetProxy = interface
    ['{B28DF51E-B397-43D3-A8AC-CB9BB7B59497}']

    // <COMBINE Authentication>
    function GetAuthentication: IInternetAuthentication;
    { Description:
        This property gives access to an authentication token that is used to
        connect to the proxy. This token is not used on the host to connect to,
        only the proxy that sits in between.
      See also:
        IInternetAuthentication
    }
    property Authentication: IInternetAuthentication read GetAuthentication;

    // <COMBINE ProxyType>
    function GetProxyType: TInternetProxyType;
    // <COMBINE ProxyType>
    procedure SetProxyType(const NewValue: TInternetProxyType);
    { Description:
        This property holds the type of proxy to use, if any. It defaults to
        ptPreconfigured so that most internet connections should automatically
        work.
      See also:
        TInternetProxyType
    }
    property ProxyType: TInternetProxyType read GetProxyType write SetProxyType;

    // <COMBINE HostName>
    function GetHostName: string;
    // <COMBINE HostName>
    procedure SetHostName(const NewValue: string);
    { Description:
        This property holds the name of the host that operates as a proxy for
        the internet session.
      See also:
        Port
    }
    property HostName: string read GetHostName write SetHostName;

    // <COMBINE Port>
    function GetPort: Word;
    // <COMBINE Port>
    procedure SetPort(const NewValue: Word);
    { Description:
        This property holds the port value of the proxy software.
      See also:
        HostName
    }
    property Port: Word read GetPort write SetPort;

    // <COMBINE Bypass>
    function GetBypass: string;
    // <COMBINE Bypass>
    procedure SetBypass(const NewValue: string);
    { Description:
        This property holds the addresses of local sites and ip addresses. Any
        access to those addresses will connect directly and not through the
        proxy.
    }
    property Bypass: string read GetBypass write SetBypass;
  end;

  { Description:
      This interface wraps an object that handles the internet session. The
      objects in this unit all require a session for them to work, but the
      objects will automatically create a dummy session if you don't provide
      them with one.
  }
  IInternetSession = interface
    ['{27F9A8CE-6CA3-4DF0-811F-CAE6A9649828}']

    { Description:
        Connect the session so that it's usable.

        Note: Calling this method executes the same code as setting the
          Connected property to True.
      See also:
        Disconnect, Connected
    }
    procedure Connect;

    { Description:
        Disconnect the session, access after this call to any object that
        relies on the session object will fail.

        Note: Calling this method executes the same code as setting the
          Connected property to False.
      See also:
        Connect, Connected
    }
    procedure Disconnect;

    // <COMBINE Connected>
    function GetConnected: Boolean;
    // <COMBINE Connected>
    procedure SetConnected(const NewValue: Boolean);
    { Description:
        This property holds the current connection status, connected or not.
        Setting this property to True will connect the session, and setting it
        to False will disconnect the session.
      See also:
        Connect, Disconnect
    }
    property Connected: Boolean read GetConnected write SetConnected;

    // <COMBINE Offline>
    function GetOffline: Boolean;
    // <COMBINE Offline>
    procedure SetOffline(const NewValue: Boolean);
    { Description:
        This property holds the offline status used for the session. Set this
        property to True before connecting the session to create an offline
        session.

        An offline session will return all downloaded items from the local
        cache, instead of retrieving them over the network. Any items not
        found in the local cache will raise an exception.
    }
    property Offline: Boolean read GetOffline write SetOffline;

    // <COMBINE UserAgent>
    function GetUserAgent: string;
    // <COMBINE UserAgent>
    procedure SetUserAgent(const NewValue: string);
    { Description:
        This property holds the useragent identifier used for the session.
        This useragent will be passed to web servers in the http header. By
        default, the name of the class that implements this interface will
        be used, but other agent names can be used.

        Note: Web servers might use the agent name to determine the capabilities
          of the client web browser and return content targetted at specific
          browsers. As such, you should check if the web sites you need to
          contact does this, and set a user agent identifier that matches the
          kind of content you wish to retrieve.
    }
    property UserAgent: string read GetUserAgent write SetUserAgent;

    // <COMBINE Proxy>
    function GetProxy: IInternetProxy;
    { Description:
        This property gives access to the proxy object used for the internet
        session. You can configure the session's use of a proxy server and any
        authentication for the proxy through this object.
    }
    property Proxy: IInternetProxy read GetProxy;

    // <COMBINE OnCallback>
    function GetOnCallback: TInternetCallback;
    // <COMBINE OnCallback>
    procedure SetOnCallback(const NewValue: TInternetCallback);
    { Description:
        This property holds an event handler that will be called periodically
        by the WinInet library with information about what the library is
        currently doing, and progress messages. You can use this to capture
        messages and other information from the server.
      See also:
        TInternetCallback
    }
    property OnCallback: TInternetCallback read GetOnCallback
      write SetOnCallback;
  end;

  { Description:
      This interface is implemented by all the objects that rely on a session
      to do their work, like IFTP and IHTTP.

      Note: The object that this interface encapsulates also support the
      IThreadCooperate interface of the lvkThread package. This means that you
      can register the objects that support the ISessionBased interface
      with a thread, and doing a Thread.Terminate while the object is
      downloading or uploading will abort the transfer.
  }
  ISessionBased = interface
    ['{C0DDAD33-43F1-4348-B6BD-AE21332EBC81}']

    // <COMBINE Session>
    function GetSession: IInternetSession;
    // <COMBINE Session>
    procedure SetSession(const NewValue: IInternetSession);
    { Description:
        This property gives access to the session object used for this
        internet object. If the programmer doesn't explicitly create a session
        object and feeds it to the internet object, a default session will
        be created and used just for this internet object.
    }
    property Session: IInternetSession read GetSession write SetSession;

    // <COMBINE HostName>
    function GetHostName: string;
    // <COMBINE HostName>
    procedure SetHostName(const NewValue: string);
    { Description:
        This property holds the HostName that this internet object will connect
        to in order to download or upload a file (or do other related things).
      See also:
        Port
    }
    property HostName: string read GetHostName write SetHostName;

    // <COMBINE Port>
    function GetPort: Word;
    // <COMBINE Port>
    procedure SetPort(const NewValue: Word);
    { Description:
        This property holds the port that this internet object will connect on
        on the remote server.
      See also:
        HostName
    }
    property Port: Word read GetPort write SetPort;

    // <COMBINE Authentication>
    function GetAuthentication: IInternetAuthentication;
    { Description:
        This property gives access to the authentication token used when
        connecting to the remote server. FTP will use an anonymous connection
        if the username/password fields of this token has not been set, and
        HTTP will use an unauthenticated connection.
      See also:
        IInternetAuthentication
    }
    property Authentication: IInternetAuthentication read GetAuthentication;

    { Description:
        This method will connect to the remote server, but not initiate
        anything else than the connection. If the server requires
        authentication then this will be performed as part of this operation.

        If this method succeeds, the program can issue download, upload, and
        other related commands on the internet object which will then be
        relied to the remote server.

        Calling Connect when the internet object is already connected will not
        do anything.
      See also:
        Disconnect, Connected
    }
    procedure Connect;

    { Description:
        This method will disconnect from the remote server. After this method
        has been called, no operations are possible on the remote server,
        unless a new connection has been made.

        Calling Disconnect when the internet object is not connected will not
        do anything.
      See also:
        Connect, Connected
    }
    procedure Disconnect;

    // <COMBINE Connected>
    function GetConnected: Boolean;
    // <COMBINE Connected>
    procedure SetConnected(const NewValue: Boolean);
    { Description:
        This property holds the current connection status of this internet
        object. The program can set this property to True or False to either
        connect to or disconnect from the remote server.

        Note: Setting this property to True is the same as calling Connect,
          and setting it to False is the same as calling Disconnect.
      See also:
        Connect, Disconnect
    }
    property Connected: Boolean read GetConnected write SetConnected;
  end;

  { Description:
      This interface forms the basis of the two different items that can be
      found in an FTP directory, directories and files. This interface
      wraps an object that supplies the basic information about both types
      of items, and only those properties that are common for the two types.
    See also:
      IFTPDirectory, IFTPFile
  }
  IFTPDirectoryItem = interface
    ['{2942B1C5-015E-46CC-BB95-9105A80E1758}']

    // <COMBINE Name>
    function GetName: string;
    { Description:
        This property returns the name of the file or directory.
      See also:
        AlternateName
    }
    property Name: string read GetName;

    // <COMBINE AlternateName>
    function GetAlternateName: string;
    { Description:
        This property returns the alternate (usually the "short") name of the
        file or directory.
      See also:
        Name
    }
    property AlternateName: string read GetAlternateName;

    // <COMBINE CreationTime>
    function GetCreationTime: TDateTime;
    { Description:
        This property returns the creation time of the file or directory,
        as reported by the ftp server.
      See also:
        LastAccessTime, LastWriteTime
    }
    property CreationTime: TDateTime read GetCreationTime;

    // <COMBINE LastAccessTime>
    function GetLastAccessTime: TDateTime;
    { Description:
        This property returns the last access time of the file or directory,
        as reported by the ftp server.
      See also:
        CreationTime, LastWriteTime
    }
    property LastAccessTime: TDateTime read GetLastAccessTime;

    // <COMBINE LastWriteTime>
    function GetLastWriteTime: TDateTime;
    { Description:
        This property returns the last write time of the file or directory,
        as reported by the ftp server.
      See also:
        CreationTime, LastAccessTime
    }
    property LastWriteTime: TDateTime read GetLastWriteTime;

    // <COMBINE Attributes>
    function GetAttributes: Cardinal;
    { Description:
        This property returns a normal file attribute bitmask. Use the
        FILE_FLAG_xxx constants to check what bits are set to learn the
        flag configuration for the given file or directory.
    }
    property Attributes: Cardinal read GetAttributes;
  end;

  { Description:
      This interface wraps an FTP directory item object. It inherits all the
      properties from the IFTPDirectoryItem interface and adds no extra
      properties to those.
    See also:
      IFTPDirectoryItem, IFTPFile
  }
  IFTPDirectory = interface(IFTPDirectoryItem)
    ['{14AD954E-A74E-42EE-9248-894202E3F4E5}']
  end;

  { Description:
      This interface wraps an FTP file item object. It inherits all the
      properties from the IFTPDirectoryItem interface and adds a Size property
      to get the size of the file.
    See also:
      IFTPDirectoryItem, IFTPDirectory
  }
  IFTPFile = interface(IFTPDirectoryItem)
    ['{59ADF4B8-092E-4C22-BF8C-8D296D337C04}']

    // <COMBINE Size>
    function GetSize: Int64;
    { Description:
        This property returns the size of the file, as reported by the
        server.
    }
    property Size: Int64 read GetSize;
  end;

  { Description:
      This interface wraps an object that holds the files and directories
      reported by the server from a Dir command being issued.
    See also:
      IFTPDirectoryItem, IFTPFile, IFTPDirectory
  }
  IFTPDirectoryListing = interface
    ['{58394A8D-E761-4F8D-B44C-C58352699F08}']

    // <COMBINE DirectoryCount>
    function GetDirectoryCount: Integer;
    { Description:
        This property returns the number of directories listed through the
        object.
      See also:
        Directories
    }
    property DirectoryCount: Integer read GetDirectoryCount;

    // <COMBINE Directories>
    function GetDirectories(const Index: Integer): IFTPDirectory;
    { Description:
        This property returns a given directory item from the directory listing.
        This property is a 0-based indexed property.
      See also:
        DirectoryCount
    }
    property Directories[const Index: Integer]: IFTPDirectory
      read GetDirectories;

    // <COMBINE FileCount>
    function GetFileCount: Integer;
    { Description:
        This property returns the number of files listed through the
        object.
      See also:
        Files
    }
    property FileCount: Integer read GetFileCount;

    // <COMBINE Files>
    function GetFiles(const Index: Integer): IFTPFile;
    { Description:
        This property returns a given file item from the directory listing.
        This property is a 0-based indexed property.
      See also:
        FileCount
    }
    property Files[const Index: Integer]: IFTPFile read GetFiles;

    // <COMBINE ItemCount>
    function GetItemCount: Integer;
    { Description:
        This property returns the number of items stored in the directory
        listing. This number is the sum of directories and files stored.
      See also:
        Items
    }
    property ItemCount: Integer read GetItemCount;

    // <COMBINE Items>
    function GetItems(const Index: Integer): IFTPDirectoryItem;
    { Description:
        This property returns a given item from the directory listing. The
        first items to be returned will be directories, followed by the
        files.
        This property is a 0-based indexed property.
      See also:
        ItemCount
    }
    property Items[const Index: Integer]: IFTPDirectoryItem
      read GetItems; default;

    { Description:
        This function returns True if a directory with the given name exists
        in the directory listing.
      See also:
        FileExists, ItemExists
    }
    function DirectoryExists(const DirectoryName: string): Boolean;

    { Description:
        This function returns True if a file with the given name exists in the
        directory listing.
      See also:
        DirectoryExists, ItemExists
    }
    function FileExists(const FileName: string): Boolean;

    { Description:
        This function returns True if an item with the given name exists in the
        directory listing. This item can be either a file or a directory. If
        you need to specifically check for a file or directory, use
        DirectoryExists or FileExists instead.
      See also:
        FileExists, DirectoryExists
    }
    function ItemExists(const Name: string): Boolean;

    // <COMBINE DirectoriesByName>
    function GetDirectoriesByName(const DirectoryName: string): IFTPDirectory;
    { Description:
        This property will return a directory item based on its name, instead
        of an index. If no directory with the given name exists in the
        directory listing, nil will be returned.
      See also:
        FilesByName, ItemsByName
    }
    property DirectoriesByName[const DirectoryName: string]: IFTPDirectory
      read GetDirectoriesByName;

    // <COMBINE FilesByName>
    function GetFilesByName(const FileName: string): IFTPFile;
    { Description:
        This property will return a file item based on its name, instead
        of an index. If no file with the given name exists in the
        directory listing, nil will be returned.
      See also:
        DirectoriesByName, ItemsByName
    }
    property FilesByName[const DirectoryName: string]: IFTPFile
      read GetFilesByName;

    // <COMBINE ItemsByName>
    function GetItemsByName(const Name: string): IFTPDirectoryItem;
    { Description:
        This property will return an item based on its name, instead
        of an index. If no item with the given name exists in the
        directory listing, nil will be returned.
      See also:
        FilesByName, DirectoriesByName
    }
    property ItemsByName[const DirectoryName: string]: IFTPDirectoryItem
      read GetItemsByName;
  end;

  { Description:
      This is the base interface for downloads. It wraps an object that supports
      the basic methods and properties for downloads through this library.
    See also:
      IInternetUpload, IHTTPDownload
  }
  IInternetDownload = interface
    ['{94E1BB17-7CA2-4AD7-AA98-64BEDCE86183}']

    // <COMBINE Size>
    function GetSize: Int64;
    { Description:
        This property returns the size of the document or file being
        downloaded. If this property returns -1 it means the server didn't
        report the size back to the library. In this case, the application
        should keep reading until Read returns 0 bytes read.
      See also:
        Position
    }
    property Size: Int64 read GetSize;

    // <COMBINE Position>
    function GetPosition: Int64;
    { Description:
        This property returns the current position in the download,
        starting at 0 and increasing for each read that returns data back
        to the application.
      See also:
        Size
    }
    property Position: Int64 read GetPosition;

    { Description:
        This method tries to read data from the document or file and
        store in the buffer provided, up to the number of bytes specified in
        BufferSize. The function will return the number of bytes actually
        read.

        Note: This function will always return at least one byte, except
          when the download has completed, in which case it will return 0.
          You can use this as a way to keep reading until no more data
          available.
      Parameters:
        Buffer - The buffer to store the data into.
        BufferSize - The number of bytes to try to read into the buffer. The
          function might not be able to fulfill this request and will
          return the number of bytes actually read and stored in the
          buffer.
      See also:
        ReadBuffer
    }
    function Read(var Buffer; const BufferSize: Cardinal): Cardinal;

    { Description:
        This method works like Read, except that it won't return the
        number of bytes read. Instead it will raise an exception if it was
        unable to read the number of bytes specified in BufferSize. This
        means that any read that returns less than the number of bytes
        specified will raise an exception. Use this with care.

        Note: If you want to implement a looser error control, use the
          Read method instead.
      Parameters:
        Buffer - The buffer to store the data into.
        BufferSize - The number of bytes to read into the buffer.
      See also:
        Read
    }
    procedure ReadBuffer(var Buffer; const BufferSize: Cardinal);

    { Description:
        This method will close down the download. Subsequent calls to Read
        or ReadBuffer will raise an exception. You can call this method
        if you suspect that the object might not go out of scope immediately,
        and you want to free the document/file on the server up for other
        access, or let the ftp connection get back to an available state.
    }
    procedure Close;

    // <COMBINE EOF>
    function GetEOF: Boolean;
    { Description:
        This property returns True if the download has reached the end of
        the document or file being downloaded. Note that if the Size property
        returns -1, meaning that the server didn't return a size for the
        item being downloaded, then this property will never return True.
    }
    property EOF: Boolean read GetEOF;

    { Description:
        This method will download the rest of the item and store the
        downloaded data in the specified stream.
      Parameters:
        Stream - The stream object to store the downloaded data into.
      See also:
        SaveToFile
    }
    procedure SaveToStream(const Stream: TStream);

    { Description:
        This method will download the rest of the item and store the
        downloaded data in a file with the specified filename.
      Parameters:
        FileName - The name of the file to create and write the downloaded
          data into.
      See also:
        SaveToFile
    }
    procedure SaveToFile(const FileName: string);
  end;

  { Description:
      This interface wraps an object responsible for a http download. It
      inherits from IInternetDownload and adds properties specifically related
      to a http download
    Parameters:
      -
    See also:
      -
  }
  IHTTPDownload = interface(IInternetDownload)
    ['{663D20D6-2EA3-4396-803D-CE5799B34A79}']

    // <COMBINE Headers>
    function GetHeaders(const HeaderType: Cardinal): string;
    { Description:
        This property retrieves one of the HTTP_QUERY_xxx header values
        that were retrieved together with the http download.
    }
    property Headers[const HeaderType: Cardinal]: string
      read GetHeaders; default;

    // <COMBINE ContentLength>
    function GetContentLength: Integer;
    { Description:
        This returns the ContentLength of the document being downloaded.
        Functional, this is the same as IInternetDownload.Size.
      See also:
        IInternetDownload.Size
    }
    property ContentLength: Integer read GetContentLength;

    // <COMBINE ContentType>
    function GetContentType: string;
    { Description:
        This property returns the mime type that describes the content being
        downloaded.
    }
    property ContentType: string read GetContentType;

    // <COMBINE Expires>
    function GetExpires: TDateTime;
    { Description:
        This property returns the date/time that the document being downloaded
        will expire on the server. This date/time value has been adjusted
        to match the local users date/time zone and setting.
    }
    property Expires: TDateTime read GetExpires;
  end;

  { Description:
      This is the base interface for uploads. It wraps an object that supports
      the basic methods and properties for uploads through this library.
    See also:
      IInternetDownload
  }
  IInternetUpload = interface
    ['{F1CC7CBD-4F4D-4A7E-9CE2-3AF243DF3691}']

    // <COMBINE Position>
    function GetPosition: Int64;
    { Description:
        This property returns the current position in the download,
        starting at 0 and increasing for each read that returns data back
        to the application.
    }
    property Position: Int64 read GetPosition;

    { Description:
        This method tries to write data to the document or file from
        the buffer provided, up to the number of bytes specified in
        BufferSize. The function will return the number of bytes actually
        written.
      Parameters:
        Buffer - The buffer to write the data from.
        BufferSize - The number of bytes to try to write from the buffer. The
          function might not be able to fulfill this request and will
          return the number of bytes actually written from the buffer.
      See also:
        WriteBuffer
    }
    function Write(const Buffer; const BufferSize: Cardinal): Cardinal;

    { Description:
        This method works like Write, except that it won't return the
        number of bytes written. Instead it will raise an exception if it was
        unable to write the number of bytes specified in BufferSize. This
        means that any write that returns less than the number of bytes
        specified will raise an exception. Use this with care.

        Note: If you want to implement a looser error control, use the
          Write method instead.
      Parameters:
        Buffer - The buffer to write the data from.
        BufferSize - The number of bytes to write from the buffer.
      See also:
        Write
    }
    procedure WriteBuffer(const Buffer; const BufferSize: Cardinal);

    { Description:
        This method will close down the upload. Subsequent calls to Write
        or WriteBuffer will raise an exception. You can call this method
        if you suspect that the object might not go out of scope immediately,
        and you want to free the document/file on the server up for other
        access, or let the ftp connection get back to an available state.
    }
    procedure Close;
  end;

  { Description:
      This object inherits from TStream and implements a simple stream-based
      wrapper for an IInternetUpload interface. This means you can use
      all the SaveToStream methods that the VCL is full of to save directly
      to a document/file over the internet without saving it to disk first.

      Basically this object works like a TStream, except that calls to Read
      or ReadBuffer will raise an exception.

      Note: If the download has no size, such as a http download missing the
        ContentLength header value, any LoadFromStream methods might not
        work since they tend to use the size of the stream as a guideline
        for how much data to read.
  }
  TInternetUploadStream = class(TStream)
  private
    FInternetUpload   : IInternetUpload;
    FCurrentPosition  : Int64;

    function GetPackageVersion: TPackageVersion;

  public
    constructor Create(const InternetUpload: IInternetUpload);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    {$IFDEF DELPHI6UP}
    function Seek(const Offset: Int64;
      Origin: TSeekOrigin): Int64; overload; override;
    {$ENDIF}
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion;
  end;

  { Description:
      This object inherits from TStream and implements a simple stream-based
      wrapper for an IInternetDownload interface. This means you can use
      all the LoadFromStream methods that the VCL is full of to load
      data directly from a document/file over the internet without
      saving it to disk first.

      Basically this object works like a TStream, except that calls to Write
      or WriteBuffer will raise an exception.
    See also:
      TInternetUploadStream
  }
  TInternetDownloadStream = class(TStream)
  private
    FInternetDownload : IInternetDownload;
    FCurrentPosition  : Int64;

    function GetPackageVersion: TPackageVersion;

  public
    constructor Create(const InternetDownload: IInternetDownload);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    {$IFDEF DELPHI6UP}
    function Seek(const Offset: Int64;
      Origin: TSeekOrigin): Int64; overload; override;
    {$ENDIF}
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion;
  end;

  { Description:
      This type is used when starting a download to specify if the download
      should use ASCII (Text) semantics, or BINARY semantics.
  }
  TTransferType = (
    // Use ASCII Semantics.
    ttAscii,
    // Use BINARY Semantics.
    ttBinary);

  { Description:
      This interface wraps an internet object for communicating with an
      ftp server. Through this interface, you can navigate around the
      ftp site, create and delete directories, rename and delete files,
      and ultimately download or upload files.
    See also:
      IHTTP
  }
  IFTP = interface(ISessionBased)
    ['{ADDB468E-A42F-4A4A-94BA-8BB04D221BEE}']

    // <COMBINE CurrentDirectory>
    function GetCurrentDirectory: string;
    // <COMBINE CurrentDirectory>
    procedure SetCurrentDirectory(const NewValue: string);
    { Description:
        This property returns the current directory that the session is in on
        the server. You can change this to navigate to a different directory.
        If you set this value to a relative directory (one that doesn't
        start with \ or /), then the connection will navigate using
        a relative change from the current directory. Example: The current
        directory is '/pub/' and the program sets the CurrentDirectory property
        to 'test/', the current directory will be changed to '/pub/test/'
        assuming the directory exists.
      See also:
        ChDir
    }
    property CurrentDirectory: string read GetCurrentDirectory
      write SetCurrentDirectory;

    // <COMBINE Passive>
    function GetPassive: Boolean;
    // <COMBINE Passive>
    procedure SetPassive(const NewValue: Boolean);
    { Description:
        This property controls wether a passive or active connection will be
        used. You must set this property before connecting to the server.
    }
    property Passive: Boolean read GetPassive write SetPassive;

    { Description:
        This method is simply a shortcut for CurrentDirectory. Setting
        CurrentDirectory to a value, and calling ChDir with the same
        value will produce the same effect.
      See also:
        CurrentDirectory
    }
    procedure ChDir(const Directory: string);

    { Description:
        This method takes stock of the files and directories located in the
        current directory, and produces an object through a IFTPDirectoryListing
        interface to get access to this list. The Mask parameter controls
        what files/directories to include in the list.
      Parameters:
        DirectoryListing - The object returned that contains the list of
          files and directories.
        Mask - A mask specifying what files and directories to include in the
          list.
      See also:
        IFTPDirectoryListing
    }
    procedure Dir(out DirectoryListing: IFTPDirectoryListing;
      const Mask: string='*');

    { Description:
        This method will delete the file from the ftp server.
      Parameters:
        FileName - The name of the file to delete.
    }
    procedure DeleteFile(const FileName: string);

    { Description:
        This method will create a directory on the ftp server.
      Parameters:
        DirectoryName - The name of the directory to create.
      See also:
        RemoveDirectory
    }
    procedure CreateDirectory(const DirectoryName: string);

    { Description:
        This method will delete a directory on the ftp server. The directory
        must be empty, otherwise it won't be deleted.
      Parameters:
        DirectoryName - The name of the directory to delete.
      See also:
        CreateDirectory
    }
    procedure RemoveDirectory(const DirectoryName: string);

    { Description:
        This method renames a file on the server.
      Parameters:
        OldFilename - The name of the file to rename.
        NewFilename - The new name of the file.
    }
    procedure RenameFile(const OldFilename, NewFilename: string);

    { Description:
        This method will download a file and store it directly to a file
        on disk. DownloadFile will not return before the file is downloaded,
        or an error occurs.
      Parameters:
        RemoteFilename - The name of the file to download from the ftp
          server.
        LocalFilename - The name to use when saving the file to disk, must
          include a valid pathname and filename.
        TransferType - The transfer semantics to use when downloading the
          file.
        ForceReload - Set this to True to reload the file from the server,
          regardless of wether a cached copy exists or not. Leave it at False
          to use the cached copy, if a cached copy is available.
        LocalAttributes - The new file attributes to assign to the new file
          locally.
      See also:
        OpenDownload
    }
    procedure DownloadFile(const RemoteFilename, LocalFilename: string;
      const TransferType: TTransferType=ttBinary;
      const ForceReload: Boolean=False;
      const LocalAttributes: Cardinal=0);

    { Description:
        This method starts an ftp download and returns an object
        through a IInternetDownload interface back to the program. This object
        can be used to download the file. This method will return as soon as
        the request has been sent and the download has started. Releasing the
        download object will stop the download.
      Parameters:
        RemoteFilename - The name of the file to download from the ftp server.
        TransferType - The transfer semantics to use when downloading the
          file.
        ForceReload - Set this to True to reload the file from the server,
          regardless of wether a cached copy exists or not. Leave it at False
          to use the cached copy, if a cached copy is available.
      See also:
        DownloadFile
    }
    function OpenDownload(const RemoteFilename: string;
      const TransferType: TTransferType=ttBinary;
      const ForceReload: Boolean=False): IInternetDownload;

    { Description:
        This method starts and ftp upload and returns an object
        through a IInternetUpload interface back to the program. This object
        can be used to upload the file. This method will return as soon as
        the request has been sent and the upload has started. Releasing the
        upload object will stop the upload.
      Parameters:
        RemoteFilename - The name to store the file in on the ftp server.
        TransferType - The transfer semantics to use when uploading the
          file.
      See also:
        -
    }
    function OpenUpload(const RemoteFilename: string;
      const TransferType: TTransferType=ttBinary): IInternetUpload;

    { Description:
        This method will upload a file to the ftp server, directly from a file
        on disk. This method will not return before the file has been
        completely uploaded, or an error has occured.
      Parameters:
        LocalFilename - The name and path of the file to upload from the
          local disk.
        RemoteFilename - The name to store the file in on the ftp server.
        TransferType - The transfer semantics to use when uploading the
          file.
      See also:
        OpenUpload
    }
    procedure UploadFile(const LocalFilename, RemoteFilename: string;
      const TransferType: TTransferType=ttBinary);

    { Description:
        This method will execute the command on the ftp server. This can be used
        to execute commands other than the ones directly supported through the
        IFTP interface.
      Parameters:
        Command - The command to execute.
    }
    procedure Execute(const Command: string);
  end;

  { Description:
      This type enumerates the different options that can be used for a http
      request to control the request operation.
    See also:
      THTTPRequestOptions
  }
  THTTPRequestOption = (
    // Include this option to have the session keep the connection open after
    // doing each request, to save on time used to connect to the server for
    // each request.
    roKeepConnection,
    // Automatically redirect if the server lets us know that we should.
    // Without this option, we'll get the redirection response instead of the
    // document it redirects to.
    roAutoRedirect,
    // Include this option to save any downloaded document to the cache
    // afterwards.
    roSaveToCache,
    // Include this option to send existing and save retrieved cookies for
    // each request.
    roSendReceiveCookies,
    // If the user has specified that a dialog should ask for permission before
    // allowing cookies to be used, this option will show such a permission
    // dialog. Without this option, a request for such a dialog will deny use
    // of cookies instead.
    roCookieDialog,
    // Include this option to use a cached copy of the document, if available.
    roLoadFromCache,
    // Allow HTTPS semantics.
    roSecure);

  { Description:
      This set can contain one or more of the option values enumerated in
      THTTPRequestOption.
    See also:
      THTTPRequestOption
  }
  THTTPRequestOptions = set of THTTPRequestOption;

  { Description:
      This interface encapsulates an object responsible for connections made to
      a web server.

      Operations possible are:
        * HEAD - Learning information about a document.
        * GET - Retrieve a document through the GET method.
        * POST - Post some data to the server and retrieve the response.
        * PUT - Upload a new or changed item to the web server.
  }
  IHTTP = interface(ISessionBased)
    ['{68E516E5-359A-4263-938F-2E0D82FD272B}']

    // <COMBINE HTTPVersion>
    function GetHTTPVersion: string;
    // <COMBINE HTTPVersion>
    procedure SetHTTPVersion(const NewValue: string);
    { Description:
        When the request is sent to the web server, it is tagged as being of
        a specific http version. The current version is HTTP/1.1, but the
        classes in this unit defaults to HTTP/1.0.
    }
    property HTTPVersion: string read GetHTTPVersion write SetHTTPVersion;

    // <COMBINE Accepts>
    function GetAccepts: TStrings;
    { Description:
        This property holds a list of acceptable mime types. When a request
        is sent to the web server, the web server can use this to prepare
        a response that matches an acceptable type. For instance, if a request
        for a html document is made, and html is not among the acceptable
        mime types, but plain text is, the web server can strip out the html
        tags and return the plain text back to the client.

        The list can contain a list of valid mime types, like this:

          text/plain
          text/html
          text/*
          image/gif
          image/jpeg
          image/*
          */*

        The text/* mime type simply means any text type is acceptable.
    }
    property Accepts: TStrings read GetAccepts;

    { Description:
        This method will query the web server for the information stored about
        the document. Functionality-wise, it will be the same as a GET just
        without the actual content of the document.
      Parameters:
        DocumentName - The name that the document has on the server. This
          name can also contain parameters to send to a cgi application.
        Headers - A list of header parameters. Each of the parameters must
          be of the form "Name: Value".
        Referer - The url the program "came from" when it encountered the url
          for this document.
        Options - Set of options to use for controlling the request.
      Returns:
        An IHTTPDownload object with information about the document.
    }
    function HEAD(const DocumentName: string;
      const Headers: array of string;
      const Referer: string='';
      const Options: THTTPRequestOptions=[]): IHTTPDownload; overload;

    { Description:
        This method will open the document on the web server and start
        a download of the document.
      Parameters:
        DocumentName - The name that the document has on the server. This
          name can also contain parameters to send to a cgi application.
        Headers - A list of header parameters. Each of the parameters must
          be of the form "Name: Value".
        Referer - The url the program "came from" when it encountered the url
          for this document.
        Options - Set of options to use for controlling the request.
      Returns:
        An IHTTPDownload object with the content of the document, plus the
          information about the document.
    }
    function GET(const DocumentName: string;
      const Headers: array of string;
      const Referer: string='';
      const Options: THTTPRequestOptions=[]): IHTTPDownload; overload;

    { Description:
        This method will download the document from the web server and
        save it to disk in a file with the specified filename.
      Parameters:
        DocumentName - The name that the document has on the server. This
          name can also contain parameters to send to a cgi application.
        DestinationFileName - A valid path and filename of the file to
          save the document content to.
        Headers - A list of header parameters. Each of the parameters must
          be of the form "Name: Value".
        Referer - The url the program "came from" when it encountered the url
          for this document.
        Options - Set of options to use for controlling the request.
    }
    procedure GET(const DocumentName, DestinationFileName: string;
      const Headers: array of string;
      const Referer: string='';
      const Options: THTTPRequestOptions=[]); overload;

    { Description:
        This method will download the document from the web server and
        save it to the specified stream object.
      Parameters:
        DocumentName - The name that the document has on the server. This
          name can also contain parameters to send to a cgi application.
        DestinationStream - The stream object to save the downloaded document
          to.
        Headers - A list of header parameters. Each of the parameters must
          be of the form "Name: Value".
        Referer - The url the program "came from" when it encountered the url
          for this document.
        Options - Set of options to use for controlling the request.
    }
    procedure GET(const DocumentName: string; const DestinationStream: TStream;
      const Headers: array of string;
      const Referer: string='';
      const Options: THTTPRequestOptions=[]); overload;

    { Description:
        This method will open the document on the web server and start
        a download of the document.
      Parameters:
        DocumentName - The name that the document has on the server. This
          name can also contain parameters to send to a cgi application.
        Referer - The url the program "came from" when it encountered the url
          for this document.
        Options - Set of options to use for controlling the request.
      Returns:
        An IHTTPDownload object with the content of the document, plus the
          information about the document.
    }
    function GET(const DocumentName: string;
      const Referer: string='';
      const Options: THTTPRequestOptions=[]): IHTTPDownload; overload;

    { Description:
        This method will open the document on the web server and start
        a download of the document. The method will also send along the
        PostData content with the request, and use a POST request type
        instead of a GET type.
      Parameters:
        PostData - The data to send along with the request.
        DocumentName - The name that the document has on the server. This
          name can also contain parameters to send to a cgi application.
        Headers - A list of header parameters. Each of the parameters must
          be of the form "Name: Value".
        Referer - The url the program "came from" when it encountered the url
          for this document.
        Options - Set of options to use for controlling the request.
      Returns:
        An IHTTPDownload object with the content of the document, plus the
          information about the document.
    }
    function POST(const PostData, DocumentName: string;
      const Headers: array of string;
      const Referer: string='';
      const Options: THTTPRequestOptions=[]): IHTTPDownload; overload;

    { Description:
        This method will open the document on the web server and save the
        document to a file on disk. The method will also send along the
        PostData content with the request, and use a POST request type
        instead of a GET type.
      Parameters:
        PostData - The data to send along with the request.
        DocumentName - The name that the document has on the server. This
          name can also contain parameters to send to a cgi application.
        DestinationFileName - A valid path and filename of the file to
          save the document content to.
        Headers - A list of header parameters. Each of the parameters must
          be of the form "Name: Value".
        Referer - The url the program "came from" when it encountered the url
          for this document.
        Options - Set of options to use for controlling the request.
    }
    function POST(const PostData, DocumentName, DestinationFileName: string;
      const Headers: array of string;
      const Referer: string='';
      const Options: THTTPRequestOptions=[]): IHTTPDownload; overload;

    { Description:
        This method will open the document on the web server and save the
        document to a stream object. The method will also send along the
        PostData content with the request, and use a POST request type
        instead of a GET type.
      Parameters:
        PostData - The data to send along with the request.
        DocumentName - The name that the document has on the server. This
          name can also contain parameters to send to a cgi application.
        DestinationStream - The stream object to save the downloaded document
          to.
        Headers - A list of header parameters. Each of the parameters must
          be of the form "Name: Value".
        Referer - The url the program "came from" when it encountered the url
          for this document.
        Options - Set of options to use for controlling the request.
    }
    procedure POST(const PostData, DocumentName: string;
      const DestinationStream: TStream; const Headers: array of string;
      const Referer: string='';
      const Options: THTTPRequestOptions=[]); overload;

    { Description:
        This method will open the document on the web server and save the
        document to a stream object. The method will also send along the
        PostData content with the request, and use a POST request type
        instead of a GET type.
      Parameters:
        PostData - The data to send along with the request, stored as a
          stream object.
        DocumentName - The name that the document has on the server. This
          name can also contain parameters to send to a cgi application.
        DestinationStream - The stream object to save the downloaded document
          to.
        Headers - A list of header parameters. Each of the parameters must
          be of the form "Name: Value".
        Referer - The url the program "came from" when it encountered the url
          for this document.
        Options - Set of options to use for controlling the request.
    }
    procedure POST(const PostData: TStream; const DocumentName: string;
      const DestinationStream: TStream; const Headers: array of string;
      const Referer: string='';
      const Options: THTTPRequestOptions=[]); overload;

    { Description:
        This method will upload a document to the web server.
      Parameters:
        SourceFileName - A valid path and filename of the file to upload.
        DocumentName - The name that the document has on the server. This
          name can also contain parameters to send to a cgi application.
        Headers - A list of header parameters. Each of the parameters must
          be of the form "Name: Value".
        Referer - The url the program "came from" when it encountered the url
          for this document.
        Options - Set of options to use for controlling the request.
    }
    procedure PUT(const SourceFileName, DocumentName: string;
      const Headers: array of string;
      const Referer: string='';
      const Options: THTTPRequestOptions=[]); overload;

    { Description:
        This method will upload a document to the web server.
      Parameters:
        SourceStream - A stream object containing the content of the
          document to upload.
        DocumentName - The name that the document has on the server. This
          name can also contain parameters to send to a cgi application.
        Headers - A list of header parameters. Each of the parameters must
          be of the form "Name: Value".
        Referer - The url the program "came from" when it encountered the url
          for this document.
        Options - Set of options to use for controlling the request.
    }
    procedure PUT(const SourceStream: TStream; const DocumentName: string;
      const Headers: array of string;
      const Referer: string='';
      const Options: THTTPRequestOptions=[]); overload;
  end;

const
  iSourceURLName    = 0;
  iLocalFileName    = 1;
  iFileExtension    = 2;
  iHeaderInfo       = 3;

  iUseCount         = 0;
  iHitRate          = 1;

  iLastModifiedTime = 0;
  iLastAccessTime   = 1;
  iLastSyncTime     = 2;
  iExpireTime       = 3;

type
  IInternetExplorerCacheItem = interface
    ['{EBB64F31-6992-4017-AF50-862A17F8359A}']

    function GetSourceURLName: string;
    property SourceURLName: string read GetSourceURLName;

    function GetLocalFileName: string;
    property LocalFileName: string read GetLocalFileName;

    function GetFileExtension: string;
    property FileExtension: string read GetFileExtension;

    function GetHeaderInfo: string;
    property HeaderInfo: string read GetHeaderInfo;

    function GetUseCount: Cardinal;
    property UseCount: Cardinal read GetUseCount;

    function GetHitRate: Cardinal;
    property HitRate: Cardinal read GetHitRate;

    function GetSize: Int64;
    property Size: Int64 read GetSize;

    function GetLastModifiedTime: TDateTime;
    property LastModifiedTime: TDateTime read GetLastModifiedTime;

    function GetLastAccessTime: TDateTime;
    property LastAccessTime: TDateTime read GetLastAccessTime;

    function GetLastSyncTime: TDateTime;
    property LastSyncTime: TDateTime read GetLastSyncTime;

    function GetExpireTime: TDateTime;
    property ExpireTime: TDateTime read GetExpireTime;
  end;

  IInternetExplorerCacheItems = interface
    ['{B5C42DE5-AA82-4846-82FB-59029E8C9CFD}']

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItems(const Index: Integer): IInternetExplorerCacheItem;
    property Items[const Index: Integer]: IInternetExplorerCacheItem read GetItems; default;
  end;

  IInternetExplorerCache = interface
    ['{0B891F58-B310-40BC-BF17-DF3E8A6E01FC}']

    function GetItems: IInternetExplorerCacheItems; overload;
    function GetItems(const RegExp: string; const Options: TlvkRegExpOptions=[roGreedy, roBol, roEol]): IInternetExplorerCacheItems; overload;
    function GetItems(const RegExp: IRegExp): IInternetExplorerCacheItems; overload;
  end;

  { Description:
      This exception is the base class for the exceptions related to the
      lvkInternet classes in this unit.
  }
  ElvkInternet = class(Exception);

  { Description:
      This is the exception class used for exceptions raised from the
      session object.
  }
  ElvkInternetSession = class(ElvkInternet);

  { Description:
      This is the exception class used for exceptions raised from the ftp
      object.
  }
  ElvkFTP = class(ElvkInternet);

  { Description:
      This is the exception class that forms a basis for the download-related
      exception classes.
  }
  ElvkInternetDownload = class(ElvkInternet);

  { Description:
      This is the exception class that is used for exceptions raised from
      reading from a download object.
  }
  ElvkInternetDownloadRead = class(ElvkInternetDownload);

  { Description:
      This is the exception class that is used for exceptions raised from
      writing to a download object (which is not allowed due to the nature of
      the object).
  }
  ElvkInternetDownloadWrite = class(ElvkInternetDownload);

  { Description:
      This is the exception class that forms a basis for the upload-related
      exception classes.
  }
  ElvkInternetUpload = class(ElvkInternet);

  { Description:
      This is the exception class that is used for exceptions raised from
      writing to the upload object.
  }
  ElvkInternetUploadWrite = class(ElvkInternetUpload);

  { Description:
      This is the exception class that is used for exceptions raised from
      reading from the upload object (which is not allowed).
  }
  ElvkInternetUploadRead = class(ElvkInternetUpload);

  { Description:
      This exception class is used for exceptions that are raised from the
      WinInet library.
  }
  EWinInet = class(Exception)
  private
    FErrorCode    : Cardinal;
    FIdentifier   : string;
    FDescription  : string;
    FOrigin       : string;

  public
    constructor Create(const ErrorCode: Cardinal; const Origin: string);

    { Description:
        This property returns the error code returnd from the WinInet
        library.
    }
    property ErrorCode: Cardinal read FErrorCode;

    { Description:
        This property holds the textual identifier of the error.
    }
    property Identifier: string read FIdentifier;

    { Description:
        This is a textual description of the exception.
    }
    property Description: string read FDescription;

    { Description:
        This is the method that the exception originated in.
    }
    property Origin: string read FOrigin;
  end;

  { Description:
      This exception class is used for exceptions raised due to a
      erroneous http response code.
  }
  ElvkHTTPResponse = class(ElvkInternet)
  private
    FStatusCode : Cardinal;
    FIdentifier : string;
    FStatusText : string;

  public
    constructor Create(const StatusCode: Cardinal; const Origin: string);

    { Description:
        This property returns the statuscode returned from the server.
    }
    property StatusCode: Cardinal read FStatusCode;

    { Description:
        This property returns the description of the status code.
    }
    property StatusText: string read FStatusText;
  end;

{ Description:
    This function returns a fully configured internet session object.
  Parameters:
    AutoConnect - If set to True, during the call to NewInternetSession the
      session will be connected.
    ProxyType - The type of proxy, if any, to use.
    UserAgent - The user agent identifier to use. If left blank, a default
      value will be used.
    Offline - Set to True to force all downloads to load from cache instead
      of downloading from server.
}
function NewInternetSession(const AutoConnect: Boolean=True;
  const ProxyType: TInternetProxyType=ptPreconfigured;
  const UserAgent: string='';
  const Offline: Boolean=False): IInternetSession;

{ Description:
    This function returns a fully configured object through the IFTP interface.
  Parameters:
    HostName - The hostname of the ftp server to connect to.
    Port - The port of the ftp server, usually 21.
    Username - The username to use when authenticating against the server.
    Password - The password to use when authenticating against the server.
    Passive - If set to True, use passive transfers.
    AutoConnect - Set to True to have NewFTP connect to the server before
      returning.
    Session - The session to use. Leave as nil to create a dummy session for
      this ftp connection.
}
function NewFTP(const HostName: string; const Port: Integer;
  const Username: string=''; const Password: string='';
  const Passive: Boolean=False;
  const AutoConnect: Boolean=True;
  const Session: IInternetSession=nil): IFTP; overload;

{ Description:
    This function returns a fully configured object through the IFTP interface,
    except it uses a ftp url instead of individual parameters.
  Parameters:
    URL - The URL to connect to. This is on the form
      ftp://username:password@hostname:port
    Passive - If set to True, use passive transfers.
    AutoConnect - Set to True to have NewFTP connect to the server before
      returning.
    Session - The session to use. Leave as nil to create a dummy session for
      this ftp connection.
}
function NewFTP(const URL: String;
  const Passive: Boolean=False;
  const AutoConnect: Boolean=True;
  const Session: IInternetSession=nil): IFTP; overload;

{ Description:
    This function returns a fully configured object through the IHTTP interface.
  Parameters:
    HostName - The hostname of the web server to connect to.
    Port - The port of the ftp server, usually 80.
    Username - The username to use when authenticating against the server.
    Password - The password to use when authenticating against the server.
    AutoConnect - Set to True to have NewHTTP connect to the server before
      returning.
    Session - The session to use. Leave as nil to create a dummy session for
      this ftp connection.
}
function NewHTTP(const HostName: string; const Port: Integer;
  const Username: string=''; const Password: string='';
  const AutoConnect: Boolean=True;
  const Session: IInternetSession=nil): IHTTP; overload;

{ Description:
    This function returns a fully configured object through the IHTTP interface,
    except it uses a ftp url instead of individual parameters.
  Parameters:
    URL - The URL to connect to. This is on the form
      http://username:password@hostname:port
    AutoConnect - Set to True to have NewHTTP connect to the server before
      returning.
    Session - The session to use. Leave as nil to create a dummy session for
      this ftp connection.
}
function NewHTTP(const URL: string;
  const AutoConnect: Boolean=True;
  const Session: IInternetSession=nil): IHTTP; overload;

{ Description:
    This function will append a trailing / character if there is none.
}
function InternetIncludeTrailingPathDelimiter(const PathName: string): string;

{ TODO 2 -oLVK -cDocumentation : Document NewInternetExplorerCache }
function NewInternetExplorerCache: IInternetExplorerCache;

implementation

uses
  {$IFDEF DELPHI6}
  DateUtils,
  {$ENDIF}
  Windows, WinSock, lvkAdvancedFileStreams;

const
  FTPTransferTypes  : array[TTransferType] of Cardinal = (
    FTP_TRANSFER_TYPE_ASCII,
    FTP_TRANSFER_TYPE_BINARY
  );

  HTTP_ACCEPTABLE_STATUS_CODES  = [100, 101, 200, 201, 202, 203, 204, 205, 206];

const
  CONTENT_TYPE_URL_ENCODED  : array[0..0] of string = (
    'Content-Type: application/x-www-form-urlencoded'
  );

type
  {$IFNDEF DELPHI6UP}
  PCardinal = ^Cardinal;
  {$ENDIF}

  IInternetObject = interface
    ['{DB37CDD0-92D0-4928-8DF5-E6FD9596DE37}']

    function GetHandle: HINTERNET;
    property Handle: HINTERNET read GetHandle;
  end;

  TInternetAuthentication = class(TInterfacedObject, IInternetAuthentication,
    IPackageVersion)
  private
    FUsername : string;
    FPassword : string;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IInternetAuthentication interface
    function GetUsername: string;
    procedure SetUsername(const NewValue: string);
    property Username: string read GetUsername write SetUsername;

    function GetPassword: string;
    procedure SetPassword(const NewValue: string);
    property Password: string read GetPassword write SetPassword;
  end;

  IInternetProxyInternal = interface
    ['{1A3A22C6-D440-4084-9114-866EF9213EF9}']

    procedure Configure(const SessionHandle: HINTERNET);
  end;

  TInternetProxy = class(TInterfacedObject, IInternetProxy,
    IInternetProxyInternal, IPackageVersion)
  private
    FAuthentication : IInternetAuthentication;
    FProxyType      : TInternetProxyType;
    FHostName       : string;
    FBypass         : string;
    FPort           : Word;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IInternetProxy interface
    function GetAuthentication: IInternetAuthentication;
    function GetProxyType: TInternetProxyType;
    procedure SetProxyType(const NewValue: TInternetProxyType);
    function GetHostName: string;
    procedure SetHostName(const NewValue: string);
    function GetPort: Word;
    procedure SetPort(const NewValue: Word);
    function GetBypass: string;
    procedure SetBypass(const NewValue: string);

    // IInternetProxyInternal interface
    procedure Configure(const SessionHandle: HINTERNET);

  public
    constructor Create;
  end;

  IInternetSessionInternal = interface
    ['{4835E720-5463-481D-B6BC-A6042277EBB8}']

    function GetContext: Cardinal;
    property Context: Cardinal read GetContext;
  end;

  TInternetSession = class(TInterfacedObject, IInternetSession,
    IInternetObject, IInternetSessionInternal, IThreadCooperate,
    IPackageVersion)
  private
    FConnection   : HINTERNET;
    FOffline      : Boolean;
    FUserAgent    : string;
    FProxy        : IInternetProxy;
    FOnCallback   : TInternetCallback;

    procedure DoConnect;
    procedure DoDisconnect;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IThreadCooperate
    procedure Terminate;

    // IInternetObject interface
    function GetHandle: HINTERNET;
    property Handle: HINTERNET read GetHandle;

    // IInternetSession interface
    procedure Connect;
    procedure Disconnect;
    function GetConnected: Boolean;
    procedure SetConnected(const NewValue: Boolean);
    property Connected: Boolean read GetConnected write SetConnected;
    function GetOffline: Boolean;
    procedure SetOffline(const NewValue: Boolean);
    property Offline: Boolean read GetOffline write SetOffline;
    function GetUserAgent: string;
    procedure SetUserAgent(const NewValue: string);
    property UserAgent: string read GetUserAgent write SetUserAgent;
    function GetProxy: IInternetProxy;
    function GetOnCallback: TInternetCallback;
    procedure SetOnCallback(const NewValue: TInternetCallback);
    property OnCallback: TInternetCallback read GetOnCallback
      write SetOnCallback;

    // IInternetSessionInternal interface
    function GetContext: Cardinal;

  public
    constructor Create;
    destructor Destroy; override;
  end;

  TSessionBased = class(TInterfacedObject, ISessionBased, IInternetObject,
    IThreadCooperate, IPackageVersion)
  private
    FSession        : IInternetSession;
    FHandle         : HINTERNET;
    FAuthentication : IInternetAuthentication;
    FHostName       : string;
    FPort           : Word;

    procedure DoConnect;
    procedure DoDisconnect;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IThreadCooperate interface
    procedure Terminate;

    // ISessionBased interface
    function GetSession: IInternetSession;
    procedure SetSession(const NewValue: IInternetSession);
    property Session: IInternetSession read GetSession write SetSession;
    function GetHostName: string;
    procedure SetHostName(const NewValue: string);
    property HostName: string read GetHostName write SetHostName;
    function GetPort: Word;
    procedure SetPort(const NewValue: Word);
    property Port: Word read GetPort write SetPort;
    function GetAuthentication: IInternetAuthentication;
    property Authentication: IInternetAuthentication read GetAuthentication;
    procedure Connect;
    procedure Disconnect;
    function GetConnected: Boolean;
    procedure SetConnected(const NewValue: Boolean);
    property Connected: Boolean read GetConnected write SetConnected;

    // IInternetObject interface
    function GetHandle: HINTERNET;
    property Handle: HINTERNET read GetHandle;

    // Internal
    function ConnectToServer(
      const Session: IInternetSession): HINTERNET; virtual; 
    procedure DisconnectFromServer; virtual;

  public
    constructor Create;
  end;

  TInternetDownload = class(TInterfacedObject, IInternetObject,
    IInternetDownload, IPackageVersion)
  private
    FHandle   : HINTERNET;
    FSource   : IInternetObject;
    FPosition : Int64;
    FSize     : Int64;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IInternetObject interface
    function GetHandle: HINTERNET;

    // IInternetDownload interface
    function GetSize: Int64;
    function GetPosition: Int64;
    function Read(var Buffer; const BufferSize: Cardinal): Cardinal;
    procedure ReadBuffer(var Buffer; const BufferSize: Cardinal);
    procedure Close;
    function GetEOF: Boolean;
    procedure SaveToStream(const Stream: TStream);
    procedure SaveToFile(const FileName: string);

  public
    constructor Create(const Source: IInternetObject; const Handle: HINTERNET);
    destructor Destroy; override;
  end;

  THTTPDownload = class(TInternetDownload, IHTTPDownload, IPackageVersion)
  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IHTTPDownload interface
    function GetHeaders(const HeaderType: Cardinal): string;
    function GetContentLength: Integer;
    function GetContentType: string;
    function GetExpires: TDateTime;
  end;

  TInternetUpload = class(TInterfacedObject, IInternetObject,
    IInternetUpload, IPackageVersion)
  private
    FHandle   : HINTERNET;
    FSource   : IInternetObject;
    FPosition : Int64;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IInternetObject interface
    function GetHandle: HINTERNET;

    // IInternetUpload interface
    function GetPosition: Int64;
    function Write(const Buffer; const BufferSize: Cardinal): Cardinal;
    procedure WriteBuffer(const Buffer; const BufferSize: Cardinal);
    procedure Close;

  public
    constructor Create(const Source: IInternetObject; const Handle: HINTERNET);
    destructor Destroy; override;
  end;

  TFTP = class(TSessionBased, IFTP, IPackageVersion)
  private
    FPassive  : Boolean;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IFTP interface
    function GetCurrentDirectory: string;
    procedure SetCurrentDirectory(const NewValue: string);
    property CurrentDirectory: string read GetCurrentDirectory
      write SetCurrentDirectory;
    function GetPassive: Boolean;
    procedure SetPassive(const NewValue: Boolean);
    property Passive: Boolean read GetPassive write SetPassive;
    procedure ChDir(const Directory: string);
    procedure Dir(out DirectoryListing: IFTPDirectoryListing;
      const Mask: string='*');
    procedure DeleteFile(const FileName: string);
    procedure Execute(const Command: string);
    procedure DownloadFile(const RemoteFilename, LocalFilename: string;
      const TransferType: TTransferType=ttBinary;
      const ForceReload: Boolean=False; const LocalAttributes: Cardinal=0);
    function OpenDownload(const RemoteFilename: string;
      const TransferType: TTransferType=ttBinary;
      const ForceReload: Boolean=False): IInternetDownload;
    function OpenUpload(const RemoteFilename: string;
      const TransferType: TTransferType=ttBinary): IInternetUpload;
    procedure UploadFile(const LocalFilename, RemoteFilename: string;
      const TransferType: TTransferType=ttBinary);
    procedure CreateDirectory(const DirectoryName: string);
    procedure RemoveDirectory(const DirectoryName: string);
    procedure RenameFile(const OldFilename, NewFilename: string);

    // Internal
    function ConnectToServer(
      const Session: IInternetSession): HINTERNET; override;

  public
    constructor Create;
  end;

  IFTPDirectoryListingInternal = interface
    ['{596596ED-DB91-4538-9AE2-F6E09D0C5A0D}']

    procedure Add(const FindFileData: TWin32FindData);
  end;

  TFTPDirectoryItem = class(TInterfacedObject, IFTPDirectoryItem,
    IPackageVersion)
  private
    FName           : string;
    FAlternateName  : string;
    FCreationTime   : TDateTime;
    FLastAccessTime : TDateTime;
    FLastWriteTime  : TDateTime;
    FAttributes     : Cardinal;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IFTPDirectoryItem interface
    function GetName: string;
    function GetAlternateName: string;
    function GetCreationTime: TDateTime;
    function GetLastAccessTime: TDateTime;
    function GetLastWriteTime: TDateTime;
    function GetAttributes: Cardinal;

  public
    constructor Create(const FindFileData: TWin32FindData);
  end;

  TFTPDirectory = class(TFTPDirectoryItem, IFTPDirectory);

  TFTPFile = class(TFTPDirectoryItem, IFTPFile, IPackageVersion)
  private
    FSize : Int64;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IFTPFile interface
    function GetSize: Int64;

  public
    constructor Create(const FindFileData: TWin32FindData);
  end;

  TFTPDirectoryListing = class(TInterfacedObject, IFTPDirectoryListing,
    IFTPDirectoryListingInternal, IPackageVersion)
  private
    FFTP          : IFTP;
    FDirectories  : IInterfaceList;
    FFiles        : IInterfaceList;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IFTPDirectoryListing interface
    function GetDirectoryCount: Integer;
    function GetDirectories(const Index: Integer): IFTPDirectory;
    function GetFileCount: Integer;
    function GetFiles(const Index: Integer): IFTPFile;
    function GetItemCount: Integer;
    function GetItems(const Index: Integer): IFTPDirectoryItem;
    function DirectoryExists(const DirectoryName: string): Boolean;
    function FileExists(const FileName: string): Boolean;
    function ItemExists(const Name: string): Boolean;
    function GetDirectoriesByName(const DirectoryName: string): IFTPDirectory;
    function GetFilesByName(const FileName: string): IFTPFile;
    function GetItemsByName(const Name: string): IFTPDirectoryItem;

    // IFTPDirectoryListingInternal interface
    procedure Add(const FindFileData: TWin32FindData);

  public
    constructor Create(const FTP: IFTP);
  end;

  THTTP = class(TSessionBased, IHTTP, IPackageVersion)
  private
    FHTTPVersion  : string;
    FAccepts      : TStrings;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IHTTP interface
    function GetHTTPVersion: string;
    procedure SetHTTPVersion(const NewValue: string);
    function GetAccepts: TStrings;
    function HEAD(const DocumentName: string;
      const Headers: array of string;
      const Referer: string='';
      const Options: THTTPRequestOptions=[]): IHTTPDownload; overload;
    function GET(const DocumentName: string;
      const Headers: array of string;
      const Referer: string='';
      const Options: THTTPRequestOptions=[]): IHTTPDownload; overload;
    procedure GET(const DocumentName, DestinationFileName: string;
      const Headers: array of string;
      const Referer: string='';
      const Options: THTTPRequestOptions=[]); overload;
    procedure GET(const DocumentName: string; const DestinationStream: TStream;
      const Headers: array of string;
      const Referer: string='';
      const Options: THTTPRequestOptions=[]); overload;
    function GET(const DocumentName: string;
      const Referer: string;
      const Options: THTTPRequestOptions): IHTTPDownload; overload;
    function POST(const PostData, DocumentName: string;
      const Headers: array of string;
      const Referer: string='';
      const Options: THTTPRequestOptions=[]): IHTTPDownload; overload;
    procedure POST(const PostData, DocumentName: string;
      const DestinationStream: TStream; const Headers: array of string;
      const Referer: string='';
      const Options: THTTPRequestOptions=[]); overload;
    procedure POST(const PostData: TStream; const DocumentName: string;
      const DestinationStream: TStream; const Headers: array of string;
      const Referer: string='';
      const Options: THTTPRequestOptions=[]); overload;
    function POST(const PostData, DocumentName, DestinationFileName: string;
      const Headers: array of string;
      const Referer: string='';
      const Options: THTTPRequestOptions=[]): IHTTPDownload; overload;
    procedure PUT(const SourceFileName, DocumentName: string;
      const Headers: array of string;
      const Referer: string='';
      const Options: THTTPRequestOptions=[]); overload;
    procedure PUT(const SourceStream: TStream; const DocumentName: string;
      const Headers: array of string;
      const Referer: string='';
      const Options: THTTPRequestOptions=[]); overload;

    // Internal
    function ConnectToServer(
      const Session: IInternetSession): HINTERNET; override;
    function Request(const Verb, DocumentName, Referer: string;
      const Headers: array of string;
      const Options: THTTPRequestOptions;
      const PostData: PChar; const PostLength: Integer): IHTTPDownload;

  public
    constructor Create;
    destructor Destroy; override;
  end;

  TInternetExplorerCacheItem = class(TInterfacedObject, IInternetExplorerCacheItem)
  private
    FSourceURLName    : string;
    FLocalFileName    : string;
    FUseCount         : Cardinal;
    FHitRate          : Cardinal;
    FSize             : Int64;
    FLastModifiedTime : TDateTime;
    FLastAccessTime   : TDateTime;
    FLastSyncTime     : TDateTime;
    FExpireTime       : TDateTime;
    FHeaderInfo       : string;
    FFileExtension    : string;

  protected
    // IInternetExplorerCacheItem interface
    function GetSourceURLName: string;
    function GetLocalFileName: string;
    function GetFileExtension: string;
    function GetHeaderInfo: string;
    function GetUseCount: Cardinal;
    function GetHitRate: Cardinal;
    function GetSize: Int64;
    function GetLastModifiedTime: TDateTime;
    function GetLastAccessTime: TDateTime;
    function GetLastSyncTime: TDateTime;
    function GetExpireTime: TDateTime;

  public
    constructor Create(const EntryInfo: TInternetCacheEntryInfo);
  end;

  TInternetExplorerCacheItems = class(TInterfacedObject, IInternetExplorerCacheItems)
  private
    FItems    : IInterfaceList;

    procedure SortItems;

  protected
    // IInternetExplorerCacheItems interface
    function GetCount: Integer;
    function GetItems(const Index: Integer): IInternetExplorerCacheItem;

  public
    constructor Create(const RegExp: string; const Options: TlvkRegExpOptions); overload;
    constructor Create(const RegExp: IRegExp); overload;
    constructor Create; overload;
  end;

  TInternetExplorerCache = class(TInterfacedObject, IInternetExplorerCache)
  protected
    // IInternetExplorerCache interface
    function GetItems: IInternetExplorerCacheItems; overload;
    function GetItems(const RegExp: string; const Options: TlvkRegExpOptions): IInternetExplorerCacheItems; overload;
    function GetItems(const RegExp: IRegExp): IInternetExplorerCacheItems; overload;
  end;

function NewInternetExplorerCache: IInternetExplorerCache;
begin
  Result := TInternetExplorerCache.Create as IInternetExplorerCache;
end;

function InternetIncludeTrailingPathDelimiter(const PathName: string): string;
begin
  if Copy(PathName, Length(PathName), 1) = '/' then
    Result := PathName
  else
    Result := PathName + '/';
end;

{$IFNDEF DELPHI6UP}
function FtpGetFileSize(hFile: HINTERNET;
  lpdwFileSizeHigh: LPDWORD): DWORD; stdcall;
external 'WININET.DLL'
{$ENDIF}

procedure RaiseLastWinInetError(const MethodName: string);
var
  ErrorCode : Cardinal;

  function GetCallerAddress: Pointer;
  asm
    mov   eax, [ebp]
  end;

begin
  ErrorCode := GetLastError;
  if ErrorCode <> ERROR_SUCCESS then
  begin
    if (ErrorCode >= 12001) and (ErrorCode <= 12156) then
      raise EWinInet.Create(ErrorCode, MethodName) at GetCallerAddress
    else
      RaiseLastWin32Error;
  end;
end;

function NewInternetSession(const AutoConnect: Boolean;
  const ProxyType: TInternetProxyType;
  const UserAgent: string;
  const Offline: Boolean): IInternetSession;
begin
  Result := TInternetSession.Create;

  Result.Proxy.ProxyType := ProxyType;
  Result.Offline := Offline;
  if UserAgent <> '' then
    Result.UserAgent := UserAgent;

  if AutoConnect then
    Result.Connect;
end;

function NewFTP(const HostName: string;
  const Port: Integer; const Username: string; const Password: string;
  const Passive, AutoConnect: Boolean;
  const Session: IInternetSession): IFTP; overload;
begin
  Result := TFTP.Create;

  Result.HostName := HostName;
  Result.Port := Port;
  Result.Authentication.Username := Username;
  Result.Authentication.Password := Password;
  Result.Passive := Passive;
  Result.Session := Session;

  if AutoConnect then
    Result.Connect;
end;

function NewFTP(const URL: String;
  const Passive: Boolean;
  const AutoConnect: Boolean;
  const Session: IInternetSession): IFTP; overload;
var
	Components  : TURLComponents;

	sScheme     : array[0..2047] of Char;
	sHostName   : array[0..2047] of Char;
	sUserName   : array[0..2047] of Char;
	sPassword   : array[0..2047] of Char;
	sUrlPath    : array[0..2047] of Char;
	sExtraInfo  : array[0..2047] of Char;
begin
	with Components do
	begin
		dwStructSize := SizeOf(Components);
		lpszScheme := sScheme;
		dwSchemeLength := SizeOf(sScheme);
		lpszHostName := sHostName;
		dwHostNameLength := SizeOf(sHostName);
		lpszUserName := sUserName;
		dwUserNameLength := SizeOf(sUserName);
		lpszPassword := sPassword;
		dwPasswordLength := SizeOf(sPassword);
		lpszUrlPath := sUrlPath;
		dwUrlPathLength := SizeOf(sUrlPath);
		lpszExtraInfo := sExtraInfo;
		dwExtraInfoLength := SizeOf(sExtraInfo);
	end;

	if not InternetCrackUrl(PChar(URL), Length(URL), 0, Components) then
    RaiseLastWinInetError('NewFTP');

  if Components.nScheme <> INTERNET_SERVICE_FTP then
    raise ElvkFTP.Create('Invalid scheme for ftp download');

  Result := NewFTP(sHostName, Components.nPort, sUsername, sPassword, Passive,
    AutoConnect, Session);
end;

function NewHTTP(const HostName: string;
  const Port: Integer; const Username: string; const Password: string;
  const AutoConnect: Boolean;
  const Session: IInternetSession): IHTTP; overload;
begin
  Result := THTTP.Create;

  Result.HostName := HostName;
  Result.Port := Port;
  Result.Authentication.Username := Username;
  Result.Authentication.Password := Password;
  Result.Session := Session;

  if AutoConnect then
    Result.Connect;
end;

function NewHTTP(const URL: string;
  const AutoConnect: Boolean;
  const Session: IInternetSession): IHTTP; overload;
var
	Components  : TURLComponents;

	sScheme     : array[0..2047] of Char;
	sHostName   : array[0..2047] of Char;
	sUserName   : array[0..2047] of Char;
	sPassword   : array[0..2047] of Char;
	sUrlPath    : array[0..2047] of Char;
	sExtraInfo  : array[0..2047] of Char;
begin
	with Components do
	begin
		dwStructSize := SizeOf(Components);
		lpszScheme := sScheme;
		dwSchemeLength := SizeOf(sScheme);
		lpszHostName := sHostName;
		dwHostNameLength := SizeOf(sHostName);
		lpszUserName := sUserName;
		dwUserNameLength := SizeOf(sUserName);
		lpszPassword := sPassword;
		dwPasswordLength := SizeOf(sPassword);
		lpszUrlPath := sUrlPath;
		dwUrlPathLength := SizeOf(sUrlPath);
		lpszExtraInfo := sExtraInfo;
		dwExtraInfoLength := SizeOf(sExtraInfo);
	end;

	if not InternetCrackUrl(PChar(URL), Length(URL), 0, Components) then
    RaiseLastWinInetError('NewFTP');

  if Components.nScheme <> INTERNET_SERVICE_HTTP then
    raise ElvkFTP.Create('Invalid scheme for http connection');

  Result := NewHTTP(sHostName, Components.nPort, sUsername, sPassword, 
    AutoConnect, Session);
end;

procedure InternetStatusCallback(InternetHandle: HINTERNET;
  dwContext: PCardinal; dwInternetStatus: Cardinal;
  lpvStatusInformation: Pointer; dwStatusInformationLength: Cardinal); stdcall;
var
  Session : TInternetSession;
begin
  if not Assigned(dwContext) then
    Exit;
  Session := TInternetSession(dwContext);
  Assert(Assigned(Session) and (Session.ClassName = 'TInternetSession'));

  if Assigned(Session.FOnCallback) then
    Session.FOnCallback(Session, dwInternetStatus, lpvStatusInformation,
      dwStatusInformationLength);
end;

{ TInternetSession }

procedure TInternetSession.Connect;
begin
  Connected := True;
end;

constructor TInternetSession.Create;
begin
  inherited;

  FUserAgent := ClassName;
  FProxy := TInternetProxy.Create;
end;

destructor TInternetSession.Destroy;
begin
  Disconnect;

  inherited;
end;

procedure TInternetSession.Disconnect;
begin
  Connected := False;
end;

procedure TInternetSession.DoConnect;
const
  SessionTypes : array[TInternetProxyType] of Cardinal = (
    INTERNET_OPEN_TYPE_DIRECT,
    INTERNET_OPEN_TYPE_PRECONFIG,
    INTERNET_OPEN_TYPE_PRECONFIG_WITH_NO_AUTOPROXY,
    INTERNET_OPEN_TYPE_PROXY
  );
const
  OfflineFlag : array[Boolean] of Cardinal = (0, INTERNET_FLAG_FROM_CACHE);
begin
  FConnection := InternetOpen(
    PChar(FUserAgent),
    SessionTypes[FProxy.ProxyType],
    PChar(FProxy.HostName),
    PChar(FProxy.Bypass),
    OfflineFlag[FOffline]);
  if not Assigned(FConnection) then
    RaiseLastWinInetError('TInternetSession.DoConnect');
  InternetSetStatusCallback(FConnection, @InternetStatusCallback);

  (FProxy as IInternetProxyInternal).Configure(FConnection);
end;

procedure TInternetSession.DoDisconnect;
begin
  InternetSetStatusCallback(FConnection, nil);
  InternetCloseHandle(FConnection);
end;

function TInternetSession.GetConnected: Boolean;
begin
  Result := Assigned(FConnection);
end;

function TInternetSession.GetContext: Cardinal;
begin
  Result := Cardinal(Self);
end;

function TInternetSession.GetHandle: HINTERNET;
begin
  Result := FConnection;
end;

function TInternetSession.GetOffline: Boolean;
begin
  Result := FOffline;
end;

function TInternetSession.GetOnCallback: TInternetCallback;
begin
  Result := FOnCallback;
end;

function TInternetSession.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TInternetSession.GetProxy: IInternetProxy;
begin
  Result := FProxy;
end;

function TInternetSession.GetUserAgent: string;
begin
  Result := FUserAgent;
end;

procedure TInternetSession.SetConnected(const NewValue: Boolean);
begin
  if NewValue <> GetConnected then
  begin
    if NewValue then
      DoConnect
    else
      DoDisconnect;
  end;
end;

procedure TInternetSession.SetOffline(const NewValue: Boolean);
begin
  if NewValue <> FOffline then
  begin
    if Connected then
      raise ElvkInternetSession.Create('Unable to change Offline setting ' +
        'while session is connected');
    FOffline := NewValue;
  end;
end;

procedure TInternetSession.SetOnCallback(
  const NewValue: TInternetCallback);
begin
  FOnCallback := NewValue;
end;

procedure TInternetSession.SetUserAgent(const NewValue: string);
begin
  if NewValue <> FUserAgent then
  begin
    if Connected then
      raise ElvkInternetSession.Create('Unable to change UserAgent setting ' +
        'while session is connected');
    FUserAgent := NewValue;
  end;
end;

procedure TInternetSession.Terminate;
begin
  InternetCloseHandle(FConnection);
end;

{ TInternetAuthentication }

function TInternetAuthentication.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TInternetAuthentication.GetPassword: string;
begin
  Result := FPassword;
end;

function TInternetAuthentication.GetUsername: string;
begin
  Result := FUsername;
end;

procedure TInternetAuthentication.SetPassword(const NewValue: string);
begin
  FPassword := NewValue;
end;

procedure TInternetAuthentication.SetUsername(const NewValue: string);
begin
  FUsername := NewValue;
end;

{ TInternetProxy }

procedure TInternetProxy.Configure(const SessionHandle: HINTERNET);
begin
  if FProxyType <> ptNone then
  begin
    if FAuthentication.Username <> '' then
      if not InternetSetOption(SessionHandle, INTERNET_OPTION_PROXY_USERNAME,
        PChar(FAuthentication.Username), Length(FAuthentication.Username)) then
        RaiseLastWinInetError('TInternetProxy.Configure');
    if FAuthentication.Password <> '' then
      if not InternetSetOption(SessionHandle, INTERNET_OPTION_PROXY_PASSWORD,
        PChar(FAuthentication.Username), Length(FAuthentication.Password)) then
        RaiseLastWinInetError('TInternetProxy.Configure');
  end;
end;

constructor TInternetProxy.Create;
begin
  inherited;
  FAuthentication := TInternetAuthentication.Create;
end;

function TInternetProxy.GetAuthentication: IInternetAuthentication;
begin
  Result := FAuthentication;
end;

function TInternetProxy.GetBypass: string;
begin
  Result := FBypass;
end;

function TInternetProxy.GetHostName: string;
begin
  Result := FHostName;
end;

function TInternetProxy.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TInternetProxy.GetPort: Word;
begin
  Result := FPort;
end;

function TInternetProxy.GetProxyType: TInternetProxyType;
begin
  Result := FProxyType;
end;

procedure TInternetProxy.SetBypass(const NewValue: string);
begin
  FBypass := NewValue;
end;

procedure TInternetProxy.SetHostName(const NewValue: string);
begin
  FHostName := NewValue;
end;

procedure TInternetProxy.SetPort(const NewValue: Word);
begin
  FPort := NewValue;
end;

procedure TInternetProxy.SetProxyType(const NewValue: TInternetProxyType);
begin
  FProxyType := NewValue;
end;

{ TSessionBased }

procedure TSessionBased.Connect;
begin
  Connected := True;
end;

function TSessionBased.ConnectToServer(
  const Session: IInternetSession): HINTERNET;
begin
  // Dummy
  Result := nil;
end;

constructor TSessionBased.Create;
begin
  inherited;

  FHostName := 'localhost';
  FAuthentication := TInternetAuthentication.Create;
  FSession := NewInternetSession;
end;

procedure TSessionBased.Disconnect;
begin
  Connected := False;
end;

procedure TSessionBased.DisconnectFromServer;
begin
  // Do nothing by default
end;

procedure TSessionBased.DoConnect;
begin
  FHandle := ConnectToServer(FSession);
end;

procedure TSessionBased.DoDisconnect;
begin
  DisconnectFromServer;
  InternetCloseHandle(FHandle);
  FHandle := nil;
end;

function TSessionBased.GetAuthentication: IInternetAuthentication;
begin
  Result := FAuthentication;
end;

function TSessionBased.GetConnected: Boolean;
begin
  Result := Assigned(FHandle);
end;

function TSessionBased.GetHandle: HINTERNET;
begin
  Result := FHandle;
end;

function TSessionBased.GetHostName: string;
begin
  Result := FHostName;
end;

function TSessionBased.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TSessionBased.GetPort: Word;
begin
  Result := FPort;
end;

function TSessionBased.GetSession: IInternetSession;
begin
  Result := FSession;
end;

procedure TSessionBased.SetConnected(const NewValue: Boolean);
begin
  if NewValue <> GetConnected then
  begin
    if NewValue then
      DoConnect
    else
      DoDisconnect;
  end;
end;

procedure TSessionBased.SetHostName(const NewValue: string);
begin
  if NewValue <> FHostName then
  begin
    if Connected then
      raise ElvkInternet.Create('Cannot change HostName property while ' +
        'connected');
    FHostName := NewValue;
  end;
end;

procedure TSessionBased.SetPort(const NewValue: Word);
begin
  if NewValue <> FPort then
  begin
    if Connected then
      raise ElvkInternet.Create('Cannot change HostName property while ' +
        'connected');
    FPort := NewValue;
  end;
end;

procedure TSessionBased.SetSession(const NewValue: IInternetSession);
begin
  if NewValue <> FSession then
  begin
    if Connected then
      raise ElvkInternet.Create('Cannot change Session property while ' +
        'connected');

    if NewValue = nil then
      FSession := NewInternetSession
    else
      FSession := NewValue;
  end;
end;

procedure TSessionBased.Terminate;
begin
  DisconnectFromServer;
  InternetCloseHandle(FHandle);
  FHandle := nil;
end;

{ TFTP }

procedure TFTP.ChDir(const Directory: string);
begin
  Assert(Directory <> '');
  CurrentDirectory := Directory;
end;

function TFTP.ConnectToServer(
  const Session: IInternetSession): HINTERNET;
const
  PassiveFlag : array[Boolean] of Cardinal = (0, INTERNET_FLAG_PASSIVE);
begin
  Result := InternetConnect((Session as IInternetObject).Handle,
    PChar(HostName), Port,
    PChar(Authentication.Username), PChar(Authentication.Password),
    INTERNET_SERVICE_FTP,
    PassiveFlag[FPassive],
    Cardinal((Session as IInternetSessionInternal).Context));

  if not Assigned(Result) then
    RaiseLastWinInetError('TFTP.ConnectToServer');
end;

constructor TFTP.Create;
begin
  inherited;

  Port := INTERNET_DEFAULT_FTP_PORT;
end;

procedure TFTP.CreateDirectory(const DirectoryName: string);
begin
  Assert(DirectoryName <> '');
  if not Connected then
    raise ElvkFTP.Create('Cannot create directory if not connected');

  if not FtpCreateDirectory(Handle, PChar(DirectoryName)) then
    RaiseLastWinInetError('TFTP.CreateDirectory');
end;

procedure TFTP.DeleteFile(const FileName: string);
begin
  Assert(FileName <> '');
  if not Connected then
    raise ElvkFTP.Create('Cannot delete file if not connected');

  if not FtpDeleteFile(Handle, PChar(FileName)) then
    RaiseLastWinInetError('TFTP.DeleteFile');
end;

procedure TFTP.Dir(out DirectoryListing: IFTPDirectoryListing;
  const Mask: string);
var
  FindFileData  : TWin32FindData;
  FindHandle    : HINTERNET;
  Listing       : IFTPDirectoryListingInternal;
begin
  if not Connected then
    raise ElvkFTP.Create('Cannot get directory listing if not connected');

  DirectoryListing := TFTPDirectoryListing.Create(Self);
  Listing := DirectoryListing as IFTPDirectoryListingInternal;
  FindHandle := FtpFindFirstFile(Handle, PChar(Mask), FindFileData,
    INTERNET_FLAG_NEED_FILE or INTERNET_FLAG_HYPERLINK or
    INTERNET_FLAG_RELOAD or INTERNET_FLAG_RESYNCHRONIZE,
    (Session as IInternetSessionInternal).Context);
  if not Assigned(FindHandle) then
    RaiseLastWinInetError('TFTP.Dir');
  try
    repeat
      Listing.Add(FindFileData);
    until not InternetFindNextFile(FindHandle, @FindFileData);
  finally
    InternetCloseHandle(FindHandle);
  end;
end;

procedure TFTP.DownloadFile(const RemoteFilename, LocalFilename: string;
  const TransferType: TTransferType; const ForceReload: Boolean;
  const LocalAttributes: Cardinal);
const
  ReloadFlag  : array[Boolean] of Cardinal = (0, INTERNET_FLAG_RELOAD);
begin
  Assert(RemoteFilename <> '');
  Assert(LocalFilename <> '');
  if not Connected then
    raise ElvkFTP.Create('Cannot download file if not connected');

  if not FtpGetFile(Handle, PChar(RemoteFilename), PChar(LocalFilename), False,
    LocalAttributes, FTPTransferTypes[TransferType] or ReloadFlag[ForceReload],
    (Session as IInternetSessionInternal).Context) then
    RaiseLastWinInetError('TFTP.DownloadFile');
end;

procedure TFTP.Execute(const Command: string);
begin
  Assert(Trim(Command) <> '');
  if not Connected then
    raise ElvkFTP.Create('Cannot execute ftp command if not connected');

  FtpCommand(Handle, True, FTP_TRANSFER_TYPE_ASCII, PChar(Command),
    (Session as IInternetSessionInternal).Context);
end;

function TFTP.GetCurrentDirectory: string;
var
  CurrentDirectory  : array[0..MAX_PATH] of Char;
  Size              : Cardinal;
begin
  if not Connected then
    raise ElvkFTP.Create('Cannot get current directory if not connected');

  Size := SizeOf(CurrentDirectory);
  if not FtpGetCurrentDirectory(Handle, CurrentDirectory, Size) then
    RaiseLastWinInetError('GetCurrentDirectory');

  Result := CurrentDirectory;
end;

function TFTP.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TFTP.GetPassive: Boolean;
begin
  Result := FPassive;
end;

function TFTP.OpenDownload(
  const RemoteFilename: string; const TransferType: TTransferType;
  const ForceReload: Boolean): IInternetDownload;
var
  FileHandle  : HINTERNET;
const
  ReloadFlag  : array[Boolean] of Cardinal = (0, INTERNET_FLAG_RELOAD);
begin
  Assert(RemoteFilename <> '');
  if not Connected then
    raise ElvkFTP.Create('Cannot download file if not connected');

  FileHandle := FtpOpenFile(Handle, PChar(RemoteFilename), GENERIC_READ,
    FTPTransferTypes[TransferType] or ReloadFlag[ForceReload],
    (Session as IInternetSessionInternal).Context);
  if Assigned(FileHandle) then
    Result := TInternetDownload.Create(Self, FileHandle)
  else
    RaiseLastWinInetError('TFTP.OpenDownload');
end;

function TFTP.OpenUpload(
  const RemoteFilename: string;
  const TransferType: TTransferType): IInternetUpload;
var
  FileHandle  : HINTERNET;
begin
  Assert(RemoteFilename <> '');
  if not Connected then
    raise ElvkFTP.Create('Cannot upload file if not connected');

  FileHandle := FtpOpenFile(Handle, PChar(RemoteFilename), GENERIC_WRITE,
    FTPTransferTypes[TransferType],
    (Session as IInternetSessionInternal).Context);
  if Assigned(FileHandle) then
    Result := TInternetUpload.Create(Self, FileHandle)
  else
    RaiseLastWinInetError('TFTP.OpenUpload');
end;

procedure TFTP.RemoveDirectory(const DirectoryName: string);
begin
  Assert(DirectoryName <> '');
  if not Connected then
    raise ElvkFTP.Create('Cannot remove a directory if not connected');

  if not FtpRemoveDirectory(Handle, PChar(DirectoryName)) then
    RaiseLastWinInetError('TFTP.RemoveDirectory');
end;

procedure TFTP.RenameFile(const OldFilename, NewFilename: string);
begin
  Assert(OldFilename <> '');
  Assert(NewFilename <> '');
  if not Connected then
    raise ElvkFTP.Create('Cannot rename a file if not connected');

  if not FtpRenameFile(Handle, PChar(OldFilename), PChar(NewFilename)) then
    RaiseLastWinInetError('TFTP.RemoveDirectory');
end;

procedure TFTP.SetCurrentDirectory(const NewValue: string);
begin
  if not Connected then
    raise ElvkFTP.Create('Cannot set current directory if not connected');

  if not FtpSetCurrentDirectory(Handle, PChar(NewValue)) then
    RaiseLastWinInetError('SetCurrentDirectory');
end;

procedure TFTP.SetPassive(const NewValue: Boolean);
begin
  if NewValue <> FPassive then
  begin
    if Connected then
      raise ElvkInternet.Create('Cannot change Passive property while ' +
        'connected');
    FPassive := NewValue;
  end;
end;

procedure TFTP.UploadFile(const LocalFilename, RemoteFilename: string;
  const TransferType: TTransferType);
begin
  Assert(LocalFilename <> '');
  Assert(RemoteFilename <> '');
  if not Connected then
    raise ElvkFTP.Create('Cannot download file if not connected');

  if not FtpPutFile(Handle, PChar(LocalFilename), PChar(RemoteFilename),
    FTPTransferTypes[Transfertype],
    (Session as IInternetSessionInternal).Context) then
    RaiseLastWinInetError('TFTP.UploadFile');
end;

{ EWinInet }

constructor EWinInet.Create(const ErrorCode: Cardinal; const Origin: string);
var
  ErrorMessage  : array[0..999] of Char;
  NewError      : Cardinal;
  Size          : Cardinal;
  Index         : Integer;
type
  TWinInetError = record
    ErrorCode   : Cardinal;
    ID          : string;
    Description : string;
  end;
const
  ErrorCodes  : array[1..85] of TWinInetError = (
    (ErrorCode    : ERROR_INTERNET_OUT_OF_HANDLES;
     ID           : 'ERROR_INTERNET_OUT_OF_HANDLES';
     Description  : 'No more handles could be generated at this time.'),
    (ErrorCode    : ERROR_INTERNET_NAME_NOT_RESOLVED;
     ID           : 'ERROR_INTERNET_NAME_NOT_RESOLVED';
     Description  : 'The server name could not be resolved.'),
    (ErrorCode    : ERROR_INTERNET_OUT_OF_HANDLES;
     ID           : 'ERROR_INTERNET_OUT_OF_HANDLES';
     Description  : 'No more handles could be generated at this time.'),
    (ErrorCode    : ERROR_INTERNET_TIMEOUT;
     ID           : 'ERROR_INTERNET_TIMEOUT';
     Description  : 'The request has timed out.'),
    (ErrorCode    : ERROR_INTERNET_EXTENDED_ERROR;
     ID           : 'ERROR_INTERNET_EXTENDED_ERROR';
     Description  : 'An extended error was returned from the server. This ' +
                    'is typically a string or buffer containing a verbose ' +
                    'error message. Call InternetGetLastResponseInfo to ' +
                    'retrieve the error text.'),
    (ErrorCode    : ERROR_INTERNET_INTERNAL_ERROR;
     ID           : 'ERROR_INTERNET_INTERNAL_ERROR';
     Description  : 'An internal error has occurred.'),
    (ErrorCode    : ERROR_INTERNET_INVALID_URL;
     ID           : 'ERROR_INTERNET_INVALID_URL';
     Description  : 'The URL is invalid.'),
    (ErrorCode    : ERROR_INTERNET_UNRECOGNIZED_SCHEME;
     ID           : 'ERROR_INTERNET_UNRECOGNIZED_SCHEME';
     Description  : 'The URL scheme could not be recognized or is not ' +
                    'supported.'),
    (ErrorCode    : ERROR_INTERNET_PROTOCOL_NOT_FOUND;
     ID           : 'ERROR_INTERNET_PROTOCOL_NOT_FOUND';
     Description  : 'The server name could not be resolved.'),
    (ErrorCode    : ERROR_INTERNET_INVALID_OPTION;
     ID           : 'ERROR_INTERNET_INVALID_OPTION';
     Description  : 'The requested protocol could not be located.'),
    (ErrorCode    : ERROR_INTERNET_BAD_OPTION_LENGTH;
     ID           : 'ERROR_INTERNET_BAD_OPTION_LENGTH';
     Description  : 'The length of an option supplied to InternetQueryOption ' +
                    'or InternetSetOption is incorrect for the type of ' +
                    'option specified.'),
    (ErrorCode    : ERROR_INTERNET_OPTION_NOT_SETTABLE;
     ID           : 'ERROR_INTERNET_OPTION_NOT_SETTABLE';
     Description  : 'The request option cannot be set, only queried.'),
    (ErrorCode    : ERROR_INTERNET_SHUTDOWN;
     ID           : 'ERROR_INTERNET_SHUTDOWN';
     Description  : 'The Win32 Internet function support is being shut down ' +
                    'or unloaded.'),
    (ErrorCode    : ERROR_INTERNET_INCORRECT_USER_NAME;
     ID           : 'ERROR_INTERNET_INCORRECT_USER_NAME';
     Description  : 'The request to connect and log on to an FTP server ' +
                    'could not be completed because the supplied user name ' +
                    'is incorrect.'),
    (ErrorCode    : ERROR_INTERNET_INCORRECT_PASSWORD;
     ID           : 'ERROR_INTERNET_INCORRECT_PASSWORD';
     Description  : 'The request to connect and log on to an FTP server ' +
                    'could not be completed because the supplied password is ' +
                    'incorrect.'),
    (ErrorCode    : ERROR_INTERNET_LOGIN_FAILURE;
     ID           : 'ERROR_INTERNET_LOGIN_FAILURE';
     Description  : 'The request to connect to and log on to an FTP server ' +
                    'failed.'),
    (ErrorCode    : ERROR_INTERNET_INVALID_OPERATION;
     ID           : 'ERROR_INTERNET_INVALID_OPERATION';
     Description  : 'The requested operation is invalid.'),
    (ErrorCode    : ERROR_INTERNET_OPERATION_CANCELLED;
     ID           : 'ERROR_INTERNET_OPERATION_CANCELLED';
     Description  : 'The operation was canceled, usually because the handle ' +
                    'on which the request was operating was closed before ' +
                    'the operation completed'),
    (ErrorCode    : ERROR_INTERNET_INCORRECT_HANDLE_TYPE;
     ID           : 'ERROR_INTERNET_INCORRECT_HANDLE_TYPE';
     Description  : 'The type of handle supplied is incorrect for this ' +
                    'operation.'),
    (ErrorCode    : ERROR_INTERNET_INCORRECT_HANDLE_STATE;
     ID           : 'ERROR_INTERNET_INCORRECT_HANDLE_STATE';
     Description  : 'The requested operation cannot be carried out because ' +
                    'the handle supplied is not in the correct state.'),
    (ErrorCode    : ERROR_INTERNET_NOT_PROXY_REQUEST;
     ID           : 'ERROR_INTERNET_NOT_PROXY_REQUEST';
     Description  : 'The request cannot be made via a proxy.'),
    (ErrorCode    : ERROR_INTERNET_REGISTRY_VALUE_NOT_FOUND;
     ID           : 'ERROR_INTERNET_REGISTRY_VALUE_NOT_FOUND';
     Description  : 'A required registry value could not be located.'),
    (ErrorCode    : ERROR_INTERNET_BAD_REGISTRY_PARAMETER;
     ID           : 'ERROR_INTERNET_BAD_REGISTRY_PARAMETER';
     Description  : 'A required registry value was located but is an ' +
                    'incorrect type or has an invalid value'),
    (ErrorCode    : ERROR_INTERNET_NO_DIRECT_ACCESS;
     ID           : 'ERROR_INTERNET_NO_DIRECT_ACCESS';
     Description  : 'Direct network access cannot be made at this time.'),
    (ErrorCode    : ERROR_INTERNET_NO_CONTEXT;
     ID           : 'ERROR_INTERNET_NO_CONTEXT';
     Description  : 'An asynchronous request could not be made because a ' +
                    'zero context value was supplied'),
    (ErrorCode    : ERROR_INTERNET_NO_CALLBACK;
     ID           : 'ERROR_INTERNET_NO_CALLBACK';
     Description  : 'An asynchronous request could not be made because a ' +
                    'callback function has not been set.'),
    (ErrorCode    : ERROR_INTERNET_REQUEST_PENDING;
     ID           : 'ERROR_INTERNET_REQUEST_PENDING';
     Description  : 'The required operation could not be completed because ' +
                    'one or more requests are pending'),
    (ErrorCode    : ERROR_INTERNET_INCORRECT_FORMAT;
     ID           : 'ERROR_INTERNET_INCORRECT_FORMAT';
     Description  : 'The format of the request is invalid.'),
    (ErrorCode    : ERROR_INTERNET_ITEM_NOT_FOUND;
     ID           : 'ERROR_INTERNET_ITEM_NOT_FOUND';
     Description  : 'The requested item could not be located.'),
    (ErrorCode    : ERROR_INTERNET_CANNOT_CONNECT;
     ID           : 'ERROR_INTERNET_CANNOT_CONNECT';
     Description  : 'The attempt to connect to the server failed.'),
    (ErrorCode    : ERROR_INTERNET_CONNECTION_ABORTED;
     ID           : 'ERROR_INTERNET_CONNECTION_ABORTED';
     Description  : 'The connection with the server has been terminated.'),
    (ErrorCode    : ERROR_INTERNET_CONNECTION_RESET;
     ID           : 'ERROR_INTERNET_CONNECTION_RESET';
     Description  : 'The connection with the server has been reset.'),
    (ErrorCode    : ERROR_INTERNET_FORCE_RETRY;
     ID           : 'ERROR_INTERNET_FORCE_RETRY';
     Description  : 'Calls for the Win32 Internet function to redo the ' +
                    'request.'),
    (ErrorCode    : ERROR_INTERNET_INVALID_PROXY_REQUEST;
     ID           : 'ERROR_INTERNET_INVALID_PROXY_REQUEST';
     Description  : 'The request to the proxy was invalid.'),
    (ErrorCode    : ERROR_INTERNET_HANDLE_EXISTS;
     ID           : 'ERROR_INTERNET_HANDLE_EXISTS';
     Description  : 'The request failed because the handle already exists.'),
    (ErrorCode    : ERROR_INTERNET_SEC_CERT_DATE_INVALID;
     ID           : 'ERROR_INTERNET_SEC_CERT_DATE_INVALID';
     Description  : 'SSL certificate date that was received from the server ' +
                    'is bad. The certificate is expired.'),
    (ErrorCode    : ERROR_INTERNET_SEC_CERT_CN_INVALID;
     ID           : 'ERROR_INTERNET_SEC_CERT_CN_INVALID';
     Description  : 'SSL certificate common name (host name field) is ' +
                    'incorrect—for example, if you entered www.server.com ' +
                    'and the common name on the certificate says ' +
                    'www.different.com.'),
    (ErrorCode    : ERROR_INTERNET_HTTP_TO_HTTPS_ON_REDIR;
     ID           : 'ERROR_INTERNET_HTTP_TO_HTTPS_ON_REDIR';
     Description  : 'The application is moving from a non-SSL to an SSL ' +
                    'connection because of a redirect.'),
    (ErrorCode    : ERROR_INTERNET_HTTPS_TO_HTTP_ON_REDIR;
     ID           : 'ERROR_INTERNET_HTTPS_TO_HTTP_ON_REDIR';
     Description  : 'The application is moving from an SSL to an non-SSL ' +
                    'connection because of a redirect.'),
    (ErrorCode    : ERROR_INTERNET_MIXED_SECURITY;
     ID           : 'ERROR_INTERNET_MIXED_SECURITY';
     Description  : 'The content is not entirely secure. Some of the ' +
                    'content being viewed may have come from unsecured ' +
                    'servers.'),
    (ErrorCode    : ERROR_INTERNET_CHG_POST_IS_NON_SECURE;
     ID           : 'ERROR_INTERNET_CHG_POST_IS_NON_SECURE';
     Description  : 'The application is posting and attempting to change ' +
                    'multiple lines of text on a server that is not secure.'),
    (ErrorCode    : ERROR_INTERNET_POST_IS_NON_SECURE;
     ID           : 'ERROR_INTERNET_POST_IS_NON_SECURE';
     Description  : 'The application is posting data to a server that is ' +
                    'not secure.'),
    (ErrorCode    : ERROR_INTERNET_CLIENT_AUTH_CERT_NEEDED;
     ID           : 'ERROR_INTERNET_CLIENT_AUTH_CERT_NEEDED';
     Description  : 'The server is requesting client authentication.'),
    (ErrorCode    : ERROR_INTERNET_INVALID_CA;
     ID           : 'ERROR_INTERNET_INVALID_CA';
     Description  : 'The function is unfamiliar with the Certificate ' +
                    'Authority that generated the server''s certificate.'),
    (ErrorCode    : ERROR_INTERNET_CLIENT_AUTH_NOT_SETUP;
     ID           : 'ERROR_INTERNET_CLIENT_AUTH_NOT_SETUP';
     Description  : 'Client authorization is not set up on this computer.'),
    (ErrorCode    : ERROR_INTERNET_ASYNC_THREAD_FAILED;
     ID           : 'ERROR_INTERNET_ASYNC_THREAD_FAILED';
     Description  : 'The application could not start an asynchronous thread.'),
    (ErrorCode    : ERROR_INTERNET_REDIRECT_SCHEME_CHANGE;
     ID           : 'ERROR_INTERNET_REDIRECT_SCHEME_CHANGE';
     Description  : 'The function could not handle the redirection, because ' +
                    'the scheme changed (for example, HTTP to FTP).'),
    (ErrorCode    : ERROR_INTERNET_DIALOG_PENDING;
     ID           : 'ERROR_INTERNET_DIALOG_PENDING';
     Description  : 'Another thread has a password dialog box in progress.'),
    (ErrorCode    : ERROR_INTERNET_RETRY_DIALOG;
     ID           : 'ERROR_INTERNET_RETRY_DIALOG';
     Description  : 'The dialog box should be retried.'),
    (ErrorCode    : ERROR_INTERNET_HTTPS_HTTP_SUBMIT_REDIR;
     ID           : 'ERROR_INTERNET_HTTPS_HTTP_SUBMIT_REDIR';
     Description  : 'The data being submitted to an SSL connection is ' +
                    'being redirected to a non-SSL connection.'),
    (ErrorCode    : ERROR_INTERNET_INSERT_CDROM;
     ID           : 'ERROR_INTERNET_INSERT_CDROM';
     Description  : 'The request requires a CD-ROM to be inserted in the ' +
                    'CD-ROM drive to locate the resource requested.'),
    (ErrorCode    : ERROR_FTP_TRANSFER_IN_PROGRESS;
     ID           : 'ERROR_FTP_TRANSFER_IN_PROGRESS';
     Description  : 'The requested operation cannot be made on the FTP ' +
                    'session handle because an operation is already in ' +
                    'progress.'),
    (ErrorCode    : ERROR_FTP_DROPPED;
     ID           : 'ERROR_FTP_DROPPED';
     Description  : 'The FTP operation was not completed because the session ' +
                    'was aborted.'),
    (ErrorCode    : ERROR_FTP_NO_PASSIVE_MODE;
     ID           : 'ERROR_FTP_NO_PASSIVE_MODE';
     Description  : 'Passive mode is not available on the server.'),
    (ErrorCode    : ERROR_GOPHER_PROTOCOL_ERROR;
     ID           : 'ERROR_GOPHER_PROTOCOL_ERROR';
     Description  : 'An error was detected while parsing data returned from ' +
                    'the Gopher server.'),
    (ErrorCode    : ERROR_GOPHER_NOT_FILE;
     ID           : 'ERROR_GOPHER_NOT_FILE';
     Description  : 'The request must be made for a file locator.'),
    (ErrorCode    : ERROR_GOPHER_DATA_ERROR;
     ID           : 'ERROR_GOPHER_DATA_ERROR';
     Description  : 'An error was detected while receiving data from the ' +
                    'Gopher server.'),
    (ErrorCode    : ERROR_GOPHER_END_OF_DATA;
     ID           : 'ERROR_GOPHER_END_OF_DATA';
     Description  : 'The end of the data has been reached.'),
    (ErrorCode    : ERROR_GOPHER_INVALID_LOCATOR;
     ID           : 'ERROR_GOPHER_INVALID_LOCATOR';
     Description  : 'The supplied locator is not valid.'),
    (ErrorCode    : ERROR_GOPHER_INCORRECT_LOCATOR_TYPE;
     ID           : 'ERROR_GOPHER_INCORRECT_LOCATOR_TYPE';
     Description  : 'The type of the locator is not correct for this ' +
                    'operation.'),
    (ErrorCode    : ERROR_GOPHER_NOT_GOPHER_PLUS;
     ID           : 'ERROR_GOPHER_NOT_GOPHER_PLUS';
     Description  : 'The requested operation can be made only against a ' +
                    'Gopher+ server, or with a locator that specifies a ' +
                    'Gopher+ operation.'),
    (ErrorCode    : ERROR_GOPHER_ATTRIBUTE_NOT_FOUND;
     ID           : 'ERROR_GOPHER_ATTRIBUTE_NOT_FOUND';
     Description  : 'The requested attribute could not be located.'),
    (ErrorCode    : ERROR_GOPHER_UNKNOWN_LOCATOR;
     ID           : 'ERROR_GOPHER_UNKNOWN_LOCATOR';
     Description  : 'The locator type is unknown.'),
    (ErrorCode    : ERROR_HTTP_HEADER_NOT_FOUND;
     ID           : 'ERROR_HTTP_HEADER_NOT_FOUND';
     Description  : 'The requested header could not be located.'),
    (ErrorCode    : ERROR_HTTP_DOWNLEVEL_SERVER;
     ID           : 'ERROR_HTTP_DOWNLEVEL_SERVER';
     Description  : 'The server did not return any headers.'),
    (ErrorCode    : ERROR_HTTP_INVALID_SERVER_RESPONSE;
     ID           : 'ERROR_HTTP_INVALID_SERVER_RESPONSE';
     Description  : 'The server response could not be parsed.'),
    (ErrorCode    : ERROR_HTTP_INVALID_HEADER;
     ID           : 'ERROR_HTTP_INVALID_HEADER';
     Description  : 'The supplied header is invalid.'),
    (ErrorCode    : ERROR_HTTP_INVALID_QUERY_REQUEST;
     ID           : 'ERROR_HTTP_INVALID_QUERY_REQUEST';
     Description  : 'The request made to HttpQueryInfo is invalid.'),
    (ErrorCode    : ERROR_HTTP_HEADER_ALREADY_EXISTS;
     ID           : 'ERROR_HTTP_HEADER_ALREADY_EXISTS';
     Description  : 'The header could not be added because it already exists.'),
    (ErrorCode    : ERROR_HTTP_REDIRECT_FAILED;
     ID           : 'ERROR_HTTP_REDIRECT_FAILED';
     Description  : 'The redirection failed because either the scheme ' +
                    'changed (for example, HTTP to FTP) or all attempts ' +
                    'made to redirect failed (default is five attempts).'),
    (ErrorCode    : ERROR_HTTP_NOT_REDIRECTED;
     ID           : 'ERROR_HTTP_NOT_REDIRECTED';
     Description  : 'The HTTP request was not redirected.'),
    (ErrorCode    : ERROR_HTTP_COOKIE_NEEDS_CONFIRMATION;
     ID           : 'ERROR_HTTP_COOKIE_NEEDS_CONFIRMATION';
     Description  : 'The HTTP cookie requires confirmation.'),
    (ErrorCode    : ERROR_HTTP_COOKIE_DECLINED;
     ID           : 'ERROR_HTTP_COOKIE_DECLINED';
     Description  : 'The HTTP cookie was declined by the server.'),
    (ErrorCode    : ERROR_HTTP_REDIRECT_NEEDS_CONFIRMATION;
     ID           : 'ERROR_HTTP_REDIRECT_NEEDS_CONFIRMATION';
     Description  : 'The redirection requires user confirmation.'),
    (ErrorCode    : ERROR_INTERNET_SECURITY_CHANNEL_ERROR;
     ID           : 'ERROR_INTERNET_SECURITY_CHANNEL_ERROR';
     Description  : 'The application experienced an internal error loading ' +
                    'the SSL libraries.'),
    (ErrorCode    : ERROR_INTERNET_UNABLE_TO_CACHE_FILE;
     ID           : 'ERROR_INTERNET_UNABLE_TO_CACHE_FILE';
     Description  : 'The function was unable to cache the file.'),
    (ErrorCode    : ERROR_INTERNET_TCPIP_NOT_INSTALLED;
     ID           : 'ERROR_INTERNET_TCPIP_NOT_INSTALLED';
     Description  : 'The required protocol stack is not loaded and the ' +
                    'application cannot start WinSock.'),
    (ErrorCode    : ERROR_INTERNET_DISCONNECTED;
     ID           : 'ERROR_INTERNET_DISCONNECTED';
     Description  : 'The Internet connection has been lost.'),
    (ErrorCode    : ERROR_INTERNET_SERVER_UNREACHABLE;
     ID           : 'ERROR_INTERNET_SERVER_UNREACHABLE';
     Description  : 'The Web site or server indicated is unreachable.'),
    (ErrorCode    : ERROR_INTERNET_PROXY_SERVER_UNREACHABLE;
     ID           : 'ERROR_INTERNET_PROXY_SERVER_UNREACHABLE';
     Description  : 'The designated proxy server cannot be reached.'),
    (ErrorCode    : ERROR_INTERNET_BAD_AUTO_PROXY_SCRIPT;
     ID           : 'ERROR_INTERNET_BAD_AUTO_PROXY_SCRIPT';
     Description  : 'There was an error in the automatic proxy configuration ' +
                    'script.'),
    (ErrorCode    : ERROR_INTERNET_UNABLE_TO_DOWNLOAD_SCRIPT;
     ID           : 'ERROR_INTERNET_UNABLE_TO_DOWNLOAD_SCRIPT';
     Description  : 'The automatic proxy configuration script could not be ' +
                    'downloaded. The INTERNET_FLAG_MUST_CACHE_REQUEST flag ' +
                    'was set.'),
    (ErrorCode    : ERROR_INTERNET_SEC_INVALID_CERT;
     ID           : 'ERROR_INTERNET_SEC_INVALID_CERT';
     Description  : 'SSL certificate is invalid.'),
    (ErrorCode    : ERROR_INTERNET_SEC_CERT_REVOKED;
     ID           : 'ERROR_INTERNET_SEC_CERT_REVOKED';
     Description  : 'SSL certificate was revoked.'),
    (ErrorCode    : ERROR_INTERNET_FAILED_DUETOSECURITYCHECK;
     ID           : 'ERROR_INTERNET_FAILED_DUETOSECURITYCHECK';
     Description  : 'The function failed due to a security check.')
  );
begin
  FIdentifier := 'ERROR_INTERNET_UNKNOWN';
  FErrorCode := ErrorCode;
  FDescription := 'Unknown error #' + IntToStr(ErrorCode);
  FOrigin := Origin;

  if ErrorCode = ERROR_INTERNET_EXTENDED_ERROR then
  begin
    Size := SizeOf(ErrorMessage);
    if InternetGetLastResponseInfo(NewError, ErrorMessage, Size) then
    begin
      FErrorCode := NewError;
      FIdentifier := 'ERROR_INTERNET_EXTENDED_ERROR';
      FDescription := ErrorMessage;
    end;
  end else if (ErrorCode >= 12001) and (ErrorCode <= 12156) then
    for Index := Low(ErrorCodes) to High(ErrorCodes) do
      if ErrorCodes[Index].ErrorCode = ErrorCode then
      begin
        FIdentifier := ErrorCodes[Index].ID;
        FDescription := ErrorCodes[Index].Description;
        Break;
      end;

  inherited CreateFmt('In %s: %s (#%d), %s', [FOrigin, FIdentifier, FErrorCode,
    FDescription]);
end;

{ TFTPDirectoryListing }

procedure TFTPDirectoryListing.Add(const FindFileData: TWin32FindData);
begin
//  if FindFileData.cFileName = 'SWLIB' then asm int 3 end;

  if (FindFileData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
    FDirectories.Add(TFTPDirectory.Create(FindFileData))
  else
    FFiles.Add(TFTPFile.Create(FindFileData));
end;

constructor TFTPDirectoryListing.Create(const FTP: IFTP);
begin
  inherited Create;
  FFTP := FTP;
  FDirectories := TInterfaceList.Create;
  FFiles := TInterfaceList.Create;
end;

function TFTPDirectoryListing.DirectoryExists(
  const DirectoryName: string): Boolean;
begin
  Result := Assigned(GetDirectoriesByName(DirectoryName));
end;

function TFTPDirectoryListing.FileExists(
  const FileName: string): Boolean;
begin
  Result := Assigned(GetFilesByName(FileName));
end;

function TFTPDirectoryListing.GetDirectories(
  const Index: Integer): IFTPDirectory;
begin
  Result := FDirectories[Index] as IFTPDirectory;
end;

function TFTPDirectoryListing.GetDirectoriesByName(
  const DirectoryName: string): IFTPDirectory;
var
  Index         : Integer;
  FTPDirectory  : IFTPDirectory;
begin
  Result := nil;
  for Index := 0 to FDirectories.Count-1 do
  begin
    FTPDirectory := FDirectories[Index] as IFTPDirectory;
    if CompareText(FTPDirectory.Name, DirectoryName) = 0 then
    begin
      Result := FTPDirectory;
      Break;
    end;
  end;
end;

function TFTPDirectoryListing.GetDirectoryCount: Integer;
begin
  Result := FDirectories.Count;
end;

function TFTPDirectoryListing.GetFileCount: Integer;
begin
  Result := FFiles.Count;
end;

function TFTPDirectoryListing.GetFiles(
  const Index: Integer): IFTPFile;
begin
  Result := FFiles[Index] as IFTPFile;
end;

function TFTPDirectoryListing.GetFilesByName(
  const FileName: string): IFTPFile;
var
  Index   : Integer;
  FTPFile : IFTPFile;
begin
  Result := nil;
  for Index := 0 to FFiles.Count-1 do
  begin
    FTPFile := FFiles[Index] as IFTPFile;
    if CompareText(FTPFile.Name, FileName) = 0 then
    begin
      Result := FTPFile;
      Break;
    end;
  end;
end;

function TFTPDirectoryListing.GetItemCount: Integer;
begin
  Result := FFiles.Count + FDirectories.Count;
end;

function TFTPDirectoryListing.GetItems(
  const Index: Integer): IFTPDirectoryItem;
begin
  if Index < FDirectories.Count then
    Result := FDirectories[Index] as IFTPDirectoryItem
  else
    Result := FFiles[Index - FDirectories.Count] as IFTPDirectoryItem;
end;

function TFTPDirectoryListing.GetItemsByName(
  const Name: string): IFTPDirectoryItem;
begin
  Result := GetDirectoriesByName(Name);
  if not Assigned(Result) then
    Result := GetFilesByName(Name);
end;

function TFTPDirectoryListing.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TFTPDirectoryListing.ItemExists(const Name: string): Boolean;
begin
  Result := Assigned(GetItemsByName(Name));
end;

{ TFTPDirectoryItem }

constructor TFTPDirectoryItem.Create(
  const FindFileData: TWin32FindData);

  function FileTimeToDateTime(const ft: TFileTime): TDateTime;
  var
    st  : TSystemTime;
    Ok  : Boolean;
  begin
    Ok := FileTimeToSystemTime(ft, st);
    if not Ok then
      RaiseLastWin32Error;
    Result := SystemTimeToDateTime(st);
  end;

begin
  inherited Create;

  FName := FindFileData.cFileName;
  FAlternateName := FindFileData.cAlternateFileName;
  FCreationTime := FileTimeToDateTime(FindFileData.ftCreationTime);
  FLastAccessTime := FileTimeToDateTime(FindFileData.ftLastAccessTime);
  FLastWriteTime := FileTimeToDateTime(FindFileData.ftLastWriteTime);
  FAttributes := FindFileData.dwFileAttributes;
end;

function TFTPDirectoryItem.GetAlternateName: string;
begin
  Result := FAlternateName;
end;

function TFTPDirectoryItem.GetAttributes: Cardinal;
begin
  Result := FAttributes;
end;

function TFTPDirectoryItem.GetCreationTime: TDateTime;
begin
  Result := FCreationTime;
end;

function TFTPDirectoryItem.GetLastAccessTime: TDateTime;
begin
  Result := FLastAccessTime;
end;

function TFTPDirectoryItem.GetLastWriteTime: TDateTime;
begin
  Result := FLastWriteTime;
end;

function TFTPDirectoryItem.GetName: string;
begin
  Result := FName;
end;

function TFTPDirectoryItem.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

{ TFTPFile }

constructor TFTPFile.Create(const FindFileData: TWin32FindData);
var
  Dummy : Int64Rec;
begin
  inherited Create(FindFileData);
  Dummy.Hi := FindFileData.nFileSizeHigh;
  Dummy.Lo := FindFileData.nFileSizeLow;

  FSize := Int64(Dummy);
end;

function TFTPFile.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TFTPFile.GetSize: Int64;
begin
  Result := FSize;
end;

{ TInternetDownload }

procedure TInternetDownload.Close;
begin
  if not Assigned(FHandle) then
    raise ElvkInternetUpload.Create('Download already closed, cannot close ' +
      'again');

  InternetCloseHandle(FHandle);
  FHandle := nil;
end;

constructor TInternetDownload.Create(const Source: IInternetObject;
  const Handle: HINTERNET);
var
  SizeRec       : Int64Rec;
  FTP           : IFTP;
  HTTP          : IHTTP;
  ContentLength : Cardinal;
  Size          : Cardinal;
  Index         : Cardinal;
begin
  inherited Create;

  Assert(Assigned(Source));
  Assert(Assigned(Handle));
  FHandle := Handle;
  FSource := Source;

  FPosition := 0;
  if Source.QueryInterface(IFTP, FTP) = S_OK then
  begin
    SizeRec.Lo := FtpGetFileSize(Handle, @SizeRec.Hi);
    FSize := Int64(SizeRec);
  end else if Source.QueryInterface(IHTTP, HTTP) = S_OK then
  begin
    Size := SizeOf(ContentLength);
    Index := 0;
    if HttpQueryInfo(Handle, HTTP_QUERY_CONTENT_LENGTH or
      HTTP_QUERY_FLAG_NUMBER, @ContentLength, Size, Index) then
      FSize := ContentLength
    else
      FSize := -1;
  end;
end;

destructor TInternetDownload.Destroy;
begin
  InternetCloseHandle(FHandle);

  inherited;
end;

function TInternetDownload.GetEOF: Boolean;
begin
  Result := FPosition = FSize;
end;

function TInternetDownload.GetHandle: HINTERNET;
begin
  Result := FHandle;
end;

function TInternetDownload.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TInternetDownload.GetPosition: Int64;
begin
  if not Assigned(FHandle) then
    raise ElvkInternetDownload.Create('Unable to retrieve position after ' +
      'download have been closed');

  Result := FPosition;
end;

function TInternetDownload.GetSize: Int64;
begin
  if not Assigned(FHandle) then
    raise ElvkInternetDownload.Create('Unable to retrieve size after ' +
      'download have been closed');

  Result := FSize;
end;

function TInternetDownload.Read(var Buffer;
  const BufferSize: Cardinal): Cardinal;
var
  BytesRead : Cardinal;
begin
  if not Assigned(FHandle) then
    raise ElvkInternetDownload.Create('Unable to read from download after ' +
      'it has been closed');

  if not InternetReadFile(FHandle, @Buffer, BufferSize, BytesRead) then
    RaiseLastWinInetError('TInternetDownload.Read');

  Result := BytesRead;
  Inc(FPosition, BytesRead);
end;

procedure TInternetDownload.ReadBuffer(var Buffer;
  const BufferSize: Cardinal);
begin
  if not Assigned(FHandle) then
    raise ElvkInternetDownload.Create('Unable to read from download after ' +
      'it has been closed');

  if Read(Buffer, BufferSize) <> BufferSize then
    raise ElvkInternetDownloadRead.Create('Unable to complete read request');
end;

procedure TInternetDownload.SaveToFile(const FileName: string);
var
  FileStream  : TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TInternetDownload.SaveToStream(const Stream: TStream);
var
  DownloadStream  : TStream;
  Buffer          : PChar;
  InBuffer        : Integer;
begin
  if GetSize = -1 then
  begin
    GetMem(Buffer, 4096);
    try
      repeat
        InBuffer := Read(Buffer^, 4096);
        if InBuffer > 0 then
          Stream.WriteBuffer(Buffer^, InBuffer);
      until InBuffer = 0;
    finally
      FreeMem(Buffer);
    end;
  end else begin
    DownloadStream := TInternetDownloadStream.Create(Self);
    try
      Stream.CopyFrom(DownloadStream, 0);
    finally
      DownloadStream.Free;
    end;
  end;
end;

{ TInternetUpload }

procedure TInternetUpload.Close;
begin
  if not Assigned(FHandle) then
    raise ElvkInternetUpload.Create('Upload already closed, cannot ' +
      'close again');

  InternetCloseHandle(FHandle);
  FHandle := nil;
end;

constructor TInternetUpload.Create(const Source: IInternetObject;
  const Handle: HINTERNET);
begin
  inherited Create;

  FHandle := Handle;
  FSource := Source;
  FPosition := 0;
end;

destructor TInternetUpload.Destroy;
begin
  InternetCloseHandle(FHandle);

  inherited;
end;

function TInternetUpload.GetHandle: HINTERNET;
begin
  Result := FHandle;
end;

function TInternetUpload.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TInternetUpload.GetPosition: Int64;
begin
  if not Assigned(FHandle) then
    raise ElvkInternetDownload.Create('Unable to retrieve position of ' +
      'upload after it has been closed');

  Result := FPosition;
end;

function TInternetUpload.Write(const Buffer;
  const BufferSize: Cardinal): Cardinal;
var
  BytesWritten  : Cardinal;
begin
  if not Assigned(FHandle) then
    raise ElvkInternetDownload.Create('Unable to write to upload after it ' +
      'has been closed');

  if not InternetWriteFile(FHandle, @Buffer, BufferSize, BytesWritten) then
    RaiseLastWinInetError('TInternetUpload.Write');

  Inc(FPosition, BytesWritten);
  Result := BytesWritten;
end;

procedure TInternetUpload.WriteBuffer(const Buffer;
  const BufferSize: Cardinal);
begin
  if not Assigned(FHandle) then
    raise ElvkInternetDownload.Create('Unable to write to upload after it ' +
      'has been closed');

  if Write(Buffer, BufferSize) <> BufferSize then
    raise ElvkInternetUploadWrite.Create('Unable to complete write request');
end;

{ TInternetDownloadStream }

constructor TInternetDownloadStream.Create(
  const InternetDownload: IInternetDownload);
begin
  inherited Create;

  Assert(Assigned(InternetDownload));

  FInternetDownload := InternetDownload;
  FCurrentPosition := InternetDownload.Position;
end;

function TInternetDownloadStream.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TInternetDownloadStream.Read(var Buffer;
  Count: Integer): Longint;
begin
  if FCurrentPosition <> FInternetDownload.Position then
    raise ElvkInternetDownloadRead.Create('Cannot read while seeked to a ' +
      'different position on this stream class');

  Result := FInternetDownload.Read(Buffer, Count);
  FCurrentPosition := FInternetDownload.Position;
end;

function TInternetDownloadStream.Seek(Offset: Integer;
  Origin: Word): Longint;
begin
  if Origin = soFromBeginning then
    FCurrentPosition := Offset
  else if Origin = soFromCurrent then
    FCurrentPosition := FCurrentPosition + Offset
  else if Origin = soFromEnd then
    FCurrentPosition := FInternetDownload.Size + Offset;

  Result := FCurrentPosition;
end;

{$IFDEF DELPHI6UP}
function TInternetDownloadStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning:
      Result := Seek(Offset, soFromBeginning);

    soCurrent:
      Result := Seek(Offset, soFromCurrent);

    soEnd:
      Result := Seek(Offset, soFromEnd);
  else
    Result := FCurrentPosition;
  end;
end;
{$ENDIF}

function TInternetDownloadStream.Write(const Buffer;
  Count: Integer): Longint;
begin
  raise ElvkInternetDownloadWrite.Create('This stream class does not ' +
    'allow writing');
end;

{ TInternetUploadStream }

constructor TInternetUploadStream.Create(
  const InternetUpload: IInternetUpload);
begin
  inherited Create;

  Assert(Assigned(InternetUpload));
  FInternetUpload := InternetUpload;
  FCurrentPosition := FInternetUpload.Position;
end;

function TInternetUploadStream.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TInternetUploadStream.Read(var Buffer;
  Count: Integer): Longint;
begin
  raise ElvkInternetDownloadWrite.Create('This stream class does not ' +
    'allow reading');
end;

function TInternetUploadStream.Seek(Offset: Integer;
  Origin: Word): Longint;
begin
  if Origin = soFromBeginning then
    FCurrentPosition := Offset
  else if Origin = soFromCurrent then
    FCurrentPosition := FCurrentPosition + Offset
  else if Origin = soFromEnd then
    FCurrentPosition := FInternetUpload.Position + Offset;

  Result := FCurrentPosition;
end;

{$IFDEF DELPHI6UP}
function TInternetUploadStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning:
      Result := Seek(Offset, soFromBeginning);

    soCurrent:
      Result := Seek(Offset, soFromCurrent);

    soEnd:
      Result := Seek(Offset, soFromEnd);
  else
    Result := FCurrentPosition;
  end;
end;
{$ENDIF}

function TInternetUploadStream.Write(const Buffer;
  Count: Integer): Longint;
begin
  Result := FInternetUpload.Write(Buffer, Count);
end;

{ THTTP }

function THTTP.ConnectToServer(
  const Session: IInternetSession): HINTERNET;
begin
  Result := InternetConnect((Session as IInternetObject).Handle,
    PChar(HostName), Port,
    PChar(Authentication.Username), PChar(Authentication.Password),
    INTERNET_SERVICE_HTTP,
    0, Cardinal((Session as IInternetSessionInternal).Context));

  if not Assigned(Result) then
    RaiseLastWinInetError('THTTP.ConnectToServer');
end;

constructor THTTP.Create;
begin
  inherited;

  FHTTPVersion := 'HTTP/1.0';
  FAccepts := TStringList.Create;
  FAccepts.Add('text/html');
  FAccepts.Add('text/plain');
  FAccepts.Add('text/*');
  FAccepts.Add('image/gif');
  FAccepts.Add('image/jpeg');
  FAccepts.Add('image/*');
  FAccepts.Add('*/*');
end;

destructor THTTP.Destroy;
begin
  FAccepts.Free;
  inherited;
end;

function THTTP.GET(const DocumentName: string;
  const Headers: array of string; const Referer: string;
  const Options: THTTPRequestOptions): IHTTPDownload;
begin
  Assert(DocumentName <> '');

  Result := Request('GET', DocumentName, Referer, Headers, Options, nil, 0);
end;

procedure THTTP.GET(const DocumentName, DestinationFileName: string;
  const Headers: array of string; const Referer: string;
  const Options: THTTPRequestOptions);
var
  FileStream  : TStream;
begin
  Assert(DestinationFileName <> '');
  FileStream := TFileStream.Create(DestinationFileName, fmCreate);
  try
    GET(DocumentName, FileStream, Headers, Referer, Options);
  finally
    FileStream.Free;
  end;
end;

procedure THTTP.GET(const DocumentName: string;
  const DestinationStream: TStream; const Headers: array of string;
  const Referer: string; const Options: THTTPRequestOptions);
var
  Download        : IHTTPDownload;
  DownloadStream  : TStream;
  Buffer          : array[0..2047] of Char;
  InBuffer        : Integer;
begin
  Assert(DocumentName <> '');

  Download := GET(DocumentName, Headers, Referer, Options);
  if Download.Size >= 0 then
  begin
    DownloadStream := TInternetDownloadStream.Create(Download);
    try
      DestinationStream.CopyFrom(DownloadStream, 0)
    finally
      DownloadStream.Free;
    end;
  end else begin
    repeat
      InBuffer := Download.Read(Buffer, SizeOf(Buffer));
      DestinationStream.WriteBuffer(Buffer, InBuffer);
    until InBuffer = 0;
  end;
  Download.Close;
end;

function THTTP.GetAccepts: TStrings;
begin
  Result := FAccepts;
end;

function THTTP.GetHTTPVersion: string;
begin
  Result := FHTTPVersion;
end;

procedure THTTP.POST(const PostData: TStream;
  const DocumentName: string; const DestinationStream: TStream;
  const Headers: array of string; const Referer: string;
  const Options: THTTPRequestOptions);
var
  PostDataStr : string;
begin
  SetLength(PostDataStr, PostData.Size);
  if PostData.Size > 0 then
  begin
    PostData.Position := 0;
    PostData.ReadBuffer(PostDataStr[1], PostData.Size);
  end;
  POST(PostDataStr, DocumentName, DestinationStream, Headers, Referer, Options);
end;

procedure THTTP.POST(const PostData, DocumentName: string;
  const DestinationStream: TStream; const Headers: array of string;
  const Referer: string; const Options: THTTPRequestOptions);
var
  Download        : IHTTPDownload;
  DownloadStream  : TStream;
  Buffer          : array[0..2047] of Char;
  InBuffer        : Integer;
begin
  Assert(DocumentName <> '');

  Download := POST(PostData, DocumentName, Headers, Referer, Options);
  if Download.Size >= 0 then
  begin
    DownloadStream := TInternetDownloadStream.Create(Download);
    try
      DestinationStream.CopyFrom(DownloadStream, 0)
    finally
      DownloadStream.Free;
    end;
  end else begin
    repeat
      InBuffer := Download.Read(Buffer, SizeOf(Buffer));
      DestinationStream.WriteBuffer(Buffer, InBuffer);
    until InBuffer = 0;
  end;
  Download.Close;
end;

function THTTP.POST(const PostData, DocumentName: string;
  const Headers: array of string; const Referer: string;
  const Options: THTTPRequestOptions): IHTTPDownload;
begin
  Assert(DocumentName <> '');

  Result := Request('POST', DocumentName, Referer, Headers, Options,
    PChar(PostData), Length(PostData));
end;

function THTTP.HEAD(const DocumentName: string;
  const Headers: array of string; const Referer: string;
  const Options: THTTPRequestOptions): IHTTPDownload;
begin
  Assert(DocumentName <> '');

  Result := Request('HEAD', DocumentName, Referer, Headers, Options, nil, 0);
end;

function THTTP.POST(const PostData, DocumentName,
  DestinationFileName: string; const Headers: array of string;
  const Referer: string;
  const Options: THTTPRequestOptions): IHTTPDownload;
var
  FileStream  : TStream;
begin
  Assert(DestinationFileName <> '');
  FileStream := TFileStream.Create(DestinationFileName, fmCreate);
  try
    POST(PostData, DocumentName, FileStream, Headers, Referer, Options);
  finally
    FileStream.Free;
  end;
end;

procedure THTTP.PUT(const SourceStream: TStream;
  const DocumentName: string; const Headers: array of string;
  const Referer: string; const Options: THTTPRequestOptions);
var
  MemoryStream  : TMemoryStream;
  PutMemoryMap  : TMemoryMappedFileStream;
begin
  Assert(Assigned(SourceStream));
  Assert(DocumentName <> '');

  if (SourceStream is THandleStream) then
  begin
    PutMemoryMap := nil;
    try
      PutMemoryMap := TMemoryMappedFileStream.Create(
        THandleStream(SourceStream), fmOpenRead, 0, SourceStream.Size);

      Request('PUT', DocumentName, Referer, Headers, Options,
        PutMemoryMap.Memory, PutMemoryMap.Size);
    finally
      PutMemoryMap.Free;
    end;
  end else if (SourceStream is TMemoryMappedFileStream) then
    Request('PUT', DocumentName, Referer, Headers, Options,
      TMemoryMappedFileStream(SourceStream).Memory, SourceStream.Size)
  else if (SourceStream is TMemoryStream) then
  begin
    Request('PUT', DocumentName, Referer, Headers, Options,
      TMemoryStream(SourceStream).Memory, SourceStream.Size)
  end else begin
    MemoryStream := TMemoryStream.Create;
    try
      MemoryStream.CopyFrom(SourceStream, 0);
      PUT(MemoryStream, DocumentName, Headers, Referer, Options);
    finally
      MemoryStream.Free;
    end;
  end;
end;

procedure THTTP.PUT(const SourceFileName, DocumentName: string;
  const Headers: array of string; const Referer: string;
  const Options: THTTPRequestOptions);
var
  PutStream : TFileStream;
begin
  Assert((SourceFileName <> '') and FileExists(SourceFileName));
  Assert(DocumentName <> '');

  PutStream := nil;
  try
    PutStream := TFileStream.Create(SourceFileName, fmOpenRead or
      fmShareDenyWrite);
    PUT(PutStream, DocumentName, Headers, Referer, Options);
  finally
    PutStream.Free;
  end;
end;

function THTTP.Request(const Verb, DocumentName,
  Referer: string;
  const Headers: array of string;
  const Options: THTTPRequestOptions;
  const PostData: PChar; const PostLength: Integer): IHTTPDownload;
var
  RequestHandle   : HINTERNET;
  AcceptsStrings  : array of string;
  AcceptsList     : packed array of PChar;
  Index           : Cardinal;
  Flags           : Cardinal;
  Option          : THTTPRequestOption;
  Header          : string;
  StatusCode      : Cardinal;
  Size            : Cardinal;
const
  OptionValues  : array[THTTPRequestOption, Boolean] of Cardinal = (
    (0, INTERNET_FLAG_KEEP_CONNECTION),
    (INTERNET_FLAG_NO_AUTO_REDIRECT, 0),
    (INTERNET_FLAG_NO_CACHE_WRITE, 0),
    (INTERNET_FLAG_NO_COOKIES, 0),
    (INTERNET_FLAG_NO_UI, 0),
    (0, INTERNET_FLAG_RELOAD),
    (0, INTERNET_FLAG_SECURE)
  );
begin
  if FAccepts.Count = 0 then
  begin
    SetLength(AcceptsList, 7);
    AcceptsList[0] := '*/*';
    AcceptsList[1] := nil;
  end else begin
    SetLength(AcceptsList, FAccepts.Count);
    SetLength(AcceptsStrings, FAccepts.Count);
    for Index := 0 to FAccepts.Count-1 do
    begin
      AcceptsStrings[Index] := FAccepts[Index];
      AcceptsList[Index] := PChar(AcceptsStrings[Index]);
    end;
    AcceptsList[High(AcceptsList)] := nil;
  end;
  Header := '';
  if Length(Headers) > 0 then
    for Index := Low(Headers) to High(Headers) do
    begin
      Header := Header + Headers[Index];
      if Integer(Index) < High(Headers) then
        Header := Header + #13#10;
    end;

  if (Header = '') and (Verb = 'POST') then
    Header := 'Content-Type: application/x-www-form-urlencoded';

  Flags := 0;
  for Option := Low(THTTPRequestOption) to High(THTTPRequestOption) do
    Flags := Flags or OptionValues[Option, Option in Options];

  RequestHandle := HttpOpenRequest(Handle, PChar(Verb), PChar(DocumentName),
    PChar(Version), PChar(Referer), @AcceptsList[0], Flags,
    (Session as IInternetSessionInternal).Context);
  if not Assigned(RequestHandle) then
    RaiseLastWinInetError('THTTP.Request');
  try
    if not HttpSendRequest(RequestHandle, PChar(Header), Length(Header),
      PostData, PostLength) then
    begin
      RaiseLastWinInetError('THTTP.Request');
    end;
  except
    InternetCloseHandle(RequestHandle);
    raise;
  end;

  Size := SizeOf(StatusCode);
  Index := 0;
  if not HttpQueryInfo(RequestHandle, HTTP_QUERY_STATUS_CODE or
    HTTP_QUERY_FLAG_NUMBER, @StatusCode, Size, Index) then
    RaiseLastWinInetError('THTTP.Request');

  if (StatusCode in HTTP_ACCEPTABLE_STATUS_CODES) or (StatusCode = 403)then
    Result := THTTPDownload.Create(Self, RequestHandle)
  else
    raise ElvkHTTPResponse.Create(StatusCode, 'THTTP.Request');
end;

procedure THTTP.SetHTTPVersion(const NewValue: string);
begin
  FHTTPVersion := NewValue;
end;

function THTTP.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function THTTP.GET(const DocumentName, Referer: string;
  const Options: THTTPRequestOptions): IHTTPDownload;
begin
  Result := GET(DocumentName, [], Referer, Options);
end;

{ ElvkHTTPResponse }

constructor ElvkHTTPResponse.Create(const StatusCode: Cardinal;
  const Origin: string);
var
  Index : Integer;
type
  TStatusCode = record
    StatusCode  : Cardinal;
    ID          : string;
    StatusText  : string;
  end;
const
  StatusCodes : array[1..40] of TStatusCode = (
    (StatusCode : HTTP_STATUS_CONTINUE;
     ID         : 'HTTP_STATUS_CONTINUE';
     StatusText : 'The request can be continued.'),
    (StatusCode : HTTP_STATUS_SWITCH_PROTOCOLS;
     ID         : 'HTTP_STATUS_SWITCH_PROTOCOLS';
     StatusText : 'Server has switched protocols in upgrade header.'),
    (StatusCode : HTTP_STATUS_OK;
     ID         : 'HTTP_STATUS_OK';
     StatusText : 'Request completed'),
    (StatusCode : HTTP_STATUS_CREATED;
     ID         : 'HTTP_STATUS_CREATED';
     StatusText : 'Object created, reason = new URI'),
    (StatusCode : HTTP_STATUS_ACCEPTED;
     ID         : 'HTTP_STATUS_ACCEPTED';
     StatusText : 'Async completion (TBS)'),
    (StatusCode : HTTP_STATUS_PARTIAL;
     ID         : 'HTTP_STATUS_PARTIAL';
     StatusText : 'Partial completion'),
    (StatusCode : HTTP_STATUS_NO_CONTENT;
     ID         : 'HTTP_STATUS_NO_CONTENT';
     StatusText : 'The server has fulfilled the request, but there is no ' +
                  'new information to send back'),
    (StatusCode : HTTP_STATUS_RESET_CONTENT;
     ID         : 'HTTP_STATUS_RESET_CONTENT';
     StatusText : 'Request completed, but clear form'),
    (StatusCode : HTTP_STATUS_PARTIAL_CONTENT;
     ID         : 'HTTP_STATUS_PARTIAL_CONTENT';
     StatusText : 'Partial GET furfilled'),
    (StatusCode : HTTP_STATUS_AMBIGUOUS;
     ID         : 'HTTP_STATUS_AMBIGUOUS';
     StatusText : 'Server couldn''t decide what to return'),
    (StatusCode : HTTP_STATUS_MOVED;
     ID         : 'HTTP_STATUS_MOVED';
     StatusText : 'Object permanently moved'),
    (StatusCode : HTTP_STATUS_REDIRECT;
     ID         : 'HTTP_STATUS_REDIRECT';
     StatusText : 'Object temporarily moved'),
    (StatusCode : HTTP_STATUS_REDIRECT_METHOD;
     ID         : 'HTTP_STATUS_REDIRECT_METHOD';
     StatusText : 'Redirection w/ new access method'),
    (StatusCode : HTTP_STATUS_NOT_MODIFIED;
     ID         : 'HTTP_STATUS_NOT_MODIFIED';
     StatusText : 'If-modified-since was not modified'),
    (StatusCode : HTTP_STATUS_USE_PROXY;
     ID         : 'HTTP_STATUS_USE_PROXY';
     StatusText : 'Redirection to proxy, location header specifies ' +
                  'proxy to use'),
    (StatusCode : HTTP_STATUS_REDIRECT_KEEP_VERB;
     ID         : 'HTTP_STATUS_REDIRECT_KEEP_VERB';
     StatusText : 'HTTP/1.1: keep same verb'),
    (StatusCode : HTTP_STATUS_BAD_REQUEST;
     ID         : 'HTTP_STATUS_BAD_REQUEST';
     StatusText : 'Invalid syntax'),
    (StatusCode : HTTP_STATUS_DENIED;
     ID         : 'HTTP_STATUS_DENIED';
     StatusText : 'Access denied'),
    (StatusCode : HTTP_STATUS_PAYMENT_REQ;
     ID         : 'HTTP_STATUS_PAYMENT_REQ';
     StatusText : 'Payment required'),
    (StatusCode : HTTP_STATUS_FORBIDDEN;
     ID         : 'HTTP_STATUS_FORBIDDEN';
     StatusText : 'Request forbidden'),
    (StatusCode : HTTP_STATUS_NOT_FOUND;
     ID         : 'HTTP_STATUS_NOT_FOUND';
     StatusText : 'Object not found'),
    (StatusCode : HTTP_STATUS_BAD_METHOD;
     ID         : 'HTTP_STATUS_BAD_METHOD';
     StatusText : 'Method is not allowed'),
    (StatusCode : HTTP_STATUS_NONE_ACCEPTABLE;
     ID         : 'HTTP_STATUS_NONE_ACCEPTABLE';
     StatusText : 'No response acceptable to client found'),
    (StatusCode : HTTP_STATUS_PROXY_AUTH_REQ;
     ID         : 'HTTP_STATUS_PROXY_AUTH_REQ';
     StatusText : 'Proxy authentication required'),
    (StatusCode : HTTP_STATUS_REQUEST_TIMEOUT;
     ID         : 'HTTP_STATUS_REQUEST_TIMEOUT';
     StatusText : 'Server timed out waiting for request'),
    (StatusCode : HTTP_STATUS_CONFLICT;
     ID         : 'HTTP_STATUS_CONFLICT';
     StatusText : 'User should resubmit with more info'),
    (StatusCode : HTTP_STATUS_GONE;
     ID         : 'HTTP_STATUS_GONE';
     StatusText : 'The resource is no longer available'),
    (StatusCode : HTTP_STATUS_AUTH_REFUSED;
     ID         : 'HTTP_STATUS_AUTH_REFUSED';
     StatusText : 'Couldn''t authorize client'),
    (StatusCode : HTTP_STATUS_PRECOND_FAILED;
     ID         : 'HTTP_STATUS_PRECOND_FAILED';
     StatusText : 'Precondition given in request failed'),
    (StatusCode : HTTP_STATUS_REQUEST_TOO_LARGE;
     ID         : 'HTTP_STATUS_REQUEST_TOO_LARGE';
     StatusText : 'Request entity was too large'),
    (StatusCode : HTTP_STATUS_URI_TOO_LONG;
     ID         : 'HTTP_STATUS_URI_TOO_LONG';
     StatusText : 'Request URI too long'),
    (StatusCode : HTTP_STATUS_UNSUPPORTED_MEDIA;
     ID         : 'HTTP_STATUS_UNSUPPORTED_MEDIA';
     StatusText : 'Unsupported media type'),
    (StatusCode : HTTP_STATUS_SERVER_ERROR;
     ID         : 'HTTP_STATUS_SERVER_ERROR';
     StatusText : 'Internal server error'),
    (StatusCode : HTTP_STATUS_NOT_SUPPORTED;
     ID         : 'HTTP_STATUS_NOT_SUPPORTED';
     StatusText : 'Required not supported'),
    (StatusCode : HTTP_STATUS_BAD_GATEWAY;
     ID         : 'HTTP_STATUS_BAD_GATEWAY';
     StatusText : 'Error response received from gateway'),
    (StatusCode : HTTP_STATUS_SERVICE_UNAVAIL;
     ID         : 'HTTP_STATUS_SERVICE_UNAVAIL';
     StatusText : 'Temporarily overloaded'),
    (StatusCode : HTTP_STATUS_GATEWAY_TIMEOUT;
     ID         : 'HTTP_STATUS_GATEWAY_TIMEOUT';
     StatusText : 'Timed out waiting for gateway'),
    (StatusCode : HTTP_STATUS_VERSION_NOT_SUP;
     ID         : 'HTTP_STATUS_VERSION_NOT_SUP';
     StatusText : 'HTTP version not supported'),
    (StatusCode : HTTP_STATUS_FIRST;
     ID         : 'HTTP_STATUS_FIRST';
     StatusText : 'The request can be continued.'),
    (StatusCode : HTTP_STATUS_LAST;
     ID         : 'HTTP_STATUS_LAST';
     StatusText : 'HTTP version not supported')
  );
begin
  FStatusCode := StatusCode;

  FIdentifier := 'HTTP_STATUS_UNKNOWN';
  FStatusCode := StatusCode;
  FStatusText := 'Unknown status code #' + IntToStr(StatusCode);

  for Index := Low(StatusCodes) to High(StatusCodes) do
    if StatusCodes[Index].StatusCode = StatusCode then
    begin
      FIdentifier := StatusCodes[Index].ID;
      FStatusText := StatusCodes[Index].StatusText;
      Break;
    end;
  inherited CreateFmt('In %s: %s (#%d), %s', [Origin, FIdentifier, FStatusCode,
    FStatusText]);
end;

{ THTTPDownload }

function THTTPDownload.GetContentLength: Integer;
var
  Size, Index : Cardinal;
begin
  Size := SizeOf(Result);
  Index := 0;
  if not HttpQueryInfo(GetHandle, HTTP_QUERY_CONTENT_LENGTH or
    HTTP_QUERY_FLAG_NUMBER, @Result, Size, Index) then
  begin
    if GetLastError = ERROR_HTTP_HEADER_NOT_FOUND then
      Result := -1
    else
      RaiseLastWinInetError('THTTPDownload.ContentLength');
  end;
end;

function THTTPDownload.GetContentType: string;
begin
  Result := GetHeaders(HTTP_QUERY_CONTENT_TYPE);
end;

function THTTPDownload.GetExpires: TDateTime;
var
  Expires   : string;
  RegExp    : IRegExp;
  YearNo    : Integer;
  TZInfo    : TTimeZoneInformation;
  GMTTime   : TSystemTime;
  LocalTime : TSystemTime;
  rc        : Cardinal;
  y, m, d   : Word;
const
  MonthNames  = 'JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC';
begin
  Expires := GetHeaders(HTTP_QUERY_EXPIRES);

  if Expires = '' then
    Result := 0.0
  else begin
    // RFC 822, updated by RFC 1123
    // rfc1123-date = wkday "," SP date1 SP time SP "GMT"
    // wkday = "Mon" | "Tue" | "Wed" | "Thu" | "Fri" | "Sat" | "Sun"
    // date1 = 2DIGIT SP month SP 4DIGIT
    // month = "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" |
    //         "Sep" | "Oct" | "Nov" | "Dec"
    // time = 2DIGIT ":" 2DIGIT ":" 2DIGIT
    RegExp := NewRegExp('^\s*(Mon|Tue|Wed|Thu|Fri|Sat|Sun)\,\s*([0-9]{2})' +
      '\s*(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\s*([0-9]{4})\s*' +
      '([0-9]{2}):([0-9]{2}):([0-9]{2})\s*(.*)$');
    Result := 0.0;

    if RegExp.MatchAgainst(Expires) then
    begin
      if CompareText(RegExp[8].Text, 'GMT') = 0 then
      begin
        Result := EncodeDate(StrToIntDef(RegExp[4].Text, 0),
          (Pos(UpperCase(RegExp[3].Text), MonthNames)-1) div 4 + 1,
          StrToIntDef(RegExp[2].Text, 1));

        Result := Result + EncodeTime(StrToIntDef(RegExp[5].Text, 0),
          StrToIntDef(RegExp[6].Text, 0), StrToIntDef(RegExp[7].Text, 0), 0);
      end;
    end;

    RegExp := NewRegExp('^\s*(Monday|Tuesday|Wednesday|Thursday|Friday|' +
      'Saturday|Sunday)\,\s*([0-9]{2})-(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|' +
      'Sep|Oct|Nov|Dec)-([0-9]{2})\s*([0-9]{2}):([0-9]{2}):([0-9]{2})\s*(.*)$');

    // RFC 850, obsoleted by RFC 1036
    // rfc850-date  = weekday "," SP date2 SP time SP "GMT"
    // weekday = "Monday" | "Tuesday" | "Wednesday" | "Thursday" | "Friday" |
    //           "Saturday" | "Sunday"
    // date2 = 2DIGIT "-" month "-" 2DIGIT
    // month = "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" |
    //         "Sep" | "Oct" | "Nov" | "Dec"
    if RegExp.MatchAgainst(Expires) then
    begin
      if CompareText(RegExp[8].Text, 'GMT') = 0 then
      begin
        YearNo := 1900 + StrToIntDef(RegExp[4].Text, 0);
        DecodeDate(Now, y, m, d);
        if y - YearNo > 50 then
          YearNo := YearNo + 100;

        Result := EncodeDate(YearNo,
          (Pos(UpperCase(RegExp[3].Text), MonthNames)-1) div 4 + 1,
          StrToIntDef(RegExp[2].Text, 1));

        Result := Result + EncodeTime(StrToIntDef(RegExp[5].Text, 0),
          StrToIntDef(RegExp[6].Text, 0), StrToIntDef(RegExp[7].Text, 0), 0);
      end;
    end;

    // ANSI C's asctime() format
    // asctime-date = wkday SP date3 SP time SP 4DIGIT
    // wkday = "Mon" | "Tue" | "Wed" | "Thu" | "Fri" | "Sat" | "Sun"
    // date3 = month SP ( 2DIGIT | ( SP 1DIGIT ))
    // month = "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" |
    //         "Sep" | "Oct" | "Nov" | "Dec"
    // time = 2DIGIT ":" 2DIGIT ":" 2DIGIT
    RegExp := NewRegExp('^\s*(Mon|Tue|Wed|Thu|Fri|Sat|Sun)\s*(Jan|Feb|Mar|' +
      'Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\s*([0-9]+)\s*([0-9]{2}):' +
      '([0-9]{2}):([0-9]{2})\s*([0-9]{4})$');
    if RegExp.MatchAgainst(Expires) then
    begin
      Result := EncodeDate(StrToIntDef(RegExp[7].Text, 0),
        (Pos(UpperCase(RegExp[2].Text), MonthNames)-1) div 4 + 1,
        StrToIntDef(RegExp[3].Text, 1));

      Result := Result + EncodeTime(StrToIntDef(RegExp[4].Text, 0),
        StrToIntDef(RegExp[5].Text, 0), StrToIntDef(RegExp[6].Text, 0), 0);
    end;

    if Result > 0.1 then
    begin
      rc := GetTimeZoneInformation(TZInfo);
      if rc = TIME_ZONE_ID_UNKNOWN then
        RaiseLastWin32Error;

      DateTimeToSystemTime(Result, GMTTime);
      Win32Check(SystemTimeToTzSpecificLocalTime(@TZInfo, GMTTime, LocalTime));
      Result := SystemTimeToDateTime(LocalTime);
    end;
  end;
end;

function THTTPDownload.GetHeaders(const HeaderType: Cardinal): string;
var
  Buffer  : array[0..2047] of Char;
  Size    : Cardinal;
  Index   : Cardinal;
  rc      : Integer;
begin
  ZeroMemory(@Buffer, SizeOf(Buffer));

  Index := 0;
  repeat
    Size := SizeOf(Buffer);
    if not HttpQueryInfo(GetHandle, HeaderType, @Buffer, Size, Index) then
    begin
      rc := GetLastError;
      if rc = ERROR_HTTP_HEADER_NOT_FOUND then
        Break
      else if rc <> ERROR_SUCCESS then
        RaiseLastWinInetError('THTTPDownload.GetHeaders');
    end;

    if Result <> '' then
      Result := Result + ', ';
    Result := Result + Buffer;
  until Index = ERROR_HTTP_HEADER_NOT_FOUND;
end;

function THTTPDownload.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

{ TInternetExplorerCache }

function TInternetExplorerCache.GetItems(
  const RegExp: string; const Options: TlvkRegExpOptions): IInternetExplorerCacheItems;
begin
  Result := TInternetExplorerCacheItems.Create(RegExp, Options) as IInternetExplorerCacheItems;
end;

function TInternetExplorerCache.GetItems(
  const RegExp: IRegExp): IInternetExplorerCacheItems;
begin
  Result := TInternetExplorerCacheItems.Create(RegExp) as IInternetExplorerCacheItems;
end;

function TInternetExplorerCache.GetItems: IInternetExplorerCacheItems;
begin
  Result := TInternetExplorerCacheItems.Create as IInternetExplorerCacheItems;
end;

{ TInternetExplorerCacheItems }

constructor TInternetExplorerCacheItems.Create(const RegExp: string;
  const Options: TlvkRegExpOptions);
begin
  Create(NewRegExp(RegExp, Options));
end;

constructor TInternetExplorerCacheItems.Create(const RegExp: IRegExp);
type
  PEntry  = ^TEntry;
  TEntry  = packed record
    Info  : TInternetCacheEntryInfo;
    Extra : array[0..8191] of Char;
  end;
var
  Entry   : PEntry;
  Size    : Cardinal;
  Item    : IInternetExplorerCacheItem;
  Handle  : THandle;
  OkToAdd : Boolean;
begin
  inherited Create;
  FItems := TInterfaceList.Create as IInterfaceList;

  New(Entry);
  try
    Size := SizeOf(TEntry);
    Handle := FindFirstUrlCacheEntryEx(nil, 0, NORMAL_CACHE_ENTRY, 0,
      PInternetCacheEntryInfo(Entry), @Size, nil, nil, nil);
    if Handle = 0 then
      RaiseLastWin32Error
    else try
      repeat
        if Assigned(RegExp) then
          OkToAdd := RegExp.MatchAgainst(Entry.Info.lpszSourceUrlName)
        else
          OkToAdd := True;

        if OkToAdd then
        begin
          Item := TInternetExplorerCacheItem.Create(Entry.Info);
          FItems.Add(Item);
        end;

        Size := SizeOf(TEntry);
      until not FindNextUrlCacheEntryEx(Handle, PInternetCacheEntryInfo(Entry), @Size, nil, nil, nil);
    finally
      FindCloseUrlCache(Handle);
    end;
  finally
    Dispose(Entry);
  end;

  SortItems;
end;

constructor TInternetExplorerCacheItems.Create;
{$IFDEF DELPHI6UP}
begin
  Create(nil);
{$ELSE}
var
  re  : IRegExp;
begin
  re := nil;
  Create(re);
{$ENDIF}
end;

function TInternetExplorerCacheItems.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TInternetExplorerCacheItems.GetItems(
  const Index: Integer): IInternetExplorerCacheItem;
begin
  Result := FItems[Index] as IInternetExplorerCacheItem;
end;

procedure TInternetExplorerCacheItems.SortItems;

  function Compare(const Index1, Index2: Integer): Integer;
  begin
    Result := CompareText(
      (FItems[Index1] as IInternetExplorerCacheItem).SourceURLName,
      (FItems[Index2] as IInternetExplorerCacheItem).SourceURLName);
  end;

  procedure QuickSort(L, R: Integer);
  var
    I, J, P: Integer;
  begin
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        while Compare(I, P) < 0 do Inc(I);
        while Compare(J, P) > 0 do Dec(J);
        if I <= J then
        begin
          FItems.Exchange(I, J);
          if P = I then
            P := J
          else if P = J then
            P := I;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  if FItems.Count > 1 then
    QuickSort(0, FItems.Count-1);
end;

{ TInternetExplorerCacheItem }

constructor TInternetExplorerCacheItem.Create(
  const EntryInfo: TInternetCacheEntryInfo);
var
  i64 : Int64Rec;

  function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
  var
    SystemTime  : TSystemTime;
  begin
    if FileTime.dwHighDateTime = UInt32(MAX_INT_32) then
      Result := 0.0
    else begin
      FileTimeToSystemTime(FileTime, SystemTime);
      Result := SystemTimeToDateTime(SystemTime);
    end;
  end;

begin
  inherited Create;

  FSourceURLName := EntryInfo.lpszSourceUrlName;
  FLocalFileName := EntryInfo.lpszLocalFileName;
  FUseCount := EntryInfo.dwUseCount;
  FHitRate := EntryInfo.dwHitRate;

  i64.Hi := EntryInfo.dwSizeHigh;
  i64.Lo := EntryInfo.dwSizeLow;
  FSize := Int64(i64);
  FLastModifiedTime := FileTimeToDateTime(EntryInfo.LastModifiedTime);
  FLastAccessTime := FileTimeToDateTime(EntryInfo.LastAccessTime);
  FLastSyncTime := FileTimeToDateTime(EntryInfo.LastSyncTime);
  FExpireTime := FileTimeToDateTime(EntryInfo.ExpireTime);
  SetLength(FHeaderInfo, EntryInfo.dwHeaderInfoSize);

  Move(EntryInfo.lpHeaderInfo^, PChar(FHeaderInfo)^, EntryInfo.dwHeaderInfoSize);
  FFileExtension := EntryInfo.lpszFileExtension;
end;

function TInternetExplorerCacheItem.GetExpireTime: TDateTime;
begin
  Result := FExpireTime;
end;

function TInternetExplorerCacheItem.GetFileExtension: string;
begin
  Result := FFileExtension;
end;

function TInternetExplorerCacheItem.GetHeaderInfo: string;
begin
  Result := FHeaderInfo;
end;

function TInternetExplorerCacheItem.GetHitRate: Cardinal;
begin
  Result := FHitRate;
end;

function TInternetExplorerCacheItem.GetLastAccessTime: TDateTime;
begin
  Result := FLastAccessTime;
end;

function TInternetExplorerCacheItem.GetLastModifiedTime: TDateTime;
begin
  Result := FLastModifiedTime;
end;

function TInternetExplorerCacheItem.GetLastSyncTime: TDateTime;
begin
  Result := FLastSyncTime;
end;

function TInternetExplorerCacheItem.GetLocalFileName: string;
begin
  Result := FLocalFileName;
end;

function TInternetExplorerCacheItem.GetSize: Int64;
begin
  Result := FSize;
end;

function TInternetExplorerCacheItem.GetSourceURLName: string;
begin
  Result := FSourceURLName;
end;

function TInternetExplorerCacheItem.GetUseCount: Cardinal;
begin
  Result := FUseCount;
end;

end.
