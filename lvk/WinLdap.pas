unit Winldap;

//
// Delphi import unit for wldap32.dll
//
// Converted from winldap.h by Luk Vermeulen (lvermeulen@seria.com)
//
// v1.0 - untested
//

interface

// declarations to keep Delphi in line
type
  ULONG = longword;
  UCHAR = byte;
  LONG = longint;
  PVOID = pointer;
  USHORT = word;
  PWCHAR = pwidechar;
  INT = integer;
  PPWCHAR = ^PWCHAR;
  PPCHAR = ^PCHAR;

(*

Copyright(c)1996  Microsoft Corporation

Module Name:

    winldap.h   LDAP client 32 API header file

Abstract:

   This module is the header file for the 32 bit LDAP client API for
   Windows NT and Windows 95.  This API is based on RFC 1823 with some
   enhancements for LDAP v3.

   Notes about Unicode support :

   If you have UNICODE defined at compile time, you'll pull in the unicode
   versions of the calls.  Note that your executable may then not work with
   other implementations of the LDAP API that don't support Unicode.  If
   UNICODE is not defined, then we define the LDAP calls without the trailing
   'A'(as in ldap_bind rather than ldap_bindA)so that your app may work
   with other implementations that don't support Unicode.

   The import library has all three forms of the call present... ldap_bindW,
   ldap_bindA, and ldap_bind.  ldap_bindA simply calls ldap_bind.  ldap_bind
   simply converts the arguments to unicode and calls ldap_bindW.  The
   reason this is done is because we have to put UTF-8 on the wire, so if
   we converted from Unicode to single byte, we'd loose information.  Since
   all core processing is done in Unicode, nothing is lost.

Updates :

   11/01/96  Modified for new API RFC draft.

Environments :

    Win32 user mode

*)

//
//  The #define LDAP_UNICODE controls if we map the undecorated calls to
//  their unicode counterparts or just leave them defined as the normal
//  single byte entry points.
//
//  If you want to write a UNICODE enabled application, you'd normally
//  just have UNICODE defined and then we'll default to using all LDAP
//  Unicode calls.
//

{$IFNDEF LDAP_UNICODE}
  {$IFDEF UNICODE}
    {$DEFINE LDAP_UNICODE}
  {$ELSE}
    {$UNDEF LDAP_UNICODE}
  {$ENDIF}
{$ENDIF}

const
  //
  //  Global constants
  //


  LDAP_PORT               = 389;
  LDAP_SSL_PORT           = 636;

//
//  We currently support going to either v2 or v3 servers, though the API
//  is only a V2 API.  We'll add support for result sets, server side
//  sorting, extended operations, etc as soon as they stabilize.
//

  LDAP_VERSION1          = 1;
  LDAP_VERSION2          = 2;
  LDAP_VERSION3          = 3;
  LDAP_VERSION           = LDAP_VERSION2;

//
//  All tags are CCFTTTTT.
//               CC        Tag Class 00 = universal
//                                   01 = application wide
//                                   10 = context specific
//                                   11 = private use
//
//                 F       Form 0 primitive
//                              1 constructed
//
//                  TTTTT  Tag Number
//

//
// LDAP v2 & v3 commands.
//

  LDAP_BIND_CMD          = $60;   // application + constructed
  LDAP_UNBIND_CMD        = $42;   // application + primitive
  LDAP_SEARCH_CMD        = $63;   // application + constructed
  LDAP_MODIFY_CMD        = $66;   // application + constructed
  LDAP_ADD_CMD           = $68;   // application + constructed
  LDAP_DELETE_CMD        = $4a;   // application + primitive
  LDAP_MODRDN_CMD        = $6c;   // application + constructed
  LDAP_COMPARE_CMD       = $6e;   // application + constructed
  LDAP_ABANDON_CMD       = $50;   // application + primitive
  LDAP_SESSION_CMD       = $71;   // application + constructed
  LDAP_EXTENDED_CMD      = $77;   // application + constructed

//
// Responses/Results for LDAP v2 & v3
//

  LDAP_RES_BIND           = $61;   // application + constructed
  LDAP_RES_SEARCH_ENTRY   = $64;   // application + constructed
  LDAP_RES_SEARCH_RESULT  = $65;   // application + constructed
  LDAP_RES_MODIFY         = $67;   // application + constructed
  LDAP_RES_ADD            = $69;   // application + constructed
  LDAP_RES_DELETE         = $6b;   // application + constructed
  LDAP_RES_MODRDN         = $6d;   // application + constructed
  LDAP_RES_COMPARE        = $6f;   // application + constructed
  LDAP_RES_SESSION        = $72;   // application + constructed
  LDAP_RES_REFERRAL       = $73;   // application + constructed
  LDAP_RES_EXTENDED       = $78;   // application + constructed
  LDAP_RES_ANY            = -1;

  LDAP_INVALID_CMD         = $ff;
  LDAP_INVALID_RES         = $ff;


//
// We'll make the error codes compatible with reference implementation
//

  LDAP_SUCCESS                    =   $00;
  LDAP_OPERATIONS_ERROR           =   $01;
  LDAP_PROTOCOL_ERROR             =   $02;
  LDAP_TIMELIMIT_EXCEEDED         =   $03;
  LDAP_SIZELIMIT_EXCEEDED         =   $04;
  LDAP_COMPARE_FALSE              =   $05;
  LDAP_COMPARE_TRUE               =   $06;
  LDAP_AUTH_METHOD_NOT_SUPPORTED  =   $07;
  LDAP_STRONG_AUTH_REQUIRED       =   $08;
  LDAP_REFERRAL_V2                =   $09;
  LDAP_PARTIAL_RESULTS            =   $09;
  LDAP_REFERRAL                   =   $0a;
  LDAP_ADMIN_LIMIT_EXCEEDED       =   $0b;
  LDAP_UNAVAILABLE_CRIT_EXTENSION =   $0c;

  LDAP_NO_SUCH_ATTRIBUTE          =   $10;
  LDAP_UNDEFINED_TYPE             =   $11;
  LDAP_INAPPROPRIATE_MATCHING     =   $12;
  LDAP_CONSTRAINT_VIOLATION       =   $13;
  LDAP_ATTRIBUTE_OR_VALUE_EXISTS  =   $14;
  LDAP_INVALID_SYNTAX             =   $15;

  LDAP_NO_SUCH_OBJECT             =   $20;
  LDAP_ALIAS_PROBLEM              =   $21;
  LDAP_INVALID_DN_SYNTAX          =   $22;
  LDAP_IS_LEAF                    =   $23;
  LDAP_ALIAS_DEREF_PROBLEM        =   $24;

  LDAP_INAPPROPRIATE_AUTH         =   $30;
  LDAP_INVALID_CREDENTIALS        =   $31;
  LDAP_INSUFFICIENT_RIGHTS        =   $32;
  LDAP_BUSY                       =   $33;
  LDAP_UNAVAILABLE                =   $34;
  LDAP_UNWILLING_TO_PERFORM       =   $35;
  LDAP_LOOP_DETECT                =   $36;

  LDAP_NAMING_VIOLATION           =   $40;
  LDAP_OBJECT_CLASS_VIOLATION     =   $41;
  LDAP_NOT_ALLOWED_ON_NONLEAF     =   $42;
  LDAP_NOT_ALLOWED_ON_RDN         =   $43;
  LDAP_ALREADY_EXISTS             =   $44;
  LDAP_NO_OBJECT_CLASS_MODS       =   $45;
  LDAP_RESULTS_TOO_LARGE          =   $46;
  LDAP_AFFECTS_MULTIPLE_DSAS      =   $47;

  LDAP_OTHER                      =   $50;
  LDAP_SERVER_DOWN                =   $51;
  LDAP_LOCAL_ERROR                =   $52;
  LDAP_ENCODING_ERROR             =   $53;
  LDAP_DECODING_ERROR             =   $54;
  LDAP_TIMEOUT                    =   $55;
  LDAP_AUTH_UNKNOWN               =   $56;
  LDAP_FILTER_ERROR               =   $57;
  LDAP_USER_CANCELLED             =   $58;
  LDAP_PARAM_ERROR                =   $59;
  LDAP_NO_MEMORY                  =   $5a;

type
  LDAP_RETCODE = integer;

//
//  Bind methods.  We support the following methods :
//
//      Simple         Clear text password... try not to use as it's not secure.
//
//      MSN            MSN(Microsoft Network)authentication. This package
//                     may bring up UI to prompt the user for MSN credentials.
//
//      DPA            Normandy authentication... new MSN authentication.  Same
//                     usage as MSN.
//
//      NTLM           NT domain authentication.  Use NULL credentials and
//                     we'll try to use default logged in user credentials.
//
//      Sicily         Negotiate with the server for any of: MSN, DPA, NTLM
//
//      SSPI           Use GSSAPI Negotiate package to negotiate security
//                     package of either Kerberos v5 or NTLM(or any other
//                     package the client and server negotiate).  Pass in
//                     NULL credentials to specify default logged in user or
//                     you may pass in a SEC_WINNT_AUTH_IDENTITY_W(defined
//                     in rpcdce.h)to specify alternate credentials.
//
//  All bind methods other than simple are synchronous only calls.
//  Calling the asynchronous bind call for any of these messages will
//  return LDAP_PARAM_ERROR.
//
//  Using any other method besides simple will cause WLDAP32 to pull in
//  the SSPI security DLLs(SECURITY.DLL etc).
//
//  On NTLM and SSPI, if you specify NULL credentials, we'll attempt to use
//  the default logged in user.
//

const
  LDAP_AUTH_SIMPLE                = $80;
  LDAP_AUTH_SASL                  = $83;

  LDAP_AUTH_OTHERKIND             = $86;

// The SICILY type covers package negotiation to MSN servers.
// Each of the supported types can also be specified without
// doing the package negotiation, assuming the caller knows
// what the server supports.

  LDAP_AUTH_SICILY                =(LDAP_AUTH_OTHERKIND or $0200);

  LDAP_AUTH_MSN                   =(LDAP_AUTH_OTHERKIND or $0800);
  LDAP_AUTH_NTLM                  =(LDAP_AUTH_OTHERKIND or $1000);
  LDAP_AUTH_DPA                   =(LDAP_AUTH_OTHERKIND or $2000);

// This will cause the client to use the GSSAPI negotiation
// package to determine the most appropriate authentication type.
// This type should be used when talking to NT5.

  LDAP_AUTH_SSPI                  =(LDAP_AUTH_OTHERKIND or $0400);

//
//  Client applications typically don't have to encode/decode LDAP filters,
//  but if they do, we define the operators here.
//
//  Filter types.

  LDAP_FILTER_AND         = $a0;    // context specific + constructed - SET OF Filters.
  LDAP_FILTER_OR          = $a1;    // context specific + constructed - SET OF Filters.
  LDAP_FILTER_NOT         = $a2;    // context specific + constructed - Filter
  LDAP_FILTER_EQUALITY    = $a3;    // context specific + constructed - AttributeValueAssertion.
  LDAP_FILTER_SUBSTRINGS  = $a4;    // context specific + constructed - SubstringFilter
  LDAP_FILTER_GE          = $a5;    // context specific + constructed - AttributeValueAssertion.
  LDAP_FILTER_LE          = $a6;    // context specific + constructed - AttributeValueAssertion.
  LDAP_FILTER_PRESENT     = $87;    // context specific + primitive   - AttributeType.
  LDAP_FILTER_APPROX      = $a8;    // context specific + constructed - AttributeValueAssertion.

//  Substring filter types

  LDAP_SUBSTRING_INITIAL  = $80;   // class context specific
  LDAP_SUBSTRING_ANY      = $81;   // class context specific
  LDAP_SUBSTRING_FINAL    = $82;   // class context specific

//
//  Possible values for ld_deref field.
//      "Never"     - never deref aliases.  return only the alias.
//      "Searching" - only deref aliases when searching, not when locating
//                    the base object of a search.
//      "Finding"   - dereference the alias when locating the base object but
//                    not during a search.
//      "Always"    - always dereference aliases.
//

  LDAP_DEREF_NEVER        = 0;
  LDAP_DEREF_SEARCHING    = 1;
  LDAP_DEREF_FINDING      = 2;
  LDAP_DEREF_ALWAYS       = 3;

//  Special values for ld_sizelimit :

  LDAP_NO_LIMIT       = 0;

//  Flags for ld_options field :

  LDAP_OPT_DNS                = $00000001;  // utilize DN & DNS
  LDAP_OPT_CHASE_REFERRALS    = $00000002;  // chase referrals
  LDAP_OPT_RETURN_REFS        = $00000004;  // return referrals to calling app

//
//  LDAP structure per connection
//

// XXX #pragma pack(push, 4)
{$ALIGN ON}

type
  PLDAP = ^LDAP;
  LDAP = record

    ld_sb: record
      sb_sd: ULONG;
      Reserved1: array [0..(10*sizeof(ULONG))] of UCHAR;
      sb_naddr: ULONG;   // notzero implies CLDAP available
      Reserved2: array [0..(6*sizeof(ULONG))] of UCHAR;
    end;


    //
    //  Following parameters MAY match up to reference implementation of LDAP
    //

    ld_host: PCHAR;
    ld_version: ULONG;
    ld_lberoptions: UCHAR;

    //
    //  Safe to assume that these parameters are in same location as
    //  reference implementation of LDAP API.
    //

    ld_deref: ULONG;

    ld_timelimit: ULONG;
    ld_sizelimit: ULONG;

    ld_errno: ULONG;
    ld_matched: PCHAR;
    ld_error: PCHAR;
    ld_msgid: ULONG;

    Reserved3: array  [0..(6*sizeof(ULONG))] of UCHAR;

    //
    //  Following parameters may match up to reference implementation of LDAP API.
    //

    ld_cldaptries: ULONG;
    ld_cldaptimeout: ULONG;
    ld_refhoplimit: ULONG;
    ld_options: ULONG;
  end;

//
//  Our timeval structure is a bit different from the reference implementation
//  since Win32 defines a _timeval structure that is different from the LDAP
//  one.
//

  PLDAP_TIMEVAL = ^LDAP_TIMEVAL;
  LDAP_TIMEVAL = record
    tv_sec: LONG;
    tv_usec: LONG;
  end;

//
//  The berval structure is used to pass in any arbitrary octet string.  It
//  is useful for attributes that cannot be represented using a null
//  terminated string.
//

  PPLDAP_BERVAL = ^PLDAP_BERVAL;
  PLDAP_BERVAL = ^LDAP_BERVAL;
  LDAP_BERVAL = record
    bv_len: ULONG;
    bv_val: PCHAR;
  end;

//
//  The following structure has to be compatible with reference implementation.
//

  PPLDAPMessage = ^PLDAPMessage;
  PLDAPMessage = ^LDAPMessage;
  LDAPMessage = record
    lm_msgid: ULONG;             // message number for given connection
    lm_msgtype: ULONG;           // message type of the form LDAP_RES_xxx

    lm_ber: PVOID;               // ber form of message

    lm_chain: PLDAPMessage;      // pointer to next result value
    lm_next: PLDAPMessage;       // pointer to next message
    lm_time: ULONG;

    //
    //  new fields below not in reference implementation
    //

    Connection: PLDAP;           // connection from which we received response
    Request: PVOID;              // owning request(opaque structure)
    lm_returncode: ULONG;        // server's return code
    lm_hopcount: USHORT;         // hop count for number of referrals followed
  end;

//
//  Values required for Modification command  These are options for the
//  mod_op field of LDAPMod structure
//

const
  LDAP_MOD_ADD            = $00;
  LDAP_MOD_DELETE         = $01;
  LDAP_MOD_REPLACE        = $02;
  LDAP_MOD_BVALUES        = $80;

type
  PLDAPModW = ^LDAPModW;
  LDAPModW = record
    mod_op: ULONG;
    mod_type: PWCHAR;
    case integer of
      0:(modv_strvals: ^PWCHAR);
      1:(modv_bvals: ^PLDAP_BERVAL);
  end;

  PLDAPModA = ^LDAPModA;
  LDAPModA = record
    mod_op: ULONG;
    mod_type: PCHAR;
    modvals: record
      case integer of
        0:(modv_strvals: ^PCHAR);
        1:(modv_bvals: ^PLDAP_BERVAL);
    end;
  end;

{$IFDEF LDAP_UNICODE}
  LDAPMod = LDAPModW;
  PLDAPMod = PLDAPModW;
{$ELSE}
  LDAPMod = LDAPModA;
  PLDAPMod = PLDAPModA;
{$ENDIF}

// XXX #pragma pack(pop)
{$ALIGN OFF}

//
//  macros compatible with reference implementation...
//

function LDAP_IS_CLDAP(ld: PLDAP): boolean;
function NAME_ERROR(n: integer): boolean;

//
//  function definitions for LDAP API
//

//
//  Create a connection block to an LDAP server.  HostName can be NULL, in
//  which case we'll try to go off and find the "default" LDAP server.
//
//  Note that if we have to go off and find the default server, we'll pull
//  in NETAPI32.DLL and ADVAPI32.DLL.
//
//  If it returns NULL, an error occurred.  Pick up error code with
//     GetLastError().
//
//  ldap_open actually opens the connection at the time of the call,
//  whereas ldap_init only opens the connection when an operation is performed
//  that requires it.

function ldap_openW(HostName: PWCHAR; PortNumber: ULONG): PLDAP; cdecl;
function ldap_openA(HostName: PCHAR; PortNumber: ULONG): PLDAP; cdecl;

function ldap_initW(HostName: PWCHAR; PortNumber: ULONG): PLDAP; cdecl;
function ldap_initA(HostName: PCHAR; PortNumber: ULONG): PLDAP; cdecl;

function ldap_sslinitW(HostName: PWCHAR; PortNumber: ULONG; secure: integer): PLDAP; cdecl;
function ldap_sslinitA(HostName: PCHAR; PortNumber: ULONG; secure: integer): PLDAP; cdecl;

{$IFDEF LDAP_UNICODE}
function ldap_open(HostName: PWCHAR; PortNumber: ULONG): PLDAP; cdecl;
function ldap_init(HostName: PWCHAR; PortNumber: ULONG): PLDAP; cdecl;
function ldap_sslinit(HostName: PWCHAR; PortNumber: ULONG; secure: integer): PLDAP; cdecl;
{$ELSE}
function ldap_open(HostName: PCHAR; PortNumber: ULONG): PLDAP; cdecl;
function ldap_init(HostName: PCHAR; PortNumber: ULONG): PLDAP; cdecl;
function ldap_sslinit(HostName: PCHAR; PortNumber: ULONG; secure: integer): PLDAP; cdecl;
{$ENDIF}

//
//  This is similar to ldap_open except it creates a connection block for
//  UDP based Connectionless LDAP services.  No TCP session is maintained.
//
//  If it returns NULL, an error occurred.  Pick up error code with
//     GetLastError().
//

function cldap_openW(HostName: PWCHAR; PortNumber: ULONG): PLDAP; cdecl;
function cldap_openA(HostName: PCHAR; PortNumber: ULONG): PLDAP; cdecl;

{$IFDEF LDAP_UNICODE}
function cldap_open(HostName: PWCHAR; PortNumber: ULONG): PLDAP; cdecl;
{$ELSE}
function cldap_open(HostName: PCHAR; PortNumber: ULONG): PLDAP; cdecl;
{$ENDIF}


//
//  Call unbind when you're done with the connection, it will free all
//  resources associated with the connection.
//
//  There is no ldap_close... use ldap_unbind even if you haven't called
//  ldap_bind on the connection.
//

function ldap_unbind(ld: PLDAP): ULONG; cdecl;
function ldap_unbind_s(ld: PLDAP): ULONG; cdecl; // calls ldap_unbind

//
//  Calls to get and set options on connection blocks... use them rather
//  than modifying the LDAP block directly.
//

function ldap_get_option(ld: PLDAP; option: integer; outvalue: pointer): ULONG; cdecl;
function ldap_set_option(ld: PLDAP; option: integer; invalue: pointer): ULONG; cdecl;

//
//  These are the values to pass to ldap_get/set_option :
//

const
  LDAP_OPT_DESC               = $01;
  LDAP_OPT_DEREF              = $02;
  LDAP_OPT_SIZELIMIT          = $03;
  LDAP_OPT_TIMELIMIT          = $04;
  LDAP_OPT_THREAD_FN_PTRS     = $05;
  LDAP_OPT_REBIND_FN          = $06;
  LDAP_OPT_REBIND_ARG         = $07;
  LDAP_OPT_REFERRALS          = $08;
  LDAP_OPT_RESTART            = $09;

  LDAP_OPT_IO_FN_PTRS         = $0a;
  LDAP_OPT_CACHE_FN_PTRS      = $0c;
  LDAP_OPT_CACHE_STRATEGY     = $0d;
  LDAP_OPT_CACHE_ENABLE       = $0e;
  LDAP_OPT_SSL                = $0f;
  LDAP_OPT_VERSION            = $10;
  LDAP_OPT_SORTKEYS           = $11;

//
//  These are new ones that we've defined, not in current RFC draft.
//

  LDAP_OPT_HOST_NAME          = $30;
  LDAP_OPT_ERROR_NUMBER       = $31;
  LDAP_OPT_ERROR_STRING       = $32;


  LDAP_OPT_ON                 = pointer(1);
  LDAP_OPT_OFF                = pointer(0);



//
//  Bind is required as the first operation to v2 servers, not so for v3
//  servers.  See above description of authentication methods.
//

function ldap_simple_bindW(ld: PLDAP; dn: PWCHAR; passwd: PWCHAR): ULONG; cdecl;
function ldap_simple_bindA(ld: PLDAP; dn: PCHAR; passwd: PCHAR): ULONG; cdecl;
function ldap_simple_bind_sW(ld: PLDAP; dn: PWCHAR; passwd: PWCHAR): ULONG; cdecl;
function ldap_simple_bind_sA(ld: PLDAP; dn: PCHAR; passwd: PCHAR): ULONG; cdecl;

function ldap_bindW(ld: PLDAP; dn: PWCHAR; cred: PWCHAR; method: ULONG): ULONG; cdecl;
function ldap_bindA(ld: PLDAP; dn: PCHAR; cred: PCHAR; method: ULONG): ULONG; cdecl;
function ldap_bind_sW(ld: PLDAP; dn: PWCHAR; cred: PWCHAR; method: ULONG): ULONG; cdecl;
function ldap_bind_sA(ld: PLDAP; dn: PCHAR; cred: PCHAR; method: ULONG): ULONG; cdecl;

{$IFDEF LDAP_UNICODE}
function ldap_simple_bind(ld: PLDAP; dn: PWCHAR; passwd: PWCHAR): ULONG; cdecl;
function ldap_simple_bind_s(ld: PLDAP; dn: PWCHAR; passwd: PWCHAR): ULONG; cdecl;
function ldap_bind(ld: PLDAP; dn: PWCHAR; cred: PWCHAR; method: ULONG): ULONG; cdecl;
function ldap_bind_s(ld: PLDAP; dn: PWCHAR; cred: PWCHAR; method: ULONG): ULONG; cdecl;
{$ELSE}
function ldap_simple_bind(ld: PLDAP; dn: PCHAR; passwd: PCHAR): ULONG; cdecl;
function ldap_simple_bind_s(ld: PLDAP; dn: PCHAR; passwd: PCHAR): ULONG; cdecl;
function ldap_bind(ld: PLDAP; dn: PCHAR; cred: PCHAR; method: ULONG): ULONG; cdecl;
function ldap_bind_s(ld: PLDAP; dn: PCHAR; cred: PCHAR; method: ULONG): ULONG; cdecl;
{$ENDIF}

//
//  Synchronous and asynch search routines.
//
//  filter follows RFC 1960 with the addition that '(' ')' '*' ' ' '\' and
//   '\0' are all escaped with '\'
//
// Scope of search.  This corresponds to the "scope" parameter on search

const
  LDAP_SCOPE_BASE         = $00;
  LDAP_SCOPE_ONELEVEL     = $01;
  LDAP_SCOPE_SUBTREE      = $02;

function ldap_searchW(
        ld:       PLDAP;
        base:     PWCHAR;        // distinguished name or ""
        scope:    ULONG;         // LDAP_SCOPE_xxxx
        filter:   PWCHAR;
        attrs:    PWCHAR;        // pointer to an array of PCHAR attribute names
        attrsonly: ULONG         // boolean on whether to only return attr names
   ): ULONG; cdecl;
function ldap_searchA(
        ld:       PLDAP;
        base:     PCHAR;         // distinguished name or ""
        scope:    ULONG;         // LDAP_SCOPE_xxxx
        filter:   PCHAR;
        attrs:    PCHAR;         // pointer to an array of PCHAR attribute names
        attrsonly: ULONG         // boolean on whether to only return attr names
   ): ULONG; cdecl;

function ldap_search_sW(
        ld:        PLDAP;
        base:      PWCHAR;
        scope:     ULONG;
        filter:    PWCHAR;
        attrs:     PWCHAR;
        attrsonly: ULONG;
        res:       PPLDAPMessage
   ): ULONG; cdecl;
function ldap_search_sA(
        ld:        PLDAP;
        base:      PCHAR;
        scope:     ULONG;
        filter:    PCHAR;
        attrs:     PCHAR;
        attrsonly: ULONG;
        res:       PPLDAPMessage
   ): ULONG; cdecl;

function ldap_search_stW(
        ld:         PLDAP;
        base:       PWCHAR;
        scope:      ULONG;
        filter:     PWCHAR;
        attrs:      PWCHAR;
        attrsonly:  ULONG;
        timeout:    PLDAP_TIMEVAL;
        res:        PPLDAPMessage
   ): ULONG; cdecl;
function ldap_search_stA(
        ld:         PLDAP;
        base:       PCHAR;
        scope:      ULONG;
        filter:     PCHAR;
        attrs:      PCHAR;
        attrsonly:  ULONG;
        timeout:    PLDAP_TIMEVAL;
        res:        PPLDAPMessage
   ): ULONG; cdecl;

{$IFDEF LDAP_UNICODE}
function ldap_search(
        ld:        PLDAP;
        base:      PWCHAR;        // distinguished name or ""
        scope:     ULONG;         // LDAP_SCOPE_xxxx
        filter:    PWCHAR;
        attrs:     PWCHAR;        // pointer to an array of PCHAR attribute names
        attrsonly: ULONG          // boolean on whether to only return attr names
   ): ULONG; cdecl;
function ldap_search_s(
        ld:        PLDAP;
        base:      PWCHAR;
        scope:     ULONG;
        filter:    PWCHAR;
        attrs:     PWCHAR;
        attrsonly: ULONG;
        res:       PPLDAPMessage
   ): ULONG; cdecl;
function ldap_search_st(
        ld:        PLDAP;
        base:      PWCHAR;
        scope:     ULONG;
        filter:    PWCHAR;
        attrs:     PWCHAR;
        attrsonly: ULONG;
        timeout:   PLDAP_TIMEVAL;
        res:       PPLDAPMessage
   ): ULONG; cdecl;
{$ELSE}
function ldap_search(
        ld:        PLDAP;
        base:      PCHAR;         // distinguished name or ""
        scope:     ULONG;         // LDAP_SCOPE_xxxx
        filter:    PCHAR;
        attrs:     PCHAR;         // pointer to an array of PCHAR attribute names
        attrsonly: ULONG          // boolean on whether to only return attr names
   ): ULONG; cdecl;
function ldap_search_s(
        ld:        PLDAP;
        base:      PCHAR;
        scope:     ULONG;
        filter:    PCHAR;
        attrs:     PCHAR;
        attrsonly: ULONG;
        res:       PPLDAPMessage
   ): ULONG; cdecl;
function ldap_search_st(
        ld:        PLDAP;
        base:      PCHAR;
        scope:     ULONG;
        filter:    PCHAR;
        attrs:     PCHAR;
        attrsonly: ULONG;
        timeout:   PLDAP_TIMEVAL;
        res:       PPLDAPMessage
   ): ULONG; cdecl;
{$ENDIF}

//
//  modify an existing entry
//

function ldap_modifyW(ld: PLDAP; dn: PWCHAR; mods: PLDAPModW): ULONG; cdecl;
function ldap_modifyA(ld: PLDAP; dn: PCHAR; mods: PLDAPModA): ULONG; cdecl;

function ldap_modify_sW(ld: PLDAP; dn: PWCHAR; mods: PLDAPModW): ULONG; cdecl;
function ldap_modify_sA(ld: PLDAP; dn: PCHAR; mods: PLDAPModA): ULONG; cdecl;

{$IFDEF LDAP_UNICODE}
function ldap_modify(ld: PLDAP; dn: PWCHAR; mods: PLDAPModW): ULONG; cdecl;
function ldap_modify_s(ld: PLDAP; dn: PWCHAR; mods: PLDAPModW): ULONG; cdecl;
{$ELSE}
function ldap_modify(ld: PLDAP; dn: PCHAR; mods: PLDAPModA): ULONG; cdecl;
function ldap_modify_s(ld: PLDAP; dn: PCHAR; mods: PLDAPModA): ULONG; cdecl;
{$ENDIF}



//
//  modrdn and modrdn2 function both as RenameObject and MoveObject.
//
//  Note that to LDAP v2 servers, only rename within a given container
//  is supported... therefore NewDistinguishedName is actually NewRDN.
//  Here are some examples :
//
//  This works to both v2 and v3 servers :
//
//    DN = CN=Bob,OU=FOO,O=BAR
//    NewDN = CN=Joe
//
//    result is: CN=Joe,OU=FOO,O=BAR
//
//  This works to only v3 and above servers :
//
//    DN = CN=Bob,OU=FOO,O=BAR
//    NewDN = CN=Joe,OU=FOOBAR,O=BAR
//
//    result is: CN=Joe,OU=FOOBAR,O=BAR
//
//  If you try the second example to a v2 server, we'll send the whole
//  NewDN over as the new RDN(rather than break up the parent OU and
//  child).  The server will then give you back some unknown error.
//

function ldap_modrdn2W(
    ExternalHandle:       PLDAP;
    DistinguishedName:    PWCHAR;
    NewDistinguishedName: PWCHAR;
    DeleteOldRdn:         INT
   ): ULONG; cdecl;
function ldap_modrdn2A(
    ExternalHandle:       PLDAP;
    DistinguishedName:    PCHAR;
    NewDistinguishedName: PCHAR;
    DeleteOldRdn:         INT
   ): ULONG; cdecl;

//
//  ldap_modrdn simply calls ldap_modrdn2 with a value of 1 for DeleteOldRdn.
//

function ldap_modrdnW(
    ExternalHandle:       PLDAP;
    DistinguishedName:    PWCHAR;
    NewDistinguishedName: PWCHAR
   ): ULONG; cdecl;
function ldap_modrdnA(
    ExternalHandle:       PLDAP;
    DistinguishedName:    PCHAR;
    NewDistinguishedName: PCHAR
   ): ULONG; cdecl;

function ldap_modrdn2_sW(
    ExternalHandle:       PLDAP;
    DistinguishedName:    PWCHAR;
    NewDistinguishedName: PWCHAR;
    DeleteOldRdn:         INT
   ): ULONG; cdecl;
function ldap_modrdn2_sA(
    ExternalHandle:       PLDAP;
    DistinguishedName:    PCHAR;
    NewDistinguishedName: PCHAR;
    DeleteOldRdn:         INT
   ): ULONG; cdecl;

function ldap_modrdn_sW(
    ExternalHandle:       PLDAP;
    DistinguishedName:    PWCHAR;
    NewDistinguishedName: PWCHAR
   ): ULONG; cdecl;
function ldap_modrdn_sA(
    ExternalHandle:       PLDAP;
    DistinguishedName:    PCHAR;
    NewDistinguishedName: PCHAR
   ): ULONG; cdecl;

{$IFDEF LDAP_UNICODE}
function ldap_modrdn2(
    ExternalHandle:       PLDAP;
    DistinguishedName:    PWCHAR;
    NewDistinguishedName: PWCHAR;
    DeleteOldRdn:         INT
   ): ULONG; cdecl;
function ldap_modrdn(
    ExternalHandle:       PLDAP;
    DistinguishedName:    PWCHAR;
    NewDistinguishedName: PWCHAR
   ): ULONG; cdecl;
function ldap_modrdn2_s(
    ExternalHandle:       PLDAP;
    DistinguishedName:    PWCHAR;
    NewDistinguishedName: PWCHAR;
    DeleteOldRdn:         INT
   ): ULONG; cdecl;
function ldap_modrdn_s(
    ExternalHandle:       PLDAP;
    DistinguishedName:    PWCHAR;
    NewDistinguishedName: PWCHAR
   ): ULONG; cdecl;
{$ELSE}
function ldap_modrdn2(
    ExternalHandle:       PLDAP;
    DistinguishedName:    PCHAR;
    NewDistinguishedName: PCHAR;
    DeleteOldRdn:         INT
   ): ULONG; cdecl;
function ldap_modrdn(
    ExternalHandle:       PLDAP;
    DistinguishedName:    PCHAR;
    NewDistinguishedName: PCHAR
   ): ULONG; cdecl;
function ldap_modrdn2_s(
    ExternalHandle:       PLDAP;
    DistinguishedName:    PCHAR;
    NewDistinguishedName: PCHAR;
    DeleteOldRdn:         INT
   ): ULONG; cdecl;
function ldap_modrdn_s(
    ExternalHandle:       PLDAP;
    DistinguishedName:    PCHAR;
    NewDistinguishedName: PCHAR
   ): ULONG; cdecl;
{$ENDIF}



//
//  Add an entry to the tree
//

function ldap_addW(ld: PLDAP; dn: PWCHAR; attrs: PLDAPModW): ULONG; cdecl;
function ldap_addA(ld: PLDAP; dn: PCHAR; attrs: PLDAPModA): ULONG; cdecl;

function ldap_add_sW(ld: PLDAP; dn: PWCHAR; attrs: PLDAPModW): ULONG; cdecl;
function ldap_add_sA(ld: PLDAP; dn: PCHAR; attrs: PLDAPModA): ULONG; cdecl;

{$IFDEF LDAP_UNICODE}
function ldap_add(ld: PLDAP; dn: PWCHAR; attrs: PLDAPModW): ULONG; cdecl;
function ldap_add_s(ld: PLDAP; dn: PWCHAR; attrs: PLDAPModW): ULONG; cdecl;
{$ELSE}
function ldap_add(ld: PLDAP; dn: PCHAR; attrs: PLDAPModA): ULONG; cdecl;
function ldap_add_s(ld: PLDAP; dn: PCHAR; attrs: PLDAPModA): ULONG; cdecl;
{$ENDIF}

//
//  Compare the attribute for a given entry to a known value.
//

function ldap_compareW(ld: PLDAP; dn: PWCHAR; attr: PWCHAR; value: PWCHAR): ULONG; cdecl;
function ldap_compareA(ld: PLDAP; dn: PCHAR; attr: PCHAR; value: PCHAR): ULONG; cdecl;

function ldap_compare_sW(ld: PLDAP; dn: PWCHAR; attr: PWCHAR; value: PWCHAR): ULONG; cdecl;
function ldap_compare_sA(ld: PLDAP; dn: PCHAR; attr: PCHAR; value: PCHAR): ULONG; cdecl;

{$IFDEF LDAP_UNICODE}
function ldap_compare(ld: PLDAP; dn: PWCHAR; attr: PWCHAR; value: PWCHAR): ULONG; cdecl;
function ldap_compare_s(ld: PLDAP; dn: PWCHAR; attr: PWCHAR; value: PWCHAR): ULONG; cdecl;
{$ELSE}
function ldap_compare(ld: PLDAP; dn: PCHAR; attr: PCHAR; value: PCHAR): ULONG; cdecl;
function ldap_compare_s(ld: PLDAP; dn: PCHAR; attr: PCHAR; value: PCHAR): ULONG; cdecl;
{$ENDIF}

//
//  Delete an object out of the tree
//

function ldap_deleteW(ld: PLDAP; dn: PWCHAR): ULONG; cdecl;
function ldap_deleteA(ld: PLDAP; dn: PCHAR): ULONG; cdecl;

function ldap_delete_sW(ld: PLDAP; dn: PWCHAR): ULONG; cdecl;
function ldap_delete_sA(ld: PLDAP; dn: PCHAR): ULONG; cdecl;

{$IFDEF LDAP_UNICODE}
function ldap_delete(ld: PLDAP; dn: PWCHAR): ULONG; cdecl;
function ldap_delete_s(ld: PLDAP; dn: PWCHAR): ULONG; cdecl;
{$ELSE}
function ldap_delete(ld: PLDAP; dn: PCHAR): ULONG; cdecl;
function ldap_delete_s(ld: PLDAP; dn: PCHAR): ULONG; cdecl;
{$ENDIF}

//
//  Give up on a request.  No guarentee that it got there as there is no
//  response from the server.
//

function ldap_abandon(ld: PLDAP; msgid: ULONG): ULONG; cdecl;



//
//  Possible values for "all" field in ldap_result.  We've enhanced it such
//  that if someone passes in LDAP_MSG_RECEIVED, we'll pass all values we've
//  received up to that point.
//

const
  LDAP_MSG_ONE       = 0;
  LDAP_MSG_ALL       = 1;
  LDAP_MSG_RECEIVED  = 2;

//
//  Get a response from a connection.  One enhancement here is that ld can
//  be null, in which case we'll return responses from any server.  Free
//  responses here with ldap_msgfree.
//
//  For connection-less LDAP, you should pass in both a LDAP connection
//  handle and a msgid.  This will ensure we know which request the app
//  is waiting on a reply to. (we actively resend request until we get
//  a response.)
//

function ldap_result(
        ld:       PLDAP;
        msgid:    ULONG;
        all:      ULONG;
        timeout:  PLDAP_TIMEVAL;
        res:      PPLDAPMessage
   ): ULONG; cdecl;

function ldap_msgfree(res: PLDAPMessage): ULONG; cdecl;

//
//  This parses a message and returns the error code.  It optionally frees
//  the message by calling ldap_msgfree.
//

function ldap_result2error(
        ld:      PLDAP;
        res:     PLDAPMessage;
        freeit:  ULONG             // boolean.. free the message?
   ): ULONG; cdecl;

//
//  ldap_err2string returns a pointer to a string describing the error.  This
//  string should not be freed.
//

function ldap_err2stringW(err: ULONG): PWCHAR; cdecl;
function ldap_err2stringA(err: ULONG): PCHAR; cdecl;

{$IFDEF LDAP_UNICODE}
function ldap_err2string(err: ULONG): PWCHAR; cdecl;
{$ELSE}
function ldap_err2string(err: ULONG): PCHAR; cdecl;
{$ENDIF}

//
//  ldap_perror does nothing and is here just for compatibility.
//

procedure ldap_perror(ld: PLDAP; msg: PCHAR); cdecl;

//
//  Return the first entry of a message.  It is freed when the message is
//  freed so should not be freed explicitly.
//

function ldap_first_entry(ld: PLDAP; res: PLDAPMessage): PLDAPMessage; cdecl;

//
//  Return the next entry of a message.  It is freed when the message is
//  freed so should not be freed explicitly.
//

function ldap_next_entry(ld: PLDAP; entry: PLDAPMessage): PLDAPMessage; cdecl;

//
//  Count the number of search entries returned by the server in a response
//  to a server request.
//

function ldap_count_entries(ld: PLDAP; res: PLDAPMessage): ULONG; cdecl;

//
//  A BerElement really maps out to a C++ class object that does BER encoding.
//  Don't mess with it as it's opaque.
//

type
  PBerElement = ^BerElement;
  BerElement = record
    opaque: PCHAR;      // this is an opaque structure used just for
                        // compatibility with reference implementation
  end;

const
  NULLBER = PBerElement(0);

//
//  For a given entry, return the first attribute.  The pointer returned is
//  actually a buffer in the connection block(with allowances for
//  multi-threaded apps)so it should not be freed.
//

function ldap_first_attributeW(
        ld:      PLDAP;
        entry:   PLDAPMessage;
        var ptr: PBerElement
       ): PWCHAR; cdecl;

function ldap_first_attributeA(
        ld:      PLDAP;
        entry:   PLDAPMessage;
        var ptr: PBerElement
       ): PCHAR; cdecl;

{$IFDEF LDAP_UNICODE}
function ldap_first_attribute(
        ld:      PLDAP;
        entry:   PLDAPMessage;
        var ptr: PBerElement
       ): PWCHAR; cdecl;
{$ELSE}
function ldap_first_attribute(
        ld:      PLDAP;
        entry:   PLDAPMessage;
        var ptr: PBerElement
       ): PCHAR; cdecl;
{$ENDIF}

//
//  Return the next attribute... again, the attribute pointer should not be
//  freed.
//

function ldap_next_attributeW(
        ld:      PLDAP;
        entry:   PLDAPMessage;
        ptr:     PBerElement
       ): PWCHAR; cdecl;

function ldap_next_attributeA(
        ld:      PLDAP;
        entry:   PLDAPMessage;
        ptr:     PBerElement
       ): PCHAR; cdecl;

{$IFDEF LDAP_UNICODE}
function ldap_next_attribute(
        ld:      PLDAP;
        entry:   PLDAPMessage;
        ptr:     PBerElement
       ): PWCHAR; cdecl;
{$ELSE}
function ldap_next_attribute(
        ld:      PLDAP;
        entry:   PLDAPMessage;
        ptr:     PBerElement
       ): PCHAR; cdecl;
{$ENDIF}

//
//  Get a given attribute's list of values.  This is used during parsing of
//  a search response.  It returns a list of pointers to values, the list is
//  null terminated.
//
//  If the values are generic octet strings and not null terminated strings,
//  use ldap_get_values_len instead.
//
//  The returned value should be freed when your done with it by calling
//  ldap_value_free.
//

function ldap_get_valuesW(
        ld:      PLDAP;
        entry:   PLDAPMessage;
        attr:    PWCHAR
       ): PPWCHAR; cdecl;
function ldap_get_valuesA(
        ld:      PLDAP;
        entry:   PLDAPMessage;
        attr:    PCHAR
       ): PPCHAR; cdecl;

{$IFDEF LDAP_UNICODE}
function ldap_get_values(
        ld:      PLDAP;
        entry:   PLDAPMessage;
        attr:    PWCHAR
       ): PPWCHAR; cdecl;
{$ELSE}
function ldap_get_values(
        ld:      PLDAP;
        entry:   PLDAPMessage;
        attr:    PCHAR
       ): PPCHAR; cdecl;
{$ENDIF}




//
//  Get a given attribute's list of values.  This is used during parsing of
//  a search response.  It returns a list of berval structures to values,
//  the list is null terminated.
//
//  If the values are null terminated strings, it may be easier to process them
//  by calling ldap_get_values instead.
//
//  The returned value should be freed when your done with it by calling
//  ldap_value_free_len.
//

function ldap_get_values_lenW(
    ExternalHandle:   PLDAP;
    Message:          PLDAPMessage;
    attr:             PWCHAR
   ): PPLDAP_BERVAL; cdecl;
function ldap_get_values_lenA(
    ExternalHandle:   PLDAP;
    Message:          PLDAPMessage;
    attr:             PCHAR
   ): PPLDAP_BERVAL; cdecl;

{$IFDEF LDAP_UNICODE}
function ldap_get_values_len(
    ExternalHandle:   PLDAP;
    Message:          PLDAPMessage;
    attr:             PWCHAR
   ): PPLDAP_BERVAL; cdecl;
{$ELSE}
function ldap_get_values_len(
    ExternalHandle:   PLDAP;
    Message:          PLDAPMessage;
    attr:             PCHAR
   ): PPLDAP_BERVAL; cdecl;
{$ENDIF}


//
//  Return the number of values in a list returned by ldap_get_values.
//

function ldap_count_valuesW(vals: PPWCHAR): ULONG; cdecl;
function ldap_count_valuesA(vals: PPCHAR): ULONG; cdecl;

{$IFDEF LDAP_UNICODE}
function ldap_count_values(vals: PPWCHAR): ULONG; cdecl;
{$ELSE}
function ldap_count_values(vals: PPCHAR): ULONG; cdecl;
{$ENDIF}



//
//  Return the number of values in a list returned by ldap_get_values_len.
//

function ldap_count_values_len(vals: PPLDAP_BERVAL): ULONG; cdecl;

//
//  Free structures returned by ldap_get_values.
//

function ldap_value_freeW(vals: PPWCHAR): ULONG; cdecl;
function ldap_value_freeA(vals: PPCHAR): ULONG; cdecl;

{$IFDEF LDAP_UNICODE}
function ldap_value_free(vals: PPWCHAR): ULONG; cdecl;
{$ELSE}
function ldap_value_free(vals: PPCHAR): ULONG; cdecl;
{$ENDIF}



//
//  Free structures returned by ldap_get_values_len.
//

function ldap_value_free_len(vals: PPLDAP_BERVAL): ULONG; cdecl;

//
//  Get the distinguished name for a given search entry.  It should be freed
//  by calling ldap_memfree.
//

function ldap_get_dnW(ld: PLDAP; entry: PLDAPMessage): PWCHAR; cdecl;
function ldap_get_dnA(ld: PLDAP; entry: PLDAPMessage): PCHAR; cdecl;

{$IFDEF LDAP_UNICODE}
function ldap_get_dn(ld: PLDAP; entry: PLDAPMessage): PWCHAR; cdecl;
{$ELSE}
function ldap_get_dn(ld: PLDAP; entry: PLDAPMessage): PCHAR; cdecl;
{$ENDIF}



//
//  When using ldap_explode_dn, you should free the returned string by
//  calling ldap_value_free.
//

function ldap_explode_dnW(dn: PCHAR; notypes: ULONG): PPWCHAR; cdecl;
function ldap_explode_dnA(dn: PCHAR; notypes: ULONG): PPCHAR; cdecl;

{$IFDEF LDAP_UNICODE}
function ldap_explode_dn(dn: PCHAR; notypes: ULONG): PPWCHAR; cdecl;
{$ELSE}
function ldap_explode_dn(dn: PCHAR; notypes: ULONG): PPCHAR; cdecl;
{$ENDIF}


//
//  When calling ldap_dn2ufn, you should free the returned string by calling
//  ldap_memfree.
//

function ldap_dn2ufnW(dn: PWCHAR): PWCHAR; cdecl;
function ldap_dn2ufnA(dn: PCHAR): PCHAR; cdecl;

{$IFDEF LDAP_UNICODE}
function ldap_dn2ufn(dn: PWCHAR): PWCHAR; cdecl;
{$ELSE}
function ldap_dn2ufn(dn: PCHAR): PCHAR; cdecl;
{$ENDIF}



//
//  This is used to free strings back to the LDAP API heap.  Don't pass in
//  values that you've gotten from ldap_open, ldap_get_values, etc.
//

procedure ldap_memfreeW(Block: PWCHAR); cdecl;
procedure ldap_memfreeA(Block: PCHAR); cdecl;

{$IFDEF LDAP_UNICODE}
procedure ldap_memfree(Block: PWCHAR); cdecl;
{$ELSE}
procedure ldap_memfree(Block: PCHAR); cdecl;
{$ENDIF}


//
//  The function ldap_ufn2dn attempts to "normalize" a user specified DN
//  to make it "proper".  It follows RFC 1781(add CN= if not present,
//  add OU= if none present, etc).  If it runs into any problems at all
//  while normalizing, it simply returns a copy of what was passed in.
//
//  It allocates the output string from the LDAP memory pool.  If the pDn
//  comes back as non-NULL, you should free it when you're done with a call
//  to ldap_memfree.
//

function ldap_ufn2dnW(
    ufn: PWCHAR;
    pDn: PPWCHAR
   ): ULONG; cdecl;
function ldap_ufn2dnA(
    ufn: PCHAR;
    pDn: PPCHAR
   ): ULONG; cdecl;

{$IFDEF LDAP_UNICODE}
function ldap_ufn2dn(
    ufn: PWCHAR;
    pDn: PPWCHAR
   ): ULONG; cdecl;
{$ELSE}
function ldap_ufn2dn(
    ufn: PCHAR;
    pDn: PPCHAR
   ): ULONG; cdecl;
{$ENDIF}

const
  LBER_USE_DER            = $01;
  LBER_USE_INDEFINITE_LEN = $02;
  LBER_TRANSLATE_STRINGS  = $04;

//
//  Call to initialize the LDAP library.  Pass in a version structure with
//  lv_size set to sizeof(LDAP_VERSION), lv_major set to LAPI_MAJOR_VER1,
//  and lv_minor set to LAPI_MINOR_VER1.  Return value will be either
//  LDAP_SUCCESS if OK or LDAP_OPERATIONS_ERROR if can't be supported.
//

  LAPI_MAJOR_VER1     = 1;
  LAPI_MINOR_VER1     = 1;

type
  PLDAP_VERSION_INFO = ^LDAP_VERSION_INFO;
  LDAP_VERSION_INFO = record
     lv_size: ULONG;
     lv_major: ULONG;
     lv_minor: ULONG;
  end;

function ldap_startup(
    version: PLDAP_VERSION_INFO
   ): ULONG; cdecl;

//
//  ldap_cleanup unloads the library when the refcount of opens goes to zero.
// (i.e. if a DLL calls it within a program that is also using it, it won't
//  free all resources)
//

function ldap_cleanup : ULONG; cdecl;

//
//  Extended API to support allowing opaque blobs of data in search filters.
//  This API takes any filter element and converts it to a text string that
//  can safely be passed in a search filter.  The attribute should have a
//  ";binary" appended to it.  An example of using this is :
//
//  filter is something like guid;binary=4826BF6CF0123444
//  this will put out on the wire guid of binary 0x4826BF6CF0123444
//

function ldap_escape_filter_elementW(
   sourceFilterElement:  PCHAR;
   sourceLength:         ULONG;
   destFilterElement:    PWCHAR;
   destLength:           ULONG
  ): ULONG; cdecl;
function ldap_escape_filter_elementA(
   sourceFilterElement:  PCHAR;
   sourceLength:         ULONG;
   destFilterElement:    PCHAR;
   destLength:           ULONG
  ): ULONG; cdecl;

{$IFDEF LDAP_UNICODE}
function ldap_escape_filter_element(
   sourceFilterElement:  PCHAR;
   sourceLength:         ULONG;
   destFilterElement:    PWCHAR;
   destLength:           ULONG
  ): ULONG; cdecl;
{$ELSE}
function ldap_escape_filter_element(
   sourceFilterElement:  PCHAR;
   sourceLength:         ULONG;
   destFilterElement:    PCHAR;
   destLength:           ULONG
  ): ULONG; cdecl;
{$ENDIF}

//
//  Misc extensions for additional debugging.
//
//  Note that these do nothing on free builds.
//

function ldap_set_dbg_flags(NewFlags: ULONG): ULONG; cdecl;

implementation

const
  sLDAPLIB = 'wldap32.dll';

{$IFDEF LDAP_UNICODE}
function ldap_open; external sLDAPLIB name 'ldap_openW';
function ldap_init; external sLDAPLIB name 'ldap_initW';
function ldap_sslinit; external sLDAPLIB name 'ldap_sslinitW';
function cldap_open; external sLDAPLIB name 'cldap_openW';
function ldap_simple_bind; external sLDAPLIB name 'ldap_simple_bindW';
function ldap_simple_bind_s; external sLDAPLIB name 'ldap_simple_bind_sW';
function ldap_bind; external sLDAPLIB name 'ldap_bindW';
function ldap_bind_s; external sLDAPLIB name 'ldap_bind_sW';
function ldap_search; external sLDAPLIB name 'ldap_searchW';
function ldap_search_s; external sLDAPLIB name 'ldap_search_sW';
function ldap_search_st; external sLDAPLIB name 'ldap_search_stW';
function ldap_modify; external sLDAPLIB name 'ldap_modifyW';
function ldap_modify_s; external sLDAPLIB name 'ldap_modify_sW';
function ldap_modrdn2; external sLDAPLIB name 'ldap_modrdn2W';
function ldap_modrdn; external sLDAPLIB name 'ldap_modrdnW';
function ldap_modrdn2_s; external sLDAPLIB name 'ldap_modrdn2_sW';
function ldap_modrdn_s; external sLDAPLIB name 'W';
function ldap_add; external sLDAPLIB name 'ldap_modrdn_sW';
function ldap_add_s; external sLDAPLIB name 'ldap_add_sW';
function ldap_compare; external sLDAPLIB name 'ldap_compareW';
function ldap_compare_s; external sLDAPLIB name 'ldap_compare_sW';
function ldap_delete; external sLDAPLIB name 'ldap_deleteW';
function ldap_delete_s; external sLDAPLIB name 'ldap_delete_sW';
function ldap_err2string; external sLDAPLIB name 'ldap_err2stringW';
function ldap_first_attribute; external sLDAPLIB name 'ldap_first_attributeW';
function ldap_next_attribute; external sLDAPLIB name 'ldap_next_attributeW';
function ldap_get_values; external sLDAPLIB name 'ldap_get_valuesW';
function ldap_get_values_len; external sLDAPLIB name 'ldap_get_values_lenW';
function ldap_count_values; external sLDAPLIB name 'ldap_count_valuesW';
function ldap_value_free; external sLDAPLIB name 'ldap_value_freeW';
function ldap_get_dn; external sLDAPLIB name 'ldap_get_dnW';
function ldap_explode_dn; external sLDAPLIB name 'ldap_explode_dnW';
function ldap_dn2ufn; external sLDAPLIB name 'ldap_dn2ufnW';
procedure ldap_memfree; external sLDAPLIB name 'ldap_memfreeW';
function ldap_ufn2dn; external sLDAPLIB name 'ldap_ufn2dnW';
function ldap_escape_filter_element; external sLDAPLIB name 'ldap_escape_filter_elementW';
{$ELSE}
function ldap_open; external sLDAPLIB name 'ldap_openA';
function ldap_init; external sLDAPLIB name 'ldap_initA';
function ldap_sslinit; external sLDAPLIB name 'ldap_sslinitA';
function cldap_open; external sLDAPLIB name 'cldap_openA';
function ldap_simple_bind; external sLDAPLIB name 'ldap_simple_bindA';
function ldap_simple_bind_s; external sLDAPLIB name 'ldap_simple_bind_sA';
function ldap_bind; external sLDAPLIB name 'ldap_bindA';
function ldap_bind_s; external sLDAPLIB name 'ldap_bind_sA';
function ldap_search; external sLDAPLIB name 'ldap_searchA';
function ldap_search_s; external sLDAPLIB name 'ldap_search_sA';
function ldap_search_st; external sLDAPLIB name 'ldap_search_stA';
function ldap_modify; external sLDAPLIB name 'ldap_modifyA';
function ldap_modify_s; external sLDAPLIB name 'ldap_modify_sA';
function ldap_modrdn2; external sLDAPLIB name 'ldap_modrdn2A';
function ldap_modrdn; external sLDAPLIB name 'ldap_modrdnA';
function ldap_modrdn2_s; external sLDAPLIB name 'ldap_modrdn2_sA';
function ldap_modrdn_s; external sLDAPLIB name 'A';
function ldap_add; external sLDAPLIB name 'ldap_modrdn_sA';
function ldap_add_s; external sLDAPLIB name 'ldap_add_sA';
function ldap_compare; external sLDAPLIB name 'ldap_compareA';
function ldap_compare_s; external sLDAPLIB name 'ldap_compare_sA';
function ldap_delete; external sLDAPLIB name 'ldap_deleteA';
function ldap_delete_s; external sLDAPLIB name 'ldap_delete_sA';
function ldap_err2string; external sLDAPLIB name 'ldap_err2stringA';
function ldap_first_attribute; external sLDAPLIB name 'ldap_first_attributeA';
function ldap_next_attribute; external sLDAPLIB name 'ldap_next_attributeA';
function ldap_get_values; external sLDAPLIB name 'ldap_get_valuesA';
function ldap_get_values_len; external sLDAPLIB name 'ldap_get_values_lenA';
function ldap_count_values; external sLDAPLIB name 'ldap_count_valuesA';
function ldap_value_free; external sLDAPLIB name 'ldap_value_freeA';
function ldap_get_dn; external sLDAPLIB name 'ldap_get_dnA';
function ldap_explode_dn; external sLDAPLIB name 'ldap_explode_dnA';
function ldap_dn2ufn; external sLDAPLIB name 'ldap_dn2ufnA';
procedure ldap_memfree; external sLDAPLIB name 'ldap_memfreeA';
function ldap_ufn2dn; external sLDAPLIB name 'ldap_ufn2dnA';
function ldap_escape_filter_element; external sLDAPLIB name 'ldap_escape_filter_elementA';
{$ENDIF}

function ldap_openW; external sLDAPLIB;
function ldap_openA; external sLDAPLIB;
function ldap_initW; external sLDAPLIB;
function ldap_initA; external sLDAPLIB;
function ldap_sslinitW; external sLDAPLIB;
function ldap_sslinitA; external sLDAPLIB;
function cldap_openW; external sLDAPLIB;
function cldap_openA; external sLDAPLIB;
function ldap_unbind; external sLDAPLIB;
function ldap_unbind_s; external sLDAPLIB;
function ldap_get_option; external sLDAPLIB;
function ldap_set_option; external sLDAPLIB;
function ldap_simple_bindW; external sLDAPLIB;
function ldap_simple_bindA; external sLDAPLIB;
function ldap_simple_bind_sW; external sLDAPLIB;
function ldap_simple_bind_sA; external sLDAPLIB;
function ldap_bindW; external sLDAPLIB;
function ldap_bindA; external sLDAPLIB;
function ldap_bind_sW; external sLDAPLIB;
function ldap_bind_sA; external sLDAPLIB;
function ldap_searchW; external sLDAPLIB;
function ldap_searchA; external sLDAPLIB;
function ldap_search_sW; external sLDAPLIB;
function ldap_search_sA; external sLDAPLIB;
function ldap_search_stW; external sLDAPLIB;
function ldap_search_stA; external sLDAPLIB;
function ldap_modifyW; external sLDAPLIB;
function ldap_modifyA; external sLDAPLIB;
function ldap_modify_sW; external sLDAPLIB;
function ldap_modify_sA; external sLDAPLIB;
function ldap_modrdn2W; external sLDAPLIB;
function ldap_modrdn2A; external sLDAPLIB;
function ldap_modrdnW; external sLDAPLIB;
function ldap_modrdnA; external sLDAPLIB;
function ldap_modrdn2_sW; external sLDAPLIB;
function ldap_modrdn2_sA; external sLDAPLIB;
function ldap_modrdn_sW; external sLDAPLIB;
function ldap_modrdn_sA; external sLDAPLIB;
function ldap_addW; external sLDAPLIB;
function ldap_addA; external sLDAPLIB;
function ldap_add_sW; external sLDAPLIB;
function ldap_add_sA; external sLDAPLIB;
function ldap_compareW; external sLDAPLIB;
function ldap_compareA; external sLDAPLIB;
function ldap_compare_sW; external sLDAPLIB;
function ldap_compare_sA; external sLDAPLIB;
function ldap_deleteW; external sLDAPLIB;
function ldap_deleteA; external sLDAPLIB;
function ldap_delete_sW; external sLDAPLIB;
function ldap_delete_sA; external sLDAPLIB;
function ldap_abandon; external sLDAPLIB;
function ldap_result; external sLDAPLIB;
function ldap_msgfree; external sLDAPLIB;
function ldap_result2error; external sLDAPLIB;
function ldap_err2stringW; external sLDAPLIB;
function ldap_err2stringA; external sLDAPLIB;
procedure ldap_perror; external sLDAPLIB;
function ldap_first_entry; external sLDAPLIB;
function ldap_next_entry; external sLDAPLIB;
function ldap_count_entries; external sLDAPLIB;
function ldap_first_attributeW; external sLDAPLIB;
function ldap_first_attributeA; external sLDAPLIB;
function ldap_next_attributeW; external sLDAPLIB;
function ldap_next_attributeA; external sLDAPLIB;
function ldap_get_valuesW; external sLDAPLIB;
function ldap_get_valuesA; external sLDAPLIB;
function ldap_get_values_lenW; external sLDAPLIB;
function ldap_get_values_lenA; external sLDAPLIB;
function ldap_count_valuesW; external sLDAPLIB;
function ldap_count_valuesA; external sLDAPLIB;
function ldap_count_values_len; external sLDAPLIB;
function ldap_value_freeW; external sLDAPLIB;
function ldap_value_freeA; external sLDAPLIB;
function ldap_value_free_len; external sLDAPLIB;
function ldap_get_dnW; external sLDAPLIB;
function ldap_get_dnA; external sLDAPLIB;
function ldap_explode_dnW; external sLDAPLIB;
function ldap_explode_dnA; external sLDAPLIB;
function ldap_dn2ufnW; external sLDAPLIB;
function ldap_dn2ufnA; external sLDAPLIB;
procedure ldap_memfreeW; external sLDAPLIB;
procedure ldap_memfreeA; external sLDAPLIB;
function ldap_ufn2dnW; external sLDAPLIB;
function ldap_ufn2dnA; external sLDAPLIB;
function ldap_startup; external sLDAPLIB;
function ldap_cleanup; external sLDAPLIB;
function ldap_escape_filter_elementW; external sLDAPLIB;
function ldap_escape_filter_elementA; external sLDAPLIB;
function ldap_set_dbg_flags; external sLDAPLIB;

function LDAP_IS_CLDAP(ld: PLDAP): boolean;
begin
  Result :=(ld^.ld_sb.sb_naddr > 0);
end;

function NAME_ERROR(n: integer): boolean;
begin
  Result :=((n and $f0) = $20);
end;

end.

