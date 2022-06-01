////////////////////////////////////////////////////////////////////////////////
// File        : VERSIONS.INC
// Description : Supply a better list of defines than simply VERxxx

// $Author: Lasse V. Karlsen $
// $Revision: 2 $
// $Date: 7.06.02 20:50 $
// $Archive: /Components/LVK/Source/Versions.inc $

////////////////////////////////////////////////////////////////////////////////
// First define the proper version define
{$UNDEF DELPHI1}	{$IFDEF VER80}	{$DEFINE DELPHI1}	{$ENDIF}
{$UNDEF DELPHI2}	{$IFDEF VER90}	{$DEFINE DELPHI2}	{$ENDIF}
{$UNDEF BCB1}			{$IFDEF VER93}	{$DEFINE BCB1}		{$ENDIF}
{$UNDEF DELPHI3}	{$IFDEF VER100}	{$DEFINE DELPHI3}	{$ENDIF}
{$UNDEF BCB3}			{$IFDEF VER110}	{$DEFINE BCB3}		{$ENDIF}
{$UNDEF DELPHI4}	{$IFDEF VER120}	{$DEFINE DELPHI4}	{$ENDIF}
{$UNDEF BCB4}			{$IFDEF VER125}	{$DEFINE BCB4}		{$ENDIF}
{$UNDEF DELPHI5}	{$IFDEF VER130}	{$DEFINE DELPHI5}	{$ENDIF}
{$UNDEF DELPHI6}	{$IFDEF VER140}	{$IFDEF MSWINDOWS}  {$DEFINE DELPHI6}	{$ENDIF} {$ENDIF}
{$UNDEF KYLIX1}   {$IFDEF VER140} {$IFDEF LINUX}      {$DEFINE KYLIX1}  {$ENDIF} {$ENDIF}
{$UNDEF DELPHI7}	{$IFDEF VER150}	{$DEFINE DELPHI7}	{$ENDIF}

{$IFDEF DELPHI1}	{$DEFINE DELPHI}
									{$DEFINE DELPHI1UP}	{$ENDIF}
{$IFDEF DELPHI2}	{$DEFINE DELPHI}
									{$DEFINE DELPHI1UP}
									{$DEFINE DELPHI2UP}	{$ENDIF}
{$IFDEF DELPHI3}	{$DEFINE DELPHI}
									{$DEFINE DELPHI1UP}
									{$DEFINE DELPHI2UP}
									{$DEFINE DELPHI3UP}	{$ENDIF}
{$IFDEF DELPHI4}	{$DEFINE DELPHI}
									{$DEFINE DELPHI1UP}
									{$DEFINE DELPHI2UP}
									{$DEFINE DELPHI3UP}
									{$DEFINE DELPHI4UP}	{$ENDIF}
{$IFDEF DELPHI5}	{$DEFINE DELPHI}
									{$DEFINE DELPHI1UP}
									{$DEFINE DELPHI2UP}
									{$DEFINE DELPHI3UP}
									{$DEFINE DELPHI4UP}
									{$DEFINE DELPHI5UP}	{$ENDIF}
{$IFDEF DELPHI6}	{$DEFINE DELPHI}
									{$DEFINE DELPHI1UP}
									{$DEFINE DELPHI2UP}
									{$DEFINE DELPHI3UP}
									{$DEFINE DELPHI4UP}
									{$DEFINE DELPHI5UP}
                  {$DEFINE DELPHI6UP} {$ENDIF}
{$IFDEF DELPHI7}	{$DEFINE DELPHI}
									{$DEFINE DELPHI1UP}
									{$DEFINE DELPHI2UP}
									{$DEFINE DELPHI3UP}
									{$DEFINE DELPHI4UP}
									{$DEFINE DELPHI5UP}
                  {$DEFINE DELPHI6UP}
                  {$DEFINE DELPHI7UP} {$ENDIF}
{$IFDEF KYLIX1}   {$DEFINE KYLIX}
                  {$DEFINE KYLIX1UP}  {$ENDIF}
{$IFDEF BCB1}			{$DEFINE BCB}
									{$DEFINE BCB1UP}		{$ENDIF}
{$IFDEF BCB3}			{$DEFINE BCB}
									{$DEFINE BCB1UP}
									{$DEFINE BCB3UP}		{$ENDIF}
{$IFDEF BCB4}			{$DEFINE BCB}
									{$DEFINE BCB1UP}
									{$DEFINE BCB3UP}
									{$DEFINE BCB4UP}		{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
// Require one of the 'known' compiler versions at this time
{$IFNDEF DELPHI}{$IFNDEF BCB} {$IFNDEF KYLIX}
	//This unit requires Delphi version 1.x-6.x, BCB version 1, 3 or 4 or Kylix 1
{$ENDIF}{$ENDIF}{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
// Now set the integer-size for this compiler
{$IFDEF DELPHI}
	{$IFDEF DELPHI2UP}
		{$DEFINE SIZE_32}
	{$ELSE}
		{$DEFINE SIZE_16}
	{$ENDIF}
{$ELSE}
  {$IFDEF BCB}
  	{$DEFINE SIZE_32}
  {$ELSE}
    {$IFDEF KYLIX}
      {$DEFINE SIZE_32}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
// Set language feature defines
{$IFDEF DELPHI}
	{$IFDEF DELPHI2UP}
		{$DEFINE HAS_LONGSTRINGS}
    {$DEFINE HAS_VCL}
	{$ENDIF}
	{$IFDEF DELPHI3UP}
		{$DEFINE HAS_INTERFACE}
	{$ENDIF}
	{$IFDEF DELPHI4UP}
		{$DEFINE HAS_LONGWORD}
		{$DEFINE HAS_DEFAULTPARAMS}
		{$DEFINE HAS_OVERLOAD}
		{$DEFINE HAS_INT64}
		{$DEFINE HAS_DYNAMICARRAYS}
		{$DEFINE HAS_MIDAS2}
		{$DEFINE HAS_IMPLEMENTS}
		{$DEFINE HAS_NEWVCL}
		{$DEFINE HAS_MINMAX}
	{$ENDIF}
	{$IFDEF DELPHI5UP}
		{$DEFINE HAS_MIDAS3}
		{$DEFINE HAS_THREADSAFESTRINGS}
	{$ENDIF}
  {$IFDEF DELPHI6UP}
    {$DEFINE HAS_VALUED_ENUMERATORS}
    {$DEFINE HAS_WEBSNAP}
    {$DEFINE HAS_DBEXPRESS}
    {$DEFINE HAS_CUSTOM_VARIANTS}
    {$DEFINE HAS_CLX}
    {$DEFINE HAS_VARIANTS_UNIT}
  {$ENDIF}
  {$IFDEF DELPHI7UP}
    {$DEFINE ALLOWS_DOT_IN_UNIT_NAME}
  {$ENDIF}
{$ELSE}
  {$IFDEF BCB}
    {$IFDEF BCB3UP}
      {$DEFINE HAS_VCL}
      {$DEFINE HAS_INTERFACE}
      {$DEFINE HAS_LONGSTRINGS}
    {$ENDIF}
    {$IFDEF BCB4UP}
      {$DEFINE HAS_LONGWORD}
      {$DEFINE HAS_DEFAULTPARAMS}
      {$DEFINE HAS_OVERLOAD}
      {$DEFINE HAS_INT64}
      {$DEFINE HAS_DYNAMICARRAYS}
      {$DEFINE HAS_MIDAS2}
      {$DEFINE HAS_IMPLEMENTS}
      {$DEFINE HAS_NEWVCL}
      {$DEFINE HAS_MINMAX}
      {$DEFINE HAS_CLX}
    {$ENDIF}
  {$ELSE}
    {$IFDEF KYLIX}
      {$IFDEF KYLIX1UP}
        {$DEFINE HAS_LONGSTRINGS}
        {$DEFINE HAS_INTERFACE}
        {$DEFINE HAS_LONGWORD}
        {$DEFINE HAS_DEFAULTPARAMS}
        {$DEFINE HAS_OVERLOAD}
        {$DEFINE HAS_INT64}
        {$DEFINE HAS_DYNAMICARRAYS}
        {$DEFINE HAS_IMPLEMENTS}
        {$DEFINE HAS_MINMAX}
        {$DEFINE HAS_THREADSAFESTRINGS}
        {$DEFINE HAS_VALUED_ENUMERATORS}
        {$DEFINE HAS_DBEXPRESS}
        {$DEFINE HAS_CUSTOM_VARIANTS}
        {$DEFINE HAS_CLX}
        {$DEFINE HAS_VARIANTS_UNIT}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

