{
  lazd_bassenc_flac 2.4
  Copyright (c) 2018 Un4seen Developments Ltd.

  BASSENC_FLAC Dynamic loading Lazarus unit

  bb-sdtp - january 2022
}

Unit lazd_bassenc_flac;

interface

{$mode ObjFPC}{$H+}

uses lazd_bassenc, dynlibs
{$IFDEF WINDOWS}, Windows{$ENDIF} ;

const
  // BASS_Encode_FLAC_NewStreams flags
  BASS_ENCODE_FLAC_RESET = $1000000;

{$IFDEF MSWINDOWS}
  bassencflacdll = 'bassenc_flac.dll';
{$ENDIF}
{$IFDEF LINUX}
  bassencflacdll = 'libbassenc_flac.so';
{$ENDIF}
{$IFDEF ANDROID}
  bassencflacdll = 'libbassenc_flac.so';
{$ENDIF}
{$IFDEF MACOS}
  {$IFDEF IOS}
    bassencflacdll = 'libbassenc_flac.a';
  {$ELSE}
    bassencflacdll = 'libbassenc_flac.dylib';
  {$ENDIF}
{$ENDIF}

var
  BASS_Encode_FLAC_Handle:Thandle=0;

  BASS_Encode_FLAC_GetVersion:function: DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  BASS_Encode_FLAC_Start:function(handle:DWORD; options:PChar; flags:DWORD; proc:ENCODEPROCEX; user:Pointer): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_FLAC_StartFile:function(handle:DWORD; options:PChar; flags:DWORD; filename:PChar): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_FLAC_NewStream:function(handle:HENCODE; options:PChar; flags:DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  Function Load_bassencflacdll (const dllfilename : String) :boolean;
  Procedure Unload_bassencflacdll;

implementation

Function Load_bassencflacdll (const dllfilename : String) :boolean;
begin
 if BASS_Encode_FLAC_Handle <> 0 then Result:= true else
  begin
    {$IFDEF UNICODE}
      BASS_Encode_FLAC_Handle:= LoadLibraryW(PWideChar(dllfilename));
    {$ELSE}
      BASS_Encode_FLAC_Handle:= LoadLibrary(PChar(dllfilename));
    {$ENDIF}
    if BASS_Encode_FLAC_Handle <> 0 then
     begin
       pointer(BASS_Encode_FLAC_GetVersion):=GetProcAddress(BASS_Encode_FLAC_Handle, PAnsiChar('BASS_Encode_FLAC_GetVersion'));
       pointer(BASS_Encode_FLAC_Start):=GetProcAddress(BASS_Encode_FLAC_Handle, PAnsiChar('BASS_Encode_FLAC_Start'));
       pointer(BASS_Encode_FLAC_StartFile):=GetProcAddress(BASS_Encode_FLAC_Handle, PAnsiChar('BASS_Encode_FLAC_StartFile'));
       pointer(BASS_Encode_FLAC_NewStream):= GetProcAddress(BASS_Encode_FLAC_Handle, PAnsiChar('BASS_Encode_FLAC_NewStream'));
    end;
    if (pointer(BASS_Encode_FLAC_GetVersion)=nil) or (pointer(BASS_Encode_FLAC_Start)=nil) or
       (pointer(BASS_Encode_FLAC_StartFile)=nil) or  (pointer(BASS_Encode_FLAC_NewStream)= nil then
    begin
      FreeLibrary(BASS_Encode_FLAC_Handle);
      Result := false
    end else Result := (BASS_Encode_FLAC_Handle) <> 0;
  end;
end;

Procedure Unload_bassencflacdll;
begin
 if BASS_Encode_FLAC_Handle <> 0 then FreeLibrary(BASS_Encode_FLAC_Handle);
 BASS_Encode_FLAC_Handle:=0;
end;

end.
