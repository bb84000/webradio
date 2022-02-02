{
  BASSenc_OGG 2.4 Delphi unit
  Copyright (c) 2016-2020 Un4seen Developments Ltd.

  See the BASSENC_OGG.CHM file for more detailed documentation
}

Unit BASSenc_OGG;

{$mode ObjFPC}{$H+}

interface

{$IFDEF MSWINDOWS}
uses BASSenc, Windows, dynlibs;
{$ELSE}
uses BASSenc, dynlibs;
{$ENDIF}

const
  // BASS_Encode_OGG_NewStream flags
  BASS_ENCODE_OGG_RESET = $1000000;
  
{$IFDEF MSWINDOWS}
  bassencoggdll = 'bassenc_ogg.dll';
{$ENDIF}
{$IFDEF LINUX}
  bassencoggdll = 'libbassenc_ogg.so';
{$ENDIF}
{$IFDEF ANDROID}
  bassencoggdll = 'libbassenc_ogg.so';
{$ENDIF}
{$IFDEF MACOS}
  {$IFDEF IOS}
    bassencoggdll = 'libbassenc_ogg.a';
  {$ELSE}
    bassencoggdll = 'libbassenc_ogg.dylib';
  {$ENDIF}
{$ENDIF}

//function BASS_Encode_OGG_GetVersion: DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bassencoggdll;

//function BASS_Encode_OGG_Start(handle:DWORD; options:PChar; flags:DWORD; proc:ENCODEPROC; user:Pointer): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bassencoggdll;
//function BASS_Encode_OGG_StartFile(handle:DWORD; options:PChar; flags:DWORD; filename:PChar): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bassencoggdll;
//function BASS_Encode_OGG_NewStream(handle:HENCODE; options:PChar; flags:DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bassencflacdll;

var
  BASS_Encode_OGG_GetVersion:function: DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  BASS_Encode_OGG_Start:function(handle:DWORD; options:PChar; flags:DWORD; proc:ENCODEPROC; user:Pointer): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_OGG_StartFile:function(handle:DWORD; options:PChar; flags:DWORD; filename:PChar): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_OGG_NewStream:function(handle:HENCODE; options:PChar; flags:DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  BASS_Encode_OGG_Handle:Thandle=0;

Function Load_bassencoggdll (const dllfilename : String) :boolean;
Procedure Unload_bassencoggdll;

implementation

Function Load_bassencoggdll (const dllfilename : String) :boolean;
begin
 if BASS_Encode_OGG_Handle <> 0 then
  Result:= true {is it already there ?}
 else
  begin
   (*go & load the dll*)
    BASS_Encode_OGG_Handle := LoadLibrary(dllfilename);
    if BASS_Encode_OGG_Handle <> 0 then
     begin {now we tie the functions to the VARs from above}
       pointer(BASS_Encode_OGG_GetVersion):=GetProcAddress(BASS_Encode_OGG_Handle, PAnsiChar('BASS_Encode_OGG_GetVersion'));
       pointer(BASS_Encode_OGG_Start):=GetProcAddress(BASS_Encode_OGG_Handle, PAnsiChar('BASS_Encode_OGG_Start'));
       pointer(BASS_Encode_OGG_StartFile):=GetProcAddress(BASS_Encode_OGG_Handle, PAnsiChar('BASS_Encode_OGG_StartFile'));
       pointer(BASS_Encode_OGG_NewStream):=GetProcAddress(BASS_Encode_OGG_Handle, PAnsiChar('BASS_Encode_OGG_NewStream'));
    end;
  end;
end;

Procedure Unload_bassencoggdll;
begin
 if BASS_Encode_OGG_Handle <> 0 then
  begin
   //BASS_Free; // make sure we release everything
   FreeLibrary(BASS_Encode_OGG_Handle);
  end;
 BASS_Encode_OGG_Handle:=0;
end;

end.
