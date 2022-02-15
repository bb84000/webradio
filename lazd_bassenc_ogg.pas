{
  lazd_bassenc_ogg 2.4
  Copyright (c) 2016-2020 Un4seen Developments Ltd.

  See the BASSENC_OGG.CHM file for more detailed documentation

    BASSENC_OGG.DLL Dynamic loading Lazarus unit

  bb-sdtp - january 2022
}

Unit lazd_bassenc_ogg;

{$mode ObjFPC}{$H+}

interface

uses lazd_bassenc, dynlibs
{$IFDEF WINDOWS}, Windows{$ENDIF} ;


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
 if BASS_Encode_OGG_Handle <> 0 then Result:= true
 else
  begin
   {$IFDEF UNICODE}
     BASS_Encode_OGG_Handle:= LoadLibraryW(PWideChar(dllfilename));
   {$ELSE}
     BASS_Encode_OGG_Handle:= LoadLibrary(PChar(dllfilename));
   {$ENDIF}
    if BASS_Encode_OGG_Handle <> 0 then
     begin {now we tie the functions to the VARs from above}
       pointer(BASS_Encode_OGG_GetVersion):=GetProcAddress(BASS_Encode_OGG_Handle, PAnsiChar('BASS_Encode_OGG_GetVersion'));
       pointer(BASS_Encode_OGG_Start):=GetProcAddress(BASS_Encode_OGG_Handle, PAnsiChar('BASS_Encode_OGG_Start'));
       pointer(BASS_Encode_OGG_StartFile):=GetProcAddress(BASS_Encode_OGG_Handle, PAnsiChar('BASS_Encode_OGG_StartFile'));
       pointer(BASS_Encode_OGG_NewStream):=GetProcAddress(BASS_Encode_OGG_Handle, PAnsiChar('BASS_Encode_OGG_NewStream'));
    end;
    // check if all functions are OK
    if (pointer(BASS_Encode_OGG_GetVersion)=nil) or (pointer(BASS_Encode_OGG_Start)=nil) or
       (pointer(BASS_Encode_OGG_StartFile)=nil) or (pointer(BASS_Encode_OGG_NewStream)= nil) then
    begin
      FreeLibrary(BASS_Encode_OGG_Handle);
    Result := false
    end else Result := (BASS_Encode_OGG_Handle) <> 0;

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
