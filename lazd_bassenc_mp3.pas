{
  lazd_bassenc_mp3.4
  Copyright (c) 2018 Un4seen Developments Ltd.
  See the BASSENC_MP3.CHM file for more detailed documentation

  BASSENC_MP3.DLL Dynamic loading Lazarus unit

  bb-sdtp - january 2022
}

Unit lazd_bassenc_mp3;

{$mode ObjFPC}{$H+}

interface

uses lazd_bassenc, dynlibs
{$IFDEF WINDOWS}, Windows{$ENDIF} ;

const
{$IFDEF MSWINDOWS}
  bassencmp3dll = 'bassenc_mp3.dll';
{$ENDIF}
{$IFDEF LINUX}
  bassencmp3dll = 'libbassenc_mp3.so';
{$ENDIF}
{$IFDEF MACOS}
  bassencmp3dll = 'libbassenc_mp3.dylib';
{$ENDIF}

var
  BASS_Encode_MP3_GetVersion:function: DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  BASS_Encode_MP3_Start:function(handle:DWORD; options:PChar; flags:DWORD; proc:ENCODEPROCEX; user:Pointer): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_MP3_StartFile:function(handle:DWORD; options:PChar; flags:DWORD; filename:PChar): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  BASS_Encode_MP3_Handle:Thandle=0; // this will hold our handle for the dll; it functions nicely as a mutli-dll prevention unit as well...

Function Load_bassencmp3dll (const dllfilename : String) :boolean;
Procedure Unload_bassencmp3dll;


implementation

Function Load_bassencmp3dll (const dllfilename : String) :boolean;
begin
 if BASS_Encode_MP3_Handle <> 0 then Result:= true else
  begin
    {$IFDEF UNICODE}
      BASS_Encode_MP3_Handle:= LoadLibraryW(PWideChar(dllfilename));
    {$ELSE}
      BASS_Encode_MP3_Handle:= LoadLibrary(PChar(dllfilename));
    {$ENDIF}
    if BASS_Encode_MP3_Handle <> 0 then
     begin
       pointer(BASS_Encode_MP3_GetVersion):=GetProcAddress(BASS_Encode_MP3_Handle, PAnsiChar('BASS_Encode_MP3_GetVersion'));
       pointer(BASS_Encode_MP3_Start):=GetProcAddress(BASS_Encode_MP3_Handle, PAnsiChar('BASS_Encode_MP3_Start'));
       pointer(BASS_Encode_MP3_StartFile):=GetProcAddress(BASS_Encode_MP3_Handle, PAnsiChar('BASS_Encode_MP3_StartFile'));
    end;
    if (pointer(BASS_Encode_MP3_GetVersion)=nil) or (pointer(BASS_Encode_MP3_Start)=nil) or
       (pointer(BASS_Encode_MP3_StartFile)=nil) then
    begin
      FreeLibrary(BASS_Encode_MP3_Handle);
      Result := false
    end else Result := (BASS_Encode_MP3_Handle) <> 0;
  end;
end;

Procedure Unload_bassencmp3dll;
begin
 if BASS_Encode_MP3_Handle <> 0 then FreeLibrary(BASS_Encode_MP3_Handle);
 BASS_Encode_MP3_Handle:=0;
end;

end.
