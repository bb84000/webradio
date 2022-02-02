{
  BASSenc_MP3 2.4 Lazarus unit
  Copyright (c) 2018 Un4seen Developments Ltd.

  See the BASSENC_MP3.CHM file for more detailed documentation
  bb-sdtp - january 2022 : Changed to dynamic loading to allow it to be placed in plugins folder
}

Unit BASSenc_MP3;

{$mode ObjFPC}{$H+}

interface

{$IFDEF MSWINDOWS}
uses BASSenc, Windows, dynlibs;
{$ELSE}
uses BASSenc, dynlibs;
{$ENDIF}

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

//function BASS_Encode_MP3_GetVersion: DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bassencmp3dll;
//function BASS_Encode_MP3_Start(handle:DWORD; options:PChar; flags:DWORD; proc:ENCODEPROCEX; user:Pointer): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bassencmp3dll;
//function BASS_Encode_MP3_StartFileBASS_Encode_MP3_StartFile(handle:DWORD; options:PChar; flags:DWORD; filename:PChar): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bassencmp3dll;

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
 if BASS_Encode_MP3_Handle <> 0 then
  Result:= true {is it already there ?}
 else
  begin
   (*go & load the dll*)
    BASS_Encode_MP3_Handle := LoadLibrary(dllfilename);
    if BASS_Encode_MP3_Handle <> 0 then
     begin {now we tie the functions to the VARs from above}
       pointer(BASS_Encode_MP3_GetVersion):=GetProcAddress(BASS_Encode_MP3_Handle, PAnsiChar('BASS_Encode_MP3_GetVersion'));
       pointer(BASS_Encode_MP3_Start):=GetProcAddress(BASS_Encode_MP3_Handle, PAnsiChar('BASS_Encode_MP3_Start'));
       pointer(BASS_Encode_MP3_StartFile):=GetProcAddress(BASS_Encode_MP3_Handle, PAnsiChar('BASS_Encode_MP3_StartFile'));
    end;
  end;
end;

Procedure Unload_bassencmp3dll;
begin
 if BASS_Encode_MP3_Handle <> 0 then
  begin
   //BASS_Free; // make sure we release everything
   FreeLibrary(BASS_Encode_MP3_Handle);
  end;
 BASS_Encode_MP3_Handle:=0;
end;

end.
