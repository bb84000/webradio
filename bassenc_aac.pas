{
  BASSenc_AAC 2.4 Lazarus unit
  Copyright (c) 2018 Un4seen Developments Ltd.

  Options :
  --object-type <value> ... "value" can be: 2/5/23/29/39. See here for descriptions: www.wikipedia.org/wiki/MPEG-4_Part_3#MPEG-4_Audio_Object_Types
  --vbr <value> ... "value" can be 0 (CBR) or 1-5 (VBR levels).
  --bitrate <value> ... bitrate (bps) for CBR.
  The default is type 2 CBR with the bitrate based on the sample rate and channel count. No tagging or MP4 options currently,

  bb-sdtp - january 2022 : Changed to dynamic loading to allow it to be placed in plugins folder
}
unit BASSenc_AAC;

{$mode ObjFPC}{$H+}

interface

{$IFDEF MSWINDOWS}
uses BASSenc, Windows, dynlibs;
{$ELSE}
uses BASSenc, dynlibs;
{$ENDIF}

const
{$IFDEF MSWINDOWS}
  bassencaacdll = 'bassenc_aac.dll';
{$ENDIF}
{$IFDEF LINUX}
  bassencaacdll = 'libbassenc_aac.so';
{$ENDIF}

//function BASS_Encode_AAC_GetVersion: DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bassencaacdll;

//function BASS_Encode_AAC_Start(handle:DWORD; options:PChar; flags:DWORD; proc:ENCODEPROCEX; user:Pointer): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bassencaacdll;
//function BASS_Encode_AAC_StartFile(handle:DWORD; options:PChar; flags:DWORD; filename:PChar): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bassencaacdll;

var
  BASS_Encode_AAC_GetVersion:function: DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  BASS_Encode_AAC_Start:function(handle:DWORD; options:PChar; flags:DWORD; proc:ENCODEPROCEX; user:Pointer): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_AAC_StartFile:function(handle:DWORD; options:PChar; flags:DWORD; filename:PChar): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  BASS_Encode_AAC_Handle:Thandle=0; // this will hold our handle for the dll; it functions nicely as a mutli-dll prevention unit as well...

Function Load_bassencaacdll (const dllfilename : String) :boolean;
Procedure Unload_bassencaacdll;

implementation

Function Load_bassencaacdll (const dllfilename : String) :boolean;
begin
 if BASS_Encode_AAC_Handle <> 0 then
  Result:= true {is it already there ?}
 else
  begin
   (*go & load the dll*)
    BASS_Encode_AAC_Handle := LoadLibrary(dllfilename);
    if BASS_Encode_AAC_Handle <> 0 then
     begin {now we tie the functions to the VARs from above}
       pointer(BASS_Encode_AAC_GetVersion):=GetProcAddress(BASS_Encode_AAC_Handle, PAnsiChar('BASS_Encode_AAC_GetVersion'));
       pointer(BASS_Encode_AAC_Start):=GetProcAddress(BASS_Encode_AAC_Handle, PAnsiChar('BASS_Encode_AAC_Start'));
       pointer(BASS_Encode_AAC_StartFile):=GetProcAddress(BASS_Encode_AAC_Handle, PAnsiChar('BASS_Encode_AAC_StartFile'));
    end;
  end;
end;

Procedure Unload_bassencaacdll;
begin
 if BASS_Encode_AAC_Handle <> 0 then
  begin
   //BASS_Free; // make sure we release everything
   FreeLibrary(BASS_Encode_AAC_Handle);
  end;
 BASS_Encode_AAC_Handle:=0;
end;

end.

