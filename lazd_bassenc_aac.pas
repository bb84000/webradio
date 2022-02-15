{
  lazd_bassenc_aac 2.4
  Copyright (c) 2018 Un4seen Developments Ltd.

  Options :
  --object-type <value> ... "value" can be: 2/5/23/29/39. See here for descriptions: www.wikipedia.org/wiki/MPEG-4_Part_3#MPEG-4_Audio_Object_Types
  --vbr <value> ... "value" can be 0 (CBR) or 1-5 (VBR levels).
  --bitrate <value> ... bitrate (bps) for CBR.
  The default is type 2 CBR with the bitrate based on the sample rate and channel count. No tagging or MP4 options currently,

  BASSENC_AAC.DLL Dynamic loading Lazarus unit

  bb-sdtp - january 2022
 }
unit lazd_bassenc_aac;

{$mode ObjFPC}{$H+}

interface

uses lazd_bassenc, dynlibs
{$IFDEF WINDOWS}, Windows{$ENDIF} ;

const
{$IFDEF WINDOWS}
  bassencaacdll = 'bassenc_aac.dll';
{$ENDIF}
{$IFDEF LINUX}
  bassencaacdll = 'libbassenc_aac.so';
{$ENDIF}

var
  BASS_Encode_AAC_GetVersion:function: DWORD; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  BASS_Encode_AAC_Start:function(handle:DWORD; options:PChar; flags:DWORD; proc:ENCODEPROCEX; user:Pointer): HENCODE; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_AAC_StartFile:function(handle:DWORD; options:PChar; flags:DWORD; filename:PChar): HENCODE; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  BASS_Encode_AAC_Handle:Thandle=0; // this will hold our handle for the dll; it functions nicely as a mutli-dll prevention unit as well...

Function Load_bassencaacdll (const dllfilename : String) :boolean;
Procedure Unload_bassencaacdll;

implementation

Function Load_bassencaacdll (const dllfilename : String) :boolean;
begin
  result:= false;
  if BASS_Encode_AAC_Handle <> 0 then Result:= true else
  begin
    {$IFDEF UNICODE}
      BASS_Encode_AAC_Handle:= LoadLibraryW(PWideChar(dllfilename));
    {$ELSE}
      BASS_Encode_AAC_Handle:= LoadLibrary(PChar(dllfilename));
    {$ENDIF}
    if BASS_Encode_AAC_Handle <> 0 then
    begin
      pointer(BASS_Encode_AAC_GetVersion):=GetProcAddress(BASS_Encode_AAC_Handle, PAnsiChar('BASS_Encode_AAC_GetVersion'));
      pointer(BASS_Encode_AAC_Start):=GetProcAddress(BASS_Encode_AAC_Handle, PAnsiChar('BASS_Encode_AAC_Start'));
      pointer(BASS_Encode_AAC_StartFile):=GetProcAddress(BASS_Encode_AAC_Handle, PAnsiChar('BASS_Encode_AAC_StartFile'));
    end;
    // check if all functions are OK
    if (pointer(BASS_Encode_AAC_GetVersion)=nil) or (pointer(BASS_Encode_AAC_Start)=nil) or
       (pointer(BASS_Encode_AAC_StartFile)=nil) then
    begin
      FreeLibrary(BASS_Encode_AAC_Handle);
    Result := false
    end else Result := (BASS_Encode_AAC_Handle) <> 0;
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

