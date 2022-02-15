{
  lazd_bass_aac 2.4 Lazarus unit
  Copyright (c) 2003-2020 Un4seen Developments Ltd.

  See the BASSFLAC.CHM file for more detailed documentation

  Call Load_BASSFLACDLL to load BASSFLAC before using any functions, and
  Unload_BASSFLACDLL to unload
  Important !!! Always use () after a function or procedure without parameter

  Lazarus port - bb - sdtp - february 2022
}


unit lazd_bass_flac;

interface

{$mode objfpc}{$H+}

uses lazd_bass, dynlibs
{$IFDEF WINDOWS}, Windows{$ENDIF} ;

const
  // BASS_CHANNELINFO type
  BASS_CTYPE_STREAM_FLAC        = $10900;
  BASS_CTYPE_STREAM_FLAC_OGG    = $10901;

  // Additional tag type
  BASS_TAG_FLAC_CUE             = 12; // cuesheet : TAG_FLAC_CUE structure
  BASS_TAG_FLAC_PICTURE         = $12000; // + index #, picture : TAG_FLAC_PICTURE structure
  BASS_TAG_FLAC_METADATA        = $12400; // + index #, application metadata : TAG_FLAC_METADATA structure

type
  TAG_FLAC_PICTURE = record
    apic: DWORD;		// ID3v2 "APIC" picture type
    mime: PAnsiChar;	// mime type
    desc: PAnsiChar;	// description
    width: DWORD;
    height: DWORD;
    depth: DWORD;
    colors: DWORD;
    length: DWORD;		// data length
    data: Pointer;
  end;

  TAG_FLAC_METADATA = record
    id: Array[0..3] of AnsiChar;
	length: DWORD;		// data length
    data: Pointer;
  end;

const
{$IFDEF MSWINDOWS}
  bassflacdll = 'bassflac.dll';
{$ENDIF}
{$IFDEF LINUX}
  bassflacdll = 'libbassflac.so';
{$ENDIF}
{$IFDEF MACOS}
  bassflacdll = 'libbassflac.dylib';
{$ENDIF}

var
  BASS_FLAC_Handle:Thandle = 0;

   BASS_FLAC_StreamCreateFile:function(mem:BOOL; f:Pointer; offset,length:QWORD; flags:DWORD): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
   BASS_FLAC_StreamCreateURL:function(url:PChar; offset:DWORD; flags:DWORD; proc:DOWNLOADPROC; user:Pointer): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
   BASS_FLAC_StreamCreateFileUser:function(system,flags:DWORD; var procs:BASS_FILEPROCS; user:Pointer): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

   function Load_BASSFLACDLL(const dllfilename : string) : boolean;
   procedure Unload_BASSFLACDLL;

implementation

Function Load_BASSFLACDLL (const dllfilename : String) :boolean;
begin
  result:= false;
  if BASS_FLAC_Handle <> 0 then Result:= true else
  begin
    {$IFDEF UNICODE}
      BASS_FLAC_Handle:= LoadLibraryW(PWideChar(dllfilename));
    {$ELSE}
      BASS_FLAC_Handle:= LoadLibrary(PChar(dllfilename));
    {$ENDIF}
    if BASS_FLAC_Handle <> 0 then
    begin
      pointer(BASS_FLAC_StreamCreateFile):=GetProcAddress(BASS_FLAC_Handle, PAnsiChar('BASS_FLAC_StreamCreateFile'));
      pointer(BASS_FLAC_StreamCreateURL):=GetProcAddress(BASS_FLAC_Handle, PAnsiChar('BASS_FLAC_StreamCreateURL'));
      pointer(BASS_FLAC_StreamCreateFileUser):=GetProcAddress(BASS_FLAC_Handle, PAnsiChar('BASS_FLAC_StreamCreateFileUser'));

    end;
    // check if all functions are OK
    if (pointer(BASS_FLAC_StreamCreateFile)=nil) or (pointer(BASS_FLAC_StreamCreateURL)=nil) or
       (pointer(BASS_FLAC_StreamCreateFileUser)=nil) then
    begin
      FreeLibrary(BASS_FLAC_Handle);
    Result := false
    end else Result := (BASS_FLAC_Handle) <> 0;
  end;
end;

Procedure Unload_BASSFLACDLL;
begin
 if BASS_FLAC_Handle <> 0 then
  begin
   FreeLibrary(BASS_FLAC_Handle);
  end;
 BASS_FLAC_Handle:=0;
end;

end.
