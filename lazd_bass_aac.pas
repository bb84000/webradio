{
  lazd_bass_aac 2.4 Lazarus unit
  Copyright (c) 2003-2020 Un4seen Developments Ltd.

  See the BASSWMA.CHM file for more detailed documentation
  
  Call Load_BASSAACDLL to load BASSAAC before using any functions, and
  Unload_BASSAACDLL to unload 
  Important !!! Always use () after a function or procedure without parameter
  
  Lazarus port - bb - sdtp - february 2022
}

Unit lazd_bass_aac;

interface

{$mode objfpc}{$H+}

uses lazd_bass, dynlibs
{$IFDEF WINDOWS}, Windows{$ENDIF} ;

const
  // additional error codes returned by BASS_ErrorGetCode
  BASS_ERROR_MP4_NOSTREAM = 6000; // non-streamable due to MP4 atom order ("mdat" before "moov")

  // additional BASS_SetConfig options
  BASS_CONFIG_MP4_VIDEO = $10700; // play the audio from MP4 videos
  BASS_CONFIG_AAC_MP4 = $10701; // support MP4 in BASS_AAC_StreamCreateXXX functions (no need for BASS_MP4_StreamCreateXXX)
  BASS_CONFIG_AAC_PRESCAN = $10702; // pre-scan ADTS AAC files for seek points and accurate length

  // additional BASS_AAC_StreamCreateFile/etc flags
  BASS_AAC_FRAME960 = $1000; // 960 samples per frame
  BASS_AAC_STEREO = $400000; // downmatrix to stereo

  // BASS_CHANNELINFO type
  BASS_CTYPE_STREAM_AAC = $10b00; // AAC
  BASS_CTYPE_STREAM_MP4 = $10b01; // MP4

const
{$IFDEF MSWINDOWS}
  bassaacdll = 'bass_aac.dll';
{$ENDIF}
{$IFDEF LINUX}
  bassaacdll = 'libbass_aac.so';
{$ENDIF}
{$IFDEF MACOS}
  bassaacdll = 'libbass_aac.dylib';
{$ENDIF}

var
  BASS_AAC_Handle:Thandle = 0;
   
  BASS_AAC_StreamCreateFile:function(mem:BOOL; f:Pointer; offset,length:QWORD; flags:DWORD): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
  BASS_AAC_StreamCreateURL:function(URL:PChar; offset:DWORD; flags:DWORD; proc:DOWNLOADPROC; user:Pointer): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
  BASS_AAC_StreamCreateFileUser:function(system,flags:DWORD; var procs:BASS_FILEPROCS; user:Pointer): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};  
  BASS_MP4_StreamCreateFile:function(mem:BOOL; f:Pointer; offset,length:QWORD; flags:DWORD): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};  
  BASS_MP4_StreamCreateFileUser:function(system,flags:DWORD; var procs:BASS_FILEPROCS; user:Pointer): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};  
  
  function Load_BASSAACDLL(const dllfilename : string) : boolean;
  procedure Unload_BASSAACDLL;

implementation

Function Load_BASSAACDLL (const dllfilename : String) :boolean;
begin
  result:= false;
  if BASS_AAC_Handle <> 0 then Result:= true else
  begin
    {$IFDEF UNICODE}
      BASS_AAC_Handle:= LoadLibraryW(PWideChar(dllfilename));
    {$ELSE}
      BASS_AAC_Handle:= LoadLibrary(PChar(dllfilename));
    {$ENDIF}
    if BASS_AAC_Handle <> 0 then
    begin
      pointer(BASS_AAC_StreamCreateFile):=GetProcAddress(BASS_AAC_Handle, PAnsiChar('BASS_AAC_StreamCreateFile'));
      pointer(BASS_AAC_StreamCreateURL):=GetProcAddress(BASS_AAC_Handle, PAnsiChar('BASS_AAC_StreamCreateURL'));
      pointer(BASS_AAC_StreamCreateFileUser):=GetProcAddress(BASS_AAC_Handle, PAnsiChar('BASS_AAC_StreamCreateFileUser'));
      pointer(BASS_MP4_StreamCreateFile):=GetProcAddress(BASS_AAC_Handle, PAnsiChar('BASS_MP4_StreamCreateFile'));
      pointer(BASS_MP4_StreamCreateFileUser):=GetProcAddress(BASS_AAC_Handle, PAnsiChar('BASS_MP4_StreamCreateFileUser'));
    end;
    // check if all functions are OK
    if (pointer(BASS_AAC_StreamCreateFile)=nil) or (pointer(BASS_AAC_StreamCreateURL)=nil) or
       (pointer(BASS_AAC_StreamCreateFileUser)=nil) or (pointer(BASS_MP4_StreamCreateFile)=nil) or
       (pointer(BASS_MP4_StreamCreateFileUser)= nil) then
    begin
      FreeLibrary(BASS_AAC_Handle);
    Result := false
    end else Result := (BASS_AAC_Handle) <> 0;
  end;

end;

Procedure Unload_BASSAACDLL;
begin
 if BASS_AAC_Handle <> 0 then
  begin
   //BASS_Free; // make sure we release everything
   FreeLibrary(BASS_AAC_Handle);
  end;
 BASS_AAC_Handle:=0;
end;
end.
