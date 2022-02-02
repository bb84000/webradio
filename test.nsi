;Installation script for WebRadio
; bb - sdtp - February 2022
;--------------------------------
  Unicode true

  !include "MUI2.nsh"
  !include x64.nsh
  !include FileFunc.nsh
!include WinMessages.nsh
!include FontReg.nsh

;--------------------------------
;Configuration

  ;General
  Name "Web radio tuner"
  OutFile "InstallWebradio.exe"
  !define lazarus_dir "C:\Users\Bernard\Documents\Lazarus"
  !define source_dir "${lazarus_dir}\webradio"
  
  RequestExecutionLevel admin
  
  ;Windows vista.. 10 manifest
  ManifestSupportedOS all

  ;!define MUI_LANGDLL_ALWAYSSHOW                     ; To display language selection dialog
  !define MUI_ICON "${source_dir}\webradio.ico"
  !define MUI_UNICON "${source_dir}\webradio.ico"

  ; The default installation directory
  InstallDir "$PROGRAMFILES\NewWebRadio"

;--------------------------------
; Interface Settings

!define MUI_ABORTWARNING

;--------------------------------
;Language Selection Dialog Settings

  ;Remember the installer language
  !define MUI_LANGDLL_REGISTRY_ROOT "HKCU"
  !define MUI_LANGDLL_REGISTRY_KEY "Software\SDTP\NewWebRadio"
  !define MUI_LANGDLL_REGISTRY_VALUENAME "Installer Language"
  !define MUI_FINISHPAGE_SHOWREADME
  !define MUI_FINISHPAGE_SHOWREADME_TEXT "$(Check_box)"
  !define MUI_FINISHPAGE_SHOWREADME_FUNCTION inst_shortcut
; Pages

  !insertmacro MUI_PAGE_WELCOME
  !insertmacro MUI_PAGE_LICENSE $(licence)
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_PAGE_FINISH

  !insertmacro MUI_UNPAGE_WELCOME
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  !insertmacro MUI_UNPAGE_FINISH

;Languages
  !insertmacro MUI_LANGUAGE "English"
  !insertmacro MUI_LANGUAGE "French"
  !insertmacro MUI_RESERVEFILE_LANGDLL

  ;Licence langage file
  LicenseLangString Licence ${LANG_ENGLISH} "${source_dir}\license.txt"
  LicenseLangString Licence ${LANG_FRENCH}  "${source_dir}\licensf.txt"

  ;Language strings for uninstall string
  LangString RemoveStr ${LANG_ENGLISH}  "Web radio tuner"
  LangString RemoveStr ${LANG_FRENCH} "Tuner Web radio"

  ;Language string for links
  LangString ProgramLnkStr ${LANG_ENGLISH} "Web radio tuner.lnk"
  LangString ProgramLnkStr ${LANG_FRENCH} "Tuner Web radio.lnk"
  LangString UninstLnkStr ${LANG_ENGLISH} "Web radio tuner uninstall.lnk"
  LangString UninstLnkStr ${LANG_FRENCH} "Désinstallation de Tuner Web radio.lnk"

  LangString ProgramDescStr ${LANG_ENGLISH} "Web radio tuner"
  LangString ProgramDescStr ${LANG_FRENCH} "Tuner Web radio"

  ;Language strings for language selection dialog
  LangString LangDialog_Title ${LANG_ENGLISH} "Installer Language|$(^CancelBtn)"
  LangString LangDialog_Title ${LANG_FRENCH} "Langue d'installation|$(^CancelBtn)"

  LangString LangDialog_Text ${LANG_ENGLISH} "Please select the installer language."
  LangString LangDialog_Text ${LANG_FRENCH} "Choisissez la langue du programme d'installation."

  ;language strings for checkbox
  LangString Check_box ${LANG_ENGLISH} "Install a shortcut on the desktop"
  LangString Check_box ${LANG_FRENCH} "Installer un raccourci sur le bureau"

  ;Cannot install
  LangString No_Install ${LANG_ENGLISH} "The application cannot be installed on a 32bit system"
  LangString No_Install ${LANG_FRENCH} "Cette application ne peut pas être installée sur un système 32bits"
  
  ; Language string for remove old install
  LangString Remove_Old ${LANG_ENGLISH} "Install will remove a previous installation."
  LangString Remove_Old ${LANG_FRENCH} "Install va supprimer une ancienne installation."

  !define MUI_LANGDLL_WINDOWTITLE "$(LangDialog_Title)"
  !define MUI_LANGDLL_INFO "$(LangDialog_Text)"
;--------------------------------

  !getdllversion  "${source_dir}\webradiowin64.exe" expv_
  !define FileVersion "${expv_1}.${expv_2}.${expv_3}.${expv_4}"

  VIProductVersion "${FileVersion}"
  VIAddVersionKey "FileVersion" "${FileVersion}"
  VIAddVersionKey "ProductName" "InstallWebRadio.exe"
  VIAddVersionKey "FileDescription" "WebRadio Installer"
  VIAddVersionKey "LegalCopyright" "sdtp - bb"
  VIAddVersionKey "ProductVersion" "${FileVersion}"
   
  ; Change nsis brand line
  BrandingText "$(ProgramDescStr) version ${FileVersion} - bb - sdtp"
  

; The stuff to install
Section "" ;No components page, name is not important
  SetShellVarContext all
  SetOutPath "$INSTDIR"

 ${If} ${RunningX64}
    SetRegView 64    ; change registry entries and install dir for 64 bit
  ${EndIf}
  Var /GLOBAL prg_to_inst
  Var /GLOBAL prg_to_del

  ${If} ${RunningX64}  ; change registry entries and install dir for 64 bit
  ; install font
     IfFileExists "$FONTS\DotMatrix.ttf" FontOk
     SetOutPath "$FONTS"
     File "${source_dir}\DotMatrix.ttf"
     WriteRegStr HKEY_LOCAL_MACHINE "SOFTWARE\Microsoft\Windows NT\CurrentVersion\Fonts" "DotMatrix Regular" "DotMatrix.ttf"
    ; fonts::registerFont "DotMatrix.ttf"
     
     FontOK:
     



 ${Else}
     ;!getdllversion  "${source_dir}\mailsinboxwin32.exe" expv_
     ;StrCpy "$prg_to_inst" "$INSTDIR\mailsinboxwin32.exe"
     ;StrCpy "$prg_to_del" "$INSTDIR\webradiowin64.exe"
     ;IfFileExists "$WINDIR\system32\libeay32.dll" ssl_lib32_found ssl_lib32_not_found
     ;ssl_lib32_not_found:
     ;  File "${lazarus_dir}\openssl\win32\libeay32.dll"
     ;  File "${lazarus_dir}\openssl\win32\ssleay32.dll"
     ;  File "${lazarus_dir}\openssl\OpenSSL License.txt"
     ;ssl_lib32_found:
   ${EndIf}

SectionEnd ; end the section

; Install shortcuts, language dependant

Section "Start Menu Shortcuts"

SectionEnd

;Uninstaller Section

Section Uninstall
SectionEnd ; end of uninstall section

Function inst_shortcut
FunctionEnd

Function .onInit
FunctionEnd