
;The BloomR "green" Installer

;Include Modern UI
!include "MUI2.nsh"
 


;--------------------------------
;General

  ;Product name
  Name "BloomR"

  Caption "BloomR File Extraction"
  OutFile "Extract_BloomR.exe"

  ;Default installation folder
  InstallDir $DESKTOP\BloomR
  
  ;Request application privileges for Windows Vista
  RequestExecutionLevel user

;--------------------------------
;Interface Settings

  !define MUI_ICON "ahk.d\bloomr.ico"
  !define MUI_ABORTWARNING

; Custom Text 
  !define MUI_PAGE_HEADER_TEXT "I am green to your system. No system files will be touched!"
  !define dir1  "By default a $\"BloomR$\" directory will be created on your Desktop "
  !define dir2  "to extract BloomR's files. $\r$\n$\r$\nYou might want to choose "
  !define dir3  "a different folder on your USB drive. Click 'Browse...' to select it."
  InstallButtonText  "Extract files"


;--------------------------------
;Pages


  !insertmacro MUI_PAGE_LICENSE "BloomR\License.txt"
  
  !define MUI_DIRECTORYPAGE_TEXT_TOP  "${dir1}${dir2}${dir3}"
  !define MUI_DIRECTORYPAGE_TEXT_DESTINATION "Folder to extract BloomR files."
  !insertmacro MUI_PAGE_DIRECTORY

  !insertmacro MUI_PAGE_INSTFILES
    
;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Installer Sections

Section "Folder selection" SecMain

  SetOutPath "$INSTDIR"
  
  ;ADD YOUR OWN FILES HERE...
  File /r "BloomR\*.*"
    
  
SectionEnd


