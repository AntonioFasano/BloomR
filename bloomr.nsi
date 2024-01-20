
;The BloomR "green" Installer
;----------------------------

;; Run with:
;; makensis /V2 /dSRCDIR=brCore bloomr.nsi
;; Replace 'brCore' as needed

;; Features
;; --------
;; Warn and halt on existing install dir.
;; Warn and halt on insufficient space 
;; Nested dirs are accepted and parents are created.
;; Illegal chars (e.g. ":") are simply removed, so:
;; c:\mypath:subidr is valid as c:\mypathsubidr, but X:\mypath is invalid without the X drive



;Include Modern UI
!include "MUI2.nsh"


;General
;--------------------------------

  ;Product name
  Name "BloomR"

  Caption "BloomR File Extraction"
  OutFile "bloomr-setup.exe"

  ;Default installation folder
  InstallDir $DESKTOP\BloomR

  ; Icon path
  !define MUI_ICON "ahk\bloomr.ico"

  ;Set application privileges
  RequestExecutionLevel user

  ; Concatenate long message strings (Currently only globals supported)
  Var /GLOBAL UMSG

;Interface Settings
;--------------------------------

  
  !define MUI_ABORTWARNING
  
  ; Licence page text
  !define lic  "I am green to your system. No system files will be touched!"

  ; Folder selection text
  !define dirh1  "Choose Destination Folder"
  !define dirh2  "Choose the folder in which to extract BloomR"
  !define dir1  "By default a $\"BloomR$\" directory will be created on your Desktop "
  !define dir2  "to extract BloomR's files. $\r$\n$\r$\nYou might want to choose "
  !define dir3  "a different folder on your USB drive. Click 'Browse...' to select it."
  InstallButtonText  "Extract files"

  ; File extraction text
  !define extr1  "Extracting BloomR files to:"
  !define extr2  "$INSTDIR"

  ; Extraction end text
  !define finish1  "Extraction is completed in BloomR folder:"
  !define finish2  "$INSTDIR"
  CompletedText "Completed. To run BloomR click on the green icon in BloomR folder" 

 
;Pages
;--------------------------------

  ; Licence page 
  !define MUI_PAGE_HEADER_TEXT "${lic}"
  !insertmacro MUI_PAGE_LICENSE "${SRCDIR}\License.txt"


  ; Folder selection page
  !define MUI_PAGE_HEADER_TEXT    "${dirh1}"
  !define MUI_PAGE_HEADER_SUBTEXT "${dirh2}"
  !define MUI_DIRECTORYPAGE_TEXT_TOP  "${dir1}${dir2}${dir3}"
  !define MUI_DIRECTORYPAGE_TEXT_DESTINATION "Folder to extract BloomR files."
  
  ; The two lines below call cause calling DirectoryLeave to validate user $INSTDIR
  ; DirectoryLeave (see defintion code below) aborts on existing dir and missing space
  !define MUI_PAGE_CUSTOMFUNCTION_LEAVE DirectoryLeave
  !define MUI_DIRECTORYPAGE_VERIFYONLEAVE

  !insertmacro MUI_PAGE_DIRECTORY

  ; INSTFILES page: text before 
  !define MUI_PAGE_HEADER_TEXT    "${extr1}"
  !define MUI_PAGE_HEADER_SUBTEXT "${extr2}"

  ; INSTFILES page: text after, set dynamically in section 
  var finishhead
  var finishsub
  !define MUI_INSTFILESPAGE_FINISHHEADER_TEXT "$finishhead"
  !define MUI_INSTFILESPAGE_FINISHHEADER_SUBTEXT "$finishsub"
  !insertmacro MUI_PAGE_INSTFILES


;Languages
;--------------------------------
 
  !insertmacro MUI_LANGUAGE "English"


;Validate user install folder
;-----------------------------
Function DirectoryLeave
   GetInstDirError $0
   ${Switch} $0
      ${Case} 0 ; Abort if $INSTDIR exists 
	 IfFileExists "$INSTDIR\*.*" 0 PathOK   ; this includes empty existing dir
 	 StrCpy $UMSG "The directory $INSTDIR already exists."
 	 StrCpy $UMSG "$UMSG $\r$\nDelete it, rename it, or use another path."
	 StrCpy $UMSG "$UMSG $\r$\n$\r$\nWARNING:"
 	 StrCpy $UMSG "$UMSG $\r$\nBefore deleting the existing BloomR, $\r$\nbackup your PRECIOUS R SCRIPTS in mybloomer folder"
         MessageBox MB_OK $UMSG
         Abort
         PathOK:
         ${Break}
      ${Case} 1 ; Invalid Path
         MessageBox MB_OK "Invalid path selected."
         Abort
         ${Break}
      ${Case} 2 ; Out Of Free Space
         MessageBox MB_OK "Not enough free space on the drive selected."
         Abort
         ${Break}
   ${EndSwitch}
FunctionEnd


;Installer Sections
;--------------------------------
Section "Folder selection" SecMain

  ; User inst dir
  SetOutPath "$INSTDIR" 

  ; Extract files
  File /r "${SRCDIR}\*.*"
    
  ; Set completion text 
  StrCpy $finishhead  "${finish1}"
  StrCpy $finishsub   "${finish2}"



SectionEnd
