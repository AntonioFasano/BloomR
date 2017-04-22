
// Converts (Bloomberg) xlx (2003) files, in argument DIRPATH and its subfolders, to xlsx files 
// Written by Antonio FASANO
// University of Rome LUISS and Salerno
// Last revision Oct 2015

// Usage:
// cscript bbg2xlsx.js dirpath


//Get Args
var argsUnnamed = WScript.Arguments.Unnamed, 
    nArgs=WScript.Arguments.length,
    fso=fso = new ActiveXObject("Scripting.FileSystemObject");
   
//No arg at all
if (nArgs==0) ShowUsage ();

//Get and check rootDir
var rootDir=argsUnnamed(0);
if (!IsFolder(rootDir)) WScript.Quit();			
rootDir= fso.GetFolder(rootDir)

//User warning 
WScript.Echo("I might overwrite xlsx in the tree: " + rootDir.Path + '\nProceed (y/N)');
var ans = WScript.StdIn.ReadLine();
if(ans.toUpperCase() != 'Y') WScript.Quit(); 


//Get xls recursively 
var  files=listFiles(rootDir.Path, '\\.xls$');

// Loop and convert them
for (k in files) {

  //Get file parts
  var xls = fso.GetFile(files[k]),
      xlsPath=fso.GetFile(xls),
      xlsName= fso.GetFileName(xls),
      xlsBase= fso.GetBaseName(xls),
      xlsFolder=fso.GetParentFolderName(xls),
      xlsxPath=xlsFolder + '\\' + xlsBase + '.xlsx';

  //Delete conflicting xlsx
  if(fso.FileExists(xlsxPath)) fso.DeleteFile(xlsxPath);

  //Open xls file
  ExcelApp = new ActiveXObject("Excel.Application");
  ExcelBook = ExcelApp.Workbooks.Open (xlsPath);

  //Save as xlsx
  WScript.Echo('Converting ' + xlsPath); 
  var xlWorkbookDefault=51
  ExcelBook.SaveAs(Filename=xlsxPath, FileFormat=xlWorkbookDefault);

  ExcelBook.Close(SaveChanges = false);
  ExcelApp.Quit();

}


 


//==========================================================
//Service Functions

function GetFullPath(file){
	
	var WshShell = WScript.CreateObject ("WScript.Shell");

	//No ':' no full path. Then attach File name to curr dir
	if (file.match(/:/)==null)
          file=WshShell.CurrentDirectory + "\\" + file;		
	return file;	
}

function IsFile (file){
	var fso = new ActiveXObject("Scripting.FileSystemObject");
	
   	if (fso.FileExists(file))
      	return true;
   else{ 
   		WScript.Echo("Cant find file '" + file + "'!")
      	return false;
   }   
}

function IsFolder (folder){
	var fso = new ActiveXObject("Scripting.FileSystemObject");
	
   	if (fso.FolderExists(folder))
      	return true;
   else{ 
   		WScript.Echo("Cant find folder '" + folder + "'!")
      	return false;
   }   
}


//Return array with fullpath to files in dirpath and dirpath subirs matching re string 
//E.g listFiles(".", '\\.xls$');

function listFiles(dirpath, re){

  var fso = new ActiveXObject("Scripting.FileSystemObject"),
      fileList =new Array();
  recurseFiles(fso.GetFolder(dirpath), re, fileList); 
  return(fileList);
}

//listFiles service function
function recurseFiles(folder, re, fileList){

  var ef = new Enumerator(folder.files);
  while (! ef.atEnd()) {
    var file = ef.item();
    if (file.name.match(re)){
      //WScript.Echo(file.name);
      fileList.push(file.Path)
    }
    ef.moveNext();
  }

  var ed = new Enumerator(folder.SubFolders);
  while (! ed.atEnd()) {
    var subfolder = ed.item();
    recurseFiles(subfolder, re, fileList);
    ed.moveNext();
  }
}


function ShowUsage (){
  WScript.Echo("USAGE:");
  WScript.Echo("cscript bbg2xlsx.js dirpath");
  WScript.Echo("All xls files in dirpath and its subdirs will be converted to xlsx"); 	  
  WScript.Quit();
}

