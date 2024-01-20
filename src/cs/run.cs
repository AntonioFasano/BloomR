using System;
using System.Diagnostics;
using System.IO;


    namespace InteractWithConsoleApp
    {
        class Program {
	    // This should be enough of a manual
	    static void usage(){            
		string HELP =
		    @"
Run a command with a tee-like log.
USAGE:
{0} [-log|-alog] file command [OPTIONS]
{0} [-nolog] command [OPTIONS]
{0} -help | -h
NB: For a command in current dir, use  '.\\command', rather than 'command'
";
		    		    
		
		string myname = AppDomain.CurrentDomain.FriendlyName;
		Console.WriteLine(HELP, myname);

            }
	    
	    // Globals
	    static bool isLog;	    
	    static StreamWriter logStream;

            static int Main(string[] args)
            {
		
		//Parse first switch (log type)
		string logArg = "";
		bool isAppend = false;		
		if(args.Length > 0) {
		    
		    logArg = args[0];					    
		    switch(logArg)
		    {
			case "-help":
			case "-h":
			case "--help":
			    usage();
			    return(0);
			
			case "-log":
			    isLog = true;
			    break;
			
			case "-alog":
			    isLog = true;
			    isAppend = true;
			    break;
			
			case "-nolog":
			    isLog = false;
			    break;
			
			default:
			    Console.WriteLine("First argument should be '-log', '-alog' or '-nolog'");
			    return(10);
		    }
		}

		// Count options
		if(args.Length < 2)
		{
		    Console.WriteLine("Too few arguments.");
		    usage();
		    return(10);
		}



		// Extract log and executable file names 
		string logfile = "";
		string executable = args[1];
		if(isLog){
		    logfile = args[1];
		    if(args.Length < 3) {
			Console.WriteLine("Too few arguments.");
			usage();
			return(10);
		    }
		    executable = args[2];
		}

		// Extract executable arguments
		string cmdArg = "";
		// opt [log] exec arg
		int cmdArgPos = (isLog ? 4 : 3) -1;
		for (int i = cmdArgPos; i < args.Length; ++i) {
		    cmdArg += " " + args[i];
		} 

		// Test file path
		string fullExe = "", fullLog = "";
		if(! maybeFullExe(executable, ref fullExe)) return(10);
		if(isLog)
		    if(! maybeFullLog(logfile, ref fullLog)) return(10);
				

		// Config process
                Process prc = new Process();
		prc.StartInfo.FileName = fullExe;
		prc.StartInfo.Arguments = cmdArg;
		prc.StartInfo.RedirectStandardOutput = true;                  
		prc.StartInfo.RedirectStandardError = true;                   
		prc.StartInfo.UseShellExecute = false; 
		prc.StartInfo.CreateNoWindow = true;		
                prc.OutputDataReceived += stdout_handle;
                prc.ErrorDataReceived += stderr_handle;
                prc.EnableRaisingEvents = true;

		// Open stream writer and give command info
		logStream = isLog ? new StreamWriter(logfile, append: isAppend) : null;
		writeToboth("Log file: {0}{1}{2}", logArg, isLog? "\n": "", fullLog); 
		writeToboth("Full executable path:\n{0}", fullExe);
		writeToboth("Exec args:\n{0}", cmdArg);
		writeToboth("End of 'run.exe' log. Start '{0}' log.\n", executable);
		
		// Start and wait for exit
                prc.Start();
                prc.BeginOutputReadLine();
                prc.BeginErrorReadLine();
                prc.WaitForExit();

		// Close stream writer and give exit info
		string n_eq = new String('=', 60);		
		writeToboth("\nEnd of '{0}' log\n{1}\n", executable, n_eq);
		if(isLog) logStream.Close();
	    
		// Parse exit code
		if(prc.ExitCode > 0)
		    Console.WriteLine("Process exited with code: {0}", prc.ExitCode);		
		return(prc.ExitCode);
            }
	    
	    static void stdout_handle(object sender, DataReceivedEventArgs e) 
            {
                if(String.Format("{0}", e.Data) != "") {
		    string text = e.Data.Replace("{", "{{").Replace("}", "}}");
		    writeToboth(text);
		}
	    }
	    
            static void stderr_handle(object sender, DataReceivedEventArgs e) 
            {		
		// Some processes use the error stream even without them, e.g Emacs for its 'message' func
                if(String.Format("{0}", e.Data) != "") {
		    string text = e.Data.Replace("{", "{{").Replace("}", "}}");
		    writeToboth("Second channel used:");
		    writeToboth(text);
		}	       
            }

	    // Write to console and possibly to log file
            static void writeToboth(string format, params object[] args)
	    {
		string text = String.Format(format, args);
		Console.WriteLine(text);
		if(isLog) logStream.WriteLine(text);	       
	    }


	    // Test executable exists and return result and by reference full path 
	    static bool maybeFullExe(string executable, ref string fullExe)
            {		
		if(executable == "") {
		    Console.WriteLine("Executable file is empty.");
		    return(false);
		}

		// it adds .exe if missing
		string executable_exe = Path.ChangeExtension(executable, ".exe");
		
		// Possibly not from PATH variable
		string nonpathExe = ""; // not retrieved from PATH variable
		if(File.Exists(executable_exe)) nonpathExe = Path.GetFullPath(executable_exe);
		
		// Possibly retrieved from PATH variable
		string inpathExe = "",
		    envPath = Environment.GetEnvironmentVariable("PATH");
		foreach (string path in envPath.Split(Path.PathSeparator))
		{
		    string fp = Path.Combine(path, executable_exe);
		    if (File.Exists(fp)) {
			inpathExe = Path.GetFullPath(fp);
			break;
		    }
		}

		// Test values
		fullExe = "";
		if(inpathExe  != "") fullExe = inpathExe;
		if(nonpathExe != "") fullExe = nonpathExe;
		if(fullExe == "") {
		    Console.WriteLine("Executable not found:\n{0}", executable);
		    return(false);
		}

		// If we have 2 exes, give priority to in-PATH exe
		// To prioritize current dir use .\\foo instead
		if(inpathExe != "" && nonpathExe != "") 
		    if(Path.GetFileName(fullExe) == Path.ChangeExtension(executable, ".exe"))
			fullExe = inpathExe;

		return(true);

	    }

	    // Test logile dir exists and return result and by reference full log path 
	    static bool maybeFullLog(string logfile, ref string fullLog)
	    {

		if(logfile == "") {
		    Console.WriteLine("logfile is empty.");
		    return(false);
		}
		
		string logdir = "";
		fullLog = ""; 
		try {
		    logdir = Path.GetDirectoryName(logfile);
		}
		catch (ArgumentException e) {
		    Console.WriteLine("Unable to extract logfile directory:\n{0}", logfile);
		    Console.WriteLine("{0}: {1}", e.GetType().Name, e.Message);
		    return(false);
		}
		
		string basename = ""; 
		try {
		    basename = Path.GetFileName(logfile);
		}
		catch (ArgumentException e) {
		    Console.WriteLine("Unable to extract logfile name:\n{0}", logfile);
		    Console.WriteLine("{0}: {1}", e.GetType().Name, e.Message);
		    return(false);
		} 

		if(logdir != "" && !Directory.Exists(logdir)) {
		    Console.WriteLine("Inexistent logfile directory:\n{0}", logdir);
		    return(false);
		}

		if(logdir == "") logdir = ".";	       
		fullLog = Path.Combine(Path.GetFullPath(logdir), basename); 
		return(true);				
	    }
        }
    }
