using System;
using System.IO;

class Program
{
    static void Main(string[] args)
    {
        if (args.Length == 0)
        {
            Console.WriteLine("Please provide a filename as an argument.");
            return;
        }

        string filename =  args.Length == 1 ? args[0] : args[1];
        bool append = args.Length > 1 && args[0] == "-a";

        using (StreamWriter writer = new StreamWriter(filename, append)) // The 'append' argument enables appending
        {
            string line;
            while ((line = Console.ReadLine()) != null)
            {
                Console.WriteLine(line);
                writer.WriteLine(line);
            }
        }
    }
}
