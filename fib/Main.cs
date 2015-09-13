class Program
{
    static void Main()
    {
        System.Console.WriteLine(0);
        System.Console.WriteLine(1);
        var buf = new long[50];
        buf[0] = 0;
        buf[1] = 1;
        for (int i = 2; i < 50; i++)
	{
            buf[i] = buf[i - 1] + buf[i - 2];
            System.Console.WriteLine(buf[i]);
	}
    }
}
