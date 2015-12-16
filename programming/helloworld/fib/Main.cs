class Program
{
    static void Main()
    {
        var buf = new long[50];
        for (int i = 0; i < buf.Length; i++)
        {
            buf[i] = i < 2 ? i : buf[i] = buf[i - 1] + buf[i - 2];
            System.Console.WriteLine(buf[i]);
        }
    }
}
