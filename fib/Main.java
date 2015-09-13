public class Main {
    public static void main(String[] args) {
	System.out.println(0);
	System.out.println(1);
	long[] buf = new long[50];
	buf[0] = 0;
	buf[1] = 1;
        for (int i = 2; i < 50; i++) {
            buf[i] = buf[i - 1] + buf[i - 2];
            System.out.println(buf[i]);
        }
    }
}
