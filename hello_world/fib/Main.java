public class Main {
    public static void main(String[] args) {
        long[] buf = new long[50];
        for (int i = 0; i < buf.length; i++) {
            buf[i] = i < 2 ? i : buf[i - 1] + buf[i - 2];
            System.out.println(buf[i]);
        }
    }
}
