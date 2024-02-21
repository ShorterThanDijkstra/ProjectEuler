public class P2 { 
    public static void main(String[] args) {
	long max = 4000_000L;
        long a = 1;
        long b = 1;
        long sum = 0;
        while (b <= max) {
            if (b % 2 == 0) {
                sum += b;
            }
            long tmp = b;
            b = a + b;
            a = tmp;
        }
        System.out.println(sum);

    }
}
